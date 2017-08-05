{-# LANGUAGE ForeignFunctionInterface #-}

module PeakExtractor
( ProgressTracker
, extractPeaks
, timedExtraction
)
where

import Peak
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Alloc (alloca, free, finalizerFree)
import Foreign.ForeignPtr

import System.TimeIt

import qualified Data.Vector.Storable as VS

import AudioInfo

type ProgressTracker = CLong -> CLong -> IO ()


foreign import ccall "MediaProcessor/MediaProc.h extractPeaks"
    c_extractPeaks :: CString
                   -> FunPtr ProgressTracker
                   -> Ptr CSize
                   -> Ptr CInt
                   -> Ptr CInt
                   -> IO (Ptr Peak)

foreign import ccall "wrapper"
    makeProgressTracker :: ProgressTracker -> IO (FunPtr ProgressTracker)

extractPeaks :: ProgressTracker -> String -> IO (Maybe AudioInfo)
extractPeaks tracker filename = do
    trackerWrapper <- makeProgressTracker tracker
    alloca $ \sizePtr -> alloca $ \sampleRatePtr -> alloca $ \samplesPerPeakPtr -> do
        peakList <- withCString filename (\filename' -> c_extractPeaks filename' trackerWrapper
                                                           sizePtr sampleRatePtr samplesPerPeakPtr)

        if peakList == nullPtr
        then return Nothing
        else do
            managedPeakList <- newForeignPtr finalizerFree peakList

            peakListSize <- peek sizePtr
            sampleRate <- peek sampleRatePtr
            samplesPerPeak <- peek samplesPerPeakPtr

            let vector = VS.convert $ VS.unsafeFromForeignPtr0 managedPeakList (fromIntegral peakListSize)

            freeHaskellFunPtr trackerWrapper
            return . Just $ AudioInfo vector (fromIntegral sampleRate) (fromIntegral samplesPerPeak)


timedExtraction :: ProgressTracker -> String -> IO ()
timedExtraction tracker filename = timeIt $ do
    extractPeaks tracker filename
    return ()
