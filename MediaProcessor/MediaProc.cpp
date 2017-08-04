#include "MediaProc.h"

#include <media_processor/media_processor.hpp>
#include <media_processor/media_file.hpp>
#include <media_processor/peak_extractor.hpp>
#include <media_processor/ffmpeg_error.hpp>

#include <algorithm>

#include <stdio.h>

Peak *extractPeaks(const char *filename, void (*tracker)(int64_t, int64_t)
                   , size_t *peaksNum, int *sampleRate, int *samplesPerPeak)
{
    av_register_all();

    Peak *Result = nullptr;

    try {
        MediaFile f(filename);

        auto audio = f.best_stream(AVMEDIA_TYPE_AUDIO);
        MediaProcessor p(f);

        *sampleRate = (*audio)->codecpar->sample_rate;
        *samplesPerPeak = (*audio)->codecpar->sample_rate / 100;

        PeakExtractor peakEx(*audio, (*audio)->codecpar->sample_rate / 100);
        p.addStreamProcessors(&peakEx);

        struct HaskellProgressTracker: public ProgressTracker
        {
            void (*Callback)(int64_t, int64_t);

            HaskellProgressTracker(void (*callback)(int64_t, int64_t)):
                Callback(callback)
            { }

            virtual void progress(int64_t curr, int64_t total) override
            {
                Callback(curr, total);
            }
        } haskellTracker(tracker);
        
        p.startProcessing(&haskellTracker);

        // TODO: Maybe optimize this, to avoid copies?
        *peaksNum = peakEx.peaks().size();
        Result = (Peak*)std::malloc(sizeof(Peak) * *peaksNum); 
        std::copy(peakEx.peaks().begin(), peakEx.peaks().end(), Result);

        return Result;
    }
    catch(const FFmpegError &err)
    {
        if(Result)
        {
            std::free(Result);
        }

        return nullptr;
    }
}
