module SubtitleList
( SubtitleList
, empty
, fromList
, size
, minElement
, minElementWithIndex
, maxElement
, maxElementWithIndex
, at
, insert
, remove
, removeAt
, subsOverlappingInterval
, subsOverlappingIntervalWithIndex
, subsOverlappingSubtitle
, subsOverlappingSubtitleWithIndex
, foldrSubs
, mapSubs
, mapSubsDialog
) where

import Subtitle

data SubtitleList  = Empty
                   | Tree Subtitle  -- Element of the node
                      Int           -- Size of the tree rooted at this node
                      Int           -- Max endpoint in the tree rooted at this node
                      SubtitleList  -- Left subtree
                      SubtitleList  -- Right subtree


empty :: SubtitleList
empty = Empty

fromList :: [Subtitle] -> SubtitleList
fromList = foldr insert empty


-- "Smart constructors"

-- Automatically calculate size of the tree and the max end point
makeTree :: Subtitle -> SubtitleList -> SubtitleList -> SubtitleList
makeTree e l r = let treeSize = 1 + size l + size r
                     maxEndPoint = maximum [end e, safeGetMaxEndPoint l, safeGetMaxEndPoint r]
                 in Tree e treeSize maxEndPoint l r

  where safeGetMaxEndPoint :: SubtitleList -> Int
        safeGetMaxEndPoint Empty = minBound
        safeGetMaxEndPoint (Tree _ _ m _ _) = m


-- Make a balanced tree
makeBalancedTree :: Subtitle -> SubtitleList -> SubtitleList -> SubtitleList
makeBalancedTree e l r
                        | lSize + rSize < 2      = makeTree e l r

                        | lSize > weight * rSize = let (Tree le _ _ ll lr) = l
                                                   in if size lr < ratio * size ll
                                                      then singleRotationRight e l r
                                                      else doubleRotationRight e l r

                        | rSize > weight * lSize = let (Tree re _ _ rl rr) = r
                                                   in if size rl < ratio * size rr
                                                      then singleRotationLeft e l r
                                                      else doubleRotationLeft e l r

                        | otherwise              = makeTree e l r

    where lSize = size l
          rSize = size r




-- Rotations used to balance the tree
singleRotationLeft :: Subtitle -> SubtitleList -> SubtitleList -> SubtitleList
singleRotationLeft e l (Tree re _ _ rl rr) = makeTree re (makeTree e l rl) rr

singleRotationRight :: Subtitle -> SubtitleList -> SubtitleList -> SubtitleList
singleRotationRight e (Tree le _ _ ll lr) r = makeTree le ll (makeTree e lr r)


doubleRotationLeft :: Subtitle -> SubtitleList -> SubtitleList -> SubtitleList
doubleRotationLeft e l (Tree re _ _ rl rr) = let newR = singleRotationRight re rl rr
                                          in singleRotationLeft e l newR

doubleRotationRight :: Subtitle -> SubtitleList -> SubtitleList -> SubtitleList
doubleRotationRight e (Tree le _ _ ll lr) r = let newL = singleRotationLeft le ll lr
                                              in singleRotationRight e newL r


-- Parameters dealing with balance of the tree
weight, ratio :: Int
weight = 3
ratio = 2


-- Queries

size :: SubtitleList -> Int
size Empty = 0
size (Tree _ s _ _ _) = s




minElement :: SubtitleList -> Maybe Subtitle
minElement Empty = Nothing
minElement (Tree e _ _ Empty _) = Just e
minElement (Tree _ _ _ l _) = minElement l

minElementWithIndex :: SubtitleList -> Maybe (Int, Subtitle)
minElementWithIndex t = (\s -> (0, s)) <$> minElement t

maxElement :: SubtitleList -> Maybe Subtitle
maxElement Empty = Nothing
maxElement (Tree e _ _ _ Empty) = Just e
maxElement (Tree _ _ _ _ r) = maxElement r

maxElementWithIndex :: SubtitleList -> Maybe (Int, Subtitle)
maxElementWithIndex t = (\s -> (size t, s)) <$> maxElement t

at :: Int -> SubtitleList -> Subtitle
at idx (Tree e _ _ l r) = let currPos = size l + 1
                          in case compare currPos idx of
                              LT -> at idx l
                              GT -> at (idx - currPos) r
                              EQ -> e

-- Insert & Delete

insert :: Subtitle -> SubtitleList -> SubtitleList
insert e Empty               = Tree e 1 (end e) Empty Empty
insert e t@(Tree e' _ _ l r) = case compare e e' of
                                  LT -> makeBalancedTree e' (insert e l) r
                                  GT -> makeBalancedTree e' l (insert e r)
                                  EQ -> makeBalancedTree e' l (insert e r)


remove :: Subtitle -> SubtitleList -> SubtitleList
remove _ Empty             = Empty
remove e (Tree e' _ _ l r) = case compare e e' of
                                LT -> makeBalancedTree e' (remove e l) r
                                GT -> makeBalancedTree e' l (remove e r)
                                EQ -> remove' l r

remove' :: SubtitleList -> SubtitleList -> SubtitleList
remove' Empty r = r
remove' l Empty = l
remove' l r     = let (rMin, r') = findAndRemoveMin r
                  in makeBalancedTree rMin l r'

findAndRemoveMin :: SubtitleList -> (Subtitle, SubtitleList)
findAndRemoveMin (Tree e _ _ Empty r) = (e, r)
findAndRemoveMin (Tree e _ _ l r)     = let (m, l') = findAndRemoveMin l
                                        in (m, makeBalancedTree e l' r)

removeAt :: Int -> SubtitleList -> SubtitleList
removeAt index (Tree e' _ _ l r) = let currPos = size l + 1
                                  in case compare currPos index of
                                           LT -> makeBalancedTree e' (removeAt index l) r
                                           GT -> makeBalancedTree e' l (removeAt (index - currPos) r)
                                           EQ -> remove' l r


-- SubtitleList specific operations

subsOverlappingInterval :: TimeInterval -> SubtitleList -> [Subtitle]
subsOverlappingInterval i Empty = []
subsOverlappingInterval i (Tree e _ _ l r) = if overlap i (timeInterval e)
                                          then lOverlaps ++ (e : rOverlaps)
                                          else lOverlaps ++ rOverlaps

    where lOverlaps = case l of
                          Empty                  -> []
                          (Tree _ _ lMaxEP _ _)  -> if lMaxEP >= low i
                                                    then subsOverlappingInterval i l
                                                    else []

          rOverlaps = case r of
                          Empty -> []
                          (Tree re _ rMaxEP _ _) -> if start re <= high i && rMaxEP >= low i
                                                    then subsOverlappingInterval i r
                                                    else []

subsOverlappingIntervalWithIndex :: TimeInterval -> SubtitleList -> [(Int, Subtitle)]
subsOverlappingIntervalWithIndex i t = subsOverlappingIntervalWithIndex' i t 0

subsOverlappingIntervalWithIndex' :: TimeInterval -> SubtitleList -> Int -> [(Int, Subtitle)]
subsOverlappingIntervalWithIndex' i Empty _ = []
subsOverlappingIntervalWithIndex' i (Tree e _ _ l r) s = let currPos = s + (size l + 1)
                                                         in if overlap i (timeInterval e)
                                                            then lOverlaps ++ ((currPos, e) : rOverlaps)
                                                            else lOverlaps ++ rOverlaps

    where lOverlaps = case l of
                          Empty -> []
                          (Tree _ _ lMaxEP _ _) -> if lMaxEP >= low i
                                                   then subsOverlappingIntervalWithIndex' i l s
                                                   else []

          rOverlaps = case r of
                          Empty -> []
                          (Tree re _ rMaxEP _ _) -> if start re <= high i && rMaxEP >= low i
                                                    then subsOverlappingIntervalWithIndex' i r (s + size l + 1)
                                                    else []

         


subsOverlappingSubtitle :: Subtitle -> SubtitleList -> [Subtitle]
subsOverlappingSubtitle = subsOverlappingInterval . timeInterval

subsOverlappingSubtitleWithIndex :: Subtitle -> SubtitleList -> [(Int, Subtitle)]
subsOverlappingSubtitleWithIndex = subsOverlappingIntervalWithIndex . timeInterval


-- Functor-like and Foldable-like functions
foldrSubs :: (Subtitle -> b -> b) -> b -> SubtitleList -> b
foldrSubs f acc Empty = acc
foldrSubs f acc (Tree e _ _ l r) = foldrSubs f (f e (foldrSubs f acc r)) l

mapSubs :: (Subtitle -> b) -> SubtitleList -> [b]
mapSubs f = foldrSubs (\s acc -> f s : acc) []

mapSubsDialog :: (String -> String) -> SubtitleList -> SubtitleList
mapSubsDialog f Empty = Empty
mapSubsDialog f (Tree e s m l r) = let newSub = e { dialog = f (dialog e) }
                                   in Tree newSub s m (mapSubsDialog f l) (mapSubsDialog f r)

