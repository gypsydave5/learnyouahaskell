module OptimalPath
  ( Section(Section)
  , Label(A, B, C)
  , RoadSystem
  , optimalPath
  ) where

data Section = Section
  { getA :: Int
  , getB :: Int
  , getC :: Int
  } deriving (Show)

type RoadSystem = [Section]

data Label
  = A
  | B
  | C
  deriving (Show)

type Path = [(Label, Int)]

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
  let timeA = sum (map snd pathA)
      timeB = sum (map snd pathB)
      timeAA' = timeA + a
      timeBCA' = timeB + b + c
      timeBB' = timeB + b
      timeACB' = timeA + a + c
      newPathToA =
        if timeAA' <= timeBCA'
          then (A, a) : pathA
          else (C, c) : (B, b) : pathB
      newPathToB =
        if timeBB' <= timeACB'
          then (B, b) : pathB
          else (C, c) : (A, a) : pathA
  in (newPathToA, newPathToB)

optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
  let (bestAPath, bestBPath) = foldl roadStep ([], []) roadSystem
  in if sum (map snd bestAPath) <= sum (map snd bestBPath)
       then reverse bestAPath
       else reverse bestBPath
