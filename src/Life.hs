-- Based on https://gist.github.com/ihabunek/81e7da0c705689fe743a very
-- slow, should be replaced with hashlife algorithm
-- https://crypto.stanford.edu/~blynn/haskell/life.html
module Life where

import Data.Set as Set

neighbours (minX, maxX, minY, maxY) p = Set.fromList [(fst p + dx, snd p + dy) | dx <- [-1..1], dy <- [-1..1], check dx dy]
  where
    check dx dy = not (dx == 0 && dy == 0) && fst p + dx >= minX && fst p + dx <= maxX
      && snd p + dy >= minY && snd p + dy <= maxY

lives b g p = ((member p g) && count `elem` [2, 3]) || (not (member p g) && count == 3)
  where count = Set.size $ Set.filter (flip member g) (neighbours b p)

nextGen b g = Set.filter (lives b g) (union g $ unions [neighbours b p | p <- toList g])
