module Haskell.Model.DateCycle where

import Data.Dates

data DateCycle = DateCycle {

	start :: [DateTime],
	jump :: DateInterval

} deriving (Show)

nextDate :: DateCycle -> (DateTime, DateCycle)
nextDate dc = ((head (start dc)), dc {start = drop 1 ((start dc) ++ [addInterval (head (start dc)) (jump dc)])})
