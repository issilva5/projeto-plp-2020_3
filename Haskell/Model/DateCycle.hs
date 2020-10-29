module Haskell.Model.DateCycle (

  DateCycle(..),
  getNextDate

) where

import Data.Dates
import qualified Data.Heap as Heap

data DateCycle = DateCycle {

  schedule :: Heap.MinHeap DateTime, -- guarda os horários de atendimento do medico
  start :: [Time], -- horarios de inicio de plantao, deve ter um Time para cada dia da semana, começando da Segunda
  end :: [Time], -- horarios de termino de plantao, deve ter um Time para cada dia da semana, começando da Segunda
  timeSc :: Int -- duração da consulta

} deriving (Show)

{-

Recebe os horários de um médico e o horário atual, e retorna uma tupla com dois elementos, na qual:
 * Primeiro elemento: próximo horário livre do médico
 * Segundo elemento: horários do médico atualizado com a retirada do livre

Esta função retira o horário.

-}
getNextDate :: DateCycle -> DateTime -> (DateTime, DateCycle)
getNextDate dc now | nextSc < now = getNextDate (snd (nextSc, dc {schedule = Heap.insert (addJump dc nextSc) noHead})) now
                   | otherwise = (nextSc, dc {schedule = Heap.insert (addJump dc nextSc) noHead})
                   where nextSc = head (Heap.take 1 (schedule dc))
                         noHead = (Heap.drop 1 (schedule dc))

dateTimeToTime :: DateTime -> Time
dateTimeToTime dt = (Time (hour dt) (minute dt) (second dt))

startD :: Int -> DateCycle -> Time
startD x dt = (start dt) !! x

endD :: Int -> DateCycle -> Time
endD x dt = (end dt) !! x

correctDate :: DateTime -> DateTime
correctDate dt | sec >= 60 = correctDate dt {minute = (min) + (quot sec 60), second = (mod sec 60)}
               | min >= 60 = correctDate dt {hour = (hor) + (quot min 60), minute = (mod min 60)}
               | hor >= 60 = correctDate (addInterval dt (Days (toInteger (quot hor 24)))) {hour = (mod hor 24)}
               | otherwise = dt
               where sec = second dt
                     min = minute dt
                     hor = hour dt

addJump :: DateCycle -> DateTime -> DateTime
addJump dc dt | (dateTimeToTime jumped) >= endTime = jumpWeek
              | otherwise = jumped
              where jumped = correctDate (addTime dt (Time 0 (timeSc dc) 0))
                    weekday = (weekdayNumber (dateWeekDay dt)) - 1
                    startTime = startD weekday dc
                    endTime = endD weekday dc
                    startHour = tHour startTime
                    startMin = tMinute startTime
                    startSec = tSecond startTime
                    jumpWeek = (addInterval dt (Weeks 1)) {hour = startHour, minute = startMin, second = startSec}
