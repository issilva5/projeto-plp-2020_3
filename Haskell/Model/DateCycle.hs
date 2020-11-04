module Haskell.Model.DateCycle (

  DateCycle(..),
  getNextDate,
  empty,
  isEmpty,
  isValid,
  seeNextDate,
  nextDay,
  newDC,
  (===)

) where

import Data.Dates
import qualified Data.Heap as Heap
import Haskell.View.Utils

data DateCycle = DateCycle {

  schedule :: Heap.MinHeap DateTime, -- guarda os horários de atendimento do medico
  start :: [Time], -- horarios de inicio de plantao, deve ter um Time para cada dia da semana, começando da Segunda
  end :: [Time], -- horarios de termino de plantao, deve ter um Time para cada dia da semana, começando da Segunda
  timeSc :: Int -- duração da consulta

}

{-

Cria um DateCycle vazio apenas para criação do médico.

-}
empty :: DateCycle
empty = DateCycle (Heap.fromList []) [] [] (-1)

{-

Cria um DateCycle.

-}
newDC :: DateTime -> [Time] -> [Time] -> Int -> DateCycle
newDC hj i f t = DateCycle (Heap.fromList (firstTimes hj i 0)) i f t

{-

Função auxiliar da criação de um DateCycle. Cria a lista com os primeiros horários do médico.

-}
firstTimes :: DateTime -> [Time] -> Int -> [DateTime]
firstTimes _ [] _ = []
firstTimes hj (x:xs) d | (tHour x) /= (-1) && (tMinute x) /= (-1) = [(nextDay hj d) {hour = tHour x, minute = tMinute x}] ++ (firstTimes hj xs (d+1))
                       | otherwise = (firstTimes hj xs (d+1))

{-

Verifica se o DateCycle é vazio (i.e. foi criado pela função empty).

-}
isEmpty :: DateCycle -> Bool
isEmpty dc = (timeSc dc) == -1

{-

Verifica se o DateCycle é válido. Deve ser usada para evitar erros ao usar getNextDate
em um DateCycle inválido.

-}
isValid :: DateCycle -> Bool
isValid dc = emptiness && lengthLists && heapEmpty
             where emptiness = (not (isEmpty dc))
                   lengthLists = (length (start dc)) == 7 && (length (end dc)) == 7
                   heapEmpty = not (Heap.isEmpty (schedule dc))

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

{-
Dada uma data e um dia da semana, retorna a data do próximo dia da semana especificado.
Exemplo nextDay (30/10/2020) Segunda => 02/11/2020
-}
nextDay :: DateTime -> Int -> DateTime
nextDay date wkd | (weekdayNumber (dateWeekDay date)) == wkd = date
                 | otherwise = (addInterval (nextMonday date) interval)
                 where interval = Days (toInteger wkd)

{-

Retorna a próxima data livre sem alterar retirá-la.

-}
seeNextDate :: DateCycle -> DateTime -> DateTime
seeNextDate dc now = fst (getNextDate dc now)

{-

Converte uma string do tipo DD/MM/YYYY em DateTime

-}
instance Read DateTime where 
    readsPrec _ str = do 
    let l = split str '/' "" 
    let year = read (l !! 2) :: Int
    let month = read (l !! 1) :: Int
    let day = read (l !! 0) :: Int   
    [(DateTime year month day 00 00 00, "")]

instance Show DateCycle where
    show (DateCycle _ s e d) = (timeShow s) ++ ";" ++ (timeShow e) ++ ";" ++ (show d)

{-

Instância do read para DateCycle.
Após realizar o read para DateCycle é necessário inicializar o DateCycle.

Para isso, utilizar a função newDC.

-}
instance Read DateCycle where
    readsPrec _ str = do
      let l = split str ';' ""
      let s = map read (split (l !! 0) ',' "") :: [Time]
      let e = map read (split (l !! 1) ',' "")  :: [Time]
      let d = read (l !! 2) :: Int
      [(DateCycle (Heap.fromList []) s e d, "")]
  
timeShow :: [Time] -> String
timeShow [] = ""
timeShow (x:xs) = ((show (tHour x)) ++ ":" ++ (show (tMinute x)) ++ ":" ++ (show (tSecond x))) ++ "," ++ (timeShow xs)

{-

Compara duas datas sem levar em conta seu horário.

-}
(===) :: DateTime -> DateTime -> Bool
(===) d1 d2 | (year d1) == (year d2) && (month d1) == (month d2) && (day d1) == (day d2) = True
            | otherwise = False

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