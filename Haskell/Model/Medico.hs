module Haskell.Model.Medico where
import Haskell.View.Utils (split)
import Prelude hiding (id)
import Haskell.Model.DateCycle

data Medico = Medico {
    id :: Int,
    nome :: String,
    crm :: String,
    idUbs :: Int,
    especialidade :: String,
    horarios :: DateCycle
} deriving (Show)

toString :: Medico -> String
toString m = show (id m) ++ ";" ++
                  (nome m) ++ ";" ++
                  (crm m) ++ ";" ++
             show (idUbs m) ++ ";" ++
                  (especialidade m) ++ ";" ++
             show (start (horarios m)) ++ ";" ++
             show (end (horarios m)) ++ ";" ++
             show (timeSc (horarios m))

instance Read Medico where
    readsPrec _ str = do
    let l = split str ';' ""
    let id = read (l !! 1) :: Int
    let nome = read (l !! 2) :: String
    let crm = read (l !! 3) :: String
    let idUbs = read (l !! 0) :: Int
    let especialidade = read (l !! 4) :: String
    let horarios = empty
    [(Medico id nome crm idUbs especialidade horarios, "")]
