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
             nome m ++ ";" ++
             crm m ++ ";" ++
             show (idUbs m) ++ ";" ++
             especialidade m ++ ";" ++
             show (horarios m)

instance Read Medico where
    readsPrec _ str = do
    let l = split str ';' ""
    let id = read (l !! 0) :: Int
    let nome = l !! 1
    let crm = l !! 2
    let idUbs = read (l !! 3) :: Int
    let especialidade = l !! 4
    let horarios = read (l !! 5) :: DateCycle
    [(Medico id nome crm idUbs especialidade horarios, "")]
