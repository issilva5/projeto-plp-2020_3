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
}

toString :: Medico -> String
toString m = show (idUbs m) ++ ";" ++
             show (id m) ++ ";" ++
             nome m ++ ";" ++
             crm m ++ ";" ++
             especialidade m ++ ";" ++
             show (horarios m)

instance Show Medico where
    show (Medico id n c idU e _) =  "----------------------------\n" ++
                                    "MEDICO" ++ (show id) ++ "\n" ++
                                    "Nome: " ++ n ++ "\n" ++
                                    "CRM: " ++ c ++ "\n" ++
                                    "UBS: " ++ (show idU) ++ "\n" ++
                                    "Especialidade: " ++ e ++ "\n"

instance Read Medico where
    readsPrec _ str = do
    let l = split str ';' ""
    let id = read (l !! 1) :: Int
    let nome = l !! 2
    let crm = l !! 3
    let idUbs = read (l !! 0) :: Int
    let especialidade = l !! 4
    let horarios = if (length l == 6) then read (l !! 5) :: DateCycle else empty
    [(Medico id nome crm idUbs especialidade horarios, "")]
