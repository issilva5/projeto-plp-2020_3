module Haskell.Model.Laudo where
import Haskell.View.Utils (split)
import Prelude hiding (id)

data Laudo = Laudo {
     id :: Int,
     idMed :: Int,
     idExame :: Int,
     texto :: String
}

toString :: Laudo -> String
toString l =
     show (id l) ++ ";" ++
     show (idMed l) ++ ";" ++
     show (idExame l) ++ ";" ++
     texto l

instance Show Laudo where
    show (Laudo id idM idE t) = "----------------------------\n" ++
                                "LAUDO " ++ (show id) ++ "\n" ++
                                "Médico responsável: " ++ (show idM) ++ "\n" ++
                                "Exame correspondente: " ++ (show idE) ++ "\n" ++
                                "Resultado: " ++ t

instance Read Laudo where
     readsPrec _ str = do
     let l = split str ';' ""
     let id = read (l !! 0) :: Int
     let idMed = read (l !! 1) :: Int
     let idExame = read (l !! 2) :: Int
     let texto = l !! 3

     [(Laudo id idMed idExame texto, "")]