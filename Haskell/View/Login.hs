module Haskell.View.Login where

import qualified Haskell.Model.BD as BD

{-

Interface inicial com o usuário. Apresenta o sistema e oferece as opções de
Login ou Cadastro.

-}
login :: BD.BD -> IO()
login dados  = do
    putStrLn "Hello world!"