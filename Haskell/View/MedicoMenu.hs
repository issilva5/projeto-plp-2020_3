module Haskell.View.MedicoMenu where

import qualified Haskell.Controller.MedicoController as MC
import qualified Haskell.Model.BD as BD

{-

Interface do médico, deve realizar as operações descritas na API

-}
medicoMenu :: BD.BD -> IO()
medicoMenu dados = do
    putStrLn "Hello world!"