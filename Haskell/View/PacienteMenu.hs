module Haskell.View.PacienteMenu where

import qualified Haskell.Model.BD as BD

{-

Interface do paciente, deve realizar as operações descritas na API

-}
pacienteMenu :: BD.BD -> IO()
pacienteMenu dados = do
    putStrLn "Hello world!"