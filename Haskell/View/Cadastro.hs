module Haskell.View.Cadastro where

import qualified Haskell.Model.BD as BD

{-

Interface de cadastro de Paciente e UBS, lê as informações, os cria e depois retorna para o login

-}
cadastra :: BD.BD -> IO()
cadastra dados = do
    putStrLn "Hello World!"