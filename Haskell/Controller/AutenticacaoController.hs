module Haskell.Controller.AutenticacaoController (
  autentica
) where

-- Verifica se o login e senha estão corretos
autentica :: String -> String -> Int 
autentica s1 s2 = -1