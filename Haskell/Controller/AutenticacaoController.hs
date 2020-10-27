module Haskell.Controller.AutenticacaoController (
  autentica
) where

-- Verifica se o login e senha estão corretos
autentica :: [(String, String)] -> String -> String -> Int 
autentica logins s1 s2 = -1
