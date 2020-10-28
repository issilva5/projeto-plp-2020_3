module Haskell.Controller.AutenticacaoController (
  autentica
) where

import Haskell.Model.BD as BD
-- Verifica se o login e senha estão corretos
autentica :: [(Int, String)] ->  String -> String -> Int 
autentica logins s1 s2 = -1