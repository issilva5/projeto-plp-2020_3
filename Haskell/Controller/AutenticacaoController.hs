module Haskell.Controller.AutenticacaoController (
  autentica
) where


-- Verifica se o login e senha estão corretos
autentica :: [(Int, String, Int)] -> Int -> String -> Int
autentica ((u, s, tipo):xs) user senha | u == user && s == senha = tipo
                                       | otherwise = autentica xs user senha
autentica [] _ _ = -1
