module Haskell.View.UBSMenu where

import qualified Haskell.Model.BD as BD

{-

Interface da UBS, deve realizar as operações descritas na API

-}
ubsMenu :: BD.BD -> IO()
ubsMenu dados = do
    putStrLn "Hello world!"