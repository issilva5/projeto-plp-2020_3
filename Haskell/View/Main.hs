import qualified Haskell.View.Login as Login
import qualified Haskell.Model.BD as BD

main :: IO()
main = do
    Login.login (BD.BD [] [] [] [] [] [] [] [] [])