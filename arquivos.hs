import System.IO
import System.IO (isEOF)
import Control.Monad (forever, when)
import System.Exit (exitSuccess)

escrever :: IO ()
escrever = do
    arq <- openFile "teste.txt" WriteMode
    hPutStrLn arq "Frase"
    hClose arq

adicionar :: Handle -> String -> IO ()
adicionar arq str = do
    hPutStrLn arq str

ler :: IO()
ler = do
    arq <- openFile "teste.txt" ReadMode
    ctd <- (hGetContents arq)
    putStrLn ctd

main = myLoop


myLoop = forever $ do
                done <- isEOF
                when done $ putStrLn "Bye!" >> exitSuccess
                arq <- openFile "teste.txt" AppendMode
                adicionar arq "FraseX"
                myLoop