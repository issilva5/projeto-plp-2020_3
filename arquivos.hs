import System.IO

escrever :: IO ()
escrever = do
    arq <- openFile "Haskell/Persistence/teste.txt" WriteMode
    hPutStrLn arq "Frase"
    putStrLn "Escrita realizada"

ler :: IO()
ler = do
    arq <- openFile "Haskell/Persistence/teste.txt" ReadMode
    ctd <- (hGetContents arq)
    putStrLn ctd