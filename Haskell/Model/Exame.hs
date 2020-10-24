data Exame = Exame {
    id :: Int,
    idPaciente :: Int,
    idMedico :: Int,
    idUBS :: Int,
    tipo :: String,
    data :: String,
    resultado :: String
} deriving (Show)