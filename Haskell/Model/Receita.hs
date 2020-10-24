data Receita = Receita {
    id :: Int,
    idPaciente :: Int,
    idMedico :: Int,
    idUBS :: Int,
    remedios :: [(Int, String)]
} deriving (Show)