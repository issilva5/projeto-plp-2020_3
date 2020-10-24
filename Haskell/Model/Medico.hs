data Medico = Medico {
    id :: Int,
    nome :: String,
    crm :: String,
    -- idUbs :: Int
    especialidade :: String,
    horarios :: [String]
} deriving (Show)