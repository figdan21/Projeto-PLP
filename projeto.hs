import Text.Printf

data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     , filmFav :: String
                     } deriving (Show)

opcao :: IO()
opcao = do
    putStrLn "Cadastrar usuario (1);\n"
    input <- getLine
    escolha (read input)

escolha :: Int -> IO()
escolha x
    | x == 1 = do
        putStrLn "Nome:"
        firstName <- getLine
        putStrLn "Sobrenome:"
        lastName <- getLine
        putStrLn "Idade:"
        age <- getLine
        putStrLn "Filme favorito:"
        filmFav <- getLine

        let person = Person (firstName) (lastName) (read age) (filmFav)

        printf "\nCadastro realizado, %s\n" firstName


main :: IO()
main = do
    opcao