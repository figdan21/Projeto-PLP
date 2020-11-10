import Data.List
import System.Directory
import System.Environment  
import System.IO

-- //////////////////////////////////////  DATA  //////////////////////////////////////

data Filme = Filme {
    indiceFilme :: Integer,
    nomeFilme :: String,
    notaFilme :: Float,
    elencoFilme :: [String],
    diretorFilme :: String,
    roteristaFilme :: String,
    generosFilme :: [String],
    premiacoesFilme :: [String],
    dataLacancamento :: String,
    sinopseFilme :: String
    } deriving (Show)

-- //////////////////////////////////////  FILMES INICIAIS  //////////////////////////////////////

filmesCadastrados :: [Filme]
filmesCadastrados = [Filme {indiceFilme = 1, nomeFilme = "Titanic", notaFilme= 0.0, elencoFilme = ["Leonardo DiCaprio","Kate Winslet","Billy Zane","Kathy Bates","Frances Fisher","Gloria Stuart","Bernard Hill","Victor Garber","Jonathan Hyde","Danny Nucci","Bill Paxton"], diretorFilme = "James Cameron",roteristaFilme = "James Cameron", generosFilme = ["Epico", "Drama", "Romance"], premiacoesFilme = ["Melhor Filme", "Melhor Diretor", "Melhor Edicao", "Melhor Fotografia"], dataLacancamento = "19/12/1997", sinopseFilme = "Titanic é um filme épico de romance e drama norte-americano de 1997, escrito, dirigido, co-produzido e co-editado por James Cameron. É uma história de ficção do naufrágio real do RMS Titanic, estrelando Leonardo DiCaprio como Jack Dawson, e Kate Winslet como Rose DeWitt Bukater, membros de diferentes classes sociais que se apaixonam durante a fatídica viagem inaugural no navio saindo de Southampton para Nova York em 15 de Abril de 1912. Apesar dos personagens principais serem fictícios, alguns personagens são figuras históricas. Gloria Stuart interpreta Rose idosa, que narra o filme; e Billy Zane interpreta Cal Hockley, o noivo rico da jovem Rose. Cameron viu a história de amor como um jeito de cativar o público para o desastre real."},
    Filme {indiceFilme = 2, nomeFilme = "A Lagoa Azul", notaFilme= 0.0, elencoFilme = ["Brooke Shields","Christopher Atkins","Leo McKern","William Daniels"], diretorFilme = "Randal Kleiser",roteristaFilme = "Douglas Day Stewart", generosFilme = ["Aventura","Drama","Romance"], premiacoesFilme = ["Oscar 1981", "Globo de Ouro 1981", "Framboesa de Ouro"], dataLacancamento = "20/06/1980", sinopseFilme = "No período vitoriano, duas crianças crescem juntas numa ilha paradisíaca, cuidadas por um velho marinheiro, após um naufrágio. Quando o velho morre, elas precisam se virar sozinhas, e acabam descobrindo o amor."}]

-- //////////////////////////////////////  MENU  //////////////////////////////////////

main :: IO ()
main = do
    menuPrint
    menuOpcao


menuPrint :: IO ()
menuPrint = do
    putStrLn "Bem-vindo!"


menuOpcao :: IO ()
menuOpcao = do
    putStrLn "0 - Sair" 
    putStrLn "1 - Listar todos os filmes"
    putStrLn "2 - Listar filmes por genero"
    putStrLn "3 - Enviar recomendação de filme"
    putStrLn "4 - Visuzalizar suas recomendações de filme"
    putStrLn "5 - Listar filmes por elenco"
    putStrLn "6 - Listar filmes por diretor"
    putStrLn "7 - Listar filmes por roterista"
    putStrLn "8 - Listar filmes por premiações"
    putStrLn "9 - Cadastrar novo usuário"
    putStrLn "\nOpcao: "
    opcao <- getLine
    if (read opcao) == 0 then putStrLn("Fim") else do opcaoEscolhida (read opcao)


menuListagem :: IO()
menuListagem = do
    putStrLn "0 - Voltar para o menu principal" 
    putStrLn "1 - Descobrir mais informações sobre um filme"
    putStr "\nOpção: "
    opcao <- getLine
    if (read opcao) == 1
        then do
            visualizarInfoFilme
    else if (read opcao) /= 0 
        then do
            putStrLn "==> Opção inválida"
    else
        putStr ""
    printEspaco


    
-- //////////////////////////////////////  ESCOLHER OPÇÃO  //////////////////////////////////////

opcaoEscolhida :: Int -> IO()
opcaoEscolhida opcao | opcao == 1 = do {imprimeFilmes ; menuListagem; menuOpcao}              
                     | opcao == 2 = do {imprimeListarFilmesPorGenero; menuOpcao}    
                     | opcao == 3 = do {enviarRecomendacao; menuOpcao}  
                     | opcao == 4 = do {menuView; menuOpcao}  
                     | opcao == 5 = do {imprimeListarFilmesPorElenco; menuOpcao}
                     | opcao == 6 = do {imprimeListarFilmesPorDiretor; menuOpcao}
                     | opcao == 7 = do {imprimeListarFilmesPorRoterista; menuOpcao}
                     | opcao == 8 = do {imprimeListarFilmesPorPremio; menuOpcao}
                     | opcao == 9 = do {cadastrarUsuario; menuOpcao}
                     | otherwise =  do {putStrLn "Opcao invalida, Porfavor escolha uma opcao valida" ; menuOpcao}

-- //////////////////////////////////////  PRINTS  //////////////////////////////////////

imprimeFilmes :: IO()
imprimeFilmes = putStrLn ("\n\n\n" ++ (listarFilmes filmesCadastrados) ++ "\n\n")

menuView :: IO()
menuView = do
    printEspaco
    putStrLn "==> Seu Nick:"
    userNick <- getLine
    view userNick 0

view :: String -> Int -> IO()
view userNick count = do
    printEspaco
    contents <- readFile "recomendacoes.txt"
    let todoTasks = lines contents
        line = todoTasks !! count
        list = words line
        nick = list !! 0
    
    if userNick == nick then do
        putStrLn $ line
        view userNick (count+1)
        else putStrLn "Essas são as recomendações para você!"
    
    printEspaco
    
imprimeListarFilmesPorGenero :: IO()
imprimeListarFilmesPorGenero = do
    printEspaco
    putStrLn "==> Insira o nome do genero na qual você deseja filtrar: "
    genero <- getLine
    let filmesDoGenero = "\n==> Os filmes que possuem esse genero são:\n" ++ unlines(listarFilmesPorGenero ( filmesCadastrados ) ([]) ( genero ))
    if filmesDoGenero == "\n==> Os filmes que possuem esse genero são:\n"
        then putStrLn "\nNão há filmes desse gênero na biblioteca.\n"
        else
            putStrLn filmesDoGenero
    printEspaco
    
imprimeListarFilmesPorElenco :: IO()
imprimeListarFilmesPorElenco = do
    printEspaco
    putStrLn "==> Insira o nome do Ator(a) na qual você deseja filtrar: "
    elenco <- getLine
    let elenco_do_Filme = "\n==> Os filmes em que esse ator atuou são:\n" ++ unlines(listarFilmesPorElenco ( filmesCadastrados ) ([]) ( elenco ))
    if elenco_do_Filme == "\n==> Os filmes em que esse ator atuou são:\n"
        then putStrLn "\nNão há filmes em que esse ator atuou  na biblioteca.\n"
        else
            putStrLn elenco_do_Filme
    printEspaco
    
imprimeListarFilmesPorDiretor :: IO()
imprimeListarFilmesPorDiretor = do
    printEspaco
    putStrLn "==> Insira o nome do Diretor no qual você deseja filtrar: "
    diretor <- getLine
    let diretor_do_Filme = "\n==> Os filmes que esse diretor dirigiu são:\n" ++ unlines(listarFilmesPorDiretor ( filmesCadastrados ) ([]) ( diretor ))
    if diretor_do_Filme == "\n==> Os filmes que esse diretor dirigiu são:\n"
        then putStrLn "\nNão há filmes dirigidos por esse diretor  na biblioteca.\n"
        else
            putStrLn diretor_do_Filme
    printEspaco
    
imprimeListarFilmesPorRoterista :: IO()
imprimeListarFilmesPorRoterista = do
    printEspaco
    putStrLn "==> Insira o nome do Roterista no qual você deseja filtrar: "
    roterista <- getLine
    let roterista_do_Filme = "\n==> Os filmes que esse roterista escreveu são:\n" ++ unlines(listarFilmesPorRoterista ( filmesCadastrados ) ([]) ( roterista ))
    if roterista_do_Filme == "\n==> Os filmes que esse roterista escreveu são:\n"
        then putStrLn "\nNão há filmes escritos por esse roterista  na biblioteca.\n"
        else
            putStrLn roterista_do_Filme
    printEspaco
    
imprimeListarFilmesPorPremio :: IO()
imprimeListarFilmesPorPremio = do
    printEspaco
    putStrLn "==> Insira o nome do Premio no qual você deseja filtrar: "
    premiacao <- getLine
    let premiacao_do_Filme = "\n==> Os filmes que possui esse Premio são:\n" ++ unlines(listarFilmesPorPremio ( filmesCadastrados ) ([]) ( premiacao ))
    if premiacao_do_Filme == "\n==> Os filmes que possui esse Premio são:\n"
        then putStrLn "\nNão há filmes premiados com esse premio na biblioteca.\n"
        else
            putStrLn premiacao_do_Filme
    printEspaco
-- //////////////////////////////////////  OPERAÇÕES  //////////////////////////////////////



enviarRecomendacao :: IO()
enviarRecomendacao = do
    printEspaco
    putStrLn "==> Digite o nome do Filme que deseja recomendar: "
    nomeFilme <- getLine
    putStrLn "==> Confirme seu Nick:"
    userNick <- getLine
    putStrLn "==> Nick do usuário que irá receber a recomendação:"
    nickRec <- getLine
    appendFile "recomendacoes.txt" (nickRec ++ " - " ++ nomeFilme ++ " - Recomendação de: " ++ userNick)
    printEspaco

cadastrarUsuario :: IO()
cadastrarUsuario = do
    printEspaco
    putStrLn "==> Nome: "
    nome <- getLine
    putStrLn "==> Sobrenome: "
    sobrenome <- getLine
    putStrLn "==> Nick: "
    nick <- getLine
    putStrLn "==> Gênero preferido: "
    genero <- getLine
    appendFile "usuarios.txt" (nick ++ " - " ++ nome ++ " " ++ sobrenome ++ " - Gênero preferido: " ++ genero ++ "\n")
    printEspaco
    
    
-- //////////////////////////////////////  AUXILIARES  //////////////////////////////////////

toStringFilme :: Filme -> String
toStringFilme (Filme {indiceFilme = indice, nomeFilme = nome, notaFilme = nota, elencoFilme = elenco, diretorFilme = diretor, roteristaFilme = roterista, generosFilme = genero, premiacoesFilme = premios, dataLacancamento = dataL, sinopseFilme = sinopse}) = show indice++". Nome Do Filme: " ++ nome ++ "  Direção: " ++ diretor ++ "  Nota do Filme: " ++ show nota

infoFilme :: Filme -> String 
infoFilme (Filme {indiceFilme = indice, nomeFilme = nome, notaFilme = nota, elencoFilme = elenco, diretorFilme = diretor, roteristaFilme = roterista, generosFilme = genero, premiacoesFilme = premios, dataLacancamento = dataL, sinopseFilme = sinopse}) = "Nome : " ++ nome ++ ['\n'] ++ "Nota do Filme: " ++ show nota ++ ['\n'] ++ "Elenco: " ++ show elenco ++ ['\n']++ "Diretor: " ++ diretor ++ ['\n'] ++ "Roteirista: " ++ roterista ++ ['\n']++ "Generos: " ++ show genero++ ['\n'] ++ "Premiações: " ++ show premios ++ ['\n']++ "Data de Lançamento: " ++ dataL ++ ['\n'] ++ "Sinopse: " ++ sinopse


listarFilmes :: [Filme] -> String
listarFilmes [] = ""
listarFilmes (x:xs) = toStringFilme x ++ ['\n'] ++ listarFilmes xs


ehDoGenero:: Filme -> String -> Bool
ehDoGenero (Filme {indiceFilme = indice, nomeFilme = nome, notaFilme = nota, elencoFilme = elenco, diretorFilme = diretor, roteristaFilme = roteista, generosFilme = genero, premiacoesFilme = premios, dataLacancamento = dataL, sinopseFilme = sinopse}) gen = elem gen genero 

ehDoElenco :: Filme -> String -> Bool
ehDoElenco (Filme {indiceFilme = indice, nomeFilme = nome, notaFilme = nota, elencoFilme = elenco, diretorFilme = diretor, roteristaFilme = roteista, generosFilme = genero, premiacoesFilme = premios, dataLacancamento = dataL, sinopseFilme = sinopse}) ator = elem ator elenco 

ehDiretor :: Filme -> String -> Bool
ehDiretor(Filme {indiceFilme = indice, nomeFilme = nome, notaFilme = nota, elencoFilme = elenco, diretorFilme = diretor, roteristaFilme = roteista, generosFilme = genero, premiacoesFilme = premios, dataLacancamento = dataL, sinopseFilme = sinopse}) dire = if diretor == dire then True else False

foiPremiado:: Filme -> String -> Bool
foiPremiado (Filme {indiceFilme = indice, nomeFilme = nome, notaFilme = nota, elencoFilme = elenco, diretorFilme = diretor, roteristaFilme = roteista, generosFilme = genero, premiacoesFilme = premios, dataLacancamento = dataL, sinopseFilme = sinopse}) premio = elem premio premios

ehRoterista :: Filme -> String -> Bool
ehRoterista(Filme {indiceFilme = indice, nomeFilme = nome, notaFilme = nota, elencoFilme = elenco, diretorFilme = diretor, roteristaFilme = roterista, generosFilme = genero, premiacoesFilme = premios, dataLacancamento = dataL, sinopseFilme = sinopse}) rote = if roterista == rote then True else False


listarFilmesPorGenero:: [Filme] -> [String] -> String -> [String]
listarFilmesPorGenero [] arrayDoGenero _ = arrayDoGenero
listarFilmesPorGenero (cabeca:cauda) arrayDoGenero genero
    | ehDoGenero cabeca genero = arrayDoGenero ++ [toStringFilme cabeca] ++ listarFilmesPorGenero cauda arrayDoGenero genero
    | otherwise = listarFilmesPorGenero cauda arrayDoGenero genero

listarFilmesPorElenco:: [Filme] -> [String] -> String -> [String]
listarFilmesPorElenco [] arrayDoElenco _ = arrayDoElenco
listarFilmesPorElenco (cabeca:cauda) arrayDoElenco elenco
    | ehDoElenco cabeca elenco = arrayDoElenco ++ [toStringFilme cabeca] ++ listarFilmesPorElenco cauda arrayDoElenco elenco
    | otherwise = listarFilmesPorElenco cauda arrayDoElenco elenco

listarFilmesPorDiretor:: [Filme] -> [String] -> String -> [String]
listarFilmesPorDiretor [] arrayDoDiretor _ = arrayDoDiretor
listarFilmesPorDiretor (cabeca:cauda) arrayDoDiretor diretor
    | ehDiretor cabeca diretor = arrayDoDiretor ++ [toStringFilme cabeca] ++ listarFilmesPorDiretor cauda arrayDoDiretor diretor
    | otherwise = listarFilmesPorDiretor cauda arrayDoDiretor diretor

listarFilmesPorPremio:: [Filme] -> [String] -> String -> [String]
listarFilmesPorPremio [] arrayDoPremio _ = arrayDoPremio
listarFilmesPorPremio (cabeca:cauda) arrayDoPremio premio
    | foiPremiado cabeca premio = arrayDoPremio ++ [toStringFilme cabeca] ++ listarFilmesPorPremio cauda arrayDoPremio premio
    | otherwise = listarFilmesPorPremio cauda arrayDoPremio premio

listarFilmesPorRoterista:: [Filme] -> [String] -> String -> [String]
listarFilmesPorRoterista [] arrayDoRoterista _ = arrayDoRoterista
listarFilmesPorRoterista (cabeca:cauda) arrayDoRoterista roterista
    | ehRoterista cabeca roterista = arrayDoRoterista ++ [toStringFilme cabeca] ++ listarFilmesPorRoterista cauda arrayDoRoterista roterista
    | otherwise = listarFilmesPorRoterista cauda arrayDoRoterista roterista


printEspaco :: IO()
printEspaco = putStrLn "\n\n\n"   

visualizarInfoFilme:: IO()
visualizarInfoFilme = do
    putStrLn "==> Escolha o indice do filme que você deseja visualizar:"
    indice <- getLine
    let parseIndice = read (indice)
    let indiceNaLista = parseIndice-1

    if parseIndice > 0 && parseIndice < ( length filmesCadastrados )+1
        then putStrLn ( infoFilme ( filmesCadastrados !!  indiceNaLista  ) ) 
    else putStrLn "Filme não existente."
    

    
listarInteiroParaFilme :: [Integer] -> [Filme]
listarInteiroParaFilme lista = do
    let disponiveis = sort lista
    listarInteiroParaFilme' disponiveis filmesCadastrados
    where
        listarInteiroParaFilme' :: [Integer] -> [Filme] -> [Filme]
        listarInteiroParaFilme' [] _ = []
        listarInteiroParaFilme' (a:as) (b:bs)
            | a == (indiceFilme b) = [b] ++ listarInteiroParaFilme' as bs
            | otherwise = [] ++ listarInteiroParaFilme' (a:as) bs   