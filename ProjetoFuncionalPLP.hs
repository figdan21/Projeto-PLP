import Data.List
import System.Directory

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
                     | opcao == 4 = do {visualizarRecomendacoes; menuOpcao}  
                     | otherwise =  do {putStrLn "Opcao invalida, Porfavor escolha uma opcao valida" ; menuOpcao}

-- //////////////////////////////////////  PRINTS  //////////////////////////////////////

imprimeFilmes :: IO()
imprimeFilmes = putStrLn ("\n\n\n" ++ (listarFilmes filmesCadastrados) ++ "\n\n")

   
visualizarRecomendacoes :: IO()
visualizarRecomendacoes = do
    printEspaco
    putStrLn "==> Suas recomendações de Filmes:\n"
    recomendacoes <- readFile "recomendacoes.txt"
    putStrLn recomendacoes
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

-- //////////////////////////////////////  OPERAÇÕES  //////////////////////////////////////


enviarRecomendacao :: IO()
enviarRecomendacao = do
    printEspaco
    putStr "==> Digite o nome do Filme que deseja recomendar: "
    nomeFilme <- getLine
    appendFile "recomendacoes.txt" (nomeFilme ++ "\n")
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
ehDoGenero (Filme {nomeFilme = nome, notaFilme = nota, elencoFilme = elenco, diretorFilme = diretor, roteristaFilme = roteista, generosFilme = genero, premiacoesFilme = premios, dataLacancamento = dataL, sinopseFilme = sinopse}) gen = elem gen genero 


listarFilmesPorGenero:: [Filme] -> [String] -> String -> [String]
listarFilmesPorGenero [] arrayDoGenero _ = arrayDoGenero
listarFilmesPorGenero (cabeca:cauda) arrayDoGenero genero
	| ehDoGenero cabeca genero = arrayDoGenero ++ [toStringFilme cabeca] ++ listarFilmesPorGenero cauda arrayDoGenero genero
	| otherwise = listarFilmesPorGenero cauda arrayDoGenero genero



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

