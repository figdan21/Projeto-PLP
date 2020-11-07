
data Filme = Filme {nomeFilme :: String,
		notaFilme :: Float,
		elencoFilme :: [String],
		diretorFilme :: String,
		roteristaFilme :: String,
		temasFilme :: [String],
		premiacoesFilme :: [String],
		dataLacancamento :: String,
		sinopseFilme :: String
		} deriving (Show)

data Usuario = Usuario { login :: String,
			senha :: String, 
			filmesRecomendados ::[(Usuario,Filme)],
			filmesParaAssistir :: [Filme],
			filmesAssistidos :: [Filme],
			seguidores :: [Usuario],
			seguindo :: [Usuario]
			} deriving (Show)



