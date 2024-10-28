{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import System.Random (StdGen, mkStdGen, randomR, randomIO)
import qualified Data.Text as T  -- Importar o módulo Text

-- Tipo para representar a posição da cobra e o alimento
type Coordenada = (Int, Int)

data EstadoJogo = TelaInicial | Jogando | GameOver deriving (Eq)

-- Estrutura do jogo
data JogoCobra = JogoCobra {
    cobra :: [Coordenada],    -- Corpo da cobra (lista de pontos)
    alimento :: Coordenada,    -- Posição do alimento
    direcao :: Coordenada,     -- Direção atual da cobra
    fimDeJogo :: Bool,         -- Estado do jogo (fim de jogo ou não)
    contagemTicks :: Float,      -- Contador de ticks para controlar a velocidade
    intervaloMovimento :: Float,
    pontuacao :: Int,
    gerador :: StdGen,
    estado :: EstadoJogo
}

-- Função principal
main :: IO ()
main = do
    gen <- randomIO :: IO Int
    let initialGen = mkStdGen gen
    activityOf (jogoInicial initialGen) atualizarJogo desenharJogo
    
-- Estado inicial do jogo
jogoInicial :: StdGen -> JogoCobra
jogoInicial gen = JogoCobra {
    cobra = [(0, 0)],                 
    alimento = fst (gerarAlimentoAleatorio gen [(0, 0)]),
    direcao = (1, 0),                
    fimDeJogo = False,                
    contagemTicks = 0,   
    intervaloMovimento = 10.0,
    pontuacao = 0,
    gerador = gen,
    estado = TelaInicial
}

-- Atualiza o estado do jogo com base em eventos
atualizarJogo :: Event -> JogoCobra -> JogoCobra
atualizarJogo (KeyPress "Enter") jogo
    | estado jogo == TelaInicial = jogo { estado = Jogando }
    | estado jogo == GameOver = jogoInicial (mkStdGen 0)
atualizarJogo (KeyPress "Up") jogo = jogo { direcao = (0, 1) }
atualizarJogo (KeyPress "Down") jogo = jogo { direcao = (0, -1) }
atualizarJogo (KeyPress "Left") jogo = jogo { direcao = (-1, 0) }
atualizarJogo (KeyPress "Right") jogo = jogo { direcao = (1, 0) }
atualizarJogo (KeyPress "w") jogo = jogo { direcao = (0, 1) }
atualizarJogo (KeyPress "s") jogo = jogo { direcao = (0, -1) }
atualizarJogo (KeyPress "a") jogo = jogo { direcao = (-1, 0) }
atualizarJogo (KeyPress "d") jogo = jogo { direcao = (1, 0) }
atualizarJogo (TimePassing _) jogo
    | fimDeJogo jogo = jogo
    | otherwise = 
        let novaContagemTicks = contagemTicks jogo + 1
            intervalo = intervaloMovimento jogo
            novaPontucao = pontuacao jogo
        in if novaContagemTicks >= intervalo
            then let novaCobra = moverCobra (cobra jogo) (direcao jogo) (alimento jogo)
                     novoFimDeJogo = verificarColisao novaCobra
                     (novoAlimento, novoGen) = if head novaCobra == alimento jogo
                                    then gerarAlimentoAleatorio (gerador jogo) novaCobra
                                    else (alimento jogo, gerador jogo)
                     novoIntervalo = if head novaCobra == alimento jogo
                                    then max 2 (intervaloMovimento jogo - 0.5)
                                    else intervaloMovimento jogo
                     novaPontuacao = if head novaCobra == alimento jogo
                                    then pontuacao jogo + 1
                                    else pontuacao jogo
                 in jogo { cobra = novaCobra, alimento = novoAlimento, fimDeJogo = novoFimDeJogo, contagemTicks = 0, intervaloMovimento = novoIntervalo,pontuacao = novaPontuacao, gerador = novoGen }
            else jogo { contagemTicks = novaContagemTicks }
atualizarJogo _ jogo = jogo

-- Movimenta a cobra na direção atual
moverCobra :: [Coordenada] -> Coordenada -> Coordenada -> [Coordenada]
moverCobra ((x, y):xs) (dx, dy) alimento
    | (x + dx, y + dy) == alimento = (x + dx, y + dy) : (x, y) : xs
    | otherwise = (x + dx, y + dy) : init ((x, y):xs)
moverCobra [] _ _ = []

-- Verifica se houve colisão (com as bordas ou consigo mesma)
verificarColisao :: [Coordenada] -> Bool
verificarColisao ((x, y):xs) = x < -10 || x > 10 || y < -10 || y > 10 || (x, y) `elem` xs
verificarColisao _ = False

-- Gera uma nova posição de alimento aleatória
gerarAlimentoAleatorio :: StdGen -> [Coordenada] -> (Coordenada, StdGen)
gerarAlimentoAleatorio gen corpoCobra = 
    let (x, novoGen) = randomR (-10, 10) gen
        (y, genFinal) = randomR (-10, 10) novoGen
    in if (x, y) `elem` corpoCobra 
       then gerarAlimentoAleatorio genFinal corpoCobra
       else ((x, y), genFinal)

-- Desenha o estado atual do jogo
desenharJogo :: JogoCobra -> Picture
desenharJogo jogo = case estado jogo of
    TelaInicial -> telaInicial
    Jogando -> desenharPartida jogo
    GameOver -> telaGameOver (pontuacao jogo)
  
-- Desenha tela incial
telaInicial :: Picture
telaInicial = pictures [
      scaled 2 2 (lettering "Jogo da Cobrinha"),
      translated 0 (-4) (lettering "Pressione Enter para começar")
    ]
    
-- Desenhar tela de game Over 
telaGameOver :: Int -> Picture
telaGameOver pontuacao = pictures [
      scaled 2 2 (lettering "fim de jogo"),
      translated 0 (-3) (lettering (T.pack ("pontuação" ++ show pontuacao))),
      translated 0 (-5) (lettering "Pressione Enter para tentar novamente")
    ]
    
--desenhar jogo em andamento
desenharPartida :: JogoCobra -> Picture
desenharPartida jogo = pictures [
      desenharCobra (cobra jogo),
      desenharAlimento (alimento jogo),
      desenharBordas,
      desenharPontuacao (pontuacao jogo)
    ]

-- Desenha a cobra
desenharCobra :: [Coordenada] -> Picture
desenharCobra = pictures . map desenharSegmento
  where
    desenharSegmento (x, y) = translated (fromIntegral x) (fromIntegral y) (colored green (solidRectangle 1 1))

-- Desenha o alimento
desenharAlimento :: Coordenada -> Picture
desenharAlimento (x, y) = translated (fromIntegral x) (fromIntegral y) (colored red (solidCircle 0.5))

-- Desenha as bordas do campo
desenharBordas :: Picture
desenharBordas = colored white (solidRectangle 22 22) <> colored black (solidRectangle 20 20)

-- Desenha a pontuação
desenharPontuacao :: Int -> Picture
desenharPontuacao pontuacao = colored white ( translated 0 (-4) (lettering (T.pack ("pontuação" ++ show pontuacao))))
