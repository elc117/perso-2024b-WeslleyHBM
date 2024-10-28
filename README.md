# 🐍🍎Jogo Da Serpente Em Haskell🍎🐍
## TRABALHO DE PARADIGMAS DA PROGRAMAÇÃO EM HASKELL

### O que era o trabalho:

```texto
Estrutura e conteúdo:

Identificação: nome e curso

Tema/objetivo: descrição do tema/objetivo do trabalho, conforme a proposta validada

Processo de desenvolvimento: comentários sobre o desenvolvimento, incluindo fontes consultadas e versões com erros

Resultado final: demonstração de execução em GIF animado ou vídeo curto (máximo 60s)

Referências e créditos (incluindo algumas solicitações, se aplicável)
```

## Indentificação
  - `Nome`: Weslley Henrique De Bitencourt Menezes;
  - `Curso`: Sistemas de Informação | UFSM.

## Descrição
Para o meu trabalho optei por utilizar da biblioteca CodeWorld e o espaço que suporta esta biblioteca, organizei estudos de algumas funções para está biblioteca para inplementar um código simples do jogo clássico da cobrinha, tive muitas dificuldades, as quais estarão brevemente descritas em um dos campos a baixo. A partir de meus estudo cheguei em uma versão brevemente finalizada, porem funcional do codigo, como descrito na solicitação de iniciativa utilizei das ferramentas da biblioteca para criar algo "novo" e do zero ao contrario de ficar em circulos apenas analisando o cófigo da professora.

## Inspiração para o trabalho
Utilizei de inpiração um dos projetos antigos de lab 2 porem em lab 2 ele era efetuado em C e fazia uso de listas sirculares então enquanto pesquisava vi que dava para fazer algo assim com o codeworld e me interecei.

## CodeWorld
  - [CodeWorld](https://code.world/haskell#): Link para o site que compila esta biblioteca.

## Explicação do código
### 1. Extensão do Haskell e Importações

```haskell

{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import System.Random (StdGen, mkStdGen, randomR, randomIO)
```

  - `{-# LANGUAGE OverloadedStrings #-}`: Ativa a extensão `OverloadedStrings`, permitindo que strings sejam tratadas de maneira mais flexível.
  - `import CodeWorld`: Importa a biblioteca CodeWorld, que fornece funções para criar gráficos e interatividade.
  - `import System.Random`: Importa funções para trabalhar com geração de números aleatórios, essenciais para posicionar o alimento da cobra de forma aleatória.

### 2. Definição de Tipos

```haskell

type Coordenada = (Int, Int)
```

  - `Coordenada`: Um tipo que representa a posição no grid do jogo, armazenando pares de inteiros (x, y).

### 3. Estrutura do Jogo

```haskell

data JogoCobra = JogoCobra {
    cobra :: [Coordenada],
    alimento :: Coordenada,
    direcao :: Coordenada,
    fimDeJogo :: Bool,
    contagemTicks :: Float,
    intervaloMovimento :: Float,
    gerador :: StdGen 
}
```
  - `JogoCobra`: Uma estrutura que contém todos os dados necessários para o jogo da cobrinha.
  - `cobra`: Lista de coordenadas que representa o corpo da cobra.
  - `alimento`: Posição atual do alimento.
  - `direcao`: Direção atual da cobra, representada por um par de inteiros.
  - `fimDeJogo`: Um booleano que indica se o jogo terminou.
  - `contagemTicks`: Um contador usado para controlar a velocidade da cobra.
  - `intervaloMovimento`: Intervalo de movimento da cobra, controlando a rapidez com que ela se move.
  - `gerador`: Gerador de números aleatórios para posicionar o alimento.

### 4. Função Principal

```haskell

main :: IO ()
main = do
    gen <- randomIO :: IO Int
    let initialGen = mkStdGen gen
    activityOf (jogoInicial initialGen) atualizarJogo desenharJogo
```
  - `main`: Função que inicializa o jogo.
  - `randomIO`: Gera um número aleatório.
  - `mkStdGen`: Cria um gerador de números aleatórios com base na semente gerada.
  - `activityOf`: Inicia a atividade do jogo, configurando o estado inicial, a função de atualização e a função de desenho.

### 5. Estado Inicial do Jogo

```haskell

jogoInicial :: StdGen -> JogoCobra
jogoInicial gen = JogoCobra {
    cobra = [(0, 0)],
    alimento = fst (gerarAlimentoAleatorio gen [(0, 0)]),
    direcao = (1, 0),
    fimDeJogo = False,
    contagemTicks = 0,
    intervaloMovimento = 10.0,
    gerador = gen
}
```
  - `jogoInicial`: Define o estado inicial do jogo.
  - A cobra começa na posição (0, 0).
  - O alimento é gerado aleatoriamente, garantindo que não comece na mesma posição da cobra.
  - A direção inicial da cobra é para a direita (1, 0).

### 6. Atualização do Jogo

```haskell

atualizarJogo :: Event -> JogoCobra -> JogoCobra
```

Essa função atualiza o estado do jogo com base em eventos (como teclas pressionadas ou passagem de tempo).
**Movimentação da Cobra**

```haskell

atualizarJogo (KeyPress "Up") jogo = jogo { direcao = (0, 1) }
atualizarJogo (KeyPress "Down") jogo = jogo { direcao = (0, -1) }
atualizarJogo (KeyPress "Left") jogo = jogo { direcao = (-1, 0) }
atualizarJogo (KeyPress "Right") jogo = jogo { direcao = (1, 0) }
```

  - Estas linhas alteram a direção da cobra com base nas teclas pressionadas.

**Lógica de Atualização**

```haskell

atualizarJogo (TimePassing _) jogo
    | fimDeJogo jogo = jogo
    | otherwise = 
        let novaContagemTicks = contagemTicks jogo + 1
            intervalo = intervaloMovimento jogo
        in if novaContagemTicks >= intervalo
            then let novaCobra = moverCobra (cobra jogo) (direcao jogo) (alimento jogo)
                     novoFimDeJogo = verificarColisao novaCobra
                     (novoAlimento, novoGen) = if head novaCobra == alimento jogo
                                    then gerarAlimentoAleatorio (gerador jogo) novaCobra
                                    else (alimento jogo, gerador jogo)
                     novoIntervalo = if head novaCobra == alimento jogo
                                    then max 2 (intervaloMovimento jogo - 0.5)
                                    else intervaloMovimento jogo
                 in jogo { cobra = novaCobra, alimento = novoAlimento, fimDeJogo = novoFimDeJogo, contagemTicks = 0, intervaloMovimento = novoIntervalo, gerador = novoGen }
            else jogo { contagemTicks = novaContagemTicks }
atualizarJogo _ jogo = jogo
```

  - Esta parte verifica se o jogo terminou e, caso contrário, atualiza a posição da cobra com base na direção, gera um novo alimento se a cobra o comer, e ajusta a velocidade da cobra.
  - Nesta parte me enbananei muito mas aos poucos pude encontrar meus erros

### 7. Movimentação e Colisões
**Movimentar a Cobra**

```haskell

moverCobra :: [Coordenada] -> Coordenada -> Coordenada -> [Coordenada]
moverCobra ((x, y):xs) (dx, dy) alimento
    | (x + dx, y + dy) == alimento = (x + dx, y + dy) : (x, y) : xs
    | otherwise = (x + dx, y + dy) : init ((x, y):xs)
moverCobra [] _ _ = []
```

  - `moverCobra`: Atualiza a posição da cobra.
  - Se a cobra atingir o alimento, aumenta o corpo da cobra.
  - Caso contrário, a cobra se move, descartando a última parte do corpo.

**Verificação de Colisões**

```haskell

verificarColisao :: [Coordenada] -> Bool
verificarColisao ((x, y):xs) = x < -10 || x > 10 || y < -10 || y > 10 || (x, y) `elem` xs
verificarColisao _ = False
```
  - `verificarColisao`: Verifica se a cobra colidiu com as bordas ou consigo mesma.

### 8. Geração de Alimento

```haskell

gerarAlimentoAleatorio :: StdGen -> [Coordenada] -> (Coordenada, StdGen)
gerarAlimentoAleatorio gen corpoCobra = 
    let (x, novoGen) = randomR (-10, 10) gen
        (y, genFinal) = randomR (-10, 10) novoGen
    in if (x, y) `elem` corpoCobra 
       then gerarAlimentoAleatorio genFinal corpoCobra
       else ((x, y), genFinal)
```

  - `gerarAlimentoAleatorio`: Gera uma nova posição aleatória para o alimento, garantindo que não esteja em uma posição ocupada pela cobra.

### 9. Desenho do Jogo
Desenho do Estado Atual

```haskell

desenharJogo :: JogoCobra -> Picture
desenharJogo (JogoCobra cobra alimento _ fimDeJogo _ _)
    | fimDeJogo = scaled 3 3 (lettering "Fim de Jogo")
    | otherwise = pictures [desenharCobra cobra, desenharAlimento alimento, desenharBordas]
```

  - `desenharJogo`: Retorna a representação gráfica do estado atual do jogo, desenhando a cobra, o alimento e as bordas.

**Desenho da Cobra**

```haskell

desenharCobra :: [Coordenada] -> Picture
desenharCobra = pictures . map desenharSegmento
  where
    desenharSegmento (x, y) = translated (fromIntegral x) (fromIntegral y) (colored green (solidRectangle 1 1))
```

  - `desenharCobra`: Desenha cada segmento do corpo da cobra na cor verde.

**Desenho do Alimento**

```haskell

desenharAlimento :: Coordenada -> Picture
desenharAlimento (x, y) = translated (fromIntegral x) (fromIntegral y) (colored red (solidCircle 0.5))
```

  - `desenharAlimento`: Desenha o alimento na cor vermelha para que se pareça uma "maça".

**Desenho das Bordas**

```haskell

desenharBordas :: Picture
desenharBordas = colored black (solidRectangle 22 22) <> colored white (solidRectangle 20 20)
```

  - `desenharBordas`: Desenha as bordas do campo de jogo.

## Principais Problemas que tive

  - **Interpretação**: Tive muito problema na parte de interpretar a linguagem e perceber os pontos em que a logica se difere da logica de programação que eu conheço.
  - **Almentar a serpente**: de começo fiz taapenas um quadrado verde que se movimentava pelo mapa e quando fui modificar a logica, fiquei algumas horas pensando onde estava errando, até perceber, que eu estava sempre apagando a posição anterior da cobra mesmo quando ela comia uma "fruta".
  - **Controle de velocidade**: demorei um tempo para pensar em um esquema de velocidade funcional pois de inicio ou ele começava muito rapido ou tinha um hiper pico de velocidade e no fim convertir tudo para float e diminui a distancia em frações pequenas.
  - **Implementação mais funcional e bonita**: Atualmente eu tenho dois códigos, sendo um deles o que estou entregando como meu trabalho, mas ele se encaixa mais como um prototipo do que uma versão final e tambem tenho uma versão um pouco mais avançada que tambem anexarei aqui, mas que consta com multiplos erros de implementação que não consegui corrigir.

## Conclusão

Este código foi bom para estudar algumas ferramentas da linguagem por mais que eu não tenha ficado 100% satifeito com o resultado final, ao menos vejo como uma vantagem ter gerado um código que ao menos compila e cumpre com a função que lhe foi encarregada, mas uma coisa que ficou bem aparente é que, não sei Haskell porem sinto que após a implementação deste código me senti um compo mais conhecedor desta linguagem que abranje tantos campos.

## Bibliografia

  - O proprio guia do [codeWorld](https://code.world/haskell#), onde estudei funções e funcionamentos;
  - O [chat GPT](https://chatgpt.com/), para tradução e exclarecimento de erros de compilação e de funcionalidade de atalhos;
  - e um [site de Haskell](https://hackage.haskell.org/package/codeworld-api) que explicava algumas coisas,(usei esta pouco, pois não entendi muito).
