module Data 
    ( Posicao, Velocidade, Tempo,
      Cobra(..), Pontos(..), GameState(..), Direcao(..)
    ) where

import System.Random (mkStdGen, randomR)

type Posicao = (Double, Double)
type Velocidade = (Double, Double)
type Tempo = Double

data Cobra = Cobra 
    { posicoes :: [Posicao]       -- Corpo da cobra (lista de posições)
    , velocidade :: Velocidade    -- Velocidade da cobra
    } deriving (Show, Eq)

data Direcao 
    = Up
    | Down
    | Left
    | Right
    deriving (Eq, Read, Show)
    
data Pontos = Pontos
    { posAleatoria :: Posicao     -- Posição atual da comida
    , apanhado :: Bool            -- Indica se foi apanhado
    } deriving (Show, Eq)

data GameState = GameState
    { cobra :: Cobra              -- Estado da cobra
    , pontos :: Pontos            -- Estado dos pontos
    , direcao :: Direcao          -- Direção atual da cobra
    , mapaDimensao :: (Double, Double) -- Dimensões do mapa
    } deriving (Show, Eq)
