
module Main where
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import System.Random (mkStdGen)

main :: IO ()
main = play
    displayMode
    backgroundColor
    stepRate
    initialWorld
    drawWorld
    handleEvent
    handleStep

displayMode :: World -> Display
displayMode word = InWindow "Snake" (resolution world) (0, 0)


backgroundColor :: Color
backgroundColor = black


stepRate :: Int
stepRate = 1


initialWorld :: World
initialWorld = NewWorld
    {resolution :: (640, 480)
    ,
    }


drawWorld :: World -> Picture
drawWorld world = blank


handleEvent :: Event -> World -> World
handleEvent event world = world


handleStep :: Float -> World -> World
handleStep time world = world


--
data World = NewWorld
    {resolution :: (Int,Int)
    ,
    } deriving (Eq, Ord, Read, Show)