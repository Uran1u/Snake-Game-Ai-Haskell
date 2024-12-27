
module Main where
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import qualified System.Random as R

main :: IO ()
main = do
    seed <- R.randomIO
    let world = initialWorld seed
    play
        (displayMode world)
        backgroundColor
        stepRate
        world
        drawWorld
        handleEvent
        handleStep

displayMode :: World -> Display
displayMode world = InWindow "Snake" (resolution world) (0, 0)


backgroundColor :: Color
backgroundColor = black


drawWorld :: World -> Picture
drawWorld world = pictures
    [ drawBounds world
    , drawSnake world
    , drawGameOver world
    , drawApples world
    ]
drawSnake :: World -> Picture
drawSnake world = pictures (map (\ p-> color green (drawbox p world)) (snake world))



drawbox :: (Int, Int) -> World -> Picture 
drawbox (x, y) world = 
    let s = size world / fromIntegral ( scale1 world)
        x' = s * fromIntegral x
        y' = s * fromIntegral y
    in translate x' y' (rectangleSolid s s)

drawApples :: World -> Picture 
drawApples world = color red (drawbox( apple world) world) 

drawBounds :: World -> Picture
drawBounds world =
    let x = size world 
    in color white (rectangleWire x x) 

size :: (Num a) => World -> a
size world = 
    let (wigth, height) = resolution world
    in fromIntegral (min wigth height)


stepRate :: Int
stepRate = 5


initialWorld :: Int -> World
initialWorld seed = NewWorld
    {resolution = (512, 512)
    ,direction = North
    ,scale1 = 10
    ,snake = [(0, 2), (0, 1), (0, 0)]
    ,gameover = False
    ,gen = R.mkStdGen seed
    ,apple = (5, -3)
    }





handleEvent :: Event -> World -> World
handleEvent event world = case event of 
    EventResize newResolution -> handleResize newResolution world
    EventKey key state _ _ -> if gameover world 
        then world
        else handleKey key state world 
        
    _ -> world


handleResize :: (Int, Int) -> World -> World 
handleResize newResplution world = world {resolution = newResplution}

handleStep :: Float -> World -> World
handleStep _time world =
    if gameover world
    then world
    else
        let oldSnake = snake world
            newSnake@((x, y) : _) = init oldSnake
            (x', y') = case direction world of
                North -> (x, y + 1)
                East -> (x + 1, y)
                South -> (x, y - 1)
                West -> (x - 1, y)
        in  if inBounds world (x', y') && not (valsnake world (x', y'))
            then if valfood world (x', y')
                 then 
                        let world' = moveFood world
                        in world' {snake = (x', y'): oldSnake }
                 else world { snake = (x', y') : newSnake }
            else world { gameover = True }

moveFood :: World -> World 
moveFood world = 
    let g0 = gen world 
        a = scale1 world `div` 2
        (x, g1) = R.randomR (-a, a) g0
        (y, g2) = R.randomR (-a, a) g1
    in  if valsnake world (x,y)
        then moveFood world { gen = g2 }
        else world { gen = g2 , apple = (x, y) }

drawGameOver :: World -> Picture
drawGameOver world = if gameover world
    then pictures 
        [
        (translate (-120) 0 (color red (scale 0.4 0.4 (text "GameOver!"))))
        , color white (translate (-50) (-20) (scale 0.2 0.2 (text ("score :"++ show ((length (snake world))-3)))))
        
        ]
    else blank

    
inBounds :: World -> (Int, Int) -> Bool
inBounds world (x, y) =
    let s = scale1 world `div` 2
    in  -s <= x && x <= s && -s <= y && y <= s

valsnake :: World -> (Int, Int) -> Bool 
valsnake world (x, y) = any ( == (x, y)) (snake world)

valfood :: World -> (Int, Int) -> Bool 
valfood world (x, y) = (x, y) == apple world

handleKey :: Key -> KeyState -> World -> World
handleKey key Down world = case key of
    Char 'w' -> world { direction = if direction world == South then South else North}
    Char 's' -> world { direction = if direction world == North then North else South}
    Char 'd' -> world { direction = if direction world == West then West else East }
    Char 'a' -> world { direction = if direction world == East then East else West }
    _ -> world
handleKey _ _ world = world

--
data World = NewWorld
    {resolution :: (Int,Int)
    ,direction :: Direction
    ,scale1 :: Int
    ,snake :: [(Int, Int)]
    ,gameover :: Bool
    , gen :: R.StdGen
    ,apple :: (Int , Int)
    } deriving ( Show)


data Direction 
    = North
    |East
    |South
    |West
    deriving ( Bounded, Enum, Eq,Ord, Read, Show)