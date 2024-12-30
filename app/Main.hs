
module Main where
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import qualified System.Random as R
import Debug.Trace (trace)
import Data.List (minimumBy)
import Data.Function (on)

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
drawSnake world = case snake world of
    (p : ps) -> pictures
        ( color orange (drawbox p world)
        : map (\ x -> color green (drawbox x world)) ps
        )
    _ -> blank

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
stepRate = 20


initialWorld :: Int -> World
initialWorld seed = NewWorld
    {resolution = (512, 512)
    ,direction = North
    ,scale1 = 10
    ,snake = [(0, 2), (0, 1), (0, 0)]
    ,gameover = False
    ,gen = R.mkStdGen seed
    ,apple = (0, 3)
    ,restart = False
    }





handleEvent :: Event -> World -> World
handleEvent event world = case event of 
    EventResize newResolution -> handleResize newResolution world
    _ ->
        if gameover world 
        then let newSeed = fst (R.next (gen world))  -- Generate a new seed from the current generator
                 newWorld = initialWorld newSeed  -- Create a new world using the new seed
             in  newWorld
            
        else handleAi world
        
    _ -> world

newSeed :: IO Int
newSeed = R.randomIO

handleAi :: World -> World 
handleAi world =
    let aiDir = aiMove world
    in trace ("AI Direction: " ++ show aiDir) world { direction = aiDir }

handleStep :: Float -> World -> World
handleStep _time world =
    if gameover world
    then world
    else
        let aiWorld = handleAi world  -- AI decides the next move
        in -- Move the snake based on the updated direction
           let oldSnake = snake aiWorld
               newSnake@((x, y) : _) = init oldSnake
               (x', y') = case direction aiWorld of
                   North -> (x, y + 1)
                   East  -> (x + 1, y)
                   South -> (x, y - 1)
                   West  -> (x - 1, y)
           in if inBounds aiWorld (x', y') && not (valsnake aiWorld (x', y'))
              then if valfood aiWorld (x', y')
                   then let world' = moveFood aiWorld
                        in world' { snake = (x', y') : oldSnake }
                   else aiWorld { snake = (x', y') : newSnake }
              else aiWorld { gameover = True }

handleResize :: (Int, Int) -> World -> World 
handleResize newResplution world = world {resolution = newResplution}



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

--Ai movements 
aiMove :: World -> Direction
aiMove world =
    let (sx, sy) = head (snake world)  -- Snake's current head position
        (fx, fy) = apple world         -- Food position
        body = tail ( snake world)
        isValid (x, y) = inBounds world (x, y) && notElem (x, y) body


        possibleMoves = [(North,(sx, sy + 1 )),
                         (East, (sx + 1, sy )),
                         (South,(sx ,sy - 1 )),
                         (West, (sx -1, sy ))]


        validMoves = filter (isValid . snd) possibleMoves
        --fbestmove
        bestMove = if null validMoves
                     then (direction world) -- Normal 
                     else fst $ minimumBy ( compare `on` distaciaDaApple . snd ) validMoves
        --calcula distancia 
        distaciaDaApple (x, y) = abs ( fx -x) + abs (fy - y) 


    in bestMove
    
-- if you want to control the snake you can  just need to change the handle event and change to the "aiWorld"
handleKey :: Key -> KeyState -> World -> World
handleKey key Down world = case key of
    Char 'w' -> world { direction = if direction world == South then South else North}
    Char 's' -> world { direction = if direction world == North then North else South}
    Char 'd' -> world { direction = if direction world == West then West else East }
    Char 'a' -> world { direction = if direction world == East then East else West }
    Char 'r' -> world { restart = if restart world == False then True else False }
handleKey _ _ world = world

generateNewSeed :: IO Int
generateNewSeed = R.randomIO

--
data World = NewWorld
    {resolution :: (Int,Int)
    ,direction :: Direction
    ,scale1 :: Int
    ,snake :: [(Int, Int)]
    ,gameover :: Bool
    ,gen :: R.StdGen
    ,apple :: (Int , Int)
    ,restart :: Bool
    } deriving ( Show)


data Direction 
    = North
    |East
    |South
    |West
    deriving ( Bounded, Enum, Eq,Ord, Read, Show)