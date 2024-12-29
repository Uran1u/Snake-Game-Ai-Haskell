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