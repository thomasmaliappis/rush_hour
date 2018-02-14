-- ["..abbb","c.adee","c==d..",".g.dhh","igjj.k","illl.k"]
-- data type State
data State = State [String] Int Int deriving (Show)
instance Eq State where
        (State string1 rows1 columns1) == (State string2 rows2 columns2) = string1 == string2 && rows1 == rows2 && columns1 == columns2

-- data type Move
data Move = Move Char Char Int deriving (Show)

-- readState
readState :: String -> State
readState xs = State y (rows y) (columns y)
        where
        y = convert (map (\x -> [x]) xs)

-- convert
convert [] = []
convert [x] = [x]
convert (x:y:xs) = if y == ['\n']
        then x:convert xs
        else convert ((x++y):xs)

-- rows
rows y = length y

-- columns
columns (y:ys) = length y

-- writeState
writeState :: State -> String
writeState (State [x] r c) = x
writeState (State (x:xs) r c) = x++['\n']++(writeState (State xs r c))

-- successorMoves
successorMoves :: State -> [(Move,Int)]
successorMoves (State matrix r c) = successorLoop (State matrix r c) 0 0

-- successorLoop
successorLoop (State matrix r c) x y =
        if c == y
                then successorLoop (State matrix r c) (x+1) 0
        else if r == x
                then []
        else if car_horizontal (State matrix r c) x y
                then (car_horizontal_moves (State matrix r c) x y)++successorLoop (State matrix r c) x (y+1)
        else if car_vertical (State matrix r c) x y
                then (car_vertical_moves (State matrix r c) x y)++successorLoop (State matrix r c) x (y+1)
        else if truck_horizontal (State matrix r c) x y
                then (truck_horizontal_moves (State matrix r c) x y)++successorLoop (State matrix r c) x (y+1)
        else if truck_vertical (State matrix r c) x y
                then (truck_vertical_moves (State matrix r c) x y)++successorLoop (State matrix r c) x (y+1)
        else successorLoop (State matrix r c) x (y+1)

-- element
element (State matrix r c) x y = if (x < r) && (x >= 0) && (y < c) && (y >= 0)
        then (matrix !! x) !! y
        else '\n'
-- element (State matrix r c) x y = (matrix !! x) !! y
-- car_horizontal
car_horizontal (State matrix r c) x y = if (y+1 < c) && (element (State matrix r c) x y == element (State matrix r c) x (y+1)) && (element (State matrix r c) x y /= '.')
        then True
        else False
car_horizontal_moves (State matrix r c) x y = car_moves_right (State matrix r c) x y 1 ++ car_moves_left (State matrix r c) x y 1

car_moves_right (State matrix r c) x y n = if (y+2 < c) && (element (State matrix r c) x (y+2) == '.')
        then (Move 'r' (element (State matrix r c) x y) n,1):car_moves_right (State matrix r c) x (y+1) (n+1)
        else []

car_moves_left (State matrix r c) x y n = if (y-1 >= 0) && (element (State matrix r c) x (y-1) == '.')
        then (Move 'l' (element (State matrix r c) x y) n,1):car_moves_left (State matrix r c) x (y-1) (n+1)
        else []

-- car_vertical
car_vertical (State matrix r c) x y = if (x+1 < r) && (element (State matrix r c) x y == element (State matrix r c) (x+1) y) && (element (State matrix r c) x y /= '.')
        then True
        else False

car_vertical_moves (State matrix r c) x y = car_moves_up (State matrix r c) x y 1 ++ car_moves_down (State matrix r c) x y 1

car_moves_up (State matrix r c) x y n = if (x-1 >= 0) && (element (State matrix r c) (x-1) y == '.')
        then (Move 'u' (element (State matrix r c) x y) n,1):car_moves_up (State matrix r c) (x-1) y (n+1)
        else []

car_moves_down (State matrix r c) x y n = if (x+2 < r) && (element (State matrix r c) (x+2) y == '.')
        then (Move 'd' (element (State matrix r c) x y) n,1):car_moves_down (State matrix r c) (x+1) y (n+1)
        else []
-- truck_horizontal
truck_horizontal (State matrix r c) x y = if (y+2 < c) && (element (State matrix r c) x y == element (State matrix r c) x (y+1)) && (element (State matrix r c) x y == element (State matrix r c) x (y+2)) && (element (State matrix r c) x y /= '.')
        then True
        else False
truck_horizontal_moves (State matrix r c) x y = truck_moves_right (State matrix r c) x y 1 ++ truck_moves_left (State matrix r c) x y 1

truck_moves_right (State matrix r c) x y n = if (y+3 < c) && (element (State matrix r c) x (y+3) == '.')
        then (Move 'r' (element (State matrix r c) x y) n,1):truck_moves_right (State matrix r c) x (y+1) (n+1)
        else []

truck_moves_left (State matrix r c) x y n = if (y-1 >= 0) && (element (State matrix r c) x (y-1) == '.')
        then (Move 'l' (element (State matrix r c) x y) n,1):truck_moves_left (State matrix r c) x (y-1) (n+1)
        else []
-- truck_vertical
truck_vertical (State matrix r c) x y = if (x+2 < r) && (element (State matrix r c) x y == element (State matrix r c) (x+1) y) && (element (State matrix r c) x y == element (State matrix r c) (x+2) y) && (element (State matrix r c) x y /= '.')
        then True
        else False
truck_vertical_moves (State matrix r c) x y = truck_moves_up (State matrix r c) x y 1 ++ truck_moves_down (State matrix r c) x y 1

truck_moves_up (State matrix r c) x y n = if (x-1 >= 0) && (element (State matrix r c) (x-1) y == '.')
        then (Move 'u' (element (State matrix r c) x y) n,1):truck_moves_up (State matrix r c) (x-1) y (n+1)
        else []

truck_moves_down (State matrix r c) x y n = if (x+3 < r) && (element (State matrix r c) (x+3) y == '.')
        then (Move 'd' (element (State matrix r c) x y) n,1):truck_moves_down (State matrix r c) (x+1) y (n+1)
        else []

makeMove:: State -> Move -> State
makeMove (State matrix r c) (Move d car n) = vehicle (State matrix r c) (Move d car n) (position (State matrix r c) (Move d car n) 0 0)

-- position
position (State matrix r c) (Move d car n) x y =
        if c == y
                then position (State matrix r c) (Move d car n) (x+1) 0
        else if r == x
                then (r,c)
        else if element (State matrix r c) x y == car
                then (x,y)
        else position (State matrix r c) (Move d car n) x (y+1)

change (z:zs) x y char = if x == 0
        then changeColumn z x y char:zs
        else z:change zs (x-1) y char

changeColumn (z:zs) x y char = if y == 0
        then char:zs
        else z:changeColumn zs x (y-1) char


vehicle (State matrix r c) (Move d car n) (cx,cy) =
        if car_horizontal (State matrix r c) cx cy
                then changeBefore (State matrix r c) (Move d car n) (cx,cy) 2
        else if car_vertical (State matrix r c) cx cy
                then changeBefore (State matrix r c) (Move d car n) (cx,cy) 2
        else if truck_horizontal (State matrix r c) cx cy
                then changeBefore (State matrix r c) (Move d car n) (cx,cy) 3
        else changeBefore (State matrix r c) (Move d car n) (cx,cy) 3


changeBefore (State matrix r c) (Move d car n) (cx,cy) counter =
        if counter /= 0
                then if d == 'r'
                        then changeBefore (State (change matrix cx cy car) r c) (Move d car n) (cx,cy) (counter-1)
                else if d == 'l'
                        then changeBefore (State (change matrix cx cy car) r c) (Move d car n) (cx,cy) (counter-1)
                else if d == 'u'
                        then changeBefore (State (change matrix cx cy car) r c) (Move d car n) (cx,cy) (counter-1)
                else changeBefore (State (change matrix cx cy car) r c) (Move d car n) (cx,cy) (counter-1)
        else if car_horizontal (State matrix r c) cx cy
                then changeAfter (State matrix r c) (Move d car n) (cx,cy) 2
        else if car_vertical (State matrix r c) cx cy
                then changeAfter (State matrix r c) (Move d car n) (cx,cy) 2
        else if truck_horizontal (State matrix r c) cx cy
                then changeAfter (State matrix r c) (Move d car n) (cx,cy) 3
        else changeAfter (State matrix r c) (Move d car n) (cx,cy) 3

changeAfter (State matrix r c) (Move d car n) (cx,cy) counter =
        if counter /= 0
                then if d == 'r'
                        then changeAfter (State (change matrix cx cy car) r c) (Move d car n) (cx,cy) (counter-1)
                else if d == 'l'
                        then changeAfter (State (change matrix cx cy car) r c) (Move d car n) (cx,cy) (counter-1)
                else if d == 'u'
                        then changeAfter (State (change matrix cx cy car) r c) (Move d car n) (cx,cy) (counter-1)
                else changeAfter (State (change matrix cx cy car) r c) (Move d car n) (cx,cy) (counter-1)
        else (State matrix r c)
