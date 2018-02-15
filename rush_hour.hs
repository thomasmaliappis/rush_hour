-- "..abbb\nc.adee\nc==d..\n.g.dhh\nigjj.k\nilll.k"
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
writeState (State [x] r c) = x++['\n']
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
        else if (element (State matrix r c) x y) == '.'
                then successorLoop (State matrix r c) x (y+1)
        else if truck_horizontal (State matrix r c) x y
                then (truck_horizontal_moves (State matrix r c) x y)++successorLoop (State matrix r c) x (y+1)
        else if truck_vertical (State matrix r c) x y
                then (truck_vertical_moves (State matrix r c) x y)++successorLoop (State matrix r c) x (y+1)
        else if car_horizontal (State matrix r c) x y
                then (car_horizontal_moves (State matrix r c) x y)++successorLoop (State matrix r c) x (y+1)
        else if car_vertical (State matrix r c) x y
                then (car_vertical_moves (State matrix r c) x y)++successorLoop (State matrix r c) x (y+1)
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
car_horizontal_moves (State matrix r c) x y = car_moves_right (State matrix r c) x y y 1 ++ car_moves_left (State matrix r c) x y y 1

car_moves_right (State matrix r c) x y z n = if (y+2 < c) && (element (State matrix r c) x (y+2) == '.')
        then (Move 'r' (element (State matrix r c) x z) n,1):car_moves_right (State matrix r c) x (y+1) z (n+1)
        else []

car_moves_left (State matrix r c) x y z n = if (y-1 >= 0) && (element (State matrix r c) x (y-1) == '.')
        then (Move 'l' (element (State matrix r c) x z) n,1):car_moves_left (State matrix r c) x (y-1) z (n+1)
        else []

-- car_vertical
car_vertical (State matrix r c) x y = if (x+1 < r) && (element (State matrix r c) x y == element (State matrix r c) (x+1) y) && (element (State matrix r c) x y /= '.')
        then True
        else False

car_vertical_moves (State matrix r c) x y = car_moves_up (State matrix r c) x y x 1 ++ car_moves_down (State matrix r c) x y x 1

car_moves_up (State matrix r c) x y z n = if (x-1 >= 0) && (element (State matrix r c) (x-1) y == '.')
        then (Move 'u' (element (State matrix r c) z y) n,1):car_moves_up (State matrix r c) (x-1) y z (n+1)
        else []

car_moves_down (State matrix r c) x y z n = if (x+2 < r) && (element (State matrix r c) (x+2) y == '.')
        then (Move 'd' (element (State matrix r c) z y) n,1):car_moves_down (State matrix r c) (x+1) y z (n+1)
        else []
-- truck_horizontal
truck_horizontal (State matrix r c) x y = if (y+2 < c) && (element (State matrix r c) x y == element (State matrix r c) x (y+1)) && (element (State matrix r c) x y == element (State matrix r c) x (y+2)) && (element (State matrix r c) x y /= '.')
        then True
        else False
truck_horizontal_moves (State matrix r c) x y = truck_moves_right (State matrix r c) x y y 1 ++ truck_moves_left (State matrix r c) x y y 1

truck_moves_right (State matrix r c) x y z n = if (y+3 < c) && (element (State matrix r c) x (y+3) == '.')
        then (Move 'r' (element (State matrix r c) x z) n,1):truck_moves_right (State matrix r c) x (y+1) z (n+1)
        else []

truck_moves_left (State matrix r c) x y z n = if (y-1 >= 0) && (element (State matrix r c) x (y-1) == '.')
        then (Move 'l' (element (State matrix r c) x z) n,1):truck_moves_left (State matrix r c) x (y-1) z (n+1)
        else []
-- truck_vertical
truck_vertical (State matrix r c) x y = if (x+2 < r) && (element (State matrix r c) x y == element (State matrix r c) (x+1) y) && (element (State matrix r c) x y == element (State matrix r c) (x+2) y) && (element (State matrix r c) x y /= '.')
        then True
        else False
truck_vertical_moves (State matrix r c) x y = truck_moves_up (State matrix r c) x y x 1 ++ truck_moves_down (State matrix r c) x y x 1

truck_moves_up (State matrix r c) x y z n = if (x-1 >= 0) && (element (State matrix r c) (x-1) y == '.')
        then (Move 'u' (element (State matrix r c) z y) n,1):truck_moves_up (State matrix r c) (x-1) y z (n+1)
        else []

truck_moves_down (State matrix r c) x y z n = if (x+3 < r) && (element (State matrix r c) (x+3) y == '.')
        then (Move 'd' (element (State matrix r c) z y) n,1):truck_moves_down (State matrix r c) (x+1) y z (n+1)
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

change [] _ _ _ = []
change (z:zs) x y char = if x == 0
        then changeColumn z x y char:zs
        else z:change zs (x-1) y char

changeColumn [] _ _ _ = []
changeColumn (z:zs) x y char = if y == 0
        then char:zs
        else z:changeColumn zs x (y-1) char


vehicle (State matrix r c) (Move d car n) (cx,cy) =
        if truck_horizontal (State matrix r c) cx cy
                then changeBefore (State matrix r c) (Move d car n) (cx,cy) 2 3
        else if truck_vertical (State matrix r c) cx cy
                then changeBefore (State matrix r c) (Move d car n) (cx,cy) 3 3
        else if car_horizontal (State matrix r c) cx cy
                then changeBefore (State matrix r c) (Move d car n) (cx,cy) 0 2
        else changeBefore (State matrix r c) (Move d car n) (cx,cy) 1 2


changeBefore (State matrix r c) (Move d car n) (cx,cy) vehicle counter =
        if counter /= 0
                then if d == 'r'
                        then changeBefore (State (change matrix cx (cy+(counter-1)) '.') r c) (Move d car n) (cx,cy) vehicle (counter-1)
                else if d == 'l'
                        then changeBefore (State (change matrix cx (cy+(counter-1)) '.') r c) (Move d car n) (cx,cy) vehicle (counter-1)
                else if d == 'u'
                        then changeBefore (State (change matrix (cx+(counter-1)) cy '.') r c) (Move d car n) (cx,cy) vehicle (counter-1)
                else changeBefore (State (change matrix (cx+(counter-1)) cy '.') r c) (Move d car n) (cx,cy) vehicle (counter-1)
        -- else (State matrix r c)
        else if vehicle == 0
                then if d == 'r'
                        then changeAfter (State matrix r c) (Move d car n) (cx,cy+n) 2
                else changeAfter (State matrix r c) (Move d car n) (cx,cy-n) 2
        else if vehicle == 1
                then if d == 'u'
                        then changeAfter (State matrix r c) (Move d car n) (cx-n,cy) 2
                else changeAfter (State matrix r c) (Move d car n) (cx+n,cy) 2
        else if vehicle == 2
                then if d == 'r'
                        then changeAfter (State matrix r c) (Move d car n) (cx,cy+n) 3
                else changeAfter (State matrix r c) (Move d car n) (cx,cy-n) 3
        else if d == 'u'
                then changeAfter (State matrix r c) (Move d car n) (cx-n,cy) 3
        else changeAfter (State matrix r c) (Move d car n) (cx+n,cy) 3

changeAfter (State matrix r c) (Move d car n) (cx,cy) counter =
        if counter /= 0
                then if d == 'r'
                        then changeAfter (State (change matrix cx (cy+(counter-1)) car) r c) (Move d car n) (cx,cy) (counter-1)
                else if d == 'l'
                        then changeAfter (State (change matrix cx (cy+(counter-1)) car) r c) (Move d car n) (cx,cy) (counter-1)
                else if d == 'u'
                        then changeAfter (State (change matrix (cx+(counter-1)) cy car) r c) (Move d car n) (cx,cy) (counter-1)
                else changeAfter (State (change matrix (cx+(counter-1)) cy car) r c) (Move d car n) (cx,cy) (counter-1)
        else (State matrix r c)

-- finalState
finalState :: State -> Bool
finalState (State matrix r c) = check (State matrix r c) 0 0

-- check
check (State matrix r c) x y =
        if c == y
                then check (State matrix r c) (x+1) 0
        else if r == x
                then False
        else if (element (State matrix r c) x y == '=') && (element (State matrix r c) x (y+1) == '=') && (y == c-2)
                then True
        else check (State matrix r c) x (y+1)

solve :: State -> [Move]
solve currentState = solveHelper currentState [currentState] (successorMoves currentState) 0

member [] _ = False
member (x:xs) y = if x == y
        then True
        else member xs y

solveHelper currentState explored moves n =
        if finalState currentState
                then []
        else if member explored newstate
                then solveHelper currentState explored moves (n+1)
                else move:(solveHelper newstate (explored++[newstate]) (successorMoves newstate) 0)
        where
                (move,cost) = moves !! (n `mod` ((length moves)-1))
                newstate = makeMove currentState move

printSolution s [] = putStrLn (writeState s)
printSolution s (m:ms) = do {putStrLn (writeState s); printSolution (makeMove s m) ms}
