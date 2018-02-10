-- data type State
data State = State [String] Int Int deriving (Show)
instance Eq State where
        (State string1 rows1 columns1) == (State string2 rows2 columns2) = string1 == string2 && rows1 == rows2 && columns1 == columns2

-- data type Move
data Move = MoveLeft Char| MoveRight Char| MoveUp Char| MoveDown Char deriving (Show)

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
