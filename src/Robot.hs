module Robot where

import Prelude

import Control.Monad.State
import Parsing 
--import Data.Matrix as MT
import Data.List


type Fuel = Int
type Point = (Int,Int)
type Material = Int

data Robot = Robot {
                energy    :: Fuel,
                position  :: Point,
                collected :: Material
             } deriving (Eq, Ord)

-- Exercise 1

instance Show Robot where
    show (Robot e p c) = "Energy:"    ++ (show e) ++ "\n" ++
                         "Position"   ++ (show p) ++ "\n" ++
                         "Collected:" ++ (show c)


data Element = Empty         -- espaço vazio
             | Entry         -- entrada da mina
             | Wall          -- parede
             | Earth         -- terra
             | Rock          -- rocha
             | Material Int  -- material, Int indica quantidade.
             deriving (Eq,Ord)

-- Exercise 2

instance Show Element where
    show Empty = " "  
    show Entry = "E"     
    show Wall  = "%"
    show Earth = "."
    show Rock  = "*"
    show (Material qtt)
        | qtt == 50  = "?"
        | qtt == 100 = ":"
        | qtt == 150 = ";"
        | otherwise  = "$"


-- Exercise 3

list_of_elements = " E%.*?:;$"

pElement :: Parser Char Element
pElement = char_to_elem <$> elemChar
    where
        elemChar = sat isElement
            where
                isElement x = x `elem` list_of_elements
                        
        char_to_elem c
            | c == ' ' = Empty     
            | c == 'E' = Entry           
            | c == '%' = Wall  
            | c == '.' = Earth 
            | c == '*' = Rock  
            | c == '?' = Material 50
            | c == ':' = Material 100
            | c == ';' = Material 150
            | c == '$' = Material 1


type Line = [Element]

data Mine = Mine {
              lines'    :: Int,
              columns  :: Int,
              elements :: [Line]
            } deriving (Eq, Ord)

--https://stackoverflow.com/questions/13846870/using-show-with-a-list-of-lists-in-haskell
instance Show Mine where
    show (Mine _ _ rows) = filter (`notElem` "[],") $ unlines $ map show rows

-- Exercise 4

validMine :: Mine -> Bool
validMine mine = right_num_lines mine && right_num_columns mine && entrance mine
    where
        right_num_lines (Mine lins _ elms) = length elms == lins

        right_num_columns (Mine _ cols elms) = and $ map (\line -> length line == cols) elms

        entrance (Mine _ _ elms) = isThereEntrance $ lastLine elms
            where
                lastLine xs = head $ reverse xs

                isThereEntrance [] = False
                isThereEntrance (x : xs) = x == Entry || isThereEntrance xs


-- Exercise 5

exampleMine :: Mine
exampleMine = Mine {
                lines' = 15,
                columns = 15,
                elements = [[Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall], 
                            [Wall,Rock,Rock,Rock,Earth,Earth,Earth,Earth,Earth,Earth,Earth,Earth,Earth,Earth,Wall],
                            [Wall,Rock,Rock,Rock,Earth,Earth,Earth,Empty,Earth,Earth,Earth,Rock,Earth,Earth,Wall],
                            [Wall,Rock,Rock,Rock,Earth,Earth,Earth,Empty,Earth,Earth,Rock,Rock,Rock,Earth,Wall],
                            [Wall,Earth,(Material 50),Earth,Earth,Earth,Earth,Empty,Earth,Earth,Earth,Rock,Earth,Earth,Wall],
                            [Wall,Earth,Earth,Empty,Empty,Empty,Empty,Empty,Earth,Earth,Empty,Earth,Earth,Earth,Wall],
                            [Wall,Earth,Earth,Earth,Earth,Empty,Earth,Earth,Earth,Earth,Empty,Earth,Earth,Earth,Wall],
                            [Wall,Earth,(Material 100),Earth,Earth,Empty,Earth,Earth,Earth,Earth,Empty,Earth,Earth,Earth,Wall],
                            [Wall,Earth,Earth,Empty,Earth,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Earth,Earth,Wall],
                            [Wall,Earth,Earth,Rock,Earth,Empty,Earth,Earth,Empty,Earth,Earth,Earth,Earth,Earth,Wall],
                            [Wall,Earth,Earth,Earth,Earth,Empty,Earth,Earth,Empty,Earth,(Material 150),(Material 150),Earth,Earth,Wall],
                            [Wall,Earth,Rock,Earth,Earth,Empty,Earth,Earth,Earth,(Material 150),(Material 150),Earth,Earth,Rock,Wall],
                            [Wall,Earth,Earth,Earth,Earth,Earth,Earth,Earth,Earth,Earth,Earth,Earth,Earth,(Material 1),Wall],
                            [Wall,Earth,Earth,Earth,Earth,Earth,Earth,Earth,Earth,Earth,Empty,Empty,Empty,Earth,Wall],
                            [Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Entry,Wall]]
            }


-- Exercise 6

pLine :: Parser Char Line
pLine = greedy pElement

-- Exercise 7

testMine :: String
testMine = "%%%\n%%%\n%E%\nasdasdas"

pMine :: Parser Char Mine
pMine = Parser runPrsr
    where
        runPrsr inp = case inp of
                            [] -> []
                            xs -> if (validMine mine) then [(mine, rest)]
                                  else []
            where
                pElms = endBy pLine (symbol '\n')

                result_pElms = runParser pElms inp
                elms = fst $ head $ result_pElms
                
                rest = snd $ head $ result_pElms

                nCols = length $ fst $ head $ runParser pLine inp

                nLins = (length $ concat elms) `div` nCols

                mine = Mine nLins nCols elms


data Instr = L -- move para esquerda
           | R -- move para direita
           | U -- move para cima
           | D -- move para baixo
           | C -- coleta material
           | S -- para para recarga.
           deriving (Eq,Ord,Show,Enum)

-- Exercise 8

list_of_instr = "LRUDCS"


pInstr :: Parser Char Instr
pInstr = char_to_instr <$> instrChar
    where
        instrChar = sat isInstr
            where
                isInstr x = x `elem` list_of_instr
        
        char_to_instr i
            | i == 'L' = L
            | i == 'R' = R
            | i == 'U' = U
            | i == 'D' = D
            | i == 'C' = C
            | i == 'S' = S

-- Exercise 9

pProgram :: Parser Char [Instr]
pProgram = greedy pInstr

type Conf = (Robot, Mine)

type ConfM a = State Conf a


-- Exercise 10

current :: ConfM Point
current = do
            (robot, _) <- get
            return (position robot)

mine :: ConfM Mine
mine = do
        (_, mine) <- get
        return mine

enoughEnergy :: Int -> ConfM Bool
enoughEnergy eWanted
                = do
                    (robot, _) <- get
                    let eRobot = energy robot
                    return (eRobot > eWanted)


incEnergy :: ConfM ()
incEnergy = do  (Robot enr pos col, mine) <- get
                let enr' = enr + 1
                let robot' = (Robot enr' pos col)
                put (robot', mine)


-- Helper function

hasMaterial :: Mine -> Point -> Bool
hasMaterial mine (x, y) = (head $ show material) `elem` materials
    where
        material = (elements mine) !! x !! y
        materials = "?:;$"

-- Exercise 11

-- (!!) :: [a] -> Int -> a
-- Defined in GHC.List
-- Ref: https://stackoverflow.com/questions/23427728/how-to-extract-the-value-from-a-certain-position-in-a-matrix-in-haskell

valid :: Instr -> ConfM Bool
valid instr =
    if instr == S then do -- charge
        return True
    else do
        (robot, m) <- get
        let (x, y) = position robot
        let pr = (x + 1, y)
        let pl = (x - 1, y)
        let pu = (x, y + 1)
        let pd = (x, y - 1)

        if instr == C then do -- collect
            enoughEnergy 10

            return ((hasMaterial m pr || hasMaterial m pl || hasMaterial m pu || hasMaterial m pd))
        else do -- movement instruction
            enoughEnergy 1
            case instr of L -> return (not $ hasWall m pl)
                          R -> return (not $ hasWall m pr)
                          U -> return (not $ hasWall m pu)
                          D -> return (not $ hasWall m pd)
    where
        hasWall mine (x, y) = (mElements !! x !! y) == Wall
            where mElements = elements mine


-- Helper function

changeRobotEnr :: Int -> ConfM ()
changeRobotEnr n =
    do
        (Robot enr pos col, mine) <- get
        let enr' = enr + n
        let robot' = (Robot enr' pos col)
        put (robot', mine)


-- Helper function
-- Ref: https://stackoverflow.com/questions/20156078/replacing-an-element-in-a-list-of-lists-in-haskell

updateMatrix :: [[a]] -> a -> (Int, Int) -> [[a]]
updateMatrix m x (r,c) =
    take r m ++
    [take c (m !! r) ++ [x] ++ drop (c + 1) (m !! r)] ++
    drop (r + 1) m


-- Exercise 12

updateMine :: Instr -> ConfM ()
updateMine instr =
    do
        isValid <- valid instr

        case isValid of
            True -> if instr == S then do -- charge instruction
                        incEnergy
                    else do
                        (x, y) <- current
                        let pr = (x + 1, y)
                        let pl = (x - 1, y)
                        let pu = (x, y + 1)
                        let pd = (x, y - 1)
                        (_, m) <- get

                        if instr == C then do -- collect instruction
                            case (hasMaterial m pr) of
                                True -> collect_material pr
                                False -> case (hasMaterial m pl) of
                                            True -> collect_material pl
                                            False -> case (hasMaterial m pu) of
                                                        True -> collect_material pu
                                                        False -> case (hasMaterial m pd) of
                                                                    True -> collect_material pd
                        else do -- movement instruction
                            case (instr == L) of
                                True -> movement pl
                                False -> case (instr == R) of
                                            True -> movement pr
                                            False -> case (instr == U) of
                                                        True -> movement pu
                                                        False -> case (instr == D) of
                                                                    True -> movement pd
    where
        collect_material point =
            do
                (Mine lin col elm) <- mine
                let elm' = updateMatrix elm Empty point
                changeRobotEnr (-10)
                (robot', _) <- get
                let mine' = Mine lin col elm'
                put (robot', mine')
        
        movement pos' =
            do
                (Robot enr _ col, mine) <- get
                let robot' = Robot enr pos' col
                put (robot', mine)
                changeRobotEnr (-1)


-- Exercise 13

exec :: Instr -> ConfM ()
exec instr =
    do
        isValid <- valid instr

        case isValid of
            True -> updateMine instr
            False -> updateMine S


-- Exercise 14

initRobot :: Mine -> Robot
initRobot mine = Robot 100 (entrance mine) 0
    where
        entrance (Mine l _ m) = (x, y)
            where
                x = l - 1
                y = entrance_pos $ lastLine m

                lastLine m = head $ reverse m

                entrance_pos line = length $ takeWhile (/= Entry) line


run :: [Instr] -> Mine -> Mine
run = undefined

readLDM :: String -> IO (Either String Mine)
readLDM = runParser pMine readFile

readLCR :: String -> IO (Either String [Instr])
readLCR = undefined