module Robot ( readLDM
             , readLCR
             , run
             )where

import Prelude hiding (lines)

import Control.Monad.State
import Parsing 



type Fuel = Int
type Point = (Int,Int)
type Material = Int

data Robot = Robot {
                energy    :: Fuel,
                position  :: Point,
                collected :: Material
             } deriving (Eq, Ord)

sampleRobot :: Robot
sampleRobot = Robot {
                 energy = 100,
                 position = (1,1),
                 collected = 0
              }

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
            | c == '$' = Material (-1)


type Line = [Element]

data Mine = Mine {
              lines    :: Int,
              columns  :: Int,
              elements :: [Line]
            } deriving (Eq, Ord)

instance Show Mine where
    show = undefined

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

pLine :: Parser Char Line
pLine = undefined

pMine :: Parser Char Mine
pMine = undefined

data Instr = L -- move para esquerda
           | R -- move para direita
           | U -- move para cima
           | D -- move para baixo
           | C -- coleta material
           | S -- para para recarga.
           deriving (Eq,Ord,Show,Enum)

pInstr :: Parser Char Instr
pInstr = undefined

pProgram :: Parser Char [Instr]
pProgram = undefined

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
enoughEnergy = undefined

incEnergy :: ConfM ()
incEnergy = undefined


valid :: Instr -> ConfM Bool
valid = undefined


updateMine :: Instr -> ConfM ()
updateMine = undefined

exec :: Instr -> ConfM ()
exec = undefined

initRobot :: Mine -> Robot
initRobot = undefined

run :: [Instr] -> Mine -> Mine
run = undefined

readLDM :: String -> IO (Either String Mine)
readLDM = undefined

readLCR :: String -> IO (Either String [Instr])
readLCR = undefined
