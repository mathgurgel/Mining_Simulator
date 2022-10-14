{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.Tasty
import Test.Tasty.QuickCheck hiding (elements)
import Robot
import Prelude hiding (lines)


sampleRobot :: Robot
sampleRobot = Robot {
                 energy = 100,
                 position = (1,1),
                 collected = 0
              }

exampleFakeMine :: Mine
exampleFakeMine = Mine {
                lines = 15,
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
                            [Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall]]
            }
exampleMineString :: String
exampleMineString = "%%%%%%%%%%%%%%%\n%***..........%\n%***... ...*..%\n%***... ..***.%\n%.?.... ...*..%\n%..     .. ...%\n%.... .... ...%\n%.:.. .... ...%\n%.. .       ..%\n%..*. .. .....%\n%.... .. .;;..%\n%.*.. ...;;..*%\n%............$%\n%.........   .%\n%%%%%%%%%%%%%E%\n" 
main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests"
            [
                testProperty "show mine" $
                    show exampleMine == exampleMineString,
                    
                testProperty "show robot" $
                    show sampleRobot == "Energy:100\nPosition(1,1)\nCollected:0",
                
                testProperty "validMine" $ 
                    validMine exampleMine == True && validMine exampleFakeMine == False
            ]

