{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.Tasty
import Test.Tasty.QuickCheck
import Robot


sampleRobot :: Robot
sampleRobot = Robot {
                 energy = 100,
                 position = (1,1),
                 collected = 0
              }

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests"
            [
                testProperty "show robot" $
                    show sampleRobot == "Energy:100\nPosition(1,1)\nCollected:0"
            ]

