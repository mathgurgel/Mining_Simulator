main :: IO ()
main = putStrLn "Test suite not yet implemented"

-- {-# LANGUAGE ScopedTypeVariables #-}

-- module Main where

-- import Test.Tasty
-- import Test.Tasty.QuickCheck

-- type Fuel = Int
-- type Point = (Int,Int)
-- type Material = Int

-- data Robot = Robot {
--                 energy    :: Fuel,
--                 position  :: Point,
--                 collected :: Material
--              } deriving (Eq, Ord)

-- sampleRobot :: Robot
-- sampleRobot = Robot {
--                  energy = 100,
--                  position = (1,1),
--                  collected = 0
--               }

-- instance Show Robot where
--     show (Robot e p c) = "Energy:"    ++ (show e)    ++ "\n" ++
--                          "Position"   ++ (show p)    ++ "\n" ++
--                          "Collected:" ++ (show c)

-- main :: IO ()
-- main = defaultMain tests

-- tests :: TestTree
-- tests = testGroup "tests"
--             [
--                 testProperty "show robot" $
--                     show sampleRobot == "Energy:100\nPosition(1,1)\nCollected:0"
--             ]

