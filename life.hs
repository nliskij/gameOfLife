--This program is free software: you can redistribute it and/or modify
--it under the terms of the GNU General Public License as published by
--the Free Software Foundation, either version 3 of the License, or
--(at your option) any later version.
--
--This program is distributed in the hope that it will be useful,
--but WITHOUT ANY WARRANTY; without even the implied warranty of
--MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--GNU General Public License for more details.
--
--You should have received a copy of the GNU General Public License
--along with this program.  If not, see <http://www.gnu.org/licenses/>.

import qualified Data.Map.Strict as Map
import Data.List (intercalate)
import Data.List.Split (chunksOf)
import Control.Concurrent

type Grid = Map.Map (Int, Int) Bool

prettyPrint :: Grid -> String
prettyPrint g = intercalate "\n" (chunksOf dim (map (\k -> repr (Map.lookup k g)) (Map.keys g)))
    where dim = maximum (map fst (Map.keys g)) + 1
          repr (Just True)  = '█'
          repr (Just False) = ' '
          repr Nothing      = ' '

dist :: (Int, Int) -> (Int, Int) -> Int
dist a b = max (abs (fst a - fst b)) (abs (snd a - snd b))

numNeighbors :: (Int, Int) -> Grid -> Int
numNeighbors cell g = length (Map.filterWithKey (\k v -> (dist k cell == 1) && v) g)

dies :: (Int, Int) -> Grid -> Bool
dies c g = numNeighbors c g < 2 || numNeighbors c g > 3

spawns :: (Int, Int) -> Grid -> Bool
spawns c g = numNeighbors c g == 3

evolve :: Grid -> Grid
evolve g = foldr (.) id (kill ++ spawn) g
    where kill = [Map.insert c False | c <- Map.keys g, dies c g]
          spawn = [Map.insert c True | c <- Map.keys g, spawns c g]

indexedGrid :: Int -> [(Int, Int)]
indexedGrid dim = [(x, y) | x <- [0..(dim - 1)], y <- [0..(dim - 1)]]

parseGrid :: [String] -> Grid
parseGrid lines = Map.fromList (zip indices isAlive)
        where indices = indexedGrid (length $ head lines)
              isAlive = map (=='█') (concat lines)

initialGrid = parseGrid ["........",
                         "........",
                         "........",
                         "...███..",
                         "....█...",
                         "........",
                         "........",
                         "........"]

main :: IO ()
main = mapM_ (\b -> putStrLn b >> threadDelay 1000000) boards
        where boards = map prettyPrint (iterate evolve initialGrid)
