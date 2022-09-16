import Data.Map.Strict as Map

--type Cell = (int,int)
boardState = [(1,1),(2,2),(3,3)]

--isAlive :: (Int,Int) -> [(Int,Int)] -> Bool
isAlive cell [] = False

isAlive cell boardState =
  if head boardState == cell
  then True
  else isAlive cell (tail boardState)

rowCells row = [(row,y) | y <- [1..10]]

renderRow result [] boardState =
  result

renderRow result row boardState = -- renderRow "" (1,1) [(1,1),(2,2),(3,3)]
  if isAlive (head row) boardState
  then renderRow (result ++ "X") (tail row) boardState
  else renderRow (result ++ " ") (tail row) boardState

rows = [1..10]
renderBoard result [] boardState = result
renderBoard result rows boardState = renderBoard (result++((renderRow "" (rowCells (head rows)) boardState) ++ "\n"))
   (tail rows)
   boardState

--tickN n boardState

--tick boardState =


getNeighborCount [] map = map
getNeighborCount boardState map = case Map.lookup (head boardState) map of Nothing -> (getNeighborCount (tail boardState) (Map.insert (head boardState) 1 map))
																					       													 (Just x) -> (getNeighborCount (tail boardState) (Map.adjust (+ 1) (head boardState) map))


--getNeighborCount [] map = map
--getNeighborCount y = case y of Nothing -> 1
--															(Just x) -> 2

-- iterate through the live rowCells
-- update each live cells neighbors in hashmap


main = putStrLn $ renderBoard "" [1..10] boardState



--f (g x) = (f . g) x
--f x = f . g . h x
--f x = f ( g ( h x ) )
