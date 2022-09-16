--type Cell = (int,int)
boardState = [(1,1),(2,2),(3,3)]

isAlive :: (Int,Int) -> [(Int,Int)] -> Bool
isAlive cell [] = False

isAlive cell boardState = 
	if head boardState == cell
		then True
	else isAlive cell (tail boardState)

rowcells row = [(row,y) | y <- [1..10]]

renderRow result [] boardState =
	result

renderRow result row boardState = 
	if isAlive (head row) boardState
	then renderRow (result ++ "X") (tail row) boardState
	else renderRow (result ++ " ") (tail row) boardState

renderBoard = 
	[1..10]

-- renderBoard =


main = print (renderRow "" (rowcells 1) boardState)
