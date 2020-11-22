> module TicTacToe where

> import Data.List
> import Data.Ord
> import GHC.Generics

> data Coordinate = C0 | C1 | C2
>   deriving (Eq, Ord, Show, Generic)

> type Index = (Coordinate, Coordinate)

> coordinates :: [Coordinate]
> coordinates = [C0, C1, C2]

> rowIxs :: Coordinate -> [Index]
> rowIxs cy = [(C0,cy), (C1,cy), (C2,cy)]

> -- columnIxs cx = [(cx,C0), (cx,C1), (cx,C2)]
> columnIxs :: Coordinate -> [Index]
> columnIxs cx = [(cx, head coordinates), (cx, head (tail coordinates)), (cx, head (reverse coordinates))]

> -- downDiagIxs = [(C0,C0), (C1,C1), (C2,C2)]
> downDiagIxs :: [Index]
> downDiagIxs = zip coordinates coordinates

> -- upDiagIxs = [(C0,C2), (C1,C1), (C2,C0)]
> upDiagIxs :: [Index]
> upDiagIxs = zip coordinates (reverse coordinates)

> winLines :: [[Index]]
> winLines =
>   downDiagIxs :
>   upDiagIxs :
>   map columnIxs coordinates ++
>   map rowIxs coordinates

> boardRows :: [[Index]]
> boardRows = map rowIxs coordinates

> allIxs :: [Index]
> allIxs = concat boardRows

> data Player = X | O
>   deriving (Eq, Show)

> opponent :: Player -> Player
> opponent X = O
> opponent O = X

> data Cell = Mark Player | Empty
>   deriving Eq

> instance Show Cell where
>   show (Mark a) = show a
>   show Empty = "."

> data Board = Board { cell :: Index -> Cell }

> instance Show Board where
>   show b =
>     unlines (map (concat . intersperse " " . map (show . cell b)) boardRows)

> example :: Index -> Cell
> example (C0,C0) = Mark X; example (C1,C0) = Mark O; example (C2,C0) = Empty
> example (C0,C1) = Mark X; example (C1,C1) = Mark X; example (C2,C1) = Empty
> example (C0,C2) = Mark O; example (C1,C2) = Empty; example (C2,C2) = Mark O

> exampleBoard :: Board
> exampleBoard = Board example

> emptyBoard :: Board
> emptyBoard = Board (const Empty)

> playerAt :: Board -> Player -> Index -> Bool
> playerAt b x i = cell b i == Mark x

> emptyAt :: Board -> Index -> Bool
> emptyAt b i = cell b i == Empty

> allEmpties :: Board -> [Index] -> [Index]
> allEmpties _ [] = []
> allEmpties b (i:is) = 
>   if emptyAt b i == True 
>     then i : allEmpties b is
>     else allEmpties b is

> emptyIxs :: Board -> [Index]
> emptyIxs b = allEmpties b allIxs

> won :: Board -> Player -> Bool
> won b x = any (\w -> (all (\i -> (playerAt b x i) == True) w) == True) winLines

> inProgress :: Board -> Bool
> inProgress b = not (won b X || won b O) && any (emptyAt b) allIxs

> write :: Index -> Player -> Board -> Board
> write i x b =
>   Board $ \i' ->
>     if i == i' && emptyAt b i then
>       Mark x
>     else
>       cell b i'

> data Outcome = Loss | Tie | Win
>   deriving (Eq, Ord, Show)

The "opponentOutcome" function flips an Outcome, to represent the outcome for
the opposing player: if one player has won, the opposing player has lost.

> opponentOutcome :: Outcome -> Outcome
> opponentOutcome Loss = Win
> opponentOutcome Tie = Tie
> opponentOutcome Win = Loss

The "boardOutcome" function computes the best outcome a given player can achieve
from a given board state.

This is a minimax algorithm from the field of AI, although this is an almost
trivial application of the concept. Don't worry if you're not familiar with the
algorithm - you don't need to understand why this function works in order to
solve the next problem.

This is an extremely inefficient AI, but it's very concise!

> boardOutcome :: Board -> Player -> Outcome
> boardOutcome b x =
>   if won b x then Win
>   else if won b (opponent x) then Loss
>   else if not (inProgress b) then Tie
>   else maximum (map (moveOutcome b x) (emptyIxs b))


*************
* PROBLEM 4 *  2 points
*************

Give a definition for the "moveOutcome" function, using "boardOutcome".
Your definition should implement this algorithm:
  - create a new board by making a mark for player x on board b at index i
  - call "boardOutcome" to get the new board outcome for *the opponent* of x
  - call "opponentOutcome" on that result to get the board outcome for x

Your definition should not be directly recursive - calling "boardOutcome" will
recurse into "moveOutcome", so "moveOutcome" doesn't need to have any calls to
"moveOutcome".

Replace the "Tie" below with your definition.

> moveOutcome :: Board -> Player -> Index -> Outcome
> moveOutcome b x i = do
>   let b' = b
>   opponentOutcome (boardOutcome (write i x b') (opponent x))



*****************
* END PROBLEM 4 *
*****************

> aiMove :: Board -> Player -> Index
> aiMove b x = maximumBy (comparing (moveOutcome b x)) (emptyIxs b)

> readCoord :: Char -> Maybe Coordinate
> readCoord '0' = Just C0
> readCoord '1' = Just C1
> readCoord '2' = Just C2
> readCoord _ = Nothing

> playerAct :: Board -> Player -> IO Board
> playerAct b x = do
>   input <- getLine
>   let tryAgain msg = putStrLn msg >> playerAct b x
>   case input of
>     [cx, ' ', cy] ->
>       case (readCoord cx, readCoord cy) of
>         (Just cx', Just cy') -> let i = (cx',cy') in
>           if emptyAt b i then return $ write i x b
>           else tryAgain "illegal move"
>         (Nothing, _) -> tryAgain "invalid input on first coordinate"
>         (_, Nothing) -> tryAgain "invalid input on second coordinate"
>     _ -> tryAgain "invalid input"

> aiAct :: Board -> Player -> Board
> aiAct b x = write (aiMove b x) x b

> exitMsg :: Board -> IO ()
> exitMsg b = do
>   if won b X then putStrLn "X wins!"
>   else if won b O then putStrLn "O wins!"
>   else putStrLn "it's a tie"

> play :: Board -> IO ()
> play b = do
>   print b
>   if inProgress b then do
>     b' <- playerAct b X
>     print b'
>     if inProgress b' then
>       play $ aiAct b' O
>     else
>       exitMsg b'
>   else
>     exitMsg b
