import Data.IORef (IORef, newIORef, readIORef, modifyIORef, writeIORef )
import System.Exit
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import System.Random
import List
import Random(Random, randomIO)

data State = 
    State {
        board :: Board,
        numberOfMines :: Int,
        numberFound :: Int,
        trueNumberFound :: Int,
        gameOver :: Bool,
        gameWon :: Bool
    }
type Board = [Row]
type Row = [Col]
data Col = Column {numAround :: Int, mined, seen, marked :: Bool} deriving Show

-- Bare board setup code...
initialState :: State
initialState = 
    State {
        board = (setupBoard 10 10),
        numberOfMines = 0,
        numberFound = 0,
        trueNumberFound = 0,
        gameOver = False,
        gameWon = False
        } where
    setupBoard :: Int -> Int -> Board
    setupBoard _ 0 = []
    setupBoard numWide numHigh = addRow numWide : setupBoard numWide (numHigh - 1) where
        addRow :: Int -> [Col]
        addRow 0 = []
        addRow numWide = Column {numAround = 0, mined = False, seen = False, marked = False} : addRow (numWide - 1)

-- Mine laying code...
-- How do you take a list and create a new copy of the list with a single element modified?
-- Is there a more efficient way than what I am currently doing?
setMine :: Board -> Int -> Int -> Board
setMine board col row = modifyCellInPlace (\cell -> cell {mined = True}) board col row

randomCell ((xmin, ymin), (xmax, ymax)) =
    do x <- randomRIO (xmin, xmax)
       y <- randomRIO (ymin, ymax)
       return (x, y)

mineBoard :: Int -> Board -> IO Board
mineBoard 0     board = return (board)
mineBoard count board | count > 0 = 
    do (x, y) <- randomCell ((0, 0), (9, 9))
       mineBoard (count - 1) (setMine board x y)

-- Is this a proper way to do this or is there a cleaner / more consise way?
howManyMines :: Board -> Int
howManyMines board = sum (map (\row -> sum (map (\col -> if mined col then 1 else 0) row)) board)

modifyCellInPlace :: (Col -> Col) -> Board -> Int -> Int -> Board
modifyCellInPlace fn board col row = 
    loopOverBoard (\board column x y -> if x == col && y == row then fn column else column) board

setSeen :: State -> Int -> Int -> State
setSeen state x y = let mined = isMined (board state) x y in
                    let numberAround = minesAround (board state) x y in
                    let ifseen = hasBeenSeen (board state) x y in
                    let brd = modifyCellInPlace (\col -> col{seen = True}) (board state) x y in
                       if mined
                            then state{gameOver = True, board = brd}
                            else if numberAround == 0 && ifseen == False
                                 then 
                                    around (\state cell -> 
                                        if (x >= 0) && (y >= 0) && (x < 10) && (y < 10)
                                        then setSeen state (fst cell) (snd cell) else state)
                                    state{board = brd} x y
                                else state{board = brd}

getTrueNumberFound state = foldl (\rowTotal row -> rowTotal + foldl (\colTotal col ->
                               if marked col 
                                  then if mined col then colTotal + 1 else colTotal - 1 
                                  else colTotal) 0 row) 0 (board $ state)

setMarked state x y = let alreadyMarked = isMarked (board state) x y in
                      let currentFound = if alreadyMarked 
                                then (numberFound state) - 1
                                else (numberFound state) + 1 in
                      let partialState = state{board = modifyCellInPlace 
                                (\col -> if marked col 
                                            then col{marked = False, seen = False}
                                            else col{marked = True, seen = True}) (board state) x y} in
                      let trueNumber = getTrueNumberFound partialState in
                        if trueNumber == 10
                        then partialState {trueNumberFound = trueNumber, gameOver = True, gameWon = True}
                        else partialState {trueNumberFound = trueNumber}

--minesAround :: Board -> Int -> Int -> Int
minesAround board x y = around (\total cell -> 
                            if isMined board (fst cell) (snd cell)
                                then (total + 1)
                                else total) 0 x y

-- Could be used in the display callback and in setMinesAround...
loopOverBoard :: (Board -> Col -> Int -> Int -> Col) -> Board -> Board
loopOverBoard fn board =
    map (\(y, row) -> 
        map (\(x, col) ->
            fn board col x y) (zip [0..9] row)) (zip [0..9] board)

-- How can this be refactored so it can be used in minesAround?
around fn state x y = let cellsAround = [
                                 (x+1, y+1),
                                 (x+1, y),
                                 (x+1, y-1),
                                 (x,   y+1),
                                 (x,   y-1),
                                 (x-1, y+1),
                                 (x-1, y),
                                 (x-1, y-1)] in
                    foldl fn state cellsAround

setNumberOfMinesAround :: Board -> Board
setNumberOfMinesAround board =
    map (\(y, row) -> 
        map (\(x, col) ->
            col{numAround = minesAround board x y}) (zip [0..9] row)) (zip [0..9] board)

-- numberOfMines should be computed
setMines :: Int -> State -> IO State
setMines numberToSet state = 
    do board <- (mineBoard numberToSet (board state))
       let number = howManyMines board
       if number == 10
          then return (state{board = board, numberOfMines = number})
          else setMines (10 - number) state {board = board}

--isMined :: Board -> Int -> Int -> Bool
isMined board x y = if (x >= 0) && (y >= 0) && (x < 10) && (y < 10) && mined ((board !! y) !! x) then True else False
isMarked board x y = if (x >= 0) && (y >= 0) && (x < 10) && (y < 10) && marked ((board !! y) !! x) then True else False
hasBeenSeen board x y = if (x >= 0) && (y >= 0) && (x < 10) && (y < 10) && seen ((board !! y) !! x) then True else False

-- Pretty print our board for debugging...
{-
pprintBoard :: Board -> IO ()
pprintBoard rows = do mapM (\row -> pprintRow row) rows; putStrLn ""; return () where
    pprintRow :: [Col] -> IO ()
    pprintRow cols = do mapM (\col -> pprintCol col) cols; putStrLn ""; return() where
        pprintCol :: Col -> IO ()
        pprintCol Column {mined = True} = putStr "* "
        pprintCol col = do putStr $ show $ numAround $ col; putStr " "
-}

newGame = do initialState <- setMines 10 $ initialState
             mutableState <- newIORef initialState
             currentState <- readIORef mutableState
             writeIORef mutableState (currentState {board = setNumberOfMinesAround $ board $ currentState})
             displayCallback $= (display mutableState)
             keyboardMouseCallback $= (Just (keyboard mutableState))
             return (mutableState)

main :: IO ()
main =
    do 
       getArgsAndInitialize
       initialWindowSize $= ((Size 200 200))
       initialDisplayMode $= [DoubleBuffered]
       wnd <- createWindow "Haskell Minesweeper."
       myInit
       mutableState <- newGame
       --putStrLn "Hi!"
       --putStr "Number of Mines: "
       --currentState <- readIORef mutableState
       --putStrLn $ show $ numberOfMines $ currentState
       --pprintBoard $ (board currentState)
       mainLoop
       destroyWindow wnd
       `catch` (\exc -> exitWith ExitSuccess)

-- Graphics related
myInit :: IO ()
myInit = do
    clearColor $= Color4 0.0 0.0 0.0 0.0

keyboard :: IORef State-> KeyboardMouseCallback
--keyboard state (SpecialKey KeyLeft) Down _ _ = do s <- readIORef state
keyboard _ (Char '\27') Down _ _ = do newGame; postRedisplay Nothing; return ()
keyboard _ (Char 'q') Down _ _ = exitWith ExitSuccess
keyboard refState (MouseButton button) Down _ (Position x y) = case button of
    LeftButton -> do let col = (fromIntegral (x `div` 20))
                     let row = (fromIntegral (y `div` 20))
                     state <- readIORef refState
                     if gameOver state then return () else writeIORef refState (setSeen state col row)
                     postRedisplay Nothing
                     return ()
    RightButton -> do let col = (fromIntegral (x `div` 20))
                      let row = (fromIntegral (y `div` 20))
                      state <- readIORef refState
                      if gameOver state then return () else writeIORef refState (setMarked state col row)
                      postRedisplay Nothing
                      return ()
    MiddleButton -> do let col = (fromIntegral (x `div` 20))
                       let row = (fromIntegral (y `div` 20))
                       state <- readIORef refState
                       if gameOver state 
                           then return ()
                           else writeIORef 
                               refState
                               (around (\state cell -> if hasBeenSeen (board state) (fst cell) (snd cell) then state else setSeen state (fst cell) (snd cell)) state col row)
                       postRedisplay Nothing
                       return ()
    _ -> return ()
keyboard _ _          _    _ _ = return ()

drawOneLine :: Vertex2 GLfloat -> Vertex2 GLfloat -> IO ()
drawOneLine p1 p2 = renderPrimitive Lines $ do vertex p1; vertex p2

-- Translate grid coordinates to opengl coordinates.
--screen :: Int -> Int -> Vertex2 Float
screen x y = Vertex2 ((x / 5.0) - 1.0) (1.0 - (y / 5.0))
fontPos x y = let realx = (x + 0.3) in
              let realy = (y + 0.6) in
              Vertex2 ((realx / 5.0) - 1.0) (1.0 - (realy / 5.0))

display :: IORef State -> DisplayCallback
display iorefState = 
    do state <- readIORef iorefState
       let maxx = (20 * 10)
       let maxy = (20 * 10)
       clear [ColorBuffer]

       -- Draw the state...
       mapM (\(y, row) -> 
        mapM (\(x, col) -> case col of
            Column {marked = True} -> do drawBox (Color3 1.0 1.0 0.0 :: Color3 GLfloat) x y
            Column {seen = True, mined = True} -> drawMine (x :: Float) (y :: Float)
            Column {seen = True} -> do drawSeen x y (numAround col)
            _ -> do drawBox (Color3 0.0 0.0 0.0 :: Color3 GLfloat) x y
        ) (zip [0..9] row)) (zip [0..9] (board state))

       -- Draw the grid...
       color (Color3 1.0 1.0 1.0 :: Color3 GLfloat)
       mapM (\x -> drawOneLine (screen x 0) (screen x maxy)) [0..10]
       mapM (\y -> drawOneLine (screen 0 y) (screen maxx y)) [0..10]

       if gameWon state 
            then do color (Color3 0.0 1.0 0.0 :: Color3 GLfloat)
                    drawOneLine (screen 0 6) (screen 3 10)
                    drawOneLine (screen 3 10) (screen 10 0)
            else if gameOver state
                then do color (Color3 1.0 0.0 0.0 :: Color3 GLfloat)
                        drawOneLine (screen 0 0) (screen 10 10)
                        drawOneLine (screen 10 0) (screen 0 10)
                else do return ()
       swapBuffers

drawBox clr x y = do
   renderPrimitive Quads $ do
        color $ clr
        vertex $ screen x y
        vertex $ screen x (y + 1)
        vertex $ screen (x + 1) (y + 1)
        vertex $ screen (x + 1) y

drawMine x y = do
    drawBox (Color3 1.0 0.0 0.0 :: Color3 GLfloat) x y

drawSeen :: Float -> Float -> Int -> IO ()
drawSeen x y numberAround = do
    preservingMatrix $ do
        drawBox (Color3 0.5 0.5 0.5 :: Color3 GLfloat) x y
        color (Color3 1.0 1.0 1.0 :: Color3 GLfloat)
        rasterPos ((fontPos x y) :: Vertex2 Float)
        if numberAround > 0 then renderString TimesRoman10 $ show $ numberAround else return ()

{- Here are some previous attempts
setMine :: Board -> Int -> Int -> Board
setMine (r:rs) col row | row > 0 = r : setMine rs col (row - 1)
setMine (r:rs) col   0 = setMineInCol r col : rs

setMine :: Board -> Int -> Int -> Board
setMine board col row = 
    let before = if row > 0 then take (row - 1) board else [] in
    before ++ [setMineInCol (board !! row) col] ++ drop row board
--setMine [] col row = error "Out of bounds while trying to set mine."

setMineInCol :: Row -> Int -> Row
setMineInCol (c:cs) col | col > 0 = c : setMineInCol cs (col - 1)
setMineInCol (c:cs) 0   = Column {numAround = 0, mined = True, seen = False} : cs
setMineInCol [] 0 = []
-- TODO: Provide a more detailed message here...
setMineInCol [] col | col > 0 = error "We shouldn't get here."
-}
