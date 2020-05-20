module Main where

import Types
import Countries

import Reporter
import Bot
import Totalizer

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)

import Control.Monad.Reader

main :: IO ()
main = do
  state <- initializeState
  forkIO $ runTotalizer state
  forkIO $ runReporter state
  forkIO $ runReaderT runBot state

  forever (threadDelay maxBound)

-- loop :: IO ()
-- loop = do
--   line <- getFile "> "
--   case line of
--     Nothing -> loop
--     Just "quit" -> return ()
--     Just "prev" -> undoMove >> loop
--     Just ""     -> doMove >> loop

-- doMove :: IO ()
-- doMove = do
--   moves <- readMoves
--   let idk = putStrLn "Need a filename to proceed."
--   case moves of
--     [] -> idk 
--     [Twelve _] -> idk
--     [Lower file] -> do
--       putStrLn "Applying Twelve..."
--       apply (Twelve file)
--   loop

-- undoMove :: IO ()
-- undoMove = do
--   moves <- readMoves
--   case moves of
--     [] -> return ()
--     (move:ms) -> do
--       unapply move
--       writeMoves ms

-- unapply :: Move -> IO ()
-- unapply = applyWith (-)


-- apply :: Move -> IO ()
-- apply = applyWith (+)


-- applyWith :: Num a => (a -> a -> a) -> Move -> IO ()
-- applyWith comb move = do
--   scores <- case move of
--     Lower file -> do
--       s <- readScores file 
--       return $ Map.delete 12 s
--     Twelve file -> do
--       s <- readScores file
--       return $ Map.fromList [(12, s ! 12)]

--   totals <- readTotal
--   let t' = Map.unionWith comb totals scores
--   writeTotal totals
--   addMove move


-- setupTotal :: IO ()
-- setupTotal = do
--   exists <- doesFileExist totalsFile
--   unless exists $ writeTotal mempty

-- setupMoves :: IO ()
-- setupMoves = do
--   exists <- doesFileExist movesFile
--   unless exists $ writeMoves mempty

-- --------------------------------------------------------------------------------
-- -- Reading and writing scores

-- writeTotal :: Map String Integer -> IO ()
-- writeTotal = writeScores totalsFile 

-- readTotal :: IO (Map String Integer)
-- readTotal = readScores totalsFile

-- writeScores :: (Show a, Show b, Ord a) => String -> Map a b -> IO ()
-- writeScores file map = writeFile file $ showMap map

-- -- Read scores from a file.
-- readScores :: (Read a, Read b, Ord a) => String -> IO (Map a b)
-- readScores file = readMap <$> readFile file

-- -- Serialization and deserialization of scores
-- readMap :: (Read a, Read b, Ord a) => String -> Map a b
-- readMap = lines >>> fmap lineToPair >>> Map.fromList
--   where lineToPair s = let [x,y] = words s in (read x, read y)

-- showMap :: (Show a, Show b, Ord a) => Map a b -> String
-- showMap = Map.toAscList >>> fmap pairToLine >>> unlines
--   where pairToLine (x,y) = unwords [show x, show y]

-- -- WARNING will discard duplicate RHS entries!
-- invertMap :: (Ord a, Ord b) => Map a b -> Map b a
-- invertMap = Map.toAscList >>> fmap (\(a,b) -> (b,a)) >>> Map.fromList

-- --------------------------------------------------------------------------------
-- -- Convenience functions for haskeline
-- getLine :: String -> IO (Maybe String)
-- getLine = runInputT defaultSettings . getInputLine

-- getFile :: String -> IO (Maybe String)
-- getFile = runInputT (setComplete completeFilename defaultSettings) . getInputLine


-- --------------------------------------------------------------------------------
-- -- Reading and writing moves

-- readMoves :: IO [Move]
-- readMoves =  readFile movesFile & fmap (lines >>> fmap read)

-- writeMoves :: [Move] -> IO ()
-- writeMoves = fmap show >>> unlines >>> writeFile movesFile

-- addMove :: Move -> IO ()
-- addMove move = do
--   ms <- readMoves
--   writeMoves (move:ms)
