module Score where

{- 
A Score is something of the general shape XYZ ##, ie a country code and a 
number of points.

Scores are the only way that users can interact with the bot.
-}
  
import Data.Attoparsec.Text hiding (take)
import Text.Read (readMaybe)
import Data.Char (isSpace)

import Data.Text (Text)

import Data.List (elemIndex)
import Data.Maybe (fromJust)

import qualified Data.Map as Map

import Control.Applicative
import Data.Function ((&))

import Types
import Countries

data Score = Score
  { countryCode :: CountryCode
  , value       :: Integer
  }

validScores :: [Integer]
validScores = [12,10,8,7,6,5,4,3,2,1,0]

--------------------------------------------------------------------------------
-- Parsing

parseScore :: Text -> Either String Score
parseScore = parseOnly scoreP

scoreP :: Parser Score
scoreP = valueCountryP <|> countryValueP
  where
    countryValueP =      Score <$> countryCodeP <*> (skipSpace >> valueP)
    valueCountryP = flip Score <$> valueP <*> (skipSpace >> countryCodeP)

countryCodeP :: Parser CountryCode
countryCodeP = do
  x <- many1 $ satisfy (not.isSpace)
  case readMaybe x :: Maybe CountryCode of
    Nothing -> fail $ "Invalid country code: " ++ x
    Just cc -> return cc

valueP :: Parser Integer
valueP = do
  x <- read <$> many1 digit
  if x `elem` validScores then return x
  else fail $ "Invalid score: " ++ show x
  

--------------------------------------------------------------------------------
-- Ballots

-- Here we assume that the integer in the score is "valid".
-- Note that we also have the special value 0, which represents removal.
updateBallot :: Ballot -> Score -> Ballot
updateBallot ballot Score{..} = 
  let
    deleteCode x y = if y == Just x then Nothing else y
    deleteNothings (_, Nothing) = []
    deleteNothings (x, Just y)  = [(x,y)]

    merge mid (l, r) = case r of
      (Nothing : rs) -> l ++ mid : rs
      rs             -> l ++ mid : rs
                  
  in validScores
    & fmap (`Map.lookup` ballot)
    & fmap (deleteCode countryCode)
    & splitAt (fromJust $ value `elemIndex` validScores)
    & merge (Just countryCode)
    & take (length validScores - 1)
    & zip validScores
    & map deleteNothings
    & concat
    & Map.fromDescList