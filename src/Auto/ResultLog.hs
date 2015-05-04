
{-|
Module      : Auto.ResultLog
Description : Logging of results 
Copyright   : (c) Bo Joel Svensson, 2015
                  Michael Vollmer, 2015
License     : GPL-3
Maintainer  : 
Stability   : experimental
Portability : 

Datastructures used to store result logs during search.
-}
module Auto.ResultLog where

-- | a forgetful lifo.
data FLIFO a =
  FLIFO {flifoLen  :: Maybe Int
        ,flifoData :: [a]
        }

mkFLIFO :: Maybe Int -> FLIFO a 
mkFLIFO l = FLIFO l []

push :: FLIFO a -> a -> FLIFO a
push (FLIFO n xs) a =
  case n of
    Nothing -> FLIFO Nothing (a:xs)
    (Just n') -> FLIFO n (take n' (a:xs))

peek :: FLIFO a -> Maybe a
peek (FLIFO _ []) = Nothing
peek (FLIFO _ (x:_)) = Just x
                     


data ResultLog result =
  ResultLog { resultLogBest :: FLIFO result
            , resultLogAll  :: Maybe (FLIFO result)
            }



-- | add a result to the log.
--   Takes a resultlog, a result and an iteration no. 
addResult :: Ord result
          => ResultLog result -> result ->  ResultLog result
addResult resLog res =
  resLog { resultLogBest = push (resultLogBest resLog) bestSoFar'
         , resultLogAll  =
           case resultLogAll resLog of
             Nothing -> Nothing
             Just lifo -> Just $ push lifo res }  

  where
    bestSoFar = peek (resultLogBest resLog)
    bestSoFar' =
      case bestSoFar of
        Nothing -> res
        (Just r) -> min r res

-- | The user needs to specify a CSV instance for the
-- result type in use
class CSV a where
  toCSVRow :: a -> String

-- | Gives two CSV formatted strings.
--   The top results and all. 
resultCSV :: CSV result => ResultLog result -> (String,Maybe String) 
resultCSV res = ( unlines $ map toCSVRow (flifoData bestRes)
                , case allRes of
                   Just r -> Just $ unlines $ map toCSVRow (flifoData r)
                   Nothing -> Nothing )
                
  where
    bestRes = resultLogBest res
    allRes  = resultLogAll  res 

