

module Auto.ResultLog where


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


addResult :: Ord result => ResultLog result -> result -> ResultLog result
addResult resLog res =
  resLog { resultLogBest = push (resultLogBest resLog) bestSoFar'
         , resultLogAll  =
           case resultLogAll resLog of
             Nothing -> Nothing
             (Just lifo) -> Just $ push lifo res }  

  where
    bestSoFar = peek (resultLogBest resLog)
    bestSoFar' =
      case bestSoFar of
        Nothing -> res
        (Just r) -> min r res

