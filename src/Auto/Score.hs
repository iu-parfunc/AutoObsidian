{- 
    Info: 

-} 
 
module Auto.Score where


type ScoreType = Double 

class Score a where
  score :: a -> ScoreType


