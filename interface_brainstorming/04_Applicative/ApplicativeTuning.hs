{-# LANGUAGE NamedFieldPuns #-}
-- | This demonstrates how to use Applicative rather than Monad to
-- provide a Reader-like construct for accessing tunable parameters.
-- Using Applicative means that we can ascertain exactly what tunables
-- are used *before* running the computation.

module ApplicativeTuning
 (
   -- * The abstract datatype for tunable computations
   Tune
 , allParams
 , getParam
   -- * Running and searching
 , tune
 , runMin
   -- * Temporary
 , test
 ) where

import Control.Applicative
import System.Random.MWC

-- | We restrict tunable parameters to Int values for now.
type Params = [Int]

type Score = Double

-- | The datatype for (auto)tunable computations.
data Tune m a =
   -- TODO: run needs to return "m a" and Tune should be parameterized over "m".
     Tune { _run :: (Params -> m a)
            -- ^ Run a tunable computation
          , allParams :: [Range] }

instance Show (Tune m a) where
  show (Tune {allParams}) =
    "<Tunable computation with params "++show allParams++">"

instance Monad m => Functor (Tune m) where
  fmap f (Tune g l) = Tune (\p -> do x <- g p; return (f x)) l

instance Monad m => Applicative (Tune m) where
  pure x = Tune (\[] -> return x) []
  Tune f l1 <*> Tune g l2 =
    let len1 = length l1
    in Tune (\ls -> let (x,y) = splitAt len1 ls
                    in do f' <- f x
                          v  <- g y
                          return $ f' v
            )
            (l1 ++ l2)

effort :: Int
effort = 1000

-- | Run a tunable computation and retrieve the best found tuning
-- parameters and their score, returning them in addition to the result.
--
-- Note, if the underlying monad were something OTHER than IO, then
-- the tuner would need to be able to run it, so as to extract scores.
tune :: Tune IO (a,Score) -> IO (a, Params, Score)
tune (Tune f ranges) =
  -- Currently this does a simple random search:
  withSystemRandom $ asGenIO $ \gen ->
   do strt <- go gen
      loop gen strt effort
  where
  loop _gen best 0 = return best
  loop gen best@(_,_,bscr) n =
    do new@(_,_,nscr) <- go gen
       if nscr > bscr
          then loop gen new (n-1)
          else loop gen best (n-1)
  go gen =
    do params <- mapM (`uniformR` gen) ranges
       (a,sc) <- f params
       return (a,params,sc)

-- | A quick way to run with all the *minimum* parameter settings,
-- i.e. without performinging any tuning.
runMin :: Tune m a -> m a
runMin (Tune f ls) = f (map fst ls)

-- | Fetch an integer in the range
getParam :: Monad m => Range -> Tune m Int
getParam r = Tune (\[p] -> return p) [r]

-- | Inclusive ranges
type Range = (Int,Int)


example1 :: Tune IO Int
example1 =
  (-) <$> getParam (1,10) <*> getParam (0,3)

example2 :: Tune IO Double
example2 =
  (\x y -> fromIntegral (x+y)) <$> getParam (11,12) <*> getParam (13,14)

-- Illustrate composability:
example3 :: Tune IO ((Int,Double),Score)
example3 = (\x y -> ((x,y), fromIntegral x + y))
           <$> example1 <*> example2

-- Illustrate using the underlying monad:
example4 :: Tune IO ((Int,Double),Score)
example4 =
  (\x _ -> x) <$>
  example3 <*>
  lift (putStrLn "IO inside example4")

lift :: m a -> Tune m a
lift act = Tune (\[] -> act) []

test :: IO ()
test =
  do putStrLn "First, runMin:"
     x <- (runMin example1)
     y <- (runMin example2)
     z <- (runMin example3)
     putStrLn $ "  example1: "++show x
     putStrLn $ "  example2: "++show y
     putStrLn $ "  example3: "++show z

     w <- (runMin example4)
     putStrLn $ "  example4: "++show w

     putStr "Next, random search\n  "
     a <- tune example3
     print a
     return ()
