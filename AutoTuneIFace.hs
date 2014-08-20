{-# LANGUAGE OverloadedLists, ScopedTypeVariables, GADTs #-}

---------------------------------------------------------------------------------------
-- EXISTING LIBRARIES (on Hackage):

-- From SPECIFIC to GENERAL search:

--   optimization  -- numerical, gradient descent
--   combinatorial-problems -- Sat/TSP

--   local-search -- "representing metaheuristics as generators of solutions"


---------------------------------------------------------------------------------------

import Data.Vector as V

-- Strawman interface for Tunees.

class Monad m => TreeTuner m where
  -- Given an (inclusive,exclusive) range, make a choice.
  discreteChoice   :: Num a        => (a,a) -> m a
  continuousChoice :: Fractional a => (a,a) -> m a

  -- For convenience, we provide a default implementation of Enum choices:
  enumChoice :: forall a . (Bounded a, Enum a) => m a 
  enumChoice = do 
    let mn, mx :: a 
        (mn,mx) = (minBound,maxBound)
        rng :: (Int,Int)
        rng = (fromEnum mn, fromEnum mx)
    n <- discreteChoice rng 
    return $! toEnum n

class TreeTuner m => NestedTuner m where
  -- A nested search for a block of choices
  nestedBlockChoice :: Num a => Vector (a,a) -> m (Vector a)

--  nestedFutureBlockChoice :: Num a => Vector (a,a) -> m (Future (Vector a))


---------------------------------------------------------------------------------------
-- TODO: Algebra of techniques?

-- Technique transformers ? Techniques as a monoid?

{- 
  eval :: Params -> Double
  nbrs :: Params -> Vector Params
  apply :: Vector Params -> ParamSpace -> SearchTechnique

  -- Probabilistic: 
  nbrs :: Params -> IO Params

-}

-- TODO: Pruning -- when do we give up?

-- TODO: Eager (pre)evalutaion before runtime?


---------------------------------------------------------------------------------------
-- TODO: Compiler construction interface (passes):

-- 
-- composing compilers/passes results 

-- nestedBlockChoice in leaves
--   * code generation functions could compose and return a function of many parameters
--   * some potential sharing between block searches on common parameters


-- function from input to params, cont 

type Tunable0 inp out = (ParamSpace, inp -> out)

-- | One-phase tunable computations:
type Tunable1 inp out = inp -> (ParamSpace, Params -> out)

-- type ReTunable inp out = inp -> (out, ParamSpace, Params -> out)

type TunableN inp out = inp -> InProgress out
data InProgress out = Yield (ParamSpace, Params -> out)
                    | Done  out

pass1 :: Tunable1 X Y
pass1 = undefined
 
pass2 :: Tunable1 Y Z
pass2 = undefined

-- pass2 . pass1 
cont :: Params -> Y
spc :: ParamSpace
(spc,cont) = pass1 X
-- Uh oh, can't call pass2 until we make some arbitrary choice for spc

-- Reversible Jump -- use a Map for each Paramspace
--  when we change the Params for pass1, we pull over old 
--  Params for pass2 


-- pass1B :: ReTunable1 X Y
-- pass1B = undefined

-- pass3 . pass1B

-- frontend :: Acc -> GpuIR
-- tune    :: Tunable1 GPUIR GPUIR
-- codegen :: GPUIR -> CUDA

---------------------------------------------------------------------------------------

data Params      = Params [Int]
data ParamSpace  = ParamSpace [(Int,Int)]
data Future a = Future a

-- 


data Blah = Foo | Bar | Baz deriving (Read,Show,Eq,Enum,Bounded)

test0 :: TreeTuner m => m Int
test0 = do 
  -- This forces backtracking strategy only:
  vec1 <- forM [1..100] $ \_ -> discreteChoice (0,10)
  return (vec1!0)

test1 :: TreeTuner m => m Blah
test1 = enumChoice 

test2 :: NestedTuner m => m Int
test2 = do 
  -- This one can enable a nested search, e.g. GA, inside the outer
  -- backtracking search:
  vec2 <- nestedBlockChoice (V.replicate 100 (0,10))
  return (vec2!0)

---------------------------------------------------------------------------------------
-- PROBLEMS with the above strawman:

-- It puts an unnecessary ORDERING on choices (backtracking monad)
--  Even with things like fissioning 


-- data MidIR = Map Exp MidIR | Fold Exp MidIR 
--            | Split MidIR | Concat MidIR MidIR
--            | Let ([Var],MidIR) MidIR | V Var

type MidIR = [([Var],AExp)]
data AExp  = V Var 
           | Map Exp Var | Fold Exp Var
           | Split Var | Concat Var Var

type Var = String
data Exp

f :: Exp
f = undefined
g :: Exp
g = undefined

-- Example Recipe->Recipe transition:
{- 
arr = undefined 
prog1 = Map f (Fold g arr)
prog2 = Let (["a1","a2"], Split (Fold g arr)) $
        Concat (Map f (V "a1"))
               (Map f (V "a2")) -}
prog1 :: MidIR
prog1 = [(["a0"], Fold g "const"),
         (["a1"], Map f "a0")]

prog2 :: MidIR
prog2 = [(["a0"], Fold g "const"), 
         (["a0a","a0b"], Split "a0"),
         (["a1a"], (Map f "a0a")),
         (["a1b"], (Map f "a0b")),
         (["a1"],  Concat "a1a" "a1b")]
prog3 = undefined -- fiss Fold, not Map
prog4 = undefined -- fiss Fold & Map
-- prog1000 ...

type FissLog = [OneFiss]
data OneFiss = OneFiss { target :: Var, newNames :: (Var,Var) }

-- prog2 = gen(prog1,r2)

r1 :: Recipe X Z
r1 = Compose MapF FoldG

r2 :: Recipe X Z
r2 = Compose (Fiss MapF MapF) FoldG

r3 :: Recipe X Z
r3 = Compose MapF (Fiss FoldG FoldG)

r4 :: Recipe X Z
r4 = Compose (Fiss MapF MapF) (Fiss FoldG FoldG)

-- p4 = Compose (Fiss MapF MapF) (Fiss FoldG FoldG)
-- p5 = (Fiss MapF FoldG)


data Recipe a b where
  Compose :: Recipe b c -> Recipe a b -> Recipe a c
  Fiss    :: Recipe a b -> Recipe a b -> Recipe a b
  MapF    :: Recipe Y Z
  FoldG   :: Recipe X Y

data X = X
data Y = Y 
data Z = Z
 
--            | Target Who
-- data Who = MapF | FoldG 
-- data Who = String -- a1

-- Rewrite rules:
--    map f a -> 

-- type Recipe = MidIR -- Infinite space of rewrite transitions...
-- type Recipe = (Bool,Bool) -- fixed cartesian space...

-- data Split = Split { who :: Var }

-- data Decision = Fuse (String,


score :: Recipe a b -> IO Double 
score = undefined

transition :: Recipe a b -> Distribution (Recipe a b)
transition = undefined 

-- data Recipe = ... | ... ??

data Distribution a 

-- transition :: Recipe -> IO Recipe 


---------------------------------------------------------------------------------------
-- Type driven approach

-- type Recipe = (OptLevel OneToThree, GCCFlags, GCCNumericParams)
-- data





