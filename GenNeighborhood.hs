
-- IR datatype from last meeting

type MidIR = [([Var], AExp)]
data AExp  = V Var
           | Map Exp Var | Fold Exp Var
           | Split Var   | Concat Var Var
           deriving (Show)

type Var = Integer
data Exp = Exp
         deriving (Show)

f :: Exp
f = undefined

g :: Exp
g = undefined


-- one example program and a permutation

prog1 :: MidIR
prog1 = [([1], Fold g 0),
         ([2], Map f 1)]

prog2 :: MidIR
prog2 = [([1], Fold g 0),
         ([3,2], Split 1),
         ([5], (Map f 2)),
         ([4], (Map f 3)),
         ([6],  Concat 5 4)]



-- generating some neighbors

nextNum = (+1) . maximum . map (maximum . nums)
  where nums (l, Fold _ v)   = v : l
        nums (l, Split v)    = v : l
        nums (l, Map _ v)    = v : l
        nums (l, Concat v n) = n : v : l

walkCode _   []   = []
walkCode gen code = code'
  where code' = case elem of
          Just x -> x ++ tail code
          Nothing -> head code : walkCode gen (tail code)
        elem  = gen $ head code

splits code = walkCode gen code
  where num = nextNum code
        gen ([n], Map f n1)  = Just [(makeSplit n1),
                                   ([num+2], Map f num), 
                                   ([num+3], Map f (num+1)),
                                   ([n], Concat (num+2) (num+3))]
        gen ([n], Fold f n1) = Just [(makeSplit n1),
                                   ([num+1], Map f num),
                                   ([num+3], Map f (num+1)),
                                   ([n], Concat (num+2) (num+3))]
        gen _              = Nothing
        makeSplit n        = ([num, num+1], Split n)
        
        

