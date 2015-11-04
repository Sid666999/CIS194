module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge       
  | Lt  
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend state var val = \x -> if x == var then val else state x

empty :: State
empty _ = 0

-- Exercise 2 -----------------------------------------
boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0                 

evalE :: State -> Expression -> Int
evalE state (Var str) = state str
evalE state (Val v)   = v
evalE state (Op exp1 bop exp2)
    | bop == Plus   = evalE state exp1 +     evalE state exp2
    | bop == Minus  = evalE state exp1 -     evalE state exp2
    | bop == Times  = evalE state exp1 *     evalE state exp2
    | bop == Divide = evalE state exp1 `div` evalE state exp2
    | bop == Gt     = boolToInt (evalE state exp1 >     evalE state exp2)
    | bop == Ge     = boolToInt (evalE state exp1 >=    evalE state exp2)
    | bop == Lt     = boolToInt (evalE state exp1 <     evalE state exp2)
    | bop == Le     = boolToInt (evalE state exp1 <=    evalE state exp2)
    | bop == Eql    = boolToInt (evalE state exp1 ==    evalE state exp2)


-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign var exp) = DAssign var exp
desugar (Incr   var)     = DAssign var (Op (Var var) Plus (Val 1))
desugar (If exp ifStat elseStat) = DIf exp (desugar ifStat) (desugar elseStat)
desugar (While exp stat) = DWhile exp (desugar stat)
desugar (For initStat condExp updateStat bodyStat) = desugar (Sequence initStat (While condExp (Sequence bodyStat updateStat)))
desugar (Sequence stat1 stat2) = DSequence (desugar stat1) (desugar stat2)
desugar Skip             = DSkip
                               


-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple state (DAssign var exp) = extend state var (evalE state exp)
evalSimple state (DIf exp ifDietStat elseDietStat) = if evalE state exp /= 0
                                                     then (evalSimple state ifDietStat)
                                                     else (evalSimple state elseDietStat)
evalSimple state (DWhile exp stat) = if evalE state exp /= 0
                                     then evalSimple (evalSimple state stat) (DWhile exp stat)
                                     else state
evalSimple state (DSequence stat1 stat2) = evalSimple (evalSimple state stat1) stat2
evalSimple state DSkip = state
                                                     
run :: State -> Statement -> State
run state statement = evalSimple state (desugar statement)

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
