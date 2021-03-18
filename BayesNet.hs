-- File:          Net.hs
-- Project:       BayesNet implementation
-- File Created:  Wednesday, 17th March 2021 10:50:48 am
-- Author(s):     Paul Martin
--
-- Last Modified: Thursday, 18th March 2021 18:10 pm
-- Modified By:   Paul Martin

module BayesNet where

import           Data.List
import           Data.Map    (Map)
import qualified Data.Map    as Map
import           Debug.Trace

annotate :: (Show a) => String -> a -> a
annotate s a = trace (s ++ show a) a

-- fromJust with a default given
fromJust' :: a -> Maybe a -> a
fromJust' x Nothing  = x
fromJust' _ (Just y) = y

-- Probability
type Prob = Float

type Inhibitors = Map RV Prob

data BoolRV =
    BoolRV
        String      -- name
        [RV]        -- parents
        Inhibitors  -- inhibitors
    | PrimBoolRV
        String      -- name
        Prob        -- P(a)

parents :: RV -> [RV]
parents (BoolRV _ xs _)  = xs
parents (PrimBoolRV _ _) = []

inhibitors :: BoolRV -> Inhibitors
inhibitors (BoolRV _ _ xs)  = xs
inhibitors (PrimBoolRV _ _) = Map.empty

instance Eq BoolRV where
    (BoolRV a x1s y1s) == (BoolRV b x2s y2s) =
        a == b && x1s == x2s && y1s == y2s
    (PrimBoolRV a x) == (PrimBoolRV b y) =
        a == b && x == y
    _ == _ = False
instance Ord BoolRV where
    compare (BoolRV a _ _) (BoolRV b _ _)     = compare a b
    compare (PrimBoolRV a _) (PrimBoolRV b _) = compare a b
    compare PrimBoolRV{} BoolRV{}             = LT
    compare BoolRV{} PrimBoolRV{}             = GT
instance Show BoolRV where
    show (BoolRV x _ _)   = x
    show (PrimBoolRV x _) = x


type RV = BoolRV

data State = IS RV  -- a: A=true
    | NOT RV -- ¬a: A=false
    | ARB RV -- (arbitrary) A = true OR false
    deriving (Eq)
instance Show State where
    show (IS a)  = show a
    show (NOT a) = "¬" ++ show a
    show (ARB a) = "?" ++ show a

neg :: State -> State
neg (IS a)  = NOT a
neg (NOT a) = IS a
neg x       = x

negateStates :: [State] -> [State]
negateStates = map neg

rvOf :: State -> RV
rvOf (IS a)  = a
rvOf (NOT a) = a
rvOf (ARB a) = a

dissociate :: [State] -> [[State]]
dissociate states = perms arbs [notArbs]
    where
        -- Generates list of all permutations
        perms :: [RV] -> [[State]] -> [[State]]
        perms [] statess = statess
        perms (a:as) statess = perms as (concat [ [states ++ [IS a], states ++ [NOT a]] | states <- statess])

        arbs :: [RV]
        arbs = [a | (ARB a) <- states]

        notArbs :: [State]
        notArbs = filter (\x -> rvOf x `notElem` arbs) states


-- P(¬a | xs), where x in Parents(a)
-- default: 1 (always inhibited, if no evidence for it)
inhibitionBy :: RV -> [RV] -> Prob
inhibitionBy (PrimBoolRV _ p) _ = 1-p  -- P(¬a) = 1 - P(a) = 1-x
inhibitionBy a []               = 1    -- P(a) = 0 | a not primitive
inhibitionBy a [x]              = fromJust' 1 (Map.lookup x (inhibitors a))
inhibitionBy a (x:xs)           = inhibitionBy a [x] * inhibitionBy a xs


pTable :: State -> [RV] -> Prob
pTable (NOT (PrimBoolRV _ x)) _ = 1 - x  -- P(¬a) = 1 - P(a) = 1-x
pTable (NOT a) []               = 1      -- P(a) = 0 | a not primitive
pTable (NOT a) xs               = a `inhibitionBy` xs
pTable (IS a) xs                = 1 - pTable (NOT a) xs
pTable (ARB _) _                = 1


p :: [State] -> [State] -> Prob
p [] _                      = 1

p [IS (PrimBoolRV _ x)] []  = x
p [NOT (PrimBoolRV _ x)] [] = 1 - x

-- P(a,¬b,C,D)
p states [] | not (null parentsNotContained) = p (states ++ arbStates) []
            | containsArbs = sum [p pureStates [] | pureStates <- dissociate states ] -- Marginalise
            | otherwise = product [pTable state (trueParents (rvOf state)) | state <- states ]
    where
        trueParents :: RV -> [RV]
        trueParents x = [ a | (IS a) <- states, a `elem` parents x ]  -- parents of x that are true in states

        rvsInStates, allParents, parentsNotContained :: [RV]
        rvsInStates = map rvOf states
        allParents = foldl' union [] [parents a | a <- rvsInStates]
        parentsNotContained = [parent | parent <- allParents, parent `notElem` rvsInStates]

        arbStates :: [State]
        arbStates = [ARB a | a <- parentsNotContained]

        containsArbs :: Bool
        containsArbs = or [True | (ARB _) <- states]

-- P(a,b|c)
p states conds = alpha * p (states `union` conds) []
    where
        alpha :: Float
        alpha = 1 / (p (states `union` conds) [] + p (negateStates states `union` conds) [])

