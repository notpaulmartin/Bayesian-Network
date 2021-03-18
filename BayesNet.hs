-- File:          Net.hs
-- Project:       BayesNet implementation
-- File Created:  Wednesday, 17th March 2021 10:50:48 am
-- Author(s):     Paul Martin
--
-- Last Modified: Wednesday, 17th March 2021 10:50:49 am
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


--                         Parent  Probability of inhibition
data Inhibitor = Inhibitor RV      Prob | UnInhibited

instance Eq Inhibitor where
    (Inhibitor (BoolRV a _ _) p1) == (Inhibitor (BoolRV b _ _) p2) =
        a == b && p1 == p2

type Inhibitors = Map RV Prob

-- data PrimBoolRV =

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

-- P(a) = P(a, b) + P(a, ¬b) | for all b in Parents(a)
-- p [state] [] = p (state : [ARB a | a <- parents (rvOf state)]) []

-- P(a,¬b,C,D)
p states [] | not (null parentsNotContained) = p (states ++ arbStates) []
            | containsArbs = sum [p pureStates [] | pureStates <- dissociate states ] -- Marginalise
            | otherwise = product [pTable state (trueParents (rvOf state)) | state <- states ]
    where
        trueParents :: RV -> [RV]
        trueParents x = [ a | (IS a) <- states, a `elem` parents x ]  -- parents of x that are true in states

        rvsInStates, allParents :: [RV]
        rvsInStates = map rvOf states

        allParents = foldl' union [] [parents a | a <- rvsInStates]

        parentsNotContained :: [RV]
        parentsNotContained = [parent | parent <- allParents, parent `notElem` rvsInStates]
        arbStates = [ARB a | a <- parentsNotContained]

        containsArbs :: Bool
        containsArbs = or [True | (ARB _) <- states]


-- P(a | xs)
p [state] conds | allCondsAreParents = pTable state positiveConds  -- Simple table lookup
                | otherwise = undefined   -- TODO: Introduce arbitrary variables and re-call
    where
        allCondsAreParents :: Bool
        allCondsAreParents = and [rvOf a `elem` parents (rvOf state) | a <- conds]

        hasAllParents, hasArbConds :: Bool
        hasAllParents = length (parents (rvOf state)) <= length conds
        hasArbConds = or [True | (ARB _) <- conds]

        positiveConds :: [RV]
        positiveConds = [a | cond@(IS a) <- conds]


p states conds | not hasAllRVs = -10
                -- All parents in conds
               | hasArbConds = 0
                -- No arbitrary RVs
               | otherwise = 10
    where
        allParents :: [RV]
        allParents = foldl' union [] [parents (rvOf a) | a <- states]
        family = (map rvOf states) `union` allParents -- All RVs in states, and their parents

        hasAllRVs, hasArbConds :: Bool
        -- Is entire family contained in states and conds
        hasAllRVs = length family <= length (map rvOf states `union` map rvOf conds)
        hasArbConds = or [True | (ARB _) <- conds]



-- Conditional Probability P(as | xs)
--       as          xs
prob :: [BoolRV] -> [RV] -> Prob

prob [] _ = 1

-- Single primitive boolean RV (no conditions)
prob [PrimBoolRV _ p] _ = p

-- Single non-primitive boolean RV (no conditions)
-- P(a) = P(a | parents a) P(parent_1) ... P(parent_n)
--      = (1 - P(¬a | parents a)) P(p_1) ... P(p_n)
prob [a] [] = (1 - a `inhibitionBy` parents a) * prob (parents a) []

-- Single boolean RV (conditions)
-- P(a|xs) = P(a) / (P(x_1) ... P(x_n))
prob [a] xs
    | not (null xsInParents) =
        (1 - a `inhibitionBy` xsInParents) / product [prob [x] [] | x <- xsNotInParents]
    | otherwise = prob [a] [] / product [prob [x] [] | x <- xs]
    where
        xsInParents = filter (`elem` parents a) xs
        xsNotInParents = filter (`notElem` parents a) xs

-- Multiple boolean RVs (no conditions)
-- P(as) = product P(a | parents a)
prob as [] = product [prob [a] (parents a) | a <- as]

prob as xs = prob as [] / product [prob [x] [] | x <- xs]

