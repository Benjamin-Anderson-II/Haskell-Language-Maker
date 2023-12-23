-- Group Members: Benjamin Anderson II - 934-353-159

-- Due Data: May 23, 2023

-- This code is for HW5 from CS_381 @ Oregon State University in Spring 2023
-- The intention of this assignment is to practice types and semantics

module HW5sol where
import HW5types

-- applies program to stack
run :: Prog -> Stack -> Result -- THIS NEEDS TO BE FIXED NOW
run [] s = A s
run (p:ps) s = case rankP (p:ps) (length s) of
    Nothing   -> RankError
    otherwise -> case semCmd p s of
        Just a  -> run ps a
        Nothing -> TypeError

-- semantics
semCmd :: Cmd -> Stack -> Maybe Stack
semCmd a b = case (a, b) of
    ((LDI n), s)                  -> Just $ (I n):s
    ((LDB b), s)                  -> Just $ (B b):s
    (LEQ, (I x):(I y):s)          -> Just $ (B $ x<=y):s
    (ADD, (I x):(I y):s)          -> Just $ (I $ x+y):s
    (MULT, (I x):(I y):s)         -> Just $ (I $ x*y):s
    (DUP, x:s)                    -> Just $ x:x:s
    (DEC, (I x):s)                -> Just $ (I $ x-1):s
    (SWAP, (x:y:s))               -> Just $ y:x:s
    ((POP k), s)                  -> Just $ [ v | (_,v) <- filter(\(x,_) -> x > k) $ zip [1..] s]
    ((IFELSE p1 p2), ((B x):s))   -> 
        if x then 
            case run p1 s of 
                TypeError -> Nothing
                A s -> Just s
        else 
            case run p2 s of
                TypeError -> Nothing
                A s -> Just s
    otherwise                     -> Nothing


-- Get the rank of a program
rankP :: Prog -> Rank -> Maybe Rank
rankP [] r = Just r
rankP (c:p) r = case rankC c r of
    Just a -> rankP p a
    Nothing -> Nothing


-- Input:
--     Rank -> Bool: Condition for which command can take place
--     Rank -> Rank: What is to be done with the Rank
--     Rank:         The rank which is to be changed

-- Output:
--     Maybe Rank: 
--         Nothing if condition failed,
--         modified rank if condition passed

check :: (Rank -> Bool) -> (Rank -> Rank) -> Rank -> Maybe Rank
check test next x =
    if test x then
        Just $ next x
    else
        Nothing


-- Adjust input rank depending on command given
rankC :: Cmd -> Rank -> Maybe Rank
rankC cmd r = case cmd of
    LDI _        -> check(const True) succ r
    LDB _        -> check(const True) succ r
    LEQ          -> check(>= 2) pred r
    ADD          -> check(>= 2) pred r
    MULT         -> check(>= 2) pred r
    DUP          -> check(>= 1) succ r
    DEC          -> check(>= 1) (+0) r
    SWAP         -> check(>= 2) (+0) r
    POP k        -> check(>= k) (subtract k) r
    IFELSE p1 p2 -> case (rankP p1 (r-1), rankP p2 (r-1)) of
        (_, Nothing)     -> Nothing
        (Nothing, _)     -> Nothing
        (Just a, Just b) -> Just $ min a b