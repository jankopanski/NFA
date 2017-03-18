module Auto
( Auto
, accepts
, emptyA
, epsA
, symA
, leftA
, sumA
, thenA
, fromLists
, toLists
) where

import Data.List

data Auto a q = A { states      :: [q]
                  , initStates  :: [q]
                  , isAccepting :: q -> Bool
                  , transition  :: q -> a -> [q]
                  }

-- notation for automata
-- A = (st, is, ia, tr)

run :: Eq q => Auto a q -> [a] -> [q]
run (A _ is _ tr) = foldl (\l a -> nub [ q' | q <- l, q' <- tr q a ]) is

join :: [q1] -> [q2] -> [Either q1 q2]
join st1 st2 = map Left st1 ++ map Right st2

accepts :: Eq q => Auto a q -> [a] -> Bool
accepts aut@(A _ _ ia _) w = any ia (run aut w)

emptyA :: Auto a ()
emptyA = A [] [] (const False) (\_ _ -> [])

epsA :: Auto a ()
epsA = A [()] [()] (const True) (\_ _ -> [])

symA :: Eq a => a -> Auto a Bool
symA c = A [False, True] [False] id (\q a -> [ True | not q && a == c ])

leftA :: Auto a q -> Auto a (Either q r)
leftA (A st is ia tr) = A st' is' ia' tr'
  where
    st' = map Left st
    is' = map Left is
    ia' = either ia (const False)
    tr' (Left q) a = map Left $ tr q a
    tr' (Right _) _ = []

sumA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
sumA (A st1 is1 ia1 tr1) (A st2 is2 ia2 tr2) = A st is ia tr
  where
    st = join st1 st2
    is = join is1 is2
    ia = either ia1 ia2
    tr (Left q) a = map Left $ tr1 q a
    tr (Right q) a = map Right $ tr2 q a

thenA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
thenA (A st1 is1 ia1 tr1) (A st2 is2 ia2 tr2) = A st is ia tr
  where
    st = join st1 st2
    is = join is1 is2
    ia = either ia1 ia2
    tr (Left q) a = let s = tr1 q a in
      map Left s ++ if any ia1 s then map Right is2 else []
    tr (Right q) a = map Right $ tr2 q a

fromLists :: (Eq q, Eq a) => [q] -> [q] -> [q] -> [(q,a,[q])] -> Auto a q
fromLists st is ia tl = A st is (`elem` ia) tr
  where tr q a = maybe [] (\(_, _, s) -> s) (find (\(q', a', _) -> q == q' && a == a') tl)

toLists :: (Enum a,Bounded a) => Auto a q -> ([q],[q],[q],[(q,a,[q])])
toLists (A st is ia tr) = (st, is, filter ia st, tl)
  where tl = [ (q, a, s) | q <- st, a <- [minBound..], let s = tr q a, not $ null s ]
-- TODO jak to przyśpieszyć?

instance (Show a, Enum a, Bounded a, Show q) => Show (Auto a q) where
  show = show . toLists

ex1 :: Auto Char Int
ex1 = A [1,2,3] [1] (== 3) t
  where
    t 1 'a' = [2]
    t 2 'b' = [3]
    t 3 'b' = [2]
    t _ _ = []

ex2 :: Auto Char Int
ex2 = A [1,2,3] [1] (/= 1) t
  where
    t 1 'a' = [2,3]
    t 2 'b' = [3]
    t 3 'b' = [3]
    t _ _ = []

ex3 = thenA (leftA ex1) (leftA ex2)

ex4 :: Auto Bool Char
ex4 = A ['a', 'b', 'c'] ['a'] (== 'c') t
  where
    t 'a' True = ['b']
    t 'b' False = ['c']
    t 'c' False = ['b']
    t _ _ = []
