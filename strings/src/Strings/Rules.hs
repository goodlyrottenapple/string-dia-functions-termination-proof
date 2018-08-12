module Strings.Rules where

import Protolude
import Prelude((!!))
import qualified Prelude as P

import Strings

import Data.Set(Set)
import qualified Data.Set as S

type Rule = Int -> Diagram -> Maybe Diagram


 --     x          x
 -- *   |          |
 -- |   |          |   *
 --  \ /           |   |
 --   X     --->   |   |
 --  / \           |   |
 -- |   |

r1 :: Rule
r1 _ (Diagram _ []) = Nothing
r1 _ (Diagram _ [_]) = Nothing
r1 0 (Diagram is ((n,Lolly a):(m,Twist):xs)) | n == m = Just $ Diagram is ((n+1,Lolly a):xs)
r1 n (Diagram is (x:xs)) = (\(Diagram is' xs') -> Diagram is' (x:xs')) <$> r1 (n-1) (Diagram is xs)


--  -- x                  x
--  -- |   *              |
--  -- |   |          *   |
--  --  \ /           |   |
--  --   X     --->   |   |
--  --  / \           |   |
--  -- |   |

r2 :: Rule
r2 _ (Diagram _ []) = Nothing
r2 _ (Diagram _ [_]) = Nothing
r2 0 (Diagram is ((n,Lolly a):(m,Twist):xs)) | n == m+1 = Just $ Diagram is ((n-1,Lolly a):xs)
r2 n (Diagram is (x:xs)) = (\(Diagram is' xs') -> Diagram is' (x:xs')) <$> r2 (n-1) (Diagram is xs)


--  -- x   y          x   y
--  -- |   |          |   |
--  --  \ /           |   |
--  --   X     --->   |   |
--  --  / \           |   |
--  --  \ /           |   |
--  --   X            |   |
--  --  / \
--  -- |   |


r3 :: Rule
r3 _ (Diagram _ []) = Nothing
r3 _ (Diagram _ [_]) = Nothing
r3 0 (Diagram is ((n,Twist):(m,Twist):xs)) | n == m = Just $ Diagram is xs
r3 n (Diagram is (x:xs)) = (\(Diagram is' xs') -> Diagram is' (x:xs')) <$> r3 (n-1) (Diagram is xs)



--  --     x          x
--  -- *   |          |
--  -- |   |          |
--  --  \ /          <*>
--  --   |     --->   |
--  --   |            |


r4 :: Rule
r4 _ (Diagram _ []) = Nothing
r4 _ (Diagram _ [_]) = Nothing
r4 0 (Diagram is ((n,Lolly b):(m,Cup):xs)) | n == m = Just $ Diagram is ((n,Diamond b):xs)
r4 n (Diagram is (x:xs)) = (\(Diagram is' xs') -> Diagram is' (x:xs')) <$> r4 (n-1) (Diagram is xs)




--  -- x              x
--  -- |   *          |
--  -- |   |          |
--  --  \ /           |
--  --   |     --->   |
--  --   |            |


r5 :: Rule
r5 _ (Diagram _ []) = Nothing
r5 _ (Diagram _ [_]) = Nothing
r5 0 (Diagram is ((n,Lolly _):(m,Cup):xs)) | n == m+1  = Just $ Diagram is xs
r5 n (Diagram is (x:xs)) = (\(Diagram is' xs') -> Diagram is' (x:xs')) <$> r5 (n-1) (Diagram is xs)


 -- x   y          x   y
 -- |   |          |   |
 --  \ /            \ /
 --   X              |
 --  / \            <*>
 -- |   |   --->     |
 --  \ /             |
 --   |


r6 :: Rule
r6 _ (Diagram _ []) = Nothing
r6 _ (Diagram _ [_]) = Nothing
r6 0 (Diagram is ((n,Twist):(m,Cup):xs)) | n == m  = Just $ Diagram is ((n,Cup):(n,Diamond (is!!(n+1))):xs)
r6 n (Diagram is (x@(m,g):xs)) = (\(Diagram _ xs') -> Diagram is (x:xs')) <$> r6 (n-1) (Diagram (updateInPorts m g is) xs)


 -- x   y   z                      x   y   z
 -- |   |   |                      |   |   |
 --  \ /    |                      |    \ /
 --   |     |          --->        |     |
 --    \   /                        \   /
 --     \ /                          \ /
 --      |                            |

r7 :: Rule
r7 _ (Diagram _ []) = Nothing
r7 _ (Diagram _ [_]) = Nothing
r7 0 (Diagram is ((n,Cup):(m,Cup):xs)) | n == m  = Just $ Diagram is ((n+1,Cup):(n,Cup):xs)
r7 n (Diagram is (x:xs)) = (\(Diagram is' xs') -> Diagram is' (x:xs')) <$> r7 (n-1) (Diagram is xs)



--  x          x
--  |          |
-- <*>         |
--  |   --->  <*>
-- <*>         |
--  |          |


r89 :: Rule
r89 _ (Diagram _ []) = Nothing
r89 _ (Diagram _ [_]) = Nothing
r89 0 (Diagram is ((n,Diamond _):(m,Diamond b):xs)) | n == m  = 
    if is!!n == b -- -b-<*>-a-<*>-b-
        then Just $ Diagram is xs
        else Just $ Diagram is ((m,Diamond b):xs)
r89 n (Diagram is (x@(m,g):xs)) = (\(Diagram _ xs') -> Diagram is (x:xs')) <$> r89 (n-1) (Diagram (updateInPorts m g is) xs)


 -- x   y          x   y
 -- |   |          |   |
 -- |  <*>          \ /
 -- |   |    --->    |
 --  \ /             |
 --   |              |

r10 :: Rule
r10 _ (Diagram _ []) = Nothing
r10 _ (Diagram _ [_]) = Nothing
r10 0 (Diagram is ((n,Diamond _):(m,Cup):xs)) | n == m+1 = Just $ Diagram is ((m,Cup):xs)
r10 n (Diagram is (x:xs)) = (\(Diagram is' xs') -> Diagram is' (x:xs')) <$> r10 (n-1) (Diagram is xs)


--  x   y          x   y
--  |   |          |   |
-- <*>  |           \ /
--  |   |    --->    |
--   \ /            <*>
--    |              |

r11 :: Rule
r11 _ (Diagram _ []) = Nothing
r11 _ (Diagram _ [_]) = Nothing
r11 0 (Diagram is ((n,Diamond b):(m,Cup):xs)) | n == m = Just $ Diagram is ((m,Cup):(n,Diamond b):xs)
r11 n (Diagram is (x:xs)) = (\(Diagram is' xs') -> Diagram is' (x:xs')) <$> r11 (n-1) (Diagram is xs)


--  *        *
--  |  --->  |
-- <*>       |
--  |


r12 :: Rule
r12 _ (Diagram _ []) = Nothing
r12 _ (Diagram _ [_]) = Nothing
r12 0 (Diagram is ((n,Lolly _):(m,Diamond b):xs)) | n == m = Just $ Diagram is ((n,Lolly b):xs)
r12 n (Diagram is (x:xs)) = (\(Diagram is' xs') -> Diagram is' (x:xs')) <$> r12 (n-1) (Diagram is xs)


--  x   y                 x   y
--  |   |                 |   |
-- <*>  |                  \ / 
--   \ /                    X
--    X         --->       / \
--   / \                  |  <*>
--  |   |                 |   |


r13 :: Rule
r13 _ (Diagram _ []) = Nothing
r13 _ (Diagram _ [_]) = Nothing
r13 0 (Diagram is ((n,Diamond a):(m,Twist):xs)) | n == m = Just $ Diagram is ((m,Twist):(n+1,Diamond a):xs)
r13 n (Diagram is (x:xs)) = (\(Diagram is' xs') -> Diagram is' (x:xs')) <$> r13 (n-1) (Diagram is xs)


 -- x   y                 x   y
 -- |   |                 |   |
 -- |  <*>                 \ / 
 --  \ /                    X
 --   X         --->       / \
 --  / \                 <*>  |
 -- |   |                 |   |

r14 :: Rule
r14 _ (Diagram _ []) = Nothing
r14 _ (Diagram _ [_]) = Nothing
r14 0 (Diagram is ((n,Diamond a):(m,Twist):xs)) | n == m+1 = Just $ Diagram is ((m,Twist):(n-1,Diamond a):xs)
r14 n (Diagram is (x:xs)) = (\(Diagram is' xs') -> Diagram is' (x:xs')) <$> r14 (n-1) (Diagram is xs)



 -- x   y   z                      x   y   z
 -- |   |   |                      |   |   |
 --  \ /    |                      |    \ /
 --   X     |          --->        |     X
 --  / \   /                        \   / \
 -- |    X                            X    |
 --  \ /   \                        /   \ /
 --   X     |                      |     X
 --  / \    |                      |    / \
 -- |   |   |                      |   |   |

r15 :: Rule
r15 _ (Diagram _ []) = Nothing
r15 _ (Diagram _ [_]) = Nothing
r15 _ (Diagram _ [_,_]) = Nothing
r15 0 (Diagram is ((n,Twist):(m,Twist):(o,Twist):xs)) | n+1 == m && n == o = Just $ Diagram is ((m,Twist):(n,Twist):(m,Twist):xs)
r15 n (Diagram is (x:xs)) = (\(Diagram is' xs') -> Diagram is' (x:xs')) <$> r15 (n-1) (Diagram is xs)



 -- x   y   z                      x   y   z
 -- |   |   |                      |   |   |
 --  \ /    |                      |    \ /
 --   X     |          --->        |     |
 --  / \   /                        \   /
 -- |   \ /                          \ /
 -- |    |                            |
 -- |   /                            <*>
 --  \ /                              |   
 --   |                               |

r16 :: Rule
r16 _ (Diagram _ []) = Nothing
r16 _ (Diagram _ [_]) = Nothing
r16 _ (Diagram _ [_,_]) = Nothing
r16 0 (Diagram is ((n,Twist):(m,Cup):(o,Cup):xs)) | n+1 == m && n == o = Just $ Diagram is ((m,Cup):(n,Cup):(n,Diamond (is!!m)):xs)
r16 n (Diagram is (x@(m,g):xs)) = (\(Diagram _ xs') -> Diagram is (x:xs')) <$> r16 (n-1) (Diagram (updateInPorts m g is) xs)



 -- x   y   z                      x   y   z
 -- |   |   |                      |   |   |
 --  \ /    |                      |    \ /
 --   X     |          --->        |     X
 --  / \   /                        \   / \
 -- |   \ /                          \ /   |
 -- |    |                            |    |
 --  \  /                             |    |
 --    X                              |    |
 --   / \                             |    |
 --  |   |                            |    |


r17 :: Rule
r17 _ (Diagram _ []) = Nothing
r17 _ (Diagram _ [_]) = Nothing
r17 _ (Diagram _ [_,_]) = Nothing
r17 0 (Diagram is ((n,Twist):(m,Cup):(o,Twist):xs)) | n+1 == m && n == o = Just $ Diagram is ((m,Twist):(n,Cup):xs)
r17 n (Diagram is (x:xs)) = (\(Diagram is' xs') -> Diagram is' (x:xs')) <$> r17 (n-1) (Diagram is xs)


 -- x   y   z                      x   y   z
 -- |   |   |                      |   |   |
 --  \ /    |                      |    \ /
 --   |     |          --->        |     X
 --    \   /                        \   / \
 --     \ /                          \ /   |
 --      X                            X    |
 --     / \                          / \   |
 --    |   |                        |   \ /
 --    |   |                        |    |

r18 :: Rule
r18 _ (Diagram _ []) = Nothing
r18 _ (Diagram _ [_]) = Nothing
r18 0 (Diagram is ((n,Cup):(m,Twist):xs)) | n == m = Just $ Diagram is ((n+1,Twist):(n,Twist):(n+1,Cup):xs)
r18 n (Diagram is (x:xs)) = (\(Diagram is' xs') -> Diagram is' (x:xs')) <$> r18 (n-1) (Diagram is xs)


 -- x   y   z                      x   y   z
 -- |   |   |                      |   |   |
 --  \ /    |                      |    \ /
 --   X     |          --->        |     |
 --  / \   /                        \   /
 -- |   \ /                          \ /
 -- |    X                            X
 -- |   / \                          / \
 --  \ /   |                        |   |
 --   |    |                        |   |

r19 :: Rule
r19 _ (Diagram _ []) = Nothing
r19 _ (Diagram _ [_]) = Nothing
r19 _ (Diagram _ [_,_]) = Nothing
r19 0 (Diagram is ((n,Twist):(m,Twist):(o,Cup):xs)) | n+1 == m && n == o = Just $ Diagram is ((m,Cup):(n,Twist):xs)
r19 n (Diagram is (x:xs)) = (\(Diagram is' xs') -> Diagram is' (x:xs')) <$> r19 (n-1) (Diagram is xs)


allRules :: [Rule]
allRules = [r1,r2,r3,r4,r5,r6,r7,r89,r10,r11,r12,r13,r14,r15,r16,r17,r18,r19]


tryPermute :: Int -> Diagram -> Maybe Diagram
tryPermute _ (Diagram _ []) = Nothing
tryPermute _ (Diagram _ [_]) = Nothing

tryPermute 0 (Diagram is ((m,Twist):(n,Twist):xs)) 
    | (S.fromList [m,m+1]) `S.intersection` (S.fromList [n,n+1]) == S.empty = 
    Just $  Diagram is ((n,Twist):(m,Twist):xs)
tryPermute 0 (Diagram is ((m,Twist):(n,Diamond a):xs)) 
    | (S.fromList [m,m+1]) `S.intersection` (S.fromList [n]) == S.empty = 
    Just $  Diagram is ((n,Diamond a):(m,Twist):xs)
tryPermute 0 (Diagram is ((m,Twist):(n,Cup):xs)) 
    | (S.fromList [m,m+1]) `S.intersection` (S.fromList [n,n+1]) == S.empty = 
    if m > n 
        then Just $ Diagram is ((n,Cup):(m-1,Twist):xs)
        else Just $ Diagram is ((n,Cup):(m,Twist):xs)
tryPermute 0 (Diagram is ((m,Twist):(n,Lolly a):xs)) 
    | m+1 /= n =
    if m >= n 
        then Just $ Diagram is ((n,Lolly a):(m+1,Twist):xs)
        else Just $ Diagram is ((n,Lolly a):(m,Twist):xs)

tryPermute 0 (Diagram is ((m,Diamond a):(n,Twist):xs)) 
    | (S.fromList [m]) `S.intersection` (S.fromList [n,n+1]) == S.empty = 
    Just $  Diagram is ((n,Twist):(m,Diamond a):xs)
tryPermute 0 (Diagram is ((m,Diamond a):(n,Diamond b):xs)) 
    | m /= n = 
    Just $  Diagram is ((n,Diamond b):(m,Diamond a):xs)
tryPermute 0 (Diagram is ((m,Diamond a):(n,Cup):xs)) 
    | (S.fromList [m]) `S.intersection` (S.fromList [n,n+1]) == S.empty = 
    if m > n 
        then Just $ Diagram is ((n,Cup):(m-1,Diamond a):xs)
        else Just $ Diagram is ((n,Cup):(m,Diamond a):xs)
tryPermute 0 (Diagram is ((m,Diamond a):(n,Lolly b):xs)) = 
    if m >= n 
        then Just $ Diagram is ((n,Lolly b):(m+1,Diamond a):xs)
        else Just $ Diagram is ((n,Lolly b):(m,Diamond a):xs)


tryPermute 0 (Diagram is ((m,Cup):(n,Twist):xs)) 
    | (S.fromList [m]) `S.intersection` (S.fromList [n,n+1]) == S.empty = 
    if m > n
        then Just $ Diagram is ((n,Twist):(m,Cup):xs)
        else Just $ Diagram is ((n+1,Twist):(m,Cup):xs)
tryPermute 0 (Diagram is ((m,Cup):(n,Diamond b):xs)) 
    | m /= n = 
    if m > n
        then Just $ Diagram is ((n,Diamond b):(m,Cup):xs)
        else Just $ Diagram is ((n+1,Diamond b):(m,Cup):xs)
tryPermute 0 (Diagram is ((m,Cup):(n,Cup):xs)) 
    | (S.fromList [m]) `S.intersection` (S.fromList [n,n+1]) == S.empty = 
    if m > n 
        then Just $ Diagram is ((n,Cup):(m-1,Cup):xs)
        else Just $ Diagram is ((n+1,Cup):(m,Cup):xs)
tryPermute 0 (Diagram is ((m,Cup):(n,Lolly b):xs)) = 
    if m >= n 
        then Just $ Diagram is ((n,Lolly b):(m+1,Cup):xs)
        else Just $ Diagram is ((n+1,Lolly b):(m,Cup):xs)


tryPermute 0 (Diagram is ((m,Lolly a):(n,Twist):xs)) 
    | (S.fromList [m]) `S.intersection` (S.fromList [n,n+1]) == S.empty = 
    if m > n
        then Just $ Diagram is ((n,Twist):(m,Lolly a):xs) 
        else Just $ Diagram is ((n-1,Twist):(m,Lolly a):xs)
tryPermute 0 (Diagram is ((m,Lolly a):(n,Diamond b):xs)) 
    | n /= m = 
    if m > n
        then Just $ Diagram is ((n,Diamond b):(m,Lolly a):xs)
        else Just $ Diagram is ((n-1,Diamond b):(m,Lolly a):xs)
tryPermute 0 (Diagram is ((m,Lolly a):(n,Cup):xs)) 
    | (S.fromList [m]) `S.intersection` (S.fromList [n,n+1]) == S.empty = 
    if m > n
        then Just $ Diagram is ((n,Cup):(m-1,Lolly a):xs)
        else Just $ Diagram is ((n-1,Cup):(m,Lolly a):xs)
tryPermute 0 (Diagram is ((m,Lolly a):(n,Lolly b):xs)) = 
    if m >= n
        then Just $ Diagram is ((n,Lolly b):(m+1,Lolly a):xs)
        else Just $ Diagram is ((n-1,Lolly b):(m,Lolly a):xs)
tryPermute 0 _ = Nothing

tryPermute n (Diagram is (x:xs)) = 
    (\(Diagram is' xs') -> Diagram is' (x:xs')) <$> tryPermute (n-1) (Diagram is xs)


equivs :: Diagram -> Set Diagram
equivs dia = equivs' [dia] (S.singleton dia)
    where
        equivs' :: [Diagram] -> Set Diagram -> Set Diagram
        equivs' [] s = s
        equivs' (x:xs) s = equivs' xs' s'
            where
                (xs', s') = foldr (\n (accXs, accS) -> case tryPermute n x of 
                    Just x' -> if x' `S.member` accS then (accXs,accS) else (x':accXs, S.insert x' accS)
                    Nothing -> (accXs,accS)) (xs,s) [0..length (d x)-2]




-- tries to apply a rule at all locations in a diagram
applyRule :: Rule -> Diagram -> Set Diagram
applyRule r dia = foldr (\n acc -> case r n dia of
    Just d' -> S.insert d' acc
    Nothing -> acc ) S.empty [0..length (d dia)-2]

-- tries to apply the rule at all locations for all eqivalent diagrams
applyRuleEquiv :: Rule -> Diagram -> Set Diagram
applyRuleEquiv r dia = foldr ((S.union). (applyRule r)) S.empty (equivs dia)


applyRulesEquiv :: [Rule] -> Diagram -> Set Diagram
applyRulesEquiv rs dia = foldr ((S.union).(flip applyRuleEquiv dia)) S.empty rs

collapseEquivs :: Set Diagram -> Set Diagram
collapseEquivs xs = foldr ((S.insert).(P.head . S.toList . equivs)) S.empty xs



data Tree a = Leaf a | Node a (Set (Tree a)) deriving (Show, Eq, Ord)


generateAllReductionPaths :: Diagram -> Tree Diagram
generateAllReductionPaths dia = case length $ xs of
    0 -> Leaf dia
    _ -> Node dia $ S.map generateAllReductionPaths xs
    where
        xs = collapseEquivs $ applyRulesEquiv allRules dia




collectFromLeaves :: Tree Diagram -> Set Diagram
collectFromLeaves t = collectFromLeaves' t S.empty
    where
        collectFromLeaves' (Leaf a) acc = S.insert (P.head $ S.toList $ equivs a) acc
        collectFromLeaves' (Node _ xs) acc  = foldr collectFromLeaves' acc xs



