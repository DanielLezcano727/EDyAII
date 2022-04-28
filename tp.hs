{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

data TTree k v = Node k (Maybe v) (TTree k v) (TTree k v) (TTree k v) | Leaf k v | E deriving (Show, Eq)

{-
t= (Node 'r' Nothing E (Node 'e' (Just 16) (Node 'a' Nothing  E (Leaf 's' 1) E) (Node 'o' (Just 2) (Leaf 'd' 9) E (Leaf 's' 4)) E) (Node 's' Nothing E (Node 'i' (Just 4) (Leaf 'e' 8)(Leaf 'n' 7) E) E))
-}

search :: Ord k => [k] -> TTree k v -> Maybe v
search _ E = Nothing
search [x] (Leaf k v) | x == k    = Just v 
                      | otherwise = Nothing
search _ (Leaf k v) = Nothing
search (x:xs) (Node k v l e r) | x == k && xs == [] = v
                               | x == k             = search xs e
                               | x < k              = search (x:xs) l
                               | otherwise          = search (x:xs) r

insert :: Ord k => [k] -> v -> TTree k v -> TTree k v
insert [x] v E = Leaf x v
insert (x:xs) v E = Node x Nothing E (insert xs v E) E 
insert (x:xs) v (Leaf k v2) | k == x && xs == [] = Leaf k v
                            | k == x             = Node k (Just v2) E (insert xs v E) E
                            | x < k              = Node k (Just v2) (insert (x:xs) v E) E E
                            | otherwise          = Node k (Just v2) E E (insert (x:xs) v E)
insert (x:xs) v (Node k v2 l e r) | x == k && xs == [] = Node k (Just v) l e r
                                  | x == k             = Node k v2 l (insert xs v e) r
                                  | x < k              = Node k v2 (insert (x:xs) v l) e r
                                  | otherwise          = Node k v2 l e (insert (x:xs) v r)

-- delete :: Ord k => [k] -> TTree k v -> TTree k v


largosSubKeys :: (Ord k, Eq v) => [k] -> TTree k v -> Int -> [Int]
largosSubKeys [] _ _ = [0]
largosSubKeys _ (Leaf k v) s = [0]
largosSubKeys (x:xs) (Node k v l e r) s | k == x && (l /= E || r /= E || v /= Nothing ) = [s] ++ largosSubKeys xs e (s+1)
                                        | k == x                                        = largosSubKeys xs e (s+1)
                                        | x < k                                         = [s] ++ largosSubKeys (x:xs) l (s+1)
                                        | otherwise                                     = [s] ++ largosSubKeys (x:xs) r (s+1)

_delete :: (Ord k, Eq v) => [k] -> TTree k v -> Int -> TTree k v
_delete _ (Leaf _ _) 0 = E
_delete [] (Node k _ l e r) _ = Node k Nothing l e r
_delete (x:xs) (Node k v l e r) 0 | x == k && l == E && r == E = E
                                  | x == k && v /= Nothing     = Node k v l E r
                                  | x == k && l == E           = r
                                  | x == k                     = l
                                  | x < k                      = Node k v E e r
                                  | otherwise                  = Node k v l e E
_delete (x:xs) (Node k v l e r) s | x == k    = Node k v l (_delete xs e (s-1)) r
                                  | x < k     = Node k v (_delete (x:xs) l (s-1)) e r
                                  | otherwise = Node k v l e (_delete (x:xs) r (s-1))



delete :: (Ord k, Eq v) => [k] -> TTree k v -> TTree k v
delete xs t = _delete xs t (maximum (largosSubKeys xs t 0))

-- delete xs E = E
-- delete xs tree = deleteFrom 

-- steps :: Ord k => [k] -> TTree k v -> Int
-- steps xs ttree = length xs - minimum (largos xs ttree)

-- largos [] _ = []
-- largos (x:xs) (Node k v l e r) | x == k = if l /= E || r /= E || v /= Nothing then (length xs):(largos xs e) else largos xs e

-- let resultado = minimum (largos xs ttree) where
--     resultado == -1

keys :: TTree k v -> [[k]]
keys a = aux a []

aux :: TTree k v -> [k] -> [[k]]
aux E _ = []
aux (Leaf k v) ks = [ks++[k]]
aux (Node k Nothing l e r) ks = (aux l ks) ++ (aux e (ks++[k])) ++ (aux r ks)
aux (Node k _ l e r) ks = (aux l ks) ++ (aux e (ks++[k])) ++ [ks++[k]] ++ (aux r ks)


{-
class Dic k v d | d -> k v where
    vacio    :: d
    insertar :: Ord k => k -> v -> d -> d
    buscar   :: Ord k => k -> v -> Maybe v
    eliminar :: Ord k => k -> d -> d
    claves   :: Ord k => d -> [(k, v)]

instance Dic [k] (Maybe v) (TTree k v) where
    vacio    = E
    insertar = insert
    buscar   = search
    eliminar = delete
    claves   = keys

-}
