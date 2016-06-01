module BasicSemantics where

import Data.List
import Syntax
import Helpers
import Helpers2
import Domains


--columeEqual :: [Val] -> [Val] -> Bool
--columeEqual [] []         = True
--columeEqual (x:xs) []     = False
--columeEqual [] (y:ys)     = False
--columeEqual (x:xs) (y:ys) = x==y && colume xs ys

--eval :: Rule -> Dset
--eval :: undefined

merge :: [Vset] -> Vset
merge []     = []
merge (x:xs) = x ++ merge xs

search :: D -> Dset -> Dset
search d [] = []
search d (dlist:dset) = case elem d dlist of
                          True  -> dlist : search d dset
                          False -> search d dset

{-
changeValue :: (a -> b) -> Val -> Val
changeValue f (Num x) = case f x of
                          y -> Num y
                          _ -> Error
changeValue f (Flt x) = case f x of
                          y -> Flt y
                          _ -> Error
changeValue f (Boo x) = case f x of
                          y -> Boo y
                          _ -> Error
changeValue f (Num x) = case f x of
                          y -> Num y
                          _ -> Error
changeValue f (Num x) = case f x of
                          y -> Num y
                          _ -> Error
changeValue f (Num x) = case f x of
                          y -> Num y
                          _ -> Error
-}


--deleteColumn :: Column -> Vset -> Vset
--deleteColumn _ []        = []
--deleteColumn (Left x) s  = case x == fst (head s) of
--                             True      -> tail s
--                             otherwise -> head s : deleteColumn (Left x) (tail s)
--deleteColumn (Right x) s = case x == 1 of
--                             True      -> tail s
--                             otherwise -> head s : deleteColumn (Right(x-1)) (tail s)

--deleteRow :: Row -> Hset -> Hset
--deleteRow _ [] = []
--deleteRow x s  = case x == 1 of
--                   True      -> tail s
--                   otherwise -> head s : deleteRow (x-1) (tail s)


--deleteValue :: Vset -> Pos -> Vset
--deleteValue [] _           = []
--deleteValue s (i, Left x)  = case x == fst (head s) of
--                               False     -> head s : deleteValue (tail s) (i, Left x)
--                               otherwise -> 

deleteColumn1 :: Int -> Dset -> Dset
deleteColumn1 _ []        = []
deleteColumn1 i (s:ss)  = snd (moveEleOut i s) : deleteColumn1 i ss

deleteRow1 :: Int -> Dset -> Dset
deleteRow1 i s= snd (moveEleOut i s)

changeHelperByPos :: Int -> D -> [D] -> [D]
changeHelperByPos i d dlist = moveEleIn i (d, snd (moveEleOut i dlist))

changeHelperBySearch :: D -> D -> [D] -> [D]
changeHelperBySearch _ _ []          = []
changeHelperBySearch d d1 (d0:dlist) = if d == d0
                                          then d1 : changeHelperBySearch d d1 dlist
                                          else d0 : changeHelperBySearch d d1 dlist

changeValueByPos1 :: (Int,Int) -> D -> Dset -> Dset
changeValueByPos1 (1,i) d (dlist:dset)  = changeHelperByPos i d dlist : dset
changeValueByPos1 (i0,i) d (dlist:dset) = dlist : changeValueByPos1 ((i0-1),i) d dset

changeValueBySearch1 :: D -> D -> Dset -> Dset
changeValueBySearch1 _ _ []            = []
changeValueBySearch1 d d1 (dlist:dset) = changeHelperBySearch d d1 dlist : changeValueBySearch1 d d1 dset


deleteColumn :: [Int] -> Dset -> Dset
deleteColumn [] s = s
deleteColumn is s = deleteColumn (tail (reverse (sort is))) (deleteColumn1 (head (reverse (sort is))) s)-- operate the last column first

deleteRow :: [Int] -> Dset -> Dset
deleteRow [] s = s
deleteRow is s = deleteRow (tail (reverse (sort is))) (deleteRow1 (head (reverse (sort is))) s)

changeValueByPos :: [(Int,Int)] -> [D] -> Dset -> Dset
changeValueByPos [] _ dset          = dset
changeValueByPos _ [] dset          = dset
changeValueByPos (p:ps) (d:ds) dset = changeValueByPos ps ds (changeValueByPos1 p d dset)

changeValueBySearch :: [D] -> [D] -> Dset -> Dset
changeValueBySearch [] _ dset = dset
changeValueBySearch _ [] dset = dset
changeValueBySearch (d:ds) (d1:d1s) dset = changeValueBySearch ds d1s (changeValueBySearch1 d d1 dset)


changeColumnType :: ValType -> Column -> Vset -> Vset
changeColumnType _ _ []            = []
changeColumnType vtype (Left x) s  = case x == fst (head s) of
                                       True      -> (fst (head s), map (changeType vtype) (snd (head s))) : tail s
                                       otherwise -> head s : changeColumnType vtype (Left x) (tail s)
changeColumnType vtype (Right x) s = case x == 1 of
                                       True      -> (fst (head s), map (changeType vtype) (snd (head s))) : tail s
                                       otherwise -> head s : changeColumnType vtype (Right (x-1)) (tail s)

changeRowType :: ValType -> Row -> Hset -> Hset
changeRowType _ _ []    = []
changeRowType vtype n s = case n == 1 of
                            True      -> mapTuple (id, (changeType vtype)) (head s) : tail s
                            otherwise -> head s : changeRowType vtype (n-1) (tail s)

changeValueType :: ValType -> Pos -> Vset -> Vset
changeValueType _ _ []               = []
changeValueType vtype (n, Left x) s  = case x == fst (head s) of
                                         False     -> head s : changeValueType vtype (n, Left x) (tail s)
                                         otherwise -> (fst (head s), changeTypeFromList vtype (snd (head s)) n) : tail s
changeValueType vtype (n, Right x) s = case x == 1 of
                                         False     -> head s : changeValueType vtype (n, Right (x-1)) (tail s)
                                         otherwise -> (fst (head s), changeTypeFromList vtype (snd (head s)) n) : tail s

deleteValue :: Pos -> Vset -> Vset
deleteValue = changeValueType EMPTY

{-
changeColumn :: (a -> b) -> Column -> Vset -> Vset
changeColumn _ _ []        = []
changeColumn f (Left x) s  = case x == fst (head s) of
                               True      -> (fst (head s), map (fval f) (snd (head s))) : tails s
                               otherwise -> head s : changeColumn f (Left x) (tail s)
changeColumn f (Right x) s = case x == 1 of
                               True      -> (fst (head s), map (fval f) (snd (head s))) : tails s
                               otherwise -> head s : changeColumn f (Right (x-1)) (tail s)
                               
changeRow :: (a -> b) -> Row -> Hset -> Hset
changeRow _ _ [] = []
changeRow f n s = case n == 1 of
                    True      -> mapTuple (id, (fval f)) (head s) : tail s
                    otherwise -> head s : changeRow f n (tail s)
                    
changeValue :: (a -> b) -> Pos -> Vset -> Vset
changeValue _ _ [] = []
changeValue f (n, Left x) s  = case x == fst (head s) of
                                 False     -> head s : changeValue f (n, Left x) (tail s)
                                 otherwise -> (fst (head s), changeValueFromList f (snd (head s)) n) : tail s
changeValue f (n, Right x) s = case x == 1 of
                                 False     -> head s : changeValue f (n, Right (x-1)) (tail s)
                                 otherwise -> (fst (head s), changeValueFromList f (snd (head s)) n) : tail s
-}
                    
--columnTranslate :: 
