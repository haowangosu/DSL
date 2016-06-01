module Domains where

import Data.List
import Syntax
import Helpers
import Helpers2


class Value t where
  eval :: t -> Val
  
instance Value Integer where
  eval x = Num x
instance Value Float where
  eval x = Flt x
instance Value Bool where
  eval x = Boo x
--instance Value [] where
--  eval []   = Str ""
--  eval x:xs = Str "x" ++ eval xs

dToV :: ValType -> D -> Val
dToV vtype ""  = Empty
dToV INTEGER s = case readInt s of
                   Just x    -> Num x
                   otherwise -> Error
dToV FLOAT s   = case readFloat s of
                   Just x    -> Flt x
                   otherwise -> Error
dToV BOOLEAN s = case readBool s of
                   Just x    -> Boo x
                   otherwise -> Error
dToV STRING s  = Str s
dToV EMPTY _   = Empty

vToD :: Val -> D
vToD Empty   = ""
vToD Error   = "**ERROR**"
vToD (Num x) = show x
vToD (Flt x) = show x
vToD (Boo x) = show x
vToD (Str x) = x

changeType :: ValType -> Val -> Val
changeType STRING v  = Str (vToD v)
changeType INTEGER v = case v of 
                         Num x     -> Num x
                         Flt x     -> Num (round x)
                         Boo x     -> if x then Num 1 else Num 0
                         Str x     -> dToV INTEGER x
                         Empty     -> Empty
                         otherwise -> Error
changeType FLOAT   v = case v of
                         Flt x     -> Flt x
                         Num x     -> Flt (fromInteger x)
                         Str x     -> dToV FLOAT x
                         Empty     -> Empty
                         otherwise -> Error
changeType BOOLEAN v = case v of 
                         Boo x     -> Boo x
                         Num x     -> if x==1 then Boo True else if x==0 then Boo False else Error
                         Str x     -> dToV BOOLEAN x
                         Empty     -> Empty
                         otherwise -> Error
changeType EMPTY v = case v of
                         Error     -> Error
                         otherwise -> Empty
                         
changeTypeFromList :: ValType -> [Val] -> Int -> [Val]
changeTypeFromList _ [] _         = []
changeTypeFromList vtype (v:vs) n = case n == 1 of
                                      True      -> changeType vtype v : vs
                                      otherwise -> v : changeTypeFromList vtype vs (n-1)
 
{-                                     
fval :: (a -> b) -> Val -> Val
fval _ Error   = Error
fval _ Empty   = Empty
fval f (Num x) = case f x of
                   y         -> eval y
                   otherwise -> Error
fval f (Flt x) = case f x of
                   y         -> eval y
                   otherwise -> Error
fval f (Boo x) = case f x of
                   y         -> eval y
                   otherwise -> Error
fval f (Str x) = case f x of
                   y         -> eval y
                   otherwise -> Error

                   
changeValueFromList :: (a -> b) -> [Val] -> Int -> [Val]
changeValueFromList _ [] _ = []
changeValueFromList f (v:vs) n = case n == 1 of
                                   True      -> fval f v : vs
                                   otherwise -> v : changeValueFromList f vs (n-1)
-}

dsetToVset :: Dset -> Header -> Vset
dsetToVset _ [] = []
dsetToVset [] _ = []
dsetToVset dset ((feature,vtype):headers) = (feature, map (dToV vtype) (headList dset)):(dsetToVset (tailList dset) headers)

vsetToDset :: Vset -> Dset
vsetToDset vset = (map.map) vToD (doubleListRotate (map snd vset))

dToH :: ValType -> Feature -> D -> (Feature, Val)
dToH vtype feature d = (feature, dToV vtype d)

dlistToHlist :: Header -> [D] -> [(Feature, Val)]
dlistToHlist _ [] = []
dlistToHlist [] _ = []
dlistToHlist ((feature,vtype):headers) (d:dlist) = dToH vtype feature d : dlistToHlist headers dlist

dsetToHset :: Dset -> Header -> Hset
dsetToHset _ [] = []
dsetToHset [] _ = []
dsetToHset dset header = map (dlistToHlist header) dset

hsetToDset :: Hset -> Dset
hsetToDset hset = (map.map) vToD ((map.map) snd hset)

synListReplace :: Syn -> [D] -> [D]
synListReplace (_, []) dlist        = dlist
synListReplace _ []                 = []
synListReplace (a,(b:bs)) (d:dlist) = if b == d then
                                         [a] ++ synListReplace (a,(b:bs)) dlist
                                      else
                                         synListReplace (a,bs) [d] ++ synListReplace (a,(b:bs)) dlist

synSetReplace :: SynDict -> Dset -> Dset
synSetReplace [] dset         = dset
synSetReplace _ []            = []
synSetReplace (s:sd) (d:dset) = synSetReplace sd (synListReplace s d : synSetReplace (s:sd) dset)