module Syntax where


type D = String
type Feature = String
type Dset = [[D]]
type Var = String

data Val = Num Integer
         | Flt Float
         | Boo Bool
         | Str String
         | Empty
         | Error
  deriving (Eq,Show)

data ValType = INTEGER | FLOAT | BOOLEAN | STRING | EMPTY

type Row = Int
type Column = Either Feature Int
type Pos = (Row,Column)

type Header = [(Feature, ValType)]
type Vset = [(Feature, [Val])]
type Hset = [[(Feature, Val)]]

data OprType = MERGE2 | CHANGEVALUE | CHANGEROW | CHANGECOLUMN | DELETEVALUE | DELETEROW | DELETECOLUMN

type Env = Var -> Dset
type Corp = (Dset,Vset,Hset)

type Syn = (D, [D])
type SynDict = [Syn]

{-
data Rule = Merge [Var] 
          | Set Var Dset
          | Get Var
          | Unset Var
          | DeleteValue Var (Int,(Either Feature Int))
          | DeleteRow Var Int
          | DeleteColume Var (Either Feature Int)
          | ChangeValue Var (Int,(Either Feature Int)) --(a -> b)
          | ChangeRow Var Int --(a -> b)
          | ChangeColume Var (Either Feature Int) --(a -> b)
          | ChangeSet Var --(a -> b)
          | ChangeValueType Var (Int,(Either Feature Int)) ValType
          | ChangeRowType Var Int
          | ChangeColumnType Var Var (Int,(Either Feature Int)) ValType
          | ChangeSetType Var ValType
          | Filter Var (Val -> Bool)
          | SortColumn Var (Either Feature Int)
          | FindError Var (Val -> Bool)
          | FindMess Var
          | Combine2 (Var, Either Feature Int) (Var, Either Feature Int)
          | ChgR Var 
-}