module Syntax where


data Bop
    = Add
    | Sub
    | Mul
    | Div
    | Eq
    | Less
    | LessEq
    | Mod
    | And
    | Or
    | Xor
    | BAnd
    | BOr
    | BXor
    deriving(Show, Enum)

data Uop
    = Not
    | Neg
    deriving(Show, Enum)


data Struct = Struct Sym [Term]
    deriving(Show, Eq)
data Term = B Int | A Sym | V Sym | VBar Sym | App Struct | Anon
    deriving(Show, Eq)
data Goal = Call Struct | Unify Term Term | Cut | Fail
    deriving(Show)
data Clause = Clause Sym [Sym] [Goal]
    deriving(Show)

data Predicate = Pred Sym [([Sym], [Goal])]
    deriving(Show)


type CodeAddr = Int
type VarAddr = Int
type Sym = String

data Instr
    -- control flow
    = IPutAtom Int
    | IPutVar Int
    | IPutRef Int
    | IPutAnon
    | IPutStruct Int Int


    | IMark Int
    | ICall Int
    | IFail
    | ISetBtp

    | IBind
    | IUnify

    | IUAtom Int
    | IUVar Int
    | IUStruct Int Int
    | IRJump Int
    | IPop
    | IUref Int
    | ICheck Int

    | ISon Int
    | IUp Int

    | IPushenv Int
    | IPopenv
    | IDelbtp
    | ITry Int
    | IJump Int
    | IInit Int
    | IHalt Int
    | INo
    | IPrune


    -- unary operations
    | IBop Bop
    -- binary operations
    | IUop Uop
    deriving(Show)


