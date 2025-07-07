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
    deriving(Show)
data Term = B Int | A Sym | V Sym | VBar Sym | App Struct | Anon
    deriving(Show)
data Goal = Call Struct | Unify Term Term
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
    | ICall2 Int
    | IFail
    | ISetBtp

    | IBind
    | IUnify

    | IUAtom Int
    | IUVar Int
    | IUStruct
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


    -- unary operations
    | IBop Bop
    -- binary operations
    | IUop Uop
    deriving(Show)


