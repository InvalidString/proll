{-# LANGUAGE RecursiveDo #-}
module Compile where
import Syntax
import Data.Map as Map hiding(update)
import Data.Set as Set
import Control.Monad.State hiding (fail)
import Control.Monad.Reader hiding (fail)
import Data.Maybe (fromJust)
import GHC.IO.Unsafe (unsafePerformIO)
import Control.Arrow hiding ((<+>))
import qualified Data.ByteString.UTF8 as B hiding(length)
import qualified Data.ByteString as B

dbg msg = seq (unsafePerformIO $ print msg)

data SymTab = SymTab { symTabIdx :: Map Sym Int, symBytes :: B.ByteString }
    deriving(Show)

putSym tab@SymTab{symTabIdx, symBytes} sym =
    case Map.lookup sym symTabIdx of
        Just i -> (i, tab)
        Nothing -> (loc,
            SymTab {
                symTabIdx = symTabIdx <> sym |-> loc,
                symBytes = symBytes <> s <> B.singleton 0
            }
         )
     where
        loc = B.length symBytes
        s = B.fromString sym
        byteLen = B.length s


type Code = ReaderT (Map Sym Int) (State ([Instr], SymTab))


infixl 8 <+>
(<+>) :: Ord k => Map k e -> Map k e -> Map k e
(<+>) = flip Map.union

infixr 9 |->
(|->) = Map.singleton

push :: MonadState ([a], b) m  => a -> m ()
push x = modify $ first $ \xs -> xs ++ [x]

--loadc = push . ILoadC
--load = push ILoad
--loada = push . ILoadA
--binop = push . IBop
--uop = push . IUop
--mul = push $ IBop Mul
--add = push $ IBop Add
--eq = push $ IBop Eq
--jumpi = push . IJmpI
--store = push IStore
--storea = push . IStoreA
pop = push IPop
--popk k = case k of
--    0 -> return ()
--    1 -> push IPop
--    x -> push $ IPopK x
--
--jumpz = push . IJmpZ
--jumpnz = push . IJmpNZ
--dup = push IDup
--inot = push $ IUop Not
--alloc = push . IAlloc
--new = push INew
mark = push . IMark
no = push INo
halt = push . IHalt
--slide k = if k == 0
--    then return ()
--    else push $ ISlide k
--ret = push . IReturn
--enter = push . IEnter
--halt = push IHalt
--



findFirstOcc (V x) = do
    vars <- get
    modify $ Set.insert x
    return $ (if x `Set.member` vars then VBar else V) x

findFirstOcc (App (Struct f xs)) = do
    App . Struct f <$> mapM findFirstOcc xs

findFirstOcc x = return x


(V x') `contains` x = x == x'
(VBar x') `contains` x = x == x'
(App (Struct f xs)) `contains` x = or $ (`contains` x) <$> xs
(A _) `contains` x = False
(B _) `contains` x = False



label :: Code CodeAddr
label = gets $ length . fst

--update = push IUpdate
--try = push . ITry
--restore = push . IRestore
--raise = push IRaise

symIdx :: String -> Code Int
symIdx name = do
    modify $ second $ \tbl -> snd $ putSym tbl name
    asks (fromJust . Map.lookup name)

putatom a = symIdx a >>= push . IPutAtom
putvar = push . IPutVar
putref = push . IPutRef
putanon = push IPutAnon
putstruct (f,n) = do
    f <- symIdx f
    push $ IPutStruct f n

call f n = do
    push $ ICall n
    push $ ICall2 f
bind = push IBind
unify = push IUnify

uatom a = symIdx a >>= push . IUAtom
uvar = push . IUVar
uref = push . IUref
ustruct (f,n) a = undefined
son = push . ISon
up = push . IUp
check xs ρ =
    forM_ xs $ push . ICheck . envLookup ρ
pushenv = push . IPushenv
popenv = push IPopenv
setbtp = push ISetBtp
delbtp = push IDelbtp
try = push . ITry
jump = push . IJump

envLookup ρ x = fromJust $ Map.lookup x ρ
envLookupPred ρ (f,n) = fromJust $ Map.lookup (f ++ "/" ++ show n) ρ


codeA (A a) ρ = putatom a
codeA Anon ρ = putanon

codeA (V v) ρ = putvar (envLookup ρ v)
codeA (VBar v) ρ = putref (envLookup ρ v)

codeA (App (Struct f xs)) ρ = do
    forM_ xs $ \x ->
        codeA x ρ
    putstruct (f,n) where n = length xs


codeG (Call (Struct f xs)) ρ = mdo
    mark b
    forM_ xs $ \x ->
        codeA x ρ
    call (envLookupPred ρ (f,n)) n
    b <- label
    return () where n = length xs


codeG (Unify (V x) t) ρ | t `contains` x =
    push IFail

codeG (Unify (V x) t) ρ = do
    putvar (envLookup ρ x)
    codeA t ρ
    bind

codeG (Unify (VBar x) t) ρ = do
    putref (envLookup ρ x)
    codeA t ρ
    unify

ivars (App (Struct _ xs)) = xs >>= ivars
ivars (VBar x) = [x]
ivars _ = []

class HasVars a where
    vars :: a -> [Sym]

instance HasVars Struct where
    vars (Struct f xs) = xs >>= vars

instance HasVars Term where
    vars (App s) = vars s
    vars (V x) = [x]
    vars (VBar x) = [x]
    vars _ = []

instance HasVars Goal where
  vars (Call s) = vars s
  vars (Unify a b) = vars a ++ vars b

instance HasVars a => HasVars [a] where
    vars = (>>= vars)


codeU (A a) ρ = uatom a
codeU (V x) ρ = uvar (envLookup ρ x)
codeU Anon ρ = pop
codeU (VBar x) ρ = uref (envLookup ρ x)

codeU s@(App (Struct f xs)) ρ = mdo
    ustruct (f,n) a
    forM_ (zip [1..] xs) $ \(i,t) -> do
        son i
        codeU t ρ
    up b
    a <- label
    check (ivars s) ρ
    codeA s ρ
    bind
    b <- label
    return ()
        where
            n = length xs

codeC (xs, gs) ρ = do
    pushenv m
    forM_ gs $ \g ->
        codeG g ρ'
    popenv
        where m = length vs
              vs = xs ++ Set.elems (Set.fromList (vars gs) Set.\\ Set.fromList xs)
              ρ' = ρ <+> foldMap (uncurry (|->)) (zip vs [1..])

codeP (Pred f rr) ρ = mdo
    l <- label
    setbtp
    forM_ (init as) $ \a ->
        try a
    delbtp
    jump (last as)

    as <- forM rr $ \r -> do
        a <- label
        codeC r ρ
        return a

    return l

arity (Pred _ rr) = case rr of
    [] -> 0
    ((x,_):_) -> length x

code (ps, gs) = mdo
    push $ IInit a
    pushenv d
    forM_ gs $ \g ->
        codeG g ρ
    halt d
    a <- label
    no
    ls <- forM ps $ \rr ->
        codeP rr ρ

    let ρ = foldMap (uncurry (|->)) (zip vs [1..])
          <> foldMap (\(p@(Pred n _ ), l) -> (n ++ "/" ++ show (arity p)) |-> l)
                (zip ps ls)

    return () where
        vs = vars gs
        d = length vs


