{-# LANGUAGE RecursiveDo #-}
import Syntax
import Data.Map as Map
import Data.Maybe
import Parse
import System.IO
import Control.Monad.Trans.Maybe


(<+>) :: Ord k => Map k e -> Map k e -> Map k e
(<+>) = flip Map.union

type Eval = MaybeT IO


data Val = B Int | Fn (Val -> Eval Val)

fromFn (Fn f) = f

type Env = Map.Map VarName Val

hoistMaybe :: Monad m => Maybe a -> MaybeT m a
hoistMaybe a = MaybeT {runMaybeT = return a}

envLookup :: VarName -> Env -> Eval Val
envLookup n ρ = hoistMaybe $ Map.lookup n ρ


bopfn Add = (+)
bopfn Sub = (-)
bopfn Mul = (*)
bopfn Eq = \a b -> if a == b then 1 else 0

unopfn Neg = negate
unopfn Not = \x -> if x == 0 then 1 else 0

eval :: Expr -> Env -> Eval Val
eval (Var n) ρ =
    envLookup n ρ
eval (BE op a b) ρ = do
    B a <- eval a ρ
    B b <- eval b ρ
    return $ B $ bopfn op a b
eval (UE op a) ρ = do
    B a <- eval a ρ
    return $ B $ unopfn op a

eval (C i) ρ = return $ B i
eval (App f args) ρ = do
    f <- eval f ρ
    xs <- mapM (`eval` ρ) args
    l f xs
    where
         l :: Val -> [Val] -> Eval Val
         l f [] = return f
         l (Fn f) (x:xs) = do
            r <- f x
            l r xs

eval (If e0 e1 e2) ρ = do
    B x0 <- eval e0 ρ
    if x0 /= 0 then
        eval e1 ρ
    else
        eval e2 ρ


eval (Fun args body) ρ = fn ρ args
    where
        fn ρ [] = eval body ρ
        fn ρ (a:as) = return $ Fn $ \ x -> fn (ρ <+> Map.singleton a x) as

eval (Let bs e) ρ = do
    ρ <- Prelude.foldl
        (\ρ (name,ex) -> do
            ρ <- ρ
            v <- eval ex ρ
            return $ ρ <+> Map.singleton name v
        ) (return ρ) bs
    eval e ρ

eval (LetRec bs e) ρ = mdo
    ρfin <- Prelude.foldl
        (\ρ (name,ex) -> do
            ρ <- ρ
            v <- eval ex ρfin
            return $ ρ <+> Map.singleton name v
        ) (return ρ) bs
    eval e ρfin


repl = do
    putStrLn "OCamel version 5.3.0"
    putStrLn ""
    loop mempty where

    ocamlOut var val = do
        putStrLn $ var ++ case val of
            B b ->  " : int = " ++ show b
            _ -> " = <fun>"


    loop ρ = do
        putStr "# "
        hFlush stdout
        str <- getLine

        ρ <- case parse replExpr str of
            Just (REE e) -> do
                v <- runMaybeT $ eval e ρ
                case v of
                    Just v -> ocamlOut "-" v
                    Nothing -> putStrLn "Error: Eval error"
                return ρ
            Just (RET (Def n e)) -> do
                v <- runMaybeT $ eval e ρ
                case v of
                    Just v -> do
                        ocamlOut ("val " ++ n) v
                        return $ ρ <+> Map.singleton n v
                    Nothing -> do
                        putStrLn "Error: Eval error"
                        return ρ
            Just (RET (DefRec n e)) -> mdo
                let ρ2 = ρ <+> Map.singleton n x
                v <- runMaybeT $ eval e ρ2
                let x = case v of ~(Just x) -> x
                case v of
                    Just x -> do
                        ocamlOut ("val " ++ n) x
                        return ρ2
                    Nothing -> do
                        putStrLn "Error: Eval error"
                        return ρ
            Just REeof -> return ρ
            Nothing -> do
                putStrLn "Error: Syntax error"
                return ρ
        loop ρ

main = repl


fac = "let fix = fun f -> (fun x -> f (x x)) (fun x y -> f (x x) y) in let fac = fun r x -> if x == 0 then 1 else x * r (x-1) in fix fac 5"
