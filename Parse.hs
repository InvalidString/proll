module Parse where
import Data.Char
import GHC.IO
import Data.Maybe
import Control.Monad.State
import Control.Applicative ((<|>))
import qualified Data.ByteString.Builder as B
import qualified Data.Map as Map
import qualified Data.Set as Set
import Syntax

newtype Parser i o = Parser ([i] -> Maybe ([i], o))
    deriving(Functor)

pfn (Parser p) = p


instance Applicative (Parser i) where
  pure x = Parser (\i -> Just (i, x))
  Parser p1 <*> Parser p2 = Parser $ \i1 -> do
    (i2, o1) <- p1 i1
    (i3, o2) <- p2 i2
    return (i3, o1 o2)

instance Monad (Parser i) where
  Parser p1 >>= f = Parser $ \i1 -> do
    (i2, o1) <- p1 i1
    let Parser p2 = f o1
    (i3, o2) <- p2 i2
    return (i3, o2)

instance MonadFail (Parser i) where
  fail _ = Parser $ const Nothing

infixl 2 |||

Parser p1 ||| Parser p2 = Parser $ \i1 ->
    p1 i1 <|> p2 i1



singleP :: (i -> Bool) -> Parser i i
singleP c = Parser f where
    f (x:xs) | c x = Just (xs, x)
    f _ = Nothing

single :: Eq i => i -> Parser i i
single c = singleP (==c)

tag :: Eq i => [i] -> Parser i [i]
tag [] = return []
tag (x:xs) = do
    o <- single x
    os <- tag xs
    return (o:os)


try :: Parser i o -> Parser i (Maybe o)
try (Parser p) = Parser $ \i -> case p i of
    Nothing -> Just (i, fail "try failed")
    Just (i, o) -> Just (i, return o)


take0n :: (i -> Bool) -> Parser i [i]
take0n p = Parser $ \i ->
    let (o, i2) = span p i in
    Just (i2, o)

take1n p = cond (not . null) $ take0n p

cond :: (o -> Bool) -> Parser i o -> Parser i o
cond f p = do
    x <- p
    if f x then return x
    else fail ""


condMap :: (o -> Maybe o2) -> Parser i o -> Parser i o2
condMap f p = do
    x <- p
    case f x of
        Just x -> return x
        Nothing -> fail ""




many :: Parser i o -> Parser i [o]
many p = do
    x <- try p
    case x of
        Just x -> do
            xs <- many p
            return (x:xs)
        Nothing -> return []

many1 p = condMap f (many p) where
    f [] = Nothing
    f xs = Just xs

delimited :: Parser i x -> Parser i o -> Parser i [o]
delimited delim elem = do
    e <- try elem
    case e of
        Nothing -> return []
        Just e -> do
            d <- try delim
            case d of
                Nothing -> return [e]
                Just _ -> do
                    es <- delimited delim elem
                    return $ e:es

string = do
    single '"'
    res <- take0n (/= '"')
    single '"'
    return res

eof = Parser f where
    f [] = Just ([], ())
    f _ = Nothing

-- mini c

-- tokens

data Token = P String | I String | N Int
    deriving(Show, Eq)

-- parser only cares that something is a type
type ParseTypeEnv = Set.Set String

punkt = tag "=="
    ||| tag "||"
    ||| tag "|"
    ||| tag "!="
    ||| tag "!"
    ||| tag "<="
    ||| tag ">="
    ||| tag "<"
    ||| tag ">"
    ||| tag "&&"
    ||| tag "&"
    ||| tag "++"
    ||| tag "^="
    ||| tag "^"
    ||| tag "%="
    ||| tag "%"
    ||| tag "+="
    ||| tag "-="
    ||| tag "*="
    ||| tag "/="
    ||| tag "--"
    ||| tag "->"
    ||| tag "+"
    ||| tag "="
    ||| tag "*"
    ||| tag "-"
    ||| tag "/"
    ||| tag "("
    ||| tag ")"
    ||| tag "["
    ||| tag "]"
    ||| tag "{"
    ||| tag "}"
    ||| tag ","
    ||| tag "."
    ||| tag ";;"
    ||| tag ";"
    ||| tag "::"
    ||| tag ":-"
    ||| tag ":"
    ||| tag "#"
    ||| tag "?"

num :: Parser Char Int
num = read <$> take1n isDigit

-- TODO maybe add _
ident = do
    a <- take1n isAlpha
    b <- take0n isAlphaNum
    return (a ++ b)


charLiteral :: Parser Char Char
charLiteral = do
    tag "'"
    c <- singleP (/= '\'')
    tag "'"
    return c

hexLiteral :: Parser Char Int
hexLiteral = do
    tag "0x"
    d <- take1n isHexDigit
    return $ read $ "0x" ++ d

token = P <$> punkt
    ||| N <$> hexLiteral
    ||| N <$> num
    ||| N . fromEnum <$> charLiteral
    ||| I <$> ident

p = single . P
i = single . I


comment = do
    tag "//"
    take0n (/= '\n')
    () <$ tag "\n" ||| eof

tokens = many $ do
    many $ () <$ take1n isSpace ||| comment
    token


-- parsing

number = condMap f $ singleP (const True) where
    f (N x) = Just x
    f _ = Nothing
identifier = condMap f $ singleP (const True) where
    f (I x) | Set.notMember x (Set.fromList [
        "if", "else", "then", "let", "rec", "fun", "in", "and", "match", "with",
        "try", "raise"
        ]) = Just x
    f _ = Nothing

infixR :: Parser i (o -> o -> o) ->  Parser i o -> Parser i o
infixR op sub = do
    a <- sub
    b <- try $ do
        cons <- op
        b <- infixR op sub
        return $ cons a b
    return $ fromMaybe a b


myfoldl1 op [x] = x
myfoldl1 op (a:b:bs) = myfoldl1 op ((a `op` b):bs)

infixL :: Parser i (o -> o -> o) ->  Parser i o -> Parser i o
infixL op sub = do
    a <- sub
    b <- many $ do
        cons <- op
        b <- sub
        return (cons, b)
    return $ foldl (\a (cons, b) -> cons a b) a b

unop :: Parser i (o -> o) -> Parser i o -> Parser i o
unop op sub = op <*> sub ||| sub





parse p str = do
    (_, ts) <- pfn tokens str
    (_, o) <- pfn p ts
    return o


struct = do
    n <- identifier
    p "("
    args <- delimited (p ",") term
    p ")"
    return $ Struct n args

term = App <$> struct
    ||| B <$> number
    ||| Anon <$ i "_"
    ||| (\l -> if isUpper $ head l then V l else A l) <$> identifier

goal = Call <$> struct ||| do a <- term ; p "=" ; Unify a <$> term


clause = do
    n <- identifier
    p "("
    args <- delimited (p ",") identifier
    p ")"
    p ":-"
    goals <- delimited (p ",") goal
    p "."
    return $ Clause n args goals


querry = do
    p "?"
    goals <- delimited (p ",") goal
    p "."
    return goals

program = do
    cs <- many clause
    g <- querry
    return (cs, g)
