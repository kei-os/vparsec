module ExprTest where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

-- XXX Mmm... TODO
verilogStyle =  emptyDef
            { commentStart = ""
            , commentEnd = ""
            , commentLine = ""
            , nestedComments = False
            , identStart = letter <|> char '_'
            , identLetter = alphaNum <|> oneOf "_'"
            , opStart = opLetter emptyDef
            , opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
            , reservedOpNames = []
            , reservedNames = []
            , caseSensitive = True
            }

lexer = P.makeTokenParser(verilogStyle)

reserved    = P.reserved lexer
operator    = P.operator lexer
reservedOp  = P.reservedOp lexer

natural     = P.natural lexer

symbol      = P.symbol lexer
lexeme      = P.lexeme lexer
whiteSpace  = P.whiteSpace lexer

parens      = P.parens lexer
braces      = P.braces lexer
angles      = P.angles lexer
brackets    = P.brackets lexer

semi        = P.semi lexer
comma       = P.comma lexer
colon       = P.colon lexer
dot         = P.dot lexer

semiSep     = P.semiSep lexer
semiSep1    = P.semiSep1 lexer
commaSep    = P.commaSep lexer
commaSep1   = P.commaSep1 lexer


{- for tiny parser test  -}
test :: Show a => Parser a -> String -> IO ()
test p input
        = case (parse p "" input) of
            Left err -> print err
            Right x -> print x


{-
    A -> A alpha | beta

    A -> beta adash
    adash -> alpha adash | e

    ----
    <expression>
        ::= <primary>
        ||= <UNARY_OPERATOR> <primary>
        ||= <expression> <BINARY_OPERATOR> <expression>
        ||= <expression> <QUESTION_MARK> <expression> : <expression>
        ||= <STRING>

    alpha ::= <BINARY_OPERATOR> <expression>
           |  <QUESTION_MARK> <expression> : <expression>

    beta ::= <primary>
          |  <UNARY_OPERATOR> <primary>
          |  <STRING>
-}



{-
ex.1 just parse

 expr ::= expr BIN_OP expr
         | Q_MARK expr : expr
         |  primary
    |/

 expr ::= primary expr'

 expr' ::= BIN_OP expr expr'
        |  Q_MARK expr : expr expr'
        |  e

-}

{-
expr :: Parser String
expr = do { a <- primary
          ; b <- expr'
          ; return $ a ++ " : " ++ b }
   <?> "expr"

expr' :: Parser String
expr' = try(do { a <- binaryOperator
           ; b <- expr
           ; c <- expr'
           ; return $ a ++ " : " ++ b ++ " : " ++ c })
   <|> do { string ""; return "NIL" }
   <?> "expr'"
-}

{-
ex.2 create AST
-}

data Expr_ = NODE String Expr_ Expr_    -- op left right
           | PRIM String
           | COND Expr_ Expr_ Expr_     -- cond ifexpr elseexpr
           | NIL
             deriving (Eq, Show)

expr :: Parser Expr_
expr = do { p <- primary
          ; ex <- expr'
          ; return $ makeexpr p ex }
   <?> "expr"
    where
        makeexpr :: String -> Expr_ -> Expr_
        makeexpr p NIL = PRIM p
        makeexpr p (NODE op _ r) = (NODE op (PRIM p) r)
        makeexpr p (COND _ ifexpr elseexpr) = (COND (PRIM p) ifexpr elseexpr)

expr' :: Parser Expr_
expr' = try(do { op <- binaryOperator
               ; ex <- expr
               ; ex' <- expr'
               ; return $ makeexpr' op ex ex' })
    <|> try(do { questionMark
               ; ifexpr <- expr
               ; colon
               ; elseexpr <- expr
               ; ex' <- expr'
               ; return $ makecond ifexpr elseexpr ex' })
   <|> do { string ""; return NIL }
   <?> "expr'"
    where
        makeexpr' :: String -> Expr_ -> Expr_ -> Expr_
--        makeexpr' op (PRIM p) _ = (NODE op NIL (PRIM p))  -- NG 3
        makeexpr' op a b = (NODE op b a)
--        makeexpr' op (PRIM p) x = (NODE op x (PRIM p))          -- OK
--        makeexpr' op (NODE o l r) x = (NODE op x (NODE o l r))  -- OK
--        makeexpr' op (NODE o l r) x = (NODE op l (NODE o NIL r) )         -- NG 1
--        makeexpr' op (NODE op' ln rn) x = (NODE op rn (NODE op' ln x) )   -- NG 2
        makecond :: Expr_ -> Expr_ -> Expr_ -> Expr_
        makecond ifexpr elseexpr NIL = (COND NIL ifexpr elseexpr)
        makecond ifexpr elseexpr (NODE op _ r) = (COND NIL ifexpr (NODE op elseexpr r))
        makecond ifexpr elseexpr (COND _ ifexpr' elseexpr')
                    = (COND NIL ifexpr (COND elseexpr ifexpr' elseexpr'))

---------------------------------------------------------------------------

identifier :: Parser String
identifier = do { c <- char '_' <|> letter
                ; cs <- many (try(alphaNum) <|> try(char '_') <|> char '$')
                ; return (c:cs) }
          <?> "identifier"

questionMark :: Parser String
questionMark = string "?"
         <?> "questionMark"

primary :: Parser String
primary = do { a <- digit; return [a] }
      <?> "primary"

unaryOperator :: Parser String
unaryOperator = try(symbol "+")
            <|> try(symbol "-")
            <|> try(symbol "!")
            <|> try(symbol "~")
            <|> try(symbol "&")
            <|> try(symbol "|")
            <|> try(symbol "^")
            <|>try(symbol "~&")
            <|> try(symbol "^|")
            <|> symbol "~^"
            <?> "unaryOperator"

-- XXX not good impl
binaryOperator :: Parser String
binaryOperator = try(symbol "+")
             <|> try(symbol "-")
             <|> try(symbol "*")
             <|> try(symbol "/")
             <?> "binaryOperator"

