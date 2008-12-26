module Vparsec where

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


-- XXX for Module
data Module_ = MODULE
    { mName     :: String
    , mPorts    :: [String]
    , mItems    :: [ModuleItem_]
    } deriving (Eq, Show)

-- XXX test for AST
data ModuleItem_ = MI_DECL          String
                 | MI_PARAM_DECL    String
                 | MI_CONT_ASSIGN   [NetAssign_]
                 | MI_PORT_DECL     Sig_
                 | MI_REG_DECL      Sig_
                 | MI_TIME_DECL     String
                 | MI_INT_DECL      String
                 | MI_NET_DECL      Sig_
                 | MI_INITIAL       Stmt_
                 | MI_ALWAYS        Stmt_
                   deriving (Eq, Show)

data Stmt_ = ST_BLOCKING_ASSIGN     BlockAssign_
           | ST_NON_BLOCKING_ASSIGN BlockAssign_
           | ST_CONTINUOUS_ASSIGN   String     -- XXX
           | ST_TIMING_CONTROL_STMT TimingControl_
           | ST_CONDITIONAL_STMT    String
--           | ST_CASE_STMT           String
--           | ST_LOOP_STMT           String
--           | ST_WAIT_STMT           String
--           | ST_DISABLE_STMT        String
--           | ST_EVENT_TRIGGER       String
           | ST_SEQ_BLOCK           String
--           | ST_PAR_BLOCK           String
--           | ST_TASK_ENABLE         String
--           | ST_SYSTEM_TASK_ENABLE  String
           | ST_NIL
             deriving (Eq, Show)

data RegAssign_ = REG_ASSIGN LValue_ Expr_ deriving (Eq, Show)        -- normal assignment (regAssignment)

data NBAssign_ = ASSIGN LValue_ DelayOrEvent_ Expr_ deriving (Eq, Show)     -- blockingAssignment
type BAssign_ = NBAssign_       -- nonBlockingAssignment

data NetAssign_ = NET_ASSIGN LValue_ Expr_ deriving (Eq, Show)
data CAssign_ = C_NET_ASSIGN [NetAssign_] deriving (Eq, Show)


data TimingControl_ = TIMING_CONTROL DelayOrEvent_ Stmt_ deriving (Eq, Show)

data SeqBlock_ = SEQ_BLOCK Stmt_ NameOfBlock_ OutputDecl_ deriving (Eq, Show)   -- temp
type NameOfBlock_ = String
type OutputDecl_ = String       -- XXX temp

data DelayOrEvent_ = DE_DELAY_CONTROL  DelayControl_
                   | DE_EVENT_CONTROL  EventControl_
                   | DE_EXPR_EV Expr_ EventControl_
                   | DE_NIL
                     deriving (Eq, Show)

data DelayControl_ = DL_NUM Integer | DL_IDENT String deriving (Eq, Show)
data EventControl_ = EV_IDENT String | EV_EXPR EventExpr_ deriving (Eq, Show)

data EventExpr_ = EV_EXPR_ Expr_
                | EV_SCALAR Edge_ String    --  XXX TODO : change String to ScEvExpr_
                | EV_LIST [EventExpr_]
                  deriving (Eq, Show)

data Edge_ = POSEDGE | NEGEDGE | VALUE_CHANGE deriving (Eq, Show)

data Primary_ = PR_NUMBER String
              | PR_IDENT String
              | PR_IDENT_EXPR String Expr_
              | PR_IDENT_RANGE String Range_
              | PR_CONCAT [Expr_]
              | PR_MULT_CONCAT String   -- XXX TODO : impl
              | PR_MINMAX_EXPR String   -- XXX TODO : impl
                deriving (Eq, Show)

type UnaryOp_   = String
type BinaryOp_  = String
type ExprCond_  = Expr_
type ExprIf_    = Expr_
type ExprElse_  = Expr_

data Expr_ = EX_PRIMARY Primary_
           | EX_U_PRIMARY UnaryOp_ Primary_
           | EX_EXPR_NODE Expr_ BinaryOp_ Expr_
           | EX_EXPR_IFELSE ExprCond_ ExprIf_ ExprElse_
           | EX_STRING String
             deriving (Eq, Show)

data LValue_ = LV_IDENT String
             | LV_IDENT_EXPR String Expr_
             | LV_IDENT_RANGE String Range_      -- identifier [ constant_expr : constant_expr ]
             | LV_CONCAT [Expr_]
               deriving (Eq, Show)

data BlockAssign_ = BLOCK_ASSIGN LValue_ DelayOrEvent_ Expr_ deriving (Eq, Show)

------------------------------------------------------------

type Max_ = Int
type Min_ = Int
type Width_ = Int
type Range_ = (Max_, Min_, Width_)

-- XXX TODO : reg / memory
data Sig_ = PORT_SIG { direction_ :: Direction_ , name_ :: [String] , range_ :: Range_ }
              | NET_SIG { netType_ :: NetType_, name_ :: [String], range_ :: Range_ }
              | REG_SIG { regType_ :: RegType_, name_ :: [String], range_ :: Range_ }

                deriving (Eq, Show, Ord)

data Direction_ = INPUT | OUTPUT | INOUT | NONE deriving (Eq, Show, Ord)
data NetType_ = WIRE deriving (Eq, Show, Ord)  -- and more
data RegType_ = REG | MEM deriving (Eq, Show, Ord)


{- for tiny parser test  -}
test :: Show a => Parser a -> String -> IO ()
test p input
        = case (parse p "" input) of
            Left err -> print err
            Right x -> print x

{-- File input version : parseVerilog "your_verilog_file.v" --}
parseVerilog :: FilePath -> IO ()
parseVerilog fname
    = do { input <- readFile fname
--         ; putStr input
         ; case parse verilog1995 fname input of
                Left err -> do { putStr "Error parsing at : " ; print err }
                Right x -> print x }

-- Verilog 1995 parser
verilog1995 :: Parser Module_
verilog1995 = description
            <?> "source text!!"

description :: Parser Module_
description = moduleDeclaration <|> udpDeclaration
          <?> "description"
                
-- XXX MEMO : letters, digits, dollar, _ :: first must be a letter or the underscore.
-- XXX size is up to 1024. (this consraint is not implemented yet)
identifier :: Parser String
identifier = do { c <- char '_' <|> letter
                ; cs <- many (try(alphaNum) <|> try(char '_') <|> char '$')
                ; return (c:cs) }
          <?> "identifier"

-- XXX Module version
moduleDeclaration :: Parser Module_
moduleDeclaration = do { try(symbol "module"); a <- moduleDeclaration'; return a }
                <|> do { try(symbol "macromodule"); a <- moduleDeclaration'; return a }
                <?> "moduleDeclaration"
    where
        moduleDeclaration' :: Parser Module_
        moduleDeclaration'
              = do { n <- lexeme nameOfModule
                   ; p <- listOfPorts
                   ; semi
                   ; m <- lexeme(many moduleItem)
                   ; symbol "endmodule"
                   ; return (MODULE { mName = n, mPorts = p, mItems = m }) }

nameOfModule :: Parser String
nameOfModule = identifier <?> "nameOfModule"

listOfPorts :: Parser [String]
listOfPorts = parens listOfPorts'
          <|> do { string ""; return [] }
          <?> "listOfPorts"
    where
        listOfPorts' = do { whiteSpace
                          ; p <- lexeme port
                          ; ps <- lexeme(many commaPorts)
                          ; return (p:ps) }
                   <?> "listOfPorts'"

-- XXX TODO : AST
port :: Parser String
port = try(portExpression)
    <|> do { a <- dot
           ; b <- lexeme nameOfPort
           ; c <- parens port'
           ; return $ a ++ b ++ c }
    <|> string ""
    <?> "port"
        where
            port' :: Parser String
            port' = portExpression
                <|> string ""
                <?> "port'"

commaPorts :: Parser String
commaPorts = do { comma; a <- port; return a }
          <?> "commaPorts"

-- XXX TODO : AST
portExpression :: Parser String
portExpression = try(portReference)
             <|> braces portExpression'
             <?> "portExpression"
                where
                    portExpression' :: Parser String
                    portExpression' = do { a <- portReference
                                         ; b <- many(commaPortReference)
                                         ; return $ a ++ concat(b) }

commaPortReference :: Parser String
commaPortReference = do { comma; a <- portReference; return a }
                  <?> "commaPortReference"

-- XXX TODO : AST
portReference :: Parser String
portReference = do { a <- nameOfVariable
--                   ; b <- portReference'    -- XXX get port information
                   ; portReference'    -- XXX get port information  (currently not use lower block's info)
                   ; return a }
            <?> "portReference"
    where
        portReference' :: Parser String
        portReference' = do { a <- brackets constantExpression'; return a }
                      <|> string ""
        constantExpression' :: Parser String
        constantExpression' = constantExpression
                          <|> do { a <- lexeme constantExpression
                                 ; b <- colon
                                 ; c <- lexeme constantExpression
                                 ; return $ a ++ b ++ c}

nameOfPort :: Parser String
nameOfPort = identifier <?> "nameOfPort"

nameOfVariable :: Parser String
nameOfVariable = identifier <?> "nameOfVariable"

-------------------------------------------------------------------------------------

udpDeclaration :: Parser Module_
udpDeclaration = return (MODULE { mName = "none", mPorts = [], mItems = [] })
             <?> "udpDeclaration"

constantExpression :: Parser String
--constantExpression = expression
constantExpression = do { expression; return "expression:ok " }
                 <?> "constantExpression"

{--------- XXX not yet ---------}
moduleItem :: Parser ModuleItem_
moduleItem = try(lexeme parameterDeclaration)
         <|> try(lexeme continuousAssign)
         <|> try(lexeme inputDeclaration)
         <|> try(lexeme outputDeclaration)
         <|> try(lexeme inoutDeclaration)
         <|> try(lexeme regDeclaration)
         <|> try(lexeme timeDeclaration)
         <|> try(lexeme integerDeclaration)
         <|> try(lexeme netDeclaration)
         <|> try(lexeme initialStatement)
         <|> try(lexeme alwaysStatement)
         <?> "moduleItem"

parameterDeclaration :: Parser ModuleItem_
parameterDeclaration = do { a <- symbol "parameter"
                          ; b <- listOfParamAssignment
                          ; c <- semi
                          ; return $ MI_PARAM_DECL $ a ++ b ++ c }
                  <?> "parameterDeclaration"

listOfParamAssignment :: Parser String
listOfParamAssignment = do { a <- paramAssignment
                           ; b <- try(many commaParamAssignment)
                           ; return $ a ++ (concat b) }
                  <?> "listOfParamAssignment"

paramAssignment :: Parser String
paramAssignment = do { a <- lexeme identifier
                     ; b <- symbol "="
                     ; c <- lexeme constantExpression
                     ; return $ a ++ b ++ c }
             <?> "paramAssignment"

commaParamAssignment :: Parser String
commaParamAssignment = do { a <- comma
                          ; b <- try(paramAssignment)
                          ; return $ a ++ b }
                  <?> "commaParamAssignment"

inputDeclaration :: Parser ModuleItem_
inputDeclaration = do { symbol "input"
                      ; r <- rangeOrEmpty
                      ; l <- listOfPortIdentifiers
                      ; semi
                      ; return $ MI_PORT_DECL $ PORT_SIG { direction_ = INPUT, name_ = l, range_ = r } }
               <?> "inputDeclaration"

listOfPortIdentifiers :: Parser [String]
listOfPortIdentifiers = do { a <- portIdentifier
                           ; b <- many commaPortIdentifier
                           ; return (a:b) }
                    <?> "listOfPortIdentifiers"
    where
        commaPortIdentifier :: Parser String
        commaPortIdentifier = do { comma ; lexeme identifier >>= return }

portIdentifier :: Parser String
portIdentifier = identifier

rangeOrEmpty :: Parser Range_
rangeOrEmpty = try(lexeme range)
           <|> do { string ""; return (0, 0, 1) }
           <?> "rangeOrEmpty"

range :: Parser Range_
range = do { symbol "["; r <- range'; symbol "]"; return r }
   <?> "range"
    where
        range' :: Parser Range_
--        range' = do { max <- lexeme constantExpression
        range' = do { max <- number     -- XXX FIXME : need valid constantExpression
                    ; colon
--                    ; min <- lexeme constantExpression
                    ; min <- number     -- XXX FIXME : need valid constantExpression
                    ; return (read max, read min, (read max) - (read min) + 1 ) }   -- XXX TODO improve
              <?> "range'"

outputDeclaration :: Parser ModuleItem_
outputDeclaration = do { symbol "output"
                       ; r <- rangeOrEmpty
                       ; l <- listOfPortIdentifiers
                       ; semi
                       ; return $ MI_PORT_DECL $ PORT_SIG { direction_ = OUTPUT, name_ = l, range_ = r } }
                <?> "outputDeclaration"

inoutDeclaration :: Parser ModuleItem_
inoutDeclaration = do { symbol "inout"
                      ; r <- rangeOrEmpty
                      ; l <- listOfPortIdentifiers
                      ; semi
                      ; return $ MI_PORT_DECL $ PORT_SIG { direction_ = INOUT, name_ = l, range_ = r } }
                <?> "inoutDeclaration"

netDeclaration :: Parser ModuleItem_
netDeclaration = do { nettype       -- XXX TODO : use SignalType_
                    ; try(vecorscal) <|> string ""
                    ; r <- rangeOrEmpty
                    ; try(delay3) <|> string ""     -- XXX TODO : impl
                    ; n <- listOfNetIdentifiers
                    ; semi
                    ; return $ MI_NET_DECL $ NET_SIG { netType_ = WIRE, name_ = n, range_ = r } } -- XXX FIXME : direction_
            <?> "netDeclaration"
    where
        vecorscal :: Parser String
        vecorscal = brackets vecorscal'
        vecorscal' = do { a <- symbol "vectored"
                        ; b <- symbol "|"
                        ; c <- symbol "scalared"
                        ; return $ a ++ b ++ c }

nettype :: Parser String
nettype = try(symbol "wire")        -- XXX TODO : impl all types
      <?> "nettype"

listOfNetIdentifiers :: Parser [String]
listOfNetIdentifiers = do { n <- identifier; ns <- many commaNetIdentifier; return (n:ns) }
    where
        commaNetIdentifier :: Parser String
        commaNetIdentifier = do { comma; identifier >>= return }

delay3 :: Parser String
delay3 = string ""      -- XXX TODO : impl

regDeclaration :: Parser ModuleItem_
regDeclaration = do { symbol "reg"
                    ; r <- rangeOrEmpty
                    ; l <- listOfRegisterVariables
                    ; semi
                    ; return $ MI_REG_DECL $ REG_SIG { regType_ = REG, name_ = l, range_ = r } }  -- XXX FIXME : direction_
             <?> "regDeclaration"

listOfRegisterVariables :: Parser [String]
listOfRegisterVariables = do { r <- lexeme registerVariable
                             ; rs <- lexeme(many commaRegisterVariable)
                             ; return (r:rs) }
                     <?> "listOfRegisterVariables"
    where
        commaRegisterVariable :: Parser String
        commaRegisterVariable = do { comma; registerVariable >>= return }

registerVariable :: Parser String
registerVariable = do { a <- lexeme identifier
                      ; rangeOrEmpty    -- XXX TODO : array size
                      ; return a }
              <?> "registerVariable"

timeDeclaration :: Parser ModuleItem_
timeDeclaration = do { a <- symbol "time"
                     ; b <- listOfRegisterVariables
                     ; c <- semi
                     ; return $ MI_TIME_DECL $ a ++ (concat b) ++ c }
              <?> "timeDeclaration"

integerDeclaration :: Parser ModuleItem_
integerDeclaration = do { a <- symbol "integer"
                        ; b <- listOfRegisterVariables
                        ; c <- semi
                        ; return $ MI_INT_DECL $ a ++ (concat b) ++ c }
              <?> "integerDeclaration"

-- XXX TODO
realDeclaration :: Parser String
realDeclaration = string ""

-- XXX TODO
eventDeclaration :: Parser String
eventDeclaration = string ""

-- XXX TODO impl (check try and lexeme)
blockDeclaration :: Parser ModuleItem_
blockDeclaration = try(lexeme parameterDeclaration)
               <|> try(lexeme regDeclaration)
               <|> try(lexeme integerDeclaration)
--               <|> try(lexeme realDeclaration)
               <|> try(lexeme timeDeclaration)
--               <|> try(lexeme eventDeclaration)
               <?> "blockDeclaration"

-- Behavioral Statements

continuousAssign :: Parser ModuleItem_
continuousAssign = do { symbol "assign"
                      ; a <- {- [drive_strength] [delay3] -} listOfNetAssignments
                      ; semi
                      ; return $ MI_CONT_ASSIGN a }
              <?> "continuousAssign"

listOfNetAssignments :: Parser [NetAssign_]
listOfNetAssignments = do { a <- netAssignment
                          ; as <- many (lexeme commaNetAssignment)
                          ; return (a:as) }
                   <?> "listOfNetAssignments"

commaNetAssignment :: Parser NetAssign_
commaNetAssignment = do { comma; a <- netAssignment; return a }

netAssignment :: Parser NetAssign_
netAssignment = do { lv <- lexeme lvalue
                   ; symbol "="
                   ; expr <- lexeme expression
                   ; return $ NET_ASSIGN lv expr }
            <?> "netAssignment"

initialStatement :: Parser ModuleItem_
initialStatement = do { symbol "initial"; a <- lexeme statement; return $ MI_INITIAL a }
             <?> "initialStatement"

alwaysStatement :: Parser ModuleItem_
alwaysStatement = do { symbol "always"; a <- lexeme statement; return $ MI_ALWAYS $ a }
              <?> "alwaysStatement"

statementOrNull :: Parser Stmt_
statementOrNull = try (lexeme statement)
              <|> do {semi; return ST_NIL}
              <?> "statementOrNull"

-- XXX this BNF is from IEEE spec.
--statement :: Parser String
statement :: Parser Stmt_
statement = try(do { a <- lexeme blockingAssignment; semi; return a })
        <|> try(do { a <- lexeme nonBlockingAssignment; semi; return a })
        <|> try(do { a <- lexeme proceduralContinuousAssignments; semi; return a })
        <|> try(do { a <- lexeme proceduralTimingControlStatement; return a })
        <|> try(do { a <- lexeme conditionalStatement; return a })
--        <|> do { a <- try(lexeme caseStatement; return a) }
--        <|> do { a <- try(lexeme loopStatement; return a) }
--        <|> do { a <- try(lexeme waitStatement; return a) }
--        <|> do { a <- try(lexeme disableStatement; return a) }
--        <|> do { a <- try(lexeme eventTrigger; return a) }
        <|> try(do { a <- lexeme seqBlock; return a })        -- XXX TODO impl
--        <|> do { a <- try(lexeme parBlock; return a) }
--        <|> do { a <- try(lexeme taskEnable; return a) }
--        <|> do { a <- try(lexeme systemTaskEnable; return a) }
        <?> "statement"

assignment :: Parser String
assignment = do { lv <- lexeme lvalue   -- XXX TODO : modify
                ; b <- symbol "="
                ; c <- expression
                ; return $ {-a ++-} b ++ {-c-} "expression:ok "  } <?> "assignment" -- XXX FIXME : modify

commaAssignment :: Parser String
commaAssignment = do { a <- comma; b <- assignment; return $ a ++ b }
              <?> "commaAssignment"

blockingAssignment :: Parser Stmt_
blockingAssignment
        = try(do { lv <- lexeme lvalue
                 ; symbol "="
                 ; expr <- expression
                 ; return $ ST_BLOCKING_ASSIGN $ BLOCK_ASSIGN lv DE_NIL expr })
      <|> try(do { lv <- lexeme lvalue
                 ; symbol "="
                 ; de <- lexeme delayOrEventControl
                 ; expr <- lexeme expression
                 ; semi
                 ; return $ ST_BLOCKING_ASSIGN $ BLOCK_ASSIGN lv de expr })
      <?> "blockingAssignment"

nonBlockingAssignment :: Parser Stmt_
nonBlockingAssignment
        = try(do { lv <- lexeme lvalue
                 ; symbol "<="
                 ; expr <- expression
                 ; return $ ST_NON_BLOCKING_ASSIGN $ BLOCK_ASSIGN lv DE_NIL expr })
      <|> try(do { lv <- lexeme lvalue
                 ; symbol "<="
                 ; de <- lexeme delayOrEventControl
                 ; expr <- lexeme expression
                 ; semi
                 ; return $ ST_NON_BLOCKING_ASSIGN $ BLOCK_ASSIGN lv de expr })
      <?> "nonBlockingAssignment"

proceduralContinuousAssignments :: Parser Stmt_
proceduralContinuousAssignments = try(do { a <- symbol "assign"
                                         ; b <- lexeme regAssignment
                                         ; c <- semi
--                                         ; return $ a ++ b ++ c}) 
                                         ; return $ ST_CONTINUOUS_ASSIGN $ "ST_CONTINUOUS_ASSIGN:1 "}) -- FIXME : temp
                              <|> try(do { a <- symbol "deassign"
                                         ; b <- lexeme reglValue
                                         ; c <- semi
--                                         ; return $ a ++ b ++ c })
                                         ; return $ ST_CONTINUOUS_ASSIGN $ "ST_CONTINUOUS_ASSIGN:2 " }) -- FIXME : temp
--                              <|> force regAssignment;    -- not impl
--                              <|> force netAssignment;    -- not impl
--                              <|> release reglValue;      -- not impl
--                              <|> release netlValue;      -- not impl
                              <?> "proceduralContinuousAssignments"

proceduralTimingControlStatement :: Parser Stmt_
proceduralTimingControlStatement
            = do { de <- delayOrEventControl
                 ; st <- statementOrNull
                 ; return $ ST_TIMING_CONTROL_STMT $ TIMING_CONTROL de st }
          <?> "proceduralTimingControlStatement"

--conditionalStatement :: Parser String
conditionalStatement :: Parser Stmt_
conditionalStatement = do { a <- symbol "if"
                          ; b <- parens expression
                          ; c <- statementOrNull
                          ; d <- try(elseStatementOrNull) <|> string ""
--                          ; return $ a ++ b ++ c ++ d }
                          ; return $ ST_CONDITIONAL_STMT $ "ST_CONDITIONAL_STMT " } -- FIXME : temp
                   <?> "conditionalStatement"
    where
        elseStatementOrNull = do { a <- symbol "else"
                                 ; b <- statementOrNull
--                                 ; return $ a ++ b }
                                 ; return $ a }

regAssignment :: Parser String
regAssignment = do { a <- lexeme reglValue
                   ; b <- symbol "="
                   ; c <- lexeme expression
                   ; return $ a ++ b {-++ c-} ++ "expression:ok "  }
            <?> "regAssignment"

--seqBlock :: Parser String
seqBlock :: Parser Stmt_
seqBlock = do { a <- symbol "begin"
              ; b <- seqBlock'
              ; c <- symbol "end"
--              ; return $ a ++ b ++ c }
              ; return $ ST_SEQ_BLOCK $ "ST_SEQ_BLOCK " }
       <?> "seqBlock"
    where
--        seqBlock' = do {a <- many statement; return (concat a)}
--        seqBlock' = do {a <- many statement; return (concat a)}
        seqBlock' = do {a <- many statement; return "seqBlock' :1 ok "}
                <|> do { a <- colon
                       ; b <- identifier
                       ; c <- many blockDeclaration
                       ; d <- many statement
--                       ; return $ a ++ b ++ (concat c) ++ (concat d) }
--                       ; return $ a ++ b ++ {-(concat c) ++ -} (concat d) }  -- XXX test for AST
                       ; return "seqBlock':2 ok " }  -- XXX FIXME : temp for Stmt_
                <?> "seqBlock'"

delayOrEventControl :: Parser DelayOrEvent_
delayOrEventControl = try (do { dl <- delayControl; return $ DE_DELAY_CONTROL dl })
                  <|> try (do { ev <- eventControl; return $ DE_EVENT_CONTROL ev })
                  <|> do { symbol "repeat"
                         ; expr <- parens expression
                         ; ev <- eventControl
                         ; return $ DE_EXPR_EV expr ev }
                  <?> "delayOrEventControl"

delayControl :: Parser DelayControl_
delayControl = try(do { symbol "#"; n <- number; return $ DL_NUM $ read n })
           <|> try(do { symbol "#"; b <- lexeme identifier; return $ DL_IDENT b })
--           <|> do { symbol "#"; e <- parens mintypmaxExpression; return e }   -- XXX not support yet
           <?> "delayControl"

mintypmaxExpression :: Parser String
mintypmaxExpression = try(do { a <- lexeme expression
                         ; b <- colon
                         ; c <- lexeme expression
                         ; d <- colon
                         ; e <- lexeme expression
                         ; return "expr1 : expr2 : expr3 " })
                  <|> do { a <- lexeme expression; return "expr4 " }
                  <?> "mintypmaxExpression"

--eventControl :: Parser String
eventControl :: Parser EventControl_
eventControl = try(do { symbol "@"; id <- lexeme identifier; return $ EV_IDENT id })
           <|> do { symbol "@"; ev <- parens eventExpression; return $ EV_EXPR ev }
           <?> "eventControl"

-- XXX use IEEE's BNF (need to omit left recursion)
-- XXX FIXME : returning dummy value
eventExpression :: Parser EventExpr_
eventExpression = do { optEventExpression; eventExpression'; return $ EV_SCALAR POSEDGE "hoge" }
              <?> "eventExpression"

optEventExpression :: Parser String
optEventExpression = try(do { symbol "posedge"; lexeme expression; return "expr:ok " })
                 <|> try(do { symbol "negedge"; b <- lexeme expression; return "expr:ok " })
                 <|> try(lexeme identifier)
                 <|> do { try(lexeme expression); return "expr:ok " }
                 <?> "optEventExpression"

-- XXX TODO : semantic value
eventExpression' :: Parser [EventExpr_]
eventExpression' = do { symbol "or"
                      ; e <- lexeme eventExpression
                      ; es <- eventExpression' 
                      ; return (e:es) }
               <|> do {a <- string ""; return [] }
               <?> "eventExpression'"

-- Expressions
lvalue :: Parser LValue_
lvalue = do { id <- identifier; return $ LV_IDENT id }
    <|> do { id <- lexeme identifier
           ; expr <- brackets expression
           ; return $ LV_IDENT_EXPR id expr }
    <|> do { id <- lexeme identifier
           ; r <- range
           ; return $ LV_IDENT_RANGE id r }
    <|> do { c <- concatenation; return $ LV_CONCAT c }
    <?> "lvalue"

reglValue :: Parser String
--reglValue = lvalue    -- XXX TODO : enable
reglValue = do { a <- lvalue; return "reglValue:ok " }
        <?> "reglValue"

expression :: Parser Expr_
expression = do { a <- optExpression
                ; b <- expression' <|> expression''
                ; return a }    -- XXX FIXME : !!
        <?> "expression"
    where
        optExpression = do { p <- try(primary); return $ EX_PRIMARY p }
                    <|> try(do { op <- lexeme unaryOperator
                               ; p <- lexeme primary
                               ; return $ EX_U_PRIMARY op p })
                    <|> do { str <- string'; return $ EX_STRING str }
        expression' = try(do { op <- lexeme binaryOperator
                             ; left <- lexeme expression       -- XXX left, rightをどう変形したか確認すること
                             ; right <- lexeme expression'
                             ; return $ EX_EXPR_NODE left op right })
                  <|> do { s <- string ""; return $ EX_STRING "" }
        expression''
            = try(do { lexeme questionMark
                     ; cond <- lexeme expression        -- XXX TODO : check cond, ifstmt, elsestmt order
                     ; colon
                     ; ifstmt <- lexeme expression      -- XXX TODO
                     ; elsestmt <- lexeme expression''  -- XXX TODO
                     ; return $ EX_EXPR_IFELSE cond ifstmt elsestmt })
          <|> do { s <-string ""; return $ EX_STRING "" }

-- XXX FIXME : not good impl.... use languageDef??
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
            -- XXX TODO and more...

primary :: Parser Primary_
primary = do { n <- try(number); return $ PR_NUMBER n }
      <|> try(do { id <- identifier
                 ; r <- range
                 ; return $ PR_IDENT_RANGE id r })
      <|> try(do { id <- identifier;  expr <- brackets expression; return $ PR_IDENT_EXPR id expr })
      <|> try(do { id <- lexeme identifier; return $ PR_IDENT id }) 
      <|> try(do { c <- concatenation; return $ PR_CONCAT c})
--      <|> try(multipleConcatenation)      -- XXX TODO impl
--      <|> try(functionCall)               -- XXX TODO impl
--      <|> try(parens mintypmaxExpression) -- XXX TODO impl
      <?> "primary"

number :: Parser String
number = lexeme hexNumber
     <|> lexeme octalNumber
     <|> lexeme binaryNumber
--     <|> lexeme realNumber
     <|> lexeme decimalNumber
     <?> "number"

-- XXX need lexeme??
decimalNumber :: Parser String
decimalNumber = try(do { a <- size <|> string ""
                       ; b <- decimalBase
                       ; c <- unsignedNumber
                       ; return $ a ++ b ++ c })
            <|> try(do { a <- sign <|> string ""
                       ; b <- unsignedNumber
                       ; return $ a ++ b })
            <?> "decimalNumber"

octalNumber :: Parser String
octalNumber = try(do { a <- size <|> string ""
                 ; b <- octalBase
                 ; c <- octalDigit
                 ; d <- many octalDigit'
                 ; return $ a ++ b ++ c ++ (concat d)})
        <?> "octalNumber"
    where
        octalDigit' = string "_" <|> octalDigit

binaryNumber :: Parser String
binaryNumber = try(do { a <- size <|> string ""
                  ; b <- binaryBase
                  ; c <- binaryDigit
                  ; d <- many binaryDigit'
                  ; return $ a ++ b ++ c ++ (concat d) })
            <?> "binaryNumber"
    where
        binaryDigit' = string "_" <|> binaryDigit

hexNumber :: Parser String
hexNumber = try(do { a <- size <|> string ""
               ; b <- hexBase
               ; c <- hexDigit'
               ; d <- many hexDigit''
               ; return $ a ++ b ++ c ++ (concat d) })
        <?> "hexNumber"
    where
        hexDigit'' = string "_" <|> hexDigit'

-- XXX TODO impl
realNumber :: Parser String
realNumber = string ""
        <?> "realNumber"
----

sign :: Parser String
sign = do { a <- oneOf "+-"; return [a] }
    <?> "sign"

size :: Parser String
size = unsignedNumber
    <?> "size"

unsignedNumber :: Parser String
unsignedNumber = do { a <- decimalDigit
                    ; b <- many decimalDigit'
                    ; return $ a ++ (concat b) }
            <?> "unsignedNumber"
    where
        decimalDigit' = string "_" <|> decimalDigit

decimalBase :: Parser String
decimalBase = try (string "'d") <|> string "'D"
          <?> "decimalBase"

binaryBase :: Parser String
binaryBase = try (string "'b") <|> string "'B"
        <?> "binaryBase"

octalBase :: Parser String
octalBase = try (string "'o") <|> string "'O"
       <?> "octalBase"

hexBase :: Parser String
hexBase = try (string "'h") <|> string "'H"
     <?> "hexBase"

decimalDigit :: Parser String
decimalDigit = do { a <- digit; return [a] }
           <?> "decimalDigit"

binaryDigit :: Parser String
binaryDigit = do { a <- oneOf "xXzZ01"; return [a] }
         <?> "binaryDigit"

octalDigit :: Parser String
octalDigit = do { a <- oneOf "xXzZ" <|> octDigit; return [a] }
        <?> "octalDigit"

hexDigit' :: Parser String
hexDigit' = do { a <- oneOf "xXzZ" <|> hexDigit; return [a] }
        <?> "hexDigit'"

string' :: Parser String
string' = string ""             -- XXX FIXME

questionMark :: Parser String
questionMark = symbol "?"       --- XXX FIXME
 
commaExpression :: Parser Expr_
commaExpression = do { comma; expr <- expression; return expr }
              <?> "commaExpression"

concatenation :: Parser [Expr_]
concatenation = braces concatenation'
           <?> "concatenation"
    where
        concatenation' :: Parser [Expr_]
        concatenation' = do { e <- lexeme expression
                            ; es <- lexeme(many commaExpression)
                            ; return $ (e:es) }

multipleConcatenation :: Parser String      -- XXX TODO impl
multipleConcatenation = string ""
                   <?> "multipleConcatenation"

functionCall :: Parser String       -- XXX TODO impl
functionCall = string ""
           <?> "functionCall"
