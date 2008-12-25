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
                 | MI_CONT_ASSIGN   String
--                 | MI_INPUT_DECL    Signals_
--                 | MI_OUTPUT_DECL   Signals_
--                 | MI_INOUT_DECL    Signals_
                 | MI_PORT_DECL     Signals_
                 | MI_REG_DECL      Signals_
                 | MI_TIME_DECL     String
                 | MI_INT_DECL      String
                 | MI_NET_DECL      Signals_
                 | MI_INITIAL       String
                 | MI_ALWAYS        String
                   deriving (Eq, Show)

data Stmt_ = ST_BLOCKING_ASSIGN     String     -- Assign_ XXX TODO : need any instance
           | ST_NON_BLOCKING_ASSIGN String     -- TODO : need any instance
           | ST_CONTINUOUS_ASSIGN   String     -- [NetAssign_]
           | ST_TIMING_CONTROL_STMT String     -- TimingControl_
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
           | StmtNil
             deriving (Eq, Show)

data Assign_ = ASSIGN LValue_ DelayOrEvent_ Expr_ deriving (Show)    -- XXX TODO : AST

data NetAssign_ = NET_ASSIGN LValue_ Expr_ deriving (Show)    -- XXX TODO : AST

data TimingControl_ = TIMING_CONTROL DelayOrEvent_ Stmt_ deriving (Show)

data SeqBlock_ = SEQ_BLOCK Stmt_ NameOfBlock_ OutputDecl_ deriving (Show)   -- temp
type NameOfBlock_ = String
type OutputDecl_ = String       -- XXX temp

data DelayOrEvent_ = DELAY_CONTROL  DelayControl_
                   | EVENT_CONTROL  EventControl_
                     deriving (Show)

data DelayControl_ = DELAY_VALUE Integer deriving (Show)
data EventControl_ = EVENT_IDENT String deriving (Show)

data Primary_ = PRIMARY____ String deriving (Show)  -- XXX FIXME : currently temp impl


data Expr_ = EX_PRIMARY Primary_
           | EX_U_PRIMARY String Primary_
           | EX_EXPR_NODE Expr_ String
           | EX_EXPR_IFELSE Expr_ Expr_ Expr_
           | EX_STRING String
             deriving (Show)

data LValue_ = LV_IDENT String
             | LV_IDENT_EXPR Expr_
             | LV_IDENT_RANGE String String String      -- identifier [ constant_expr : constant_expr ]
             | LV_CONCAT [Expr_]
               deriving (Show)

------------------------------------------------------------

type Max_ = Int
type Min_ = Int
type Width_ = Int
type Range_ = (Max_, Min_, Width_)

-- XXX TODO : reg / memory
--data Signals_ = Signals_ { name_ :: [String], range_ :: Range_ } deriving (Show, Eq, Ord)
data Signals_ = SIGNALS
    { direction_ :: Direction_
    , name_ :: [String]
    , range_ :: Range_ } deriving (Show, Eq, Ord)
data Direction_ = INPUT | OUTPUT | INOUT | NONE deriving (Eq, Show, Ord)
data SignalType_ = REG | MEM | WIRE deriving (Eq, Show)  -- and more


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
moduleDeclaration = do { try(symbol "module")
                       ; a <- moduleDeclaration'
                       ; return a }
                <|> do { try(symbol "macromodule")
                       ; a <- moduleDeclaration'
                       ; return a }
                <?> "moduleDeclaration"
    where
        moduleDeclaration' :: Parser Module_
        moduleDeclaration'
              = do { n <- lexeme nameOfModule
                   ; p <- listOfPorts
                   ; semi
                   ; m <- lexeme(many moduleItem)
                   ; symbol "endmodule"
                   ; return (MODULE { mName = n, mPorts = p, mItems = m })
                   }

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
commaPorts = do { comma
                ; a <- port
                ; return a }
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
commaPortReference = do { comma
                        ; a <- portReference
                        ; return a }
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
constantExpression = expression
                 <?> "constantExpression"

{--------- XXX not yet ---------}
--moduleItem :: Parser String
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

--parameterDeclaration :: Parser String
parameterDeclaration :: Parser ModuleItem_
parameterDeclaration = do { a <- symbol "parameter"
                          ; b <- listOfParamAssignment
                          ; c <- semi
--                          ; return $ a ++ b ++ c }
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
                      ; return $ MI_PORT_DECL $ SIGNALS { direction_ = INPUT, name_ = l, range_ = r } }
               <?> "inputDeclaration"

listOfPortIdentifiers :: Parser [String]
listOfPortIdentifiers = do { a <- portIdentifier
                           ; b <- many commaPortIdentifier
--                           ; return $ a ++ (concat b) }
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
range = do { symbol "["
           ; a <- range'
           ; symbol "]"
           ; return a }
   <?> "range"
    where
--        range' :: Parser String
        range' :: Parser Range_
        range' = do { max <- lexeme constantExpression
                    ; colon
                    ; min <- lexeme constantExpression
--                    ; return $ a ++ b ++ c }
                    ; return (read max, read min, (read max) - (read min) + 1 ) }   -- XXX TODO improve
              <?> "range'"

outputDeclaration :: Parser ModuleItem_
outputDeclaration = do { symbol "output"
                       ; r <- rangeOrEmpty
                       ; l <- listOfPortIdentifiers
                       ; semi
                       ; return $ MI_PORT_DECL $ SIGNALS { direction_ = OUTPUT, name_ = l, range_ = r } }
                <?> "outputDeclaration"

inoutDeclaration :: Parser ModuleItem_
inoutDeclaration = do { symbol "inout"
                      ; r <- rangeOrEmpty
                      ; l <- listOfPortIdentifiers
                      ; semi
                      ; return $ MI_PORT_DECL $ SIGNALS { direction_ = INOUT, name_ = l, range_ = r } }
                <?> "inoutDeclaration"

netDeclaration :: Parser ModuleItem_
netDeclaration = do { nettype       -- XXX TODO : use SignalType_
                    ; try(vecorscal) <|> string ""
                    ; r <- rangeOrEmpty
                    ; try(delay3) <|> string ""     -- XXX TODO : impl
                    ; n <- listOfNetIdentifiers
                    ; semi
                    ; return $ MI_NET_DECL $ SIGNALS { direction_ = NONE, name_ = n, range_ = r } } -- XXX FIXME : direction_
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
                    ; return $ MI_REG_DECL $ SIGNALS { direction_ = NONE, name_ = l, range_ = r } }  -- XXX FIXME : direction_
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

--timeDeclaration :: Parser String
timeDeclaration :: Parser ModuleItem_
timeDeclaration = do { a <- symbol "time"
                     ; b <- listOfRegisterVariables
                     ; c <- semi
                     ; return $ MI_TIME_DECL $ a ++ (concat b) ++ c }
              <?> "timeDeclaration"

--integerDeclaration :: Parser String
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
--blockDeclaration :: Parser String
blockDeclaration :: Parser ModuleItem_
blockDeclaration = try(lexeme parameterDeclaration)
               <|> try(lexeme regDeclaration)
               <|> try(lexeme integerDeclaration)
--               <|> try(lexeme realDeclaration)
               <|> try(lexeme timeDeclaration)
--               <|> try(lexeme eventDeclaration)
               <?> "blockDeclaration"

-- Behavioral Statements

--continuousAssign :: Parser String
continuousAssign :: Parser ModuleItem_
continuousAssign = do { a <- symbol "assign"
                      ; b <- {- [drive_strength] [delay3] -} listOfNetAssignments
                      ; c <- semi
--                      ; return $ a ++ b ++ c }
                      ; return $ MI_CONT_ASSIGN $ a ++ b ++ c }
              <?> "continuousAssign"

listOfNetAssignments :: Parser String
listOfNetAssignments = do { a <- netAssignment
                          ; b <- many (lexeme commaNetAssignment)
                          ; return $ a ++ (concat b) }
                   <?> "listOfNetAssignments"
    where
        commaNetAssignment = do { a <- comma
                                ; b <- netAssignment
                                ; return $ a ++ b }

netAssignment :: Parser String
netAssignment = do { a <- lexeme lvalue
                   ; b <- symbol "="
                   ; c <- lexeme expression
                   ; return $ a ++ b ++ c }
            <?> "netAssignment"

--initialStatement :: Parser String
initialStatement :: Parser ModuleItem_
initialStatement = do { a <- symbol "initial"
                      ; b <- lexeme statement
--                      ; return $ a ++ b }
                      ; return $ MI_INITIAL $ a ++ b }
             <?> "initialStatement"

--alwaysStatement :: Parser String
alwaysStatement :: Parser ModuleItem_
alwaysStatement = do { a <- symbol "always"
                     ; b <- lexeme statement
--                     ; return $ a ++ b }
                     ; return $ MI_ALWAYS $ a ++ b }
              <?> "alwaysStatement"

statementOrNull :: Parser String
statementOrNull = try(lexeme statement) <|> semi
              <?> "statementOrNull"

-- XXX this BNF is from IEEE spec.
statement :: Parser String
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
assignment = do { a <- lexeme lvalue
                ; b <- symbol "="
                ; c <- expression
                ; return $ a ++ b ++ c } <?> "assignment"

commaAssignment :: Parser String
commaAssignment = do { a <- comma
                     ; b <- assignment
                     ; return $ a ++ b } <?> "commaAssignment"

blockingAssignment :: Parser String
blockingAssignment = try(do { a <- lexeme lvalue
                            ; b <- symbol "="
                            ; c <- expression
                            ; return $ a ++ b ++ c })
                <|> try(do { a <- lexeme lvalue
                       ; b <- symbol "="
                       ; c <- lexeme delayOrEventControl
                       ; d <- lexeme expression
                       ; e <- semi
                       ; return $ a ++ b ++ c ++ d ++ e })
                <?> "blockingAssignment"

nonBlockingAssignment :: Parser String
nonBlockingAssignment = try(do { a <- lexeme lvalue
                            ; b <- symbol "<="
                            ; c <- expression
                            ; return $ a ++ b ++ c })
                <|> try(do { a <- lexeme lvalue
                       ; b <- symbol "<="
                       ; c <- lexeme delayOrEventControl
                       ; d <- lexeme expression
                       ; e <- semi
                       ; return $ a ++ b ++ c ++ d ++ e })
                <?> "nonBlockingAssignment"

proceduralContinuousAssignments :: Parser String
proceduralContinuousAssignments = try(do { a <- symbol "assign"
                                         ; b <- lexeme regAssignment
                                         ; c <- semi
                                         ; return $ a ++ b ++ c}) 
                              <|> try(do { a <- symbol "deassign"
                                         ; b <- lexeme reglValue
                                         ; c <- semi
                                         ; return $ a ++ b ++ c })
--                              <|> force regAssignment;    -- not impl
--                              <|> force netAssignment;    -- not impl
--                              <|> release reglValue;      -- not impl
--                              <|> release netlValue;      -- not impl
                              <?> "proceduralContinuousAssignments"

proceduralTimingControlStatement :: Parser String
proceduralTimingControlStatement = do { a <- delayOrEventControl
                                      ; b <- statementOrNull
                                      ; return $ a ++ b }
                               <?> "proceduralTimingControlStatement"

conditionalStatement :: Parser String
conditionalStatement = do { a <- symbol "if"
                          ; b <- parens expression
                          ; c <- statementOrNull
                          ; d <- try(elseStatementOrNull) <|> string ""
                          ; return $ a ++ b ++ c ++ d }
                   <?> "conditionalStatement"
    where
        elseStatementOrNull = do { a <- symbol "else"
                                 ; b <- statementOrNull
                                 ; return $ a ++ b }

regAssignment :: Parser String
regAssignment = do { a <- lexeme reglValue
                   ; b <- symbol "="
                   ; c <- lexeme expression
                   ; return $ a ++ b ++ c }
            <?> "regAssignment"

seqBlock :: Parser String
seqBlock = do { a <- symbol "begin"
              ; b <- seqBlock'
              ; c <- symbol "end"
              ; return $ a ++ b ++ c }
       <?> "seqBlock"
    where
        seqBlock' = do {a <- many statement; return (concat a)}
                <|> do { a <- colon
                       ; b <- identifier
                       ; c <- many blockDeclaration
                       ; d <- many statement
--                       ; return $ a ++ b ++ (concat c) ++ (concat d) }
                       ; return $ a ++ b ++ {-(concat c) ++ -} (concat d) }  -- XXX test for AST
                <?> "seqBlock'"

delayOrEventControl :: Parser String
delayOrEventControl = try(delayControl)
                  <|> try(do { a <- eventControl; return a })
                  <|> do { a <- symbol "repeat"
                         ; b <- parens expression
                         ; c <- eventControl
                         ; return $ a ++ b ++ c }
                  <?> "delayOrEventControl"

delayControl :: Parser String
delayControl = try(do{ a <- symbol "#"
                     ; b <- number
                     ; return $ a ++ b })
           <|> try(do { a <- symbol "#"
                      ; b <- lexeme identifier
                      ; return $ a ++ b })
           <|> do { a <- symbol "#"
                  ; b <- parens mintypmaxExpression
                  ; return $ a ++ b }
           <?> "delayControl"

mintypmaxExpression :: Parser String
mintypmaxExpression = try(do { a <- lexeme expression
                         ; b <- colon
                         ; c <- lexeme expression
                         ; d <- colon
                         ; e <- lexeme expression
                         ; return $ a ++ b ++ c ++ d ++ e })
                  <|> lexeme expression
                  <?> "mintypmaxExpression"

eventControl :: Parser String
eventControl = try(do { a <- symbol "@"
                  ; b <- lexeme identifier
                  ; return $ a ++ b })
           <|> do { a <- symbol "@"
                  ; b <- parens eventExpression
                  ; return $ a ++ b }
           <?> "eventControl"

-- XXX use IEEE's BNF (need to omit left recursion)
eventExpression :: Parser String
eventExpression = do { a <- optEventExpression
                     ; b <- eventExpression'
                     ; return $ a ++ b }
              <?> "eventExpression"

optEventExpression :: Parser String
optEventExpression = try(do { a <- symbol "posedge"
                            ; b <- lexeme expression
                            ; return $ a ++ b })
                 <|> try(do { a <- symbol "negedge"
                            ; b <- lexeme expression
                            ; return $ a ++ b })
                 <|> try(lexeme identifier)
                 <|> try(lexeme expression)
                 <?> "optEventExpression"

eventExpression' :: Parser String
eventExpression' = do { a <- symbol "or"
                      ; b <- lexeme eventExpression
                      ; c <- eventExpression' 
                      ; return $ a ++ b ++ c }
               <|> string ""
               <?> "eventExpression'"

-- Expressions

lvalue :: Parser String
lvalue = identifier
    <|> do { a <- lexeme identifier
           ; b <- brackets expression
           ; return $ a ++ b }
    <|> do { a <- lexeme identifier
--           ; b <- range
--           ; return $ a ++ b }
           ; range
           ; return a }
    <|> concatenation
    <?> "lvalue"

-- same as lvalue
reglValue :: Parser String
reglValue = lvalue
       <?> "reglValue"

-- XXX omit left recursion
expression :: Parser String
expression = do { a <- optExpression
                ; b <- expression' <|> expression''
                ; return $ a ++ b }
        <?> "expression"
    where
        optExpression = try(primary)
                    <|> try(do { a <- lexeme unaryOperator
                               ; b <- lexeme primary
                               ; return $ a ++ b })
                    <|> string'
        expression' = try(do { a <- lexeme binaryOperator
                             ; b <- lexeme expression
                             ; c <- lexeme expression'
                             ; return $ a ++ b ++ c })
                  <|> string ""
        expression''
            = try(do { a <- lexeme questionMark
                     ; b <- lexeme expression
                     ; c <- colon
                     ; d <- lexeme expression
                     ; e <- lexeme expression''
                     ; return $ a ++ b ++ c ++ d ++ e })
          <|> string ""

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

primary :: Parser String
primary = try(number)
      <|> try(do { a <- identifier
--                ; b <- range
--                ; return $ a ++ b })
                 ; range
                 ; return a })
      <|> try(brackets expression)
      <|> try(lexeme identifier) 
      <|> try(concatenation)
--      <|> try(multipleConcatenation)      -- XXX TODO impl
--      <|> try(functionCall)               -- XXX TODO impl
      <|> try(parens mintypmaxExpression)
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
 
commaExpression :: Parser String
commaExpression = do { a <- comma
                     ; b <- expression
                     ; return $ a ++ b }

concatenation :: Parser String
concatenation = braces concatenation'
           <?> "concatenation"
    where
        concatenation' = do { a <- lexeme expression
                            ; b <- lexeme(many commaExpression)
                            ; return $ a ++ (concat b) }

multipleConcatenation :: Parser String      -- XXX TODO impl
multipleConcatenation = string ""
                   <?> "multipleConcatenation"

functionCall :: Parser String       -- XXX TODO impl
functionCall = string ""
           <?> "functionCall"
