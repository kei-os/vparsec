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
data Module = Module
    { mName     :: String
    , mPorts    :: [String]
    , mItems    :: [ModuleItem]
    } deriving (Eq, Show)

-- XXX test for AST
data ModuleItem = MIDECL        String
                | PARAM_DECL    String
                | CONT_ASSIGN   String
                | INPUT_DECL    PortDecl
                | OUTPUT_DECL   PortDecl
                | INOUT_DECL    String
                | REG_DECL      String
                | TIME_DECL     String
                | INT_DECL      String
                | NET_DECL      String
                | INITIAL       String
                | ALWAYS        String deriving (Eq, Show)

type Max = Int
type Min = Int
type Width = Int
type Range = (Max, Min, Width)

data PortDecl = PortDecl { pName :: [String], pRange :: Range } deriving (Show, Eq, Ord)

data Direction = Input | Output | Inout deriving (Eq, Show)

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
         ; putStr input
         ; case parse verilog1995 fname input of
                Left err -> do { putStr "Error parsing at : " ; print err }
                Right x -> print x }

-- Verilog 1995 parser
verilog1995 :: Parser Module
verilog1995 = description
            <?> "source text!!"

description :: Parser Module
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
moduleDeclaration :: Parser Module
moduleDeclaration = do { try(symbol "module")
                       ; a <- moduleDeclaration_
                       ; return a }
                <|> do { try(symbol "macromodule")
                       ; a <- moduleDeclaration_
                       ; return a }
                <?> "moduleDeclaration"
    where
        moduleDeclaration_ :: Parser Module
        moduleDeclaration_
              = do { a <- lexeme nameOfModule           -- Name
--                   ; b <- listOfPorts <|> string ""     -- Ports    XXX
                   ; b <- listOfPorts
                   ; semi
                   ; c <- lexeme(many moduleItem)       -- Items
                   ; symbol "endmodule"
                   ; return (Module { mName = a, mPorts = b, mItems = c })
                   }

nameOfModule :: Parser String
nameOfModule = identifier <?> "nameOfModule"

listOfPorts :: Parser [String]
listOfPorts = parens listOfPorts_
          <|> do { string ""; return [] }
          <?> "listOfPorts"
    where
        listOfPorts_ = do { whiteSpace
                          ; a <- lexeme port
                          ; b <- lexeme(many commaPorts)
--                          ; return $ a ++ (concat b) }
                          ; return (a:b) }
                   <?> "listOfPorts_"

-- XXX TODO : AST
port :: Parser String
port = try(portExpression)
    <|> do { a <- dot
           ; b <- lexeme nameOfPort
           ; c <- parens port_
           ; return $ a ++ b ++ c }
    <|> string ""
    <?> "port"
        where
            port_ :: Parser String
            port_ = portExpression
                <|> string ""
                <?> "port_"

commaPorts :: Parser String
commaPorts = do { comma
                ; a <- port
                ; return a }
          <?> "commaPorts"

-- XXX TODO : AST
portExpression :: Parser String
portExpression = try(portReference)
             <|> braces portExpression_
             <?> "portExpression"
                where
                    portExpression_ :: Parser String
                    portExpression_ = do { a <- portReference
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
--                   ; b <- portReference_    -- XXX get port information
                   ; portReference_    -- XXX get port information  (currently not use lower block's info)
                   ; return a }
            <?> "portReference"
    where
        portReference_ :: Parser String
        portReference_ = do { a <- brackets constantExpression_; return a }
                      <|> string ""
        constantExpression_ :: Parser String
        constantExpression_ = constantExpression
                          <|> do { a <- lexeme constantExpression
                                 ; b <- colon
                                 ; c <- lexeme constantExpression
                                 ; return $ a ++ b ++ c}

nameOfPort :: Parser String
nameOfPort = identifier <?> "nameOfPort"

nameOfVariable :: Parser String
nameOfVariable = identifier <?> "nameOfVariable"

-------------------------------------------------------------------------------------

udpDeclaration :: Parser Module
udpDeclaration = return (Module { mName = "none", mPorts = [], mItems = [] })
             <?> "udpDeclaration"

constantExpression :: Parser String
constantExpression = expression
                 <?> "constantExpression"

{--------- XXX not yet ---------}
--moduleItem :: Parser String
moduleItem :: Parser ModuleItem
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
parameterDeclaration :: Parser ModuleItem
parameterDeclaration = do { a <- symbol "parameter"
                          ; b <- listOfParamAssignment
                          ; c <- semi
--                          ; return $ a ++ b ++ c }
                          ; return $ PARAM_DECL $ a ++ b ++ c }
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

inputDeclaration :: Parser ModuleItem
inputDeclaration = do { symbol "input"
                      ; r <- rangeOrEmpty
                      ; l <- listOfPortIdentifiers
                      ; semi
                      ; return $ INPUT_DECL $ PortDecl { pName = l, pRange = r } }
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

rangeOrEmpty :: Parser Range
rangeOrEmpty = try(lexeme range)
           <|> do { string ""; return (0, 0, 1) }
           <?> "rangeOrEmpty"

--range :: Parser String
range :: Parser Range
--range = brackets range_
range = do { symbol "["
           ; a <- range_
           ; symbol "]"
           ; return a }
   <?> "range"
    where
--        range_ :: Parser String
        range_ :: Parser Range
        range_ = do { max <- lexeme constantExpression
                    ; colon
                    ; min <- lexeme constantExpression
--                    ; return $ a ++ b ++ c }
                    ; return (read max, read min, (read max) - (read min) + 1 ) }   -- XXX TODO improve
              <?> "range_"

outputDeclaration :: Parser ModuleItem
outputDeclaration = do { symbol "output"
                       ; r <- rangeOrEmpty
                       ; l <- listOfPortIdentifiers
                       ; semi
--                       ; return $ OUTPUT_DECL $ a {-++ b ++ c-} ++ d }
                       ; return $ OUTPUT_DECL $ PortDecl { pName = l, pRange = r } }
                <?> "outputDeclaration"

--inoutDeclaration :: Parser String
inoutDeclaration :: Parser ModuleItem
inoutDeclaration = do { a <- symbol "inout"
--                      ; b <- range <|> string ""
                      ; rangeOrEmpty    -- XXX TODO : impl range
--                      ; c <- listOfPortIdentifiers
                      ; listOfPortIdentifiers
                      ; d <- semi
--                      ; return $ a ++ b ++ c ++ d }
                      ; return $ INOUT_DECL $ a {-++ b ++ c-} ++ d }
                <?> "inoutDeclaration"

--netDeclaration :: Parser String
netDeclaration :: Parser ModuleItem
netDeclaration = do { a <- nettype
                    ; b <- try(vecorscal) <|> string ""
--                    ; c <- try(range) <|> string ""
                    ; rangeOrEmpty      -- XXX TODO : impl range
                    ; d <- try(delay3) <|> string ""
                    ; e <- listOfNetIdentifiers
                    ; f <- semi
--                    ; return $ a ++ b ++ c ++ d ++ e ++ f }
                    ; return $ NET_DECL $ a ++ b {-++ c-} ++ d ++ e ++ f }
            <?> "netDeclaration"
    where
        vecorscal :: Parser String
        vecorscal = brackets vecorscal_
        vecorscal_ = do { a <- symbol "vectored"
                        ; b <- symbol "|"
                        ; c <- symbol "scalared"
                        ; return $ a ++ b ++ c }

nettype :: Parser String
nettype = try(symbol "wire")        -- XXX TODO : impl all types
      <?> "nettype"

listOfNetIdentifiers :: Parser String
listOfNetIdentifiers = do { a <- identifier
                          ; b <- many commaNetIdentifier
                          ; return $ a ++ (concat b) }
    where
        commaNetIdentifier :: Parser String
        commaNetIdentifier = do { a <- comma; b <- identifier; return $ a ++ b }

delay3 :: Parser String
delay3 = string ""      -- XXX TODO : impl

--regDeclaration :: Parser String
regDeclaration :: Parser ModuleItem
regDeclaration = do { a <- symbol "reg"
--                    ; b <- range <|> string ""
                    ; rangeOrEmpty  -- XXX TODO : impl range
                    ; c <- listOfRegisterVariables
                    ; d <- semi
--                    ; return $ a ++ b ++ c ++ d }
                    ; return $ REG_DECL $ a {-++ b -} ++ c ++ d }
             <?> "regDeclaration"

listOfRegisterVariables :: Parser String
listOfRegisterVariables = do { a <- lexeme registerVariable
                             ; b <- lexeme(many commaRegisterVariable)
                             ; return $ a ++ (concat b) }
                     <?> "listOfRegisterVariables"
    where
        commaRegisterVariable :: Parser String
        commaRegisterVariable = do { a <- comma
                                   ; b <- registerVariable
                                   ; return $ a ++ b }

registerVariable :: Parser String
registerVariable = do { a <- lexeme identifier
--                      ; b <- lexeme range <|> string ""
--                      ; return $ a ++ b }
                      ; rangeOrEmpty    -- XXX TODO : impl range
                      ; return a }
              <?> "registerVariable"

--timeDeclaration :: Parser String
timeDeclaration :: Parser ModuleItem
timeDeclaration = do { a <- symbol "time"
                     ; b <- listOfRegisterVariables
                     ; c <- semi
--                     ; return $ a ++ b ++ c }
                     ; return $ TIME_DECL $ a ++ b ++ c }
              <?> "timeDeclaration"

--integerDeclaration :: Parser String
integerDeclaration :: Parser ModuleItem
integerDeclaration = do { a <- symbol "integer"
                        ; b <- listOfRegisterVariables
                        ; c <- semi
--                        ; return $ a ++ b ++ c }
                        ; return $ INT_DECL $ a ++ b ++ c }
              <?> "integerDeclaration"

-- XXX TODO
realDeclaration :: Parser String
realDeclaration = string ""

-- XXX TODO
eventDeclaration :: Parser String
eventDeclaration = string ""

-- XXX TODO impl (check try and lexeme)
--blockDeclaration :: Parser String
blockDeclaration :: Parser ModuleItem
blockDeclaration = try(lexeme parameterDeclaration)
               <|> try(lexeme regDeclaration)
               <|> try(lexeme integerDeclaration)
--               <|> try(lexeme realDeclaration)
               <|> try(lexeme timeDeclaration)
--               <|> try(lexeme eventDeclaration)
               <?> "blockDeclaration"

-- Behavioral Statements

--continuousAssign :: Parser String
continuousAssign :: Parser ModuleItem
continuousAssign = do { a <- symbol "assign"
                      ; b <- {- [drive_strength] [delay3] -} listOfNetAssignments
                      ; c <- semi
--                      ; return $ a ++ b ++ c }
                      ; return $ CONT_ASSIGN $ a ++ b ++ c }
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
initialStatement :: Parser ModuleItem
initialStatement = do { a <- symbol "initial"
                      ; b <- lexeme statement
--                      ; return $ a ++ b }
                      ; return $ INITIAL $ a ++ b }
             <?> "initialStatement"

--alwaysStatement :: Parser String
alwaysStatement :: Parser ModuleItem
alwaysStatement = do { a <- symbol "always"
                     ; b <- lexeme statement
--                     ; return $ a ++ b }
                     ; return $ ALWAYS $ a ++ b }
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
              ; b <- seqBlock_
              ; c <- symbol "end"
              ; return $ a ++ b ++ c }
       <?> "seqBlock"
    where
        seqBlock_ = do {a <- many statement; return (concat a)}
                <|> do { a <- colon
                       ; b <- identifier
                       ; c <- many blockDeclaration
                       ; d <- many statement
--                       ; return $ a ++ b ++ (concat c) ++ (concat d) }
                       ; return $ a ++ b ++ {-(concat c) ++ -} (concat d) }  -- XXX test for AST
                <?> "seqBlock_"

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
                     ; b <- eventExpression_
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

eventExpression_ :: Parser String
eventExpression_ = do { a <- symbol "or"
                      ; b <- lexeme eventExpression
                      ; c <- eventExpression_ 
                      ; return $ a ++ b ++ c }
               <|> string ""
               <?> "eventExpression_"

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
                ; b <- expression_ <|> expression__
                ; return $ a ++ b }
        <?> "expression"
    where
        optExpression = try(primary)
                    <|> try(do { a <- lexeme unaryOperator
                               ; b <- lexeme primary
                               ; return $ a ++ b })
                    <|> string'
        expression_ = try(do { a <- lexeme binaryOperator
                             ; b <- lexeme expression
                             ; c <- lexeme expression_
                             ; return $ a ++ b ++ c })
                  <|> string ""
        expression__        
            = try(do { a <- lexeme questionMark
                     ; b <- lexeme expression
                     ; c <- colon
                     ; d <- lexeme expression
                     ; e <- lexeme expression__
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
                 ; d <- many octalDigit_
                 ; return $ a ++ b ++ c ++ (concat d)})
        <?> "octalNumber"
    where
        octalDigit_ = string "_" <|> octalDigit

binaryNumber :: Parser String
binaryNumber = try(do { a <- size <|> string ""
                  ; b <- binaryBase
                  ; c <- binaryDigit
                  ; d <- many binaryDigit_
                  ; return $ a ++ b ++ c ++ (concat d) })
            <?> "binaryNumber"
    where
        binaryDigit_ = string "_" <|> binaryDigit

hexNumber :: Parser String
hexNumber = try(do { a <- size <|> string ""
               ; b <- hexBase
               ; c <- _hexDigit
               ; d <- many _hexDigit_
               ; return $ a ++ b ++ c ++ (concat d) })
        <?> "hexNumber"
    where
        _hexDigit_ = string "_" <|> _hexDigit

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
                    ; b <- many decimalDigit_
                    ; return $ a ++ (concat b) }
            <?> "unsignedNumber"
    where
        decimalDigit_ = string "_" <|> decimalDigit

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

_hexDigit :: Parser String
_hexDigit = do { a <- oneOf "xXzZ" <|> hexDigit; return [a] }
        <?> "_hexDigit"

string' :: Parser String
string' = string ""             -- XXX FIXME

questionMark :: Parser String
questionMark = symbol "?"       --- XXX FIXME
 
commaExpression :: Parser String
commaExpression = do { a <- comma
                     ; b <- expression
                     ; return $ a ++ b }

concatenation :: Parser String
concatenation = braces concatenation_
           <?> "concatenation"
    where
        concatenation_ = do { a <- lexeme expression
                            ; b <- lexeme(many commaExpression)
                            ; return $ a ++ (concat b) }

multipleConcatenation :: Parser String      -- XXX TODO impl
multipleConcatenation = string ""
                   <?> "multipleConcatenation"

functionCall :: Parser String       -- XXX TODO impl
functionCall = string ""
           <?> "functionCall"
