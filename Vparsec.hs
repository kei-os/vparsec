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
verilog1995 :: Parser String
verilog1995 = description
            <?> "source text!!"

description :: Parser String
description = moduleDeclaration <|> udpDeclaration
          <?> "description"
                
-- XXX MEMO : letters, digits, dollar, _ :: first must be a letter or the underscore.
-- XXX size is up to 1024. (this consraint is not implemented yet)
identifier :: Parser String
identifier = do { c <- char '_' <|> letter
                ; cs <- many (try(alphaNum) <|> try(char '_') <|> char '$')
                ; return (c:cs) }
          <?> "identifier"

moduleDeclaration :: Parser String
moduleDeclaration = do { a <- try(symbol "module")
                       ; b <- moduleDeclaration_
                       ; return $ a ++ b }
                <|> do { a <- try(symbol "macromodule")
                       ; b <- moduleDeclaration_
                       ; return $ a ++ b }
                <?> "moduleDeclaration"
    where
        moduleDeclaration_ :: Parser String
        moduleDeclaration_
              = do { a <- lexeme nameOfModule
                   ; b <- listOfPorts <|> string ""
                   ; c <- semi
                   ; d <- lexeme(many moduleItem)
                   ; e <- symbol "endmodule"
                   ; return $ a ++ b ++ c ++ (concat d) ++ e }

nameOfModule :: Parser String
nameOfModule = identifier <?> "nameOfModule"

listOfPorts :: Parser String
listOfPorts = parens listOfPorts_
            <?> "listOfPorts"
    where
        listOfPorts_ = do { whiteSpace
                          ; a <- lexeme port
                          ; b <- lexeme(many commaPorts)
                          ; return $ a ++ (concat b) }
                   <?> "listOfPorts_"

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
commaPorts = do { a <- comma
                ; b <- port
                ; return $ a ++ b }
          <?> "commaPorts"

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
commaPortReference = do { a <- comma
                        ; b <- portReference
                        ; return $ a ++ b }
                  <?> "commaPortReference"

portReference :: Parser String
portReference = do { a <- nameOfVariable
                   ; b <- portReference_
                   ; return $ a ++ b }
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


udpDeclaration :: Parser String
udpDeclaration = string ""
             <?> "udpDeclaration"

constantExpression :: Parser String
constantExpression = expression
                 <?> "constantExpression"

{--------- XXX not yet ---------}
moduleItem :: Parser String
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

parameterDeclaration :: Parser String
parameterDeclaration = do { a <- symbol "parameter"
                          ; b <- listOfParamAssignment
                          ; c <- semi
                          ; return $ a ++ b ++ c }
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

inputDeclaration :: Parser String
inputDeclaration = do { a <- symbol "input"
                      ; b <- range <|> string ""
                      ; c <- listOfPortIdentifiers
                      ; d <- semi
                      ; return $ a ++ b ++ c ++ d}
               <?> "inputDeclaration"

listOfPortIdentifiers :: Parser String
listOfPortIdentifiers = do { a <- portIdentifier
                           ; b <- many commaPortIdentifier
                           ; return $ a ++ (concat b) }
                    <?> "listOfPortIdentifiers"
    where
        commaPortIdentifier :: Parser String
        commaPortIdentifier = do { a <- comma
                                 ; b <- lexeme identifier
                                 ; return $ a ++ b }

portIdentifier :: Parser String
portIdentifier = identifier

range :: Parser String
range = brackets range_
   <?> "range"
    where
        range_ :: Parser String
        range_ = do { a <- lexeme constantExpression
                    ; b <- colon
                    ; c <- lexeme constantExpression
                    ; return $ a ++ b ++ c }

outputDeclaration :: Parser String
outputDeclaration = do { a <- symbol "output"
                       ; b <- range <|> string ""
                       ; c <- listOfPortIdentifiers
                       ; d <- semi
                       ; return $ a ++ b ++ c ++ d }
                <?> "outputDeclaration"

inoutDeclaration :: Parser String
inoutDeclaration = do { a <- symbol "inout"
                      ; b <- range <|> string ""
                      ; c <- listOfPortIdentifiers
                      ; d <- semi
                      ; return $ a ++ b ++ c ++ d }
                <?> "inoutDeclaration"

netDeclaration :: Parser String
netDeclaration = do { a <- nettype
                    ; b <- try(vecorscal) <|> string ""
                    ; c <- try(range) <|> string ""
                    ; d <- try(delay3) <|> string ""
                    ; e <- listOfNetIdentifiers
                    ; f <- semi
                    ; return $ a ++ b ++ c ++ d ++ e ++ f }
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

regDeclaration :: Parser String
regDeclaration = do { a <- symbol "reg"
                    ; b <- range <|> string ""
                    ; c <- listOfRegisterVariables
                    ; d <- semi
                    ; return $ a ++ b ++ c ++ d }
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
                      ; b <- lexeme range <|> string ""
                      ; return $ a ++ b }
              <?> "registerVariable"

timeDeclaration :: Parser String
timeDeclaration = do { a <- symbol "time"
                     ; b <- listOfRegisterVariables
                     ; c <- semi
                     ; return $ a ++ b ++ c }
              <?> "timeDeclaration"

integerDeclaration :: Parser String
integerDeclaration = do { a <- symbol "integer"
                        ; b <- listOfRegisterVariables
                        ; c <- semi
                        ; return $ a ++ b ++ c }
              <?> "integerDeclaration"

-- XXX TODO
realDeclaration :: Parser String
realDeclaration = string ""

-- XXX TODO
eventDeclaration :: Parser String
eventDeclaration = string ""

-- XXX TODO impl (check try and lexeme)
blockDeclaration :: Parser String
blockDeclaration = try(lexeme parameterDeclaration)
               <|> try(lexeme regDeclaration)
               <|> try(lexeme integerDeclaration)
--               <|> try(lexeme realDeclaration)
               <|> try(lexeme timeDeclaration)
--               <|> try(lexeme eventDeclaration)
               <?> "blockDeclaration"

-- Behavioral Statements

continuousAssign :: Parser String
continuousAssign = do { a <- symbol "assign"
                      ; b <- {- [drive_strength] [delay3] -} listOfNetAssignments
                      ; c <- semi
                      ; return $ a ++ b ++ c }
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

initialStatement :: Parser String
initialStatement = do { a <- symbol "initial"
                      ; b <- lexeme statement
                      ; return $ a ++ b }
             <?> "initialStatement"

alwaysStatement :: Parser String
alwaysStatement = do { a <- symbol "always"
                     ; b <- lexeme statement
                     ; return $ a ++ b }
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
                       ; return $ a ++ b ++ (concat c) ++ (concat d) }
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
           ; b <- range
           ; return $ a ++ b }
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
                 ; b <- range
                 ; return $ a ++ b })
      <|> try(brackets expression)
      <|> try(lexeme identifier) 
      <|> try(concatenation)
--      <|> try(multipleConcatenation)      -- XXX TODO impl
--      <|> try(functionCall)               -- XXX TODO impl
      <|> try(parens mintypmaxExpression)
      <?> "primary"

number :: Parser String
number = lexeme decimalNumber
--     <|> lexeme octalNumber
--     <|> lexeme binaryNumber
--     <|> lexeme hexNumber
--     <|> lexeme realNumber
     <?> "number"

decimalNumber :: Parser String
{-
decimalNumber = do { a <- try(oneOf "+-") <|> string ""     -- XXX FIXME : Char to String
                   ; b <- many(digit <|> char '_')
                   ; return $ a ++ (concat b) }
            <?> "decimalNumber"
-}
decimalNumber = do { a <- many1(digit <|> char '_'); return a }     -- work around
          <?> "decimalNumber"

-- XXX TODO impl
octalNumber :: Parser String
octalNumber = string ""
        <?> "octalNumber"

-- XXX TODO impl
binaryNumber :: Parser String
binaryNumber = string ""
        <?> "binaryNumber"

-- XXX TODO impl
hexNumber :: Parser String
hexNumber = string ""
        <?> "hexNumber"

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
                    ; b <- many _decimalDigit
                    ; return $ a ++ (concat b) }
            <?> "unsignedNumber"
    where
        _decimalDigit = string "_" <|> decimalDigit

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
