module Vparsec.Types where

data Module_ = MODULE
    { mName     :: String
    , mPorts    :: [String]
    , mItems    :: [ModuleItem_]
    } deriving (Eq, Show)

data ModuleItem_ = MI_PARAM_DECL    String              -- XXX TODO impl
                 | MI_CONT_ASSIGN   [NetAssign_]
                 | MI_PORT_DECL     Sig_
                 | MI_REG_DECL      Sig_
                 | MI_TIME_DECL     Sig_
                 | MI_INT_DECL      Sig_
                 | MI_NET_DECL      Sig_
                 | MI_INITIAL       Stmt_
                 | MI_ALWAYS        Stmt_
                   deriving (Eq, Show)

data Stmt_ = ST_BLOCKING_ASSIGN     BlockAssign_
           | ST_NON_BLOCKING_ASSIGN BlockAssign_
           | ST_PROCEDURAL_ASSIGN   RegAssign_
           | ST_TIMING_CONTROL_STMT TimingControl_
           | ST_CONDITIONAL_STMT    Condition_
--           | ST_CASE_STMT           String
--           | ST_LOOP_STMT           String
--           | ST_WAIT_STMT           String
--           | ST_DISABLE_STMT        String
--           | ST_EVENT_TRIGGER       String
             | ST_SEQ_BLOCK           Block_
--           | ST_PAR_BLOCK           String
--           | ST_TASK_ENABLE         String
--           | ST_SYSTEM_TASK_ENABLE  String
           | ST_NIL
             deriving (Eq, Show)

data RegAssign_ = REG_ASSIGN LValue_ Expr_ deriving (Eq, Show)        -- normal assignment (regAssignment)

data NBAssign_ = ASSIGN LValue_ DelayOrEvent_ Expr_ deriving (Eq, Show)     -- blockingAssignment
type BAssign_ = NBAssign_       -- nonBlockingAssignment

data NetAssign_ = NET_ASSIGN LValue_ Expr_ deriving (Eq, Show)

-- XXX first, support only "assign"
data ProcAssign_ = PROC_ASSIGN LValue_ Expr_ deriving (Eq, Show)

data TimingControl_ = TIMING_CONTROL DelayOrEvent_ Stmt_ deriving (Eq, Show)

data Condition_ = CONDITION CondExpr_ IfStmt_ ElseStmt_ deriving (Eq, Show)
type IfStmt_ = Stmt_
type ElseStmt_ = Stmt_

--data SeqBlock_ = SEQ_BLOCK Stmt_ NameOfBlock_ OutputDecl_ deriving (Eq, Show)   -- temp
--type NameOfBlock_ = String
--type OutputDecl_ = String       -- XXX temp

data DelayOrEvent_ = DE_DELAY_CONTROL DelayControl_
                   | DE_EVENT_CONTROL [Event_]
                   | DE_EXPR_EV Expr_ [Event_]
                   | DE_NIL
                     deriving (Eq, Show)

--data DelayControl_ = DL_NUM Integer | DL_IDENT String deriving (Eq, Show)
data DelayControl_ = DL_NUM Number_ | DL_IDENT String deriving (Eq, Show)

data Edge_ = POS | NEG | VALUE deriving (Eq, Show)
data Event_ = EVENT Edge_ Expr_ deriving (Eq, Show) -- new


data Primary_ = PR_NUMBER Number_       -- XXX TODO : test (Number_)
              | PR_IDENT String
              | PR_IDENT_EXPR String Expr_
              | PR_IDENT_RANGE String Range_
              | PR_CONCAT [Expr_]
              | PR_MINMAX_EXPR Expr_ Expr_ Expr_    -- min ( ,typ ,max)    -- XXX TODO : check order
                deriving (Eq, Show)

type UnaryOp_   = String
type BinaryOp_  = String
type CondExpr_  = Expr_
type IfExpr_    = Expr_
type ElseExpr_  = Expr_

data Expr_ = EX_PRIMARY Primary_
           | EX_U_PRIMARY UnaryOp_ Primary_
           | EX_NODE Expr_ BinaryOp_ Expr_             -- left op right
           | EX_COND CondExpr_ IfExpr_ ElseExpr_     -- cond ifexpr elseexpr
           | EX_STRING String
           | EX_NIL
             deriving (Eq, Show)

data LValue_ = LV_IDENT String
             | LV_IDENT_EXPR String Expr_
             | LV_IDENT_RANGE String Range_      -- identifier [ constant_expr : constant_expr ]
             | LV_CONCAT [Expr_]
               deriving (Eq, Show)

data BlockAssign_ = BLOCK_ASSIGN LValue_ DelayOrEvent_ Expr_ deriving (Eq, Show)

data BlockItem_ = BI_PARAM          -- XXX TODO impl
                | BI_REG Sig_
                | BI_INT            -- XXX TODO impl
                | BI_REAL           -- XXX TODO impl
                | BI_TIME           -- XXX TODO impl
                | BI_REALTIME       -- XXX TODO impl
                | BI_EVENT          -- XXX TODO impl
                  deriving (Eq, Show)

data Block_ = BLOCK String [BlockItem_] [Stmt_] deriving (Eq, Show)

------------------------------------------------------------

-- should i use Integer (not Int)??
type Typ_ = Int
--type Max_ = Int
type Max_ = Number_     -- XXX TODO : test
--type Min_ = Int
type Min_ = Number_     -- XXX TODO : test
--type Width_ = Int
type Width_ = String    -- XXX FIXME : temp for Number_
type Range_ = (Max_, Min_, Width_)

-- XXX TODO : reg / memory
data Sig_ = PORT_SIG { direction_ :: Direction_ , name_ :: [String] , range_ :: Range_ }
          | NET_SIG { netType_ :: NetType_, name_ :: [String], range_ :: Range_ }
          | REG_SIG { regType_ :: RegType_, name_ :: [String], range_ :: Range_ }
            deriving (Eq, Show, Ord)

data Direction_ = IN | OUT | INOUT | NONE deriving (Eq, Show, Ord)
data NetType_ = NET_WIRE | NET_TRI | NET_TRI1 | NET_SUPPLY0 | NET_WAND
              | NET_TRIAND | NET_TRI0 | NET_SUPPLY1 | NET_WOR | NET_TRIOR
                deriving (Eq, Show, Ord)

data RegType_ = REG | MEM deriving (Eq, Show, Ord)

data NumType_ = NUM_BIN | NUM_OCT | NUM_DEC | NUM_HEX deriving (Eq, Show, Ord)
data Number_ = NUMBER NumType_ String String deriving (Eq, Show, Ord)   -- type size value

