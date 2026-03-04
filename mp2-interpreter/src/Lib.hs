module Lib where
import Data.HashMap.Strict as H (HashMap, empty, fromList, insert, lookup, union)


--- Data Types
--- ----------

--- ### Environments and Results

type Env  = H.HashMap String Val
type PEnv = H.HashMap String Stmt

type Result = (String, PEnv, Env)

--- ### Values

data Val = IntVal Int
         | BoolVal Bool
         | CloVal [String] Exp Env
         | ExnVal String
    deriving (Eq)

instance Show Val where
    show (IntVal i) = show i
    show (BoolVal i) = show i
    show (CloVal xs body env) = "<" ++ show xs   ++ ", "
                                    ++ show body ++ ", "
                                    ++ show env  ++ ">"
    show (ExnVal s) = "exn: " ++ s

--- ### Expressions

data Exp = IntExp Int
         | BoolExp Bool
         | FunExp [String] Exp
         | LetExp [(String,Exp)] Exp
         | AppExp Exp [Exp]
         | IfExp Exp Exp Exp
         | IntOpExp String Exp Exp
         | BoolOpExp String Exp Exp
         | CompOpExp String Exp Exp
         | VarExp String
    deriving (Show, Eq)

--- ### Statements

data Stmt = SetStmt String Exp
          | PrintStmt Exp
          | QuitStmt
          | IfStmt Exp Stmt Stmt
          | ProcedureStmt String [String] Stmt
          | CallStmt String [Exp]
          | SeqStmt [Stmt]
    deriving (Show, Eq)

--- Primitive Functions
--- -------------------

intOps :: H.HashMap String (Int -> Int -> Int)
intOps = H.fromList [ ("+", (+))
                    , ("-", (-))
                    , ("*", (*))
                    , ("/", (div))
                    ]

boolOps :: H.HashMap String (Bool -> Bool -> Bool)
boolOps = H.fromList [ ("and", (&&))
                     , ("or", (||))
                     ]

compOps :: H.HashMap String (Int -> Int -> Bool)
compOps = H.fromList [ ("<", (<))
                     , (">", (>))
                     , ("<=", (<=))
                     , (">=", (>=))
                     , ("/=", (/=))
                     , ("==", (==))
                     ]

--- Problems
--- ========

--- Lifting Functions
--- -----------------

-- Arguments...
-- 1. function that takes binary inputs, returns an integer
-- 2. two Val data types
-- Functionality...
-- Returns a Val data type from the 1st arguments return, type casting it to Val
liftIntOp :: (Int -> Int -> Int) -> Val -> Val -> Val
liftIntOp op (IntVal x) (IntVal y) = IntVal $ op x y
liftIntOp _ _ _ = ExnVal "Cannot lift"

liftBoolOp :: (Bool -> Bool -> Bool) -> Val -> Val -> Val
liftBoolOp boolOp (BoolVal x) (BoolVal y) = BoolVal (boolOp x y)
liftBoolOp _ _ _                          = ExnVal "Cannot lift"

liftCompOp :: (Int -> Int -> Bool) -> Val -> Val -> Val
liftCompOp compOp (IntVal x) (IntVal y) = BoolVal (compOp x y)
liftCompOp _ _ _                          = ExnVal "Cannot lift"


--- Eval
--- ----

eval :: Exp -> Env -> Val

--- ### Constants

eval (IntExp i)  _ = IntVal i
eval (BoolExp i) _ = BoolVal i

--- ### Variables

eval (VarExp s) env =
    -- perform a lookup to see if this var is in our env, otherwise, we return an Exception
    case H.lookup s env of
        Just vv -> vv
        Nothing -> ExnVal "No match in env"

--- ### Arithmetic

eval (IntOpExp op e1 e2) env =
    let 
        v1 = eval e1 env
        v2 = eval e2 env
        Just f = H.lookup op intOps -- for now, we do not need to care about empty lookups
    in 
        case (op, v2) of 
            ("/", IntVal 0) -> ExnVal "Division by 0" 
            _ -> liftIntOp f v1 v2

--- ### Boolean and Comparison Operators

eval (BoolOpExp op e1 e2) env =
    let
        v1 = eval e1 env
        v2 = eval e2 env
        Just f = H.lookup op boolOps -- again, ignore lookup fails
    in  liftBoolOp f v1 v2
        

eval (CompOpExp op e1 e2) env =
    let
        v1 = eval e1 env
        v2 = eval e2 env
        Just f = H.lookup op compOps -- again, ignore lookup fails
    in  liftCompOp f v1 v2

--- ### If Expressions

eval (IfExp e1 e2 e3) env =
    let
        v1 = eval e1 env
        v2 = eval e2 env
        v3 = eval e3 env
    in 
        case v1 of
            (BoolVal True) -> v2
            (BoolVal False) -> v3
            _ -> ExnVal "Condition is not a Bool" 

--- ### Functions and Function Application

eval (FunExp params body) env = CloVal params body env

-- AppExp has two VALID cases...
-- 1. The first case is when we throw in an explicit FunExp as the first expression.
--    As this is the first time that the function is being defined, then the fn. can
--    be considered to be somewhat like a lambda function that gets called immedietaley,
--    with the current env. being the state of the world that the function uses.
-- 2. The second case is when the first expression is a string - this means that the
--    function should be defined within the environment, and will have its own closure
--    of variables defined (within its encapsulated env.).
-- Otherwise, we want to return a Exception - Apply to non-closure

eval (AppExp e1 args) env =
    let
        v1 = eval e1 env
    in
        case v1 of
            -- Here, we want to map each paremeter to the arguments values,
            -- then from there 
            (CloVal par body clenv) -> 
                let
                    -- NOTE: I am assuming that since this function is being applied, that it
                    -- will not have access to global values
                    argVals = map (\e -> eval e env) args -- this is needed, else we get a map of [Strings Exp]
                    newVars = zip par argVals
                    fnenv = H.union (H.fromList newVars) clenv
                in eval body fnenv
            _                       -> ExnVal "Apply to non-closure" 


--- ### Let Expressions

eval (LetExp pairs body) env =
    let
        pairVals = map (\(x, e) -> (x, eval e env)) pairs
        lenv = H.union (H.fromList pairVals) env
    in eval body lenv

--- Statements
--- ----------

-- seqHelper is a helper function that allows us to update
-- and accumulate both the STRINGS that get output during a sequence, as 
-- well as the environments that accumulate during the running of the sequences.
-- This helps ensure that procedures and other variables remain in our environment when we run
-- through sequences.
seqHelper :: Result -> Stmt -> Result
seqHelper (strAcc, penvAcc, envAcc) stmt =
    let (str, newpenv, newenv) = exec stmt penvAcc envAcc
    in (strAcc ++ str, newpenv, newenv)

-- Statement Execution
-- -------------------

exec :: Stmt -> PEnv -> Env -> Result
exec (PrintStmt e) penv env = (val, penv, env)
    where val = show $ eval e env

--- ### Set Statements

exec (SetStmt var e) penv env =
    let
        v = eval e env
    in ("", penv, (H.insert var v env))

--- ### Sequencing

exec (SeqStmt stmts) penv env = case stmts of
    [] -> ("", penv, env)
    _  -> foldl seqHelper ("", penv, env) stmts
    
--- ### If Statements

exec (IfStmt e1 s1 s2) penv env =
    let
        v1 = eval e1 env
    in 
        case v1 of
            (BoolVal True) -> exec s1 penv env
            (BoolVal False) -> exec s2 penv env
            _ -> ("exn: Condition is not a Bool", penv, env)   

--- ### Procedure and Call Statements

-- We are going to store each procedure in two ways.
-- We first store the procedures BODY, labelled with a key the same as the procedures name.
-- We then store the procedures VARIABLES, labelled with a key as such: procedurename_VAR
exec p@(ProcedureStmt name args body) penv env = ("", newpenv, env)
    where newpenv = H.insert name (ProcedureStmt name args body) penv
    
exec (CallStmt name args) penv env =
    case H.lookup name penv of
        Nothing                    -> ("Procedure " ++ name ++ " undefined", penv, env)
        Just (ProcedureStmt n a b) ->
            let
                argVals = map (\e -> eval e env) args
                newVars = zip a argVals
                newenv = H.union (H.fromList newVars) env
            in exec b penv newenv


