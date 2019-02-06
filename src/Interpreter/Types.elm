module Interpreter.Types exposing (Env, Expr(..), Name, Op(..), Pattern(..))

import Dict exposing (Dict)
import Set exposing (Set)

type alias Env =
    Dict Name Expr


type Op
    = OpAdd
    | OpMul
    | OpSub


type alias Name =
    String


type Expr
    = EInt Int
    | EBinOp Expr Op Expr
    | EVariable Name
    | ELet (Dict Name Expr) Expr
    | EFunction Pattern Expr
    | ELambda Env Pattern Expr
    | EApply Expr Expr -- fn arg
    | ECtor Name (List Expr)
    | ECase Expr (List ( Pattern, Expr ))
    | ERecord (Dict Name Expr)
    | ENegate Expr


type Pattern
    = PAnything
    | PVar Name
    | PInt Int
    | PECtor Name (List Pattern)
    | PERecord (Set Name)
