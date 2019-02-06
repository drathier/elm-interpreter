module Interpreter.Types exposing (Env, Expr(..), Name, Op(..), Pattern(..))

import Dict exposing (Dict)
import Set exposing (Set)

type alias Env =
    Dict Name Expr


type Op
    = Add
    | Mul
    | Sub


type alias Name =
    String


type Expr
    = VInt Int
    | BinOp Expr Op Expr
    | Variable Name
    | Let (Dict Name Expr) Expr
    | Function Pattern Expr
    | Lambda Env Pattern Expr
    | Apply Expr Expr -- fn arg
    | Ctor Name (List Expr)
    | Case Expr (List ( Pattern, Expr ))
    | Record (Dict Name Expr)
    | Negate Expr


type Pattern
    = PAnything
    | PVar Name
    | PInt Int
    | PCtor Name (List Pattern)
    | PRecord (Set Name)
