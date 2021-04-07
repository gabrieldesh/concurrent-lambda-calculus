module Evaluation exposing (evalProgram, Value(..), valueToString)

import AbstractSyntax exposing (..)
import Utils exposing (lookup)
import Maybe exposing (andThen)


-- VALUES

type alias Environment = List (Id, Value)

type Value
  = Value_LinearFn Id Environment Term
  | Value_UnrestrictedFn Id Environment Term
  | Value_SimultaneousPair Value Value
  | Value_Unit
  | Value_AlternativePair Environment Term Term
  | Value_Top
  | Value_Inl Value
  | Value_Inr Value
  | Value_Bang Environment Term

valueToString : Value -> String
valueToString value =
  case value of
    Value_LinearFn _ _ _ ->
      "<linear function code>"
    
    Value_UnrestrictedFn _ _ _ ->
      "<unrestricted function code>"
    
    Value_SimultaneousPair v1 v2 ->
      "{" ++ valueToString v1 ++ ", " ++ valueToString v2 ++ "}"
    
    Value_Unit ->
      "*"

    Value_AlternativePair _ _ _ ->
      "<<code>, <code>>"
    
    Value_Top ->
      "<>"
    
    Value_Inl v ->
      "inl " ++ valueToString v
    
    Value_Inr v ->
      "inr " ++ valueToString v
    
    Value_Bang _ _ ->
      "!<code>"



-- PROGRAM EVALUATION

evalProgram : LambdaProgram -> Maybe Value
evalProgram (_, defs, mainTerm) =
  evalDefEnv [] defs |> andThen (\env ->
  evalTerm env mainTerm)

evalDefEnv : Environment -> DefinitionEnvironment -> Maybe Environment
evalDefEnv env defEnv =
  case defEnv of
    [] ->
      Just env
    
    (id, _, term) :: xs ->
      evalTerm env term |> andThen (\value ->
      evalDefEnv ((id, value) :: env) xs)



-- TERM EVALUATION

evalTerm : Environment -> Term -> Maybe Value
evalTerm env term =
  case term of
    Term_Var id ->
      lookup id env
    
    Term_LinearLambda id _ e ->
      Just (Value_LinearFn id env e)
    
    Term_UnrestrictedLambda id _ e ->
      Just (Value_UnrestrictedFn id env e)
    
    Term_Application e1 e2 ->
      evalTerm env e1 |> andThen (\v1 ->
      case v1 of
        Value_LinearFn id fnEnv fnBody ->
          evalTerm env e2 |> andThen (\v2 ->
          evalTerm ((id, v2) :: fnEnv) fnBody)
        
        Value_UnrestrictedFn id fnEnv fnBody ->
          evalTerm env e2 |> andThen (\v2 ->
          evalTerm ((id, v2) :: fnEnv) fnBody)
        
        _ ->
          Nothing)
    
    Term_SimultaneousPair e1 e2 ->
      evalTerm env e1 |> andThen (\v1 ->
      evalTerm env e2 |> andThen (\v2 ->
      Just (Value_SimultaneousPair v1 v2)))
    
    Term_SimultaneousLet id1 id2 e1 e2 ->
      evalTerm env e1 |> andThen (\v ->
      case v of
        Value_SimultaneousPair v1 v2 ->
          evalTerm ((id2, v2) :: (id1, v1) :: env) e2
        
        _ ->
          Nothing)
    
    Term_Unit ->
      Just Value_Unit
    
    Term_UnitLet e1 e2 ->
      evalTerm env e1 |> andThen (\v ->
      case v of
        Value_Unit ->
          evalTerm env e2
        
        _ ->
          Nothing)
    
    Term_AlternativePair e1 e2 ->
      Just (Value_AlternativePair env e1 e2)
    
    Term_Fst e ->
      evalTerm env e |> andThen (\v ->
      case v of
        Value_AlternativePair env2 e1 _ ->
          evalTerm env2 e1
        
        _ ->
          Nothing)
    
    Term_Snd e ->
      evalTerm env e |> andThen (\v ->
      case v of
        Value_AlternativePair env2 _ e2 ->
          evalTerm env2 e2
        
        _ ->
          Nothing)
    
    Term_Top ->
      Just Value_Top
    
    Term_Inl _ e ->
      evalTerm env e |> andThen (\v ->
      Just (Value_Inl v))
    
    Term_Inr _ e ->
      evalTerm env e |> andThen (\v ->
      Just (Value_Inr v))
    
    Term_Case e id1 e1 id2 e2 ->
      evalTerm env e |> andThen (\v ->
      case v of
        Value_Inl v1 ->
          evalTerm ((id1, v1) :: env) e1
        
        Value_Inr v2 ->
          evalTerm ((id2, v2) :: env) e2
        
        _ ->
          Nothing)
    
    Term_Abort _ _ ->
      Nothing
    
    Term_Bang e ->
      Just (Value_Bang env e)
    
    Term_BangLet id e1 e2 ->
      evalTerm env e1 |> andThen (\v ->
      case v of
        Value_Bang bangEnv bangBody ->
          evalTerm bangEnv bangBody |> andThen (\v1 ->
          evalTerm ((id, v1) :: env) e2)
        
        _ ->
          Nothing)