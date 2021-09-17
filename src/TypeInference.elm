module TypeInference exposing (typeInferProgram)

import AbstractSyntax exposing (..)
import Utils exposing (lookup, lookupAndUpdate)
import Result exposing (andThen, mapError)


-- PROGRAM TYPE INFERENCE

typeInferProgram : LambdaProgram -> Result String Type
typeInferProgram { atomicTypes, typedefs, unrestrictedContext, linearContext, defs, mainTerm } =
  checkTypeEnvironment atomicTypes typedefs |> andThen (\typeContext ->
  checkSimpleContext typeContext [] unrestrictedContext Unrestricted |> andThen (\context1 ->
  checkSimpleContext typeContext context1 linearContext (Linear True) |> andThen (\context2 ->
  typeInferDefEnv typeContext typedefs context2 defs |> andThen (\context ->
  typeInferTerm typeContext typedefs context mainTerm |> andThen (\(aType, outCtx, slack) ->
  if slack
  then
    Ok aType
  else
    checkOutContext outCtx |> andThen (\_ ->
    Ok aType))))))

checkOutContext : Context -> Result String ()
checkOutContext outCtx =
  case outCtx of
    [] ->
      Ok ()

    (id, (_, Linear True)) :: _ ->
      Err ("The linear variable " ++ id ++ " is not used")
    
    _ :: xs ->
      checkOutContext xs



--- TYPE AND TYPE ENVIRONMENT WELL-FORMEDNESS

-- Checks if all type constants are defined before their use.
checkTypeEnvironment : TypeContext -> TypeEnvironment -> Result String TypeContext
checkTypeEnvironment typeContext env =
  case env of
    [] ->
      Ok typeContext
    
    (id, aType) :: xs ->
      checkType typeContext aType
        |> mapError (\e -> "In the type definition of " ++ id ++ ": " ++ e)
        |> andThen (\_ ->
      checkTypeEnvironment (id :: typeContext) xs)

-- Checks if all type constants occurring in a type are defined, given a context of defined type constants.
checkType : TypeContext -> Type -> Result String ()
checkType typeContext aType =
  case aType of
    Type_Constant id ->
      if List.member id typeContext
      then Ok ()
      else Err ("The type " ++ id ++ " was not declared")
    
    Type_LinearFn t1 t2 ->
      checkType typeContext t1 |> andThen (\_ ->
      checkType typeContext t2)
    
    Type_UnrestrictedFn t1 t2 ->
      checkType typeContext t1 |> andThen (\_ ->
      checkType typeContext t2)
    
    Type_SimultaneousProduct t1 t2 ->
      checkType typeContext t1 |> andThen (\_ ->
      checkType typeContext t2)
    
    Type_Unit ->
      Ok ()
    
    Type_AlternativeProduct t1 t2 ->
      checkType typeContext t1 |> andThen (\_ ->
      checkType typeContext t2)
    
    Type_Top ->
      Ok ()
    
    Type_Sum t1 t2 ->
      checkType typeContext t1 |> andThen (\_ ->
      checkType typeContext t2)
    
    Type_Zero ->
      Ok ()
    
    Type_OfCourse t1 ->
      checkType typeContext t1



-- CONTEXT WELL-FORMEDNESS

-- Checks if all types in a SimpleContext are well-formed. Returns an error (if any) or a Context
-- assigning to each variable its declared type and the Multiplicity given as parameter.
checkSimpleContext : TypeContext -> Context -> SimpleContext -> Multiplicity -> Result String Context
checkSimpleContext typeContext context simpleContext multiplicity =
  case simpleContext of
    [] ->
      Ok context
    
    (id, aType) :: xs ->
      checkType typeContext aType
        |> mapError (\e -> "In the declaration of " ++ id ++ ": " ++ e)
        |> andThen (\_ ->
      checkSimpleContext typeContext ((id, (aType, multiplicity)) :: context) xs multiplicity)



-- DEFINITION ENVIRONMENT TYPE INFERENCE

-- Checks if all definitions are well-typed. Returns a type error (if any) or the context assigning to
-- each defined variable its inferred type.
typeInferDefEnv : TypeContext -> TypeEnvironment -> Context -> DefinitionEnvironment -> Result String Context
typeInferDefEnv typeContext typeEnv context defEnv =
  case defEnv of
    [] ->
      Ok context
    
    (id, maybeType, term) :: xs ->
      typeInferTerm typeContext typeEnv (markAllAsUnavailable context) term
        |> mapError (\e -> "In the definition of " ++ id ++ ": " ++ e)
        |> andThen (\(inferredType, _, _) ->
      case maybeType of
        Just declaredType ->
          checkType typeContext declaredType
            |> mapError (\e -> "In the type annotation of " ++ id ++ ": " ++ e)
            |> andThen (\_ ->
          if typesAreEqual typeEnv inferredType declaredType
          then typeInferDefEnv typeContext typeEnv ((id, (declaredType, Unrestricted)) :: context) xs
          else
            Err ("Declared type of " ++ id ++ " is " ++ typeToString declaredType
              ++ ", but the inferred type is " ++ typeToString inferredType))
        
        Nothing ->
          typeInferDefEnv typeContext typeEnv ((id, (inferredType, Unrestricted)) :: context) xs)



-- TYPE ALIAS RESOLUTION

resolveTypeAlias : TypeEnvironment -> Type -> Type
resolveTypeAlias typeEnv aType =
  case aType of
    Type_Constant id ->
      case lookup id typeEnv of
        Just anotherType ->
          resolveTypeAlias typeEnv anotherType
        
        Nothing ->
          Type_Constant id
    
    x -> x

typesAreEqual : TypeEnvironment -> Type -> Type -> Bool
typesAreEqual typeEnv type1 type2 =
  case (resolveTypeAlias typeEnv type1, resolveTypeAlias typeEnv type2) of
    (Type_Constant id1, Type_Constant id2) ->
      id1 == id2

    (Type_LinearFn type1_1 type1_2, Type_LinearFn type2_1 type2_2) ->
      typesAreEqual typeEnv type1_1 type2_1 && typesAreEqual typeEnv type1_2 type2_2
    
    (Type_UnrestrictedFn type1_1 type1_2, Type_UnrestrictedFn type2_1 type2_2) ->
      typesAreEqual typeEnv type1_1 type2_1 && typesAreEqual typeEnv type1_2 type2_2
    
    (Type_SimultaneousProduct type1_1 type1_2, Type_SimultaneousProduct type2_1 type2_2) ->
      typesAreEqual typeEnv type1_1 type2_1 && typesAreEqual typeEnv type1_2 type2_2
    
    (Type_Unit, Type_Unit) ->
      True
    
    (Type_AlternativeProduct type1_1 type1_2, Type_AlternativeProduct type2_1 type2_2) ->
      typesAreEqual typeEnv type1_1 type2_1 && typesAreEqual typeEnv type1_2 type2_2
    
    (Type_Top, Type_Top) ->
      True
    
    (Type_Sum type1_1 type1_2, Type_Sum type2_1 type2_2) ->
      typesAreEqual typeEnv type1_1 type2_1 && typesAreEqual typeEnv type1_2 type2_2
    
    (Type_Zero, Type_Zero) ->
      True
    
    (Type_OfCourse type1_1, Type_OfCourse type2_1) ->
      typesAreEqual typeEnv type1_1 type2_1
    
    _ ->
      False



-- TERM TYPE INFERENCE

{- The term type inference is based on this handout by Frank Pfenning
https://www.cs.cmu.edu/~fp/courses/linear/handouts/linfp.pdf
Note that the handout is a draft, so it may contain errors.

There are two differences relative to the representation of contexts:
1. Instead of separating contexts as linear and unrestricted, this implementation uses a single context, 
where variables are marked with their multiplicity. If the same variable name is declared as linear and
unrestricted, this allows us to distinguish which declaration occurs in an inner scope, since the order 
of the single context list gives the order in which the variables were declared.

2. Instead of removing linear variables from the context when they are used, linear variables are marked 
with a boolean flag that indicates if they are available. The flag is set to False when the variable is 
used. This allows us to distinguish if a linear variable was not declared or if it was declared but was 
already used, resulting in more precise error messages. -}

type Multiplicity = Linear Bool | Unrestricted

type alias Context = List (Id, (Type, Multiplicity))

markAsUnavailable : Id -> Context -> Context
markAsUnavailable = lookupAndUpdate (\(aType, multiplicity) ->
  case multiplicity of
    Linear _     -> (aType, Linear False)
    Unrestricted -> (aType, Unrestricted))

markAllAsUnavailable : Context -> Context
markAllAsUnavailable = List.map (\entry ->
  case entry of
    (key, (aType, Linear _)) ->
      (key, (aType, Linear False))
    
    x -> x)

contextsAreCompatible : Context -> Bool -> Context -> Bool -> Bool
contextsAreCompatible context1 slack1 context2 slack2 =
  case (context1, context2) of
    ([], []) ->
      True
    
    ((_, (_, Unrestricted)) :: xs, (_, (_, Unrestricted)) :: ys) ->
      contextsAreCompatible xs slack1 ys slack2

    ((_, (_, Linear available1)) :: xs, (_, (_, Linear available2)) :: ys) ->
      let varsAreCompatible =
            (available1 && available2) || ((slack1 || not available1) && (slack2 || not available2))
      in
        varsAreCompatible && contextsAreCompatible xs slack1 ys slack2
    
    _ -> -- Should not happen if contexts have the same variables in the same order and multiplicity
      False

contextIntersection : Context -> Context -> Context
contextIntersection context1 context2 =
  case (context1, context2) of
    ([], []) ->
      []
    
    ((id, (aType, Unrestricted)) :: xs, (_, (_, Unrestricted)) :: ys) ->
      (id, (aType, Unrestricted)) :: (contextIntersection xs ys)

    ((id, (aType, Linear available1)) :: xs, (_, (_, Linear available2)) :: ys) ->
      (id, (aType, Linear (available1 && available2))) :: (contextIntersection xs ys)
    
    _ -> -- Should not happen if contexts have the same variables in the same order and multiplicity
      []

isAvailable : Id -> Context -> Bool
isAvailable id context =
  case lookup id context of
    Just (_, Linear available) -> available
    _ -> False

typeInferTerm : TypeContext -> TypeEnvironment -> Context -> Term -> Result String (Type, Context, Bool)
typeInferTerm typeContext typeEnv context term =
  case term of
    Term_Var id ->
      case lookup id context of
        Just (aType, Unrestricted) -> Ok (aType, context, False)
        Just (aType, Linear True)  -> Ok (aType, markAsUnavailable id context, False)
        Just (_    , Linear False) -> Err ("The linear variable " ++ id ++ " is used more than once")
        Nothing -> Err ("The variable " ++ id ++ " was not declared")
    
    Term_LinearLambda id varType body ->
      checkType typeContext varType |> andThen (\_ ->
      typeInferTerm typeContext typeEnv ((id, (varType, Linear True)) :: context) body
        |> andThen (\(bodyType, outCtx, slack) ->
      if slack || not (isAvailable id outCtx)
      then Ok (Type_LinearFn varType bodyType, List.drop 1 outCtx, slack)
      else Err ("The linear variable " ++ id ++ " is not used")))
    
    Term_UnrestrictedLambda id varType body ->
      checkType typeContext varType |> andThen (\_ ->
      typeInferTerm typeContext typeEnv ((id, (varType, Unrestricted)) :: context) body
        |> andThen (\(bodyType, outCtx, slack) ->
      Ok (Type_UnrestrictedFn varType bodyType, List.drop 1 outCtx, slack)))
    
    Term_Application e1 e2 ->
      typeInferTerm typeContext typeEnv context e1 |> andThen (\(type1, outCtx1, slack1) ->
      case resolveTypeAlias typeEnv type1 of
        Type_LinearFn a b ->
          typeInferTerm typeContext typeEnv outCtx1 e2 |> andThen (\(type2, outCtx2, slack2) ->
          if typesAreEqual typeEnv type2 a
          then Ok (b, outCtx2, slack1 || slack2)
          else
            Err ("An expression of type " ++ typeToString a ++ " was expected, but it has type "
              ++ typeToString type2))
        
        Type_UnrestrictedFn a b ->
          typeInferTerm typeContext typeEnv (markAllAsUnavailable context) e2 |> andThen (\(type2, _, _) ->
          if typesAreEqual typeEnv type2 a
          then Ok (b, outCtx1, slack1)
          else
            Err ("An expression of type " ++ typeToString a ++ " was expected, but it has type "
              ++ typeToString type2))

        _ -> Err ("An expression of a function type was expected, but it has type " ++ typeToString type1))
    
    Term_SimultaneousPair e1 e2 ->
      typeInferTerm typeContext typeEnv context e1 |> andThen (\(type1, outCtx1, slack1) ->
      typeInferTerm typeContext typeEnv outCtx1 e2 |> andThen (\(type2, outCtx2, slack2) ->
      Ok (Type_SimultaneousProduct type1 type2, outCtx2, slack1 || slack2)))
    
    Term_SimultaneousLet id1 id2 e1 e2 ->
      typeInferTerm typeContext typeEnv context e1 |> andThen (\(type1, outCtx1, slack1) ->
      case resolveTypeAlias typeEnv type1 of
        Type_SimultaneousProduct a b ->
          typeInferTerm typeContext typeEnv ((id2, (b, Linear True)) :: (id1, (a, Linear True)) :: outCtx1) e2
            |> andThen (\(type2, outCtx2, slack2) ->
          let
            availableId1 = isAvailable id1 outCtx2
            availableId2 = isAvailable id2 outCtx2
          in
            if slack2 || (not availableId1 && not availableId2)
            then Ok (type2, List.drop 2 outCtx2, slack1 || slack2)
            else
              if availableId2
              then Err ("The linear variable " ++ id2 ++ " is not used")
              else Err ("The linear variable " ++ id1 ++ " is not used"))
        
        _ ->
          Err ("An expression of simultaneous product type was expected, but it has type "
            ++ typeToString type1))
    
    Term_Unit ->
      Ok (Type_Unit, context, False)
    
    Term_UnitLet e1 e2 ->
      typeInferTerm typeContext typeEnv context e1 |> andThen (\(type1, outCtx1, slack1) ->
      case resolveTypeAlias typeEnv type1 of
        Type_Unit ->
          typeInferTerm typeContext typeEnv outCtx1 e2 |> andThen (\(type2, outCtx2, slack2) ->
          Ok (type2, outCtx2, slack1 || slack2))
        
        _ ->
          Err ("An expression of unit type was expected, but it has type " ++ typeToString type1))
    
    Term_AlternativePair e1 e2 ->
      typeInferTerm typeContext typeEnv context e1 |> andThen (\(type1, outCtx1, slack1) ->
      typeInferTerm typeContext typeEnv context e2 |> andThen (\(type2, outCtx2, slack2) ->
      if contextsAreCompatible outCtx1 slack1 outCtx2 slack2
      then Ok (Type_AlternativeProduct type1 type2, contextIntersection outCtx1 outCtx2, slack1 && slack2)
      else
        Err "Some linear variable is used in one component of the alternative pair, but not in the other"))
    
    Term_Fst e ->
      typeInferTerm typeContext typeEnv context e |> andThen (\(aType, outCtx, slack) ->
      case resolveTypeAlias typeEnv aType of
        Type_AlternativeProduct a _ ->
          Ok (a, outCtx, slack)
        
        _ ->
          Err ("An expression of alternative product type was expected, but it has type "
            ++ typeToString aType))
    
    Term_Snd e ->
      typeInferTerm typeContext typeEnv context e |> andThen (\(aType, outCtx, slack) ->
      case resolveTypeAlias typeEnv aType of
        Type_AlternativeProduct _ b ->
          Ok (b, outCtx, slack)
        
        _ ->
          Err ("An expression of alternative product type was expected, but it has type "
            ++ typeToString aType))
    
    Term_Top ->
      Ok (Type_Top, context, True)
    
    Term_Inl type2 e ->
      checkType typeContext type2 |> andThen (\_ ->
      typeInferTerm typeContext typeEnv context e |> andThen (\(type1, outCtx, slack) ->
      Ok (Type_Sum type1 type2, outCtx, slack)))
    
    Term_Inr type1 e ->
      checkType typeContext type1 |> andThen (\_ ->
      typeInferTerm typeContext typeEnv context e |> andThen (\(type2, outCtx, slack) ->
      Ok (Type_Sum type1 type2, outCtx, slack)))

    Term_Case e id1 e1 id2 e2 ->
      typeInferTerm typeContext typeEnv context e |> andThen (\(type1, outCtx1, slack1) ->
      case resolveTypeAlias typeEnv type1 of
        Type_Sum a b ->
          typeInferTerm typeContext typeEnv ((id1, (a, Linear True)) :: outCtx1) e1
            |> andThen (\(type2, outCtx2, slack2) ->
          if (not slack2) && (isAvailable id1 outCtx2)
          then Err ("The linear variable " ++ id1 ++ " is not used")
          else
            typeInferTerm typeContext typeEnv ((id2, (b, Linear True)) :: outCtx1) e2
              |> andThen (\(type3, outCtx3, slack3) ->
            if (not slack3) && (isAvailable id2 outCtx3)
            then Err ("The linear variable " ++ id2 ++ " is not used")
            else
              if not (typesAreEqual typeEnv type2 type3)
              then
                Err ("The types of both case branches should be equal, but the first branch has type "
                  ++ typeToString type2 ++ " while the second branch has type " ++ typeToString type3)
              else
                let
                  outCtx2_tail = List.drop 1 outCtx2
                  outCtx3_tail = List.drop 1 outCtx3
                in
                  if contextsAreCompatible outCtx2_tail slack2 outCtx3_tail slack3
                  then Ok (type2, contextIntersection outCtx2_tail outCtx3_tail, slack1 || (slack2 && slack3))
                  else
                    Err ("Some linear variable is used in one branch of the case expression, "
                      ++ "but not in the other")))
        
        _ ->
          Err ("An expression of sum type was expected, but it has type " ++ typeToString type1))
    
    Term_Absurd aType e ->
      checkType typeContext aType |> andThen (\_ ->
      typeInferTerm typeContext typeEnv context e |> andThen (\(type1, outCtx, _) ->
      case resolveTypeAlias typeEnv type1 of
        Type_Zero ->
          Ok (aType, outCtx, True)
        
        _ ->
          Err ("An expression of type 0 was expected, but it has type " ++ typeToString type1)))
    
    Term_Bang e ->
      typeInferTerm typeContext typeEnv (markAllAsUnavailable context) e |> andThen (\(aType, _, _) ->
      Ok (Type_OfCourse aType, context, False))
    
    Term_BangLet id e1 e2 ->
      typeInferTerm typeContext typeEnv context e1 |> andThen (\(type1, outCtx1, slack1) ->
      case resolveTypeAlias typeEnv type1 of
        Type_OfCourse a ->
          typeInferTerm typeContext typeEnv ((id, (a, Unrestricted)) :: outCtx1) e2
            |> andThen (\(type2, outCtx2, slack2) ->
          Ok (type2, List.drop 1 outCtx2, slack1 || slack2))
        
        _ ->
          Err ("An expression of \"of course\" type was expected, but it has type " ++ typeToString type1))