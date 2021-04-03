module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (rows, cols)
import Html.Events exposing (onInput)
import Combine exposing (..)
import Result
import Result
import Result
import Result
import Result
import Result
import Result
import Result
import Maybe exposing (withDefault)
import Html.Attributes exposing (autofocus)
import Tuple exposing (first)


-- MAIN


main : Program () Model Msg
main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL

type alias Model = { code : String }

init : () -> (Model, Cmd Msg)
init _ = ({ code = "" }, Cmd.none)


-- UPDATE

type Msg = Change String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    Change newCode -> ( { model | code = newCode }, Cmd.none )



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW

view : Model -> Browser.Document Msg
view model = 
  { title = "Linear lambda calculus simulator"
  , body =
      [ h1 [] [text "Linear lambda calculus simulator"]
      , textarea [rows 25, cols 80, autofocus True, onInput Change] []
      , viewParsed model.code
      ]
  }

viewParsed : String -> Html Msg
viewParsed code =
  case parseCheckEval code of
    SyntaxError _ ->
      pre [] [ text "Syntax error" ]

    TypeError _ error ->
      pre [] [ text ("Error: " ++ error) ]
    
    WellTyped _ aType value ->
      pre [] [ text ("Value: \n" ++ (valueToString value)), br [] [], br [] [], text ("Type: \n" ++ typeToString aType) ]
      


-- Parsing, type inference and evaluation chain

type ParsingResult
  = SyntaxError String
  | TypeError LambdaProgram String
  | WellTyped LambdaProgram Type Value

parseCheckEval : String -> ParsingResult
parseCheckEval code =
  case parseProgram code of
    Ok program ->
      case typeInferProgram program of
        Ok aType ->
          let
            (_, defs, mainTerm) = program
            -- buildEnvFromDefs and eval should succeed since program is well-typed
            env = withDefault [] (buildEnvFromDefs [] defs)
            value = withDefault Value_Unit (eval env mainTerm)
          in WellTyped program aType value
        Err error ->
          TypeError program error
    Err errors ->
      SyntaxError (String.join " or " errors)

typeInferProgram : LambdaProgram -> Result String Type
typeInferProgram (typeEnv, defEnv, mainTerm) =
  checkTypeEnvironment [] typeEnv |> Result.andThen (\_ ->
  typeInferDefEnv typeEnv [] defEnv |> Result.andThen (\context ->
  typeInfer typeEnv context mainTerm |> Result.andThen (\(aType, _, _) ->
  Ok aType)))

typeInferDefEnv : TypeEnvironment -> Context -> DefinitionEnvironment -> Result String Context
typeInferDefEnv typeEnv context defEnv =
  case defEnv of
    [] ->
      Ok context
    
    (id, maybeType, term) :: xs ->
      typeInfer typeEnv context term
        |> Result.mapError (\e -> "In the definition of " ++ id ++ ": " ++ e)
        |> Result.andThen (\(inferredType, _, _) ->
      case maybeType of
        Just declaredType ->
          if typesAreEqual typeEnv inferredType declaredType
          then typeInferDefEnv typeEnv ((id, (declaredType, Unrestricted)) :: context) xs
          else Err ("Declared type of " ++ id ++ " is " ++ typeToString declaredType ++ ", but the inferred type is " ++ typeToString inferredType)
        
        Nothing ->
          typeInferDefEnv typeEnv ((id, (inferredType, Unrestricted)) :: context) xs)

buildEnvFromDefs : Environment -> DefinitionEnvironment -> Maybe Environment
buildEnvFromDefs env defEnv =
  case defEnv of
    [] ->
      Just env
    
    (id, _, term) :: xs ->
      eval env term |> Maybe.andThen (\value ->
      buildEnvFromDefs ((id, value) :: env) xs)


-- TYPES

type alias Id = String

type Type
  = Type_Constant Id
  | Type_LinearFn Type Type
  | Type_UnrestrictedFn Type Type
  | Type_SimultaneousProduct Type Type
  | Type_Unit
  | Type_AlternativeProduct Type Type
  | Type_Top
  | Type_Sum Type Type
  | Type_Zero
  | Type_OfCourse Type


typeToString : Type -> String
typeToString aType =
  case aType of
    Type_Constant id ->
      id
    
    Type_LinearFn type1 type2 ->
      "(" ++ typeToString type1 ++ ") -o (" ++ typeToString type2 ++ ")"
    
    Type_UnrestrictedFn type1 type2 ->
      "(" ++ typeToString type1 ++ ") -> (" ++ typeToString type2 ++ ")"
    
    Type_SimultaneousProduct type1 type2 ->
      "(" ++ typeToString type1 ++ ") * (" ++ typeToString type2 ++ ")"
    
    Type_Unit ->
      "1"
    
    Type_AlternativeProduct type1 type2 ->
      "(" ++ typeToString type1 ++ ") & (" ++ typeToString type2 ++ ")"
    
    Type_Top ->
      "T"
    
    Type_Sum type1 type2 ->
      "(" ++ typeToString type1 ++ ") + (" ++ typeToString type2 ++ ")"
    
    Type_Zero ->
      "0"
    
    Type_OfCourse type1 ->
      "!" ++ typeToString type1



-- TERMS

type Term
  = Term_Var Id
  | Term_LinearLambda Id Type Term
  | Term_UnrestrictedLambda Id Type Term
  | Term_Application Term Term
  | Term_SimultaneousPair Term Term
  | Term_SimultaneousLet Id Id Term Term
  | Term_Unit
  | Term_UnitLet Term Term
  | Term_AlternativePair Term Term
  | Term_Fst Term
  | Term_Snd Term
  | Term_Top
  | Term_Inl Type Term
  | Term_Inr Type Term
  | Term_Case Term Id Term Id Term
  | Term_Abort Type Term
  | Term_Bang Term
  | Term_BangLet Id Term Term
    

termToString : Term -> String
termToString term = termToIndentedString 0 term

termToIndentedString : Int -> Term -> String
termToIndentedString indentLevel term = 
  let
    indentation = String.repeat indentLevel "  "
  in
    case term of
      Term_Var id ->
        indentation ++ "Var " ++ id
      
      Term_LinearLambda id type1 e ->
        String.concat
          [ indentation ++ "LinearLambda " ++ id ++ " : " ++ typeToString type1 ++ "\n"
          , termToIndentedString (indentLevel + 1) e
          ]
      
      Term_UnrestrictedLambda id type1 e ->
        String.concat
          [ indentation ++ "UnrestrictedLambda " ++ id ++ " : " ++ typeToString type1 ++ "\n"
          , termToIndentedString (indentLevel + 1) e
          ]
      
      Term_Application e1 e2 ->
        String.concat
          [ indentation ++ "App\n"
          , termToIndentedString (indentLevel + 1) e1 ++ "\n"
          , termToIndentedString (indentLevel + 1) e2
          ]
      
      Term_SimultaneousPair e1 e2 ->
        String.concat
          [ indentation ++ "SimultaneousPair\n"
          , termToIndentedString (indentLevel + 1) e1 ++ "\n"
          , termToIndentedString (indentLevel + 1) e2
          ]
      
      Term_SimultaneousLet id1 id2 e1 e2 ->
        String.concat
          [ indentation ++ "SimultaneousLet " ++ id1 ++ " " ++ id2 ++ "\n"
          , termToIndentedString (indentLevel + 1) e1 ++ "\n"
          , termToIndentedString (indentLevel + 1) e2
          ]
      
      Term_Unit ->
        indentation ++ "Unit"
      
      Term_UnitLet e1 e2 ->
        String.concat
          [ indentation ++ "UnitLet\n"
          , termToIndentedString (indentLevel + 1) e1 ++ "\n"
          , termToIndentedString (indentLevel + 1) e2
          ]

      Term_AlternativePair e1 e2 ->
        String.concat
          [ indentation ++ "AlternativePair\n"
          , termToIndentedString (indentLevel + 1) e1 ++ "\n"
          , termToIndentedString (indentLevel + 1) e2
          ]
      
      Term_Fst e ->
        String.concat
          [ indentation ++ "Fst\n"
          , termToIndentedString (indentLevel + 1) e
          ]
      
      Term_Snd e ->
        String.concat
          [ indentation ++ "Snd\n"
          , termToIndentedString (indentLevel + 1) e
          ]
      
      Term_Top ->
        indentation ++ "Top"
      
      Term_Inl type1 e ->
        String.concat
          [ indentation ++ "Inl [ " ++ typeToString type1 ++ " ]\n"
          , termToIndentedString (indentLevel + 1) e
          ]
      
      Term_Inr type1 e ->
        String.concat
          [ indentation ++ "Inr [ " ++ typeToString type1 ++ " ]\n"
          , termToIndentedString (indentLevel + 1) e
          ]
      
      Term_Case e id1 e1 id2 e2 ->
        String.concat
          [ indentation ++ "case " ++ id1 ++ " " ++ id2 ++ "\n"
          , termToIndentedString (indentLevel + 1) e ++ "\n"
          , termToIndentedString (indentLevel + 1) e1 ++ "\n"
          , termToIndentedString (indentLevel + 1) e2
          ]
      
      Term_Abort type1 e ->
        String.concat
          [ indentation ++ "Abort [ " ++ typeToString type1 ++ " ]\n"
          , termToIndentedString (indentLevel + 1) e
          ]
      
      Term_Bang e ->
        String.concat
          [ indentation ++ "Bang\n"
          , termToIndentedString (indentLevel + 1) e
          ]
      
      Term_BangLet id e1 e2 ->
        String.concat
          [ indentation ++ "BangLet " ++ id ++ "\n"
          , termToIndentedString (indentLevel + 1) e1 ++ "\n"
          , termToIndentedString (indentLevel + 1) e2
          ]



-- PROGRAM

type alias TypeEnvironment = List (Id, Type)

type alias DefinitionEnvironment = List (Id, Maybe Type, Term)

type alias LambdaProgram = (TypeEnvironment, DefinitionEnvironment, Term)



-- PARSING

parseProgram : String -> Result (List String) LambdaProgram
parseProgram input =
  case parse programParser input of
    Ok (_, _, program) ->
      Ok program
    Err (_, _, errors) ->
      Err errors

token : Parser s a -> Parser s a
token parser = 
  whitespace |> keep parser

programParser : Parser () LambdaProgram
programParser =
  optional [] typedefClause |> andThen (\typeEnv ->
  optional [] defClause |> andThen (\defEnv ->
  expression |> andThen (\mainTerm ->
  whitespace |> andThen (\_ ->
  end |> andThen (\_ ->
  succeed (typeEnv, defEnv, mainTerm))))))

typedefClause : Parser () TypeEnvironment
typedefClause =
  token (string "typedef") |> andThen (\_ ->
  typedefList |> andThen (\typeEnv ->
  token (string "end") |> andThen (\_ ->
  succeed typeEnv)))

typedefList : Parser () TypeEnvironment
typedefList =
  sepEndBy (token (string ";")) typeDefinition

typeDefinition : Parser () (Id, Type)
typeDefinition =
  identifier |> andThen (\id ->
  token (string "=") |> andThen (\_ ->
  typeExpr |> andThen (\aType ->
  succeed (id, aType))))

defClause : Parser () DefinitionEnvironment
defClause =
  token (string "def") |> andThen (\_ ->
  defList |> andThen (\defEnv ->
  token (string "end") |> andThen (\_ ->
  succeed defEnv)))

defList : Parser () DefinitionEnvironment
defList =
  sepEndBy (token (string ";")) definition

definition : Parser () (Id, Maybe Type, Term)
definition =
  identifier |> andThen (\id ->
  maybe typeAnnotation |> andThen (\maybeType ->
  token (string "=") |> andThen (\_ ->
  expression |> andThen (\term ->
  succeed (id, maybeType, term)))))

typeAnnotation : Parser () Type
typeAnnotation =
  token (string ":") |> andThen (\_ ->
  typeExpr)

expression : Parser () Term
expression =
  chainl (succeed Term_Application) atom

atom : Parser () Term
atom = 
  choice
    [ var
    , linearLambda
    , unrestrictedLambda
    , simultaneousPair
    , simultaneousLet
    , unit
    , unitLet
    , alternativePair
    , fst
    , snd
    , top
    , inl
    , inr
    , caseExpr
    , abort
    , bang
    , bangLet
    , exprBetweenParens
    ]

var : Parser () Term
var = Combine.map Term_Var identifier

linearLambda : Parser () Term
linearLambda = 
  token (string "\\^") |> andThen (\_ ->
  identifier |> andThen (\id ->
  token (string ":") |> andThen (\_ ->
  typeExpr |> andThen (\aType ->
  token (string ".") |> andThen (\_ ->
  expression |> andThen (\exp ->
  succeed (Term_LinearLambda id aType exp)
  ))))))

unrestrictedLambda : Parser () Term
unrestrictedLambda = 
  token (string "\\") |> andThen (\_ ->
  identifier |> andThen (\id ->
  token (string ":") |> andThen (\_ ->
  typeExpr |> andThen (\aType ->
  token (string ".") |> andThen (\_ ->
  expression |> andThen (\exp ->
  succeed (Term_UnrestrictedLambda id aType exp)
  ))))))

simultaneousPair : Parser () Term
simultaneousPair =
  token (string "{") |> andThen (\_ ->
  expression |> andThen (\e1 ->
  token (string ",") |> andThen (\_ ->
  expression |> andThen (\e2 ->
  token (string "}") |> andThen (\_ ->
  succeed (Term_SimultaneousPair e1 e2))))))

simultaneousLet : Parser () Term
simultaneousLet =
  token (string "let") |> andThen (\_ ->
  token (string "{") |> andThen (\_ ->
  identifier |> andThen (\id1 ->
  token (string ",") |> andThen (\_ ->
  identifier |> andThen (\id2 ->
  token (string "}") |> andThen (\_ ->
  token (string "=") |> andThen (\_ ->
  expression |> andThen (\e1 ->
  token (string "in") |> andThen (\_ ->
  expression |> andThen (\e2 ->
  succeed (Term_SimultaneousLet id1 id2 e1 e2)))))))))))

unit : Parser () Term
unit = token (string "*") |> onsuccess Term_Unit

unitLet : Parser () Term
unitLet =
  token (string "let") |> andThen (\_ ->
  token (string "*") |> andThen (\_ ->
  token (string "=") |> andThen (\_ ->
  expression |> andThen (\e1 ->
  token (string "in") |> andThen (\_ ->
  expression |> andThen (\e2 ->
  succeed (Term_UnitLet e1 e2)))))))

alternativePair : Parser () Term
alternativePair =
  token (string "<") |> andThen (\_ ->
  expression |> andThen (\e1 ->
  token (string ",") |> andThen (\_ ->
  expression |> andThen (\e2 ->
  token (string ">") |> andThen (\_ ->
  succeed (Term_AlternativePair e1 e2))))))

fst : Parser () Term
fst =
  token (string "fst") |> andThen (\_ ->
  atom |> andThen (\e ->
  succeed (Term_Fst e)))

snd : Parser () Term
snd =
  token (string "snd") |> andThen (\_ ->
  atom |> andThen (\e ->
  succeed (Term_Snd e)))

top : Parser () Term
top = token (string "<>") |> onsuccess Term_Top

inl : Parser () Term
inl =
  token (string "inl") |> andThen (\_ ->
  token (string "[") |> andThen (\_ ->
  typeExpr |> andThen (\aType ->
  token (string "]") |> andThen (\_ ->
  atom |> andThen (\e ->
  succeed (Term_Inl aType e))))))

inr : Parser () Term
inr =
  token (string "inr") |> andThen (\_ ->
  token (string "[") |> andThen (\_ ->
  typeExpr |> andThen (\aType ->
  token (string "]") |> andThen (\_ ->
  atom |> andThen (\e ->
  succeed (Term_Inr aType e))))))

caseExpr : Parser () Term
caseExpr =
  token (string "case") |> andThen (\_ ->
  expression |> andThen (\e ->
  token (string "of") |> andThen (\_ ->
  token (string "inl") |> andThen (\_ ->
  identifier |> andThen (\id1 ->
  token (string "=>") |> andThen (\_ ->
  expression |> andThen (\e1 ->
  token (string "|") |> andThen (\_ ->
  token (string "inr") |> andThen (\_ ->
  identifier |> andThen (\id2 ->
  token (string "=>") |> andThen (\_ ->
  expression |> andThen (\e2 ->
  succeed (Term_Case e id1 e1 id2 e2)))))))))))))

abort : Parser () Term
abort =
  token (string "abort") |> andThen (\_ ->
  token (string "[") |> andThen (\_ ->
  typeExpr |> andThen (\aType ->
  token (string "]") |> andThen (\_ ->
  atom |> andThen (\e ->
  succeed (Term_Abort aType e))))))

bang : Parser () Term
bang =
  token (string "!") |> andThen (\_ ->
  atom |> andThen (\e ->
  succeed (Term_Bang e)))

bangLet : Parser () Term
bangLet =
  token (string "let") |> andThen (\_ ->
  token (string "!") |> andThen (\_ ->
  identifier |> andThen (\id ->
  token (string "=") |> andThen (\_ ->
  expression |> andThen (\e1 ->
  token (string "in") |> andThen (\_ ->
  expression |> andThen (\e2 ->
  succeed (Term_BangLet id e1 e2))))))))

exprBetweenParens : Parser () Term
exprBetweenParens = between (token (string "(")) (token (string ")")) (lazy (\_ -> expression))

reservedWords : List String
reservedWords =
  ["let", "in", "fst", "snd", "inl", "inr", "case", "of", "abort", "typedef", "def", "end", "T"]

identifier : Parser () String
identifier = 
  token (regex "[A-Za-z_][0-9A-Za-z_-]*") |> andThen (\id ->
  if List.member id reservedWords
  then fail (id ++ " is a reserved word")
  else succeed id)

typeExpr : Parser () Type
typeExpr =
  chainr fnConnective additiveChain

fnConnective : Parser () (Type -> Type -> Type)
fnConnective =
  choice
    [ token (string "-o") |> onsuccess Type_LinearFn
    , token (string "->") |> onsuccess Type_UnrestrictedFn
    ]

additiveChain : Parser () Type
additiveChain =
  chainl additiveConnective multiplicativeChain

additiveConnective : Parser () (Type -> Type -> Type)
additiveConnective =
  choice
    [ token (string "&") |> onsuccess Type_AlternativeProduct
    , token (string "+") |> onsuccess Type_Sum
    ]

multiplicativeChain : Parser () Type
multiplicativeChain =
  chainl multiplicativeConnective typeAtom

multiplicativeConnective : Parser () (Type -> Type -> Type)
multiplicativeConnective = token (string "*") |> onsuccess (Type_SimultaneousProduct)

typeAtom : Parser () Type
typeAtom =
  choice
    [ typeConstant
    , token (string "1") |> onsuccess Type_Unit
    , token (string "T") |> onsuccess Type_Top
    , token (string "0") |> onsuccess Type_Zero
    , typeOfCourse
    , typeBetweenParens
    ]

typeConstant : Parser () Type
typeConstant = Combine.map Type_Constant identifier

typeOfCourse : Parser () Type
typeOfCourse =
  token (string "!") |> andThen (\_ ->
  typeAtom |> andThen (\aType ->
  succeed (Type_OfCourse aType)))

typeBetweenParens : Parser () Type
typeBetweenParens = between (token (string "(")) (token (string ")")) (lazy (\_ -> typeExpr))



--- TYPE AND TYPE ENVIRONMENT WELL-FORMEDNESS

type alias TypeContext = List Id

checkTypeEnvironment : TypeContext -> TypeEnvironment -> Result String ()
checkTypeEnvironment context env =
  case env of
    [] ->
      Ok ()
    
    (id, aType) :: xs ->
      checkType context aType
        |> Result.mapError (\e -> "In the type definition of " ++ id ++ ": " ++ e)
        |> Result.andThen (\_ ->
      checkTypeEnvironment (id :: context) xs)

checkType : TypeContext -> Type -> Result String ()
checkType context aType =
  case aType of
    Type_Constant id ->
      if List.member id context
      then Ok ()
      else Err ("The type " ++ id ++ " was not defined")
    
    Type_LinearFn t1 t2 ->
      checkType context t1 |> Result.andThen (\_ ->
      checkType context t2)
    
    Type_UnrestrictedFn t1 t2 ->
      checkType context t1 |> Result.andThen (\_ ->
      checkType context t2)
    
    Type_SimultaneousProduct t1 t2 ->
      checkType context t1 |> Result.andThen (\_ ->
      checkType context t2)
    
    Type_Unit ->
      Ok ()
    
    Type_AlternativeProduct t1 t2 ->
      checkType context t1 |> Result.andThen (\_ ->
      checkType context t2)
    
    Type_Top ->
      Ok ()
    
    Type_Sum t1 t2 ->
      checkType context t1 |> Result.andThen (\_ ->
      checkType context t2)
    
    Type_Zero ->
      Ok ()
    
    Type_OfCourse t1 ->
      checkType context t1

envToContext : TypeEnvironment -> TypeContext
envToContext typeEnv = List.map first typeEnv



-- TYPE ALIAS RESOLUTION

resolveTypeAlias : TypeEnvironment -> Type -> Type
resolveTypeAlias typeEnv aType =
  case aType of
    Type_Constant id ->
      case lookup id typeEnv of
        Just anotherType ->
          resolveTypeAlias typeEnv anotherType
        
        Nothing ->
          -- Should not happen if aType is well-formed according to typeEnv
          -- i.e. if checkType (envToContext typeEnv) aType == True
          Type_Constant id
    
    x -> x

typesAreEqual : TypeEnvironment -> Type -> Type -> Bool
typesAreEqual typeEnv type1 type2 =
  case (resolveTypeAlias typeEnv type1, resolveTypeAlias typeEnv type2) of
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


-- TYPE INFERENCE

type Multiplicity = Linear Bool | Unrestricted

type alias Context = List (Id, (Type, Multiplicity))

lookup : k -> List (k, a) -> Maybe a
lookup searchKey list =
  case list of
    [] -> Nothing

    (key, value) :: xs ->
      if key == searchKey
      then Just value
      else lookup searchKey xs

lookupAndUpdate : (a -> Maybe a) -> k -> List (k, a) -> List (k, a)
lookupAndUpdate f searchKey list =
  case list of
    [] -> []

    (key, value) :: xs ->
      if key == searchKey
      then
        case f value of
          Just newValue -> (key, newValue) :: xs
          Nothing -> xs
      else
        (key, value) :: lookupAndUpdate f searchKey xs

markAsUnavailable : Id -> Context -> Context
markAsUnavailable = lookupAndUpdate (\(aType, multiplicity) ->
  case multiplicity of
    Linear _     -> Just (aType, Linear False)
    Unrestricted -> Just (aType, Unrestricted))

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

typeInfer : TypeEnvironment -> Context -> Term -> Result String (Type, Context, Bool)
typeInfer typeEnv context term =
  case term of
    Term_Var id ->
      case lookup id context of
        Just (aType, Unrestricted) -> Ok (aType, context, False)
        Just (aType, Linear True)  -> Ok (aType, markAsUnavailable id context, False)
        Just (_    , Linear False) -> Err ("The linear variable " ++ id ++ " is used more than once")
        Nothing -> Err ("The variable " ++ id ++ " was not declared")
    
    Term_LinearLambda id varType body ->
      checkType (envToContext typeEnv) varType |> Result.andThen (\_ ->
      typeInfer typeEnv ((id, (varType, Linear True)) :: context) body
        |> Result.andThen (\(bodyType, outCtx, slack) ->
      if slack || not (isAvailable id outCtx)
      then Ok (Type_LinearFn varType bodyType, List.drop 1 outCtx, slack)
      else Err ("The linear variable " ++ id ++ " is not used")))
    
    Term_UnrestrictedLambda id varType body ->
      checkType (envToContext typeEnv) varType |> Result.andThen (\_ ->
      typeInfer typeEnv ((id, (varType, Unrestricted)) :: context) body
        |> Result.andThen (\(bodyType, outCtx, slack) ->
      Ok (Type_UnrestrictedFn varType bodyType, List.drop 1 outCtx, slack)))
    
    Term_Application e1 e2 ->
      typeInfer typeEnv context e1 |> Result.andThen (\(type1, outCtx1, slack1) ->
      case resolveTypeAlias typeEnv type1 of
        Type_LinearFn a b ->
          typeInfer  typeEnv outCtx1 e2 |> Result.andThen (\(type2, outCtx2, slack2) ->
          if typesAreEqual typeEnv type2 a
          then Ok (b, outCtx2, slack1 || slack2)
          else Err ("An expression of type " ++ typeToString a ++ " was expected, but it has type " ++ typeToString type2))
        
        Type_UnrestrictedFn a b ->
          typeInfer typeEnv (markAllAsUnavailable context) e2 |> Result.andThen (\(type2, _, _) ->
          if typesAreEqual typeEnv type2 a
          then Ok (b, outCtx1, slack1)
          else Err ("An expression of type " ++ typeToString a ++ " was expected, but it has type " ++ typeToString type2))

        _ -> Err ("An expression of a function type was expected, but it has type " ++ typeToString type1))
    
    Term_SimultaneousPair e1 e2 ->
      typeInfer typeEnv context e1 |> Result.andThen (\(type1, outCtx1, slack1) ->
      typeInfer typeEnv outCtx1 e2 |> Result.andThen (\(type2, outCtx2, slack2) ->
      Ok (Type_SimultaneousProduct type1 type2, outCtx2, slack1 || slack2)))
    
    Term_SimultaneousLet id1 id2 e1 e2 ->
      typeInfer typeEnv context e1 |> Result.andThen (\(type1, outCtx1, slack1) ->
      case resolveTypeAlias typeEnv type1 of
        Type_SimultaneousProduct a b ->
          typeInfer typeEnv ((id2, (b, Linear True)) :: (id1, (a, Linear True)) :: outCtx1) e2
            |> Result.andThen (\(type2, outCtx2, slack2) ->
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
        
        _ -> Err ("An expression of simultaneous product type was expected, but it has type " ++ typeToString type1))
    
    Term_Unit ->
      Ok (Type_Unit, context, False)
    
    Term_UnitLet e1 e2 ->
      typeInfer typeEnv context e1 |> Result.andThen (\(type1, outCtx1, slack1) ->
      case resolveTypeAlias typeEnv type1 of
        Type_Unit ->
          typeInfer typeEnv outCtx1 e2 |> Result.andThen (\(type2, outCtx2, slack2) ->
          Ok (type2, outCtx2, slack1 || slack2))
        
        _ ->
          Err ("An expression of unit type was expected, but it has type " ++ typeToString type1))
    
    Term_AlternativePair e1 e2 ->
      typeInfer typeEnv context e1 |> Result.andThen (\(type1, outCtx1, slack1) ->
      typeInfer typeEnv context e2 |> Result.andThen (\(type2, outCtx2, slack2) ->
      if contextsAreCompatible outCtx1 slack1 outCtx2 slack2
      then Ok (Type_AlternativeProduct type1 type2, contextIntersection outCtx1 outCtx2, slack1 && slack2)
      else Err "Some linear variable is used in one component of the alternative pair, but not in the other"))
    
    Term_Fst e ->
      typeInfer typeEnv context e |> Result.andThen (\(aType, outCtx, slack) ->
      case resolveTypeAlias typeEnv aType of
        Type_AlternativeProduct a _ ->
          Ok (a, outCtx, slack)
        
        _ ->
          Err ("An expression of alternative product type was expected, but it has type " ++ typeToString aType))
    
    Term_Snd e ->
      typeInfer typeEnv context e |> Result.andThen (\(aType, outCtx, slack) ->
      case resolveTypeAlias typeEnv aType of
        Type_AlternativeProduct _ b ->
          Ok (b, outCtx, slack)
        
        _ ->
          Err ("An expression of alternative product type was expected, but it has type " ++ typeToString aType))
    
    Term_Top ->
      Ok (Type_Top, context, True)
    
    Term_Inl type2 e ->
      checkType (envToContext typeEnv) type2 |> Result.andThen (\_ ->
      typeInfer typeEnv context e |> Result.andThen (\(type1, outCtx, slack) ->
      Ok (Type_Sum type1 type2, outCtx, slack)))
    
    Term_Inr type1 e ->
      checkType (envToContext typeEnv) type1 |> Result.andThen (\_ ->
      typeInfer typeEnv context e |> Result.andThen (\(type2, outCtx, slack) ->
      Ok (Type_Sum type1 type2, outCtx, slack)))

    Term_Case e id1 e1 id2 e2 ->
      typeInfer typeEnv context e |> Result.andThen (\(type1, outCtx1, slack1) ->
      case resolveTypeAlias typeEnv type1 of
        Type_Sum a b ->
          typeInfer typeEnv ((id1, (a, Linear True)) :: outCtx1) e1
            |> Result.andThen (\(type2, outCtx2, slack2) ->
          if (not slack2) && (isAvailable id1 outCtx2)
          then Err ("The linear variable " ++ id1 ++ " is not used")
          else
            typeInfer typeEnv ((id2, (b, Linear True)) :: outCtx1) e2
              |> Result.andThen (\(type3, outCtx3, slack3) ->
            if (not slack3) && (isAvailable id2 outCtx3)
            then Err ("The linear variable " ++ id2 ++ " is not used")
            else
              if not (typesAreEqual typeEnv type2 type3)
              then Err ("The types of both case branches should be equal, but the first branch has type " ++
                typeToString type2 ++ " while the second branch has type " ++ typeToString type3)
              else
                let
                  outCtx2_tail = List.drop 1 outCtx2
                  outCtx3_tail = List.drop 1 outCtx3
                in
                  if contextsAreCompatible outCtx2_tail slack2 outCtx3_tail slack3
                  then Ok (type2, contextIntersection outCtx2_tail outCtx3_tail, slack1 || (slack2 && slack3))
                  else Err "Some linear variable is used in one branch of the case expression, but not in the other"))
        
        _ ->
          Err ("An expression of sum type was expected, but it has type " ++ typeToString type1))
    
    Term_Abort aType e ->
      checkType (envToContext typeEnv) aType |> Result.andThen (\_ ->
      typeInfer typeEnv context e |> Result.andThen (\(type1, outCtx, _) ->
      case resolveTypeAlias typeEnv type1 of
        Type_Zero ->
          Ok (aType, outCtx, True)
        
        _ ->
          Err ("An expression of type 0 was expected, but it has type " ++ typeToString type1)))
    
    Term_Bang e ->
      typeInfer typeEnv (markAllAsUnavailable context) e |> Result.andThen (\(aType, _, _) ->
      Ok (Type_OfCourse aType, context, False))
    
    Term_BangLet id e1 e2 ->
      typeInfer typeEnv context e1 |> Result.andThen (\(type1, outCtx1, slack1) ->
      case resolveTypeAlias typeEnv type1 of
        Type_OfCourse a ->
          typeInfer typeEnv ((id, (a, Unrestricted)) :: outCtx1) e2
            |> Result.andThen (\(type2, outCtx2, slack2) ->
          Ok (type2, List.drop 1 outCtx2, slack1 || slack2))
        
        _ ->
          Err ("An expression of \"of course\" type was expected, but it has type " ++ typeToString type1))
  


-- VALUES AND ENVIRONMENTS

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



-- EVALUATION

eval : Environment -> Term -> Maybe Value
eval env term =
  case term of
    Term_Var id ->
      lookup id env
    
    Term_LinearLambda id _ e ->
      Just (Value_LinearFn id env e)
    
    Term_UnrestrictedLambda id _ e ->
      Just (Value_UnrestrictedFn id env e)
    
    Term_Application e1 e2 ->
      eval env e1 |> Maybe.andThen (\v1 ->
      case v1 of
        Value_LinearFn id fnEnv fnBody ->
          eval env e2 |> Maybe.andThen (\v2 ->
          eval ((id, v2) :: fnEnv) fnBody)
        
        Value_UnrestrictedFn id fnEnv fnBody ->
          eval env e2 |> Maybe.andThen (\v2 ->
          eval ((id, v2) :: fnEnv) fnBody)
        
        _ ->
          Nothing)
    
    Term_SimultaneousPair e1 e2 ->
      eval env e1 |> Maybe.andThen (\v1 ->
      eval env e2 |> Maybe.andThen (\v2 ->
      Just (Value_SimultaneousPair v1 v2)))
    
    Term_SimultaneousLet id1 id2 e1 e2 ->
      eval env e1 |> Maybe.andThen (\v ->
      case v of
        Value_SimultaneousPair v1 v2 ->
          eval ((id2, v2) :: (id1, v1) :: env) e2
        
        _ ->
          Nothing)
    
    Term_Unit ->
      Just Value_Unit
    
    Term_UnitLet e1 e2 ->
      eval env e1 |> Maybe.andThen (\v ->
      case v of
        Value_Unit ->
          eval env e2
        
        _ ->
          Nothing)
    
    Term_AlternativePair e1 e2 ->
      Just (Value_AlternativePair env e1 e2)
    
    Term_Fst e ->
      eval env e |> Maybe.andThen (\v ->
      case v of
        Value_AlternativePair env2 e1 _ ->
          eval env2 e1
        
        _ ->
          Nothing)
    
    Term_Snd e ->
      eval env e |> Maybe.andThen (\v ->
      case v of
        Value_AlternativePair env2 _ e2 ->
          eval env2 e2
        
        _ ->
          Nothing)
    
    Term_Top ->
      Just Value_Top
    
    Term_Inl _ e ->
      eval env e |> Maybe.andThen (\v ->
      Just (Value_Inl v))
    
    Term_Inr _ e ->
      eval env e |> Maybe.andThen (\v ->
      Just (Value_Inr v))
    
    Term_Case e id1 e1 id2 e2 ->
      eval env e |> Maybe.andThen (\v ->
      case v of
        Value_Inl v1 ->
          eval ((id1, v1) :: env) e1
        
        Value_Inr v2 ->
          eval ((id2, v2) :: env) e2
        
        _ ->
          Nothing)
    
    Term_Abort _ _ ->
      Nothing
    
    Term_Bang e ->
      Just (Value_Bang env e)
    
    Term_BangLet id e1 e2 ->
      eval env e1 |> Maybe.andThen (\v ->
      case v of
        Value_Bang bangEnv bangBody ->
          eval bangEnv bangBody |> Maybe.andThen (\v1 ->
          eval ((id, v1) :: env) e2)
        
        _ ->
          Nothing)