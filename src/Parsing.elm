module Parsing exposing (parseProgram)

import AbstractSyntax exposing (..)
import Combine exposing (..)
import Combine.Char exposing (anyChar)


parseProgram : String -> Result (List String) LambdaProgram
parseProgram input =
  case parse programParser input of
    Ok (_, _, program) ->
      Ok program
    Err (_, _, errors) ->
      Err errors



-- BASIC

token : Parser s a -> Parser s a
token parser = 
  whitespaceOrComments |> keep parser

whitespaceOrComments : Parser s ()
whitespaceOrComments =
  skipMany (or (skip (regex "\\s")) comment)

comment : Parser s ()
comment = or inlineComment multilineComment

inlineComment : Parser s ()
inlineComment = skip (regex "--[^\n]*")

multilineComment : Parser s ()
multilineComment =
  skip (string "{-")
    |> ignore (manyTill anyChar (string "-}"))

reservedWords : List String
reservedWords =
  ["let", "in", "fst", "snd", "inl", "inr", "case", "of", "abort", "atomic", "types", "unrestricted",
   "linear", "context", "typedef", "def", "end", "T"]

identifier : Parser s String
identifier = 
  token (regex "[A-Za-z_][0-9A-Za-z_-]*") |> andThen (\id ->
  if List.member id reservedWords
  then fail (id ++ " is a reserved word")
  else succeed id)



-- PROGRAM

programParser : Parser s LambdaProgram
programParser =
  optional [] atomicTypesClause |> andThen (\atomicTypes ->
  optional [] typedefClause |> andThen (\typedefs ->
  optional [] unrestrictedContextClause |> andThen (\unrestrictedContext ->
  optional [] linearContextClause |> andThen (\linearContext ->
  optional [] defClause |> andThen (\defs ->
  expression |> andThen (\mainTerm ->
  whitespaceOrComments |> andThen (\_ ->
  end |> andThen (\_ ->
  succeed
    { atomicTypes = atomicTypes
    , typedefs = typedefs
    , unrestrictedContext = unrestrictedContext
    , linearContext = linearContext
    , defs = defs
    , mainTerm = mainTerm
    }))))))))

atomicTypesClause : Parser s TypeContext
atomicTypesClause =
  token (string "atomic") |> andThen (\_ ->
  token (string "types") |> andThen (\_ ->
  atomicTypesList |> andThen (\atomicTypes ->
  token (string "end") |> andThen (\_ ->
  succeed atomicTypes))))

atomicTypesList : Parser s TypeContext
atomicTypesList =
  sepEndBy (token (string ";")) identifier

typedefClause : Parser s TypeEnvironment
typedefClause =
  token (string "typedef") |> andThen (\_ ->
  typedefList |> andThen (\typeEnv ->
  token (string "end") |> andThen (\_ ->
  succeed typeEnv)))

typedefList : Parser s TypeEnvironment
typedefList =
  sepEndBy (token (string ";")) typeDefinition

typeDefinition : Parser s (Id, Type)
typeDefinition =
  identifier |> andThen (\id ->
  token (string "=") |> andThen (\_ ->
  typeExpr |> andThen (\aType ->
  succeed (id, aType))))

unrestrictedContextClause : Parser s SimpleContext
unrestrictedContextClause =
  token (string "unrestricted") |> andThen (\_ ->
  token (string "context") |> andThen (\_ ->
  contextList |> andThen (\context ->
  token (string "end") |> andThen (\_ ->
  succeed context))))

linearContextClause : Parser s SimpleContext
linearContextClause =
  token (string "linear") |> andThen (\_ ->
  token (string "context") |> andThen (\_ ->
  contextList |> andThen (\context ->
  token (string "end") |> andThen (\_ ->
  succeed context))))

contextList : Parser s SimpleContext
contextList =
  sepEndBy (token (string ";")) contextDeclaration

contextDeclaration : Parser s (Id, Type)
contextDeclaration =
  identifier |> andThen (\id ->
  token (string ":") |> andThen (\_ ->
  typeExpr |> andThen (\aType ->
  succeed (id, aType))))

defClause : Parser s DefinitionEnvironment
defClause =
  token (string "def") |> andThen (\_ ->
  defList |> andThen (\defEnv ->
  token (string "end") |> andThen (\_ ->
  succeed defEnv)))

defList : Parser s DefinitionEnvironment
defList =
  sepEndBy (token (string ";")) definition

definition : Parser s (Id, Maybe Type, Term)
definition =
  identifier |> andThen (\id ->
  maybe typeAnnotation |> andThen (\maybeType ->
  token (string "=") |> andThen (\_ ->
  expression |> andThen (\term ->
  succeed (id, maybeType, term)))))

typeAnnotation : Parser s Type
typeAnnotation =
  token (string ":") |> andThen (\_ ->
  typeExpr)



-- TERM

expression : Parser s Term
expression =
  chainl (succeed Term_Application) atom

atom : Parser s Term
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

var : Parser s Term
var = Combine.map Term_Var identifier

linearLambda : Parser s Term
linearLambda = 
  token (string "\\^") |> andThen (\_ ->
  identifier |> andThen (\id ->
  token (string ":") |> andThen (\_ ->
  typeExpr |> andThen (\aType ->
  token (string ".") |> andThen (\_ ->
  expression |> andThen (\exp ->
  succeed (Term_LinearLambda id aType exp)
  ))))))

unrestrictedLambda : Parser s Term
unrestrictedLambda = 
  token (string "\\") |> andThen (\_ ->
  identifier |> andThen (\id ->
  token (string ":") |> andThen (\_ ->
  typeExpr |> andThen (\aType ->
  token (string ".") |> andThen (\_ ->
  expression |> andThen (\exp ->
  succeed (Term_UnrestrictedLambda id aType exp)
  ))))))

simultaneousPair : Parser s Term
simultaneousPair =
  token (string "{") |> andThen (\_ ->
  expression |> andThen (\e1 ->
  token (string ",") |> andThen (\_ ->
  expression |> andThen (\e2 ->
  token (string "}") |> andThen (\_ ->
  succeed (Term_SimultaneousPair e1 e2))))))

simultaneousLet : Parser s Term
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

unit : Parser s Term
unit = token (string "*") |> onsuccess Term_Unit

unitLet : Parser s Term
unitLet =
  token (string "let") |> andThen (\_ ->
  token (string "*") |> andThen (\_ ->
  token (string "=") |> andThen (\_ ->
  expression |> andThen (\e1 ->
  token (string "in") |> andThen (\_ ->
  expression |> andThen (\e2 ->
  succeed (Term_UnitLet e1 e2)))))))

alternativePair : Parser s Term
alternativePair =
  token (string "<") |> andThen (\_ ->
  expression |> andThen (\e1 ->
  token (string ",") |> andThen (\_ ->
  expression |> andThen (\e2 ->
  token (string ">") |> andThen (\_ ->
  succeed (Term_AlternativePair e1 e2))))))

fst : Parser s Term
fst =
  token (string "fst") |> andThen (\_ ->
  atom |> andThen (\e ->
  succeed (Term_Fst e)))

snd : Parser s Term
snd =
  token (string "snd") |> andThen (\_ ->
  atom |> andThen (\e ->
  succeed (Term_Snd e)))

top : Parser s Term
top = token (string "<>") |> onsuccess Term_Top

inl : Parser s Term
inl =
  token (string "inl") |> andThen (\_ ->
  token (string "[") |> andThen (\_ ->
  typeExpr |> andThen (\aType ->
  token (string "]") |> andThen (\_ ->
  atom |> andThen (\e ->
  succeed (Term_Inl aType e))))))

inr : Parser s Term
inr =
  token (string "inr") |> andThen (\_ ->
  token (string "[") |> andThen (\_ ->
  typeExpr |> andThen (\aType ->
  token (string "]") |> andThen (\_ ->
  atom |> andThen (\e ->
  succeed (Term_Inr aType e))))))

caseExpr : Parser s Term
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

abort : Parser s Term
abort =
  token (string "abort") |> andThen (\_ ->
  token (string "[") |> andThen (\_ ->
  typeExpr |> andThen (\aType ->
  token (string "]") |> andThen (\_ ->
  atom |> andThen (\e ->
  succeed (Term_Abort aType e))))))

bang : Parser s Term
bang =
  token (string "!") |> andThen (\_ ->
  atom |> andThen (\e ->
  succeed (Term_Bang e)))

bangLet : Parser s Term
bangLet =
  token (string "let") |> andThen (\_ ->
  token (string "!") |> andThen (\_ ->
  identifier |> andThen (\id ->
  token (string "=") |> andThen (\_ ->
  expression |> andThen (\e1 ->
  token (string "in") |> andThen (\_ ->
  expression |> andThen (\e2 ->
  succeed (Term_BangLet id e1 e2))))))))

exprBetweenParens : Parser s Term
exprBetweenParens = between (token (string "(")) (token (string ")")) (lazy (\_ -> expression))



-- TYPE

typeExpr : Parser s Type
typeExpr =
  chainr fnConnective additiveChain

fnConnective : Parser s (Type -> Type -> Type)
fnConnective =
  choice
    [ token (string "-o") |> onsuccess Type_LinearFn
    , token (string "->") |> onsuccess Type_UnrestrictedFn
    ]

additiveChain : Parser s Type
additiveChain =
  chainl additiveConnective multiplicativeChain

additiveConnective : Parser s (Type -> Type -> Type)
additiveConnective =
  choice
    [ token (string "&") |> onsuccess Type_AlternativeProduct
    , token (string "+") |> onsuccess Type_Sum
    ]

multiplicativeChain : Parser s Type
multiplicativeChain =
  chainl multiplicativeConnective typeAtom

multiplicativeConnective : Parser s (Type -> Type -> Type)
multiplicativeConnective = token (string "*") |> onsuccess (Type_SimultaneousProduct)

typeAtom : Parser s Type
typeAtom =
  choice
    [ typeConstant
    , token (string "1") |> onsuccess Type_Unit
    , token (string "T") |> onsuccess Type_Top
    , token (string "0") |> onsuccess Type_Zero
    , typeOfCourse
    , typeBetweenParens
    ]

typeConstant : Parser s Type
typeConstant = Combine.map Type_Constant identifier

typeOfCourse : Parser s Type
typeOfCourse =
  token (string "!") |> andThen (\_ ->
  typeAtom |> andThen (\aType ->
  succeed (Type_OfCourse aType)))

typeBetweenParens : Parser s Type
typeBetweenParens = between (token (string "(")) (token (string ")")) (lazy (\_ -> typeExpr))