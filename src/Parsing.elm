module Parsing exposing (parseProgram)

import AbstractSyntax exposing (..)
import Combine exposing (..)
import Combine.Char exposing (anyChar)


parseProgram : String -> Result (List String) CLCProgram
parseProgram input =
  case parse programParser input of
    Ok (_, _, program) ->
      Ok program
    Err (_, _, errors) ->
      Err errors



-- BÁSICO

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
  ["rec", "End", "Accept", "Request", "lin", "un", "let", "in", "inl", "inr", "case", "of", "send", "receive", 
   "select", "left", "right", "branch", "close", "new", "access", "accept", "request", "session", "fork", "spawn",
   "fold", "unfold", "typevars", "typedefs", "vars", "end", "dual"]

identifier : Parser s String
identifier = 
  token (regex "[A-Za-z_]['0-9A-Za-z_-]*") |> andThen (\id ->
  if List.member id reservedWords
  then fail (id ++ " é uma palavra reservada")
  else succeed id)

typeAnnotation : Parser s Type
typeAnnotation =
  token (string ":") |> andThen (\_ ->
  typeExpr)


kindExpr : Parser s Kind
kindExpr =
  choice
    [ token (string "*s") |> onsuccess Kind_SessionType
    , token (string "*ns") |> onsuccess Kind_NonSessionType
    ]

multiplicityTag : Parser s Multiplicity
multiplicityTag =
  choice
    [ token (string "lin") |> onsuccess Lin
    , token (string "un") |> onsuccess Un
    ]


-- PROGRAMA

programParser : Parser s CLCProgram
programParser = 
  optional [] typevarsClause |> andThen (\typevars ->
  optional [] typedefsClause |> andThen (\typedefs ->
  optional [] varsClause |> andThen (\vars ->
  expression |> andThen (\mainTerm ->
  whitespaceOrComments |> andThen (\_ ->
  end |> andThen (\_ ->
  succeed
    { typevars = typevars
    , typedefs = typedefs
    , vars = vars
    , mainTerm = mainTerm
    }))))))

typevarsClause : Parser s TypeVarContext
typevarsClause =
  token (string "typevars") |> andThen (\_ ->
  typevarsList |> andThen (\typevars ->
  token (string "end") |> andThen (\_ ->
  succeed typevars)))

typevarsList : Parser s TypeVarContext
typevarsList =
  sepEndBy (token (string ";")) typevarDeclaration

typevarDeclaration : Parser s (Id, Kind)
typevarDeclaration =
  identifier |> andThen (\id ->
  token (string ":") |> andThen (\_ ->
  kindExpr |> andThen (\kind ->
  succeed (id, kind))))

typedefsClause : Parser s TypeVarEnv
typedefsClause =
  token (string "typedefs") |> andThen (\_ ->
  typedefsList |> andThen (\typedefs ->
  token (string "end") |> andThen (\_ ->
  succeed typedefs)))

typedefsList : Parser s TypeVarEnv
typedefsList =
  sepEndBy (token (string ";")) typeDefinition

typeDefinition : Parser s (Id, Type)
typeDefinition =
  identifier |> andThen (\id ->
  token (string "=") |> andThen (\_ ->
  typeExpr |> andThen (\aType ->
  succeed (id, aType))))

varsClause : Parser s VarDeclarations
varsClause =
  token (string "vars") |> andThen (\_ ->
  varsList |> andThen (\context ->
  token (string "end") |> andThen (\_ ->
  succeed context)))

varsList : Parser s VarDeclarations
varsList =
  sepEndBy (token (string ";")) varDeclaration

varDeclaration : Parser s (Id, (Multiplicity, Type))
varDeclaration =
  multiplicityTag |> andThen (\mult ->
  identifier |> andThen (\id ->
  token (string ":") |> andThen (\_ ->
  typeExpr |> andThen (\aType ->
  succeed (id, (mult, aType))))))



-- TERMO

expression : Parser s Term
expression =
  chainl (succeed Term_Application) atom

atom : Parser s Term
atom = 
  choice
    [ var
    , linearLambda
    , unrestrictedLambda
    , pair
    , letPair
    , unit
    , letUnit
    , inl
    , inr
    , caseExpr
    , ofCourse
    , letOfCourse
    , send
    , receive
    , selectLeft
    , selectRight
    , branch
    , close
    , newAccess
    , accept
    , request
    , newSession
    , fork
    , spawn
    , fold
    , unfold
    , letLin
    , letUn
    , letRec
    , exprBetweenParens
    ]

var : Parser s Term
var = Combine.map Term_Var identifier

linearLambda : Parser s Term
linearLambda = 
  token (string "\\") |> andThen (\_ ->
  token (string "lin") |> andThen (\_ ->
  identifier |> andThen (\id ->
  token (string ":") |> andThen (\_ ->
  typeExpr |> andThen (\aType ->
  token (string ".") |> andThen (\_ ->
  expression |> andThen (\exp ->
  succeed (Term_LinearLambda id aType exp)
  )))))))

unrestrictedLambda : Parser s Term
unrestrictedLambda = 
  token (string "\\") |> andThen (\_ ->
  token (string "un") |> andThen (\_ ->
  identifier |> andThen (\id ->
  token (string ":") |> andThen (\_ ->
  typeExpr |> andThen (\aType ->
  token (string ".") |> andThen (\_ ->
  expression |> andThen (\exp ->
  succeed (Term_UnrestrictedLambda id aType exp)
  )))))))

pair : Parser s Term
pair =
  token (string "{") |> andThen (\_ ->
  expression |> andThen (\e1 ->
  token (string ",") |> andThen (\_ ->
  expression |> andThen (\e2 ->
  token (string "}") |> andThen (\_ ->
  succeed (Term_Pair e1 e2))))))

letPair : Parser s Term
letPair =
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
  succeed (Term_LetPair id1 id2 e1 e2)))))))))))

unit : Parser s Term
unit = 
  token (string "{") |> andThen (\_ ->
  token (string "}") |> andThen (\_ ->
  succeed Term_Unit))

letUnit : Parser s Term
letUnit =
  token (string "let") |> andThen (\_ ->
  token (string "{") |> andThen (\_ ->
  token (string "}") |> andThen (\_ ->
  token (string "=") |> andThen (\_ ->
  expression |> andThen (\e1 ->
  token (string "in") |> andThen (\_ ->
  expression |> andThen (\e2 ->
  succeed (Term_LetUnit e1 e2))))))))

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
  token (string "->") |> andThen (\_ ->
  expression |> andThen (\e1 ->
  token (string ";") |> andThen (\_ ->
  token (string "inr") |> andThen (\_ ->
  identifier |> andThen (\id2 ->
  token (string "->") |> andThen (\_ ->
  expression |> andThen (\e2 ->
  succeed (Term_Case e id1 e1 id2 e2)))))))))))))

ofCourse : Parser s Term
ofCourse =
  token (string "@") |> andThen (\_ ->
  atom |> andThen (\e ->
  succeed (Term_OfCourse e)))

letOfCourse : Parser s Term
letOfCourse =
  token (string "let") |> andThen (\_ ->
  token (string "@") |> andThen (\_ ->
  identifier |> andThen (\id ->
  token (string "=") |> andThen (\_ ->
  expression |> andThen (\e1 ->
  token (string "in") |> andThen (\_ ->
  expression |> andThen (\e2 ->
  succeed (Term_LetOfCourse id e1 e2))))))))

send : Parser s Term
send =
  token (string "send") |> andThen (\_ ->
  atom |> andThen (\e1 ->
  atom |> andThen (\e2 ->
  succeed (Term_Send e1 e2))))

receive : Parser s Term
receive =
  token (string "receive") |> andThen (\_ ->
  atom |> andThen (\e ->
  succeed (Term_Receive e)))

selectLeft : Parser s Term
selectLeft =
  token (string "select") |> andThen (\_ ->
  token (string "left") |> andThen (\_ ->
  atom |> andThen (\e ->
  succeed (Term_SelectLeft e))))

selectRight : Parser s Term
selectRight =
  token (string "select") |> andThen (\_ ->
  token (string "right") |> andThen (\_ ->
  atom |> andThen (\e ->
  succeed (Term_SelectRight e))))

branch : Parser s Term
branch =
  token (string "branch") |> andThen (\_ ->
  expression |> andThen (\e ->
  token (string "of") |> andThen (\_ ->
  token (string "left") |> andThen (\_ ->
  identifier |> andThen (\id1 ->
  token (string "->") |> andThen (\_ ->
  expression |> andThen (\e1 ->
  token (string ";") |> andThen (\_ ->
  token (string "right") |> andThen (\_ ->
  identifier |> andThen (\id2 ->
  token (string "->") |> andThen (\_ ->
  expression |> andThen (\e2 ->
  succeed (Term_Branch e id1 e1 id2 e2)))))))))))))

close : Parser s Term
close =
  token (string "close") |> andThen (\_ ->
  atom |> andThen (\e ->
  succeed (Term_Close e)))

newAccess : Parser s Term
newAccess =
  token (string "new") |> andThen (\_ ->
  token (string "access") |> andThen (\_ ->
  token (string "[") |> andThen (\_ ->
  typeExpr |> andThen (\aType ->
  token (string "]") |> andThen (\_ ->
  identifier |> andThen (\id1 ->
  token (string ",") |> andThen (\_ ->
  identifier |> andThen (\id2 ->
  token (string "in") |> andThen (\_ ->
  expression |> andThen (\e ->
  succeed (Term_NewAccess aType id1 id2 e)))))))))))

accept : Parser s Term
accept =
  token (string "accept") |> andThen (\_ ->
  atom |> andThen (\e ->
  succeed (Term_Accept e)))

request : Parser s Term
request =
  token (string "request") |> andThen (\_ ->
  atom |> andThen (\e ->
  succeed (Term_Request e)))

newSession : Parser s Term
newSession =
  token (string "new") |> andThen (\_ ->
  token (string "session") |> andThen (\_ ->
  token (string "[") |> andThen (\_ ->
  typeExpr |> andThen (\aType ->
  token (string "]") |> andThen (\_ ->
  succeed (Term_NewSession aType))))))

fork : Parser s Term
fork =
  token (string "fork") |> andThen (\_ ->
  atom |> andThen (\e ->
  succeed (Term_Fork e)))

spawn : Parser s Term
spawn =
  token (string "spawn") |> andThen (\_ ->
  atom |> andThen (\e ->
  succeed (Term_Spawn e)))

fold : Parser s Term
fold =
  token (string "fold") |> andThen (\_ ->
  token (string "[") |> andThen (\_ ->
  typeExpr |> andThen (\aType ->
  token (string "]") |> andThen (\_ ->
  atom |> andThen (\e ->
  succeed (Term_Fold aType e))))))

unfold : Parser s Term
unfold =
  token (string "unfold") |> andThen (\_ ->
  atom |> andThen (\e ->
  succeed (Term_Unfold e)))

letLin : Parser s Term
letLin =
  token (string "let") |> andThen (\_ ->
  token (string "lin") |> andThen (\_ ->
  identifier |> andThen (\id ->
  maybe typeAnnotation |> andThen (\maybeType ->
  token (string "=") |> andThen (\_ ->
  expression |> andThen (\e1 ->
  token (string "in") |> andThen (\_ ->
  expression |> andThen (\e2 ->
  succeed (Term_LetLin id maybeType e1 e2)))))))))

letUn : Parser s Term
letUn =
  token (string "let") |> andThen (\_ ->
  token (string "un") |> andThen (\_ ->
  identifier |> andThen (\id ->
  maybe typeAnnotation |> andThen (\maybeType ->
  token (string "=") |> andThen (\_ ->
  expression |> andThen (\e1 ->
  token (string "in") |> andThen (\_ ->
  expression |> andThen (\e2 ->
  succeed (Term_LetUn id maybeType e1 e2)))))))))

letRec : Parser s Term
letRec =
  token (string "let") |> andThen (\_ ->
  token (string "rec") |> andThen (\_ ->
  identifier |> andThen (\id ->
  typeAnnotation |> andThen (\aType ->
  token (string "=") |> andThen (\_ ->
  expression |> andThen (\e1 ->
  token (string "in") |> andThen (\_ ->
  expression |> andThen (\e2 ->
  succeed (Term_LetRec id aType e1 e2)))))))))

exprBetweenParens : Parser s Term
exprBetweenParens = between (token (string "(")) (token (string ")")) (lazy (\_ -> expression))



-- TIPO

typeExpr : Parser s Type
typeExpr =
  chainr fnTypeCons sumChain

fnTypeCons : Parser s (Type -> Type -> Type)
fnTypeCons =
  choice
    [ token (string "-o") |> onsuccess Type_LinearFn
    , token (string "->") |> onsuccess Type_UnrestrictedFn
    ]

sumChain : Parser s Type
sumChain =
  chainl sumTypeCons productChain

sumTypeCons : Parser s (Type -> Type -> Type)
sumTypeCons =
  token (string "+") |> onsuccess Type_Sum

productChain : Parser s Type
productChain =
  chainl productTypeCons typeAtom

productTypeCons : Parser s (Type -> Type -> Type)
productTypeCons = 
  token (string "*") |> onsuccess (Type_Product)

typeAtom : Parser s Type
typeAtom =
  choice
    [ typeVariable
    , typeRec
    , typeSend
    , typeReceive
    , typeSelect
    , typeBranch
    , typeEnd
    , typeUnit
    , typeOfCourse
    , typeAccept
    , typeRequest
    , typeDual
    , typeBetweenParens
    ]

typeVariable : Parser s Type
typeVariable = Combine.map Type_Var identifier

typeRec : Parser s Type
typeRec =
  token (string "rec") |> andThen (\_ ->
  identifier |> andThen (\id ->
  token (string ":") |> andThen (\_ ->
  kindExpr |> andThen (\kind ->
  token (string ".") |> andThen (\_ ->
  typeExpr |> andThen (\aType ->
  succeed (Type_Rec id kind aType)))))))

typeSend : Parser s Type
typeSend =
  token (string "!") |> andThen (\_ ->
  typeExpr |> andThen (\type1 ->
  token (string ".") |> andThen (\_ ->
  typeExpr |> andThen (\type2 ->
  succeed (Type_Send type1 type2)))))

typeReceive : Parser s Type
typeReceive =
  token (string "?") |> andThen (\_ ->
  typeExpr |> andThen (\type1 ->
  token (string ".") |> andThen (\_ ->
  typeExpr |> andThen (\type2 ->
  succeed (Type_Receive type1 type2)))))

typeSelect : Parser s Type
typeSelect =
  token (string "+") |> andThen (\_ ->
  token (string "{") |> andThen (\_ ->
  typeExpr |> andThen (\type1 ->
  token (string ",") |> andThen (\_ ->
  typeExpr |> andThen (\type2 ->
  token (string "}") |> andThen (\_ ->
  succeed (Type_Select type1 type2)))))))

typeBranch : Parser s Type
typeBranch =
  token (string "&") |> andThen (\_ ->
  token (string "{") |> andThen (\_ ->
  typeExpr |> andThen (\type1 ->
  token (string ",") |> andThen (\_ ->
  typeExpr |> andThen (\type2 ->
  token (string "}") |> andThen (\_ ->
  succeed (Type_Branch type1 type2)))))))

typeEnd : Parser s Type
typeEnd =
  token (string "End") |> onsuccess Type_End

typeUnit : Parser s Type
typeUnit =
  token (string "1") |> onsuccess Type_Unit

typeOfCourse : Parser s Type
typeOfCourse =
  token (string "@") |> andThen (\_ ->
  typeAtom |> andThen (\aType ->
  succeed (Type_OfCourse aType)))

typeAccept : Parser s Type
typeAccept =
  token (string "Accept") |> andThen (\_ ->
  typeAtom |> andThen (\aType ->
  succeed (Type_Accept aType)))

typeRequest : Parser s Type
typeRequest =
  token (string "Request") |> andThen (\_ ->
  typeAtom |> andThen (\aType ->
  succeed (Type_Request aType)))

typeDual : Parser s Type
typeDual =
  token (string "dual") |> andThen (\_ ->
  typeBetweenParens |> andThen (\aType ->
  succeed (Type_Dual aType)))

typeBetweenParens : Parser s Type
typeBetweenParens = 
  between (token (string "(")) (token (string ")")) (lazy (\_ -> typeExpr))