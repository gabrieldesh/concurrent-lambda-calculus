module AbstractSyntax exposing
  ( Id
  , Type(..)
  , typeToString
  , Term(..)
  , Kind(..)
  , Multiplicity(..)
  , TypeVarContext
  , TypeDefs
  , TermVarContext
  , STLProgram
  )


type alias Id = String

type Kind = Kind_SessionType | Kind_NonSessionType

type Multiplicity = Lin | Un

-- TYPES

type Type
  = Type_Var Id
  | Type_VarDual Id
  | Type_Rec Id Kind Type
  | Type_Send Type Type
  | Type_Receive Type Type
  | Type_Select Type Type
  | Type_Branch Type Type
  | Type_End
  | Type_LinearFn Type Type
  | Type_UnrestrictedFn Type Type
  | Type_SimultaneousProduct Type Type
  | Type_Unit
  | Type_Sum Type Type
  | Type_OfCourse Type
  | Type_Accept Type
  | Type_Request Type
  | Type_Dual Type


kindToString : Kind -> String
kindToString kind =
  case kind of
    Kind_SessionType -> "*s"
    Kind_NonSessionType -> "*ns"

typeToString : Type -> String
typeToString aType =
  case aType of
    Type_Var id ->
      id
    
    Type_VarDual id ->
      "dual(" ++ id ++ ")"
    
    Type_Rec id kind type1 ->
      "rec " ++ id ++ " : " ++ kindToString kind ++ " . " ++ typeToString type1
    
    Type_Send type1 type2 ->
      "!" ++ typeToString type1 ++ "." ++ typeToString type2
    
    Type_Receive type1 type2 ->
      "?" ++ typeToString type1 ++ "." ++ typeToString type2
    
    Type_Select type1 type2 ->
      "+{ " ++ typeToString type1 ++ ", " ++ typeToString type2 ++ " }"
    
    Type_Branch type1 type2 ->
      "&{ " ++ typeToString type1 ++ ", " ++ typeToString type2 ++ " }"

    Type_End ->
      "End"
    
    Type_LinearFn type1 type2 ->
      "(" ++ typeToString type1 ++ ") -o (" ++ typeToString type2 ++ ")"
    
    Type_UnrestrictedFn type1 type2 ->
      "(" ++ typeToString type1 ++ ") -> (" ++ typeToString type2 ++ ")"
    
    Type_SimultaneousProduct type1 type2 ->
      "(" ++ typeToString type1 ++ ") * (" ++ typeToString type2 ++ ")"
    
    Type_Unit ->
      "1"
    
    Type_Sum type1 type2 ->
      "(" ++ typeToString type1 ++ ") + (" ++ typeToString type2 ++ ")"
    
    Type_OfCourse type1 ->
      "@(" ++ typeToString type1 ++ ")"
    
    Type_Accept type1 ->
      "Accept (" ++ typeToString type1 ++ ")"
    
    Type_Request type1 ->
      "Request (" ++ typeToString type1 ++ ")"
    
    Type_Dual type1 ->
      "dual(" ++ typeToString type1 ++ ")"



-- TERMS

type Term
  = Term_Var Id
  | Term_LinearLambda Id Type Term
  | Term_UnrestrictedLambda Id Type Term
  | Term_Application Term Term
  | Term_Pair Term Term
  | Term_LetPair Id Id Term Term
  | Term_Unit
  | Term_LetUnit Term Term
  | Term_Inl Type Term
  | Term_Inr Type Term
  | Term_Case Term Id Term Id Term
  | Term_OfCourse Term
  | Term_LetOfCourse Id Term Term
  | Term_Send Term Term
  | Term_Receive Term
  | Term_SelectLeft Term
  | Term_SelectRight Term
  | Term_Branch Term Id Term Id Term
  | Term_Close Term
  | Term_NewAccess Type Id Id Term
  | Term_Accept Term
  | Term_Request Term
  | Term_NewSession Type
  | Term_Fork Term
  | Term_Spawn Term
  | Term_Fold Type Term
  | Term_Unfold Term
  | Term_LetLin Id (Maybe Type) Term Term
  | Term_LetUn Id (Maybe Type) Term Term
  | Term_LetRec Id Type Term Term



-- PROGRAM

type alias TypeVarContext = List (Id, Kind)

type alias TypeDefs = List (Id, Type)

type alias TermVarContext = List (Id, (Multiplicity, Type))

type alias STLProgram =
  { typevars : TypeVarContext
  , typedefs : TypeDefs
  , vars : TermVarContext
  , mainTerm : Term
  }