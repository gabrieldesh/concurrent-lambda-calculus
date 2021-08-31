module AbstractSyntax exposing
  ( Id
  , Type(..)
  , typeToString
  , Term(..)
  , termToString
  , TypeContext
  , TypeEnvironment
  , DefinitionEnvironment
  , SimpleContext
  , LambdaProgram
  )


type alias Id = String


-- TYPES

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
      "@" ++ typeToString type1



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

type alias TypeContext = List Id

type alias TypeEnvironment = List (Id, Type)

type alias SimpleContext = List (Id, Type)

type alias DefinitionEnvironment = List (Id, Maybe Type, Term)

type alias LambdaProgram =
  { atomicTypes : TypeContext
  , typedefs : TypeEnvironment
  , unrestrictedContext : SimpleContext
  , linearContext : SimpleContext
  , defs : DefinitionEnvironment
  , mainTerm : Term
  }