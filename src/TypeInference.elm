module TypeInference exposing (typeInferProgram)

import AbstractSyntax exposing (..)
import Utils exposing (lookup)
import Result exposing (andThen, mapError)


-- INFERÊNCIA DE TIPO DO PROGRAMA

typeInferProgram : STLProgram -> Result String Type
typeInferProgram { typevars, typedefs, vars, mainTerm } =
  checkTypedefs (List.reverse typevars) typedefs |> andThen (\typeVarContext ->
  let
    typeVarEnv = List.reverse typedefs
  in
  checkVarDeclarations typeVarContext typeVarEnv [] vars |> andThen (\termVarContext ->
  typeInferTerm typeVarContext typeVarEnv termVarContext mainTerm
    |> andThen (\(aType, outputContext) ->
  checkUsedLinVars outputContext |> andThen (\_ ->
  Ok (resolveDualEager aType)))))

checkUsedLinVars : TermVarContext -> Result String ()
checkUsedLinVars context =
  case context of
    [] ->
      Ok ()

    (id, (_, Linear NotUsed)) :: _ ->
      Err ("A variável linear " ++ id ++ " não foi usada")
    
    _ :: xs ->
      checkUsedLinVars xs



--- BOA-FORMAÇÃO DE TIPOS

-- Checa se todas as definições de tipo são bem-formadas. Retorna um erro se algum tipo é mal-formado, ou o 
-- TypeVarContext estendido com as novas variáveis de tipo definidas.
checkTypedefs : TypeVarContext -> TypeVarEnv -> Result String TypeVarContext
checkTypedefs typeVarContext typeVarEnv =
  case typeVarEnv of
    [] ->
      Ok typeVarContext
    
    (id, aType) :: xs ->
      checkType typeVarContext aType
        |> mapError (\e -> "Na definição de " ++ id ++ ": " ++ e)
        |> andThen (\kind ->
      checkTypedefs ((id, kind) :: typeVarContext) xs)

-- Checa se um tipo é bem-formado. Retorna o kind, ou um erro se o tipo é mal-formado.
checkType : TypeVarContext -> Type -> Result String Kind
checkType typeVarContext aType =
  case aType of
    Type_Var id ->
      case lookup id typeVarContext of
        Just kind -> Ok kind
        Nothing   -> Err ("A variável de tipo " ++ id ++ " não foi declarada")
    
    Type_Rec id declaredKind t ->
      checkType ((id, declaredKind) :: typeVarContext) t |> andThen (\inferredKind ->
      if inferredKind == declaredKind
      then Ok declaredKind
      else Err ("O kind declarado da variável recursiva " ++ id ++ " é " ++ kindToString declaredKind 
        ++ ", mas o corpo da recursão, " ++ typeToString t 
        ++ ", é do kind " ++ kindToString inferredKind))
    
    Type_Send t1 t2 ->
      checkType typeVarContext t1 |> andThen (\_ ->
      checkType typeVarContext t2 |> andThen (\kind ->
      if kind == Kind_SessionType
      then Ok Kind_SessionType
      else Err ("A continuação de um tipo send (!_._) deve ser um tipo de sessão, mas o tipo fornecido foi "
        ++ typeToString t2 ++ ", que é do kind *ns")))
    
    Type_Receive t1 t2 ->
      checkType typeVarContext t1 |> andThen (\_ ->
      checkType typeVarContext t2 |> andThen (\kind ->
      if kind == Kind_SessionType
      then Ok Kind_SessionType
      else Err ("A continuação de um tipo receive (?_._) deve ser um tipo de sessão, mas o tipo fornecido foi "
        ++ typeToString t2 ++ ", que é do kind *ns")))
    
    Type_Select t1 t2 ->
      checkType typeVarContext t1 |> andThen (\kind1 ->
      checkType typeVarContext t2 |> andThen (\kind2 ->
      if kind1 == Kind_SessionType
      then
        if kind2 == Kind_SessionType
        then Ok Kind_SessionType
        else Err ("A opção right de um tipo select (+{_, _}) deve ser um tipo de sessão, mas o tipo fornecido foi "
          ++ typeToString t2 ++ ", que é do kind *ns")
      else
        Err ("A opção left de um tipo select (+{_, _}) deve ser um tipo de sessão, mas o tipo fornecido foi "
          ++ typeToString t1 ++ ", que é do kind *ns")))
    
    Type_Branch t1 t2 ->
      checkType typeVarContext t1 |> andThen (\kind1 ->
      checkType typeVarContext t2 |> andThen (\kind2 ->
      if kind1 == Kind_SessionType
      then
        if kind2 == Kind_SessionType
        then Ok Kind_SessionType
        else Err ("A opção right de um tipo branch (&{_, _}) deve ser um tipo de sessão, mas o tipo fornecido foi "
          ++ typeToString t2 ++ ", que é do kind *ns")
      else
        Err ("A opção left de um tipo branch (&{_, _}) deve ser um tipo de sessão, mas o tipo fornecido foi "
          ++ typeToString t1 ++ ", que é do kind *ns")))
    
    Type_End ->
      Ok Kind_SessionType
    
    Type_LinearFn t1 t2 ->
      checkType typeVarContext t1 |> andThen (\_ ->
      checkType typeVarContext t2 |> andThen (\_ ->
      Ok Kind_NonSessionType))
    
    Type_UnrestrictedFn t1 t2 ->
      checkType typeVarContext t1 |> andThen (\_ ->
      checkType typeVarContext t2 |> andThen (\_ ->
      Ok Kind_NonSessionType))
    
    Type_Product t1 t2 ->
      checkType typeVarContext t1 |> andThen (\_ ->
      checkType typeVarContext t2 |> andThen (\_ ->
      Ok Kind_NonSessionType))
    
    Type_Unit ->
      Ok Kind_NonSessionType
    
    Type_Sum t1 t2 ->
      checkType typeVarContext t1 |> andThen (\_ ->
      checkType typeVarContext t2 |> andThen (\_ ->
      Ok Kind_NonSessionType))
    
    Type_OfCourse t ->
      checkType typeVarContext t |> andThen (\_ ->
      Ok Kind_NonSessionType)
    
    Type_Accept t ->
      checkType typeVarContext t |> andThen (\kind ->
      if kind == Kind_SessionType
      then Ok Kind_NonSessionType
      else Err ("O construtor de tipo Accept espera receber um tipo de sessão, mas o tipo fornecido foi "
        ++ typeToString t ++ ", que é do kind *ns"))
    
    Type_Request t ->
      checkType typeVarContext t |> andThen (\kind ->
      if kind == Kind_SessionType
      then Ok Kind_NonSessionType
      else Err ("O construtor de tipo Request espera receber um tipo de sessão, mas o tipo fornecido foi "
        ++ typeToString t ++ ", que é do kind *ns"))
    
    Type_Dual t ->
      checkType typeVarContext t |> andThen (\kind ->
      if kind == Kind_SessionType
      then Ok Kind_SessionType
      else Err ("A operação de dual é definida apenas para tipos de sessão, mas o tipo fornecido foi "
        ++ typeToString t ++ ", que é do kind *ns"))

checkAndApplySubs : TypeVarContext -> TypeVarEnv -> Type -> Result String (Type, Kind)
checkAndApplySubs typeVarContext typeVarEnv aType =
  checkType typeVarContext aType |> andThen (\kind ->
  let
    t = applySubs typeVarEnv aType
  in
  Ok (t, kind))



-- BOA-FORMAÇÃO DO CONTEXTO DE VARIÁVEIS DE TERMO

-- Guarda o tipo e multiplicidade de cada variável de termo, e informação de uso das variáveis lineares.
type alias TermVarContext = List (Id, (Type, VarUsage))

type VarUsage = Linear LinearStatus | Unrestricted

type LinearStatus = NotUsed | Used | UsedUnrestrictedly

-- Marca uma variável linear como usada.
markAsUsed : Id -> TermVarContext -> TermVarContext
markAsUsed k context = 
  case context of
    [] -> []

    (id, (aType, Unrestricted)) :: xs ->
      (id, (aType, Unrestricted)) :: (markAsUsed k xs)
    
    (id, (aType, Linear status)) :: xs ->
      if id == k then
        (id, (aType, Linear Used)) :: xs
      else
        (id, (aType, Linear status)) :: (markAsUsed k xs)

-- Marca que todas as variáveis lineares estão em um contexto que pode ser replicado de forma irrestrita
-- (zero ou mais vezes), como por exemplo a construção de um tipo "of course" (@) ou um argumento aplicado 
-- a uma função irrestrita.
markAsUsedUnrestrictedly : TermVarContext -> TermVarContext
markAsUsedUnrestrictedly context =
  let
    f (id, (aType, varUsage)) =
      case varUsage of
        Unrestricted ->
          (id, (aType, Unrestricted))
    
        Linear _ ->
          (id, (aType, Linear UsedUnrestrictedly))
  in
    List.map f context

isUsed : Id -> TermVarContext -> Bool
isUsed id context =
  case lookup id context of
    Just (_, Linear Used) -> True
    _                     -> False

-- Checa se o tipo em cada declaração de variável de termo é bem-formado, e substitui as variáveis de tipo pelas 
-- suas definições. Retorna um erro se algum tipo é mal-formado, ou retorna as declarações na forma de um 
-- TermVarContext, com as variáveis lineares marcadas como não usadas.
checkVarDeclarations : TypeVarContext -> TypeVarEnv -> TermVarContext -> VarDeclarations 
  -> Result String TermVarContext
checkVarDeclarations typeVarContext typeVarEnv termVarContext vars =
  case vars of
    [] ->
      Ok termVarContext
    
    (id, (multiplicity, aType)) :: xs ->
      checkAndApplySubs typeVarContext typeVarEnv aType
        |> mapError (\e -> "Na declaração de " ++ id ++ ": " ++ e)
        |> andThen (\(t, _) ->
      let
        usage = case multiplicity of
          Lin -> Linear NotUsed
          Un  -> Unrestricted
        extendedContext = (id, (t, usage)) :: termVarContext
      in
      checkVarDeclarations typeVarContext typeVarEnv extendedContext xs)



-- SUBSTITUIÇÃO EM TIPOS

-- Substitui todas as definições de typeVarEnv dentro do tipo aType.
applySubs : TypeVarEnv -> Type -> Type
applySubs typeVarEnv aType =
  case typeVarEnv of
    [] -> aType

    (id, t) :: xs ->
      applySubs xs (subs aType id t)

-- Substitui a variável id pelo tipo t dentro do tipo aType
-- aType[id := t]
subs : Type -> Id -> Type -> Type
subs aType id t =
  subsAux (freeVars t) aType id t

subsAux : List Id -> Type -> Id -> Type -> Type
subsAux freeVarsT aType id t = 
  case aType of
    Type_Var otherId ->
      if otherId == id then
        t
      else
        Type_Var otherId
    
    Type_Rec binderId kind body ->
      if id == binderId then
        Type_Rec binderId kind body
      else
        if (List.member binderId freeVarsT) then
          let
            newId       = findNewId binderId (freeVarsT ++ (freeVars body))
            newBody     = subs body binderId (Type_Var newId)
            subsNewBody = subsAux freeVarsT newBody id t
          in
          Type_Rec newId kind subsNewBody
        else
          let
            subsBody = subsAux freeVarsT body id t
          in
          Type_Rec binderId kind subsBody
    
    Type_Send t1 t2 ->
      let
        subsT1 = subsAux freeVarsT t1 id t
        subsT2 = subsAux freeVarsT t2 id t
      in
      Type_Send subsT1 subsT2
    
    Type_Receive t1 t2 ->
      let
        subsT1 = subsAux freeVarsT t1 id t
        subsT2 = subsAux freeVarsT t2 id t
      in
      Type_Receive subsT1 subsT2
    
    Type_Select t1 t2 ->
      let
        subsT1 = subsAux freeVarsT t1 id t
        subsT2 = subsAux freeVarsT t2 id t
      in
      Type_Select subsT1 subsT2
    
    Type_Branch t1 t2 ->
      let
        subsT1 = subsAux freeVarsT t1 id t
        subsT2 = subsAux freeVarsT t2 id t
      in
      Type_Branch subsT1 subsT2
    
    Type_End ->
      Type_End
    
    Type_LinearFn t1 t2 ->
      let
        subsT1 = subsAux freeVarsT t1 id t
        subsT2 = subsAux freeVarsT t2 id t
      in
      Type_LinearFn subsT1 subsT2
    
    Type_UnrestrictedFn t1 t2 ->
      let
        subsT1 = subsAux freeVarsT t1 id t
        subsT2 = subsAux freeVarsT t2 id t
      in
      Type_UnrestrictedFn subsT1 subsT2
    
    Type_Product t1 t2 ->
      let
        subsT1 = subsAux freeVarsT t1 id t
        subsT2 = subsAux freeVarsT t2 id t
      in
      Type_Product subsT1 subsT2
    
    Type_Unit ->
      Type_Unit
    
    Type_Sum t1 t2 ->
      let
        subsT1 = subsAux freeVarsT t1 id t
        subsT2 = subsAux freeVarsT t2 id t
      in
      Type_Sum subsT1 subsT2
    
    Type_OfCourse t1 ->
      let
        subsT1 = subsAux freeVarsT t1 id t
      in
      Type_OfCourse subsT1
    
    Type_Accept t1 ->
      let
        subsT1 = subsAux freeVarsT t1 id t
      in
      Type_Accept subsT1
    
    Type_Request t1 ->
      let
        subsT1 = subsAux freeVarsT t1 id t
      in
      Type_Request subsT1
    
    Type_Dual t1 ->
      let
        subsT1 = subsAux freeVarsT t1 id t
      in
      Type_Dual subsT1

freeVars : Type -> List Id
freeVars aType =
  case aType of
    Type_Var id ->
      [id]
    
    Type_Rec id _ t ->
      List.filter (\x -> x /= id) (freeVars t)
    
    Type_Send t1 t2 ->
      (freeVars t1) ++ (freeVars t2)
    
    Type_Receive t1 t2 ->
      (freeVars t1) ++ (freeVars t2)
    
    Type_Select t1 t2 ->
      (freeVars t1) ++ (freeVars t2)
    
    Type_Branch t1 t2 ->
      (freeVars t1) ++ (freeVars t2)
    
    Type_End ->
      []
    
    Type_LinearFn t1 t2 ->
      (freeVars t1) ++ (freeVars t2)
    
    Type_UnrestrictedFn t1 t2 ->
      (freeVars t1) ++ (freeVars t2)
    
    Type_Product t1 t2 ->
      (freeVars t1) ++ (freeVars t2)
    
    Type_Unit ->
      []
    
    Type_Sum t1 t2 ->
      (freeVars t1) ++ (freeVars t2)
    
    Type_OfCourse t ->
      freeVars t
    
    Type_Accept t ->
      freeVars t
    
    Type_Request t ->
      freeVars t
    
    Type_Dual t ->
      freeVars t

-- Renomeia id até encontrar um nome que não ocorre em idList (lista de nomes já usados)
findNewId : Id -> List Id -> Id
findNewId id idList =
  if List.member id idList then
    findNewId (id ++ "'") idList
  else
    id



-- DUALIZAÇÃO

-- Resolve as operações de dualização em aType até obter um tipo que não seja da forma dual(A), exceto se A for 
-- uma variável (ou se A não é um tipo de sessão, o que não deve ocorrer se aType é bem-formado).
resolveDualLazy : Type -> Type
resolveDualLazy aType =
  case aType of
    Type_Dual (Type_Var id) ->
      Type_Dual (Type_Var id)
    
    Type_Dual (Type_Rec id kind body) ->
      let
        bodyWithDualizedVar = subs body id (Type_Dual (Type_Var id))
      in
      Type_Rec id kind (Type_Dual bodyWithDualizedVar)
    
    Type_Dual (Type_Send t1 t2) ->
      Type_Receive t1 (Type_Dual t2)
    
    Type_Dual (Type_Receive t1 t2) ->
      Type_Send t1 (Type_Dual t2)
    
    Type_Dual (Type_Select t1 t2) ->
      Type_Branch (Type_Dual t1) (Type_Dual t2)
    
    Type_Dual (Type_Branch t1 t2) ->
      Type_Select (Type_Dual t1) (Type_Dual t2)
    
    Type_Dual (Type_End) ->
      Type_End
    
    Type_Dual (Type_Dual t) ->
      resolveDualLazy t
    
    t -> t

-- Resolve todas as operações de dualização em aType, exceto dual de variável.
resolveDualEager : Type -> Type
resolveDualEager aType =
  case resolveDualLazy aType of
    Type_Var id ->
      Type_Var id
    
    Type_Rec id kind t ->
      Type_Rec id kind (resolveDualEager t)
    
    Type_Send t1 t2 ->
      Type_Send (resolveDualEager t1) (resolveDualEager t2)
    
    Type_Receive t1 t2 ->
      Type_Receive (resolveDualEager t1) (resolveDualEager t2)
    
    Type_Select t1 t2 ->
      Type_Select (resolveDualEager t1) (resolveDualEager t2)
    
    Type_Branch t1 t2 ->
      Type_Branch (resolveDualEager t1) (resolveDualEager t2)
    
    Type_End ->
      Type_End
    
    Type_LinearFn t1 t2 ->
      Type_LinearFn (resolveDualEager t1) (resolveDualEager t2)
    
    Type_UnrestrictedFn t1 t2 ->
      Type_UnrestrictedFn (resolveDualEager t1) (resolveDualEager t2)
    
    Type_Product t1 t2 ->
      Type_Product (resolveDualEager t1) (resolveDualEager t2)
    
    Type_Unit ->
      Type_Unit
    
    Type_Sum t1 t2 ->
      Type_Sum (resolveDualEager t1) (resolveDualEager t2)
    
    Type_OfCourse t ->
      Type_OfCourse (resolveDualEager t)
    
    Type_Accept t ->
      Type_Accept (resolveDualEager t)
    
    Type_Request t ->
      Type_Request (resolveDualEager t)
    
    Type_Dual t ->
      Type_Dual (resolveDualEager t)



-- EQUIVALÊNCIA DE TIPOS

-- Testa se dois tipos são iguais a menos de alfa-equivalência.
typesAreEqual : Type -> Type -> Bool
typesAreEqual type1 type2 =
  case (resolveDualLazy type1, resolveDualLazy type2) of
    (Type_Var id1, Type_Var id2) ->
      id1 == id2
    
    (Type_Rec id1 _ t1, Type_Rec id2 _ t2) ->
      let
        newId = findNewId id1 (freeVars t1 ++ freeVars t2)
        t1VarRenamed = subs t1 id1 (Type_Var newId)
        t2VarRenamed = subs t2 id2 (Type_Var newId)
      in
      typesAreEqual t1VarRenamed t2VarRenamed
    
    (Type_Send ta1 tb1, Type_Send ta2 tb2) ->
      (typesAreEqual ta1 ta2) && (typesAreEqual tb1 tb2)
    
    (Type_Receive ta1 tb1, Type_Receive ta2 tb2) ->
      (typesAreEqual ta1 ta2) && (typesAreEqual tb1 tb2)
    
    (Type_Select ta1 tb1, Type_Select ta2 tb2) ->
      (typesAreEqual ta1 ta2) && (typesAreEqual tb1 tb2)
    
    (Type_Branch ta1 tb1, Type_Branch ta2 tb2) ->
      (typesAreEqual ta1 ta2) && (typesAreEqual tb1 tb2)
    
    (Type_End, Type_End) ->
      True
    
    (Type_LinearFn ta1 tb1, Type_LinearFn ta2 tb2) ->
      (typesAreEqual ta1 ta2) && (typesAreEqual tb1 tb2)
    
    (Type_UnrestrictedFn ta1 tb1, Type_UnrestrictedFn ta2 tb2) ->
      (typesAreEqual ta1 ta2) && (typesAreEqual tb1 tb2)
    
    (Type_Product ta1 tb1, Type_Product ta2 tb2) ->
      (typesAreEqual ta1 ta2) && (typesAreEqual tb1 tb2)
    
    (Type_Unit, Type_Unit) ->
      True
    
    (Type_Sum ta1 tb1, Type_Sum ta2 tb2) ->
      (typesAreEqual ta1 ta2) && (typesAreEqual tb1 tb2)
    
    (Type_OfCourse t1, Type_OfCourse t2) ->
      (typesAreEqual t1 t2)
    
    (Type_Accept t1, Type_Accept t2) ->
      (typesAreEqual t1 t2)
    
    (Type_Request t1, Type_Request t2) ->
      (typesAreEqual t1 t2)
    
    (Type_Dual t1, Type_Dual t2) ->
      (typesAreEqual t1 t2)

    _ -> False



-- INFERÊNCIA DE TIPO DE TERMOS

showType : Type -> String
showType aType = typeToString (resolveDualEager aType)

checkOptionalAnnotation : TypeVarContext -> TypeVarEnv -> Type -> Maybe Type -> Result String Type
checkOptionalAnnotation typeVarContext typeVarEnv t1 maybeType =
  case maybeType of
    Just aType ->
      checkAndApplySubs typeVarContext typeVarEnv aType |> andThen (\(t2, _) ->
      if typesAreEqual t1 t2 then
        Ok t2
      else
        Err ("O tipo declarado é " ++ typeToString aType ++ ", mas o tipo inferido foi " ++ showType t1))
    
    Nothing ->
      Ok t1

typeInferTerm : TypeVarContext -> TypeVarEnv -> TermVarContext -> Term 
  -> Result String (Type, TermVarContext)
typeInferTerm typeVarContext typeVarEnv termVarContext term =
  case term of
    Term_Var id ->
      case lookup id termVarContext of
        Just (t, Unrestricted) ->
          Ok (t, termVarContext)
        
        Just (t, Linear NotUsed) ->
          Ok (t, markAsUsed id termVarContext)

        Just (_ , Linear Used) ->
          Err ("A variável linear " ++ id ++ " foi usada mais de uma vez")

        Just (_ , Linear UsedUnrestrictedly) ->
          Err ("A variável linear " ++ id ++ " está sendo usada de forma irrestrita (zero ou mais vezes)")
        
        Nothing ->
          Err ("A variável " ++ id ++ " não foi declarada")
    
    Term_LinearLambda id varType e ->
      checkAndApplySubs typeVarContext typeVarEnv varType |> andThen (\(t1, _) ->
      let
        extendedContext = (id, (t1, Linear NotUsed)) :: termVarContext
      in
      typeInferTerm typeVarContext typeVarEnv extendedContext e |> andThen (\(t2, outCtx) ->
      if isUsed id outCtx then
        Ok (Type_LinearFn t1 t2, List.drop 1 outCtx)
      else
        Err ("A variável linear " ++ id ++ " não foi usada")))
    
    Term_UnrestrictedLambda id varType e ->
      checkAndApplySubs typeVarContext typeVarEnv varType |> andThen (\(t1, _) ->
      let
        extendedContext = (id, (t1, Unrestricted)) :: termVarContext
      in
      typeInferTerm typeVarContext typeVarEnv extendedContext e |> andThen (\(t2, outCtx) ->
      Ok (Type_UnrestrictedFn t1 t2, List.drop 1 outCtx)))
    
    Term_Application e1 e2 ->
      typeInferTerm typeVarContext typeVarEnv termVarContext e1 |> andThen (\(t1, outCtx1) ->
      case resolveDualLazy t1 of
        Type_LinearFn ta tb ->
          typeInferTerm typeVarContext typeVarEnv outCtx1 e2 |> andThen (\(t2, outCtx2) ->
          if typesAreEqual t2 ta
          then Ok (tb, outCtx2)
          else
            Err ("Uma expressão do tipo " ++ showType ta ++ " era esperada, mas o tipo inferido foi "
              ++ showType t2))
        
        Type_UnrestrictedFn ta tb ->
          typeInferTerm typeVarContext typeVarEnv (markAsUsedUnrestrictedly termVarContext) e2
            |> andThen (\(t2, _) ->
          if typesAreEqual t2 ta
          then Ok (tb, outCtx1)
          else
            Err ("Uma expressão do tipo " ++ showType ta ++ " era esperada, mas o tipo inferido foi "
              ++ showType t2))

        _ -> Err ("Uma expressão de um tipo função era esperada, mas o tipo inferido foi " ++ showType t1))
    
    Term_Pair e1 e2 ->
      typeInferTerm typeVarContext typeVarEnv termVarContext e1 |> andThen (\(t1, outCtx1) ->
      typeInferTerm typeVarContext typeVarEnv outCtx1 e2 |> andThen (\(t2, outCtx2) ->
      Ok (Type_Product t1 t2, outCtx2)))
    
    Term_LetPair id1 id2 e1 e2 ->
      if id1 == id2 then
        Err "Os nomes das duas variáveis introduzidas pelo desconstrutor de par não podem ser iguais"
      else
      typeInferTerm typeVarContext typeVarEnv termVarContext e1 |> andThen (\(t1, outCtx1) ->
      case resolveDualLazy t1 of
        Type_Product ta tb ->
          let
            extendedContext = (id1, (ta, Linear NotUsed)) :: (id2, (tb, Linear NotUsed)) :: outCtx1
          in
          typeInferTerm typeVarContext typeVarEnv extendedContext e2 |> andThen (\(t2, outCtx2) ->
          if not (isUsed id1 outCtx2) then
            Err ("A variável linear " ++ id1 ++ " não foi usada")
          else if not (isUsed id2 outCtx2) then
            Err ("A variável linear " ++ id2 ++ " não foi usada")
          else
            Ok (t2, List.drop 2 outCtx2))
        
        _ ->
          Err ("Uma expressão de um tipo produto era esperada, mas o tipo inferido foi " ++ showType t1))
    
    Term_Unit ->
      Ok (Type_Unit, termVarContext)
    
    Term_LetUnit e1 e2 ->
      typeInferTerm typeVarContext typeVarEnv termVarContext e1 |> andThen (\(t1, outCtx1) ->
      case resolveDualLazy t1 of
        Type_Unit ->
          typeInferTerm typeVarContext typeVarEnv outCtx1 e2 |> andThen (\(t2, outCtx2) ->
          Ok (t2, outCtx2))
        
        _ ->
          Err ("Uma expressão do tipo 1 era esperada, mas o tipo inferido foi " ++ showType t1))
    
    Term_Inl type2 e ->
      checkAndApplySubs typeVarContext typeVarEnv type2 |> andThen (\(t2, _) ->
      typeInferTerm typeVarContext typeVarEnv termVarContext e |> andThen (\(t1, outCtx) ->
      Ok (Type_Sum t1 t2, outCtx)))
    
    Term_Inr type1 e ->
      checkAndApplySubs typeVarContext typeVarEnv type1 |> andThen (\(t1, _) ->
      typeInferTerm typeVarContext typeVarEnv termVarContext e |> andThen (\(t2, outCtx) ->
      Ok (Type_Sum t1 t2, outCtx)))

    Term_Case e id1 e1 id2 e2 ->
      typeInferTerm typeVarContext typeVarEnv termVarContext e |> andThen (\(t, outCtx) ->
      case resolveDualLazy t of
        Type_Sum ta tb ->
          let
            extendedContext1 = (id1, (ta, Linear NotUsed)) :: outCtx
            extendedContext2 = (id2, (tb, Linear NotUsed)) :: outCtx
          in
          typeInferTerm typeVarContext typeVarEnv extendedContext1 e1 |> andThen (\(t1, outCtx1) ->
          typeInferTerm typeVarContext typeVarEnv extendedContext2 e2 |> andThen (\(t2, outCtx2) ->
          if not (isUsed id1 outCtx1) then
            Err ("A variável linear " ++ id1 ++ " não foi usada")
          else if not (isUsed id2 outCtx2) then
            Err ("A variável linear " ++ id2 ++ " não foi usada")
          else if not (typesAreEqual t1 t2) then
            Err ("Os tipos de ambos os ramos de uma expressão case devem ser iguais, mas o primeiro ramo é "
              ++ "do tipo " ++ showType t1 ++ ", enquanto o segundo ramo é do tipo " ++ showType t2)
          else
          let
            outCtx1VarRemoved = List.drop 1 outCtx1
            outCtx2VarRemoved = List.drop 1 outCtx2
          in
          if outCtx1VarRemoved == outCtx2VarRemoved then
            Ok (t1, outCtx1VarRemoved)
          else
            Err ("Alguma variável linear está sendo usada em um dos ramos da expressão case, "
              ++ "mas não no outro")))
        
        _ ->
          Err ("Uma expressão de um tipo soma era esperada, mas o tipo inferido foi " ++ showType t))
    
    Term_OfCourse e ->
      typeInferTerm typeVarContext typeVarEnv (markAsUsedUnrestrictedly termVarContext) e
        |> andThen (\(aType, _) ->
      Ok (Type_OfCourse aType, termVarContext))
    
    Term_LetOfCourse id e1 e2 ->
      typeInferTerm typeVarContext typeVarEnv termVarContext e1 |> andThen (\(t1, outCtx1) ->
      case resolveDualLazy t1 of
        Type_OfCourse ta ->
          let
            extendedContext = (id, (ta, Unrestricted)) :: outCtx1
          in
          typeInferTerm typeVarContext typeVarEnv extendedContext e2 |> andThen (\(t2, outCtx2) ->
          Ok (t2, List.drop 1 outCtx2))
        
        _ ->
          Err ("Uma expressão de um tipo \"of course\" era esperada, mas o tipo inferido foi " ++ showType t1))
    
    Term_Send e1 e2 ->
      typeInferTerm typeVarContext typeVarEnv termVarContext e1 |> andThen (\(t1, outCtx1) ->
      typeInferTerm typeVarContext typeVarEnv outCtx1 e2 |> andThen (\(t2, outCtx2) ->
      case resolveDualLazy t2 of
        Type_Send ta tb ->
          if typesAreEqual t1 ta
          then Ok (tb, outCtx2)
          else
            Err ("Uma expressão do tipo " ++ showType ta ++ " era esperada, mas o tipo inferido foi "
              ++ showType t1)

        _ -> Err ("Uma expressão de um tipo send era esperada, mas o tipo inferido foi " ++ showType t2)))
    
    Term_Receive e ->
      typeInferTerm typeVarContext typeVarEnv termVarContext e |> andThen (\(t, outCtx) ->
      case resolveDualLazy t of
        Type_Receive ta tb ->
          Ok (Type_Product ta tb, outCtx)

        _ -> Err ("Uma expressão de um tipo receive era esperada, mas o tipo inferido foi " ++ showType t))
    
    Term_SelectLeft e ->
      typeInferTerm typeVarContext typeVarEnv termVarContext e |> andThen (\(t, outCtx) ->
      case resolveDualLazy t of
        Type_Select ta tb ->
          Ok (ta, outCtx)

        _ -> Err ("Uma expressão de um tipo select era esperada, mas o tipo inferido foi " ++ showType t))
    
    Term_SelectRight e ->
      typeInferTerm typeVarContext typeVarEnv termVarContext e |> andThen (\(t, outCtx) ->
      case resolveDualLazy t of
        Type_Select ta tb ->
          Ok (tb, outCtx)

        _ -> Err ("Uma expressão de um tipo select era esperada, mas o tipo inferido foi " ++ showType t))

    Term_Branch e id1 e1 id2 e2 ->
      typeInferTerm typeVarContext typeVarEnv termVarContext e |> andThen (\(t, outCtx) ->
      case resolveDualLazy t of
        Type_Branch ta tb ->
          let
            extendedContext1 = (id1, (ta, Linear NotUsed)) :: outCtx
            extendedContext2 = (id2, (tb, Linear NotUsed)) :: outCtx
          in
          typeInferTerm typeVarContext typeVarEnv extendedContext1 e1 |> andThen (\(t1, outCtx1) ->
          typeInferTerm typeVarContext typeVarEnv extendedContext2 e2 |> andThen (\(t2, outCtx2) ->
          if not (isUsed id1 outCtx1) then
            Err ("A variável linear " ++ id1 ++ " não foi usada")
          else if not (isUsed id2 outCtx2) then
            Err ("A variável linear " ++ id2 ++ " não foi usada")
          else if not (typesAreEqual t1 t2) then
            Err ("Os tipos de ambos os ramos de uma expressão branch devem ser iguais, mas o primeiro ramo é "
              ++ "do tipo " ++ showType t1 ++ ", enquanto o segundo ramo é do tipo " ++ showType t2)
          else
          let
            outCtx1VarRemoved = List.drop 1 outCtx1
            outCtx2VarRemoved = List.drop 1 outCtx2
          in
          if outCtx1VarRemoved == outCtx2VarRemoved then
            Ok (t1, outCtx1VarRemoved)
          else
            Err ("Alguma variável linear está sendo usada em um dos ramos da expressão branch, "
              ++ "mas não no outro")))
        
        _ ->
          Err ("Uma expressão de um tipo branch era esperada, mas o tipo inferido foi " ++ showType t))
    
    Term_Close e ->
      typeInferTerm typeVarContext typeVarEnv termVarContext e |> andThen (\(t, outCtx) ->
      case resolveDualLazy t of
        Type_End ->
          Ok (Type_Unit, outCtx)

        _ -> Err ("Uma expressão do tipo End era esperada, mas o tipo inferido foi " ++ showType t))
    
    Term_NewAccess aType id1 id2 e ->
      if id1 == id2 then
        Err "Os nomes das duas variáveis introduzidas por \"new access\" não podem ser iguais"
      else
      checkAndApplySubs typeVarContext typeVarEnv aType |> andThen (\(t, kind) ->
      if kind /= Kind_SessionType then
        Err ("O tipo fornecido para a operação \"new access\" deve ser um tipo de sessão, mas o tipo fornecido "
          ++ "foi " ++ typeToString aType ++ ", que é do kind *ns")
      else
      let
        extendedContext = (id1, (Type_Accept t, Unrestricted))
                       :: (id2, (Type_Request (Type_Dual t), Unrestricted))
                       :: termVarContext
      in
      typeInferTerm typeVarContext typeVarEnv extendedContext e |> andThen (\(t1, outCtx1) ->
      Ok (t1, List.drop 2 outCtx1)))
    
    Term_Accept e ->
      typeInferTerm typeVarContext typeVarEnv termVarContext e |> andThen (\(t, outCtx) ->
      case resolveDualLazy t of
        Type_Accept ta ->
          Ok (ta, outCtx)

        _ -> Err ("Uma expressão de um tipo Accept era esperada, mas o tipo inferido foi " ++ showType t))
    
    Term_Request e ->
      typeInferTerm typeVarContext typeVarEnv termVarContext e |> andThen (\(t, outCtx) ->
      case resolveDualLazy t of
        Type_Request ta ->
          Ok (ta, outCtx)

        _ -> Err ("Uma expressão de um tipo Request era esperada, mas o tipo inferido foi " ++ showType t))
    
    Term_NewSession aType ->
      checkAndApplySubs typeVarContext typeVarEnv aType |> andThen (\(t, kind) ->
      if kind /= Kind_SessionType then
        Err ("O tipo fornecido para a operação \"new session\" deve ser um tipo de sessão, mas o tipo fornecido "
          ++ "foi " ++ typeToString aType ++ ", que é do kind *ns")
      else
      Ok (Type_Product t (Type_Dual t), termVarContext))
    
    Term_Fork e ->
      typeInferTerm typeVarContext typeVarEnv termVarContext e |> andThen (\(t, outCtx) ->
      case resolveDualLazy t of
        Type_LinearFn ta Type_Unit ->
          checkType typeVarContext ta |> andThen (\kind ->
          if kind /= Kind_SessionType then
            Err ("O parâmetro da função fornecida para a operação \"fork\" deve ser de um tipo de sessão, mas "
              ++ "o tipo inferido foi " ++ showType ta ++ ", que é do kind *ns")
          else
            Ok (Type_Dual ta, outCtx))

        _ -> Err ("Uma expressão de um tipo da forma A -o 1, onde A é um tipo de sessão, era esperada, mas o "
               ++ "tipo inferido foi " ++ showType t))
    
    Term_Spawn e ->
      typeInferTerm typeVarContext typeVarEnv termVarContext e |> andThen (\(t, outCtx) ->
      case resolveDualLazy t of
        Type_Unit ->
          Ok (Type_Unit, outCtx)

        _ -> Err ("Uma expressão do tipo 1 era esperada, mas o tipo inferido foi " ++ showType t))
    
    Term_Fold aType e ->
      checkAndApplySubs typeVarContext typeVarEnv aType |> andThen (\(t, _) ->
      case t of
        Type_Rec id _ ta ->
          typeInferTerm typeVarContext typeVarEnv termVarContext e |> andThen (\(t1, outCtx1) ->
          let
            unfoldedType = subs ta id t
          in
          if typesAreEqual t1 unfoldedType then
            Ok (t, outCtx1)
          else
            Err ("Uma expressão do tipo " ++ showType unfoldedType ++ " era esperada, mas o tipo inferido foi "
              ++ showType t1))
        
        _ ->
          Err "O tipo fornecido na operação de fold deve ser um tipo recursivo")
    
    Term_Unfold e ->
      typeInferTerm typeVarContext typeVarEnv termVarContext e |> andThen (\(t, outCtx) ->
      case resolveDualLazy t of
        Type_Rec id _ ta ->
          let
            unfoldedType = subs ta id t
          in
          Ok (unfoldedType, outCtx)

        _ -> Err ("Uma expressão de um tipo recursivo era esperada, mas o tipo inferido foi " ++ showType t))
    
    Term_LetLin id maybeType e1 e2 ->
      typeInferTerm typeVarContext typeVarEnv termVarContext e1 |> andThen (\(t1, outCtx1) ->
      checkOptionalAnnotation typeVarContext typeVarEnv t1 maybeType
        |> mapError (\e -> "Na definição de " ++ id ++ ": " ++ e)
        |> andThen (\t ->
      let
        extendedContext = (id, (t, Linear NotUsed)) :: outCtx1
      in
      typeInferTerm typeVarContext typeVarEnv extendedContext e2 |> andThen (\(t2, outCtx2) ->
      if isUsed id outCtx2 then
        Ok (t2, List.drop 1 outCtx2)
      else
        Err ("A variável linear " ++ id ++ " não foi usada"))))
    
    Term_LetUn id maybeType e1 e2 ->
      typeInferTerm typeVarContext typeVarEnv (markAsUsedUnrestrictedly termVarContext) e1
        |> andThen (\(t1, _) ->
      checkOptionalAnnotation typeVarContext typeVarEnv t1 maybeType
        |> mapError (\e -> "Na definição de " ++ id ++ ": " ++ e)
        |> andThen (\t ->
      let
        extendedContext = (id, (t, Unrestricted)) :: termVarContext
      in
      typeInferTerm typeVarContext typeVarEnv extendedContext e2 |> andThen (\(t2, outCtx2) ->
      Ok (t2, List.drop 1 outCtx2))))
    
    Term_LetRec id aType e1 e2 ->
      checkAndApplySubs typeVarContext typeVarEnv aType
        |> mapError (\e -> "Na definição de " ++ id ++ ": " ++ e)
        |> andThen (\(t, _) ->
      let
        extendedContext = (id, (t, Unrestricted)) :: termVarContext
      in
      typeInferTerm typeVarContext typeVarEnv (markAsUsedUnrestrictedly extendedContext) e1
        |> andThen (\(t1, _) ->
      if not (typesAreEqual t1 t) then
        Err ("Na definição de " ++ id ++ ": O tipo declarado é " ++ typeToString aType ++ ", mas o tipo "
          ++ "inferido foi " ++ showType t1)
      else
      typeInferTerm typeVarContext typeVarEnv extendedContext e2 |> andThen (\(t2, outCtx2) ->
      Ok (t2, List.drop 1 outCtx2))))