module Evaluation exposing
  ( Value(..)
  , valueToString
  , Configuration
  , EvalStatus(..)
  , initialConfig
  , step
  )

import AbstractSyntax exposing (..)
import Utils exposing (..)
import Random exposing (Seed)
import Random.List


type alias Env = List (Id, EnvEntry)
type EnvEntry = ValueEntry Value | ClosureEntry Term Env

type Value
  = Value_ChannelName ChannelId
  | Value_LinearFn Id Term Env
  | Value_UnrestrictedFn Id Term Env
  | Value_Pair Value Value
  | Value_Unit
  | Value_Inl Value
  | Value_Inr Value
  | Value_OfCourse Term Env
  | Value_Fold Value

type EvaluationContext 
  = EC_Empty
  | EC_App_Left Term Env EvaluationContext
  | EC_LinApp_Right Value EvaluationContext
  | EC_Pair_Left Term Env EvaluationContext
  | EC_Pair_Right Value EvaluationContext
  | EC_LetPair Id Id Term Env EvaluationContext
  | EC_LetUnit Term Env EvaluationContext
  | EC_Inl EvaluationContext
  | EC_Inr EvaluationContext
  | EC_Case Id Term Id Term Env EvaluationContext
  | EC_LetOfCourse Id Term Env EvaluationContext
  | EC_Send_Left Term Env EvaluationContext
  | EC_Send_Right Value EvaluationContext
  | EC_Receive EvaluationContext
  | EC_SelectLeft EvaluationContext
  | EC_SelectRight EvaluationContext
  | EC_Branch Id Term Id Term Env EvaluationContext
  | EC_Close EvaluationContext
  | EC_Accept EvaluationContext
  | EC_Request EvaluationContext
  | EC_Fork EvaluationContext
  | EC_Fold EvaluationContext
  | EC_Unfold EvaluationContext
  | EC_LetLin Id Term Env EvaluationContext

type PotentialRedex
  = Pot_Var Id
  | Pot_LinApp Value Value
  | Pot_UnApp Value Term Env
  | Pot_LetPair Id Id Value Term Env
  | Pot_LetUnit Value Term Env
  | Pot_Case Value Id Term Id Term Env
  | Pot_LetOfCourse Id Value Term Env
  | Pot_Unfold Value
  | Pot_LetLin Id Value Term Env
  | Pot_LetUn Id Term Term Env
  | Pot_LetRec Id Term Term Env
  | Pot_Send Value Value
  | Pot_Receive Value
  | Pot_SelectLeft Value
  | Pot_SelectRight Value
  | Pot_Branch Value Id Term Id Term Env
  | Pot_Close Value
  | Pot_NewAccess Id Id Term Env
  | Pot_Accept Value
  | Pot_Request Value
  | Pot_NewSession
  | Pot_Fork Value
  | Pot_Spawn Term Env

type Flag = Main | Child

type Thread = Thread Flag EvaluationContext Term Env

type FocusedThread = FocusedThread Flag EvaluationContext PotentialRedex

type alias ChannelQueue = List FocusedThread

type alias ChannelId = Int

type alias Configuration = 
  { channels      : List (ChannelId, ChannelQueue)
  , threads       : List Thread
  , nextChannelId : ChannelId
  , returnValue   : Maybe Value
  }

valueToString : Value -> String
valueToString value =
  case value of
    Value_ChannelName _ ->
      "<nome de canal>"
    
    Value_LinearFn _ _ _ ->
      "<função linear>"
    
    Value_UnrestrictedFn _ _ _ ->
      "<função irrestrita>"
    
    Value_Pair v1 v2 ->
      "{" ++ valueToString v1 ++ ", " ++ valueToString v2 ++ "}"
    
    Value_Unit ->
      "{}"
    
    Value_Inl v ->
      "inl (" ++ valueToString v ++ ")"
    
    Value_Inr v ->
      "inr (" ++ valueToString v ++ ")"
    
    Value_OfCourse _ _ ->
      "<clausura of course>"
    
    Value_Fold v ->
      "fold (" ++ valueToString v ++ ")"


type ValueOrFocus = Val Value | Focus EvaluationContext PotentialRedex

refocus : Term -> Env -> EvaluationContext -> ValueOrFocus
refocus term env ec =
  case term of
    Term_Var id ->
      case lookup id env of
        Just (ValueEntry v) ->
          refocusAux ec v
        
        Just (ClosureEntry e env1) ->
          refocus e env1 ec
        
        Nothing ->
          Focus ec (Pot_Var id)
    
    Term_LinearLambda id _ e ->
      refocusAux ec (Value_LinearFn id e env)
    
    Term_UnrestrictedLambda id _ e ->
      refocusAux ec (Value_UnrestrictedFn id e env)
    
    Term_Application e1 e2 ->
      refocus e1 env (EC_App_Left e2 env ec)
    
    Term_Pair e1 e2 ->
      refocus e1 env (EC_Pair_Left e2 env ec)
    
    Term_LetPair id1 id2 e1 e2 ->
      refocus e1 env (EC_LetPair id1 id2 e2 env ec)
    
    Term_Unit ->
      refocusAux ec Value_Unit
    
    Term_LetUnit e1 e2 ->
      refocus e1 env (EC_LetUnit e2 env ec)
    
    Term_Inl _ e ->
      refocus e env (EC_Inl ec)
    
    Term_Inr _ e ->
      refocus e env (EC_Inr ec)
    
    Term_Case e id1 e1 id2 e2 ->
      refocus e env (EC_Case id1 e1 id2 e2 env ec)
    
    Term_OfCourse e ->
      refocusAux ec (Value_OfCourse e env)
    
    Term_LetOfCourse id e1 e2 ->
      refocus e1 env (EC_LetOfCourse id e2 env ec)
    
    Term_Send e1 e2 ->
      refocus e1 env (EC_Send_Left e2 env ec)
    
    Term_Receive e ->
      refocus e env (EC_Receive ec)
    
    Term_SelectLeft e ->
      refocus e env (EC_SelectLeft ec)
    
    Term_SelectRight e ->
      refocus e env (EC_SelectRight ec)
    
    Term_Branch e id1 e1 id2 e2 ->
      refocus e env (EC_Branch id1 e1 id2 e2 env ec)
    
    Term_Close e ->
      refocus e env (EC_Close ec)
    
    Term_NewAccess _ id1 id2 e ->
      Focus ec (Pot_NewAccess id1 id2 e env)
    
    Term_Accept e ->
      refocus e env (EC_Accept ec)
    
    Term_Request e ->
      refocus e env (EC_Request ec)
    
    Term_NewSession _ ->
      Focus ec (Pot_NewSession)
    
    Term_Fork e ->
      refocus e env (EC_Fork ec)
    
    Term_Spawn e ->
      Focus ec (Pot_Spawn e env)
    
    Term_Fold _ e ->
      refocus e env (EC_Fold ec)
    
    Term_Unfold e ->
      refocus e env (EC_Unfold ec)
    
    Term_LetLin id _ e1 e2 ->
      refocus e1 env (EC_LetLin id e2 env ec)
    
    Term_LetUn id _ e1 e2 ->
      Focus ec (Pot_LetUn id e1 e2 env)
    
    Term_LetRec id _ e1 e2 ->
      Focus ec (Pot_LetRec id e1 e2 env)

refocusAux : EvaluationContext -> Value -> ValueOrFocus
refocusAux evalCtx v =
  case evalCtx of
    EC_Empty ->
      Val v
    
    EC_App_Left e2 env ec ->
      case v of
        Value_LinearFn _ _ _ ->
          refocus e2 env (EC_LinApp_Right v ec)
        
        _ ->
          Focus ec (Pot_UnApp v e2 env)
    
    EC_LinApp_Right v1 ec ->
      Focus ec (Pot_LinApp v1 v)
    
    EC_Pair_Left e2 env ec ->
      refocus e2 env (EC_Pair_Right v ec)
    
    EC_Pair_Right v1 ec ->
      refocusAux ec (Value_Pair v1 v)
    
    EC_LetPair id1 id2 e2 env ec ->
      Focus ec (Pot_LetPair id1 id2 v e2 env)
    
    EC_LetUnit e2 env ec ->
      Focus ec (Pot_LetUnit v e2 env)
    
    EC_Inl ec ->
      refocusAux ec (Value_Inl v)
    
    EC_Inr ec ->
      refocusAux ec (Value_Inr v)
    
    EC_Case id1 e1 id2 e2 env ec ->
      Focus ec (Pot_Case v id1 e1 id2 e2 env)
    
    EC_LetOfCourse id e2 env ec ->
      Focus ec (Pot_LetOfCourse id v e2 env)
    
    EC_Send_Left e2 env ec ->
      refocus e2 env (EC_Send_Right v ec)
    
    EC_Send_Right v1 ec ->
      Focus ec (Pot_Send v1 v)
    
    EC_Receive ec ->
      Focus ec (Pot_Receive v)
    
    EC_SelectLeft ec ->
      Focus ec (Pot_SelectLeft v)
    
    EC_SelectRight ec ->
      Focus ec (Pot_SelectRight v)
    
    EC_Branch id1 e1 id2 e2 env ec ->
      Focus ec (Pot_Branch v id1 e1 id2 e2 env)
    
    EC_Close ec ->
      Focus ec (Pot_Close v)
    
    EC_Accept ec ->
      Focus ec (Pot_Accept v)
    
    EC_Request ec ->
      Focus ec (Pot_Request v)
    
    EC_Fork ec ->
      Focus ec (Pot_Fork v)
    
    EC_Fold ec ->
      refocusAux ec (Value_Fold v)
    
    EC_Unfold ec ->
      Focus ec (Pot_Unfold v)
    
    EC_LetLin id e2 env ec ->
      Focus ec (Pot_LetLin id v e2 env)


reduce : Thread -> Configuration -> Result () Configuration
reduce (Thread flag evalCtx term environment) config =
  case refocus term environment evalCtx of
    Val v ->
      case flag of
        Main  -> Ok { config | returnValue = Just v }
        Child -> Ok config
    
    Focus ec potRed ->
      case potRed of
        Pot_Var _ ->
          Err ()
        
        Pot_LinApp (Value_LinearFn id e1 env1) v2 ->
          let
            extendedEnv = (id, ValueEntry v2) :: env1
            thread = Thread flag ec e1 extendedEnv
          in
          Ok { config | threads = thread :: config.threads }
        
        Pot_UnApp (Value_UnrestrictedFn id e1 env1) e2 env ->
          let
            extendedEnv = (id, ClosureEntry e2 env) :: env1
            thread = Thread flag ec e1 extendedEnv
          in
          Ok { config | threads = thread :: config.threads }
        
        Pot_LetPair id1 id2 (Value_Pair v1 v2) e2 env ->
          let
            extendedEnv = (id1, ValueEntry v1) :: (id2, ValueEntry v2) :: env
            thread = Thread flag ec e2 extendedEnv
          in
          Ok { config | threads = thread :: config.threads }
        
        Pot_LetUnit Value_Unit e2 env ->
          let
            thread = Thread flag ec e2 env
          in
          Ok { config | threads = thread :: config.threads }
        
        Pot_Case (Value_Inl v1) id1 e1 _ _ env ->
          let
            extendedEnv = (id1, ValueEntry v1) :: env
            thread = Thread flag ec e1 extendedEnv
          in
          Ok { config | threads = thread :: config.threads }
            
        Pot_Case (Value_Inr v2) _ _ id2 e2 env ->
          let
            extendedEnv = (id2, ValueEntry v2) :: env
            thread = Thread flag ec e2 extendedEnv
          in
          Ok { config | threads = thread :: config.threads }
        
        Pot_LetOfCourse id (Value_OfCourse e1 env1) e2 env ->
          let
            extendedEnv = (id, ClosureEntry e1 env1) :: env
            thread = Thread flag ec e2 extendedEnv
          in
          Ok { config | threads = thread :: config.threads }
        
        Pot_Unfold (Value_Fold v1) ->
          let
            thread = Thread flag ec (Term_Var "x") [("x", ValueEntry v1)]
          in
          Ok { config | threads = thread :: config.threads }
        
        Pot_Unfold (Value_ChannelName chId) ->
          let
            thread = Thread flag ec (Term_Var "x") [("x", ValueEntry (Value_ChannelName chId))]
          in
          Ok { config | threads = thread :: config.threads }
        
        Pot_LetLin id v1 e2 env ->
          let
            extendedEnv = (id, ValueEntry v1) :: env
            thread = Thread flag ec e2 extendedEnv
          in
          Ok { config | threads = thread :: config.threads }
        
        Pot_LetUn id e1 e2 env ->
          let
            extendedEnv = (id, ClosureEntry e1 env) :: env
            thread = Thread flag ec e2 extendedEnv
          in
          Ok { config | threads = thread :: config.threads }
        
        Pot_LetRec id e1 e2 env ->
          let
            dummyType = Type_Unit
            extendedEnv = (id, ClosureEntry (Term_LetRec id dummyType e1 e1) env) :: env
            thread = Thread flag ec e2 extendedEnv
          in
          Ok { config | threads = thread :: config.threads }
        
        Pot_Send v (Value_ChannelName chId) ->
          case lookup chId config.channels of
            Just ((FocusedThread flag2 ec2 (Pot_Receive _)) :: xs) ->
              let
                value1  = Value_ChannelName chId
                value2  = Value_Pair v (Value_ChannelName chId)
                thread1 = Thread flag  ec  (Term_Var "x") [("x", ValueEntry value1)]
                thread2 = Thread flag2 ec2 (Term_Var "x") [("x", ValueEntry value2)]
              in
              Ok { config | channels = update chId xs config.channels
                          , threads  = thread1 :: thread2 :: config.threads }
            
            Just xs ->
              let
                thread = FocusedThread flag ec (Pot_Send v (Value_ChannelName chId))
              in
              Ok { config | channels = update chId (xs ++ [thread]) config.channels }
            
            Nothing -> Err ()
        
        Pot_Receive (Value_ChannelName chId) ->
          case lookup chId config.channels of
            Just ((FocusedThread flag2 ec2 (Pot_Send v _)) :: xs) ->
              let
                value1  = Value_Pair v (Value_ChannelName chId)
                value2  = Value_ChannelName chId
                thread1 = Thread flag  ec  (Term_Var "x") [("x", ValueEntry value1)]
                thread2 = Thread flag2 ec2 (Term_Var "x") [("x", ValueEntry value2)]
              in
              Ok { config | channels = update chId xs config.channels
                          , threads  = thread1 :: thread2 :: config.threads }
            
            Just xs ->
              let
                thread = FocusedThread flag ec (Pot_Receive (Value_ChannelName chId))
              in
              Ok { config | channels = update chId (xs ++ [thread]) config.channels }

            Nothing -> Err ()
        
        Pot_SelectLeft (Value_ChannelName chId) ->
          case lookup chId config.channels of
            Just ((FocusedThread flag2 ec2 (Pot_Branch _ id1 e1 _ _ env)) :: xs) ->
              let
                value       = Value_ChannelName chId
                extendedEnv = (id1, ValueEntry value) :: env
                thread1     = Thread flag  ec  (Term_Var "x") [("x", ValueEntry value)]
                thread2     = Thread flag2 ec2 e1 extendedEnv
              in
              Ok { config | channels = update chId xs config.channels
                          , threads  = thread1 :: thread2 :: config.threads }
            
            Just xs ->
              let
                thread = FocusedThread flag ec (Pot_SelectLeft (Value_ChannelName chId))
              in
              Ok { config | channels = update chId (xs ++ [thread]) config.channels }

            Nothing -> Err ()
        
        Pot_SelectRight (Value_ChannelName chId) ->
          case lookup chId config.channels of
            Just ((FocusedThread flag2 ec2 (Pot_Branch _ _ _ id2 e2 env)) :: xs) ->
              let
                value       = Value_ChannelName chId
                extendedEnv = (id2, ValueEntry value) :: env
                thread1     = Thread flag  ec  (Term_Var "x") [("x", ValueEntry value)]
                thread2     = Thread flag2 ec2 e2 extendedEnv
              in
              Ok { config | channels = update chId xs config.channels
                          , threads  = thread1 :: thread2 :: config.threads }
            
            Just xs ->
              let
                thread = FocusedThread flag ec (Pot_SelectRight (Value_ChannelName chId))
              in
              Ok { config | channels = update chId (xs ++ [thread]) config.channels }

            Nothing -> Err ()
        
        Pot_Branch (Value_ChannelName chId) id1 e1 id2 e2 env ->
          case lookup chId config.channels of
            Just ((FocusedThread flag2 ec2 (Pot_SelectLeft _)) :: xs) ->
              let
                value       = Value_ChannelName chId
                extendedEnv = (id1, ValueEntry value) :: env
                thread1     = Thread flag  ec  e1 extendedEnv
                thread2     = Thread flag2 ec2 (Term_Var "x") [("x", ValueEntry value)]
              in
              Ok { config | channels = update chId xs config.channels
                          , threads  = thread1 :: thread2 :: config.threads }

            Just ((FocusedThread flag2 ec2 (Pot_SelectRight _)) :: xs) ->
              let
                value       = Value_ChannelName chId
                extendedEnv = (id2, ValueEntry value) :: env
                thread1     = Thread flag  ec  e2 extendedEnv
                thread2     = Thread flag2 ec2 (Term_Var "x") [("x", ValueEntry value)]
              in
              Ok { config | channels = update chId xs config.channels
                          , threads  = thread1 :: thread2 :: config.threads }
            
            Just xs ->
              let
                thread = FocusedThread flag ec (Pot_Branch (Value_ChannelName chId) id1 e1 id2 e2 env)
              in
              Ok { config | channels = update chId (xs ++ [thread]) config.channels }

            Nothing -> Err ()
        
        Pot_Close (Value_ChannelName chId) ->
          case lookup chId config.channels of
            Just ((FocusedThread flag2 ec2 (Pot_Close _)) :: _) ->
              let
                thread1 = Thread flag  ec  (Term_Var "x") [("x", ValueEntry Value_Unit)]
                thread2 = Thread flag2 ec2 (Term_Var "x") [("x", ValueEntry Value_Unit)]
              in
              Ok { config | channels = delete chId config.channels
                          , threads  = thread1 :: thread2 :: config.threads }
            
            Just xs ->
              let
                thread = FocusedThread flag ec (Pot_Close (Value_ChannelName chId))
              in
              Ok { config | channels = update chId (xs ++ [thread]) config.channels }

            Nothing -> Err ()
        
        Pot_NewAccess id1 id2 e env ->
          let
            newChId     = config.nextChannelId
            value       = Value_ChannelName newChId
            extendedEnv = (id1, ValueEntry value) :: (id2, ValueEntry value) :: env
            thread      = Thread flag ec e extendedEnv
          in
          Ok { config | channels      = (newChId, []) :: config.channels
                      , threads       = thread :: config.threads
                      , nextChannelId = config.nextChannelId + 1         }
        
        Pot_Accept (Value_ChannelName chId) ->
          case lookup chId config.channels of
            Just ((FocusedThread flag2 ec2 (Pot_Request _)) :: xs) ->
              let
                newChId = config.nextChannelId
                value   = Value_ChannelName newChId
                thread1 = Thread flag  ec  (Term_Var "x") [("x", ValueEntry value)]
                thread2 = Thread flag2 ec2 (Term_Var "x") [("x", ValueEntry value)]
              in
              Ok { config | channels      = (newChId, []) :: (update chId xs config.channels)
                          , threads       = thread1 :: thread2 :: config.threads
                          , nextChannelId = config.nextChannelId + 1                          }
            
            Just xs ->
              let
                thread = FocusedThread flag ec (Pot_Accept (Value_ChannelName chId))
              in
              Ok { config | channels = update chId (xs ++ [thread]) config.channels }
            
            Nothing -> Err ()
        
        Pot_Request (Value_ChannelName chId) ->
          case lookup chId config.channels of
            Just ((FocusedThread flag2 ec2 (Pot_Accept _)) :: xs) ->
              let
                newChId = config.nextChannelId
                value   = Value_ChannelName newChId
                thread1 = Thread flag  ec  (Term_Var "x") [("x", ValueEntry value)]
                thread2 = Thread flag2 ec2 (Term_Var "x") [("x", ValueEntry value)]
              in
              Ok { config | channels      = (newChId, []) :: (update chId xs config.channels)
                          , threads       = thread1 :: thread2 :: config.threads
                          , nextChannelId = config.nextChannelId + 1                          }
            
            Just xs ->
              let
                thread = FocusedThread flag ec (Pot_Request (Value_ChannelName chId))
              in
              Ok { config | channels = update chId (xs ++ [thread]) config.channels }
            
            Nothing -> Err ()
        
        Pot_NewSession ->
          let
            newChId = config.nextChannelId
            value   = Value_Pair (Value_ChannelName newChId) (Value_ChannelName newChId)
            thread  = Thread flag ec (Term_Var "x") [("x", ValueEntry value)]
          in
          Ok { config | channels      = (newChId, []) :: config.channels
                      , threads       = thread :: config.threads
                      , nextChannelId = config.nextChannelId + 1         }
        
        Pot_Fork (Value_LinearFn id e env) ->
          let
            newChId     = config.nextChannelId
            value       = Value_ChannelName newChId
            extendedEnv = (id, ValueEntry value) :: env
            thread1     = Thread flag ec (Term_Var "x") [("x", ValueEntry value)]
            thread2     = Thread Child EC_Empty e extendedEnv
          in
          Ok { config | channels      = (newChId, []) :: config.channels
                      , threads       = thread1 :: thread2 :: config.threads
                      , nextChannelId = config.nextChannelId + 1             }
        
        Pot_Spawn e env ->
          let
            thread1 = Thread flag ec (Term_Var "x") [("x", ValueEntry Value_Unit)]
            thread2 = Thread Child EC_Empty e env
          in
          Ok { config | threads = thread1 :: thread2 :: config.threads }

        _ ->
          Err ()


type EvalStatus = Running | Finished | Lock | Error | Stopped

initialConfig : Term -> Configuration
initialConfig term =
  { channels      = []
  , threads       = [Thread Main EC_Empty term []]
  , nextChannelId = 0
  , returnValue   = Nothing
  }

noThreadsAreBlocked : List (ChannelId, ChannelQueue) -> Bool
noThreadsAreBlocked channels =
  List.all (\(_, queue) -> List.isEmpty queue) channels

step : Seed -> Configuration -> (EvalStatus, Configuration, Seed)
step seed config =
  let
    pickRandomThread = Random.List.choose config.threads
    ((maybeThread, threads), newSeed) = Random.step pickRandomThread seed
  in
  case maybeThread of
    Just thread ->
      case reduce thread { config | threads = threads } of
        Ok newConfig ->
          (Running, newConfig, newSeed)
        
        Err () ->
          (Error, config, newSeed)

    Nothing ->
      if noThreadsAreBlocked config.channels then
        (Finished, config, newSeed)
      else
        (Lock, config, newSeed)
      