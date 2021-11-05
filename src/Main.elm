module Main exposing (main)

import AbstractSyntax exposing (..)
import Parsing exposing (parseProgram)
import TypeInference exposing (typeInferProgram)
import Evaluation exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import File exposing (File)
import File.Select as Select
import File.Download as Download
import Task
import Time exposing (posixToMillis)
import Random exposing (Seed, initialSeed)
import Process exposing (sleep)


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

type InterpreterState
  = SyntaxError String
  | TypeError String
  | WaitingSeed Configuration CLCProgram Type
  | Evaluation EvalStatus Configuration Seed CLCProgram Type

type alias Model = { code : String, state : InterpreterState }

init : () -> (Model, Cmd Msg)
init _ = ({ code = "", state = SyntaxError "" }, Cmd.none)



-- UPDATE

type Msg = ChangeCode String | GotSeed Seed | EvalStep | Stop | ReRun | LoadFile | SaveFile | GotFile File

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    ChangeCode newCode ->
      case parseProgram newCode of
        Err errors ->
          ( { code = newCode, state = SyntaxError (String.join " or " errors) }
          , Cmd.none )
        Ok program ->
          case typeInferProgram program of
            Err error -> 
              ( { code = newCode, state = TypeError error }
              , Cmd.none )
            Ok aType ->
              ( { code = newCode, state = WaitingSeed (initialConfig program.mainTerm) program aType }
              , getSeedFromTime )
    
    GotSeed seed ->
      case model.state of
        WaitingSeed config program aType ->
          update EvalStep { model | state = Evaluation Running config seed program aType }
        
        _ -> (model, Cmd.none)
    
    EvalStep ->
      case model.state of
        Evaluation Running config seed program aType ->
          let
            (newEvalStatus, newConfig, newSeed) = Evaluation.step seed config
          in
          ( { model | state = Evaluation newEvalStatus newConfig newSeed program aType }
          , fireMsg EvalStep )
        
        _ -> (model, Cmd.none)
    
    Stop ->
      case model.state of
        Evaluation Running config seed program aType ->
          let
            newConfig = { config | channels = [], threads = [] }
          in
          ( { model | state = Evaluation Stopped newConfig seed program aType }, Cmd.none )
        
        _ -> (model, Cmd.none)
    
    ReRun ->
      case model.state of
        Evaluation Running _ _ _ _ ->
          (model, Cmd.none)
        
        Evaluation _ _ seed program aType ->
          update EvalStep 
            { model | state = Evaluation Running (initialConfig program.mainTerm) seed program aType }
        
        _ -> (model, Cmd.none)

    LoadFile -> (model, Select.file [] GotFile)

    SaveFile -> (model, Download.string "programa.clc" "text/plain" model.code)

    GotFile file -> (model, Task.perform ChangeCode (File.toString file))


getSeedFromTime : Cmd Msg
getSeedFromTime =
  Task.perform (\p -> GotSeed (initialSeed (posixToMillis p))) Time.now

fireMsg : Msg -> Cmd Msg
fireMsg msg =
  Task.perform (\() -> msg) (sleep 0)



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW

view : Model -> Browser.Document Msg
view model = 
  { title = "Simulador de cálculo-lambda concorrente"
  , body =
      [ h1 [] [text "Simulador de cálculo-lambda concorrente"]
      , div [style "margin-bottom" "20px"]
        [ button [ onClick LoadFile ] [ text "Carregar arquivo" ]
        , button [ onClick SaveFile ] [ text "Salvar arquivo" ]
        ]
      , textarea [rows 23, cols 80, autofocus True, onInput ChangeCode, value model.code] [ ]
      , viewState model.state
      ]
  }

viewState : InterpreterState -> Html Msg
viewState state =
  case state of
    SyntaxError _ ->
      div []
        [ pre [] [ text "Erro de sintaxe" ]
        ]

    TypeError error ->
      div []
        [ pre [] [ text ("Erro de tipo: " ++ error) ]
        ]
    
    WaitingSeed _ _ aType ->
      div []
        [ pre [] [ text ("Tipo: \n" ++ typeToString aType) ]
        ]
    
    Evaluation status { returnValue } _ _ aType ->
      let
        statusString =
          case status of
            Running  -> "Rodando"
            Finished -> "Terminado"
            Lock     -> "Lock"
            Error    -> "Erro"
            Stopped  -> "Parado"
        valueString =
          case returnValue of
            Just value -> "Valor de retorno: " ++ valueToString value
            Nothing    -> ""
      in
      div []
        [ pre [] [ text ("Tipo: " ++ typeToString aType) ]
        , pre [] [ text ("Status: " ++ statusString) ]
        , pre [] [ text valueString ]
        , case status of
            Running -> button [ onClick Stop  ] [ text "Parar"         ]
            _       -> button [ onClick ReRun ] [ text "Rodar de novo" ]
        ]