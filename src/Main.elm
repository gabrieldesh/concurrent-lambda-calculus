module Main exposing (main)

import AbstractSyntax exposing (..)
import Parsing exposing (parseProgram)
--import TypeInference exposing (typeInferProgram)
--import Evaluation exposing (evalProgram, Value(..), valueToString)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import File exposing (File)
import File.Select as Select
import File.Download as Download
import Task


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

type Msg = ChangeCode String | LoadFile | SaveFile | GotFile File

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    ChangeCode newCode -> ( { model | code = newCode }, Cmd.none )

    LoadFile -> (model, Select.file [] GotFile)

    SaveFile -> (model, Download.string "programa.stl" "text/plain" model.code)

    GotFile file -> (model, Task.perform ChangeCode (File.toString file))



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
      , textarea [rows 25, cols 80, autofocus True, onInput ChangeCode, value model.code] [ ]
      , viewParsed model.code
      ]
  }

viewParsed : String -> Html Msg
viewParsed code =
  case parseInferEval code of
    SyntaxError _ ->
      pre [] [ text "Erro de sintaxe" ]

    TypeError _ error ->
      pre [] [ text ("Sintaxe OK") ]
    
    EvaluationError _ aType ->
      pre [] [ text ("Value: \n<stuck>\n\n")
             , text ("Type: \n" ++ typeToString aType) ]
    
    {-Success _ aType value ->
      pre [] [ text ("Value: \n" ++ (valueToString value) ++ "\n\n")
             , text ("Type: \n" ++ typeToString aType) ]-}
      


-- PARSING, TYPE INFERENCE AND EVALUATION

type ParsingResult
  = SyntaxError String
  | TypeError STLProgram String
  | EvaluationError STLProgram Type
  --| Success STLProgram Type Value

parseInferEval : String -> ParsingResult
parseInferEval code =
  case parseProgram code of
    Ok program ->
      {-_case typeInferProgram program of
        Ok aType ->
          case evalProgram program of
            Just value ->
              Success program aType value
            Nothing ->
              EvaluationError program aType
        Err error -> -}
          TypeError program "error"
    Err errors ->
      SyntaxError (String.join " or " errors)