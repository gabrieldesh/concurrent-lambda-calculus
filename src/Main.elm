module Main exposing (main)

import AbstractSyntax exposing (..)
import Parsing exposing (parseProgram)
import TypeInference exposing (typeInferProgram)
import Evaluation exposing (evalProgram, Value(..), valueToString)
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

    SaveFile -> (model, Download.string "program.llc" "text/plain" model.code)

    GotFile file -> (model, Task.perform ChangeCode (File.toString file))



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
      , div [style "margin-bottom" "20px"]
        [ button [ onClick LoadFile ] [ text "Load file" ]
        , button [ onClick SaveFile ] [ text "Save file" ]
        ]
      , textarea [rows 25, cols 80, autofocus True, onInput ChangeCode, value model.code] [ ]
      , viewParsed model.code
      ]
  }

viewParsed : String -> Html Msg
viewParsed code =
  case parseInferEval code of
    SyntaxError _ ->
      pre [] [ text "Syntax error" ]

    TypeError _ error ->
      pre [] [ text ("Error: " ++ error) ]
    
    WellTyped _ aType value ->
      pre [] [ text ("Value: \n" ++ (valueToString value) ++ "\n\n")
             , text ("Type: \n" ++ typeToString aType) ]
      


-- PARSING, TYPE INFERENCE AND EVALUATION

type ParsingResult
  = SyntaxError String
  | TypeError LambdaProgram String
  | WellTyped LambdaProgram Type Value

parseInferEval : String -> ParsingResult
parseInferEval code =
  case parseProgram code of
    Ok program ->
      case typeInferProgram program of
        Ok aType ->
          -- evalProgram should suceed since program is well-typed
          let value = Maybe.withDefault Value_Unit (evalProgram program)
          in WellTyped program aType value
        Err error ->
          TypeError program error
    Err errors ->
      SyntaxError (String.join " or " errors)