module I18n exposing ( Model, Msg(..), init, update, view )

import Array
import Debug
import Json.Decode exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import List
import List.Extra as List
import Maybe
import Process
import Time
import Task

type alias Model translations = { languages : List (String, String)
                                , urlBuilder : String -> String
                                , decoder : Decoder translations
                                , lang : String
                                , translations : translations
                                }

type Msg translations = Fetch String
                      | FetchSucceed translations 
                      | FetchFail String Http.Error

init : List (String, String) -> (String -> String) -> t -> Decoder t -> String
     -> (Model t, Cmd (Msg t))
init languages urlBuilder defTrans decoder lang =
    let model = { languages = languages
                , urlBuilder = urlBuilder
                , decoder = decoder
                , lang = lang
                , translations = defTrans
                }
    in ( model, fetch model lang )

update : Msg t -> Model t -> (Model t, Cmd (Msg t))
update msg model =
    case msg of
        Fetch lang ->
            ( { model | lang = lang }, fetch model lang )
        FetchSucceed translations ->
            ( { model | translations = translations }, Cmd.none )
        FetchFail lang err ->
            let msg = always <| Fetch lang
            in ( model
               , Task.perform msg msg <| Process.sleep <| 5 * Time.second
               )
                
view : Model t -> Html (Msg t)
view model =
    span [ style [ ( "width", "100%" ) ] ]
    <| List.concat
    <| List.indexedMap (languagePicker model) model.languages

fetch : Model t -> String -> Cmd (Msg t)
fetch model lang =
    let lang' = fst <| findLanguage model.languages lang
    in Task.perform (FetchFail lang') FetchSucceed
        <| Http.get model.decoder
        <| model.urlBuilder lang'

languagePicker : Model t -> Int -> (String, String) -> List (Html (Msg t))
languagePicker model  n ( lang, label ) =
    let ( tag, attrs ) = if model.lang == lang
                         then ( span, [] )
                         else ( a
                              , [ href "javascript:void()"
                                , onClick <| Fetch lang
                                ]
                              )
        link = tag (style [ ( "padding", "0 20px" ) ] :: attrs) [ text label ]
    in if n == 0
       then [ link ]
       else [ text "|", link ] 

findLanguage : List (String, String) -> String -> (String, String)
findLanguage languages lang = Maybe.withDefault ( "", "" )
                              <| Maybe.oneOf
                              <| [ List.find (((==) lang) << fst) languages
                                 , List.head languages
                                 ]
