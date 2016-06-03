module MistbornPairingI18n exposing ( Model, Msg(..), init, update, view )

import Array
import Debug
import Dict
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

type alias Translations = { backButton : String
                          , levelClear : String
                          , allClear : String
                          , symbolIntro : Array.Array String
                          , themes : Dict.Dict String String
                          }
type alias Model = { lang : String
                   , translations : Translations
                   }

type Msg = Fetch String
         | FetchSucceed Translations
         | FetchFail String Http.Error

init : String -> (Model, Cmd Msg)
init lang = ( { lang = lang
              , translations = { backButton = "<<"
                               , levelClear = ""
                               , allClear = ""
                               , symbolIntro = Array.empty
                               , themes = Dict.empty
                               }
              }
            , fetch lang
            )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Fetch lang -> ( { model | lang = lang }, fetch lang )
        FetchSucceed translations ->
            ( { model | translations = translations }, Cmd.none )
        FetchFail lang err ->
            let msg = always <| Fetch lang
            in ( model
               , Task.perform msg msg <| Process.sleep <| 5 * Time.second
               )
                
view : Model -> Html Msg
view model =
    div [ style [ ( "width", "100%" ) ] ]
    <| (a [ href "index.html", style [ ( "padding", "0 20px" ) ] ]
        <| [ text model.translations.backButton ])
        :: List.concat (List.indexedMap (languagePicker model) languages)

fetch : String -> Cmd Msg
fetch lang = let lang' = fst <| findLanguage lang
             in Task.perform (FetchFail lang') FetchSucceed
                <| flip Http.get ("mistborn-pairing." ++ lang' ++ ".json")
                <| object5 Translations
                    ("backButton" := string)
                    ("levelClear" := string)
                    ("allClear" := string)
                    ("symbolIntro" := array string)
                    ("themes" := dict string)

languagePicker : Model -> Int -> (String, String) -> List (Html Msg)
languagePicker model n ( lang, label ) =
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

findLanguage : String -> (String, String)
findLanguage lang = Maybe.withDefault ( "", "" )
                    <| Maybe.oneOf
                    <| [ List.find (((==) lang) << fst) languages
                       , List.head languages
                       ]
           
languages : List (String, String)
languages = [ ( "en", "English" )
            , ( "zh", "中文" )
            ]
