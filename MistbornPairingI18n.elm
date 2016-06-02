module MistbornPairingI18n exposing ( Model, Msg(..), init, update, view )

import Array
import Dict
import Json.Decode exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Maybe
import Process
import Time
import Task

type alias Translations = { backButton : String
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
    let padding = style [ ( "padding", "0 20px" ) ]
    in div [ style [ ( "width", "100%" ) ]
           ] [ a [ href "index.html"
                 , padding ] [ text model.translations.backButton ]
             , a [ href "javascript:void()"
                 , padding
                 , onClick <| Fetch "en"
                 ] [ text "English" ]
             , text "|"
             , a [ href "javascript:void()"
                 , padding
                 , onClick <| Fetch "zh"
                 ] [ text <| "中文" ]
             ]

fetch : String -> Cmd Msg
fetch lang = Task.perform (FetchFail lang) FetchSucceed
                <| flip Http.get ("mistborn-pairing." ++ lang ++ ".json")
                <| object3 Translations
                    ("backButton" := string)
                    ("symbolIntro" := array string)
                    ("themes" := dict string)
