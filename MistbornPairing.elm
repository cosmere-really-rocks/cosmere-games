module MistbornPairing exposing ( main, init, update, view )

import Array
import Debug
import Dict
import Html exposing (..)
import Html.App as Html
import Html.Attributes as Html exposing (..)
import Html.Events exposing (..)
import List
import List.Extra as List
import List.Split as List
import Maybe
import Navigation
import Platform.Cmd as Platform
import Process
import Random as Rand
import Random.Pcg as Random
import Result
import String
import Svg exposing (..)
import Svg.Attributes as Svg exposing (..)
import Task
import Time
import UrlParser exposing ((</>))

import MistbornPairingI18n as I18n

main = Navigation.program urlParser { init = init
                                    , view = view
                                    , update = update
                                    , urlUpdate = urlUpdate
                                    , subscriptions = subscriptions
                                    }

type alias Params = { lang : String, theme : String }
       
type alias Tile = Int
type alias TilePos = (Int, Int)
type alias Tiles = Dict.Dict TilePos Tile
type alias Model = { rows : Int
                   , cols : Int
                   , tiles : Tiles
                   , hoverAt : Maybe TilePos
                   , clicked : List TilePos
                   , path : List TilePos
                   , theme : (String, String)
                   , i18n : I18n.Model
                   }

type Msg = GenerateBoard (List Tile)
         | ShuffleBoard (List Tile)
         | MouseEnter TilePos
         | MouseLeave TilePos
         | ClickOn TilePos
         | Paired (List TilePos)
         | ChangeTheme String
         | OnI18n I18n.Msg

urlParser : Navigation.Parser Params
urlParser = Navigation.makeParser fromUrl

init : Params -> (Model, Cmd Msg)
init params = let r = 6
                  c = 12
                  ( model', cmd' ) = I18n.init params.lang
              in ( { rows = r
                   , cols = c
                   , tiles = Dict.empty
                   , hoverAt = Nothing
                   , clicked = []
                   , path = []
                   , theme = findTheme params.theme
                   , i18n = model'
                   }
                 , Platform.batch [ runGenerator GenerateBoard
                                        <| (Random.list (r * c // 2)
                                            <| Random.int 0 <| symbols - 1)
                                            `Random.andThen` generateBoard
                                  , Platform.map OnI18n cmd'
                                  ]
                 )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GenerateBoard tiles ->
            ( { model | tiles = convertBoard model tiles }, Cmd.none )
        ShuffleBoard tiles ->
            let newBoard = setTiles model.tiles tiles
            in if hasPairs model newBoard
               then ( { model | tiles = newBoard }, Cmd.none )
               else ( model, runGenerator ShuffleBoard <| shuffle tiles )
        MouseEnter pos ->
            ( { model | hoverAt = Just pos }, Cmd.none )
        MouseLeave pos ->
            let pos' = if model.hoverAt == Just pos
                       then Nothing
                       else model.hoverAt
            in ( { model | hoverAt = pos' }, Cmd.none )
        ClickOn pos ->
            let setClicked m = { m | clicked = [ pos ] }
            in case model.clicked of
                   [ pos' ] ->
                       if pos == pos'
                       then ( { model | clicked = [] }, Cmd.none )
                       else let t = getTile model pos
                                t' = getTile model pos'
                            in if t /= t'
                               then ( setClicked model, Cmd.none )
                               else
                                   checkPairing pos pos'
                                   <| { model | clicked = [ pos, pos' ] }
                   _ -> ( setClicked
                              { model
                                  | path = []
                                  , tiles = resetTiles model.tiles model.clicked
                              }
                       , Cmd.none
                       )
        Paired clicked ->
            ( if model.clicked /= clicked
              then model
              else { model
                       | clicked = []
                       , tiles = resetTiles model.tiles model.clicked
                   }
            , Cmd.none
            )
        ChangeTheme theme ->
            ( { model | theme = findTheme theme }
            , Navigation.newUrl <| toUrl <| Params model.i18n.lang theme
            )
        OnI18n i18n ->
            let ( m, c ) = I18n.update i18n model.i18n
            in ( { model | i18n = m }
               , Platform.batch [ Platform.map OnI18n c
                                , Navigation.newUrl
                                    <| toUrl
                                    <| Params m.lang
                                    <| fst model.theme
                                ]
               )

urlUpdate : Params -> Model -> (Model, Cmd Msg)
urlUpdate params model =
    let ( m, c ) = if params.theme == fst model.theme
                   then ( model, Cmd.none )
                   else update (ChangeTheme params.theme) model
        ( m', c' ) = if params.lang == model.i18n.lang
                     then ( model.i18n, Cmd.none )
                     else I18n.update (I18n.Fetch params.lang) model.i18n
    in ( { m | i18n = m' }, Platform.batch [ c, Platform.map OnI18n c' ] )
          
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

view : Model -> Html Msg
view model =
    let w = toString ((model.cols + 2) * 80) ++ "px"
        h = toString ((model.rows + 2) * 105) ++ "px"
        styles = [ ( "width", w )
                 , ( "height", h )
                 , ( "top", "0" )
                 , ( "left", "0" )
                 , ( "position", "absolute" )
                 ]
    in div [ Html.style styles ]
        [ div [ Html.style [ ("z-index", "100" )
                           , ( "position", "absolute" )
                           , ( "width", "100%" )
                           ]
              ]
              [ Html.map OnI18n <| I18n.view model.i18n
              , div [ Html.style [ ( "width", "100%" ) ] ]
                  <| List.concat
                  <| List.indexedMap (themePicker model) themes
              , h3 [ Html.style [ ( "color", "blue" )
                                , ( "text-align", "center" )
                                ]
                   ] [ Html.text
                       <| Maybe.withDefault ""
                       <| model.hoverAt `Maybe.andThen`
                           (flip Array.get model.i18n.translations.symbolIntro
                            << getTile model)
                     ]
              ]
        , div [ Html.style <| styles ++ [ ( "z-index", "10" )
                                        , ( "background",  "rgba(0,0,0,0)" )
                                        ]
              ]
              <| List.concatMap (showTile model)
              <| Dict.toList model.tiles
        , Svg.svg [ Html.style <| ( "z-index", "0" ) :: styles ]
            <| if model.clicked == []
               then []
               else showPath model
        ]

runGenerator : (a -> msg) -> Random.Generator a -> Cmd msg
runGenerator f gen = Rand.generate f
                   <| Rand.map (fst
                                << Random.step gen
                                << Random.initialSeed)
                   <| Rand.int Rand.minInt Rand.maxInt
        
shuffle : List Int -> Random.Generator (List Int)
shuffle l =
    let pick n l = ( Maybe.withDefault -1 <| List.head <| List.drop n l
                   , List.take n l ++ List.drop (n + 1) l
                   )
        shuffle' ( a, b ) =
            let len = List.length b
            in if len == 0
               then Random.constant ( a, b )
               else (Random.map (flip pick b) <| Random.int 0 (len - 1))
                   `Random.andThen` (\(t, b') -> shuffle' ( t :: a, b' ))
    in Random.map fst <| shuffle' ( [], l )

setTiles : Tiles -> List Int -> Tiles
setTiles board = Dict.fromList << List.zip (Dict.keys board)
                
generateBoard : List Int -> Random.Generator (List Int)
generateBoard halfTiles = shuffle <| halfTiles ++ halfTiles

convertBoard : Model -> List Int -> Tiles
convertBoard model tiles =
    let xs = List.concat <| List.repeat model.rows [ 1 .. model.cols ]
        ys = List.concatMap (List.repeat model.cols) [ 1 .. model.rows ]
    in Dict.fromList <| List.map3 (\x y t -> ( ( x, y ), t )) xs ys tiles

showPath : Model -> List (Svg Msg)
showPath model =
    let makePoints = String.join " " <| List.map makePoint model.path
        makePoint ( x, y ) = toString (x * 80 + 40) ++ ","
                             ++ toString (y * 105 + 53)
    in [ polyline [ fill "none"
                  , stroke "red"
                  , strokeWidth "3px"
                  , points makePoints
                  ] []
       ]
                    
showTile : Model -> (TilePos, Tile) -> List (Html Msg)
showTile model ( pos, t ) =
    let ( x, y ) = pos
        isClicked = List.member pos model.clicked
        isHoverAt = model.hoverAt == Just pos
        border = if isClicked
                 then "inset 3px gold"
                 else if isHoverAt
                      then "inset 3px blue"
                      else "solid 1px black"
        width = if isClicked || isHoverAt then "70px" else "75px"
        height = if isClicked || isHoverAt then "95px" else "100px"
    in if t < 0
       then []
       else [ img [ src ("themes/" ++ fst (model.theme) ++ "/" ++ toString t
                         ++ "." ++ snd (model.theme))
                  , Html.style [ ( "width", width )
                               , ( "height", height )
                               , ( "top", toString (y * 105) ++ "px" )
                               , ( "left", toString (x * 80) ++ "px" )
                               , ( "position", "absolute" )
                               , ( "border", border )
                               , ( "border-radius", "10px" )
                               , ( "background", "white" )
                               ]
                  , onClick <| ClickOn pos
                  , onMouseEnter <| MouseEnter pos
                  , onMouseLeave <| MouseLeave pos
                  ] []
            ]

hasPairs : Model -> Tiles -> Bool
hasPairs model tiles =
    let syms = [ 0 .. symbols - 1 ]
        posesOf sym = Dict.toList tiles
                    |> List.filter (((==) sym) << snd)
                    |> List.map fst
        check pos pos' = checkPairing' model pos pos' /= Nothing
        checkPoses poses =
            case poses of
                []  -> False
                pos :: rest -> List.any (check pos) rest || checkPoses rest
    in List.any checkPoses <| List.map posesOf syms
        
checkPairing : TilePos -> TilePos -> Model -> (Model, Cmd Msg)
checkPairing pos pos' model = 
    case checkPairing' model pos pos' of
        Just path ->
            let msg = always <| Paired model.clicked
                newBoard = resetTiles model.tiles model.clicked
            -- in flip update model <| msg ()
            in if Dict.isEmpty newBoard || hasPairs model newBoard
               then ( { model | path = path }
                    , Task.perform msg msg
                        <| Process.sleep
                        <| 0.3 * Time.second
                    )
               else ( model
                    , runGenerator ShuffleBoard
                        <| shuffle
                        <| Dict.values newBoard
                    )
        Nothing ->
            ( { model | clicked = [] }, Cmd.none )

checkPairing' : Model -> TilePos -> TilePos -> Maybe (List TilePos)
checkPairing' model pos pos' =
    let flipTuple = uncurry <| flip (,)
        dimSum = model.rows + model.cols
        checkPath lastPass makeCoord dim pos pos' =
            let ( x, y ) = pos
                ( x', y' ) = pos'
            in if x > x'
               then checkPath lastPass makeCoord dim pos' pos
               else [ checkPath' makeCoord dim x y x' y'
                    , if lastPass
                      then Nothing
                      else
                          -- we try the same algo with switched axes
                          -- to reuse code
                          checkPath True (flip makeCoord) (dimSum - dim)
                              (flipTuple pos) (flipTuple pos')
                    ] |> Maybe.oneOf
        checkPath' makeCoord dim x y x' y' =
            let simple =
                    if y /= y'
                        -- same level, see if there's a straight line
                        || (List.any ((/=) -1)
                            <| flip List.map [ x + 1 .. x' - 1 ]
                            <| getTile model << flip makeCoord y)
                    then Nothing
                    else Just [ makeCoord x y, makeCoord x' y' ]
            in if simple /= Nothing
               then simple
               else let left = List.map negate [ 1 .. x + 1 ]
                        right = [ 1 .. dim + 1 - x ]
                    in Maybe.oneOf
                        [ checkPaths makeCoord x y x' y' left
                        , checkPaths makeCoord x y x' y' right
                        ]
        checkPaths makeCoord x y x' y' dx =
            -- first find a unobstructed top path from ( x, y )
            -- assuming y < y'
            let topPath = flip List.takeWhile dx
                          <| ((==) -1)
                              << getTile model
                              << flip makeCoord y
                              << ((+) x)
            -- for each empty cell in the top path, see if the L shape
            -- topping there can reach ( x', y' )
            in Maybe.oneOf
                <| flip List.map topPath
                <| checkLShape makeCoord x y x' y'
                    << ((+) x)
        checkLShape makeCoord x y x' y' x'' =
            -- if the whole path is L shaped, don't counnt ( x', y' )
            -- which is known to be /= -1
            let vertPath = connect y y'
                vertPath' = if x'' == x'
                            then List.filter ((/=) y') vertPath
                            else vertPath
            -- vertical path has to be unobstructed
            in if (List.any ((/=) -1)
                   <| flip List.map vertPath'
                   <| getTile model << makeCoord x'')
                -- as well as the bottom path
                || (List.any ((/=) -1)
                    <| List.map (getTile model << flip makeCoord y')
                    <| List.filter ((/=) x')
                    <| connect x' x'')
            then Nothing
            else Just [ makeCoord x y
                      , makeCoord x'' y
                      , makeCoord x'' y'
                      , makeCoord x' y'
                      ]
        connect i i' = [ Basics.min i i' .. Basics.max i i' ]
    in checkPath False (,) model.cols pos pos'

getTile : Model -> TilePos -> Tile
getTile model = Maybe.withDefault -1 << flip Dict.get model.tiles

resetTiles : Tiles -> List TilePos -> Tiles
resetTiles = List.foldl Dict.remove

symbols : Int
symbols = 16

themePicker : Model -> Int -> (String, String) -> List (Html Msg)
themePicker model n ( theme, _ ) =
    let link = Html.a [ Html.href "javascript:void()"
                      , Html.style [ ( "padding", "0 20px" )
                                   , ( "text-align", "center" ) ]
                      , onClick <| ChangeTheme <| theme
                      ] [ Html.text
                          <| Maybe.withDefault ""
                          <| Dict.get theme
                          <| model.i18n.translations.themes
                        ]
    in if n == 0
       then [ link ]
       else [ Html.text "|", link ]

themes : List (String, String)
themes = [ ( "plain", "svg" )
         , ( "lien", "jpg" )
         ]

findTheme : String -> (String, String)
findTheme theme = Maybe.withDefault ( "", "" )
                  <| Maybe.oneOf [ List.find ((==) theme << fst) themes
                                 , List.head themes
                                 ]
                      
toUrl : Params -> String
toUrl params =
  "#/lang/" ++ params.lang ++ "/theme/" ++ params.theme

langParser : UrlParser.Parser (String -> a) a
langParser = UrlParser.s "lang" </> UrlParser.string

themeParser : UrlParser.Parser (String -> a) a
themeParser = UrlParser.s "theme" </> UrlParser.string
      
fromUrl : Navigation.Location -> Params
fromUrl = Result.withDefault { lang = "zh", theme = "plain" }
          << UrlParser.parse Params (langParser </> themeParser)
          << String.dropLeft 2
          << .hash
