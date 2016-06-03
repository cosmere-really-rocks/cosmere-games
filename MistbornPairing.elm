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

type alias Tile = Int
type alias TilePos = (Int, Int)
type alias Tiles = List Int
type alias Board = Dict.Dict TilePos Tile

type alias Config = { rows : Int
                    , cols : Int
                    , tileWidth: Int
                    , tileHeight: Int
                    , topPadding : Int
                    }       
type alias HoleFiller = Config -> Board -> TilePos -> Maybe TilePos
type alias Params = { lang : String, theme : String }
    
type alias Model = { config : Config
                   , holeFiller : HoleFiller
                   , board : Board
                   , hoverAt : Maybe TilePos
                   , clicked : List TilePos
                   , path : List TilePos
                   , hint : List TilePos
                   , hinted : Bool
                   , theme : (String, String)
                   , i18n : I18n.Model
                   }

type Msg = UpdateBoard Board
         | MouseEnter TilePos
         | MouseLeave TilePos
         | ClickOn TilePos
         | Paired (List TilePos)
         | Hint
         | ChangeTheme String
         | OnI18n I18n.Msg

urlParser : Navigation.Parser Params
urlParser = Navigation.makeParser fromUrl

init : Params -> (Model, Cmd Msg)
init params = let ( model', cmd' ) = I18n.init params.lang
                  r = config.rows
                  c = config.cols
                  xs = List.concat <| List.repeat r [ 1 .. c ]
                  ys = List.concatMap (List.repeat c) [ 1 .. r ]
                  board = Dict.fromList
                          <| List.map2 (\x y -> ( ( x, y ), -1 )) xs ys 
              in ( { config = config
                   , holeFiller = gravity
                   , board = board
                   , hoverAt = Nothing
                   , clicked = []
                   , path = []
                   , hint = []
                   , hinted = False
                   , theme = findTheme params.theme
                   , i18n = model'
                   }
                 , Platform.batch [ runGenerator UpdateBoard
                                        <| (Random.list (r * c // 2)
                                            <| Random.int 0 <| symbols - 1)
                                            `Random.andThen` generateBoard board
                                  , Platform.map OnI18n cmd'
                                  ]
                 )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        UpdateBoard board -> updateBoard { model | board = board }
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
                       else let t = getTile model.board pos
                                t' = getTile model.board pos'
                            in if t /= t'
                               then ( setClicked model, Cmd.none )
                               else
                                   checkPairing pos pos'
                                   <| { model
                                          | clicked = [ pos, pos' ]
                                          , hinted = False
                                      }
                   _ ->
                       let board' = if model.hinted
                                    then model.board
                                    else resetTiles model.board model.clicked
                           ( m, c ) = update (Paired model.clicked)
                                      <| { model | board = board' }
                       in ( setClicked m, c )
        Paired clicked ->
            if model.clicked /= clicked
            then ( model, Cmd.none )
            else flip update model
                <| UpdateBoard
                <| resetTiles model.board model.clicked
        Hint ->
            let m = if model.hinted || List.length model.clicked /= 2
                    then model
                    else fst <| update (Paired model.clicked) model
                ends = flip List.map [ List.head m.hint, List.last m.hint ]
                       <| Maybe.withDefault ( 0, 0 )
            in ( { m | clicked = ends, path = model.hint, hinted = True }
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
    let w = toString ((model.config.cols + 2) * 80) ++ "px"
        h = toString ((model.config.rows + 2) * 105) ++ "px"
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
                  <| Html.img [ Html.src "hint.png"
                              , Html.style [ ( "width", "30px" )
                                           , ( "height", "30px" )
                                           ]
                              , onClick Hint
                              ] []
                      :: (List.concat
                          <| List.indexedMap (themePicker model) themes)
              , h3 [ Html.style [ ( "color", "blue" )
                                , ( "text-align", "center" )
                                ]
                   ] [ let intro = model.i18n.translations.symbolIntro
                       in Html.text
                           <| Maybe.withDefault ""
                           <| model.hoverAt
                               `Maybe.andThen` (flip Array.get intro
                                                << getTile model.board)
                     ]
              ]
        , div [ Html.style <| styles ++ [ ( "z-index", "10" )
                                        , ( "background",  "rgba(0,0,0,0)" )
                                        ]
              ]
              <| List.concatMap (showTile model)
              <| Dict.toList model.board
        , Svg.svg [ Html.style <| ( "z-index", "0" ) :: styles ]
            <| if model.clicked == []
               then []
               else showPath model.config model.path
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

setTiles : Board -> Tiles -> Board
setTiles board = Dict.fromList << List.zip (Dict.keys board)
                
generateBoard : Board -> Tiles -> Random.Generator Board
generateBoard board halfTiles =
    Random.map (setTiles board) <| shuffle <| halfTiles ++ halfTiles

updateBoard : Model -> (Model, Cmd Msg)
updateBoard model =
    let board' = fillHoles model.holeFiller model.config model.board
                 <| model.clicked
        model' = { model
                     | clicked = []
                     , path = []
                     , hinted = False
                     , board = board'
                 }
        hint = findPair model.config board'
    in if Dict.isEmpty board' || hint /= Nothing
       then ( { model' | hint = Maybe.withDefault [] hint }, Cmd.none )
       else ( model'
            , runGenerator UpdateBoard
                <| Random.map (setTiles board')
                <| shuffle
                <| Dict.values board'
            )

fillHoles : HoleFiller -> Config -> Board -> List TilePos -> Board
fillHoles filler config board poses =
    if poses == []
    then board
    else let ( board', poses' ) =
             List.foldl (\pos ( board, poses ) ->
                             case filler config board pos of
                                 Just pos' ->
                                     ( Dict.insert pos (getTile board pos')
                                       <| Dict.remove pos' board
                                     , pos' :: poses
                                     )
                                 _ -> ( board, poses )) ( board, [] ) poses
         in fillHoles filler config board' poses'
           
showPath : Config -> List TilePos -> List (Svg Msg)
showPath config path =
    let makePoints = String.join " " <| List.map makePoint path
        makePoint ( x, y ) =
            toString ((x * 2 + 1) * w // 2) ++ ","
                ++ toString ((y * 2 + 1) * h // 2 + p)
        w = config.tileWidth + 5
        h = config.tileHeight + 5
        p = config.topPadding
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
        toSize = (<|) toString >> (flip (++) "px")
        config = model.config
        delta = if isClicked || isHoverAt then 5 else 0
        width = toSize <| config.tileWidth - delta
        height = toSize <| config.tileHeight - delta
    in if t < 0
       then []
       else [ img [ src ("themes/" ++ fst (model.theme) ++ "/" ++ toString t
                         ++ "." ++ snd (model.theme))
                  , Html.style [ ( "width", width )
                               , ( "height", height )
                               , ( "top"
                                 , toSize <| (y * (config.tileHeight + 5)
                                              + config.topPadding) )
                               , ( "left"
                                 , toSize <| x * (config.tileWidth + 5) )
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

findPair : Config -> Board -> Maybe (List TilePos)
findPair config board =
    let syms = [ 0 .. symbols - 1 ]
        posesOf sym = Dict.toList board
                    |> List.filter ((==) sym << snd)
                    |> List.map fst
        check pos pos' = checkPairing' config board pos pos'
        checkPoses poses =
            case poses of
                []  -> Nothing
                pos :: rest -> Maybe.oneOf
                               <| checkPoses rest
                                   :: List.map (check pos) rest
    in Maybe.oneOf <| List.map checkPoses <| List.map posesOf syms
        
checkPairing : TilePos -> TilePos -> Model -> (Model, Cmd Msg)
checkPairing pos pos' model = 
    case checkPairing' model.config model.board pos pos' of
        Just path ->
            let msg = always <| Paired model.clicked
            -- in flip update model <| msg ()
            in ( { model | path = path }
               , Task.perform msg msg <| Process.sleep <| 0.3 * Time.second
               )
        Nothing ->
            ( { model | clicked = [] }, Cmd.none )

checkPairing' : Config -> Board -> TilePos -> TilePos -> Maybe (List TilePos)
checkPairing' config board pos pos' =
    let flipTuple = uncurry <| flip (,)
        dimSum = config.rows + config.cols
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
                            <| getTile board << flip makeCoord y)
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
                          <| ((==) -1
                              << getTile board
                              << flip makeCoord y
                              << (+) x)
            -- for each empty cell in the top path, see if the L shape
            -- topping there can reach ( x', y' )
            in Maybe.oneOf
                <| flip List.map topPath
                <| (checkLShape makeCoord x y x' y'
                    << (+) x)
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
                   <| getTile board << makeCoord x'')
                -- as well as the bottom path
                || (List.any ((/=) -1)
                    <| List.map (getTile board << flip makeCoord y')
                    <| List.filter ((/=) x')
                    <| connect x' x'')
            then Nothing
            else Just [ makeCoord x y
                      , makeCoord x'' y
                      , makeCoord x'' y'
                      , makeCoord x' y'
                      ]
        connect i i' = [ Basics.min i i' .. Basics.max i i' ]
    in checkPath False (,) config.cols pos pos'

getTile : Board -> TilePos -> Tile
getTile board = Maybe.withDefault -1 << flip Dict.get board

resetTiles : Board -> List TilePos -> Board
resetTiles = List.foldl Dict.remove

symbols : Int
symbols = 16

themePicker : Model -> Int -> (String, String) -> List (Html Msg)
themePicker model n ( theme, _ ) =
    let tag = if theme == fst model.theme then Html.span else Html.a
        link = tag [ Html.href "javascript:void()"
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

config : Config
config = { rows = 9
         , cols = 18
         , tileWidth = 50
         , tileHeight = 65
         , topPadding = 50
         }
              
stasis : HoleFiller
stasis _ _ _ = Nothing
              
gravity : HoleFiller
gravity _ board ( x, y ) =
    let ys = flip List.map [ 1 .. y - 1 ] <| (-) y
    in Maybe.map ((,) x << fst)
        <| List.find ((/=) Nothing << snd)
        <| List.zip ys
        <| flip List.map ys
        <| (flip Dict.get board << (,) x)

            
