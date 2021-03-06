module MistbornPairing exposing ( main )

import Array
import Debug
import Dict
import Html exposing (Html, a, div, h1, h3, img, span, text)
import Html.App as Html
import Html.Attributes exposing (href, src, style)
import Html.Events exposing (..)
import Json.Decode as Json exposing ((:=))
import List
import List.Extra as List
import List.Split as List
import Maybe
import Navigation
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Process
import Random as Rand
import Random.Pcg as Random
import Result
import String
import Svg exposing (polyline, svg)
import Svg.Attributes exposing (fill, points, stroke, strokeWidth)
-- import Svg.Events exposing (..)
import Task
import Time
import UrlParser exposing ((</>))

import CountDownBar as Bar
import I18n

main = Navigation.program urlParser { init = init 0
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
type alias HoleFiller = Board -> TilePos -> Maybe TilePos
type alias Params = { lang : String, theme : String, level : Int }

type alias Translations = { backButton : String
                          , levelClear : String
                          , allClear : String
                          , gameOver : String
                          , symbolIntro : Array.Array String
                          , themes : Dict.Dict String String
                          }
    
type alias Model = { level : Int
                   , holeFiller : HoleFiller
                   , board : Board
                   , hoverAt : Maybe TilePos
                   , clicked : List TilePos
                   , path : List TilePos
                   , score : Int
                   , hint : List TilePos
                   , hinted : Bool
                   , theme : (String, String)
                   , i18n : I18n.Model Translations
                   , bar : Bar.Model
                   }

type Msg = LevelUp
         | UpdateBoard Board
         | MouseEnter TilePos
         | MouseLeave TilePos
         | ClickOn TilePos
         | Paired (List TilePos)
         | Hint
         | ChangeTheme String
         | OnI18n (I18n.Msg Translations)
         | OnCountDown Bar.Msg

urlParser : Navigation.Parser Params
urlParser = Navigation.makeParser <| fromUrl << .hash

init : Int -> Params -> (Model, Cmd Msg)
init score params =
    let ( m', c' ) = I18n.init languages i18nUrlBuilder i18nDefaultTranslations
                     i18nDecoder params.lang
        ( m'', c'' ) = Bar.init [ ( "width", "500px" )
                                , ( "height", "30px" )
                                , ( "border", "solid 1px black" )
                                , ( "border-radius", "3px" )
                                ] (100 * Time.millisecond) 3000 -- 300s
        r = config.rows
        c = config.cols
        xs = List.concat <| List.repeat r [ 1 .. c ]
        ys = List.concatMap (List.repeat c) [ 1 .. r ]
        board = Dict.fromList <| List.map2 (\x y -> ( ( x, y ), -1 )) xs ys 
    in ( { level = params.level
         , holeFiller = Maybe.withDefault stasis
                        <| Array.get params.level levels
         , board = board
         , hoverAt = Nothing
         , clicked = []
         , path = []
         , score = score
         , hint = []
         , hinted = False
         , theme = findTheme params.theme
         , i18n = m'
         , bar = m''
         }
       , Cmd.batch [ runGenerator UpdateBoard
                         <| (Random.list (r * c // 2)
                             <| Random.int 0 <| symbols - 1)
                             `Random.andThen` generateBoard board
                   , Cmd.batch [ Cmd.map OnI18n c'
                               , Cmd.map OnCountDown c''
                               ]
                   ]
       )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        LevelUp -> ( model
                   , updateUrl { model | level = model.level + 1 }
                       <| fst model.theme
                   )
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
                   [] -> ( setClicked model, Cmd.none)
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
                   _ -> if model.hinted
                        then ( setClicked { model | path = [] }, Cmd.none )
                        else let ( m, c ) = update (Paired model.clicked) model
                             in ( setClicked m, c )
        Paired clicked ->
            if Bar.isTimedOut model.bar ||  model.clicked /= clicked
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
        ChangeTheme theme -> ( model, updateUrl model theme )
        OnI18n i18n ->
            let ( m, c ) = I18n.update i18n model.i18n
                model' = { model | i18n = m }
            in ( model'
               , Cmd.batch [ Cmd.map OnI18n c
                           , updateUrl model' <| fst model'.theme
                           ]
               )
        OnCountDown bar ->
            let ( m, c ) = Bar.update bar model.bar
                model' = { model | bar = m }
            in ( model', Cmd.map OnCountDown c )

urlUpdate : Params -> Model -> (Model, Cmd Msg)
urlUpdate params model =
    if params.level /= model.level
    then init model.score params
    else let ( m, c ) = if params.lang == model.i18n.lang
                        then ( model, Cmd.none )
                        else update (OnI18n <| I18n.Fetch params.lang) model
         in ( { m | theme = findTheme params.theme }, c )
              
subscriptions : Model -> Sub Msg
subscriptions model = Sub.map OnCountDown <| Bar.subscriptions model.bar

view : Model -> Html Msg
view model =
    let translations = model.i18n.translations
        w = toString ((config.cols + 2) * (config.tileWidth + 5)) ++ "px"
        h = toString ((config.rows + 2) * (config.tileHeight + 5)
                      + config.topPadding) ++ "px"
        styles = [ ( "width", w )
                 , ( "height", h )
                 , ( "top", "0" )
                 , ( "left", "0" )
                 , ( "position", "absolute" )
                 ]
    in div [ style [ ( "display", "flex" )
                   , ( "width", "100%" )
                   ]
           ]
        <| if Dict.isEmpty model.board || Bar.isTimedOut model.bar
           then [ h1 [ style [ ( "text-align", "center" )
                             , ( "vertical-align", "center" )
                             , ( "margin", "auto" )
                             ] ]
                  <| [ text
                           <| (if Bar.isTimedOut model.bar
                               then translations.gameOver
                               else if model.level == Array.length levels - 1
                                    then translations.allClear 
                                    else translations.levelClear)
                               ++ toString model.score
                     ]
                ]
           else [ div [ style [ ("z-index", "100" )
                             , ( "position", "absolute" )
                             , ( "width", "100%" )
                             ]
                     ]
                     [ div [] [ a [ href "index.html"
                                  , style [ ( "padding", "0 20px" ) ]
                                  ] [ text translations.backButton ]
                              , Html.map OnI18n <| I18n.view model.i18n
                              , Html.map OnCountDown <| Bar.view model.bar
                              ]
                     , div [ style [ ( "width", "100%" ) ] ]
                         <| img [ src "hint.png"
                                , style [ ( "width", "30px" )
                                        , ( "height", "30px" )
                                        ]
                                , onClick Hint
                                ] []
                             :: (List.concat
                                 <| List.indexedMap (themePicker model) themes)
                     , h3 [ style [ ( "color", "blue" )
                                  , ( "text-align", "center" )
                                  ]
                          ] [ let intro = translations.symbolIntro
                              in text
                                  <| Maybe.withDefault ""
                                  <| (model.hoverAt
                                      `Maybe.andThen` (flip Array.get intro
                                                       << getTile model.board))
                            ]
                     ]
               , div [ style <| styles ++ [ ( "z-index", "10" )
                                          , ( "background",  "rgba(0,0,0,0)" )
                                          ]
                     ]
                     <| List.concatMap (showTile model)
                     <| Dict.toList model.board
               , svg [ style <| ( "z-index", "0" ) :: styles ]
                   <| if model.clicked == []
                      then []
                      else showPath config model.path
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
    let board' = fillHoles model.holeFiller model.board
                 <| model.clicked
        model' = { model
                     | clicked = []
                     , path = []
                     , hinted = False
                     , board = board'
                 }
        hint = findPair board'
    in if Dict.isEmpty board'
       then let levelScore = model.bar.ticksLeft * (10 + model.level)
                model' = { model | score = model.score + levelScore }
            in if model.level == Array.length levels - 1
            then ( model', Cmd.none )
            else let msg = always LevelUp
                 in ( model'
                    , Task.perform msg msg <| Process.sleep <| 3 * Time.second
                    )
       else case  hint of
                Just hint' -> ( { model' | hint = hint' }, Cmd.none )
                _ -> ( model'
                     , runGenerator UpdateBoard
                         <| Random.map (setTiles board')
                         <| shuffle
                         <| Dict.values board'
                     )

fillHoles : HoleFiller -> Board -> List TilePos -> Board
fillHoles filler board poses =
    if poses == []
    then board
    else let ( board', poses' ) =
             List.foldl (\pos ( board, poses ) ->
                             case filler board pos of
                                 Just pos' ->
                                     ( Dict.insert pos (getTile board pos')
                                       <| Dict.remove pos' board
                                     , pos' :: poses
                                     )
                                 _ -> ( board, poses )) ( board, [] ) poses
         in fillHoles filler board' poses'
           
showPath : Config -> List TilePos -> List (Html Msg)
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
        delta = if isClicked || isHoverAt then 5 else 0
        width = toSize <| config.tileWidth - delta
        height = toSize <| config.tileHeight - delta
    in if t < 0
       then []
       else [ img [ src ("themes/" ++ fst (model.theme) ++ "/" ++ toString t
                         ++ "." ++ snd (model.theme))
                  , style [ ( "width", width )
                          , ( "height", height )
                          , ( "top", toSize <| (y * (config.tileHeight + 5)
                                                + config.topPadding) )
                          , ( "left", toSize <| x * (config.tileWidth + 5) )
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

findPair : Board -> Maybe (List TilePos)
findPair board =
    let syms = [ 0 .. symbols - 1 ]
        posesOf sym = Dict.toList board
                    |> List.filter ((==) sym << snd)
                    |> List.map fst
        check pos pos' = checkPairing' board pos pos'
        checkPoses poses =
            case poses of
                []  -> Nothing
                pos :: rest -> Maybe.oneOf
                               <| checkPoses rest
                                   :: List.map (check pos) rest
    in Maybe.oneOf <| List.map checkPoses <| List.map posesOf syms
        
checkPairing : TilePos -> TilePos -> Model -> (Model, Cmd Msg)
checkPairing pos pos' model = 
    case checkPairing' model.board pos pos' of
        Just path ->
            let msg = always <| Paired model.clicked
            -- in flip update model <| msg ()
            in ( { model
                     | path = path
                     -- 10 points for each tile cleared
                     , score = model.score + (10 + model.level) * 2 }
               , Task.perform msg msg <| Process.sleep <| 0.3 * Time.second
               )
        Nothing ->
            ( { model | clicked = [] }, Cmd.none )

checkPairing' :  Board -> TilePos -> TilePos -> Maybe (List TilePos)
checkPairing' board pos pos' =
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
            let xs = flip List.map dx <| (+) x
                topPath = x :: (flip List.takeWhile xs
                                <| ((==) -1
                                    << getTile board
                                    << flip makeCoord y))
            -- for each empty cell in the top path, see if the L shape
            -- topping there can reach ( x', y' )
            in Maybe.oneOf
                <| flip List.map topPath
                <| checkLShape makeCoord x y x' y'
        checkLShape makeCoord x y x' y' x'' =
            -- if the whole path is L shaped, don't counnt ( x', y' )
            -- which is known to be /= -1
            let vertPath = connect y y'
                vertPath' = if x'' == x'
                            then List.filter ((/=) y') vertPath
                            else vertPath
                vertPath'' = if x'' == x
                             then List.filter ((/=) y) vertPath'
                             else vertPath'
            -- vertical path has to be unobstructed
            in if (List.any ((/=) -1)
                   <| flip List.map vertPath''
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
    let tag = if theme == fst model.theme then span else a
        link = tag [ href "javascript:void()"
                   , style [ ( "padding", "0 20px" )
                                , ( "text-align", "center" ) ]
                   , onClick <| ChangeTheme <| theme
                   ] [ text
                       <| Maybe.withDefault ""
                       <| Dict.get theme
                       <| model.i18n.translations.themes
                     ]
    in if n == 0
       then [ link ]
       else [ text "|", link ]

themes : List (String, String)
themes = [ ( "plain", "svg" )
         , ( "lien", "jpg" )
         ]

findTheme : String -> (String, String)
findTheme theme = Maybe.withDefault ( "", "" )
                  <| Maybe.oneOf [ List.find ((==) theme << fst) themes
                                 , List.head themes
                                 ]

updateUrl : Model -> String -> Cmd Msg
updateUrl model theme = Navigation.newUrl
                        <| toUrl
                        <| Params model.i18n.lang theme model.level
                      
toUrl : Params -> String
toUrl params =
  "#/lang/" ++ params.lang ++ "/theme/" ++ params.theme
      ++ "/level/" ++ (toString params.level)

langParser : UrlParser.Parser (String -> a) a
langParser = (UrlParser.s "lang" </> UrlParser.string) --<?> "zh"

themeParser : UrlParser.Parser (String -> a) a
themeParser = (UrlParser.s "theme" </> UrlParser.string) --<?> "plain"

levelParser : UrlParser.Parser (Int -> a) a
levelParser = (UrlParser.s "level" </> UrlParser.int) --<?> 0

-- infixl 8 <?>
-- (<?>) : UrlParser.Parser (a -> output) b -> a
--       -> UrlParser.Parser (a -> output) b
-- (<?>) (UrlParser.Parser parser) default =
--     UrlParser.Parser <| \chunks formatter ->
--         Result.withDefault ( chunks, formatter default )
--             <| parser chunks formatter
              
fromUrl : String -> Params
fromUrl = Result.withDefault { lang = "zh", theme = "plain", level = 0 }
          << UrlParser.parse Params (langParser </> themeParser </> levelParser)
          << String.dropLeft 2

config : Config
config = { rows = 8
         , cols = 12
         , tileWidth = 50
         , tileHeight = 50
         , topPadding = 100
         }

languages : List (String, String)
languages = [ ( "en", "English" )
            , ( "zh", "中文" )
            ]

i18nUrlBuilder : String -> String
i18nUrlBuilder lang = "mistborn-pairing." ++ lang ++ ".json"

i18nDecoder : Json.Decoder Translations
i18nDecoder = Json.object6 Translations
              ("backButton" := Json.string)
              ("levelClear" := Json.string)
              ("allClear" := Json.string)
              ("gameOver" := Json.string)
              ("symbolIntro" := Json.array Json.string)
              ("themes" := Json.dict Json.string)

i18nDefaultTranslations : Translations
i18nDefaultTranslations = { backButton = "<<"
                          , levelClear = ""
                          , allClear = ""
                          , gameOver = ""
                          , symbolIntro = Array.empty
                          , themes = Dict.empty
                          }

levels : Array.Array HoleFiller
levels = Array.fromList [ stasis
                        , gravity
                        , rocket
                        , lashLeft
                        , lashRight
                        , plowDown
                        , plowUp
                        , plowLeft
                        , plowRight
                        , bite
                        , jawOpener
                        , symplegades
                        , ripper
                        ]
    
stasis : HoleFiller
stasis _ _ = Nothing

lashing : (TilePos -> Int) -> (Int -> TilePos) -> (Int -> List Int)
        -> HoleFiller
lashing pickCoord makeCoord surge board coord =
    let y = pickCoord coord
        ys = surge y
    in Maybe.map (makeCoord << fst)
        <| List.find ((/=) Nothing << snd)
        <| List.zip ys
        <| flip List.map ys
        <| (flip Dict.get board << makeCoord)

downSurge : Int -> Int -> List Int
downSurge range y = flip List.map [ 1 .. y - range ] <| (-) y
            
upSurge : Int -> Int -> List Int
upSurge range y = flip List.map [ 1 .. range - y ] <| (+) y

addX : TilePos -> Int -> TilePos
addX ( x, _ ) = (,) x
            
addY : TilePos -> Int -> TilePos
addY ( _, y ) = flip (,) y
            
gravity' : Int -> HoleFiller
gravity' range board pos = lashing snd (addX pos) (downSurge range) board pos

rocket' : Int -> HoleFiller
rocket' range board pos = lashing snd (addX pos) (upSurge range) board pos

lashLeft' : Int -> HoleFiller
lashLeft' range board pos = lashing fst (addY pos) (upSurge range) board pos

lashRight' : Int -> HoleFiller
lashRight' range board pos = lashing fst (addY pos) (downSurge range) board pos

gravity : HoleFiller
gravity = gravity' 1
          
rocket : HoleFiller
rocket = rocket' config.rows

lashLeft : HoleFiller
lashLeft = lashLeft' config.cols

lashRight : HoleFiller
lashRight = lashRight' 1

fancyFiller : (TilePos -> HoleFiller) -> HoleFiller
fancyFiller picker board pos = (picker pos) board pos
    
plowDown : HoleFiller
plowDown = fancyFiller <| \( x, _ ) -> if x % 2 == 0 then gravity else rocket

plowUp : HoleFiller
plowUp = fancyFiller <| \( x, _ ) -> if x % 2 /= 0 then gravity else rocket

plowLeft : HoleFiller
plowLeft = fancyFiller <| \( _, y ) -> if y % 2 == 0
                                       then lashLeft
                                       else lashRight

plowRight : HoleFiller
plowRight = fancyFiller <| \( _, y ) -> if y % 2 /= 0
                                        then lashRight
                                        else lashLeft

bite : HoleFiller
bite = fancyFiller <| \( _, y ) -> if y <= config.rows // 2
                                          then gravity
                                          else rocket

jawOpener : HoleFiller
jawOpener = fancyFiller <| \( _, y ) -> let middle = config.rows // 2
                                        in if y == middle || y == middle + 1
                                           then stasis
                                           else if y > middle + 1
                                                then gravity' <| middle + 1
                                                else rocket' middle

-- the clashing rocks
symplegades : HoleFiller
symplegades = fancyFiller <| \( x, _ ) -> if x <= config.cols // 2
                                          then lashRight
                                          else lashLeft
                                                     
ripper : HoleFiller
ripper = fancyFiller <| \( x, _ ) -> let middle = config.cols // 2
                                     in if x == middle || x == middle + 1
                                        then stasis
                                        else if x > middle + 1
                                             then lashRight' <| middle + 1
                                             else lashLeft' <| middle
