import Array
import Debug
import Html exposing (..)
import Html.App as Html
import Html.Attributes as Html exposing (..)
import Html.Events exposing (..)
import List
import List.Extra as List
import List.Split as List
import Maybe
import Process
import Random as Rand
import Random.Pcg as Random
import String
import Svg exposing (..)
import Svg.Attributes as Svg exposing (..)
import Task
import Time

main = Html.program { init = init
                    , view = view
                    , update = update
                    , subscriptions = subscriptions
                    }

type alias Tiles = Array.Array Int
type alias Model = { rows : Int
                   , cols : Int
                   , tiles : Tiles
                   , clicked : List (Int, Int)
                   , path : List (Int, Int)
                   }

type Msg = GenerateBoard (List Int)
         | ClickOn Int Int
         | Paired (List (Int, Int))

init : (Model, Cmd Msg)
init = let r = 6
           c = 12
       in ( { rows = r, cols = c, tiles = Array.empty, clicked = [], path = [] }
          , Rand.generate GenerateBoard
              <| Rand.map (fst
                           << (Random.step
                                   <| (Random.list (r * c // 2)
                                       <| Random.int 0 15)
                              `Random.andThen` generateBoard)
                           << Random.initialSeed)
              <| Rand.int Rand.minInt Rand.maxInt
          )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GenerateBoard tiles ->
            ( { model | tiles = convertBoard model tiles }, Cmd.none )
        ClickOn x y ->
            let setClicked m = { m | clicked = [ ( x, y ) ] }
            in case model.clicked of
                   [ ( x', y' ) ] ->
                       if x == x' && y == y'
                       then ( { model | clicked = [] }, Cmd.none )
                       else let t = getTile model <| indexOf model x y
                                t' = getTile model <| indexOf model x' y'
                            in if t /= t'
                               then ( setClicked model, Cmd.none )
                               else checkPairing
                                   { model | clicked = [ ( x, y )
                                                       , ( x', y' )
                                                       ] } x y x' y'
                   _ ->
                       ( resetTiles model.clicked
                             <| setClicked { model | path = [] }
                       , Cmd.none
                       )
        Paired clicked ->
            ( if model.clicked /= clicked
              then model
              else resetTiles model.clicked { model | clicked = [] }
            , Cmd.none
            )

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
        [ h2 [ Html.style [ ( "color", "blue" )
                          , ( "text-align", "center" )
                   ]
             ]
              [ Html.text
                  <| Maybe.withDefault ""
                  <| Maybe.oneOf
                  <| List.map (flip Array.get symbolIntro
                               << getTile model
                               << uncurry (indexOf model))
                  <| model.clicked
                ]
        , div [ Html.style <| styles ++ [ ( "z-index", "10" )
                                        , ( "background",  "rgba(0,0,0,0)" )
                                        ]
              ]
              <| List.concat
              <| Array.toList
              <| Array.indexedMap (showTile model) model.tiles
        , Svg.svg [ Html.style <| ( "z-index", "0" ) :: styles ]
            <| if model.clicked == []
               then []
               else showPath model
        ]
              
generateBoard : List Int -> Random.Generator (List Int)
generateBoard halfTiles =
    let tiles = halfTiles ++ halfTiles
        pick n l = ( Maybe.withDefault -1 <| List.head <| List.drop n l
                   , List.take n l ++ List.drop (n + 1) l
                   )
        shuffle ( a, b ) =
            let len = List.length b
            in if len == 0
               then Random.constant ( a, b )
               else (Random.map (flip pick b) <| Random.int 0 (len - 1))
                   `Random.andThen` (\(t, b') -> shuffle ( t :: a, b' ))
    in Random.map fst <| shuffle ( [], tiles )

convertBoard : Model -> List Int -> Tiles
convertBoard model tiles =
    let chunks = List.chunksOfLeft model.cols tiles
        padChunks = \chunk -> -1 :: (chunk ++ [-1])
        padding = List.repeat (model.cols + 2) -1
    in Array.fromList
        <| padding ++ List.concatMap padChunks chunks ++ padding

coordOf : Model -> Int -> (Int, Int)
coordOf model i = ( i % (model.cols + 2), i // (model.cols + 2) )

indexOf : Model -> Int -> Int -> Int
indexOf model x y = y * (model.cols + 2) + x

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
                    
showTile : Model -> Int -> Int -> List (Html Msg)
showTile model i t =
    let ( x, y ) = coordOf model i
        isClicked = List.member ( x, y ) model.clicked
        border = if isClicked
                 then "inset 3px gold"
                 else "solid 1px black"
        width = if isClicked then "70px" else "75px"
        height = if isClicked then "95px" else "100px"
    in if t < 0
       then []
       else [ img [ src (toString t ++ ".svg")
                  , Html.style [ ( "width", width )
                               , ( "height", height )
                               , ( "top", toString (y * 105) ++ "px" )
                               , ( "left", toString (x * 80) ++ "px" )
                               , ( "position", "absolute" )
                               , ( "border", border )
                               , ( "border-radius", "10px" )
                               , ( "background", "white" )
                               ]
                  , onClick <| ClickOn x y
                  ] []
            ]

checkPairing : Model -> Int -> Int -> Int -> Int -> (Model, Cmd Msg)
checkPairing model x y x' y' =
    let dimSum = model.rows + model.cols
        checkPath lastPass makeCoord calcIndex dim x y x' y' =
            if x > x'
            then checkPath lastPass makeCoord calcIndex dim x' y' x y
            else case checkPath' makeCoord calcIndex dim x y x' y' of
                     Just path ->
                         let msg = always <| Paired model.clicked
                         -- in flip update model <| msg ()
                         in ( { model | path = path }
                            , Task.perform msg msg
                                <| Process.sleep
                                <| 1 * Time.second
                            )
                     Nothing ->
                         if lastPass
                         then ( { model | clicked = [] }, Cmd.none )
                         else
                             -- we try the same algo with switched axes
                             -- to reuse code
                             checkPath True (flip makeCoord) (flip calcIndex)
                                 (dimSum - dim) y x y' x'
        checkPath' makeCoord calcIndex dim x y x' y' =
            let simple =
                    if y /= y'
                        -- same level, see if there's a straight line
                        || (List.any ((/=) -1)
                            <| Debug.log "blocking"
                            <| List.map (getTile model << flip calcIndex y)
                            <| [x + 1 .. x' - 1])
                    then Nothing
                    else Just [ makeCoord x y, makeCoord x' y' ]
            in if simple /= Nothing
               then simple
               else let left = List.map negate [1 .. x + 1]
                        right = [1 .. dim + 1 - x]
                    in Maybe.oneOf
                        [ checkPaths makeCoord calcIndex x y x' y' left
                        , checkPaths makeCoord calcIndex x y x' y' right
                        ]
        checkPaths makeCoord calcIndex x y x' y' dx =
            -- first find a unobstructed top path from ( x, y )
            -- assuming y < y'
            let topPath = Debug.log "top path" <| List.takeWhile (((==) -1)
                                          << getTile model
                                          << flip calcIndex y
                                          << ((+) x)) dx
            -- for each empty cell in the top path, see if the L shape
            -- topping there can reach ( x', y' )
            in Maybe.oneOf
                <| List.map (checkLShape makeCoord calcIndex x y x' y'
                             << ((+) x)) topPath
        checkLShape makeCoord calcIndex x y x' y' x'' =
            -- if the whole path is L shaped, don't counnt ( x', y' )
            -- which is known to be /= -1
            let vertPath = connect y y'
                vertPath' = Debug.log "vert path" <| if x'' == x'
                            then List.filter ((/=) y') vertPath
                            else vertPath
            -- vertical path has to be unobstructed
            in if (List.any ((/=) -1)
                   <| List.map (getTile model << calcIndex x'')
                   <| vertPath')
                -- as well as the bottom path
                || (List.any ((/=) -1)
                    <| List.map (getTile model << flip calcIndex y')
                    <| List.filter ((/=) x')
                    <| connect x' x'')
            then Nothing
            else Just [ makeCoord  x y,
                        makeCoord  x'' y
                      , makeCoord x'' y'
                      , makeCoord x' y' ]
        connect i i' = [Basics.min i i' .. Basics.max i i']
    in checkPath False (,) (indexOf model) model.cols x y x' y'

getTile : Model -> Int -> Int
getTile model = Maybe.withDefault -1 << flip Array.get model.tiles

resetTiles : List (Int, Int) -> Model -> Model
resetTiles coords model =
    case coords of
        ( x, y ) :: coords' ->
            resetTiles coords' { model | tiles = Array.set (indexOf model x y)
                                                 -1 model.tiles
                              }
        [] -> model

symbolIntro : Array.Array String
symbolIntro = Array.fromList [ "铝 （铝虫） 消化掉体内所有金属"
                             , "弯管合金 （滑行） 加快自己周围的时间流逝"
                             , "黄铜 （安抚者） 安抚周遭人的情绪"
                             , "青铜 （搜寻者） 听到金属脉动"
                             , "镉 （脉动） 延缓自己周围的时间流逝"
                             , "铬 （水蛭） 藉由碰触，清空另一位镕金术师的所有金属存量"
                             , "红铜 （烟阵） 隐藏金属脉动"
                             , "硬铝 （硬铝虫） 燃尽其他正在燃烧的金属"
                             , "电金 （预言师） 看到自己未来的可能道路"
                             , "金 （命师） 看到过去的自己"
                             , "铁 （扯手） 拉引附近金属"
                             , "镍铬 （镍爆） 藉由碰触，烧尽另一位镕金术师的所有金属存量"
                             , "白镴 （打手） 增加身体机能"
                             , "钢 （射币） 钢推附近的金属"
                             , "锡 （锡眼） 增加五感的敏锐度"
                             , "锌 （煽动者） 煽动附近的人的情绪"
                             ]
              
