import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Input exposing (clickable)
import Color exposing (..)
import Task exposing (Task, sleep, andThen, succeed)
import Time exposing (..)
import Keyboard


type alias Model =
  { animationState : Int
  , pause : Bool
  }


initialModel : Model
initialModel =
  { animationState = 1
  , pause = True
  }


view : Signal.Address Action -> Model -> Graphics.Element.Element
view address model =
  collage 800 480
    [ rect 800 480
        |> filled (rgb 18 93 75)

    , rect 800 110
        |> filled (rgb 166 118 65)
        |> moveY -183

    , image 100 60 (playerAssetPath model)
        |> toForm
        |> move (-300, -100)

    , show (toString model)
        |> Graphics.Collage.toForm
    ]
  --|> clickable (Signal.message actions.address UpdateAnimationState)


playerAssetPath : Model -> String
playerAssetPath model =
  if model.pause then
    "assets/i1.png"
  else
    "assets/k" ++ (toString model.animationState) ++ ".png"


type Action
  = NoOp
  | TogglePause
  | UpdateAnimationState


update : Action -> Model -> Model
update action model =
  case Debug.log "action" action of
    NoOp ->
      model

    TogglePause ->
      { model | pause = not model.pause }

    UpdateAnimationState ->
      if model.animationState > 7 then
        { model | animationState = 1 }
      else
        { model | animationState = model.animationState + 1 }


actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp


tasksMailbox : Signal.Mailbox (Task x ())
tasksMailbox =
  Signal.mailbox (Task.succeed ())


model : Signal Model
model =
  Signal.foldp update initialModel input


port tasks : Signal (Task x ())
port tasks =
  tasksMailbox.signal


main =
  Signal.map (view actions.address) model


input =
  let
    delta = Signal.map (\t -> t/20) (fps 10)
    gameLoop = Signal.sampleOn delta (Signal.map2 (,) delta Keyboard.arrows)
    toAction = always UpdateAnimationState
    arrowsToAction = Signal.map toAction gameLoop
    togglePause = Signal.map (\pressed ->
      if pressed then
        TogglePause
      else
        NoOp) Keyboard.space
  in
    Signal.mergeMany [ actions.signal, arrowsToAction, togglePause ]
