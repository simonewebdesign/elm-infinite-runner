import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Input exposing (clickable)
import Color exposing (..)
import Task exposing (Task, sleep, andThen, succeed)
import Time exposing (..)
import Keyboard


type alias Model =
  { animationState : Int
  , counter : Int
  }


initialModel : Model
initialModel =
  { animationState = 1
  , counter = 0
  }


view : Signal.Address Action -> Model -> Graphics.Element.Element
view address model =
  collage 800 480
    [ rect 800 480
        |> filled (rgb 18 93 75)
        --|> toForm
        --|> moveY 200
    , image 100 60 ("assets/k" ++ (toString model.animationState) ++ ".png")
        |> toForm
        |> moveY -210
    , show (toString model)
        |> Graphics.Collage.toForm
    ]
  |> clickable (Signal.message actions.address UpdateAnimationState)
  --|> clickable (Signal.message tasksMailbox.address start)


type Action
  = NoOp
  | UpdateAnimationState
  | UpdateCounter


update : Action -> Model -> Model
update action model =
  case Debug.log "action" action of
    NoOp ->
      model

    UpdateAnimationState ->
      if model.animationState > 7 then
        { model | animationState = 1 }
      else
        { model | animationState = model.animationState + 1 }

    UpdateCounter ->
      { model | counter = model.counter + 1 }      


actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp


tasksMailbox : Signal.Mailbox (Task x ())
tasksMailbox =
  Signal.mailbox (Task.succeed ())


model : Signal Model
model =
  Signal.foldp update initialModel input


--start : Task x ()
--start =
  --sleep 1000 `andThen` \_ -> succeed Update
  --sleep 1000 `andThen` \_ -> Signal.send actions.address Update


port tasks : Signal (Task x ())
port tasks =
  tasksMailbox.signal


main =
  Signal.map (view actions.address) model
--main : Signal Element
--main =
  --Signal.map view (Signal.foldp update model input)


--input : Signal (Float, Keys)
input =
  let
    delta = Signal.map (\t -> t/20) (fps 10)
    gameLoop = Signal.sampleOn delta (Signal.map2 (,) delta Keyboard.arrows)
    toAction = always UpdateAnimationState
    arrowsToAction = Signal.map toAction gameLoop
  in
    Signal.mergeMany [ actions.signal, arrowsToAction ]
