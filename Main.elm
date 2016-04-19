import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Input exposing (clickable)
import Color exposing (..)
import Task exposing (Task, sleep, andThen, succeed)

main =
  Signal.map (view actions.address) model


type alias Model =
  { animationState : Int
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
  = Start
  | UpdateAnimationState


update : Action -> Model -> Model
update action model =
  case Debug.log "action" action of
    Start ->
      model

    UpdateAnimationState ->
      { model | animationState = model.animationState + 1 }


actions : Signal.Mailbox Action
actions =
  Signal.mailbox Start


tasksMailbox : Signal.Mailbox (Task x ())
tasksMailbox =
  Signal.mailbox (Task.succeed ())


initialModel : Model
initialModel =
  { animationState = 1
  }


model : Signal Model
model =
  Signal.foldp update initialModel actions.signal


--start : Task x ()
--start =
  --sleep 1000 `andThen` \_ -> succeed Update
  --sleep 1000 `andThen` \_ -> Signal.send actions.address Update


port tasks : Signal (Task x ())
port tasks =
  tasksMailbox.signal
