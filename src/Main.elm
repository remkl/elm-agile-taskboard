port module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode


-- Model


type alias Task =
    { description : String
    , status : String
    }


type alias Model =
    { tasks : List Task
    , taskInput : String
    , draggingTask : Maybe Task
    }


initModel : Model
initModel =
    { taskInput = ""
    , tasks = []
    , draggingTask = Nothing
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )



-- Update


type Msg
    = TaskInput String
    | SubmitTask Int
    | DeleteTask Task
    | DragTask Task
    | DropTask String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TaskInput input ->
            ( { model | taskInput = input }, Cmd.none )

        SubmitTask key ->
            if key == 13 then
                newTask model
            else
                ( model, Cmd.none )

        DeleteTask task ->
            let
                filteredTasks =
                    model.tasks
                        |> List.filter (\t -> t.description /= task.description)
            in
                ( { model | tasks = filteredTasks }, Cmd.none )

        DragTask task ->
            ( { model | draggingTask = Just task }, Cmd.none )

        DropTask newStatus ->
            updateTask model newStatus


newTask : Model -> ( Model, Cmd Msg )
newTask model =
    let
        newModel =
            { model
                | tasks = model.tasks ++ [ Task model.taskInput "Backlog" ]
                , taskInput = ""
            }
    in
        ( newModel, Cmd.none )


updateTask : Model -> String -> ( Model, Cmd Msg )
updateTask model newStatus =
    let
        tasks =
            case model.draggingTask of
                Just task ->
                    updateStatus model task newStatus

                Nothing ->
                    ( model, Cmd.none )
    in
        tasks


updateStatus : Model -> Task -> String -> ( Model, Cmd Msg )
updateStatus model task newStatus =
    let
        newTasks =
            model.tasks
                |> List.map
                    (\t ->
                        if t.description == task.description then
                            { t | status = newStatus }
                        else
                            t
                    )
    in
        ( { model | tasks = newTasks, draggingTask = Nothing }, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    div [ class "page__container" ]
        [ inputHeader model
        , boardBody model
        ]


inputHeader : Model -> Html Msg
inputHeader model =
    div [ class "input__container" ]
        [ input
            [ class "input"
            , type_ "text"
            , placeholder "Enter task"
            , value model.taskInput
            , onInput TaskInput
            , onKeyDown SubmitTask
            ]
            []
        ]


boardBody : Model -> Html Msg
boardBody model =
    let
        backlog =
            filterByStatus "Backlog" model.tasks

        progressing =
            filterByStatus "Progressing" model.tasks

        testing =
            filterByStatus "Testing" model.tasks

        done =
            filterByStatus "Done" model.tasks
    in
        div [ class "board__container" ]
            [ taskSection "Backlog" <| renderList backlog
            , taskSection "Progressing" <| renderList progressing
            , taskSection "Testing" <| renderList testing
            , taskSection "Done" <| renderList done
            ]


filterByStatus : String -> List Task -> List Task
filterByStatus status tasks =
    tasks
        |> List.filter (\t -> t.status == status)


taskSection : String -> Html Msg -> Html Msg
taskSection name list =
    div [ class "board__section", id name, attribute "ondragover" "return false", onDrop (DropTask name) ]
        [ h3 [ class "board__section__header" ] [ text name ]
        , div [ class "board__section__body" ] [ list ]
        ]


renderList : List Task -> Html Msg
renderList tasks =
    tasks
        |> List.map taskList
        |> ul [ class "task__list" ]


taskList : Task -> Html Msg
taskList task =
    let
        { description } =
            task
    in
        li [ class ("task__item"), attribute "draggable" "true", onDragStart (DragTask task) ]
            [ div [ class "task__item__header" ]
                [ span [ class "task__delete", onClick (DeleteTask task) ] [ text "X" ]
                ]
            , p [ class "task__item__body" ] [ text description ]
            ]



-- Event helpers


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Decode.map tagger keyCode)


onDragStart : msg -> Attribute msg
onDragStart eventMessage =
    on "dragstart" (Decode.succeed eventMessage)


onDragEnd : msg -> Attribute msg
onDragEnd eventMessage =
    on "dragend" (Decode.succeed eventMessage)


onDrop : msg -> Attribute msg
onDrop eventMessage =
    onWithOptions "drop"
        { preventDefault = True
        , stopPropagation = False
        }
        (Decode.succeed eventMessage)



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
