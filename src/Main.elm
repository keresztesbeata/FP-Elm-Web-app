module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Http
import Json.Decode as De
import Model exposing (..)
import Model.Event as Event
import Model.Event.Category as EventCategory
import Model.PersonalDetails as PersonalDetails
import Model.Repo as Repo

import Task

type Msg
    = GetRepos
    | GotRepos (Result Http.Error (List Repo.Repo))
    | SelectEventCategory EventCategory.EventCategory
    | DeselectEventCategory EventCategory.EventCategory


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

fetchRepos: Cmd Msg
fetchRepos =     
    --Http.get
        --{ url = "https://github.com/elm/compiler"
        --, expect = Http.expectJson GotRepos (De.list Repo.decodeRepo)
        --}
    Http.request 
    { method = "GET"
    , headers = [Http.header "Access-Control-Allow-Origin:" "*"]
    , url = "https://github.com/elm/compiler"
    , body = Http.emptyBody
    , expect = Http.expectJson GotRepos (De.list Repo.decodeRepo)
    , timeout = Nothing
    , tracker = Nothing
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel
    , fetchRepos
    )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetRepos ->
            ( model, fetchRepos )

        GotRepos res ->
            ( {model | repos = model.repos ++ (Result.withDefault [] res)}
            , Cmd.none )    

        SelectEventCategory category ->
            ( {model | selectedEventCategories = EventCategory.set category True model.selectedEventCategories}, Cmd.none)

        DeselectEventCategory category ->
            ( {model | selectedEventCategories = EventCategory.set category False model.selectedEventCategories}, Cmd.none)


eventCategoryToMsg : ( EventCategory.EventCategory, Bool ) -> Msg
eventCategoryToMsg ( event, selected ) =
    if selected then
        SelectEventCategory event

    else
        DeselectEventCategory event


view : Model -> Html Msg
view model =
    let
        eventCategoriesView =
            EventCategory.view model.selectedEventCategories |> Html.map eventCategoryToMsg

        eventsView =
            model.events
                |> List.filter (.category >> (\cat -> EventCategory.isEventCategorySelected cat model.selectedEventCategories))
                |> List.map Event.view
                |> div []
                |> Html.map never

        reposView =
            model.repos
                |> Repo.sortByStars
                |> List.take 5
                |> List.map Repo.view
                |> div []
    in
    div []
        [ PersonalDetails.view model.personalDetails
        , h2 [] [ text "Experience" ]
        , eventCategoriesView
        , eventsView
        , h2 [] [ text "My top repos" ]
        , reposView
        ]
