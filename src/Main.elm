module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style, value, type_, checked)
import Html.Events exposing (..)
import Http
import Json.Decode as De
import Model exposing (..)
import Model.Event as Event
import Model.Event.Category as EventCategory
import Model.PersonalDetails as PersonalDetails
import Model.Repo as Repo

type Msg
    = GetRepos
    | GotRepos (Result Http.Error (List Repo.Repo))
    | SelectEventCategory EventCategory.EventCategory
    | DeselectEventCategory EventCategory.EventCategory
    | SelectRepoSortField String
    | SelectRepoSortOrder Bool

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

{- Fetch the repositories from the hardcoded url, and decode the list of json objects using the predefined decoder.
-}
fetchRepos: Cmd Msg
fetchRepos =     
    Http.get
        { url = "https://api.github.com/users/keresztesbeata/repos"
        , expect = Http.expectJson GotRepos (De.list Repo.decodeRepo)
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
            -- in case the result returns an error, no repos will be added to the model
            ( {model | repos = model.repos ++ (Result.withDefault [] res)}
            , Cmd.none )    

        SelectEventCategory category ->
            ( {model | selectedEventCategories = EventCategory.set category True model.selectedEventCategories}, Cmd.none)

        DeselectEventCategory category ->
            ( {model | selectedEventCategories = EventCategory.set category False model.selectedEventCategories}, Cmd.none)

        SelectRepoSortOrder sortOrder ->
            ( {model | repoSortOrder = sortOrder}, Cmd.none)
            
        SelectRepoSortField sortFieldName ->
            ( {model | repoSortField = Repo.stringToSortField sortFieldName}, Cmd.none)

eventCategoryToMsg : ( EventCategory.EventCategory, Bool ) -> Msg
eventCategoryToMsg ( event, selected ) =
    if selected then
        SelectEventCategory event

    else
        DeselectEventCategory event


view : Model -> Html Msg
view model =
    let
        -- define style attributes
        pageFormatter = 
            [
                style "background-color" "#2B2D2F", 
                style "margin-top" "0%",
                style "margin-bottom" "0%"   
            ]

        centerAlign = 
            [ 
                style "margin-left" "auto", 
                style "margin-right" "auto", 
                style "width" "75%" 
            ]

        spaceBetweenSections = 
            [ 
                style "margin-top" "5%", 
                style "margin-bottom" "5%" 
            ]

        sectionHeader = 
            [
                style "color" "white", 
                style "font-weight" "bold", 
                style "font-size" "200%",
                style "text-align" "center"
            ]

        dropDownFormatter = 
            [ 
                style "background-color" "#ccccff", 
                style "text-align" "center",
                style "width" "25%",
                style "font-size" "101%",
                style "border-radius" "10px"
            ]

        inLineCenterAlignFormatter = 
            [
                style "display" "flex",
                style "justify-content" "center" 
            ]
            
        sideLinesFormatter = 
            [
                style "border-left" "2px solid grey",
                style "border-right" "2px solid grey"
            ]

        eventCategoriesView =
            EventCategory.view model.selectedEventCategories |> Html.map eventCategoryToMsg

        repoSortFieldDropDownView =
            select ([ Html.Events.onInput SelectRepoSortField ] ++ dropDownFormatter)
                [ option [ value <| Repo.sortFieldToString Repo.Stars ] [ text <| Repo.sortFieldToString Repo.Stars ]
                , option [ value <| Repo.sortFieldToString Repo.Name ] [ text <| Repo.sortFieldToString Repo.Name ]
                , option [ value <| Repo.sortFieldToString Repo.PushedAt ] [ text <| Repo.sortFieldToString Repo.PushedAt ]
                ]

        sortOrderCheckBoxView =
            let
                labelFormatter = [ style "color" "white", style "font-size" "110%"]
            in
            div labelFormatter [input [ type_ "checkbox", onCheck SelectRepoSortOrder, checked model.repoSortOrder] [], text "ASC" ]                

        eventsView =
            model.events
                |> List.filter (.category >> (\cat -> EventCategory.isEventCategorySelected cat model.selectedEventCategories))
                |> List.map Event.view
                |> div (centerAlign ++ sideLinesFormatter)
                |> Html.map never

        reposView =
            model.repos
                |> Repo.sortByFieldInOrder model.repoSortField model.repoSortOrder
                |> List.take 5
                |> List.map Repo.view
                |> div (centerAlign ++ sideLinesFormatter)

    in
    div pageFormatter
    [
        div centerAlign
        [ PersonalDetails.view model.personalDetails
        , hr spaceBetweenSections []
        , h2 sectionHeader [ text "Experience" ]
        , eventCategoriesView
        , eventsView
        , hr spaceBetweenSections []
        , h2 sectionHeader [ text "My top repos" ]
        , div inLineCenterAlignFormatter
        [
            div [style "color" "white"] [text "Sort by :"]
            , repoSortFieldDropDownView
            , sortOrderCheckBoxView
        ], reposView
        ]
    ]

