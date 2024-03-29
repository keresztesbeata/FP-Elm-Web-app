module Model.Event.Category exposing (EventCategory(..), SelectedEventCategories, allSelected, eventCategories, isEventCategorySelected, set, view, eventCategoryToString)

import Html exposing (Html, div, input, text)
import Html.Attributes exposing (checked, class, style, type_)
import Html.Events exposing (onCheck)

type EventCategory
    = Academic
    | Work
    | Project
    | Award


eventCategories =
    [ Academic, Work, Project, Award ]


{-| Type used to represent the state of the selected event categories
-}
type SelectedEventCategories
    = SelectedEventCategories ( List (EventCategory, Bool) )


{-| Returns an instance of `SelectedEventCategories` with all categories selected

    isEventCategorySelected Academic allSelected --> True

-}
allSelected : SelectedEventCategories
allSelected = 
    eventCategories
    |> List.map (\eventCategory -> (eventCategory, True))
    |> SelectedEventCategories

{-| Given a the current state and a `category` it returns whether the `category` is selected.

    isEventCategorySelected Academic allSelected --> True

-}
isEventCategorySelected : EventCategory -> SelectedEventCategories -> Bool
isEventCategorySelected category (SelectedEventCategories current) =
    List.any (\(eventCategory, selected) -> eventCategory == category && selected) current

{-| Given an `category`, a boolean `value` and the current state, it sets the given `category` in `current` to `value`.

    allSelected |> set Academic False |> isEventCategorySelected Academic --> False

    allSelected |> set Academic False |> isEventCategorySelected Work --> True

-}
set : EventCategory -> Bool -> SelectedEventCategories -> SelectedEventCategories
set category value (SelectedEventCategories current) =
    current
    |> List.map (
        \(eventCategory, selected) -> 
            if eventCategory == category then 
                (eventCategory, value) 
            else 
                (eventCategory, selected)
                ) 
    |> SelectedEventCategories 

checkbox : String -> Bool -> EventCategory -> Html ( EventCategory, Bool )
checkbox name state category =
    div [ 
            style "display" "inline", 
            class "category-checkbox" 
        ] 
        [ input [ type_ "checkbox", onCheck (\c -> ( category, c )), checked state ] []
        , text name
        ]


view : SelectedEventCategories -> Html ( EventCategory, Bool )
view (SelectedEventCategories model) =
    let
        checkboxRowFormatter = 
            [   
                style "color" "white", 
                style "font-size" "125%", 
                style "display" "flex", 
                style "flex-direction" "row", 
                style "justify-content" "space-around" 
            ]
    in
    div checkboxRowFormatter 
    (List.map (\(category, selected) -> checkbox (eventCategoryToString category) selected category) model)
    
    
eventCategoryToString: EventCategory -> String
eventCategoryToString eventCategory =
    case eventCategory of
        Academic ->
            "Academic"

        Work ->
            "Work"

        Project ->
            "Project"            

        Award ->
            "Award"
