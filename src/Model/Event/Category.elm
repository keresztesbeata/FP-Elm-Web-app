module Model.Event.Category exposing (EventCategory(..), SelectedEventCategories, allSelected, eventCategories, isEventCategorySelected, set, view)

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
    |> List.map (\e -> (e, True))
    |> SelectedEventCategories

{-| Given a the current state and a `category` it returns whether the `category` is selected.

    isEventCategorySelected Academic allSelected --> True

-}
isEventCategorySelected : EventCategory -> SelectedEventCategories -> Bool
isEventCategorySelected category (SelectedEventCategories current) =
    List.any (\(c, selected) -> c == category && selected) current


{-| Given an `category`, a boolean `value` and the current state, it sets the given `category` in `current` to `value`.

    allSelected |> set Academic False |> isEventCategorySelected Academic --> False

    allSelected |> set Academic False |> isEventCategorySelected Work --> True

-}
set : EventCategory -> Bool -> SelectedEventCategories -> SelectedEventCategories
set category value (SelectedEventCategories current) =
    let 
        containsElem elem list = 
            list
                |> List.map Tuple.first
                |> List.any (\first -> first == elem)
    in
    if containsElem category current then
            SelectedEventCategories (List.map (\(c, selected) -> if c == category then (c, value) else (c, selected)) current)
        else
            SelectedEventCategories ((category, value) :: current)

checkbox : String -> Bool -> EventCategory -> Html ( EventCategory, Bool )
checkbox name state category =
    div [ style "display" "inline", class "category-checkbox" ]
        [ input [ type_ "checkbox", onCheck (\c -> ( category, c )), checked state ] []
        , text name
        ]


view : SelectedEventCategories -> Html ( EventCategory, Bool )
view (SelectedEventCategories model) =
    div [] 
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
