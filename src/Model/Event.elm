module Model.Event exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, classList, href, style)
import Model.Event.Category exposing (EventCategory(..), eventCategoryToString)
import Model.Interval as Interval exposing (Interval)

type alias Event =
    { title : String
    , interval : Interval
    , description : Html Never
    , category : EventCategory
    , url : Maybe String
    , tags : List String
    , important : Bool
    }

{- Sort events by interval length in ascending order
-}
sortByInterval : List Event -> List Event
sortByInterval events =
    List.sortWith (\event1 event2 -> Interval.compare event1.interval event2.interval) events

view : Event -> Html Never
view event =
    let
        selectColorByEventType eventCategory = 
            case eventCategory of
                Academic ->
                    "rgb(134, 45, 89, 0.5)"

                Award ->
                    "rgb(217, 255, 179, 0.5)"

                Work ->
                    "rgb(0, 230, 184, 0.5)"

                Project ->
                    "rgb(255, 187, 51, 0.5)"

        eventCardFormatter eventCategory = 
            [ 
                style "background-color" <| selectColorByEventType eventCategory, 
                style "border-radius" "10px", 
                style "color" "white",
                style "opacity" "0.8",
                style "text-align" "center",  
                style "padding-top" "5%", 
                style "padding-bottom" "2%", 
                style "margin" "5% 5% 0% 5%",
                style "padding-left" "5%",
                style "padding-right" "5%"
            ]

        linkFormatter = 
            [ 
                style "text-decoration" "none", 
                style "color" "#ff6699"
            ]

        viewTag tag = p [] [text tag]
        

        {title, interval, description, category, url, tags, important} = event
    in
    div ([classList [("event", True), ("event-important", important)]] ++ (eventCardFormatter category))
    [
        h3 [class "event-title"] [text title]
        , div [class "event-interval"] [ Interval.view interval ]
        , em [class "event-description"] [description]
        , p [class "event-category"] [text <| eventCategoryToString category]
        , a ([href <| Maybe.withDefault "#" url, class "event-url"] ++ linkFormatter) [text "Goto event"]
        , ul [] (List.map viewTag tags)
    ]
