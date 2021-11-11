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
                    "#f5d6eb"

                Award ->
                    "#d9ffb3"

                Work ->
                    "#ccffff"

                Project ->
                    "#ffd480"

        eventCardFormatter eventCategory = 
            [ 
                style "background-color" <| selectColorByEventType eventCategory, 
                style "opacity" "0.75", 
                style "border-radius" "10px", 
                style "text-align" "center",  
                style "padding-top" "5%", 
                style "padding-bottom" "2%", 
                style "margin" "5% 5% 0% 5%"
            ]

        linkFormatter = 
            [ 
                style "text-decoration" "none", 
                style "color" "#FF1493"
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
