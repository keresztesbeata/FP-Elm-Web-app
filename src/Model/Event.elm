module Model.Event exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, classList, href)
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
        viewTag tag = p [] [text tag]
        {title, interval, description, category, url, tags, important} = event
    in
    div [classList [("event", True), ("event-important", important)]]
    [
        p [class "event-title"] [text title]
        , div [class "event-interval"] [ Interval.view interval ]
        , p [class "event-description"] [description]
        , p [class "event-category"] [text <| eventCategoryToString category]
        , a [href <| Maybe.withDefault "#" url, class "event-url"] [text "Goto event"]
        , ul [] (List.map viewTag tags)
    ]
