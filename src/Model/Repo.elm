module Model.Repo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href)
import Json.Decode as De
import Model.Date

type alias Repo =
    { name : String
    , description : Maybe String
    , url : String
    , pushedAt : String
    , stars : Int
    }

type SortField = Name | Stars | PushedAt

sortFieldToString: SortField -> String
sortFieldToString sortField = 
    case sortField of
        Name ->
            "Name"

        PushedAt ->    
            "Pushed at"

        Stars ->
            "Stars"

stringToSortField: String -> SortField
stringToSortField sortFieldName =
    case sortFieldName of
        "Pushed at" ->
            PushedAt

        "Stars" ->
            Stars

        _ ->
            Name


view : Repo -> Html msg
view repo =
    let
        {name, description, url, pushedAt, stars} = repo
    in
    div [class "repo"]
    [
        p [class "repo-name"] [text name]
        , p [class "repo-description"] [ text <| Maybe.withDefault "" <| description]
        , p [class "repo-url"] [ a [href url] [text "Goto repository"] ]
        , p [] [text pushedAt]
        , p [class "repo-stars"] [text <| String.fromInt <| stars]
    ]

{- Sort the repositories by stars in ascending order
-}
sortByStars : List Repo -> List Repo
sortByStars repos =
    List.sortBy .stars repos

{- Sort the repositories by name in alphabetical order
-}
sortByName : List Repo -> List Repo
sortByName repos =
    List.sortBy .name repos

{- Sort the repositories by pushedAt dates in ascending order
-}
sortByPushedAtDate : List Repo -> List Repo
sortByPushedAtDate repos =
    List.sortWith (\repo1 repo2 -> compare repo1.pushedAt repo2.pushedAt) repos

{- Sort the list of repos by the given field ( by default in ascending order)
-}
sortByField : SortField -> List Repo -> List Repo
sortByField sortField repos =
    case sortField of
        PushedAt -> 
            sortByPushedAtDate repos

        Stars -> 
            sortByStars repos
        
        Name ->
            sortByName repos

{- Sort the list by the given field and consider the sort order as well.
    order:
    - True => ascending 
    - False => descending
-}
sortByFieldInOrder : SortField -> Bool -> List Repo -> List Repo
sortByFieldInOrder sortField sortOrder list=
    list
    |> sortByField sortField
    |> if sortOrder then identity else List.reverse 

{-| Deserializes a JSON object to a `Repo`.
Field mapping (JSON -> Elm):

  - name -> name
  - description -> description
  - html\_url -> url
  - pushed\_at -> pushedAt
  - stargazers\_count -> stars

-}
decodeRepo : De.Decoder Repo
decodeRepo =
    De.map5 Repo
        (De.field "name" De.string)
        (De.field "description" (De.maybe De.string))
        (De.field "url" De.string)
        (De.field "pushed_at" De.string)
        (De.field "stargazers_count" De.int)

