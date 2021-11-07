module Model.Repo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href)
import Json.Decode as De


type alias Repo =
    { name : String
    , description : Maybe String
    , url : String
    , pushedAt : String
    , stars : Int
    }


view : Repo -> Html msg
view repo =
    let
        {name, description, url, pushedAt, stars} = repo
    in
    div [class "repo"]
    [
        p [class "repo-name"] [text name]
        , p [class "repo-description"] [text (description |> Maybe.withDefault "")]
        , p [class "repo-url"] [ a [href url] [text "Link to repo"] ]
        , p [] [text pushedAt]
        , p [class "repo-stars"] [text (String.fromInt stars)]
    ]


sortByStars : List Repo -> List Repo
sortByStars repos =
    -- sort the repositories by stars in descending order
    List.sortWith (\x y -> compare y.stars x.stars) repos


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
