module Model.PersonalDetails exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, classList, id, href)


type alias DetailWithName =
    { name : String
    , detail : String
    }


type alias PersonalDetails =
    { name : String
    , contacts : List DetailWithName
    , intro : String
    , socials : List DetailWithName
    }


view : PersonalDetails -> Html msg
view details =
    let 
        contactView contact = p [class "contact-detail"] [text (contact.name ++ " : " ++ contact.detail)]
        socialLinkView socialLink = a [class "social-link", href socialLink.detail] [text socialLink.name]
        {name, contacts, intro, socials} = details
    in
        div []
        [   
            h1 [id "name"] [text name]
            , p [] (List.map contactView contacts)
            , em [id "intro"] [text intro]
            , p [] (List.map socialLinkView socials)
        ]    