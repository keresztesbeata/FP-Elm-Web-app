module Model.PersonalDetails exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, classList, id, href, style, src, alt)


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
        pictureFormatter = 
            [ 
                style "width" "100%" , 
                style "opacity" "0.5", 
                style "margin-top" "10%", 
                style "margin-right" "10%"
            ]

        columnsFormatter = 
            [ 
                style "column-count" "2", 
                style "column-gap" "5%", 
                style "color" "white" 
            ]

        cardLayoutFormatter = 
            [ 
                style "text-align" "right", 
                style "margin-left" "auto", 
                style "margin-right" "0%", 
                style "width" "fit-content", 
                style "margin-top" "50%"
            ] 

        linkFormatter = 
            [ 
                style "text-decoration" "none", 
                style "color" "#FF1493"
            ]

        footerFormatter = 
            [ 
                style "text-align" "left", 
                style "margin-bottom" "0%", 
                style "margin-top" "20%"
            ]

        detailWithNameView: String -> Html msg -> Html msg
        detailWithNameView detailName detailComponent = 
            div 
                [
                    style "display" "flex", 
                    style "justify-content" "flex-start"
                ] 
                [ 
                    text <| detailName ++ ":" , 
                    detailComponent 
                ]
        
        contactView contact = detailWithNameView contact.name <| div [class "contact-detail"] [text contact.detail]
        socialLinkView socialLink = detailWithNameView socialLink.name <| a ((++) [class "social-link", href socialLink.detail] <| linkFormatter) [text socialLink.detail]
        
        {name, contacts, intro, socials} = details
    in
    div columnsFormatter
    [
        div cardLayoutFormatter
        [   
            h1 [id "name"] [text <| String.toUpper name]
            , em [id "intro" ] [text intro]
        ]
        , div footerFormatter [
            p [] [text "Contact me:"]
            , p [] (List.map contactView contacts)
            , p [] [text "You can find me also on:"]
            , p [] (List.map socialLinkView socials)
        ]
        , img ([alt "Profile picture", src "/images/profilePicture.jpg"] ++ pictureFormatter) []
    ]