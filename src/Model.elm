module Model exposing (..)

import Html exposing (b, div, p, text)
import Model.Date as Date
import Model.Event as Event exposing (Event)
import Model.Event.Category exposing (EventCategory(..), SelectedEventCategories, allSelected)
import Model.Interval as Interval
import Model.PersonalDetails exposing (DetailWithName, PersonalDetails)
import Model.Repo exposing (Repo, SortField(..))


type alias Model =
    { personalDetails : PersonalDetails
    , events : List Event
    , selectedEventCategories : SelectedEventCategories
    , repos : List Repo
    , repoSortField : SortField
    , repoSortOrder : Bool
    }


academicEvents : List Event
academicEvents =
    [ { title = "Silvania High School, Mathematics - Informatics Profile"
      , interval = Interval.withDurationYears (Date.onlyYear 2015) 4
      , description = p [] [ text "I obtained ", b [] [ text "very" ], text " good grades." ]
      , category = Academic
      , url = Nothing
      , tags = []
      , important = False
      }
    , { title = "Technical University of Cluj-Napoca, Faculty of Computer Science"
      , interval = Interval.open (Date.onlyYear 2019)
      , description = div [] []
      , category = Academic
      , url = Nothing
      , tags = []
      , important = False
      }
    ]


workEvents : List Event
workEvents =
    [ { title = "Internship"
      , interval = Interval.withDurationMonths 2021 Date.Jul 2
      , description = text "Full Stack Software Engineer (Java & Angular JS) - MicroFocus"
      , category = Work
      , url = Just "https://jobs.microfocus.com/global/en/job/7021598/Full-Stack-Software-Engineer"
      , tags = []
      , important = False
      }
    ]


projectEvens : List Event
projectEvens =
    [ { title = "Online Learning Management System"
      , interval = Interval.withDurationMonths 2020 Date.Dec 1
      , description = text "Web application in Php"
      , category = Project
      , url = Just "https://github.com/keresztesbeata/Online_learning_management_system"
      , tags = []
      , important = True
      }
    , { title = "Team & Project Management App"
      , interval = Interval.withDurationMonths 2020 Date.Nov 1
      , description = text <| "Java desktop application, front-end in Spring. In collaboration with: Fazakas Borbála. "
      , category = Project
      , url = Just "https://github.com/keresztesbeata/Project-Management-App-F-K"
      , tags = []
      , important = True
      }
    , { title = "ATM (Automated Teller Machine)"
      , interval = Interval.withDurationMonths 2020 Date.Oct 4
      , description = text "VHDL application implemented and tested on Basys3 dev board. In collaboration with: Fazakas Borbála. "
      , category = Project
      , url = Just "https://github.com/keresztesbeata/ATM"
      , tags = []
      , important = True
      }
    , { title = "Relational Databases"
      , interval = Interval.withDurationMonths 2020 Date.Sep 5
      , description = text "Relational, normalized database in SQLite, MsSQL and SQLServer, with additional queries in SQL for managing the stored data."  
      , category = Project
      , url = Just "https://github.com/keresztesbeata/Relational-Databases"
      , tags = []
      , important = False
      }
    ]


personalDetails : PersonalDetails
personalDetails =
    { name = "Keresztes Beáta"
    , intro = "I am a Computer Science undergraduate student, passionate about programming and mathematics."
    , contacts = [ DetailWithName "email" "keresztesbeata00@yahoo.com"]
    , socials = [ DetailWithName "github" "https://github.com/keresztesbeata"]
    }


initModel : Model
initModel =
    { personalDetails = personalDetails
    , events = Event.sortByInterval <| academicEvents ++ workEvents ++ projectEvens
    , selectedEventCategories = allSelected
    , repos = []
    , repoSortField = Stars
    , repoSortOrder = False -- descending order
    }
