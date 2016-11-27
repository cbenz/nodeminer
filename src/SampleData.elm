module SampleData exposing (..)

import Tree exposing (node, Tree)


sampleTree : Tree
sampleTree =
    node "ROOT"
        0
        [ node "Inbox"
            1
            [ node "Faire les courses #urgent" 2 []
            , node "Aller au cinéma http://allocine.com/" 3 []
            , node "Rappeler ces personnes"
                4
                [ node "@Bruno" 5 []
                , node "@Laëtitia" 6 []
                ]
            ]
        , node "Projets"
            7
            [ node "NoteMiner" 8 []
            ]
        ]
