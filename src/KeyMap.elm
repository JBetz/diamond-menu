module KeyMap exposing (..)

import Char exposing (KeyCode)
import Dict exposing (Dict(..))


type alias KeyMap =
    Dict KeyCode Int


type KeyLayout
    = Qwerty
    | Dvorak
    | Azerty
    | Qwertz



-- QWERTY
-- qwe
-- asd
-- zxc
--
-- DVORAK
-- ',.
-- aoe
-- ;qj
--
-- AZERTY
-- aze
-- qsd
-- wxc
--
-- QWERTZ
-- qwe
-- asd
-- yxc


keyMap : KeyLayout -> KeyMap
keyMap layout =
    case layout of
        Qwerty ->
            Dict.fromList [ ( 87, 0 ), ( 81, 1 ), ( 69, 2 ), ( 65, 3 ), ( 83, 4 ), ( 68, 5 ), ( 90, 6 ), ( 67, 7 ), ( 88, 8 ) ]

        Dvorak ->
            Dict.fromList [ ( 188, 0 ), ( 222, 1 ), ( 190, 2 ), ( 65, 3 ), ( 79, 4 ), ( 69, 5 ), ( 59, 6 ), ( 74, 7 ), ( 88, 8 ) ]

        Azerty ->
            Dict.fromList [ ( 90, 0 ), ( 65, 1 ), ( 69, 2 ), ( 81, 3 ), ( 83, 4 ), ( 68, 5 ), ( 90, 6 ), ( 67, 7 ), ( 88, 8 ) ]

        Qwertz ->
            Dict.fromList [ ( 87, 0 ), ( 81, 1 ), ( 69, 2 ), ( 65, 3 ), ( 83, 4 ), ( 68, 5 ), ( 89, 6 ), ( 67, 7 ), ( 88, 8 ) ]
