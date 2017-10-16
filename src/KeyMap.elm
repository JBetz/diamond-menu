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


keyMap : KeyLayout -> KeyMap
keyMap layout =
    case layout of
        Qwerty ->
            Dict.fromList [ ( w, 0 ), ( q, 1 ), ( e, 2 ), ( a, 3 ), ( s, 4 ), ( d, 5 ), ( z, 6 ), ( c, 7 ), ( x, 8 ) ]

        Dvorak ->
            Dict.fromList [ ( 188, 0 ), ( 222, 1 ), ( 190, 2 ), ( 65, 3 ), ( 79, 4 ), ( 69, 5 ), ( 59, 6 ), ( 74, 7 ), ( 88, 8 ) ]

        Azerty ->
            Dict.fromList [ ( 90, 0 ), ( 65, 1 ), ( 69, 2 ), ( 81, 3 ), ( 83, 4 ), ( 68, 5 ), ( 90, 6 ), ( 67, 7 ), ( 88, 8 ) ]

        Qwertz ->
            Dict.fromList [ ( 87, 0 ), ( 81, 1 ), ( 69, 2 ), ( 65, 3 ), ( 83, 4 ), ( 68, 5 ), ( 89, 6 ), ( 67, 7 ), ( 88, 8 ) ]


a =
    65


b =
    66


c =
    67


d =
    68


e =
    69


f =
    70


g =
    71


h =
    72


i =
    73


j =
    74


k =
    75


l =
    76


m =
    77


n =
    78


o =
    79


p =
    80


q =
    81


r =
    82


s =
    83


t =
    84


u =
    85


v =
    86


w =
    87


x =
    88


y =
    89


z =
    90


shift =
    16


ctrl =
    17


alt =
    18
