module KeyMap exposing (KeyMap, KeyLayout, keyMap)

{-|
This module is for defining mappings between keyboard layouts and menu selection
characters.

# Model
@docs KeyMap, KeyLayout, keyMap

-}

import Dict exposing (Dict(..))
import Keyboard exposing (KeyCode)
import Keyboard.Extra exposing (Key(..), toCode)

{-| -}
type alias KeyMap =
    Dict KeyCode Int

{-| -}
type KeyLayout
    = Qwerty
    | Dvorak
    | Azerty
    | Qwertz

{-| -}
keyMap : KeyLayout -> KeyMap
keyMap layout =
    case layout of
        Qwerty ->
            mkMap [ CharW, CharQ, CharE, CharA, CharS, CharD, CharZ, CharC, CharX ]

        Dvorak ->
            mkMap [ Comma, Quote, Period, CharA, CharO, CharE, Semicolon, CharJ, CharQ ]

        Azerty ->
            mkMap [ CharZ, CharA, CharE, CharQ, CharS, CharD, CharW, CharC, CharX ]

        Qwertz ->
            mkMap [ CharW, CharQ, CharE, CharA, CharS, CharD, CharY, CharC, CharX ]


mkMap : List Key -> KeyMap
mkMap keys =
    Dict.fromList <|
        List.map2 (,) (List.map toCode keys) (List.range 0 8)
