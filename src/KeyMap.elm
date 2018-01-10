module KeyMap exposing (KeyMap, KeyLayout(..), keyMap)

{-|
This module is for defining mappings between keyboard layouts and menu selection
keys.

# Model
@docs KeyMap, KeyLayout, keyMap

-}

import Dict exposing (Dict(..))
import Keyboard exposing (KeyCode)
import Keyboard.Extra exposing (Key(..), toCode)

{-| -}
type alias KeyMap =
    Dict KeyCode Int

{-| Currently supported keyboard layouts. If the layout you need isn't on this list,
create a mapping using the existing source as an example, and open a PR for it to be
added to the library.

-}
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
