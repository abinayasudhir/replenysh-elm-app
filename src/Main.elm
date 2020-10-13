module Main exposing (..)

import Browser
import Replenysh.State exposing (init, update)
import Replenysh.Types exposing (Model, Msg(..))
import Replenysh.View exposing (view)


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
