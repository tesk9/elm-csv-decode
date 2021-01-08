module Spec exposing (suite)

import Csv.Decode as Decode
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "Csv.Decode"
        [ describe "andThen" andThenSpec
        ]


andThenSpec : List Test
andThenSpec =
    let
        decoder =
            Decode.assertField "site" "blog"
                |> Decode.andMap (Decode.field "id" (String.toInt >> Ok))
                |> Decode.andThen
                    (\maybeInt ->
                        case maybeInt of
                            Just id ->
                                if id > 0 then
                                    Decode.succeed (Just id)

                                else
                                    Decode.fail "id must be greater than zero"

                            Nothing ->
                                Decode.fail "id must be an int"
                    )
    in
    [ test "fails" <|
        \() ->
            { headers = [ "site", "id" ]
            , records = [ [ "blog", "-2" ] ]
            }
                |> Decode.decodeCsv decoder
                |> Expect.equal
                    (Err
                        (Decode.DecodeErrors
                            [ ( 0, "id must be greater than zero" ) ]
                        )
                    )
    ]
