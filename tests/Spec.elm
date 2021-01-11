module Spec exposing (suite)

import Csv.Decode as Decode
import Expect
import Result
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Csv.Decode"
        [ describe "andThen" andThenSpec
        ]


andThenSpec : List Test
andThenSpec =
    let
        idResult : Int -> Decode.Decoder a Int
        idResult id =
            if id > 0 then
                Decode.succeed id

            else
                Decode.fail "id must be greater than zero"

        idDecoder : Decode.Decoder (Int -> Int) Int
        idDecoder =
            Decode.assertField "site" "blog"
                |> Decode.andMap (Decode.field "id" (String.toInt >> Result.fromMaybe "id must be an int"))
                |> Decode.andThen idResult
    in
    [ test "succeeds" <|
        \() ->
            { headers = [ "site", "id" ]
            , records = [ [ "blog", "1" ] ]
            }
                |> Decode.decodeCsv idDecoder
                |> Expect.equal (Ok [ 1 ])
    , test "fails on range out-of-bounds" <|
        \() ->
            { headers = [ "site", "id" ]
            , records = [ [ "blog", "-2" ] ]
            }
                |> Decode.decodeCsv idDecoder
                |> Expect.equal
                    (Err
                        (Decode.DecodeErrors
                            [ ( 0, "id must be greater than zero" ) ]
                        )
                    )
    , test "fails on not an int error without changing incoming error" <|
        \() ->
            { headers = [ "site", "id" ]
            , records = [ [ "blog", "zesty" ] ]
            }
                |> Decode.decodeCsv idDecoder
                |> Expect.equal
                    (Err
                        (Decode.DecodeErrors
                            [ ( 0, "id must be an int" ) ]
                        )
                    )
    , test "combines values at multiple headers" <|
        \() ->
            let
                validateFields ( site, idStr ) =
                    if String.trim site == "" then
                        Decode.fail "site cannot be blank"

                    else
                        case String.toInt idStr of
                            Just id ->
                                if id < 0 then
                                    Decode.fail "id must be greater than zero"

                                else
                                    Decode.succeed ( site, idStr )

                            Nothing ->
                                Decode.fail "id must be an int"

                decoder =
                    Decode.field "site" Ok
                        |> Decode.andMap (Decode.field "id" Ok)
                        |> Decode.map (\site id -> ( site, id ))
                        |> Decode.andThen validateFields
            in
            { headers = [ "site", "id" ]
            , records = [ [ "blog", "1" ], [ "about", "2" ] ]
            }
                |> Decode.decodeCsv decoder
                |> Expect.equal (Ok [ ( "blog", "1" ), ( "about", "2" ) ])
    ]
