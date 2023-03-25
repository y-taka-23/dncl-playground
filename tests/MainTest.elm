module MainTest exposing (suite)

import Expect
import Main
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "greet"
        [ test "appends the name to the message" <|
            \_ ->
                Main.greet "Elm"
                    |> Expect.equal "Hello, Elm!"
        ]
