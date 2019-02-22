module SpannedString exposing (SpannedString, span)

{-| This is a tiny package that allows you to span substrings in a given string.


# Definition

@docs SpannedString, span

-}


{-| A `SpannedString` is just an alias to `List (String, Bool)`.
-}
type alias SpannedString =
    List ( String, Bool )


{-| Create a `SpannedString` from a given string and a list of indices.

    span "Hello world!" [ 0, 1, 2, 3, 4 ] == [ ( "Hello", True ), ( " world!", False ) ]

    span "47 Rue Falguière, Paris" [ 8, 9, 10 ] == [ ( "47 Rue ", False ), ( "Fal", True ), ( "guière, Paris", False ) ]

-}
span : String -> List Int -> SpannedString
span string indices =
    indices
        |> List.foldl spanIndex (fromString string)
        |> List.foldr reduce []


type alias SpannableString =
    List ( Int, SpannableChar )


reduce : ( Int, SpannableChar ) -> SpannedString -> SpannedString
reduce ( _, sc ) ss =
    case ss of
        [] ->
            [ ( String.fromChar sc.value, sc.tag ) ]

        [ x ] ->
            if sc.tag == Tuple.second x then
                [ Tuple.mapFirst (String.cons sc.value) x ]

            else
                [ ( String.fromChar sc.value, sc.tag ), x ]

        head :: tail ->
            if sc.tag == Tuple.second head then
                Tuple.mapFirst (String.cons sc.value) head :: tail

            else
                ( String.fromChar sc.value, sc.tag ) :: head :: tail


type alias SpannableChar =
    { value : Char
    , tag : Bool
    }


fromString : String -> SpannableString
fromString string =
    string
        |> String.toList
        |> List.indexedMap toSpannableChar


toSpannableChar : Int -> Char -> ( Int, SpannableChar )
toSpannableChar index char =
    ( index, { value = char, tag = False } )


spanIndex : Int -> SpannableString -> SpannableString
spanIndex index string =
    let
        tag ( i, s ) =
            if i == index then
                ( i, { s | tag = True } )

            else
                ( i, s )
    in
    List.map tag string
