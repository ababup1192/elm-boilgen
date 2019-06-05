module Main exposing
    ( DbField(..)
    , Model
    , Msg(..)
    , dbFieldArrayToDDL
    , dbFieldArrayToInsertMethod
    , init
    , main
    , update
    , view
    )

import Array exposing (Array)
import Array.Extra as Array
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import String.Interpolate exposing (interpolate)
import Task as Task



---- MODEL ----


type alias Model =
    { columns : Array DbField }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Array.empty, Cmd.none )


{-| -TODO: FK制約を増やす。 UI込み -
-}
type alias BigInt_ =
    { fieldName : String
    , fieldLength : Int
    , isUnsigned : Bool
    , isNotNull : Bool
    }


type alias VarChar_ =
    { fieldName : String
    , fieldLength : Int
    , isNotNull : Bool
    }


type alias Boolean_ =
    { fieldName : String
    , isNotNull : Bool
    }


type alias Datetime_ =
    { fieldName : String
    , isNotNull : Bool
    }


type alias Enum_ =
    { fieldName : String
    , values : List String
    , isNotNull : Bool
    }


type DbField
    = PrimaryKey String
    | BigInt BigInt_
    | VarChar VarChar_
    | Boolean Boolean_
    | Datetime Datetime_
    | Enum Enum_


{-| -TODO: DDLに変換作業 TDD
-}
dbFieldToDDL : DbField -> String
dbFieldToDDL dbField =
    case dbField of
        PrimaryKey fieldName ->
            "`" ++ fieldName ++ "` bigint(20) unsigned NOT NULL AUTO_INCREMENT"

        BigInt { fieldName, fieldLength, isUnsigned, isNotNull } ->
            interpolate "`{0}` bigint({1}){2}{3}"
                [ fieldName
                , String.fromInt fieldLength
                , visibleWord isUnsigned " unsigned"
                , visibleWord isNotNull " NOT NULL"
                ]

        VarChar { fieldName, fieldLength, isNotNull } ->
            interpolate "`{0}` varchar({1}){2}"
                [ fieldName
                , String.fromInt fieldLength
                , visibleWord isNotNull " NOT NULL"
                ]

        Boolean { fieldName, isNotNull } ->
            interpolate "`{0}` boolean{1}"
                [ fieldName
                , visibleWord isNotNull " NOT NULL"
                ]

        Datetime { fieldName, isNotNull } ->
            interpolate "`{0}` datetime(6){1}"
                [ fieldName
                , visibleWord isNotNull " NOT NULL"
                ]

        Enum { fieldName, values, isNotNull } ->
            interpolate "`{0}` enum({1}){2}"
                [ fieldName
                , values
                    |> List.map (\v -> "'" ++ v ++ "'")
                    |> String.join ", "
                , visibleWord isNotNull " NOT NULL"
                ]


dbFieldToFormatArg : DbField -> String
dbFieldToFormatArg dbField =
    let
        nullableWrapper isNotNull fieldName =
            if isNotNull then
                lowerCamel fieldName

            else
                "nullableTextToStr(" ++ lowerCamel fieldName ++ ")"
    in
    case dbField of
        PrimaryKey fieldName ->
            lowerCamel fieldName

        BigInt { fieldName } ->
            lowerCamel fieldName

        VarChar { fieldName, isNotNull } ->
            nullableWrapper isNotNull fieldName

        Boolean { fieldName, isNotNull } ->
            lowerCamel fieldName

        Datetime { fieldName, isNotNull } ->
            nullableWrapper isNotNull fieldName

        Enum { fieldName, isNotNull } ->
            nullableWrapper isNotNull fieldName


dbFieldToFieldName : DbField -> String
dbFieldToFieldName dbField =
    case dbField of
        PrimaryKey fieldName ->
            fieldName

        BigInt { fieldName } ->
            fieldName

        VarChar { fieldName } ->
            fieldName

        Boolean { fieldName } ->
            fieldName

        Datetime { fieldName } ->
            fieldName

        Enum { fieldName } ->
            fieldName


dbFieldToFormatString : DbField -> String
dbFieldToFormatString dbField =
    let
        textFormat isNotNull =
            if isNotNull then
                "'%s'"

            else
                "%s"
    in
    case dbField of
        VarChar { isNotNull } ->
            textFormat isNotNull

        Datetime { isNotNull } ->
            textFormat isNotNull

        Enum { isNotNull } ->
            textFormat isNotNull

        _ ->
            "%s"


dbFieldArrayToDDL : Array DbField -> String
dbFieldArrayToDDL dbFieldArray =
    let
        isPrimaryKey dbField =
            case dbField of
                PrimaryKey _ ->
                    True

                _ ->
                    False

        primaryTextArray =
            dbFieldArray
                |> Array.filter isPrimaryKey
                |> Array.get 0
                |> Maybe.andThen
                    (\dbField ->
                        case dbField of
                            PrimaryKey fieldName ->
                                Just <| Array.fromList [ "PRIMARY KEY (`" ++ fieldName ++ "`)" ]

                            _ ->
                                Nothing
                    )
                |> Maybe.withDefault Array.empty

        fieldTextArray =
            dbFieldArray
                |> Array.map dbFieldToDDL

        fieldTexts =
            Array.append fieldTextArray primaryTextArray
                |> Array.toList
                |> String.join ",\n\t"
    in
    "CREATE TABLE `table` (\n\t" ++ fieldTexts ++ "\n) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;"


dbFieldArrayToInsertMethod : String -> Array DbField -> String
dbFieldArrayToInsertMethod tableName dbFieldArray =
    let
        dbFieldList =
            dbFieldArray |> Array.toList

        args =
            dbFieldList
                |> List.map (dbFieldToFieldName >> lowerCamel >> (\n -> "String " ++ n))
                |> String.join ", "

        dbFieldNames =
            (dbFieldList
                |> List.map dbFieldToFieldName
                |> String.join ", "
            )
                ++ ", created_at, created_by, updated_at, updated_by, version_no"

        formatStrings =
            (dbFieldList
                |> List.map dbFieldToFormatString
                |> String.join ", "
            )
                ++ ",'2019-04-01', 1, '2019-04-01', 1, 1"

        formatArgs =
            dbFieldList |> List.map dbFieldToFormatArg |> String.join ", "
    in
    interpolate """private String create{0}By({1}) {
\treturn String.format("INSERT INTO `{2}` ({3}) " +
\t\t"VALUES " +
\t\t"({4});", {5});
}"""
        [ upperCamelize tableName
        , args
        , tableName
        , dbFieldNames
        , formatStrings
        , formatArgs
        ]


{-| -TODO: 見た目を作っていく
-}
dbFieldToView : DbField -> Html Msg
dbFieldToView dbField =
    case dbField of
        PrimaryKey fieldName ->
            article [ class "base_field" ]
                []

        BigInt { fieldName, fieldLength, isUnsigned, isNotNull } ->
            article [ class "base_field" ]
                []

        VarChar { fieldName, fieldLength, isNotNull } ->
            article [ class "base_field" ]
                []

        Boolean { fieldName, isNotNull } ->
            article [ class "base_field" ]
                []

        Datetime { fieldName, isNotNull } ->
            article [ class "base_field" ]
                []

        Enum { fieldName, values, isNotNull } ->
            article [ class "base_field" ]
                []



---- UPDATE ----


{-| -TODO: ひたすらに、DbField種類の各値の変更Msgと関数を増やしていく -
-}
updateBigIntFieldName : String -> DbField -> DbField
updateBigIntFieldName fieldName dbField =
    case dbField of
        BigInt bigInt ->
            BigInt { bigInt | fieldName = fieldName }

        _ ->
            dbField


type alias Index =
    Int


type Msg
    = UpdateBigIntFieldName Index String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ columns } as model) =
    case msg of
        UpdateBigIntFieldName idx fieldName ->
            ( { model
                | columns =
                    Array.update idx (updateBigIntFieldName fieldName) columns
              }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "elm boilgen"
    , body =
        [ div [ class "cp-layout-table" ]
            [ div []
                [ text "Field" ]
            , div []
                [ text "Type" ]
            , div []
                [ text "Length" ]
            , div []
                [ text "Unsigned" ]
            , div []
                [ text "Not Null" ]
            , div []
                [ text "Auto Increment" ]
            , div []
                [ input [ class "input", type_ "text", value "id" ]
                    []
                ]
            , div []
                [ div [ class "select" ]
                    [ select []
                        [ option []
                            [ text "PRIMARY KEY" ]
                        ]
                    ]
                ]
            , div []
                [ input [ class "input", type_ "number", value "20" ]
                    []
                ]
            , div []
                [ div [ class "checkbox" ]
                    [ label []
                        [ input [ attribute "checked" "", name "check1", type_ "checkbox" ]
                            []
                        , span []
                            []
                        ]
                    ]
                ]
            , div []
                [ div [ class "checkbox" ]
                    [ label []
                        [ input [ attribute "checked" "", name "check1", type_ "checkbox", checked True ]
                            []
                        , span []
                            []
                        ]
                    ]
                ]
            , div []
                [ div [ class "checkbox" ]
                    [ label []
                        [ input [ attribute "checked" "", name "check1", type_ "checkbox" ]
                            []
                        , span []
                            []
                        ]
                    ]
                ]
            , div []
                [ input [ class "input", type_ "text", value "hoge" ]
                    []
                ]
            , div []
                [ div [ class "select" ]
                    [ select []
                        [ option []
                            [ text "BIGINT" ]
                        ]
                    ]
                ]
            , div []
                [ input [ class "input", type_ "number", value "20" ]
                    []
                ]
            , div []
                [ div [ class "checkbox" ]
                    [ label []
                        [ input [ attribute "checked" "", name "check1", type_ "checkbox" ]
                            []
                        , span []
                            []
                        ]
                    ]
                ]
            , div []
                [ div [ class "checkbox" ]
                    [ label []
                        [ input [ attribute "checked" "", name "check1", type_ "checkbox" ]
                            []
                        , span []
                            []
                        ]
                    ]
                ]
            , div []
                []
            , div []
                [ input [ class "input", type_ "text", value "foo" ]
                    []
                ]
            , div []
                [ div [ class "select" ]
                    [ select []
                        [ option []
                            [ text "VARCHAR" ]
                        ]
                    ]
                ]
            , div []
                [ input [ class "input", type_ "text", value "100" ]
                    []
                ]
            , div []
                []
            , div []
                [ div [ class "checkbox" ]
                    [ label []
                        [ input [ attribute "checked" "", name "check1", type_ "checkbox" ]
                            []
                        , span []
                            []
                        ]
                    ]
                ]
            , div []
                []
            , div []
                [ input [ class "input", type_ "text", value "flag" ]
                    []
                ]
            , div []
                [ div [ class "select" ]
                    [ select []
                        [ option []
                            [ text "BOOLEAN" ]
                        ]
                    ]
                ]
            , div []
                []
            , div []
                []
            , div []
                [ div [ class "checkbox" ]
                    [ label []
                        [ input [ attribute "checked" "", name "check1", type_ "checkbox" ]
                            []
                        , span []
                            []
                        ]
                    ]
                ]
            , div []
                []
            , div []
                [ input [ class "input", type_ "text", value "time" ]
                    []
                ]
            , div []
                [ div [ class "select" ]
                    [ select []
                        [ option []
                            [ text "DATETIME" ]
                        ]
                    ]
                ]
            , div []
                [ input [ class "input", type_ "text", value "6" ]
                    []
                ]
            , div []
                []
            , div []
                [ div [ class "checkbox" ]
                    [ label []
                        [ input [ attribute "checked" "", name "check1", type_ "checkbox" ]
                            []
                        , span []
                            []
                        ]
                    ]
                ]
            , div []
                []
            , div []
                [ input [ class "input", type_ "text", value "hoge_status" ]
                    []
                ]
            , div [ class "enum" ]
                [ div [ class "layout-enum" ]
                    [ div [ class "select" ]
                        [ select []
                            [ option []
                                [ text "ENUM" ]
                            ]
                        ]
                    , input [ class "input", placeholder "new value...", type_ "text" ]
                        []
                    , button [ class "button" ]
                        [ text "Add" ]
                    ]
                , ul [ class "cp-layout-items-tag" ]
                    [ li []
                        [ text "UP" ]
                    , li []
                        [ text "LEFT" ]
                    , li []
                        [ text "RIGHT" ]
                    , li []
                        [ text "DOWN" ]
                    ]
                ]
            , div []
                []
            , div []
                []
            , div []
                [ div [ class "checkbox" ]
                    [ label []
                        [ input [ attribute "checked" "", name "check1", type_ "checkbox" ]
                            []
                        , span []
                            []
                        ]
                    ]
                ]
            , div []
                []
            ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


visibleWord : Bool -> String -> String
visibleWord isVisible word =
    if isVisible then
        word

    else
        ""


headUpper : String -> String
headUpper str =
    let
        chars =
            str |> String.toList
    in
    case chars of
        [] ->
            ""

        h :: t ->
            String.fromList <| Char.toUpper h :: t


lowerCamel : String -> String
lowerCamel str =
    case String.uncons (upperCamelize str) of
        Just ( head, tail ) ->
            String.cons (Char.toLower head) tail

        Nothing ->
            str


upperCamelize : String -> String
upperCamelize str =
    String.split "_" str
        |> List.map
            (\word ->
                case String.uncons word of
                    Just ( head, tail ) ->
                        String.cons (Char.toUpper head) tail

                    Nothing ->
                        word
            )
        |> String.concat
