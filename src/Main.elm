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
import File.Download as Download
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as JD
import String.Interpolate exposing (interpolate)
import Task as Task



---- MODEL ----


type alias Model =
    { tableName : String, dbFields : Array DbField, newEnumValue : String }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { tableName = ""
      , dbFields =
            Array.fromList
                [ PrimaryKey "id"
                , BigInt
                    { fieldName = "bigint"
                    , fieldLength = 20
                    , isUnsigned = True
                    , isNotNull = True
                    }
                , VarChar
                    { fieldName = "text"
                    , fieldLength = 10
                    , isNotNull = True
                    }
                ]
      , newEnumValue = ""
      }
    , Cmd.none
    )


initBigint : DbField
initBigint =
    BigInt { fieldName = "", fieldLength = 5, isUnsigned = False, isNotNull = True }


initVarchar : DbField
initVarchar =
    VarChar { fieldName = "", fieldLength = 5, isNotNull = True }


initBoolean : DbField
initBoolean =
    Boolean { fieldName = "", isNotNull = True }


initDatetime : DbField
initDatetime =
    Datetime { fieldName = "", isNotNull = True }


initEnum : DbField
initEnum =
    Enum { fieldName = "", values = [], isNotNull = True }


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


isPrimaryKey : DbField -> Bool
isPrimaryKey dbField =
    case dbField of
        PrimaryKey _ ->
            True

        _ ->
            False


isBigint : DbField -> Bool
isBigint dbField =
    case dbField of
        BigInt _ ->
            True

        _ ->
            False


isVarChar : DbField -> Bool
isVarChar dbField =
    case dbField of
        VarChar _ ->
            True

        _ ->
            False


isDatetime : DbField -> Bool
isDatetime dbField =
    case dbField of
        Datetime _ ->
            True

        _ ->
            False


isBoolean : DbField -> Bool
isBoolean dbField =
    case dbField of
        Boolean _ ->
            True

        _ ->
            False


isEnum : DbField -> Bool
isEnum dbField =
    case dbField of
        Enum _ ->
            True

        _ ->
            False


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


dbFieldArrayToDDL : String -> Array DbField -> String
dbFieldArrayToDDL tableName dbFieldArray =
    let
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
            (Array.append
                dbFieldArray
             <|
                Array.fromList
                    [ Datetime { fieldName = "created_at", isNotNull = True }
                    , BigInt { fieldName = "created_by", fieldLength = 20, isUnsigned = True, isNotNull = True }
                    , Datetime { fieldName = "updated_at", isNotNull = True }
                    , BigInt { fieldName = "updated_by", fieldLength = 20, isUnsigned = True, isNotNull = True }
                    , BigInt { fieldName = "version_no", fieldLength = 20, isUnsigned = True, isNotNull = True }
                    ]
            )
                |> Array.map dbFieldToDDL

        fieldTexts =
            Array.append fieldTextArray primaryTextArray
                |> Array.toList
                |> String.join ",\n\t"
    in
    "CREATE TABLE `" ++ tableName ++ "` (\n\t" ++ fieldTexts ++ "\n) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;"


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



---- UPDATE ----


updatePrimaryKeyFieldName : String -> DbField -> DbField
updatePrimaryKeyFieldName fieldName dbField =
    case dbField of
        PrimaryKey _ ->
            PrimaryKey fieldName

        _ ->
            dbField


updateBigIntFieldName : String -> DbField -> DbField
updateBigIntFieldName fieldName dbField =
    case dbField of
        BigInt bigInt ->
            BigInt { bigInt | fieldName = fieldName }

        _ ->
            dbField


updateBigIntTurnUnsigned : DbField -> DbField
updateBigIntTurnUnsigned dbField =
    case dbField of
        BigInt bigInt ->
            BigInt { bigInt | isUnsigned = not bigInt.isUnsigned }

        _ ->
            dbField


updateBigIntTurnNotNull : DbField -> DbField
updateBigIntTurnNotNull dbField =
    case dbField of
        BigInt bigInt ->
            BigInt { bigInt | isNotNull = not bigInt.isNotNull }

        _ ->
            dbField


updateBigIntLength : String -> DbField -> DbField
updateBigIntLength lenText dbField =
    case dbField of
        BigInt bigInt ->
            BigInt
                { bigInt
                    | fieldLength =
                        Maybe.withDefault bigInt.fieldLength <| String.toInt lenText
                }

        _ ->
            dbField


updateVarcharFieldName : String -> DbField -> DbField
updateVarcharFieldName fieldName dbField =
    case dbField of
        VarChar varchar ->
            VarChar { varchar | fieldName = fieldName }

        _ ->
            dbField


updateVarcharTurnNotNull : DbField -> DbField
updateVarcharTurnNotNull dbField =
    case dbField of
        VarChar varchar ->
            VarChar { varchar | isNotNull = not varchar.isNotNull }

        _ ->
            dbField


updateVarcharLength : String -> DbField -> DbField
updateVarcharLength lenText dbField =
    case dbField of
        VarChar varchar ->
            VarChar
                { varchar
                    | fieldLength =
                        Maybe.withDefault varchar.fieldLength <| String.toInt lenText
                }

        _ ->
            dbField


updateBooleanFieldName : String -> DbField -> DbField
updateBooleanFieldName fieldName dbField =
    case dbField of
        Boolean boolean ->
            Boolean { boolean | fieldName = fieldName }

        _ ->
            dbField


updateBooleanTurnNotNull : DbField -> DbField
updateBooleanTurnNotNull dbField =
    case dbField of
        Boolean boolean ->
            Boolean { boolean | isNotNull = not boolean.isNotNull }

        _ ->
            dbField


updateDatetimeFieldName : String -> DbField -> DbField
updateDatetimeFieldName fieldName dbField =
    case dbField of
        Datetime dtime ->
            Datetime { dtime | fieldName = fieldName }

        _ ->
            dbField


updateDatetimeTurnNotNull : DbField -> DbField
updateDatetimeTurnNotNull dbField =
    case dbField of
        Datetime datetime ->
            Datetime { datetime | isNotNull = not datetime.isNotNull }

        _ ->
            dbField


updateEnumFieldName : String -> DbField -> DbField
updateEnumFieldName fieldName dbField =
    case dbField of
        Enum enum ->
            Enum { enum | fieldName = fieldName }

        _ ->
            dbField


updateEnumTurnNotNull : DbField -> DbField
updateEnumTurnNotNull dbField =
    case dbField of
        Enum enum ->
            Enum { enum | isNotNull = not enum.isNotNull }

        _ ->
            dbField


updateEnumValues : String -> DbField -> DbField
updateEnumValues newEnumValue dbField =
    case dbField of
        Enum enum ->
            Enum
                { enum
                    | values =
                        if List.member newEnumValue enum.values then
                            enum.values

                        else
                            newEnumValue :: enum.values
                }

        _ ->
            dbField


deleteEnumValue : String -> DbField -> DbField
deleteEnumValue deletedEnumValue dbField =
    case dbField of
        Enum enum ->
            Enum { enum | values = List.filter (not << (==) deletedEnumValue) enum.values }

        _ ->
            dbField


typeTextToInitDbField : String -> DbField
typeTextToInitDbField typeText =
    case typeText of
        "primary" ->
            PrimaryKey ""

        "bigint" ->
            initBigint

        "varchar" ->
            initVarchar

        "boolean" ->
            initBoolean

        "datetime" ->
            initDatetime

        "enum" ->
            initEnum

        _ ->
            PrimaryKey "Implementation Error"


type alias Index =
    Int


type Msg
    = UpdateTableName String
    | UpdatePrimaryKeyFieldName Index String
    | UpdateBigIntFieldName Index String
    | UpdateVarcharFieldName Index String
    | UpdateBooleanFieldName Index String
    | UpdateDatetimeFieldName Index String
    | UpdateEnumFieldName Index String
    | UpdateBigIntTurnUnsigned Index
    | UpdateBigIntTurnNotNull Index
    | UpdateVarcharTurnNotNull Index
    | UpdateBooleanTurnNotNull Index
    | UpdateDatetimeTurnNotNull Index
    | UpdateEnumTurnNotNull Index
    | UpdateBigIntLength Index String
    | UpdateVarcharLength Index String
    | UpdateFieldType Index String
    | UpdateNewEnumValue String
    | UpdateEnumValues Index
    | DeleteEnumValue String Index
    | AddDbField
    | DownloadDDL
    | DownloadInsertStatement


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { tableName, dbFields, newEnumValue } =
            model
    in
    case msg of
        UpdateTableName tname ->
            ( { model | tableName = tname }, Cmd.none )

        UpdatePrimaryKeyFieldName idx fieldName ->
            ( { model
                | dbFields =
                    Array.update idx (updatePrimaryKeyFieldName fieldName) dbFields
              }
            , Cmd.none
            )

        UpdateBigIntFieldName idx fieldName ->
            ( { model
                | dbFields =
                    Array.update idx (updateBigIntFieldName fieldName) dbFields
              }
            , Cmd.none
            )

        UpdateVarcharFieldName idx fieldName ->
            ( { model
                | dbFields =
                    Array.update idx (updateVarcharFieldName fieldName) dbFields
              }
            , Cmd.none
            )

        UpdateBooleanFieldName idx fieldName ->
            ( { model
                | dbFields =
                    Array.update idx (updateBooleanFieldName fieldName) dbFields
              }
            , Cmd.none
            )

        UpdateDatetimeFieldName idx fieldName ->
            ( { model
                | dbFields =
                    Array.update idx (updateDatetimeFieldName fieldName) dbFields
              }
            , Cmd.none
            )

        UpdateEnumFieldName idx fieldName ->
            ( { model
                | dbFields =
                    Array.update idx (updateEnumFieldName fieldName) dbFields
              }
            , Cmd.none
            )

        UpdateBigIntTurnUnsigned idx ->
            ( { model
                | dbFields =
                    Array.update idx updateBigIntTurnUnsigned dbFields
              }
            , Cmd.none
            )

        UpdateBigIntTurnNotNull idx ->
            ( { model
                | dbFields =
                    Array.update idx updateBigIntTurnNotNull dbFields
              }
            , Cmd.none
            )

        UpdateVarcharTurnNotNull idx ->
            ( { model
                | dbFields =
                    Array.update idx updateVarcharTurnNotNull dbFields
              }
            , Cmd.none
            )

        UpdateBooleanTurnNotNull idx ->
            ( { model
                | dbFields =
                    Array.update idx updateBooleanTurnNotNull dbFields
              }
            , Cmd.none
            )

        UpdateDatetimeTurnNotNull idx ->
            ( { model
                | dbFields =
                    Array.update idx updateDatetimeTurnNotNull dbFields
              }
            , Cmd.none
            )

        UpdateEnumTurnNotNull idx ->
            ( { model
                | dbFields =
                    Array.update idx updateEnumTurnNotNull dbFields
              }
            , Cmd.none
            )

        UpdateBigIntLength idx lengthText ->
            ( { model
                | dbFields =
                    Array.update idx (updateBigIntLength lengthText) dbFields
              }
            , Cmd.none
            )

        UpdateVarcharLength idx lengthText ->
            ( { model
                | dbFields =
                    Array.update idx (updateVarcharLength lengthText) dbFields
              }
            , Cmd.none
            )

        UpdateNewEnumValue enumValue ->
            ( { model | newEnumValue = enumValue }, Cmd.none )

        UpdateFieldType idx typeText ->
            ( { model
                | dbFields =
                    Array.set idx (typeTextToInitDbField typeText) dbFields
              }
            , Cmd.none
            )

        UpdateEnumValues idx ->
            ( { model
                | dbFields =
                    Array.update idx (updateEnumValues newEnumValue) dbFields
                , newEnumValue = ""
              }
            , Cmd.none
            )

        DeleteEnumValue deletedEnumValue idx ->
            ( { model
                | dbFields =
                    Array.update idx (deleteEnumValue deletedEnumValue) dbFields
              }
            , Cmd.none
            )

        AddDbField ->
            ( { model
                | dbFields =
                    Array.push initBigint dbFields
              }
            , Cmd.none
            )

        DownloadDDL ->
            ( model, Download.string (tableName ++ ".sql") "text/plain" <| dbFieldArrayToDDL tableName dbFields )

        DownloadInsertStatement ->
            ( model, Download.string (tableName ++ ".java") "text/plain" <| dbFieldArrayToInsertMethod tableName dbFields )



---- VIEW ----


fieldTypeSelectView : Index -> DbField -> Html Msg
fieldTypeSelectView idx dbField =
    div [ class "select" ]
        [ select [ onChange <| UpdateFieldType idx ]
            [ option [ selected <| isPrimaryKey dbField, value "primary" ]
                [ text "PRIMARY KEY" ]
            , option [ selected <| isBigint dbField, value "bigint" ]
                [ text "BIGINT" ]
            , option [ selected <| isVarChar dbField, value "varchar" ]
                [ text "VARCHAR" ]
            , option [ selected <| isBoolean dbField, value "boolean" ]
                [ text "BOOLEAN" ]
            , option [ selected <| isDatetime dbField, value "datetime" ]
                [ text "DATETIME" ]
            , option [ selected <| isEnum dbField, value "enum" ]
                [ text "ENUM" ]
            ]
        ]


dbFieldToView : Int -> String -> DbField -> List (Html Msg)
dbFieldToView idx newEnumValue dbField =
    case dbField of
        PrimaryKey fieldName ->
            [ div []
                [ input [ class "input", type_ "text", value fieldName, onInput <| UpdatePrimaryKeyFieldName idx ]
                    []
                ]
            , div []
                [ fieldTypeSelectView idx dbField
                ]
            , div []
                [ input [ class "input", readonly True, type_ "number", value "20" ]
                    []
                ]
            , div []
                [ div [ class "checkbox" ]
                    [ label []
                        [ input [ checked True, disabled True, type_ "checkbox" ]
                            []
                        , span [ class "disabled" ]
                            []
                        ]
                    ]
                ]
            , div []
                [ div [ class "checkbox" ]
                    [ label []
                        [ input [ checked True, disabled True, type_ "checkbox", checked True ]
                            []
                        , span [ class "disabled" ]
                            []
                        ]
                    ]
                ]
            , div []
                [ div [ class "checkbox" ]
                    [ label []
                        [ input [ checked True, disabled True, type_ "checkbox" ]
                            []
                        , span [ class "disabled" ]
                            []
                        ]
                    ]
                ]
            ]

        BigInt { fieldName, fieldLength, isUnsigned, isNotNull } ->
            [ div []
                [ input [ class "input", type_ "text", value fieldName, onInput <| UpdateBigIntFieldName idx ]
                    []
                ]
            , div []
                [ fieldTypeSelectView idx dbField
                ]
            , div []
                [ input [ class "input", type_ "number", value <| String.fromInt fieldLength, onInput <| UpdateBigIntLength idx ]
                    []
                ]
            , div []
                [ checkboxView isUnsigned <| UpdateBigIntTurnUnsigned idx
                ]
            , div []
                [ checkboxView isNotNull <| UpdateBigIntTurnNotNull idx
                ]
            , div []
                []
            ]

        VarChar { fieldName, fieldLength, isNotNull } ->
            [ div []
                [ input [ class "input", type_ "text", value fieldName, onInput <| UpdateVarcharFieldName idx ]
                    []
                ]
            , div []
                [ fieldTypeSelectView idx dbField
                ]
            , div []
                [ input [ class "input", type_ "number", value <| String.fromInt fieldLength, onInput <| UpdateVarcharLength idx ]
                    []
                ]
            , div []
                []
            , div []
                [ checkboxView isNotNull <| UpdateVarcharTurnNotNull idx
                ]
            , div []
                []
            ]

        Boolean { fieldName, isNotNull } ->
            [ div []
                [ input [ class "input", type_ "text", value fieldName, onInput <| UpdateBooleanFieldName idx ]
                    []
                ]
            , div []
                [ fieldTypeSelectView idx dbField
                ]
            , div []
                []
            , div []
                []
            , div []
                [ checkboxView isNotNull <| UpdateBooleanTurnNotNull idx
                ]
            , div []
                []
            ]

        Datetime { fieldName, isNotNull } ->
            [ div []
                [ input [ class "input", type_ "text", value fieldName, onInput <| UpdateDatetimeFieldName idx ]
                    []
                ]
            , div []
                [ fieldTypeSelectView idx dbField
                ]
            , div []
                [ input [ class "input", readonly True, type_ "text", value "6" ]
                    []
                ]
            , div []
                []
            , div []
                [ checkboxView isNotNull <| UpdateDatetimeTurnNotNull idx
                ]
            , div []
                []
            ]

        Enum { fieldName, values, isNotNull } ->
            [ div []
                [ input [ class "input", type_ "text", value fieldName, onInput <| UpdateEnumFieldName idx ]
                    []
                ]
            , div [ class "enum" ]
                [ div [ class "layout-enum" ]
                    [ fieldTypeSelectView idx dbField
                    , input [ class "input", placeholder "new value...", type_ "text", value newEnumValue, onInput UpdateNewEnumValue ]
                        []
                    , button [ class "button", onClick <| UpdateEnumValues idx ]
                        [ text "Add" ]
                    ]
                , ul [ class "cp-layout-items-tag" ] <|
                    List.map
                        (\v ->
                            li [ onClick <| DeleteEnumValue v idx ]
                                [ text v ]
                        )
                        values
                ]
            , div []
                []
            , div []
                []
            , div []
                [ checkboxView isNotNull <| UpdateEnumTurnNotNull idx
                ]
            , div []
                []
            ]


view : Model -> Browser.Document Msg
view model =
    let
        { dbFields, newEnumValue } =
            model

        dbFieldList =
            Array.toList dbFields
    in
    { title = "elm boilgen"
    , body =
        [ div [ class "downloads" ]
            [ button [ class "button is-primary", onClick DownloadDDL ] [ text "DDL" ]
            , button [ class "button is-primary", onClick DownloadInsertStatement ] [ text "Insert Statement" ]
            ]
        , div [ class "field" ]
            [ div [ class "control" ]
                [ input [ class "input is-primary", placeholder "Input table name", type_ "text", onInput UpdateTableName ]
                    []
                ]
            ]
        , div [ class "cp-layout-table" ] <|
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
            ]
                ++ (dbFieldList |> List.indexedMap Tuple.pair |> List.concatMap (\( idx, dbField ) -> dbFieldToView idx newEnumValue dbField))
        , div [ class "add-column" ]
            [ span [ class "button is-success", onClick AddDbField ] [ text "+" ]
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


onChange : (String -> msg) -> Attribute msg
onChange tagger =
    on "change" (JD.map tagger targetValue)


visibleWord : Bool -> String -> String
visibleWord isVisible word =
    if isVisible then
        word

    else
        ""


checkboxView : Bool -> Msg -> Html Msg
checkboxView flag msg =
    div [ class "checkbox" ]
        [ label []
            [ input [ checked flag, type_ "checkbox", onClick <| msg ]
                []
            , span []
                []
            ]
        ]


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
