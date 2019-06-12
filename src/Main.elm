port module Main exposing
    ( DbField(..)
    , Model
    , Msg(..)
    , dbFieldArrayToCucumber
    , dbFieldArrayToDDL
    , dbFieldArrayToScalaCode
    , dbFieldsDecoder
    , dbFieldsParser
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
import Json.Encode as JE
import Parser as P exposing ((|.), (|=))
import Set
import String.Interpolate exposing (interpolate)
import Task as Task



---- PORTS ----


port saveDbFields : JE.Value -> Cmd msg


port clearDbFields : () -> Cmd msg


dbFieldsEncoder : Array DbField -> JE.Value
dbFieldsEncoder dbFields =
    JE.array dbFieldEncoder dbFields


dbFieldEncoder : DbField -> JE.Value
dbFieldEncoder dbField =
    case dbField of
        PrimaryKey fieldName ->
            JE.object
                [ ( "type_", JE.string "primary" )
                , ( "fieldName", JE.string fieldName )
                ]

        BigInt { fieldName, fieldLengthMaybe, isUnsigned, isNotNull } ->
            JE.object
                [ ( "type_", JE.string "bigint" )
                , ( "fieldName", JE.string fieldName )
                , ( "fieldLengthMaybe", maybeIntEncoder fieldLengthMaybe )
                , ( "isUnsigned", JE.bool isUnsigned )
                , ( "isNotNull", JE.bool isNotNull )
                ]

        DbInt { fieldName, isUnsigned, isNotNull } ->
            JE.object
                [ ( "type_", JE.string "int" )
                , ( "fieldName", JE.string fieldName )
                , ( "isUnsigned", JE.bool isUnsigned )
                , ( "isNotNull", JE.bool isNotNull )
                ]

        VarChar { fieldName, fieldLengthMaybe, isNotNull } ->
            JE.object
                [ ( "type_", JE.string "varchar" )
                , ( "fieldName", JE.string fieldName )
                , ( "fieldLengthMaybe", maybeIntEncoder fieldLengthMaybe )
                , ( "isNotNull", JE.bool isNotNull )
                ]

        Boolean { fieldName, isNotNull } ->
            JE.object
                [ ( "type_", JE.string "boolean" )
                , ( "fieldName", JE.string fieldName )
                , ( "isNotNull", JE.bool isNotNull )
                ]

        Datetime { fieldName, isNotNull } ->
            JE.object
                [ ( "type_", JE.string "datetime" )
                , ( "fieldName", JE.string fieldName )
                , ( "isNotNull", JE.bool isNotNull )
                ]

        Enum { fieldName, values, isNotNull } ->
            JE.object
                [ ( "type_", JE.string "enum" )
                , ( "fieldName", JE.string fieldName )
                , ( "values", JE.list JE.string values )
                , ( "isNotNull", JE.bool isNotNull )
                ]


dbFieldsDecoder : JD.Decoder (Array DbField)
dbFieldsDecoder =
    JD.array
        dbFieldDecoder


dbFieldDecoder : JD.Decoder DbField
dbFieldDecoder =
    JD.field "type_" JD.string
        |> JD.andThen dbFieldDecoderHelper


dbFieldDecoderHelper : String -> JD.Decoder DbField
dbFieldDecoderHelper typeText =
    case typeText of
        "primary" ->
            JD.map PrimaryKey
                (JD.field "fieldName" JD.string)

        "bigint" ->
            JD.map4 BigInt_
                (JD.field "fieldName" JD.string)
                (JD.field "fieldLengthMaybe" <| JD.maybe JD.int)
                (JD.field "isUnsigned" JD.bool)
                (JD.field "isNotNull" JD.bool)
                |> JD.map BigInt

        "int" ->
            JD.map3 DbInt_
                (JD.field "fieldName" JD.string)
                (JD.field "isUnsigned" JD.bool)
                (JD.field "isNotNull" JD.bool)
                |> JD.map DbInt

        "varchar" ->
            JD.map3 VarChar_
                (JD.field "fieldName" JD.string)
                (JD.field "fieldLengthMaybe" <| JD.maybe JD.int)
                (JD.field "isNotNull" JD.bool)
                |> JD.map VarChar

        "boolean" ->
            JD.map2 Boolean_
                (JD.field "fieldName" JD.string)
                (JD.field "isNotNull" JD.bool)
                |> JD.map Boolean

        "datetime" ->
            JD.map2 Datetime_
                (JD.field "fieldName" JD.string)
                (JD.field "isNotNull" JD.bool)
                |> JD.map Datetime

        "enum" ->
            JD.map3 Enum_
                (JD.field "fieldName" JD.string)
                (JD.field "values" <| JD.list JD.string)
                (JD.field "isNotNull" JD.bool)
                |> JD.map Enum

        _ ->
            JD.fail "Invalid DbField"



---- MODEL ----


type alias Model =
    { tableName : String, dbFields : Array DbField, newEnumValue : String, importedStatement : String }


type alias DbFieldsRecord =
    { dbFields : Array DbField
    }


init : JD.Value -> ( Model, Cmd Msg )
init dbFieldsJsonValue =
    case JD.decodeValue dbFieldsDecoder dbFieldsJsonValue of
        Ok initDbFields ->
            ( { tableName = ""
              , dbFields =
                    initDbFields
              , newEnumValue = ""
              , importedStatement = ""
              }
            , Cmd.none
            )

        Err err ->
            ( { tableName = ""
              , dbFields =
                    Array.fromList
                        [ PrimaryKey "id"
                        , BigInt
                            { fieldName = "bigint"
                            , fieldLengthMaybe = Just 20
                            , isUnsigned = True
                            , isNotNull = True
                            }
                        , VarChar
                            { fieldName = "text"
                            , fieldLengthMaybe = Just 10
                            , isNotNull = True
                            }
                        ]
              , newEnumValue = ""
              , importedStatement = ""
              }
            , clearDbFields ()
            )


initBigint : String -> DbField
initBigint fieldName =
    BigInt { fieldName = fieldName, fieldLengthMaybe = Just 10, isUnsigned = False, isNotNull = True }


initDbInt : String -> DbField
initDbInt fieldName =
    DbInt { fieldName = fieldName, isUnsigned = False, isNotNull = True }


initVarchar : String -> DbField
initVarchar fieldName =
    VarChar { fieldName = fieldName, fieldLengthMaybe = Just 50, isNotNull = True }


initBoolean : String -> DbField
initBoolean fieldName =
    Boolean { fieldName = fieldName, isNotNull = True }


initDatetime : String -> DbField
initDatetime fieldName =
    Datetime { fieldName = fieldName, isNotNull = True }


initEnum : String -> DbField
initEnum fieldName =
    Enum { fieldName = fieldName, values = [], isNotNull = True }


dbFieldsParser : P.Parser (Array DbField)
dbFieldsParser =
    P.succeed Array.fromList
        |. P.spaces
        |= P.sequence
            { start = ""
            , separator = ","
            , end = ""
            , spaces = P.spaces
            , item = dbFieldParser
            , trailing = P.Forbidden
            }
        |. P.spaces


dbFieldParser : P.Parser DbField
dbFieldParser =
    P.oneOf
        [ P.backtrackable dbFieldPrimaryKeyParser
        , P.backtrackable dbFielBigIntParser
        , P.backtrackable dbFieldIntParser
        , P.backtrackable dbFieldVarCharParser
        , P.backtrackable dbFieldBooleanParser
        , P.backtrackable dbFieldDatetimeParser
        , P.backtrackable dbFieldEnumParser
        ]


dbFieldPrimaryKeyParser : P.Parser DbField
dbFieldPrimaryKeyParser =
    P.succeed PrimaryKey
        |. P.symbol "`"
        |= fieldNameParser
        |. P.symbol "`"
        |. P.spaces
        |. P.symbol "bigint(20) unsigned NOT NULL AUTO_INCREMENT"


dbFielBigIntParser : P.Parser DbField
dbFielBigIntParser =
    P.map BigInt <|
        P.succeed BigInt_
            |. P.symbol "`"
            |= fieldNameParser
            |. P.symbol "`"
            |. P.spaces
            |. P.symbol "bigint"
            |= lengthParser
            |. P.spaces
            |= isUnsignedParser
            |. P.spaces
            |= isNotNullParser


dbFieldIntParser : P.Parser DbField
dbFieldIntParser =
    P.map DbInt <|
        P.succeed DbInt_
            |. P.symbol "`"
            |= fieldNameParser
            |. P.symbol "`"
            |. P.spaces
            |. P.symbol "int"
            |. P.spaces
            |= isUnsignedParser
            |. P.spaces
            |= isNotNullParser


dbFieldVarCharParser : P.Parser DbField
dbFieldVarCharParser =
    P.map VarChar <|
        P.succeed VarChar_
            |. P.symbol "`"
            |= fieldNameParser
            |. P.symbol "`"
            |. P.spaces
            |. P.symbol "varchar"
            |= lengthParser
            |. P.spaces
            |= isNotNullParser


dbFieldBooleanParser : P.Parser DbField
dbFieldBooleanParser =
    P.map Boolean <|
        P.succeed Boolean_
            |. P.symbol "`"
            |= fieldNameParser
            |. P.symbol "`"
            |. P.spaces
            |. P.symbol "boolean"
            |. P.spaces
            |= isNotNullParser


dbFieldDatetimeParser : P.Parser DbField
dbFieldDatetimeParser =
    P.map Datetime <|
        P.succeed Datetime_
            |. P.symbol "`"
            |= fieldNameParser
            |. P.symbol "`"
            |. P.spaces
            |. P.symbol "datetime(6)"
            |. P.spaces
            |= isNotNullParser


dbFieldEnumParser : P.Parser DbField
dbFieldEnumParser =
    P.map Enum <|
        P.succeed Enum_
            |. P.symbol "`"
            |= fieldNameParser
            |. P.symbol "`"
            |. P.spaces
            |. P.symbol "enum"
            |= P.sequence
                { start = "("
                , separator = ","
                , end = ")"
                , spaces = P.spaces
                , item = enumValueParser
                , trailing = P.Forbidden
                }
            |. P.spaces
            |= isNotNullParser


lengthParser : P.Parser (Maybe Int)
lengthParser =
    P.oneOf
        [ P.succeed Just
            |. P.symbol "("
            |= P.int
            |. P.symbol ")"
        , P.succeed Nothing
        ]


isUnsignedParser : P.Parser Bool
isUnsignedParser =
    P.oneOf
        [ P.map (\_ -> True) (P.keyword "unsigned")
        , P.succeed False
        ]


isNotNullParser : P.Parser Bool
isNotNullParser =
    P.oneOf
        [ P.map (\_ -> True) (P.keyword "NOT NULL")
        , P.succeed False
        ]


fieldNameParser : P.Parser String
fieldNameParser =
    P.variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.fromList [ "select", "from", "where" ]
        }


enumValueParser : P.Parser String
enumValueParser =
    P.succeed identity
        |. P.symbol "'"
        |= P.variable
            { start = Char.isUpper
            , inner = \c -> Char.isAlphaNum c || Char.isUpper c || c == '_'
            , reserved = Set.fromList [ "select", "from", "where" ]
            }
        |. P.symbol "'"


type alias BigInt_ =
    { fieldName : String
    , fieldLengthMaybe : Maybe Int
    , isUnsigned : Bool
    , isNotNull : Bool
    }


type alias DbInt_ =
    { fieldName : String
    , isUnsigned : Bool
    , isNotNull : Bool
    }


type alias VarChar_ =
    { fieldName : String
    , fieldLengthMaybe : Maybe Int
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
    | DbInt DbInt_
    | VarChar VarChar_
    | Boolean Boolean_
    | Datetime Datetime_
    | Enum Enum_


dbFieldToDDL : DbField -> String
dbFieldToDDL dbField =
    case dbField of
        PrimaryKey fieldName ->
            "`" ++ fieldName ++ "` bigint(20) unsigned NOT NULL AUTO_INCREMENT"

        BigInt { fieldName, fieldLengthMaybe, isUnsigned, isNotNull } ->
            interpolate "`{0}` bigint{1}{2}{3}"
                [ fieldName
                , Maybe.withDefault "" <| Maybe.map (\fieldLength -> "(" ++ String.fromInt fieldLength ++ ")") fieldLengthMaybe
                , visibleWord isUnsigned " unsigned"
                , visibleWord isNotNull " NOT NULL"
                ]

        DbInt { fieldName, isUnsigned, isNotNull } ->
            interpolate "`{0}` int{1}{2}"
                [ fieldName
                , visibleWord isUnsigned " unsigned"
                , visibleWord isNotNull " NOT NULL"
                ]

        VarChar { fieldName, fieldLengthMaybe, isNotNull } ->
            interpolate "`{0}` varchar{1}{2}"
                [ fieldName
                , Maybe.withDefault "" <| Maybe.map (\fieldLength -> "(" ++ String.fromInt fieldLength ++ ")") fieldLengthMaybe
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
                lowerCamelize fieldName

            else
                "nullableTextToStr(" ++ lowerCamelize fieldName ++ ")"
    in
    case dbField of
        PrimaryKey fieldName ->
            lowerCamelize fieldName

        BigInt { fieldName } ->
            lowerCamelize fieldName

        DbInt { fieldName } ->
            lowerCamelize fieldName

        VarChar { fieldName, isNotNull } ->
            nullableWrapper isNotNull fieldName

        Boolean { fieldName, isNotNull } ->
            lowerCamelize fieldName

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

        DbInt { fieldName } ->
            fieldName

        VarChar { fieldName } ->
            fieldName

        Boolean { fieldName } ->
            fieldName

        Datetime { fieldName } ->
            fieldName

        Enum { fieldName } ->
            fieldName


dbFieldToDataTableValue : DbField -> String
dbFieldToDataTableValue dbField =
    case dbField of
        PrimaryKey _ ->
            "1"

        BigInt _ ->
            "1"

        DbInt _ ->
            "1"

        VarChar _ ->
            "char"

        Boolean _ ->
            "true"

        Datetime _ ->
            "2019-04-01 00:00:00"

        Enum { values } ->
            Maybe.withDefault "" <| List.head values


dbFieldToScalaArgs : DbField -> String
dbFieldToScalaArgs dbField =
    let
        decorateOption isNotNull scalaTypes =
            if isNotNull then
                scalaTypes

            else
                "Option[" ++ scalaTypes ++ "]"

        createArgs fieldName isNotNull scalaTypes =
            lowerCamelize fieldName ++ ": " ++ decorateOption isNotNull scalaTypes
    in
    case dbField of
        PrimaryKey fieldName ->
            lowerCamelize fieldName ++ ": Long"

        BigInt { fieldName, isNotNull } ->
            createArgs fieldName isNotNull "Long"

        DbInt { fieldName, isNotNull } ->
            createArgs fieldName isNotNull "Int"

        VarChar { fieldName, isNotNull } ->
            createArgs fieldName isNotNull "String"

        Boolean { fieldName, isNotNull } ->
            createArgs fieldName isNotNull "Boolean"

        Datetime { fieldName, isNotNull } ->
            createArgs fieldName isNotNull "ZonedDateTime"

        Enum { fieldName, values, isNotNull } ->
            createArgs fieldName isNotNull <| upperCamelize fieldName


dbFieldToScalaMapObj : DbField -> String -> String
dbFieldToScalaMapObj dbField tableName =
    let
        decorateOption isNotNull scalaTypes =
            if isNotNull then
                scalaTypes

            else
                "Option[" ++ scalaTypes ++ "]"

        createArgs fieldName rightValue =
            "\t\t\"" ++ lowerCamelize fieldName ++ "\" -> " ++ tableName ++ "." ++ rightValue
    in
    case dbField of
        PrimaryKey fieldName ->
            createArgs fieldName <| lowerCamelize fieldName ++ ".toString.asJson"

        BigInt { fieldName, isNotNull } ->
            createArgs fieldName <| lowerCamelize fieldName ++ ".asJson"

        DbInt { fieldName, isNotNull } ->
            createArgs fieldName <| lowerCamelize fieldName ++ ".asJson"

        VarChar { fieldName, isNotNull } ->
            createArgs fieldName <| lowerCamelize fieldName ++ ".asJson"

        Boolean { fieldName, isNotNull } ->
            createArgs fieldName <| lowerCamelize fieldName ++ ".asJson"

        Datetime { fieldName, isNotNull } ->
            createArgs fieldName <| lowerCamelize fieldName ++ ".toInstant.asJson"

        Enum { fieldName, values, isNotNull } ->
            createArgs fieldName <| lowerCamelize fieldName ++ ".code.asJson"


dbFieldToScalaNameBind : DbField -> String
dbFieldToScalaNameBind dbField =
    let
        bindText fieldName =
            lowerCamelize fieldName ++ " = " ++ lowerCamelize fieldName
    in
    case dbField of
        PrimaryKey fieldName ->
            bindText fieldName

        BigInt { fieldName } ->
            bindText fieldName

        DbInt { fieldName } ->
            bindText fieldName

        VarChar { fieldName } ->
            bindText fieldName

        Boolean { fieldName } ->
            bindText fieldName

        Datetime { fieldName } ->
            bindText fieldName

        Enum { fieldName } ->
            bindText fieldName ++ ".code"


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


isDbInt : DbField -> Bool
isDbInt dbField =
    case dbField of
        DbInt _ ->
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
                    , BigInt { fieldName = "created_by", fieldLengthMaybe = Just 20, isUnsigned = True, isNotNull = True }
                    , Datetime { fieldName = "updated_at", isNotNull = True }
                    , BigInt { fieldName = "updated_by", fieldLengthMaybe = Just 20, isUnsigned = True, isNotNull = True }
                    , BigInt { fieldName = "version_no", fieldLengthMaybe = Just 20, isUnsigned = True, isNotNull = True }
                    ]
            )
                |> Array.map dbFieldToDDL

        fieldTexts =
            Array.append fieldTextArray primaryTextArray
                |> Array.toList
                |> String.join ",\n\t"

        fkList =
            dbFieldArray
                |> Array.toList
                |> List.filter (not << isPrimaryKey)
                |> List.map dbFieldToFieldName
                |> List.filter (String.endsWith "_id")

        fkTexts =
            if List.isEmpty fkList then
                ""

            else
                ",\n\t/* You shoud check the following */\n\t"
                    ++ (fkList
                            |> List.map
                                (\fieldName ->
                                    let
                                        -- Drop "_id"
                                        tName =
                                            String.dropRight 3 fieldName
                                    in
                                    interpolate "CONSTRAINT `FK_{0}_{1}S_{2}` FOREIGN KEY (`{3}`) REFERENCES `{4}s` (`{3}`)" [ String.toUpper tableName, String.toUpper tName, String.toUpper fieldName, fieldName, tName ]
                                )
                            |> String.join ",\n\t"
                       )
    in
    "CREATE TABLE `" ++ tableName ++ "` (\n\t" ++ fieldTexts ++ fkTexts ++ "\n) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;"


dbFieldArrayToInsertStatement : String -> String -> List DbField -> String
dbFieldArrayToInsertStatement upperCamelTableName tableName dbFieldList =
    let
        args =
            dbFieldList
                |> List.map (dbFieldToFieldName >> lowerCamelize >> (\n -> "String " ++ n))
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
        [ upperCamelTableName
        , args
        , tableName
        , dbFieldNames
        , formatStrings
        , formatArgs
        ]


dbFieldListToDataTableMethod : String -> List DbField -> String
dbFieldListToDataTableMethod upperCamelTableName dbFieldList =
    let
        getFieldNameStateFromDataTables =
            dbFieldList
                |> List.map (dbFieldToFieldName >> (\t -> "dtm.get(\"" ++ t ++ "\")"))
                |> String.join ", "
    in
    interpolate """public void create{0}(DataTable dataTable) {
\tdataTable.asMaps().stream().map(dtm -> this.create{0}By(
\t\t{1}
\t)).forEach(this::executeStatement);
}"""
        [ upperCamelTableName
        , getFieldNameStateFromDataTables
        ]


dbFieldListToDataTable : List DbField -> String
dbFieldListToDataTable dbFieldList =
    let
        dataTableHeaders =
            dbFieldList
                |> List.map dbFieldToFieldName
                |> String.join "|"

        dataTableValues =
            dbFieldList
                |> List.map dbFieldToDataTableValue
                |> String.join "|"
    in
    interpolate """/*
|{0}|
|{1}|
*/"""
        [ dataTableHeaders, dataTableValues ]


dbFieldArrayToCucumber : String -> Array DbField -> String
dbFieldArrayToCucumber tableName dbFieldArray =
    let
        dbFieldList =
            dbFieldArray |> Array.toList

        upperCamelTableName =
            upperCamelize tableName
    in
    [ dbFieldArrayToInsertStatement upperCamelTableName tableName
    , dbFieldListToDataTableMethod upperCamelTableName
    , dbFieldListToDataTable
    ]
        |> List.map (\f -> f dbFieldList)
        |> String.join "\n\n"


dbFieldListToDummyTableObject : String -> List DbField -> String
dbFieldListToDummyTableObject tableName dbFieldList =
    let
        dbNotPrimaryFieldList =
            dbFieldList |> List.filter (not << isPrimaryKey)

        scalaArgsListText =
            dbNotPrimaryFieldList
                |> List.map (dbFieldToScalaArgs >> (++) "\t\t")
                |> String.join ",\n"

        scalaNameBindText =
            dbNotPrimaryFieldList
                |> List.map (dbFieldToScalaNameBind >> (++) "\t\t\t")
                |> String.join ",\n"
    in
    interpolate """object Dummy{0} {
\tdef create{0}(
{1},
\t\tversionNo: Long = 1L
\t): {0} =
\t\t{0}.create(
{2},
\t\t\tcreatedAt = ZonedDateTime.of(2020, 4, 11, 18, 0, 0, 0, ZoneId.of("UTC")),
\t\t\tcreatedBy = 1L,
\t\t\tupdatedAt = ZonedDateTime.of(2020, 4, 11, 18, 0, 0, 0, ZoneId.of("UTC")),
\t\t\tupdatedBy = 1L,
\t\t\tversionNo = versionNo
\t\t)
}""" [ upperCamelize tableName, scalaArgsListText, scalaNameBindText ]


dbFieldListToCirceJsonMethod : String -> List DbField -> String
dbFieldListToCirceJsonMethod tableName dbFieldList =
    let
        scalaArgsListText =
            dbFieldList
                |> List.map (dbFieldToScalaArgs >> (++) "\t")
                |> String.join ",\n"

        scalaMapObjText =
            dbFieldList
                |> List.map (\dbField -> dbFieldToScalaMapObj dbField (lowerCamelize tableName))
                |> String.join ",\n"
    in
    interpolate """def create{0}Json(
\t{1}: {0}
): Json =
\tJson.obj(
{2},
\t\t"versionNo" -> {1}.versionNo.asJson
\t)""" [ upperCamelize tableName, lowerCamelize tableName, scalaMapObjText ]


dbFieldListToEnums : List DbField -> String
dbFieldListToEnums dbFieldList =
    dbFieldList
        |> List.filter isEnum
        |> List.map
            (\dbField ->
                case dbField of
                    Enum { fieldName, values } ->
                        let
                            toCaseObject value =
                                interpolate """\tcase object {0} extends {1}("{0}", /* TODO */ "")""" [ value, upperCamelize fieldName ]

                            valueCaseObjects =
                                values |> List.map toCaseObject |> String.join "\n\n"
                        in
                        interpolate """abstract class {0}(val code: String, value: String)

object {0} {

{1}

}"""
                            [ upperCamelize fieldName, valueCaseObjects ]

                    _ ->
                        ""
            )
        |> String.join "\n\n"


dbFieldArrayToScalaCode : String -> Array DbField -> String
dbFieldArrayToScalaCode tableName dbFieldArray =
    let
        dbFieldList =
            Array.toList dbFieldArray
    in
    dbFieldListToDummyTableObject tableName dbFieldList
        ++ "\n\n"
        ++ dbFieldListToCirceJsonMethod tableName dbFieldList
        ++ "\n\n"
        ++ dbFieldListToEnums dbFieldList



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


updateDbIntFieldName : String -> DbField -> DbField
updateDbIntFieldName fieldName dbField =
    case dbField of
        DbInt dbInt ->
            DbInt { dbInt | fieldName = fieldName }

        _ ->
            dbField


updateBigIntTurnUnsigned : DbField -> DbField
updateBigIntTurnUnsigned dbField =
    case dbField of
        BigInt bigInt ->
            BigInt { bigInt | isUnsigned = not bigInt.isUnsigned }

        _ ->
            dbField


updateDbIntTurnUnsigned : DbField -> DbField
updateDbIntTurnUnsigned dbField =
    case dbField of
        DbInt dbInt ->
            DbInt { dbInt | isUnsigned = not dbInt.isUnsigned }

        _ ->
            dbField


updateBigIntTurnNotNull : DbField -> DbField
updateBigIntTurnNotNull dbField =
    case dbField of
        BigInt bigInt ->
            BigInt { bigInt | isNotNull = not bigInt.isNotNull }

        _ ->
            dbField


updateDbIntTurnNotNull : DbField -> DbField
updateDbIntTurnNotNull dbField =
    case dbField of
        DbInt dbInt ->
            DbInt { dbInt | isNotNull = not dbInt.isNotNull }

        _ ->
            dbField


updateBigIntLength : String -> DbField -> DbField
updateBigIntLength lenText dbField =
    case dbField of
        BigInt bigInt ->
            BigInt
                { bigInt
                    | fieldLengthMaybe = String.toInt lenText
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
                    | fieldLengthMaybe =
                        String.toInt lenText
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


typeTextToInitDbField : String -> String -> DbField
typeTextToInitDbField typeText fieldName =
    case typeText of
        "primary" ->
            PrimaryKey ""

        "bigint" ->
            initBigint fieldName

        "int" ->
            initDbInt fieldName

        "varchar" ->
            initVarchar fieldName

        "boolean" ->
            initBoolean fieldName

        "datetime" ->
            initDatetime fieldName

        "enum" ->
            initEnum fieldName

        _ ->
            PrimaryKey "Implementation Error"


type alias Index =
    Int


type Msg
    = UpdateTableName String
    | UpdatePrimaryKeyFieldName Index String
    | UpdateBigIntFieldName Index String
    | UpdateDbIntFieldName Index String
    | UpdateVarcharFieldName Index String
    | UpdateBooleanFieldName Index String
    | UpdateDatetimeFieldName Index String
    | UpdateEnumFieldName Index String
    | UpdateBigIntTurnUnsigned Index
    | UpdateBigIntTurnNotNull Index
    | UpdateDbIntTurnUnsigned Index
    | UpdateDbIntTurnNotNull Index
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
    | DeleteDbField Index
    | DownloadDDL
    | DownloadCucumber
    | DownloadScala
    | UpdateImportedStatement String
    | ImportDDL


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { tableName, dbFields, newEnumValue, importedStatement } =
            model

        saveDbFieldsCmd newDbFields =
            saveDbFields <| dbFieldsEncoder newDbFields
    in
    case msg of
        UpdateTableName tname ->
            ( { model | tableName = tname }, Cmd.none )

        UpdatePrimaryKeyFieldName idx fieldName ->
            let
                newDbFields =
                    Array.update idx (updatePrimaryKeyFieldName fieldName) dbFields
            in
            ( { model
                | dbFields = newDbFields
              }
            , saveDbFieldsCmd newDbFields
            )

        UpdateBigIntFieldName idx fieldName ->
            let
                newDbFields =
                    Array.update idx (updateBigIntFieldName fieldName) dbFields
            in
            ( { model
                | dbFields = newDbFields
              }
            , saveDbFieldsCmd newDbFields
            )

        UpdateDbIntFieldName idx fieldName ->
            let
                newDbFields =
                    Array.update idx (updateDbIntFieldName fieldName) dbFields
            in
            ( { model
                | dbFields = newDbFields
              }
            , saveDbFieldsCmd newDbFields
            )

        UpdateVarcharFieldName idx fieldName ->
            let
                newDbFields =
                    Array.update idx (updateVarcharFieldName fieldName) dbFields
            in
            ( { model
                | dbFields = newDbFields
              }
            , saveDbFieldsCmd newDbFields
            )

        UpdateBooleanFieldName idx fieldName ->
            let
                newDbFields =
                    Array.update idx (updateBooleanFieldName fieldName) dbFields
            in
            ( { model
                | dbFields = newDbFields
              }
            , saveDbFieldsCmd newDbFields
            )

        UpdateDatetimeFieldName idx fieldName ->
            let
                newDbFields =
                    Array.update idx (updateDatetimeFieldName fieldName) dbFields
            in
            ( { model
                | dbFields = newDbFields
              }
            , saveDbFieldsCmd newDbFields
            )

        UpdateEnumFieldName idx fieldName ->
            let
                newDbFields =
                    Array.update idx (updateEnumFieldName fieldName) dbFields
            in
            ( { model
                | dbFields = newDbFields
              }
            , saveDbFieldsCmd newDbFields
            )

        UpdateBigIntTurnUnsigned idx ->
            let
                newDbFields =
                    Array.update idx updateBigIntTurnUnsigned dbFields
            in
            ( { model
                | dbFields = newDbFields
              }
            , saveDbFieldsCmd newDbFields
            )

        UpdateDbIntTurnUnsigned idx ->
            let
                newDbFields =
                    Array.update idx updateDbIntTurnUnsigned dbFields
            in
            ( { model
                | dbFields = newDbFields
              }
            , saveDbFieldsCmd newDbFields
            )

        UpdateBigIntTurnNotNull idx ->
            let
                newDbFields =
                    Array.update idx updateBigIntTurnNotNull dbFields
            in
            ( { model
                | dbFields = newDbFields
              }
            , saveDbFieldsCmd newDbFields
            )

        UpdateDbIntTurnNotNull idx ->
            let
                newDbFields =
                    Array.update idx updateDbIntTurnNotNull dbFields
            in
            ( { model
                | dbFields = newDbFields
              }
            , saveDbFieldsCmd newDbFields
            )

        UpdateVarcharTurnNotNull idx ->
            let
                newDbFields =
                    Array.update idx updateVarcharTurnNotNull dbFields
            in
            ( { model
                | dbFields = newDbFields
              }
            , saveDbFieldsCmd newDbFields
            )

        UpdateBooleanTurnNotNull idx ->
            let
                newDbFields =
                    Array.update idx updateBooleanTurnNotNull dbFields
            in
            ( { model
                | dbFields = newDbFields
              }
            , saveDbFieldsCmd newDbFields
            )

        UpdateDatetimeTurnNotNull idx ->
            let
                newDbFields =
                    Array.update idx updateDatetimeTurnNotNull dbFields
            in
            ( { model
                | dbFields = newDbFields
              }
            , saveDbFieldsCmd newDbFields
            )

        UpdateEnumTurnNotNull idx ->
            let
                newDbFields =
                    Array.update idx updateEnumTurnNotNull dbFields
            in
            ( { model
                | dbFields = newDbFields
              }
            , saveDbFieldsCmd newDbFields
            )

        UpdateBigIntLength idx lengthText ->
            let
                newDbFields =
                    Array.update idx (updateBigIntLength lengthText) dbFields
            in
            ( { model
                | dbFields = newDbFields
              }
            , saveDbFieldsCmd newDbFields
            )

        UpdateVarcharLength idx lengthText ->
            let
                newDbFields =
                    Array.update idx (updateVarcharLength lengthText) dbFields
            in
            ( { model
                | dbFields = newDbFields
              }
            , saveDbFieldsCmd newDbFields
            )

        UpdateNewEnumValue enumValue ->
            ( { model | newEnumValue = enumValue }, Cmd.none )

        UpdateFieldType idx typeText ->
            let
                currentFields =
                    Maybe.withDefault (initBigint "") (Array.get idx dbFields)

                newDbFields =
                    Array.set idx (typeTextToInitDbField typeText <| dbFieldToFieldName currentFields) dbFields
            in
            ( { model
                | dbFields = newDbFields
              }
            , saveDbFieldsCmd newDbFields
            )

        UpdateEnumValues idx ->
            let
                newDbFields =
                    Array.update idx (updateEnumValues newEnumValue) dbFields
            in
            ( { model
                | dbFields = newDbFields
                , newEnumValue = ""
              }
            , saveDbFieldsCmd newDbFields
            )

        DeleteEnumValue deletedEnumValue idx ->
            let
                newDbFields =
                    Array.update idx (deleteEnumValue deletedEnumValue) dbFields
            in
            ( { model
                | dbFields = newDbFields
              }
            , saveDbFieldsCmd newDbFields
            )

        AddDbField ->
            let
                newDbFields =
                    Array.push (initBigint "") dbFields
            in
            ( { model
                | dbFields = newDbFields
              }
            , saveDbFieldsCmd newDbFields
            )

        DeleteDbField idx ->
            let
                newDbFields =
                    Array.removeAt idx dbFields
            in
            ( { model | dbFields = newDbFields }, saveDbFieldsCmd newDbFields )

        DownloadDDL ->
            ( model, Download.string (tableName ++ ".sql") "text/plain" <| dbFieldArrayToDDL tableName dbFields )

        DownloadCucumber ->
            ( model, Download.string (upperCamelize tableName ++ ".java") "text/plain" <| dbFieldArrayToCucumber tableName dbFields )

        DownloadScala ->
            ( model, Download.string (upperCamelize tableName ++ ".scala") "text/plain" <| dbFieldArrayToScalaCode tableName dbFields )

        UpdateImportedStatement statement ->
            ( { model | importedStatement = statement }, Cmd.none )

        ImportDDL ->
            let
                newDbFieldsResult =
                    P.run dbFieldsParser importedStatement
            in
            case newDbFieldsResult of
                Ok newDbFields ->
                    ( { model | dbFields = newDbFields }, Cmd.none )

                Err err ->
                    Debug.log (Debug.toString err) <|
                        ( model, Cmd.none )



---- VIEW ----


fieldTypeSelectView : Index -> DbField -> Html Msg
fieldTypeSelectView idx dbField =
    div [ class "select" ]
        [ select [ onChange <| UpdateFieldType idx ]
            [ option [ selected <| isPrimaryKey dbField, value "primary" ]
                [ text "PRIMARY KEY" ]
            , option [ selected <| isBigint dbField, value "bigint" ]
                [ text "BIGINT" ]
            , option [ selected <| isDbInt dbField, value "int" ]
                [ text "INT" ]
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
                , span [ class "trash-icon", onClick <| DeleteDbField idx ] [ i [ class "fas fa-trash" ] [] ]
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

        BigInt { fieldName, fieldLengthMaybe, isUnsigned, isNotNull } ->
            [ div []
                [ input [ class "input", type_ "text", value fieldName, onInput <| UpdateBigIntFieldName idx ]
                    []
                , span [ class "trash-icon", onClick <| DeleteDbField idx ] [ i [ class "fas fa-trash" ] [] ]
                ]
            , div []
                [ fieldTypeSelectView idx dbField
                ]
            , div []
                [ input [ class "input", type_ "number", value <| Maybe.withDefault "" <| Maybe.map String.fromInt fieldLengthMaybe, onInput <| UpdateBigIntLength idx ]
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

        DbInt { fieldName, isUnsigned, isNotNull } ->
            [ div []
                [ input [ class "input", type_ "text", value fieldName, onInput <| UpdateDbIntFieldName idx ]
                    []
                , span [ class "trash-icon", onClick <| DeleteDbField idx ] [ i [ class "fas fa-trash" ] [] ]
                ]
            , div []
                [ fieldTypeSelectView idx dbField
                ]
            , div []
                []
            , div []
                [ checkboxView isUnsigned <| UpdateDbIntTurnUnsigned idx
                ]
            , div []
                [ checkboxView isNotNull <| UpdateDbIntTurnNotNull idx
                ]
            , div []
                []
            ]

        VarChar { fieldName, fieldLengthMaybe, isNotNull } ->
            [ div []
                [ input [ class "input", type_ "text", value fieldName, onInput <| UpdateVarcharFieldName idx ]
                    []
                , span [ class "trash-icon", onClick <| DeleteDbField idx ] [ i [ class "fas fa-trash" ] [] ]
                ]
            , div []
                [ fieldTypeSelectView idx dbField
                ]
            , div []
                [ input [ class "input", type_ "number", value <| Maybe.withDefault "" <| Maybe.map String.fromInt fieldLengthMaybe, onInput <| UpdateVarcharLength idx ]
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
                , span [ class "trash-icon", onClick <| DeleteDbField idx ] [ i [ class "fas fa-trash" ] [] ]
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
                , span [ class "trash-icon", onClick <| DeleteDbField idx ] [ i [ class "fas fa-trash" ] [] ]
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
                , span [ class "trash-icon", onClick <| DeleteDbField idx ] [ i [ class "fas fa-trash" ] [] ]
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
        { dbFields, newEnumValue, importedStatement } =
            model

        dbFieldList =
            Array.toList dbFields
    in
    { title = "elm boilgen"
    , body =
        [ div [ class "downloads" ]
            [ button [ class "button", onClick DownloadDDL ] [ text "DDL" ]
            , button [ class "button", onClick DownloadCucumber ] [ text "Cucumber" ]
            , button [ class "button", onClick DownloadScala ] [ text "Scala" ]
            ]
        , div [ class "import-ddl" ]
            [ button [ class "button", onClick ImportDDL ] [ text "DDL Import" ]
            , textarea [ class "import-statement", placeholder "`id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,\n`hoge_id` bigint(20) unsigned NOT NULL,\n...", onInput UpdateImportedStatement ] [ text importedStatement ]
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


maybeIntEncoder : Maybe Int -> JE.Value
maybeIntEncoder vMaybe =
    case vMaybe of
        Just v ->
            JE.int v

        Nothing ->
            JE.null


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



{- lowerCamelize "foo_bar" == fooBar -}


lowerCamelize : String -> String
lowerCamelize str =
    case String.uncons (upperCamelize str) of
        Just ( head, tail ) ->
            String.cons (Char.toLower head) tail

        Nothing ->
            str



{- upperCamelize "foo_bar" == FooBar -}


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
