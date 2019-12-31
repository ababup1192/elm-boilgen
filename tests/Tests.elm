module Tests exposing
    ( dbFieldArrayToCucumberTest
    , dbFieldArrayToDDLTest
    , dbFieldArrayToElmCodeTest
    , dbFieldArrayToScalaCodeTest
    , dbFieldArrayToTypeScriptCodeTest
    , dbFieldsParserTest
    , insertAtTest
    )

import Array exposing (Array)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as JD
import Main exposing (..)
import Parser as P
import Test exposing (..)


dbFieldsDecoderTest : Test
dbFieldsDecoderTest =
    describe "" <|
        [ test "" <|
            \_ ->
                let
                    array =
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
                in
                """
          [{"type_":"primary","fieldName":"id"},{"type_":"bigint","fieldName":"bigint","fieldLengthMaybe":20,"isUnsigned":true,"isNotNull":true},{"type_":"varchar","fieldName":"text","fieldLengthMaybe":10,"isNotNull":true}]
          """
                    |> JD.decodeString dbFieldsDecoder
                    |> Expect.equal (Ok array)
        ]


dbFieldArrayToDDLTest : Test
dbFieldArrayToDDLTest =
    describe "dbFieldArrayToDDLTest"
        [ test "DDLが生成される" <|
            \_ ->
                let
                    dbFieldArray =
                        Array.fromList
                            [ PrimaryKey "id"
                            , BigInt
                                { fieldName = "hoge_id"
                                , fieldLengthMaybe = Just 20
                                , isUnsigned = True
                                , isNotNull = True
                                }
                            , BigInt
                                { fieldName = "foo"
                                , fieldLengthMaybe = Just 5
                                , isUnsigned = False
                                , isNotNull = True
                                }
                            , BigInt
                                { fieldName = "bar"
                                , fieldLengthMaybe = Just 10
                                , isUnsigned = False
                                , isNotNull = False
                                }
                            , DbInt
                                { fieldName = "ho"
                                , isUnsigned = True
                                , isNotNull = True
                                }
                            , VarChar
                                { fieldName = "aaa"
                                , fieldLengthMaybe = Just 10
                                , isNotNull = True
                                }
                            , VarChar
                                { fieldName = "bbb"
                                , fieldLengthMaybe = Just 20
                                , isNotNull = False
                                }
                            , Boolean
                                { fieldName = "bool"
                                , isNotNull = True
                                }
                            , Datetime
                                { fieldName = "dt"
                                , isNotNull = True
                                }
                            , Enum
                                { fieldName = "enm"
                                , values = [ "DEFAULT", "FIRST", "SECOND" ]
                                , isNotNull = True
                                }
                            ]
                in
                dbFieldArrayToDDL "tables" dbFieldArray
                    |> Expect.equal (String.trim """
CREATE TABLE `tables` (
\t`id` bigint(20) unsigned NOT NULL,
\t`hoge_id` bigint(20) unsigned NOT NULL,
\t`foo` bigint(5) NOT NULL,
\t`bar` bigint(10),
\t`ho` int unsigned NOT NULL,
\t`aaa` varchar(10) NOT NULL,
\t`bbb` varchar(20),
\t`bool` boolean NOT NULL,
\t`dt` datetime(6) NOT NULL,
\t`enm` enum('DEFAULT', 'FIRST', 'SECOND') NOT NULL,
\t`created_at` datetime(6) NOT NULL,
\t`created_by` bigint(20) unsigned NOT NULL,
\t`updated_at` datetime(6) NOT NULL,
\t`updated_by` bigint(20) unsigned NOT NULL,
\t`version_no` bigint(20) unsigned NOT NULL,
\tPRIMARY KEY (`id`),
\t/* You shoud check the following */
\tCONSTRAINT `FK_TABLES_HOGES_HOGE_ID` FOREIGN KEY (`hoge_id`) REFERENCES `hoges` (`hoge_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
                    """)
        ]


dbFieldArrayToCucumberTest : Test
dbFieldArrayToCucumberTest =
    describe "dbFieldArrayToCucumberTest"
        [ test "Cucumber用のメソッド群が生成される" <|
            \_ ->
                let
                    dbFieldArray =
                        Array.fromList
                            [ PrimaryKey "id"
                            , BigInt
                                { fieldName = "foo"
                                , fieldLengthMaybe = Just 10
                                , isUnsigned = False
                                , isNotNull = True
                                }
                            , DbInt
                                { fieldName = "ho"
                                , isUnsigned = True
                                , isNotNull = True
                                }
                            , VarChar
                                { fieldName = "text"
                                , fieldLengthMaybe = Just 10
                                , isNotNull = False
                                }
                            , Boolean
                                { fieldName = "hoge_flag"
                                , isNotNull = False
                                }
                            , Datetime
                                { fieldName = "start_at"
                                , isNotNull = True
                                }
                            , Enum
                                { fieldName = "enm"
                                , values = [ "DEFAULT", "FIRST", "SECOND" ]
                                , isNotNull = False
                                }
                            ]
                in
                dbFieldArrayToCucumber "tables" dbFieldArray
                    |> Expect.equal (String.trim """
private String createTablesBy(String id, String foo, String ho, String text, String hogeFlag, String startAt, String enm) {
\treturn String.format("INSERT INTO `tables` (id, foo, ho, text, hoge_flag, start_at, enm, created_at, created_by, updated_at, updated_by, version_no) " +
\t\t"VALUES " +
\t\t"(%s, %s, %s, %s, %s, '%s', %s,'2019-04-01', 1, '2019-04-01', 1, 1);", id, foo, ho, nullableTextToStr(text), hogeFlag, startAt, nullableTextToStr(enm));
}

public void createTables(DataTable dataTable) {
\tdataTable.asMaps().stream().map(dtm -> this.createTablesBy(
\t\tdtm.get("id"), dtm.get("foo"), dtm.get("ho"), dtm.get("text"), dtm.get("hoge_flag"), dtm.get("start_at"), dtm.get("enm")
\t)).forEach(this::executeStatement);
}

/*
|id|foo|ho|text|hoge_flag|start_at|enm|
|1|1|1|char|true|2019-04-01 00:00:00|DEFAULT|
*/
""")
        ]


dbFieldArrayToScalaCodeTest : Test
dbFieldArrayToScalaCodeTest =
    describe "dbFieldArrayToScalaCodeTest"
        [ test "Scalaのボイラプレートが生成される" <|
            \_ ->
                let
                    dbFieldArray =
                        Array.fromList
                            [ PrimaryKey "id"
                            , BigInt
                                { fieldName = "foo_id"
                                , fieldLengthMaybe = Just 10
                                , isUnsigned = False
                                , isNotNull = True
                                }
                            , DbInt
                                { fieldName = "ho"
                                , isUnsigned = True
                                , isNotNull = True
                                }
                            , VarChar
                                { fieldName = "text"
                                , fieldLengthMaybe = Just 10
                                , isNotNull = False
                                }
                            , Boolean
                                { fieldName = "hoge_flag"
                                , isNotNull = True
                                }
                            , Datetime
                                { fieldName = "start_at"
                                , isNotNull = True
                                }
                            , Enum
                                { fieldName = "bar_status"
                                , values = [ "UP", "DOWN", "LEFT", "RIGHT" ]
                                , isNotNull = True
                                }
                            ]
                in
                dbFieldArrayToScalaCode "tables" dbFieldArray
                    |> Expect.equal (String.trim """
object DummyTables {
\tdef createTables(
\t\tfooId: Long,
\t\tho: Int,
\t\ttext: Option[String],
\t\thogeFlag: Boolean,
\t\tstartAt: ZonedDateTime,
\t\tbarStatus: BarStatus,
\t\tversionNo: Long = 1L
\t): Tables =
\t\tTables.create(
\t\t\tid = /* TODO: replace generateId */,
\t\t\tfooId = fooId,
\t\t\tho = ho,
\t\t\ttext = text,
\t\t\thogeFlag = hogeFlag,
\t\t\tstartAt = startAt,
\t\t\tbarStatus = barStatus.code,
\t\t\tcreatedAt = ZonedDateTime.of(2020, 4, 11, 18, 0, 0, 0, ZoneId.of("UTC")),
\t\t\tcreatedBy = 1L,
\t\t\tupdatedAt = ZonedDateTime.of(2020, 4, 11, 18, 0, 0, 0, ZoneId.of("UTC")),
\t\t\tupdatedBy = 1L,
\t\t\tversionNo = versionNo
\t\t)
}

def createTablesJson(
\ttables: Tables
): Json =
\tJson.obj(
\t\t"id" -> tables.id.toString.asJson,
\t\t"fooId" -> tables.fooId.asJson,
\t\t"ho" -> tables.ho.asJson,
\t\t"text" -> tables.text.asJson,
\t\t"hogeFlag" -> tables.hogeFlag.asJson,
\t\t"startAt" -> tables.startAt.toInstant.asJson,
\t\t"barStatus" -> tables.barStatus.code.asJson,
\t\t"versionNo" -> tables.versionNo.asJson
\t)

abstract class BarStatus(val code: String, value: String)

object BarStatus {

\tcase object UP extends BarStatus("UP", /* TODO */ "")

\tcase object DOWN extends BarStatus("DOWN", /* TODO */ "")

\tcase object LEFT extends BarStatus("LEFT", /* TODO */ "")

\tcase object RIGHT extends BarStatus("RIGHT", /* TODO */ "")

}

case class Tables(
\tid: Long,
\tfoo: Foo,
\tho: Int,
\ttext: Option[String],
\thogeFlag: Boolean,
\tstartAt: ZonedDateTime,
\tbarStatus: BarStatus,
\tversionNo: Long
)

case class Text private (value: Option[String]) extends AnyVal

object Text {
\tprivate[this] val maxLength = 10

\tdef apply(value: Option[String]): Text = {
\t\tval trimmed = value.map(_.trim)
\t\trequire(trimmed.forall(t => !t.isEmpty && t.length <= maxLength))
\t\tnew Text(trimmed)
\t}
}

class TableCommandSpec extends FunSpec {
\tdescribe("Textが1文字の時") {
\t\tit("インスタンス化に成功すること") {
\t\t\tassert(Text(Some("a")).value == Some("a"))
\t\t}
\t}

\tdescribe("Textが10文字の時") {
\t\tit("インスタンス化に成功すること") {
\t\t\tassert(Text(Some("a" * 10)).value == Some("a" * 10))
\t\t}
\t}

\tdescribe("Textが空白の時") {
\t\tit("IllegalArgumentExceptionを投げること") {
\t\t\tassertThrows[IllegalArgumentException] {
\t\t\t\tText(Some(" "))
\t\t\t}
\t\t}
\t}

\tdescribe("Textが11文字の時") {
\t\tit("IllegalArgumentExceptionを投げること") {
\t\t\tassertThrows[IllegalArgumentException] {
\t\t\t\tText(Some("a" * 11))
\t\t\t}
\t\t}
\t}
}
""")
        ]


dbFieldArrayToTypeScriptCodeTest : Test
dbFieldArrayToTypeScriptCodeTest =
    describe "dbFieldArrayToTypeScriptCodeTest"
        [ test "TypeScriptのボイラプレートが生成される" <|
            \_ ->
                let
                    dbFieldArray =
                        Array.fromList
                            [ PrimaryKey "id"
                            , BigInt
                                { fieldName = "foo_id"
                                , fieldLengthMaybe = Just 10
                                , isUnsigned = False
                                , isNotNull = True
                                }
                            , DbInt
                                { fieldName = "ho"
                                , isUnsigned = True
                                , isNotNull = True
                                }
                            , VarChar
                                { fieldName = "text"
                                , fieldLengthMaybe = Just 10
                                , isNotNull = False
                                }
                            , Boolean
                                { fieldName = "hoge_flag"
                                , isNotNull = True
                                }
                            , Datetime
                                { fieldName = "start_at"
                                , isNotNull = True
                                }
                            , Enum
                                { fieldName = "bar_status"
                                , values = [ "UP", "DOWN", "LEFT", "RIGHT" ]
                                , isNotNull = True
                                }
                            ]
                in
                dbFieldArrayToTypeScriptCode "tables" dbFieldArray
                    |> Expect.equal (String.trim """
export type Table = {
\treadonly id: string;
\treadonly foo: Foo;
\treadonly ho: number;
\treadonly text: string | null;
\treadonly hogeFlag: boolean;
\treadonly startAt: Date;
\treadonly barStatus: BarStatus;
};


export type BarStatus = 'UP' | 'DOWN' | 'LEFT' | 'RIGHT'
""")
        ]


dbFieldArrayToElmCodeTest : Test
dbFieldArrayToElmCodeTest =
    describe "dbFieldArrayToElmCodeTest"
        [ test "Elmのボイラプレートが生成される" <|
            \_ ->
                let
                    dbFieldArray =
                        Array.fromList
                            [ PrimaryKey "id"
                            , BigInt
                                { fieldName = "foo_id"
                                , fieldLengthMaybe = Just 10
                                , isUnsigned = False
                                , isNotNull = True
                                }
                            , DbInt
                                { fieldName = "ho"
                                , isUnsigned = True
                                , isNotNull = True
                                }
                            , VarChar
                                { fieldName = "text"
                                , fieldLengthMaybe = Just 10
                                , isNotNull = False
                                }
                            , Boolean
                                { fieldName = "hoge_flag"
                                , isNotNull = True
                                }
                            , Datetime
                                { fieldName = "start_at"
                                , isNotNull = True
                                }
                            , Enum
                                { fieldName = "bar_status"
                                , values = [ "UP", "DOWN", "LEFT", "RIGHT" ]
                                , isNotNull = True
                                }
                            ]
                in
                dbFieldArrayToElmCode "tables" dbFieldArray
                    |> Expect.equal (String.trim """
type alias Table =
\t{ foo : Foo
\t, ho : Int
\t, textMaybe : Maybe String
\t, hogeFlag : Bool
\t, startAt : Int
\t, barStatus : BarStatus
\t}


type BarStatus
\t= Up
\t| Down
\t| Left
\t| Right
""")
        ]


dbFieldsParserTest : Test
dbFieldsParserTest =
    describe "dbFieldsParserTest"
        [ test "DDL(フィールド部分のみ)がParseできる" <|
            \_ ->
                String.trim """
`id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
`hoge_id` bigint(20) unsigned NOT NULL,
`foo` bigint(5) NOT NULL,
`ho` int unsigned NOT NULL,
`aaa` varchar(10) NOT NULL,
`bool` boolean NOT NULL,
`dat` date NOT NULL,
`dt` datetime(6) NOT NULL,
`enm` enum('DEFAULT_VALUE', 'FIRST', 'SECOND') NOT NULL
                """
                    |> P.run dbFieldsParser
                    |> Expect.equal
                        (Ok <|
                            Array.fromList
                                [ PrimaryKey "id"
                                , BigInt
                                    { fieldName = "hoge_id"
                                    , fieldLengthMaybe = Just 20
                                    , isUnsigned = True
                                    , isNotNull = True
                                    }
                                , BigInt
                                    { fieldName = "foo"
                                    , fieldLengthMaybe = Just 5
                                    , isUnsigned = False
                                    , isNotNull = True
                                    }
                                , DbInt
                                    { fieldName = "ho"
                                    , isUnsigned = True
                                    , isNotNull = True
                                    }
                                , VarChar
                                    { fieldName = "aaa"
                                    , fieldLengthMaybe = Just 10
                                    , isNotNull = True
                                    }
                                , Boolean
                                    { fieldName = "bool"
                                    , isNotNull = True
                                    }
                                , Date
                                    { fieldName = "dat"
                                    , isNotNull = True
                                    }
                                , Datetime
                                    { fieldName = "dt"
                                    , isNotNull = True
                                    }
                                , Enum
                                    { fieldName = "enm"
                                    , values = [ "DEFAULT_VALUE", "FIRST", "SECOND" ]
                                    , isNotNull = True
                                    }
                                ]
                        )
        ]


insertAtTest : Test
insertAtTest =
    describe "insertAtTest"
        [ test "DDL(フィールド部分のみ)がParseできる" <|
            \_ ->
                insertAt 0 'b' (Array.fromList [ 'a', 'c' ])
                    |> Expect.equal (Array.fromList [ 'b', 'a', 'c' ])
        ]
