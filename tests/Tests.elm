module Tests exposing
    ( dbFieldArrayToCucumberTest
    , dbFieldArrayToDDLTest
    , dbFieldArrayToScalaCodeTest
    )

import Array exposing (Array)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (..)
import Test exposing (..)


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
                                , fieldLength = 20
                                , isUnsigned = True
                                , isNotNull = True
                                }
                            , BigInt
                                { fieldName = "foo"
                                , fieldLength = 5
                                , isUnsigned = False
                                , isNotNull = True
                                }
                            , BigInt
                                { fieldName = "bar"
                                , fieldLength = 10
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
                                , fieldLength = 10
                                , isNotNull = True
                                }
                            , VarChar
                                { fieldName = "bbb"
                                , fieldLength = 20
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
\t`id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
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
                                , fieldLength = 10
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
                                , fieldLength = 10
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
                                { fieldName = "foo"
                                , fieldLength = 10
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
                                , fieldLength = 10
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
\t\tfoo: Long,
\t\tho: Int,
\t\ttext: Option[String],
\t\thogeFlag: Boolean,
\t\tstartAt: ZonedDateTime,
\t\tbarStatus: BarStatus,
\t\tversionNo: Long = 1L
\t): Tables =
\t\tTables.create(
\t\t\tfoo = foo,
\t\t\tho = ho,
\t\t\ttext = text,
\t\t\thogeFlag = hogeFlag,
\t\t\tstartAt = startAt,
\t\t\tbarStatus = barStatus.code,
\t\t\tcreatedAt = ZonedDateTime.of(2019, 4, 1, 1, 0, 0, 0, ZoneId.of("UTC"),
\t\t\tcreatedBy = 1L,
\t\t\tupdatedAt = ZonedDateTime.of(2019, 4, 1, 1, 0, 0, 0, ZoneId.of("UTC"),
\t\t\tupdatedBy = 1L,
\t\t\tversionNo = versionNo
\t\t)
}

def createTablesJson(
\tid: Long,
\tfoo: Long,
\tho: Int,
\ttext: Option[String],
\thogeFlag: Boolean,
\tstartAt: ZonedDateTime,
\tbarStatus: BarStatus,
\tversionNo: Long
): Json =
\tJson.obj(
\t\t"id" -> id.toString.asJson,
\t\t"foo" -> foo.asJson,
\t\t"ho" -> ho.asJson,
\t\t"text" -> text.asJson,
\t\t"hogeFlag" -> hogeFlag.asJson,
\t\t"startAt" -> startAt.toInstant.asJson,
\t\t"barStatus" -> barStatus.code.asJson,
\t\t"versionNo" -> versionNo.asJson
\t)

abstract class BarStatus(val code: String, value: String)

object BarStatus {

\tcase object UP extends BarStatus("UP", /* TODO */ "")

\tcase object DOWN extends BarStatus("DOWN", /* TODO */ "")

\tcase object LEFT extends BarStatus("LEFT", /* TODO */ "")

\tcase object RIGHT extends BarStatus("RIGHT", /* TODO */ "")

}
""")
        ]
