module Tests exposing (dbFieldArrayToDDLTest, dbFieldArrayToInsertMethodTest)

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
                                { fieldName = "hoge"
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
\t`hoge` bigint(20) unsigned NOT NULL,
\t`foo` bigint(5) NOT NULL,
\t`bar` bigint(10),
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
\tPRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
                    """)
        ]


dbFieldArrayToInsertMethodTest : Test
dbFieldArrayToInsertMethodTest =
    describe "dbFieldArrayToInsertMethodTest"
        [ test "Insertメソッドとデータテーブルが生成される" <|
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
                dbFieldArrayToInsertMethod "tables" dbFieldArray
                    |> Expect.equal (String.trim """
private String createTablesBy(String id, String foo, String text, String hogeFlag, String startAt, String enm) {
\treturn String.format("INSERT INTO `tables` (id, foo, text, hoge_flag, start_at, enm, created_at, created_by, updated_at, updated_by, version_no) " +
\t\t"VALUES " +
\t\t"(%s, %s, %s, %s, '%s', %s,'2019-04-01', 1, '2019-04-01', 1, 1);", id, foo, nullableTextToStr(text), hogeFlag, startAt, nullableTextToStr(enm));
}

public void createTables(DataTable dataTable) {
\tStream<Map<String, String>> dataTableMapStream = dataTable.asMaps().stream();
\t\tdataTableMapStream.map(dtm -> this.createTablesBy(
\t\t\tdtm.get("id"), dtm.get("foo"), dtm.get("text"), dtm.get("hoge_flag"), dtm.get("start_at"), dtm.get("enm")
\t\t)
\t).forEach(this::executeStatement);
}

/*
|id|foo|text|hoge_flag|start_at|enm|
|1|1|char|true|2019-04-01 00:00:00|DEFAULT|
*/
""")
        ]
