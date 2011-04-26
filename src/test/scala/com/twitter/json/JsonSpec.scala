/*
 * Copyright 2009 Twitter, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License. You may obtain
 * a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.twitter.json

import extensions._
import org.specs._
import scala.collection.immutable


class JsonSpec extends Specification {
  "Json" should {
    "quote strings" in {
      "unicode within latin-1" in {
        Json.quote("hello\n\u009f") mustEqual "\"hello\\n\\u009f\""
      }

      "unicode outside of latin-1 (the word Tokyo)" in {
        Json.quote("\u6771\u4eac") mustEqual "\"\\u6771\\u4eac\""
      }

      "string containing unicode outside of the BMP (using UTF-16 surrogate pairs)" in {
        // NOTE: The json.org spec is unclear on how to handle supplementary characters.
        val ridiculous = new java.lang.StringBuilder()
        ridiculous.appendCodePoint(0xfe03e)
        Json.quote(ridiculous.toString) mustEqual "\"\\udbb8\\udc3e\""
      }

      "xml" in {
        Json.quote("<xml>sucks</xml>") mustEqual "\"<xml>sucks<\\/xml>\""
      }

      "nested objects" in {
        Json.build(Json.build(List(1, 2))).toString mustEqual "[1,2]"
        // If this triggers, it means you are excessively escaping, sucker.
        Json.build(Json.build(List(1, 2))).toString must_!= "\"[1,2]\""
      }
    }

    "parse strings" in {
      "double slashes like one finds in URLs" in {
        Json.parse("""["hey! http:\/\/www.lollerskates.com"]""") mustEqual
          List("hey! http://www.lollerskates.com")
      }

      "quoted newline" in {
        Json.parse("""["hi\njerk"]""") mustEqual
          List("hi\njerk")
      }

      "empty string" in {
        Json.parse("""[""]""") mustEqual List("")
      }

      "quoted quote" in {
        Json.parse("""["x\"x"]""") mustEqual
          List("x\"x")
      }

      "accept unquoted DEL char, as isn't considered control char in Json spec" in {
        //Json.parse("""["A^?B"]""") mustEqual List("A^?B")
        Json.parse("[\"A\u007fB\"]") mustEqual List("A\u007fB")
      }

      "parse escaped string thing followed by whitespace" in {
        Json.parse("[\"\\u2603  q\"]") mustEqual List("\u2603  q")
        Json.parse("[\"\\t q\"]") mustEqual List("\t q")
      }

      "parse unicode outside of the BMP" in {
        Json.parse("[\"\\udbb8\\udc3e\"]") mustEqual List(new String(Character.toChars(0x0FE03E)))
      }

      "does not strip leading whitespace" in {
        Json.parse("""[" f"]""") mustEqual List(" f")
      }

      "parse escaped backspace at end of string" in {
        Json.parse("""["\\", "\\"]""") mustEqual List("""\""", """\""")
      }

      "parse long string" in {
        Json.parse("{ \"long string\":\"" + (1 to 1000).map(x=>"That will be a long string").mkString + "\" }  ") must
          not throwA(new Exception)
      }
    }

    "parse numbers" in {
      "floating point numbers" in {
        Json.parse("[1.42]") mustEqual List(BigDecimal("1.42"))
      }

      "floating point with exponent" in {
        Json.parse("[1.42e10]") mustEqual List(BigDecimal("1.42e10"))
      }

      "integer with exponent" in {
        Json.parse("[42e10]") mustEqual List(BigDecimal("42e10"))
      }

      "integer numbers" in {
        Json.parse("[42]") mustEqual List(42)
      }
    }

    "parse maps" in {
      "empty map" in {
        Json.parse("{}") mustEqual Map()
      }

      "empty list" in {
        Json.parse("{\"nil\":[]}") mustEqual Map("nil" -> Nil)
      }

      "empty map as value" in {
        Json.parse("{\"empty\":{}}") mustEqual Map("empty" -> Map())
      }

      "simple map" in {
        Json.parse("{\"user_id\": 1554, \"message\": \"your phone is being turned off.\"}") mustEqual
          Map("user_id" -> 1554, "message" -> "your phone is being turned off.")
      }

      "simple map with long" in {
        Json.parse("{\"user_id\": 1554, \"status_id\": 9015551486 }") mustEqual
          Map("user_id" -> 1554, "status_id" -> 9015551486L)
      }

      "map with map" in {
        Json.parse("{\"name\":\"nathaniel\",\"status\":{\"text\":\"i like to dance!\"," +
                   "\"created_at\":666},\"zipcode\":94103}") mustEqual
          Map("name" -> "nathaniel",
              "status" -> Map("text" -> "i like to dance!",
                              "created_at" -> 666),
              "zipcode" -> 94103)
      }

      "map with list" in {
        Json.parse("{\"names\":[\"nathaniel\",\"brittney\"]}") mustEqual
          Map("names" -> List("nathaniel", "brittney"))
      }

      "map with two lists" in {
        Json.parse("{\"names\":[\"nathaniel\",\"brittney\"],\"ages\":[4,7]}") mustEqual
          Map("names" -> List("nathaniel", "brittney"),
              "ages" -> List(4, 7))
      }

      "map with list, boolean and map" in {
        Json.parse("{\"names\":[\"nathaniel\",\"brittney\"],\"adults\":false," +
            "\"ages\":{\"nathaniel\":4,\"brittney\":7}}") mustEqual
          Map("names" -> List("nathaniel", "brittney"),
              "adults" -> false,
               "ages" -> Map("nathaniel" -> 4,
                             "brittney" -> 7))
      }
    }

    "build maps" in {
      "empty map" in {
        Json.build(Map()).toString mustEqual "{}"
      }

      "empty list" in {
        Json.build(Map("nil" -> Nil)).toString mustEqual "{\"nil\":[]}"
      }

      "empty map as value" in {
        Json.build(Map("empty" -> Map())).toString mustEqual "{\"empty\":{}}"
      }

      "simple map" in {
        Json.build(Map("name" -> "nathaniel",
                       "likes" -> "to dance",
                       "age" -> 4)).toString mustEqual
        "{\"age\":4,\"likes\":\"to dance\",\"name\":\"nathaniel\"}"

        Json.build(List(1, 2, 3)).toString mustEqual "[1,2,3]"
      }

      "simple map with long" in {
        Json.build(Map("user_id" -> 1554, "status_id" -> 9015551486L)).toString mustEqual
          "{\"status_id\":9015551486,\"user_id\":1554}"
      }

      "Map with nested Map" in {
        Json.build(Map("name" -> "nathaniel",
                       "status" -> Map("text" -> "i like to dance!",
                                       "created_at" -> 666),
                       "zipcode" -> 94103)).toString mustEqual
          "{\"name\":\"nathaniel\",\"status\":{\"created_at\":666,\"text\":\"i like to dance!\"}," +
            "\"zipcode\":94103}"
      }

      "immutable maps" in {
        import scala.collection.immutable.Map

        "nested" in {
          Json.build(Map("name" -> "nathaniel",
                         "status" -> Map("created_at" -> 666, "text" -> "i like to dance!"),
                         "zipcode" -> 94103)).toString mustEqual
            "{\"name\":\"nathaniel\",\"status\":{\"created_at\":666,\"text\":\"i like to dance!\"}," +
              "\"zipcode\":94103}"
        }

        "appended" in {
          val statusMap = Map("status" -> Map("text" -> "i like to dance!",
                                         "created_at" -> 666))
          Json.build(Map.empty ++
                     Map("name" -> "nathaniel") ++
                     statusMap ++
                     Map("zipcode" -> 94103)).toString mustEqual
            "{\"name\":\"nathaniel\",\"status\":{\"created_at\":666,\"text\":\"i like to dance!\"}," +
              "\"zipcode\":94103}"

        }
      }

      "mutable maps" in {
        "nested" in {
          import scala.collection.mutable.Map

          "literal map" in {
            val map = Map("name" -> "nathaniel",
                          "status" -> Map("text" -> "i like to dance!",
                                          "created_at" -> 666),
                          "zipcode" -> 94103)


            val output = Json.build(map).toString
            val rehydrated = Json.parse(output)

            rehydrated mustEqual map
          }

          "appended" in {
            val statusMap = Map("status" -> Map("text" -> "i like to dance!",
                                                "created_at" -> 666))

            val nestedMap = Map[String,Any]() ++
                            Map("name" -> "nathaniel") ++
                            statusMap ++
                            Map("zipcode" -> 94103)

            val output = Json.build(nestedMap).toString
            val rehydrated = Json.parse(output)

            rehydrated mustEqual nestedMap
          }
        }
      }

      "map with list" in {
        Json.build(Map("names" -> List("nathaniel", "brittney"))).toString mustEqual
          "{\"names\":[\"nathaniel\",\"brittney\"]}"
      }

      "map with two lists" in {
        Json.build(Map("names" -> List("nathaniel", "brittney"),
                       "ages" -> List(4, 7))).toString mustEqual
        "{\"ages\":[4,7],\"names\":[\"nathaniel\",\"brittney\"]}"
      }

      "map with list, boolean and map" in {
        Json.build(Map("names" -> List("nathaniel", "brittney"),
                       "adults" -> false,
                       "ages" -> Map("nathaniel" -> 4,
                                     "brittney" -> 7))).toString mustEqual
          "{\"adults\":false," +
            "\"ages\":{\"brittney\":7,\"nathaniel\":4}," +
            "\"names\":[\"nathaniel\",\"brittney\"]}"
      }
    }

    "parse lists" in {
      "empty list" in {
        Json.parse("[]") mustEqual Nil
      }

      "empty empty list" in {
        Json.parse("[[]]") mustEqual List(Nil)
      }

      "list with empty Map" in {
        Json.parse("[{}]") mustEqual List(Map())
      }

      "simple list" in {
        Json.parse("[\"id\", 1]") mustEqual List("id", 1)
      }

      "nested list" in {
        Json.parse("[\"more lists!\",[1,2,\"three\"]]") mustEqual
          List("more lists!", List(1, 2, "three"))
      }

      "list with map" in {
        Json.parse("[\"maptastic!\",{\"1\":2}]") mustEqual
          List("maptastic!", Map("1" -> 2))
      }

      "list with two maps" in {
        Json.parse("[{\"1\":2},{\"3\":4}]") mustEqual
          List(Map("1" -> 2), Map("3" -> 4))
      }

      "list with list, boolean, map" in {
        Json.parse("{\"names\":[\"nathaniel\",\"brittney\"],\"adults\":false," +
            "\"ages\":{\"nathaniel\":4,\"brittney\":7}}") mustEqual
          Map("names" -> List("nathaniel", "brittney"),
              "adults" -> false,
              "ages" -> Map("nathaniel" -> 4,
                            "brittney" -> 7))
      }

      "list with map containing list" in {
        Json.parse("[{\"1\":[2,3]}]") mustEqual
          List(Map("1" -> List(2, 3)))
      }

      "list with map containing map" in {
        Json.parse("[{\"1\":{\"2\":\"3\"}}]") mustEqual
          List(Map("1" -> Map("2" -> "3")))
      }

      "list in the middle" in {
        Json.parse("""{"JobWithTasks":{"tasks":[{"Add":{"updated_at":12,"position":13}}],"error_count":1}}""") mustEqual
          Map("JobWithTasks" -> Map("tasks" -> List(Map("Add" -> Map("updated_at" -> 12, "position" -> 13))), "error_count" -> 1))
      }
    }

    "build lists" in {
      "empty empty list" in {
        Json.build(List(Nil)).toString mustEqual "[[]]"
      }

      "list with empty Map" in {
        Json.build(List(Map())).toString mustEqual "[{}]"
      }

      "simple list" in {
        Json.build(List("id", 1)).toString mustEqual "[\"id\",1]"
      }

      "nested list" in {
        Json.build(List("more lists!", List(1, 2, "three"))).toString mustEqual
          "[\"more lists!\",[1,2,\"three\"]]"
      }

      "list with map" in {
        Json.build(List("maptastic!", Map("1" -> 2))).toString mustEqual
          "[\"maptastic!\",{\"1\":2}]"
      }

      "list with two maps" in {
        Json.build(List(Map("1" -> 2), Map("3" -> 4))).toString mustEqual
          "[{\"1\":2},{\"3\":4}]"
      }

      "list with map containing list" in {
        Json.build(List(Map("1" -> List(2, 3)))).toString mustEqual
         "[{\"1\":[2,3]}]"
      }

      "list with map containing map" in {
        Json.build(List(Map("1" -> Map("2" -> "3")))).toString mustEqual
          "[{\"1\":{\"2\":\"3\"}}]"
      }
    }

    "build numbers" in {
      Json.build(List(42, 23L, 1.67, BigDecimal("1.67456352431287348917591342E+50"))).toString mustEqual "[42,23,1.67,1.67456352431287348917591342E+50]";
      Json.build(List(0.0, 5.25)).toString mustEqual "[0.0,5.25]"
    }

    "arrays" in {
      "simple arrays can be encoded" in {
        Json.build(Array(0, 1)).toString mustEqual "[0,1]"
      }

      "nested" in {
        "inside of arrays" in {
          Json.build(Array(Array(0, 1), 2.asInstanceOf[AnyRef])).toString mustEqual "[[0,1],2]"
          Json.build(Array(Array(0, 1), Array(2, 3))).toString mustEqual
            "[[0,1],[2,3]]"
        }

        "inside of Lists" in {
          Json.build(List(Array(0, 1))).toString mustEqual "[[0,1]]"
          Json.build(List(Array(0, 1), Array(2, 3))).toString mustEqual "[[0,1],[2,3]]"
        }
      }

      "maps" in {
        "can contain arrays" in {
          Json.build(List(Map("1" -> Array(0, 2)))).toString mustEqual
          "[{\"1\":[0,2]}]"
        }

        "can be contained in arrays" in {
          Json.build(Array(Map("1" -> 2))).toString mustEqual "[{\"1\":2}]"
        }
      }
    }

    "build JsonSerializable objects" in {
      val obj = new JsonSerializable {
        def toJson() = "\"abracadabra\""
      }
      Json.build(List(obj, 23)).toString mustEqual "[\"abracadabra\",23]"
    }
  }
}
