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

import net.lag.extensions._
import org.specs._
import scala.collection.immutable


object JsonSpec extends Specification {
  "Json" should {
    "quote strings" in {
      "unicode" in {
        Json.quote("hello\n\u009f") mustEqual "\"hello\\n\\u009f\""
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

      "quoted quote" in {
        Json.parse("""["x\"x"]""") mustEqual
          List("x\"x")
      }

      "accept unquoted DEL char, as isn't considered control char in Json spec" in {
        //Json.parse("""["A^?B"]""") mustEqual List("A^?B")
        Json.parse("[\"A\u007fB\"]") mustEqual List("A\u007fB")
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
        "{\"name\":\"nathaniel\",\"likes\":\"to dance\",\"age\":4}"

        Json.build(List(1, 2, 3)).toString mustEqual "[1,2,3]"
      }

      "simple map with long" in {
        Json.build(Map("user_id" -> 1554, "status_id" -> 9015551486L)).toString mustEqual
          "{\"user_id\":1554,\"status_id\":9015551486}"
      }

      "Map with nested Map" in {
        Json.build(Map("name" -> "nathaniel",
                       "status" -> Map("text" -> "i like to dance!",
                                       "created_at" -> 666),
                       "zipcode" -> 94103)).toString mustEqual
          "{\"name\":\"nathaniel\",\"status\":{\"text\":\"i like to dance!\"," +
            "\"created_at\":666},\"zipcode\":94103}"
      }

      "map with list" in {
        Json.build(Map("names" -> List("nathaniel", "brittney"))).toString mustEqual
          "{\"names\":[\"nathaniel\",\"brittney\"]}"
      }

      "map with two lists" in {
        Json.build(Map("names" -> List("nathaniel", "brittney"),
                       "ages" -> List(4, 7))).toString mustEqual
        "{\"names\":[\"nathaniel\",\"brittney\"],\"ages\":[4,7]}"
      }

      "map with list, boolean and map" in {
        Json.build(Map("names" -> List("nathaniel", "brittney"),
                       "adults" -> false,
                       "ages" -> Map("nathaniel" -> 4,
                                     "brittney" -> 7))).toString mustEqual
          "{\"names\":[\"nathaniel\",\"brittney\"],\"adults\":false," +
            "\"ages\":{\"nathaniel\":4,\"brittney\":7}}"
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
        Json.build(List("maptastic!", Map(1 -> 2))).toString mustEqual
          "[\"maptastic!\",{\"1\":2}]"
      }

      "list with two maps" in {
        Json.build(List(Map(1 -> 2), Map(3 -> 4))).toString mustEqual
          "[{\"1\":2},{\"3\":4}]"
      }

      "list with map containing list" in {
        Json.build(List(Map(1 -> List(2, 3)))).toString mustEqual
         "[{\"1\":[2,3]}]"
      }

      "list with map containing map" in {
        Json.build(List(Map("1" -> Map("2" -> "3")))).toString mustEqual
          "[{\"1\":{\"2\":\"3\"}}]"
      }
    }
    
    "build numbers" in {
      Json.build(List(42, 23L, 1.67, BigDecimal("1.67456352431287348917591342E+50"))).toString mustEqual "[42,23,1.67,1.67456352431287348917591342E+50]";
    }

    "build JsonSerializable objects" in {
      val obj = new JsonSerializable {
        def toJson() = "\"abracadabra\""
      }
      Json.build(List(obj, 23)).toString mustEqual "[\"abracadabra\",23]"
    }
  }
}
