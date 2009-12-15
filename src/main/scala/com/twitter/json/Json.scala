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
import scala.collection.Map
import scala.collection.immutable.EmptyMap
import scala.util.parsing.combinator._


trait JsonSerializable {
  def toJson(): String
}

/**
 * An Exception thrown when parsing or building JSON.
 */
class JsonException(reason: String) extends Exception(reason)


/**
 * Stolen (awesomely) from the scala book and fixed by making string quotation explicit.
 */
private class JsonParser extends JavaTokenParsers {
  def obj: Parser[Map[String, Any]] = "{" ~> repsep(member, ",") <~ "}" ^^ (new EmptyMap ++ _)

  def arr: Parser[List[Any]] = "[" ~> repsep(value, ",") <~ "]"

  def member: Parser[(String, Any)] = string ~ ":" ~ value ^^ {
    case name ~ ":" ~ value => (name, value)
  }

  def number: Parser[Any] = floatingPointNumber ^^ {
    case num if num.matches(".*[.eE].*") => BigDecimal(num)
    case num => {
      val rv = num.toLong
      if (rv >= Math.MIN_INT && rv <= Math.MAX_INT) rv.toInt else rv
    }
  }

  def string: Parser[String] =
    "\"" ~> """([^\"[\x00-\x1F]\\]+|\\[\\/bfnrt"]|\\u[a-fA-F0-9]{4})*""".r <~ "\"" ^^
      { _.replace("""\/""", "/").unquoteC }

  def value: Parser[Any] = obj | arr | string | number |
    "null" ^^ (x => null) | "true" ^^ (x => true) | "false" ^^ (x => false)

  def parse(s: String) = {
    parseAll(value, s) match {
      case Success(result, _) => result
      case x @ Failure(msg, z) => throw new JsonException(x.toString)
      case x @ Error(msg, _) => throw new JsonException(x.toString)
    }
  }
}


/**
 * An explanation of Scala types and their JSON representations.
 *
 * Natively supported scalar types are: Boolean, Int, Long, String.
 * Collections are Seq[T], Map[String, T] where T includes the scalars defined above, or
 * recursive Seq or Map. You are in flavor country.
 */
object Json {
  /**
   * Quote a string according to "JSON rules".
   */
  def quote(s: String) = {
    "\"" + s.regexSub("""[\u0000-\u001f\u0080-\u00a0\u2000-\u2100/\"\\]""".r) { m =>
      m.matched.charAt(0) match {
        case '\r' => "\\r"
        case '\n' => "\\n"
        case '\t' => "\\t"
        case '"' => "\\\""
        case '\\' => "\\\\"
        case '/' => "\\/"     // to avoid sending "</"
        case c => "\\u%04x" format c.asInstanceOf[Int]
      }
    } + "\""
  }

  /**
   * Returns a JSON representation of the given object, as a JsonQuoted object.
   */
  def build(obj: Any): JsonQuoted = {
    val rv = obj match {
      case JsonQuoted(body) => body
      case null => "null"
      case x: Boolean => x.toString
      case x: Number => x.toString
      case list: Seq[_] =>
        list.map(build(_).body).mkString("[", ",", "]")
      case map: Map[_, _] =>
        (for ((key, value) <- map.elements) yield {
          quote(key.toString) + ":" + build(value).body
        }).mkString("{", ",", "}")
      case x: JsonSerializable => x.toJson()
      case x =>
        quote(x.toString)
    }
    JsonQuoted(rv)
  }

  /**
   * Parses a JSON String representation into its native Scala reprsentation.
   */
  def parse(s: String): Any = (new JsonParser).parse(s)
}


/**
 * Wrapper for the JSON string representation of a data structure. This class exists to
 * allow objects to be converted into JSON, attached to another data structure, and not
 * re-encoded.
 */
case class JsonQuoted(body: String) {
  override def toString = body
}
