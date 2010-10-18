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

/**
 * A typed wrapper around the results of JsonParser.parse(...).  This defines methods
 * that allow you to dereference a JsonObject as an array, i.e. myBlob(5), a
 * map, i.e. myBlob("key"), or convert it into a primitive type.  If at any
 * point the type of the referenced JsonObject is not appropriate, it will throw
 * a JsonValueException.
 *
 * This provides a convenient way to access members of a JSON object.  Instead
 * of doing a match/case on each level of the tree, you can do a series of
 * dereferences in one statement and wrap the whole thing in a try/catch.  E.g.:
 *
 * val jsonObject = new JsonObject(Json.parse("""
 *   {"results": [{"success": true}]}"""))
 * val firstSuccess = try {
 *   jsonObject("results")(0)("success").asBoolean
 * } catch {
 *   case JsonValueException(m: String) =>
 *     println("Whoops: " + m)
 *   false
 * }
 */
case class JsonValueException(m: String) extends Exception(m);

case class JsonObject(contents: Any, parent: Option[JsonObject]) {
  def this(contents: Any) = this(contents, None)

  // Treat contents as an array and fetch a value
  def apply(index: Int): JsonObject = {
    val list = this.asList
    if (list.length > index)
      new JsonObject(list(index), Some(this))
    else
      throw new JsonValueException("JSON list is too short. Requested " +
        "item " + index + " from a list " + list.length + " long")
  }

  // Treat contents as a dict and fetch a value
  def apply(key: String): JsonObject = {
    if (this.asMap contains key)
      new JsonObject(this.asMap(key), Some(this))
    else
      throw new JsonValueException("JSON component doesn't contain key '" + key + "'");
  }

  /* Note: I would like to implement these all as one, e.g.:
     def as[T](x: Any): T = { x match { case y: T => y; case _ => throw ... } }

     However, due to type erasure, the T in 'case y: T' goes unchecked and the whole function is pointless.  Ideas?

   */
  def asInt: Int = {
    contents match {
      case i: Int => i
      case _ => throw new JsonValueException("JSON component is not an Int.")
    }
  }

  def asDouble: Double = {
    contents match {
      case d: BigDecimal =>
        d.doubleValue
      case _ => throw new JsonValueException("JSON component '" + contents + "' is not a Double.")
    }
  }

  def asString: String = {
    contents match {
      case s: String => s
      case _ => throw new JsonValueException("JSON component is not a String.")
    }
  }

  def asBoolean: Boolean = {
    contents match {
      case b: Boolean => b
      case _ => throw new JsonValueException("JSON component is not a Boolean.")
    }
  }

  def asList: List[Any] = {
    contents match {
      case list: List[Any] => list
      case _ => throw new JsonValueException("JSON component '" + contents + "' is not a List.")
    }
  }

  def asMap: Map[Any, Any] = {
    contents match {
      case map: Map[Any, Any] => map
      case _ => throw new JsonValueException("JSON component '" + contents + "' is not a Map.")
    }
  }
}
