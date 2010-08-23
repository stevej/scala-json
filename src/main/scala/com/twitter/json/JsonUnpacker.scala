package com.twitter.json

import java.lang.reflect._
import scala.reflect.Manifest
import org.objenesis.ObjenesisStd

class JsonUnpacker {
  val objenesis = new ObjenesisStd()
  var ignoreExtraFields = false

  def methodsMatching(obj: AnyRef, name: String) = {
    obj.getClass.getMethods.find { method =>
      method.getName == name &&
        method.getReturnType == classOf[Unit] &&
        method.getParameterTypes.size == 1
    }.toList
  }

  def setIntField[T](obj: T, field: Field, value: Int) {
    val t = field.getType
    if (t == classOf[Int]) {
      field.setInt(obj, value)
    } else if (t == classOf[Long]) {
      field.setLong(obj, value.toLong)
    } else if (t == classOf[Short]) {
      field.setShort(obj, value.toShort)
    } else if (t == classOf[Char]) {
      field.setChar(obj, value.toChar)
    } else if (t == classOf[Byte]) {
      field.setByte(obj, value.toByte)
    } else {
      throw new JsonException("Missing field conversion: " + field.getName + " of type " +
                              field.getType.toString + " missing conversion from int")
    }
  }

  def setLongField[T](obj: T, field: Field, value: Long) {
    val t = field.getType
    if (t == classOf[Int]) {
      field.setInt(obj, value.toInt)
    } else if (t == classOf[Long]) {
      field.setLong(obj, value)
    } else if (t == classOf[Short]) {
      field.setShort(obj, value.toShort)
    } else if (t == classOf[Char]) {
      field.setChar(obj, value.toChar)
    } else if (t == classOf[Byte]) {
      field.setByte(obj, value.toByte)
    } else {
      throw new JsonException("Missing field conversion: " + field.getName + " of type " +
                              field.getType.toString + " missing conversion from long")
    }
  }

  def setField[T](obj: T, field: Field, value: Any) {
    value match {
      case x: Int =>
        setIntField(obj, field, x)
      case x: Long =>
        setLongField(obj, field, x)
      case x: BigDecimal =>
      case x: Boolean =>
      case x: String =>
      // null
      // array, object
    }
  }

  @throws(classOf[JsonException])
  def unpackObject[T](json: Map[String, Any], cls: Class[T]): T = {
    val (obj, fields) = makeObject(cls)

    fields.foreach { field =>
      json.get(field.getName) match {
        case None =>
          throw new JsonException("Missing field: " + field.getName)
        case Some(value) =>
          setField(obj, field, value)
      }
    }

    if (!ignoreExtraFields) {
      val extraFields = json.keys -- fields.map { _.getName }
      if (extraFields.size > 0) {
        throw new JsonException("Extra fields in json: " + extraFields.mkString(", "))
      }
    }

    obj
  }

  /**
   * So evil. Make an object without calling its constructor. Then find all
   * the declared fields and make them accessible. This opens up a case class
   * for naughtiness.
   */
  def makeObject[T](cls: Class[T]): (T, List[Field]) = {
    val obj = objenesis.newInstance(cls).asInstanceOf[T]
    val fields = cls.getDeclaredFields().filter { field => !(field.getName contains '$') }.toList
    fields.foreach { _.setAccessible(true) }
    (obj, fields)
  }
}

object JsonUnpacker {
  def apply[T](s: String)(implicit manifest: Manifest[T]) =
    new JsonUnpacker().unpackObject(Json.parse(s).asInstanceOf[Map[String, Any]], manifest.erasure)
}
