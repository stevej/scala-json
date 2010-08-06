/*
 * Copyright 2010 Twitter, Inc.
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


object JsonUnpackerSpec extends Specification {
  "JsonUnpacker" should {
    "object of ints" in {
      case class Point(x: Int, y: Int)
      JsonUnpacker[Point]("""{"x":50,"y":25}""") mustEqual Point(50, 25)

    }
  }
}
