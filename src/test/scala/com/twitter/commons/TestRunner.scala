/** Copyright 2008 Twitter, Inc. */
package com.twitter.commons

import net.lag.configgy.Configgy
import net.lag.logging.Logger

object TestRunner extends FilterableSpecsFileRunner("src/test/scala/com/twitter/**/*.scala") {
  if (System.getProperty("debugtrace") == null) {
    Logger.get("").setLevel(Logger.OFF)
  }
}
