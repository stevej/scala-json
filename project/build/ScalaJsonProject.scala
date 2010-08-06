import sbt._
import com.twitter.sbt._


class ScalaJsonProject(info: ProjectInfo) extends StandardProject(info) {
  val specs = "org.scala-tools.testing" % "specs" % "1.6.2.1"
  val vscaladoc = "org.scala-tools" % "vscaladoc" % "1.1-md-3"
  val configgy = "net.lag" % "configgy" % "1.5.3"
  val objenesis = "org.objenesis" % "objenesis" % "1.1"

  val publishTo = Resolver.sftp("green.lag.net", "green.lag.net", "/web/repo")

  override def pomExtra =
    <licenses>
      <license>
        <name>Apache 2</name>
        <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
}
