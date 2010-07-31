import sbt._
import com.twitter.sbt._


class ScalaJsonProject(info: ProjectInfo) extends StandardProject(info) {
  val specs = "org.scala-tools.testing" % "specs_2.8.0" % "1.6.5"

  Credentials(Path.userHome / ".ivy2" / "credentials", log)
  val publishTo = "nexus" at "http://nexus.scala-tools.org/content/repositories/releases/"

  override def pomExtra =
    <licenses>
      <license>
        <name>Apache 2</name>
        <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
        <distribution>repo</distribution>
      </license>
      <license>
        <name>Scala License</name>
        <url>http://www.scala-lang.org/node/146</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
}
