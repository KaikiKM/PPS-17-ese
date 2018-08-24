name := "PPS-17-ese"

version := "0.1"

scalaVersion := "2.12.6"

libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.5" % "test"

//markup-loading dependencies
libraryDependencies += "org.yaml" % "snakeyaml" % "1.21"
libraryDependencies += "net.jcazevedo" %% "moultingyaml" % "0.4.0"
libraryDependencies += "org.danilopianini" % "thread-inheritable-resource-loader" % "0.3.0"
libraryDependencies += "commons-io" % "commons-io" % "2.6"
libraryDependencies ++= Seq(
  "com.beachape" %% "enumeratum" % "1.5.13"
)
libraryDependencies += "org.scalafx" %% "scalafx" % "8.0.144-R12"
libraryDependencies += "it.unibo.alice.tuprolog" % "tuprolog" % "3.2"
