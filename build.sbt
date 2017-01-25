name := "backend"

version := "1.0"

scalaVersion := "2.11.0"

// https://mvnrepository.com/artifact/org.eclipse.jgit/org.eclipse.jgit
libraryDependencies += "org.eclipse.jgit" % "org.eclipse.jgit" % "4.5.0.201609210915-r"
// https://mvnrepository.com/artifact/org.slf4j/slf4j-simple
libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.21"

libraryDependencies += "org.scala-graph" %% "graph-core" % "1.11.4"
libraryDependencies += "org.scala-graph" %% "graph-json" % "1.11.0"

//Webserver
libraryDependencies ++= {
    val akkaV = "2.4.16"
    val sprayV = "1.3.4"
    Seq(
        "io.spray"            %  "spray-can_2.11"     % sprayV,
        "io.spray"            %  "spray-routing_2.11" % sprayV,
        "io.spray"            %  "spray-testkit_2.11" % sprayV  % "test",
        "com.typesafe.akka"   %%  "akka-actor"    % akkaV,
        "com.typesafe.akka"   %%  "akka-testkit"  % akkaV   % "test",
        "org.specs2"          %%  "specs2-core"   % "3.8.7" % "test"
    )
}
