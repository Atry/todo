enablePlugins(ScalaJSPlugin)

libraryDependencies += "com.thoughtworks.binding" %%% "html" % "13.0.0-M0+197-aad220b3"
libraryDependencies += "com.thoughtworks.binding" %%% "keywords-bind" % "13.0.0-M0+197-aad220b3"
libraryDependencies += "com.thoughtworks.dsl" %%% "keywords-await" % "2.0.0-M2+300-ad3b467f"
libraryDependencies += "com.thoughtworks.dsl" %%% "keywords-get" % "2.0.0-M2+300-ad3b467f"
libraryDependencies += "com.thoughtworks.dsl" %%% "domains-scalaz" % "2.0.0-M2+300-ad3b467f"

libraryDependencies += "com.lihaoyi" %%% "upickle" % "1.4.3"

scalacOptions += "-Ykind-projector:underscores"

crossPaths := false

crossTarget in fullOptJS := baseDirectory.value

crossTarget in fastOptJS := baseDirectory.value
