enablePlugins(ScalaJSPlugin)

libraryDependencies += "com.thoughtworks.binding" %%% "html" % "13.0.0-M0+67-c054d010"
libraryDependencies += "com.thoughtworks.binding" %%% "keywords-bind" % "13.0.0-M0+67-c054d010"
libraryDependencies += "com.thoughtworks.dsl" %%% "keywords-await" % "2.0.0-M2+265-3c2f0891"
libraryDependencies += "com.thoughtworks.dsl" %%% "keywords-get" % "2.0.0-M2+265-3c2f0891"
libraryDependencies += "com.thoughtworks.dsl" %%% "domains-scalaz" % "2.0.0-M2+265-3c2f0891"

libraryDependencies += "com.lihaoyi" %%% "upickle" % "1.4.3"

scalacOptions += "-Ykind-projector:underscores"

crossPaths := false

crossTarget in fullOptJS := baseDirectory.value

crossTarget in fastOptJS := baseDirectory.value
