lazy val wirth = (project in file(".")).settings(
  scalaVersion := "2.11.7",
	resolvers ++= Seq(
  	Resolver.sonatypeRepo("releases"),
  	Resolver.sonatypeRepo("snapshots")
	),
	libraryDependencies ++= Seq(
		"com.chuusai" %% "shapeless" % "2.2.5",
		"com.lihaoyi" %% "fastparse" % "0.3.3",
		"org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
	)
)
