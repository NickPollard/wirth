lazy val root = (project in file(".")).settings(
  scalaVersion := "2.11.7",
	resolvers ++= Seq(
  	Resolver.sonatypeRepo("releases"),
  	Resolver.sonatypeRepo("snapshots")
	),
	libraryDependencies ++= Seq(
		"com.chuusai" %% "shapeless" % "2.2.5",
		"com.lihaoyi" %% "fastparse" % "0.3.3"
	)
)
