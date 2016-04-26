interpreter: interpreter.scala main.scala
	mkdir -p classes
	scalac -d classes $^

run:
	scala -classpath classes Main
