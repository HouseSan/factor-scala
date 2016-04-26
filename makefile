interpreter:
	mkdir -p classes
	scalac -d classes interpreter.scala

run:
	scala -classpath classes interpreter
