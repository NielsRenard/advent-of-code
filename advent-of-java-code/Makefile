ROOT = $(dir $(realpath $(firstword $(MAKEFILE_LIST))))

exec:
	@mvn -q clean install -DskipTests && java -jar ${ROOT}/target/aoc-1.0-SNAPSHOT.jar

run:exec
