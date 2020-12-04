ROOT = $(dir $(realpath $(firstword $(MAKEFILE_LIST))))

package-and-run:
	mvn -q clean install -DskipTests && java -jar ${ROOT}/target/aoc-1.0-SNAPSHOT.jar

run:
	mvn exec:java -Dexec.mainClass="aoc.App"
