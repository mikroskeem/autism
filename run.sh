#!/bin/sh
set -e

mkdir -p cache
"${JAVA_HOME}"/bin/java \
	-Duser.home=cache \
	-Dsun.awt.noerasebackground=true \
	-Dcom.jagex.configuri=jagex-jav://oldschool.runescape.com/jav_config.ws \
	-cp "${JAR:-jagexappletviewer.jar}" jagexappletviewer \
	"$(basename "${PWD}")"
