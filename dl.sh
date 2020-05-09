#!/bin/sh
set -e

curl -L -o mac.dmg 'https://www.runescape.com/downloads/OldSchool.dmg'
7z x mac.dmg 'Old School RuneScape/Old School RuneScape.app/Contents/Java/jagexappletviewer.jar'
mv 'Old School RuneScape/Old School RuneScape.app/Contents/Java/jagexappletviewer.jar' jagexappletviewer.jar

rm -rf mac.dmg 'Old School Runescape/'
