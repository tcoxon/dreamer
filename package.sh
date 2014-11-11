#!/bin/bash
set -ex

KEYSTORE=""
KEYSTORE_ALIAS=dreamer1
if test "x$1" == "x"; then
    echo "Provide path to the keystore" >&2
    exit 1
else
    KEYSTORE="$1"
fi

sbt assembly

JAR=target/scala-2.9.3/dreamer.jar

if test "x$KEYSTORE" != "x"; then
    jarsigner -keystore $KEYSTORE $JAR $KEYSTORE_ALIAS
fi
cp $JAR html/
cp res/Beeb.ttf html/


STAGING=$(mktemp -d)
STAGING1=$STAGING/dreamer
mkdir $STAGING1
cp $JAR $STAGING1
cp README.md $STAGING1/README.txt
pushd $STAGING
    zip -rm dreamer.zip dreamer
popd
mv $STAGING/dreamer.zip html/dreamer.zip

