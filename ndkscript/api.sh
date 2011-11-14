#!/bin/sh
# Install the apk 
#
if [ "$#" != "1" ]; then
    echo "Usage: api.sh input" 
else
    for file in ` ls $1 `
    do
        TMPFILE=$1'/'$file
        if [ -f $TMPFILE ] && [ "${file##*.}" = "apk" ]; then
            echo 'Installing apk... '$TMPFILE
            adb install $TMPFILE
        fi
    done
fi