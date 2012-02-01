#!/bin/sh
# Install the apk 
#
if [ "$#" != "1" ] && [ "$#" != "3" ]; then
    echo "Usage: api.sh input" 
else
    # install all apk in folder
    for file in ` ls $1 `
    do
        TMPFILE=$1'/'$file
        if [ -f $TMPFILE ] && [ "${file##*.}" = "apk" ]; then
            echo 'Installing apk... '$TMPFILE
            adb install $TMPFILE
        fi
    done
    # run apk
    if [ "$#" == "3" ]; then
        adb shell am start -n $2'/'$2'.'$3
    fi

fi