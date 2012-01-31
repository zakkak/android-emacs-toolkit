#!/bin/sh
# Run exe on adb
# Parameters: projectName projectPath
#
echo 'Running on adb...'
if [ "$#" != "2" ]; then
    echo "Usage: adr.sh projectName projectPath" 
else
    DESTINATION="/data/"$1 # '/data/myTest'
    LIBPATH=$2"/libs/armeabi" # 'D:/home/gccProject/COPF/android_ndk/libs/armeabi'
    echo "[ProjectName: "$1" Projectpath: "$2"]"

    if [ "$NDKSCRIPTPATH" == "" ]; then
        baseDirForScriptSelf=$(cd "$(dirname "$0")"; pwd) 
        #echo "full path to currently executed script is : ${baseDirForScriptSelf}"
        if [ "${baseDirForScriptSelf:0:10}" == "/cygdrive/" ]; then
            baseDirForScriptSelf=${baseDirForScriptSelf/'/cygdrive/'/''}
            NDKSCRIPTPATH=${baseDirForScriptSelf:0:1}':'${baseDirForScriptSelf:1}
        else
            NDKSCRIPTPATH=$baseDirForScriptSelf
        fi
    fi

    function tranLibs(){
        for file in ` ls $1 `
        do
            if [ -d $1"/"$file ]; then
                tranLibs $1"/"$file
            else
                echo "Installing..."$1"/"$file 
                adb push "$LIBPATH/$file" "$DESTINATION/$file"
            fi
        done
    }
    tranLibs $LIBPATH

    echo "Installing...adbt.sh" 
    adb push "$NDKSCRIPTPATH/_adbt.sh" $DESTINATION/adbt.sh
    adb shell "chmod 777 $DESTINATION/adbt.sh"

    function runExe(){
        for file in ` ls $1 `
        do
            if [ -d $1"/"$file ]; then
                runExe $1"/"$file
            else
                tmpIndex=`expr index "$file" "b."`
                if [ $tmpIndex == 0 ]; then
                    echo "Running..."$1"/"$file
                    adb shell $DESTINATION/adbt.sh "$DESTINATION" "$file" 
                fi
            fi
        done
    }
    runExe $LIBPATH
fi
