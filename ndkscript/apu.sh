#!/bin/sh
# Unistall the apk 
# Please cd to the folder of the apk
#
if [ "$#" -lt 1 ]; then
    echo "Usage: api.sh input" 
else
    echo 'Uninstalling apk... '$1 
    APKCLASS=$1 # 'com.example.test'
    adb uninstall $APKCLASS
    if [ "$#" == "2" ]; then
        APK=$2 # 'F:/workspace/test01/bin/test01.apk'
        rm $APK
    fi
fi
