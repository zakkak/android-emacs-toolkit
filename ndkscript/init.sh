#!/bin/sh
# Init environment
# Change to your path
#
echo "Init enviroment for Android NDK and SDK"
if [ "$#" != "3" ]; then
    echo "Usage: init.sh ndkscriptpath androidndkpath androidsdkpath"
else
    # ndk script path
    export NDKSCRIPTPATH=$1

    # init NDK path
    export NDK=$2
    # export ANDROID_NDK_ROOT=$2

    # init SDK path
    export ANDROID_SDK=$3

    # formate ndk script path
    if [ "${NDKSCRIPTPATH:1:1}" == ":" ]; then
        TMPNDKSCRIPTPATH="/cygdrive/"${NDKSCRIPTPATH:0:1}${NDKSCRIPTPATH:2}
    else
        TMPNDKSCRIPTPATH=$NDKSCRIPTPATH
    fi

    # formate ndk root path
    if [ "${NDK:1:1}" == ":" ]; then
        TMPNDK="/cygdrive/"${NDK:0:1}${NDK:2}
    else
        TMPNDK=$NDK
    fi

    # formate sdk root path
    if [ "${ANDROID_SDK:1:1}" == ":" ]; then
        TMPSDK="/cygdrive/"${ANDROID_SDK:0:1}${ANDROID_SDK:2}
    else
        TMPSDK=$ANDROID_SDK
    fi

    # add some path to PATH
    export PATH=$TMPNDKSCRIPTPATH:$TMPNDK:$TMPSDK/tools:$TMPSDK/platform-tools:$TMPNDK/toolchains/arm-linux-androideabi-4.4.3/prebuilt/linux-x86/bin:$PATH
fi
