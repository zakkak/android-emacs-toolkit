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

	# init SDK path
    export ANDROID_SDK=$3

    # add some path to PATH
    export PATH=$NDKSCRIPTPATH:$NDK:$ANDROID_SDK/tools:$ANDROID_SDK/platform-tools:$PATH
fi
