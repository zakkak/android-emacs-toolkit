#!/bin/sh
# Build or Rebuild with NDK
# Please cd to the NDK root folder
#
if [ "$1" != "r" ]; then
    echo "Building with NDK..."
else
    echo 'Rebuilding the NDK...'
    $NDK/ndk-build clean
fi
$NDK/ndk-build

# # Copy libs to workspace
# if [ "$#" == "2" ]; then
# #    SOURSE="$PWD/libs/armeabi"
#     SOURSE=${PWD/'jni'/''}"libs/armeabi"
#     if [ ${SOURSE:0:10} == "/cygdrive/" ]; then
#         SOURSE=${SOURSE/'/cygdrive/'/''}
#         SOURSE=${SOURSE:0:1}':'${SOURSE:1}
#     fi

#     DESTINATION=$2/libs # 'F:/workspace/test01/libs'
#     echo "[Copy file form $SOURSE to $DESTINATION]"
#     cp -R $SOURSE $DESTINATION
# fi
