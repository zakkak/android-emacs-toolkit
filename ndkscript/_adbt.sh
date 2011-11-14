# run sh in adb
# e.g. 
# adb push "C:/android-ndk-r5b/myscript/_adbt.sh" /data/adbt.sh
# adb shell
# chmod 777 /data/adbt.sh
# /data/adbt.sh path name
#
echo "[Project path on adb: $1  Exe: $2]" 
#DESTINATION=$1 #e.g. '/data/myTest'
chmod 777 $1/$2
LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$1
$1/$2

