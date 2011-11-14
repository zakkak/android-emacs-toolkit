LOCAL_PATH := $(call my-dir)
#获取当前目录
include $(CLEAR_VARS)
#清除一些变量
LOCAL_MODULE    := tutorial
#要生成的库名
LOCAL_SRC_FILES := tutorial01.cpp tutorial02.cpp
LOCAL_LDLIBS := -llog 
#库对应的源文件
include $(BUILD_SHARED_LIBRARY)
#生成动态库libtutorial.so

include $(CLEAR_VARS)
#清除一些变量
LOCAL_MODULE    := test
#定义另外一个库的名
LOCAL_SRC_FILES := test01.cpp
#定义库对应的源文件
LOCAL_LDLIBS := -ldl -llog 
LOCAL_SHARED_LIBRARIES := tutorial
#libtest.so需要引用的库libdl.so:加载动态函数需要，liblog.so 日志打印需要，默认是system/lib目录下
include $(BUILD_SHARED_LIBRARY)
#生成共享库


include $(CLEAR_VARS)

LOCAL_MODULE    := helloworld

LOCAL_SRC_FILES := helloworld.cpp

LOCAL_LDLIBS := -ldl -llog 

include $(BUILD_EXECUTABLE)


