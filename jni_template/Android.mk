LOCAL_PATH := $(call my-dir)

include $(CLEAR_VARS)

LOCAL_MODULE    := helloworld

LOCAL_SRC_FILES := helloworld.cpp

LOCAL_LDLIBS := -ldl -llog 

include $(BUILD_EXECUTABLE)


