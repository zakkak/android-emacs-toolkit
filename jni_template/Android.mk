LOCAL_PATH := $(call my-dir)

include $(CLEAR_VARS)

LOCAL_MODULE    := helloworld

LOCAL_SRC_FILES := helloworld.cpp

LOCAL_LDLIBS := -ldl -llog 

include $(BUILD_EXECUTABLE)



include $(CLEAR_VARS)

LOCAL_MODULE    := hello-jni

LOCAL_SRC_FILES := hello-jni.c

include $(BUILD_SHARED_LIBRARY)
