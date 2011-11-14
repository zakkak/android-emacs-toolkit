#include <string.h>
#include <jni.h>
#include <dlfcn.h>
#include <android/log.h>
#include <stdlib.h>

#include "tutorial02.h"

#define  LOG_TAG    "libgl2jni"
#define  LOGI(...)  __android_log_print(ANDROID_LOG_INFO,LOG_TAG,__VA_ARGS__)
#define  LOGE(...)  __android_log_print(ANDROID_LOG_ERROR,LOG_TAG,__VA_ARGS__)

#ifdef __cplusplus
extern "C" {
#endif

 //extern int getinformation();
jint Java_com_example_test_test01_getinformation(JNIEnv* env,jobject thiz)
{
	//getinformation();   ??thr .so file will load to the sdcard with the folder data/data/com.example.test/lib/
	LOGI("Opening libtutorial.so...");
	void*  filehandle = dlopen("/data/data/com.example.test/lib/libtutorial.so", RTLD_LAZY );
	if(!filehandle)
	{
		LOGE("open so ERROR!");
		return -1;
	}

	LOGI("Loading symbol getinformation...");
	typedef int( * getinformation_t ) ();

	dlerror();
	getinformation_t getinformation = (getinformation_t)dlsym(filehandle, "getinformation");
	const char *dlsym_error = dlerror();
	if (dlsym_error)
	{
		LOGI("Cannot load symbol 'hello': ");
		LOGI(dlsym_error);
		dlclose(filehandle);
		return -1;
	}

	LOGI("Calling function getinformation...");
	int ll = getinformation();
	LOGI("return value=%d",ll);

	// dynamic_cast
	//A* a = (A*)ll;
	//a->fun1();
	//B* b = dynamic_cast<B*>(a);
	//B* b = dynamic_cast<B*>((C*)ll);
	B* b = (B*)ll;
	if (!b)
	{
		LOGE("Error: dynamic_cast");
	}
	b->fun2();

	dlclose(filehandle);
	filehandle=0;
    return ll;
}

#ifdef __cplusplus
}
#endif
