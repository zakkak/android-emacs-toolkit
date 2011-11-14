
#include <android/log.h>

#define  LOG_TAG    "my2"
#define  LOGI(...)  __android_log_print(ANDROID_LOG_INFO,LOG_TAG,__VA_ARGS__)
#define  LOGE(...)  __android_log_print(ANDROID_LOG_ERROR,LOG_TAG,__VA_ARGS__)

class  A
{
public :
	virtual   void  fun1() = 0;
};
class  B
{
public :
	virtual   void  fun2(){};
};
class  C: public  A
{
public :
	void  fun1();
};

class  D: public  C, public  B
{
public :
	void  fun1();

	void  fun2();
};

int getinformation2(int,int);
