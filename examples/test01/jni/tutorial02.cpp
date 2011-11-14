#include <stdio.h>
#include <string.h>
#include <typeinfo>

#include "tutorial02.h"


void C::fun1()
{
	LOGE("called C::fun1");
}

void D::fun1()
{
	LOGE("called D::fun1");
}

void D::fun2()
{
	LOGE("called D::fun2");
}

int getinformation2(int i,int j)
{
	//// typeid
	//B  * p;
	//D ob;
	//p =& ob;
	//LOGE(" typeid(*p).name()= ");
	//LOGE(typeid( * p).name());

	//// dynamic_cast
	//A* a = new D();
	//a->fun1();
	//B* b = dynamic_cast<B*>(a);
	//if (!b)
	//{
	//	LOGE("Error: dynamic_cast");
	//}
	//b->fun2();

	B* a = new D();
	//return (int)(dynamic_cast<A*>(a));
	return (int)((A*)a);
}

