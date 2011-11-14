#include <stdio.h>
#include <string.h>
#include <iostream>
#include <list>
#include <vector>
#include <typeinfo>

#include "tutorial02.h"

#ifdef __cplusplus
extern "C" {
#endif

int getinformation()
{
	std::vector<std::string> vec;
	int c = getinformation2(19,810);
	return c;
}

#ifdef __cplusplus
}
#endif
