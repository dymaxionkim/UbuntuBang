#include <stdarg.h>
#include "extUtil.h"

void errMsg(char *format, ...){
	va_list args;
	va_start(args, format);
	vprintf(format, args);
#ifndef WIN32
	printf("\a");
#endif
}

void printf_fflush(const char *format, ...){
	va_list args;
	va_start(args, format);
	vprintf(format, args);
	fflush(stdout);
}
