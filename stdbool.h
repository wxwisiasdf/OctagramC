#ifndef _STDBOOL_H_
#define _STDBOOL_H_ 1

#ifdef __STDC__
#ifndef __STDC_VERSION__
typedef enum { false = 0, true } bool;
#else
#define bool _Bool
#endif
#else
#define bool _Bool
#endif

#endif
