#ifndef POLYDRAW_H
#define POLYDRAW_H

/* _DEFAULT_SOURCE is needed for usleep */
#define _DEFAULT_SOURCE

#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xresource.h>
#include <cairo.h>
#include <cairo-xlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <time.h>

#define WAIT 5000

/* Defaults */

#ifndef DEFAULT_DISPLAY
    #define DEFAULT_DISPLAY NULL /* On a POSIX-conformant system, if the display_name is NULL, it defaults to the value of the DISPLAY environment variable. */
#endif

void openWindow(void);

#endif /* POLYDRAW_H */
