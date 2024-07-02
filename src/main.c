#include "polydraw.h"

/* Global state */
static unsigned int canvasWidth  = 400;
static unsigned int canvasHeight = 400;

static int update(double dt) {
    (void) dt;
    return 1;
}

static int loop(cairo_t* cr,cairo_surface_t* surface){
    int repeat = update((double)WAIT / 1000000.0);

    cairo_push_group(cr);
    cairo_set_source_rgb(cr, 1, 1, 1);
    cairo_paint(cr);
    /* cairo_set_source_rgb(cr, 0, 0, 0); */


    cairo_pop_group_to_source(cr);
    cairo_paint(cr);
    cairo_surface_flush(surface);
    return repeat;
}

int main (int argc, char** argv) {
    (void) argc;
    (void) argv;
    srand(time(NULL));

    Display *display = XOpenDisplay(DEFAULT_DISPLAY);

    if (NULL == display) {
        fprintf(stderr, "Unable to open connection to X server");
        return 1;
    }

    const int screen  = DefaultScreen(display);
    Visual *visual    = DefaultVisual(display, screen);
    Drawable drawable = XCreateSimpleWindow(display,
                        DefaultRootWindow(display),
                        0, 0,               /* x, y */
                        canvasWidth, canvasHeight,
                        0,                  /* border_width */
                        0,                  /* border */
                        0);                 /* background */
    XMapWindow(display, drawable);

    cairo_surface_t *surface = cairo_xlib_surface_create(display,
                                drawable, visual,
                                canvasWidth, canvasHeight);
    cairo_xlib_surface_set_size(surface, canvasWidth, canvasHeight);
    cairo_t* cr = cairo_create(surface);

    while(loop(cr, surface)) usleep(WAIT);
    sleep(2);


    cairo_surface_destroy(surface);
    cairo_destroy(cr);

    XCloseDisplay(display);

  return 0;
}

