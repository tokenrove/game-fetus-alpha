/*
 * low-level timer
 *
 * warning: we make the assumption that SDL is already initialized
 * before anything here gets used.
 * 
 * tokenrove / 2003
 */

#include "SDL.h"
#include <time.h>

/* Change this undef to a define to use nanosleep instead of SDL_Delay() */
#ifdef __USE_POSIX
#undef USE_NANOSLEEP
#endif

static unsigned long frame_end;


unsigned long timer_get_ticks(void);
void timer_start_frame(int frame_length);
int timer_end_frame(void);

/* timer_get_ticks()
 * Returns the number of ticks since the SDL library was initialized. */
unsigned long timer_get_ticks(void)
{
    return SDL_GetTicks();
}


/* timer_start_frame()
 * Marks the beginning of a frame, such that it can be synchronized by
 * timer_end_frame().  As per the SDL warnings, frame_length's
 * resolution can't be expected to be better than 10ms. */
void timer_start_frame(int frame_length)
{
    frame_end = SDL_GetTicks() + frame_length;
}


/* timer_end_frame()
 * Syncs the program with the frame timer.  Returns 1 if we've overshot
 * the allocated time, 0 otherwise. */
int timer_end_frame(void)
{
    unsigned long now = SDL_GetTicks();
#ifdef USE_NANOSLEEP
    struct timespec ts;
#endif

    if(now > frame_end) return 1;

#ifdef USE_NANOSLEEP
    ts.tv_sec = 0;
    ts.tv_nsec = (frame_end-now)*1000;
    nanosleep(&ts, NULL);
#else
    SDL_Delay(frame_end-now);
#endif
    return 0;
}


/* EOF timer.c */
