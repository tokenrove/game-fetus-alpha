
#include "SDL.h"
#include <assert.h>
#include <string.h>
#include <stdio.h>

struct ll_event_s {
    int type;
    int value;
    int axis;
};


static SDL_Joystick *joystick;


/* ll_event_init -- Setup joysticks, basically. */
void ll_event_init(void)
{
    /* Note: we only support one joystick at the moment. */
    if(SDL_NumJoysticks() > 0) {
        joystick = SDL_JoystickOpen(0);
        /* not that we ever actually use the joystick variable... */
        SDL_JoystickEventState(SDL_ENABLE);
    }
}


void ll_event_shutdown(void)
{
    if(SDL_JoystickOpened(0))
        SDL_JoystickClose(joystick);
    joystick = NULL;
}


int ll_poll_event(struct ll_event_s* event)
{
    SDL_Event ev;
    int rv;

    rv = SDL_PollEvent(&ev);
    if(!rv) return rv;

    memset(event, 0, sizeof(struct ll_event_s));
    if(ev.type == SDL_KEYDOWN) {
	event->type = 2;
	event->value = ev.key.keysym.sym;
    } else if(ev.type == SDL_KEYUP) {
	event->type = 3;
	event->value = ev.key.keysym.sym;
    } else if(ev.type == SDL_JOYAXISMOTION) {
        event->type = 4;
        assert(ev.jaxis.which == 0);
        event->axis = ev.jaxis.axis;
        event->value = (ev.jaxis.value > 0) ? 1 : 
	    (ev.jaxis.value < 0) ? -1 : 0;
    } else if(ev.type == SDL_JOYBUTTONDOWN) {
        event->type = 5;
        assert(ev.jbutton.which == 0);
        event->value = ev.jbutton.button;
    } else if(ev.type == SDL_JOYBUTTONUP) {
        event->type = 6;
        assert(ev.jbutton.which == 0);
        event->value = ev.jbutton.button;
    }
    return rv;
}


int ll_wait_event(struct ll_event_s* event)
{
    SDL_Event ev;
    int rv;

    rv = SDL_WaitEvent(&ev);
    if(rv && ev.type == SDL_KEYDOWN) {
	event->type = 2;
	event->value = ev.key.keysym.sym;
    } else if(rv && ev.type == SDL_KEYUP) {
	event->type = 3;
	event->value = ev.key.keysym.sym;
    }
    /* Note: joystick events ignored. */
    return rv;
}
