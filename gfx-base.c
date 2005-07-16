
#include "SDL.h"
#include "SDL_image.h"
#include <assert.h>
#include <stdlib.h>
#include <stdint.h>

/* Module-wide variables */
static SDL_Surface* vsurface;

void ll_gfx_free_surface(void* sface);


void* ll_gfx_init(int fullscreen, int width, int height, int bpp)
{
    int modeflags;
    SDL_Surface* vbuffer;

    assert(SDL_Init(SDL_INIT_VIDEO|SDL_INIT_AUDIO|SDL_INIT_TIMER|SDL_INIT_JOYSTICK) == 0);
    atexit(SDL_Quit);

    modeflags = SDL_HWSURFACE|SDL_SWSURFACE|SDL_DOUBLEBUF|SDL_HWPALETTE;

    if(fullscreen)
	modeflags ^= SDL_FULLSCREEN;

    vsurface = SDL_SetVideoMode(width, height, bpp, modeflags);
    assert(vsurface != NULL);

    SDL_ShowCursor(0);

    if(vsurface->flags&SDL_DOUBLEBUF)
        vbuffer = vsurface;
    else
        vbuffer = SDL_CreateRGBSurface(SDL_SWSURFACE,
                vsurface->w, vsurface->h,
                vsurface->format->BitsPerPixel, vsurface->format->Rmask,
                vsurface->format->Gmask, vsurface->format->Bmask,
                vsurface->format->Amask);
    assert(vbuffer != NULL);
    return vbuffer;
}

void ll_gfx_shutdown(void* vbuffer)
{
    if(vbuffer != vsurface)
	ll_gfx_free_surface(vbuffer);
    SDL_QuitSubSystem(SDL_INIT_VIDEO);
}


void* ll_gfx_new_image_buffer(int width, int height)
{
    SDL_Surface *img;

    img = SDL_CreateRGBSurface(SDL_SWSURFACE, width, height,
        vsurface->format->BitsPerPixel, vsurface->format->Rmask,
        vsurface->format->Gmask, vsurface->format->Bmask,
        vsurface->format->Amask);
    if(img == NULL) return NULL;
    SDL_SetColors(img, vsurface->format->palette->colors, 0,
            vsurface->format->palette->ncolors);
    SDL_SetColorKey(img, SDL_SRCCOLORKEY|SDL_RLEACCEL, 0);

    return img;
}

void* ll_gfx_load_image(char *filename, int colorkey)
{
    SDL_Surface *img;

    img = IMG_Load(filename);
    if(colorkey)
        SDL_SetColorKey(img, SDL_SRCCOLORKEY|SDL_RLEACCEL, 0);
    return img;
}

void ll_gfx_free_surface(void* sface)
{
    SDL_FreeSurface((SDL_Surface*)sface);
}


void ll_gfx_use_image_palette(void* img_, void* vbuffer_)
{
    SDL_Surface *img = img_, *vbuffer = vbuffer_;

    if(img && img->format->palette) {
        assert(SDL_SetColors(vsurface, img->format->palette->colors, 0,
                img->format->palette->ncolors) == 1);
        assert(SDL_SetColors(vbuffer, img->format->palette->colors, 0,
                img->format->palette->ncolors) == 1);
    }
}


void ll_gfx_refresh_display(void* vbuffer_)
{
    SDL_Surface *vbuffer = vbuffer_;

    if((vsurface->flags&SDL_DOUBLEBUF) == 0)
	SDL_BlitSurface(vbuffer, NULL, vsurface, NULL);

    SDL_Flip(vsurface);
}


int ll_gfx_surface_w(void* sface)
{
    return ((SDL_Surface*)sface)->w;
}

int ll_gfx_surface_h(void* sface)
{
    return ((SDL_Surface*)sface)->h;
}


/* XXX should do something if this fails! */
void ll_gfx_lock_surface(void* sface)
{
    SDL_LockSurface((SDL_Surface*)sface);
}

void ll_gfx_unlock_surface(void* sface)
{
    SDL_UnlockSurface((SDL_Surface*)sface);
}


void ll_gfx_blit_surface_stub(void* src, int x, int y, int w, int h,
                              void* dst, int x2, int y2)
{
    SDL_Rect srect, drect;

    drect.x = x2;
    drect.y = y2;
    if(w >= 0) {
        srect.x = x;  srect.y = y;  srect.w = w;  srect.h = h;
        SDL_BlitSurface(src, &srect, dst, &drect);
    } else
        SDL_BlitSurface(src, NULL, dst, &drect);
}

void ll_gfx_blit_surface(void* src, SDL_Rect* srect,
			 void* dst, SDL_Rect* drect)
{
    SDL_BlitSurface(src, srect, dst, drect);
}


/* Assumes we're already locked. */
void ll_gfx_draw_pixel(void* sface_, int x, int y, int color)
{
    SDL_Surface* sface = sface_;

    if(sface->format->BytesPerPixel == 1) {
	((Uint8 *)sface->pixels)[x+y*sface->pitch] = color;
    } else if(sface->format->BytesPerPixel == 4) {
	((Uint32 *)sface->pixels)[x+y*(sface->pitch>>2)] = color;
    } else			/* unsupported. */
	assert(0);
}

void ll_gfx_fill_rect_stub(void* sface, int x, int y, int w, int h, int color)
{
    SDL_Rect r;

    if(w >= 0) {
        r.x = x; r.y = y; r.w = w; r.h = h;
        SDL_FillRect(sface, &r, color);
    } else SDL_FillRect(sface, NULL, color);
}

void ll_gfx_fill_rect(void* sface, SDL_Rect* rect, int color)
{
    SDL_FillRect(sface, rect, color);
}
