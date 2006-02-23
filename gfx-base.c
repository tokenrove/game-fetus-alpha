
#include "SDL.h"
#include "SDL_image.h"
#include <assert.h>
#include <stdlib.h>
#include <stdint.h>

/* Module-wide variables */
static SDL_Surface* vsurface;
static int vsurface_scale;

void ll_gfx_free_surface(void* sface);


void* ll_gfx_init(int fullscreen, int scale, int width, int height, int bpp)
{
    int modeflags;
    SDL_Surface* vbuffer;
    const SDL_VideoInfo *vinfo;

    if(scale == 1 || scale == 2)
	vsurface_scale = scale;
    else
	return NULL;

    assert(SDL_Init(SDL_INIT_VIDEO|SDL_INIT_AUDIO|SDL_INIT_TIMER|SDL_INIT_JOYSTICK) == 0);
    atexit(SDL_Quit);

    modeflags = SDL_HWSURFACE|SDL_SWSURFACE|SDL_DOUBLEBUF|SDL_HWPALETTE;

    if(fullscreen)
	modeflags ^= SDL_FULLSCREEN;

    vinfo = SDL_GetVideoInfo();

    vsurface = SDL_SetVideoMode(scale*width, scale*height, bpp, modeflags);
    assert(vsurface != NULL);

    SDL_ShowCursor(0);

    vbuffer = SDL_CreateRGBSurface(SDL_SWSURFACE,
				   width, height,
				   bpp,
				   vsurface->format->Rmask,
				   vsurface->format->Gmask,
				   vsurface->format->Bmask,
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
    SDL_SetColorKey(img, SDL_SRCCOLORKEY|SDL_RLEACCEL, 0);
    SDL_SetColors(img, vsurface->format->palette->colors, 0,
		  vsurface->format->palette->ncolors);

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
        assert(SDL_SetColors(vbuffer, img->format->palette->colors, 0,
			     img->format->palette->ncolors) == 1);
        assert(SDL_SetColors(vsurface, img->format->palette->colors, 0,
			     img->format->palette->ncolors) == 1);
    }
}


/*
 * A simple implementation of the scale2x algorithm by Andrea Mazzoleni.
 * Could be optimized.
 */

#define SCALE2X_WRITE_PIXELS_M(p, x, y) \
   do {                                 \
       if(b != h && d != f) {           \
	   p[0]        = d == b ? d : e; \
           p[1]        = b == f ? f : e; \
           p[dpitch]   = d == h ? d : e; \
           p[1+dpitch] = h == f ? f : e; \
       } else {                    \
           p[0] = e;               \
           p[1] = e;               \
           p[dpitch] = e;          \
           p[1+dpitch] = e;        \
       }                           \
   } while(0)

void scale2x(SDL_Surface *src, SDL_Surface *dst)
{
    int x, y, pitch, dpitch;
    Uint8 a,b,c,d,e,f,g,h,i;
    Uint8 *spels, *dpels;

    SDL_LockSurface(src);
    SDL_LockSurface(dst);

    pitch = src->pitch;
    dpitch = dst->pitch;
    spels = (Uint8*)src->pixels;
    dpels = (Uint8*)dst->pixels;
    e = spels[0]; f = spels[1];
    h = spels[1*pitch]; i = spels[1+1*pitch];
    a = b = e; c = f; d = e; g = h;	/* border */
    SCALE2X_WRITE_PIXELS_M(dpels, 0, 0);
    for(y = 0, x = 1; x < src->w-1; x++) {
	d = e; e = f; f = spels[x+1];
	g = h; h = i; i = spels[x+1+pitch];
	a = e; b = d; c = f;
	SCALE2X_WRITE_PIXELS_M(dpels, x, 0);
	dpels += 2;
    }
    a = b; b = c;
    d = e; e = f;
    g = h; h = i;
    SCALE2X_WRITE_PIXELS_M(dpels, x, 0);

    dpels = (Uint8*)dst->pixels;
    dpels += dpitch*2;
    for(y = 1; y < src->h-1; y++) {
	spels += pitch;
	b = spels[-pitch]; c = spels[1+-pitch];
	e = spels[0]; f = spels[1];
	h = spels[pitch]; i = spels[1+pitch];
	a = b; d = e; g = h;	/* border */
	SCALE2X_WRITE_PIXELS_M(dpels, 0, y);
	dpels += 2;
	for(x = 1; x < src->w-1; x++) {
	    a = b; b = c; c = spels[x+1-pitch];
	    d = e; e = f; f = spels[x+1];
	    g = h; h = i; i = spels[x+1+pitch];
	    SCALE2X_WRITE_PIXELS_M(dpels, x, y);
	    dpels += 2;
	}
	a = b; b = c;
	d = e; e = f;
	g = h; h = i;
	SCALE2X_WRITE_PIXELS_M(dpels, x, y);
	dpels -= x*2;
	dpels += dpitch*2;
    }

    b = spels[-pitch]; c = spels[1-pitch];
    e = spels[0]; f = spels[1];
    a = b; d = e; g = d; h = e; i = f;	/* border */
    SCALE2X_WRITE_PIXELS_M(dpels, 0, y);
    for(x = 1; x < src->w-1; x++) {
	a = b; b = c; c = spels[x+1-pitch];
	d = e; e = f; f = spels[x+1];
	g = d; h = e; i = f;
	SCALE2X_WRITE_PIXELS_M(dpels, x, y);
    }
    a = b; b = c;
    d = e; e = f;
    g = h; h = i;
    SCALE2X_WRITE_PIXELS_M(dpels, x, y);

    SDL_UnlockSurface(dst);
    SDL_UnlockSurface(src);
}


void ll_gfx_refresh_display(void* vbuffer_)
{
    SDL_Surface *vbuffer = vbuffer_;

    if(vsurface_scale == 2)
	scale2x(vbuffer, vsurface);
    else
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

void ll_gfx_blit_surface(void* src, SDL_Rect* srect,
			 void* dst, int x, int y)
{
    SDL_Rect drect;

    drect.x = x;
    drect.y = y;
    SDL_BlitSurface(src, srect, dst, &drect);
}


/* Assumes we're already locked. */
void ll_gfx_draw_pixel(void* sface_, int x, int y, int color)
{
    SDL_Surface* sface = sface_;

    if(sface->format->BytesPerPixel == 1) {
	((Uint8 *)sface->pixels)[x+y*sface->pitch] = color;
    } else if(sface->format->BytesPerPixel == 2) {
	((Uint16 *)sface->pixels)[x+y*(sface->pitch>>1)] = color;
    } else if(sface->format->BytesPerPixel == 4) {
	((Uint32 *)sface->pixels)[x+y*(sface->pitch>>2)] = color;
    } else			/* unsupported. */
	assert(0);
}

void ll_gfx_fill_rect(void* sface, SDL_Rect* rect, int color)
{
    SDL_FillRect(sface, rect, color);
}
