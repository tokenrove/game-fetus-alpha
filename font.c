
#include "SDL.h"
#include "SDL_ttf.h"
#include <assert.h>
#include <stdlib.h>

void ll_font_init(void)
{
    assert(TTF_Init() == 0);
    atexit(TTF_Quit);
}


void* ll_font_open(char* filename, int ptsize)
{
    return TTF_OpenFont(filename, ptsize);
}

void ll_font_close(void* font)
{
    TTF_CloseFont((TTF_Font*)font);
}


void* ll_font_render_solid(void* font, const char* string, int r, int g, int b)
{
    SDL_Color fg;

    fg.r = r; fg.b = b; fg.g = g;
    return TTF_RenderText_Solid((TTF_Font*)font, string, fg);
}

void* ll_font_render_blended(void* font, const char* string,
			     int r, int g, int b)
{
    SDL_Color fg;

    fg.r = r; fg.b = b; fg.g = g;
    return TTF_RenderText_Blended((TTF_Font*)font, string, fg);
}

void* ll_font_render_shaded(void* font, const char* string,
			    int r1, int g1, int b1,
			    int r2, int g2, int b2)
{
    SDL_Color fg, bg;

    fg.r = r1; fg.b = b1; fg.g = g1;
    bg.r = r2; bg.b = b2; bg.g = g2;
    return TTF_RenderText_Shaded((TTF_Font*)font, string, fg, bg);
}


/*
 * ll_font_set_style
 * ll_font_height
 * ll_font_line_skip
 * ll_font_size_text
 * ll_font_ascent
 * ll_font_descent
 */
