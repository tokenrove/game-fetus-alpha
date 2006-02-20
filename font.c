
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


void* ll_font_render_solid(void* font, const char* string, SDL_Color fg)
{
    return TTF_RenderText_Solid((TTF_Font*)font, string, fg);
}

void* ll_font_render_blended(void* font, const char* string, SDL_Color fg)
{
    return TTF_RenderText_Blended((TTF_Font*)font, string, fg);
}

void* ll_font_render_shaded(font, string, fg, bg)
     void* font;
     const char* string;
     SDL_Color fg, bg;
{
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
