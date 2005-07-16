
CFLAGS=-Wall -pedantic `sdl-config --cflags`
LDFLAGS=`sdl-config --libs` -lSDL_image -lSDL_ttf

low-level.so: gfx-base.c timer.c event.c font.c
	gcc -shared $(CFLAGS) $^ -o $@ $(LDFLAGS)
