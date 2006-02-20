
CFLAGS=-Wall -pedantic -std=c99 `sdl-config --cflags`
LDFLAGS=`sdl-config --libs` -lSDL_image -lSDL_ttf
CC=gcc

.PHONY: clean

low-level.so: gfx-base.c timer.c event.c font.c
	$(CC) -shared $(CFLAGS) $^ -o $@ $(LDFLAGS)

clean:
	$(RM) low-level.so
