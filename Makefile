#PKGS=raylib glfw3 glu gl glew xrandr xxf86vm sdl2 SDL2_ttf
#PKGS=sdl2 SDL2_ttf glfw3 gl glew xrandr xxf86vm 
PKGS=cairo
CFLAGS=-Wall -Wextra -ggdb -pedantic -std=c11 `pkg-config --cflags --static $(PKGS)`
LIBS=-lm `pkg-config --libs --static $(PKGS)`

main: main.c
	gcc $(CFLAGS) -o main main.c $(LIBS)

clean:
	rm -rf main

.PHONY: clean

