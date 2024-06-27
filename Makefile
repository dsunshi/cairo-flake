PKGS=raylib glfw3 glu gl glew xrandr xxf86vm
CFLAGS=-Wall -Wextra -ggdb -pedantic -std=c11 `pkg-config --cflags --static $(PKGS)`
LIBS=`pkg-config --libs --static $(PKGS)`

main: main.c
	g++ $(CFLAGS) -o main main.c $(LIBS) `pkg-config --libs --cflags sdl2 SDL2_ttf`

