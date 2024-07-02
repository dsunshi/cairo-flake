PKGS=cairo
CFLAGS=-Wall -Wextra -ggdb -pedantic -std=c11 `pkg-config --cflags --static $(PKGS)`
LIBS=-lm `pkg-config --libs --static $(PKGS)`

main: main.c
	gcc $(CFLAGS) -o polydraw main.c $(LIBS)

clean:
	rm -rf polydraw

.PHONY: clean

