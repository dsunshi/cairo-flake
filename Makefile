PKGS=cairo
CFLAGS=-Wall -Wextra -ggdb -pedantic -std=c11 `pkg-config --cflags --static $(PKGS)`
LIBS=-lm `pkg-config --libs --static $(PKGS)`

bin/polydraw: src/main.c
	gcc $(CFLAGS) -o $@ $^ $(LIBS)

clean:
	rm -rf bin/polydraw

.PHONY: clean

