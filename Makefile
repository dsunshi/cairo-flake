PKGS=cairo
CFLAGS=-Wall -Wextra -ggdb -pedantic -std=c11 `pkg-config --cflags --static $(PKGS)`
LIBS=-lm `pkg-config --libs --static $(PKGS)`

all: bin/polydraw

src/Draw.o: src/Draw.hs
	ghc -threaded -fforce-recomp -shared -dynamic -fPIC -c $^

bin/polydraw: src/main.c src/Draw.o
	gcc $(CFLAGS) -o $@ $^ $(LIBS)

clean:
	rm -rf bin/polydraw
	rm -rf src/*_stub.h
	rm -rf src/*.o
	rm -rf src/*.hi

.PHONY: clean

