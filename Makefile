PKGS=cairo x11
CFLAGS=-Wall -Wextra -ggdb -pedantic -std=c11 `pkg-config --cflags $(PKGS)`
LIBS=-lm `pkg-config --libs $(PKGS)`
HASKELL=--make -dynamic -shared -fPIC -flink-rts

all: bin/polydraw

src/Draw.o: src/Draw.hs
	ghc -c -O $^

polydraw.o: src/polydraw.c
	gcc $(CFLAGS) -c $^

bin/polydraw: polydraw.o src/Draw.o
	ghc -o $@ $^ $(LIBS)

clean:
	rm -rf bin/polydraw
	rm -rf src/*_stub.h
	rm -rf src/*.o
	rm -rf *.o
	rm -rf src/*.out
	rm -rf *.out
	rm -rf src/*.hi

.PHONY: clean

