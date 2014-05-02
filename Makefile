all: dleak.so

SRC=dleak.c callcontext.c

dleak.so: $(SRC)
	gcc -Wall -g -pthread -shared -fPIC -o dleak.so $(SRC) -ldl

distclean: clean
	rm -f dleak.so

clean:
	rm -f *~ core

