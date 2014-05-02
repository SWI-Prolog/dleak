all: dleak.so

SRC=dleak.c md5.c callcontext.c

dleak.so: $(SRC)
	gcc -Wall -O2 -g -pthread -shared -fPIC -o dleak.so $(SRC)

distclean: clean
	rm -f dleak.so

clean:
	rm -f *~ core

