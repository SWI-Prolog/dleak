all: dleak.so

dleak.so: dleak.c
	gcc -g -pthread -shared -fPIC -o dleak.so dleak.c

clean:
	rm -f *~ core
