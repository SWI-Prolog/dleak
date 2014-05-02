all: dleak.so

SRC=dleak.c md5.c callcontext.c

dleak.so: $(SRC)
	gcc -g -pthread -shared -fPIC -o dleak.so $(SRC)

clean:
	rm -f *~ core
