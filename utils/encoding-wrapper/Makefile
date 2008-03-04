# UTF-8 encoding
#

ifeq ($(DEBUG), true)
CFLAGS=-Wall -g -DDEBUG
else
CFLAGS=-Wall -g
endif

SONAME=libew.so
UID=`id -u`
GID=`id -g`

so: encoding-wrapper.c
	gcc $(CFLAGS) -fPIC -c encoding-wrapper.c
	gcc $(CFLAGS) -shared -o $(SONAME) encoding-wrapper.o -ldl

t: t.c
	gcc $(CFLAGS) -DUID=$(UID) -DGID=$(GID) -o t t.c

test: so t
	@rm -rf \=\=*
	@LD_LIBRARY_PATH=. LD_PRELOAD=libew.so ./t

clean:
	@rm -rf *.o *.so t \=\=*

