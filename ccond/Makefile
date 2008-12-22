CPPFLAGS = -DCCOND_DEBUG -DHAVE_EXEC_INFO_H
CFLAGS = -g -Wall
CC=g++

test_ccond: test_ccond.c ccond.c ccond.h

test: test_ccond
	./test_ccond

clean:
	rm -f test_ccond

