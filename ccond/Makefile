CPPFLAGS = -DCCOND_DEBUG -DHAVE_EXEC_INFO_H -DCCOND_ENABLE_THREAD_SAFE=1
CFLAGS = -g -Wall
CC=g++

test_ccond: test_ccond.c ccond.c ccond.h

test: test_ccond
	./test_ccond

bench: bench_ccond.c ccond.c ccond.h
	g++ bench_ccond.c ccond.c -o bench_ccond
	./bench_ccond 10000 10000
	@echo --------------------------------------------------
	gcc bench_ccond.c ccond.c -o bench_ccond
	./bench_ccond 10000 10000
	@echo --------------------------------------------------
	gcc -DCCOND_ENABLE_THREAD_SAFE=1 bench_ccond.c ccond.c -o bench_ccond
	./bench_ccond 10000 10000
	@echo --------------------------------------------------
	g++ -DCCOND_ENABLE_THREAD_SAFE=1 bench_ccond.c ccond.c -o bench_ccond
	./bench_ccond 10000 10000

clean:
	rm -f test_ccond bench_ccond

