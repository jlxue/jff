CFLAGS = -Wall -g

LIB_BBSDB_HEADERS = board.h util.h
LIB_BBSDB_SOURCES = board.c util.c

BOARD_TEST = t
BOARD_TEST_SOURCES = t.c


all: $(BOARD_TEST)

test: $(BOARD_TEST)
	./dotest

$(BOARD_TEST): $(LIB_BBSDB_HEADERS) $(LIB_BBSDB_SOURCES)	\
	$(BOARD_TEST_SOURCES)

clean:
	rm -f *.o *~ $(BOARD_TEST)


