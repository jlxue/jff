CFLAGS = -Wall -g

LIB_BBSDB_HEADERS = board.h util.h
LIB_BBSDB_SOURCES = board.c util.c

BBS_CLIENT = bbs
BBS_CLIENT_SOURCES = bbs.c

BOARD_DAEMON = boardd
BOARD_DAEMON_SOURCES = boardd.c

BOARD_TEST = t
BOARD_TEST_SOURCES = t.c


all: $(BBS_CLIENT) $(BOARD_DAEMON) $(BOARD_TEST)

$(BBS_CLIENT): $(LIB_BBSDB_HEADERS) $(LIB_BBSDB_SOURCES) \
	$(BBS_CLIENT_SOURCES)

$(BOARD_DAEMON): $(LIB_BBSDB_HEADERS) $(LIB_BBSDB_SOURCES)	\
	$(BOARD_DAEMON_SOURCES)

test: $(BOARD_TEST)
	rm -rf ./for_test/
	./dotest

$(BOARD_TEST): $(LIB_BBSDB_HEADERS) $(LIB_BBSDB_SOURCES)	\
	$(BOARD_TEST_SOURCES)

clean:
	rm -rf *.o *~ for_test $(BBS_CLIENT) $(BOARD_DAEMON) $(BOARD_TEST)


