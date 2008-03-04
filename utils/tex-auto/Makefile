# UTF-8 encoding
#

V := latest

ifeq ($D, y)
CFLAGS=-Wall -g -DDEBUG
else
CFLAGS=-Wall -g
endif

libtex-auto.so: tex-auto.c
	gcc $(CFLAGS) -fPIC -c $^
	gcc $(CFLAGS) -shared -o $@ $(^:.c=.o) -ldl

doc: README-zh.txt
	asciidoc -a toc -a numbered $^

dist:
	git archive --format=tar --prefix=tex-auto/ HEAD > tex-auto-$V.tar
	gzip tex-auto-$V.tar

clean:
	rm -rf *.o *.so *.gz

