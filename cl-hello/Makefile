default: clisp

clisp:
	clisp -x "(asdf:operate 'asdf:load-op 'cl-hello)"
	if which cl-launch; then \
		cl-launch -d cl-hello.core -s cl-hello -l clisp \
			-o cl-hello --init "(hello:main cl-launch:*arguments*)" ; \
	else \
		clisp -K full -i clisp-dump.lisp ; \
	fi

sbcl:
	sbcl --noinform --noprint --disable-debugger --eval \
		"(asdf:operate 'asdf:load-op 'cl-hello)" --eval \
	   	"(quit)"
	if which cl-launch; then \
		cl-launch -d cl-hello.core -s cl-hello -l sbcl \
			-o cl-hello --init "(hello:main cl-launch:*arguments*)" ; \
	else \
		sbcl --load sbcl-dump.lisp ; \
	fi

clean:
	rm -f *.fasl *.fas *.lib cl-hello cl-hello.core

