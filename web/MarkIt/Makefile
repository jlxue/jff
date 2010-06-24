.PHONY: all

CWD_URL		:= $(shell perl -e 'use URI::file; print URI::file->cwd')
MARKIT_URL	?= $(CWD_URL)/markit.min.js

all: markit.url.html

markit.url.html: markit.url.js
	perl -pe 'BEGIN {print q(<a href=")}; s/"/%22/g; END {print q(">Mark It!</a>)}' $^ > $@

markit.url.js: markit.min.js
	perl -e 'my @a=<>; chomp @a; my $$a=join("", @a); $$a =~ s/;$$//; print "javascript:void(", $$a, ")"' $^ > $@

markit.min.js: markit.merged.js
	java -jar google-closure-compiler/compiler.jar $(addprefix --js , $^) --js_output_file $@

markit.merged.js: markit.html markit.js
	perl merge.pl $^ > $@

clean:
	rm -rf markit.min.js markit.merged.js markit.url.js markit.url.html

