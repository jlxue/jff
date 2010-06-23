.PHONY: all

all: bookmark.url.html

bookmark.url.html: bookmark.url.js
	perl -pe 'BEGIN {print q(<a href=")}; s/"/%22/g; END {print q(">Mark It!</a>)}' $^ > $@

bookmark.url.js: bookmark.min.js
	perl -e 'my @a=<>; chomp @a; my $$a=join("", @a); $$a =~ s/;$$//; print "javascript:void(", $$a, ")"' $^ > $@

bookmark.min.js: bookmark.merged.js
	java -jar google-closure-compiler/compiler.jar $(addprefix --js , $^) --js_output_file $@

bookmark.merged.js: bookmark.html bookmark.js
	perl merge.pl $^ > $@

clean:
	rm -rf bookmark.min.js bookmark.merged.js bookmark.url.js bookmark.url.html

