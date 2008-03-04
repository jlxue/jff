dumpelf: dumpelf.c section.c nameof.c
	gcc -g -o $@ $^

nameof.c: dumpelf.c
	grep -E "^char\* nameof" $^  | perl gen_nameof.pl > $@

