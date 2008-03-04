all: html pdf

html: Evolution_of_a_POE_Server.html

pdf: Evolution_of_a_POE_Server.pdf


Evolution_of_a_POE_Server.html: Evolution_of_a_POE_Server.txt
	asciidoc -a toc -a numbered -a toclevels=4 $^


Evolution_of_a_POE_Server.xml: Evolution_of_a_POE_Server.txt
	asciidoc -b docbook -a toc -a numbered -a toclevels=4 $^

Evolution_of_a_POE_Server.tex: Evolution_of_a_POE_Server.xml
	dblatex -P latex.encoding=utf8 -b pdftex -t tex $^
	# POE::Kernel-{}>run()  =>  POE::Kernel->run()
	sed -i -e 's/-{}>/\$$\\rightarrow\$$/g' $@

Evolution_of_a_POE_Server.pdf: Evolution_of_a_POE_Server.tex
	[ -e tex ] || mkdir tex
	cp -r /usr/share/dblatex/latex/style/* tex
	cp -r /usr/share/dblatex/latex/misc/* tex
	sed -e '1,20s/documentclass{article}/documentclass{ctexartutf8}/' \
	    -e '1,20s/\(.*usepackage\[T2A,T2D.*\)/%\1/' 		\
	    -e '1,20s/\(.*usepackage{ucs}\)/%\1/' 				\
	    -e '1,20s/\(.*usepackage\[utf8x\]{inputenc}\)/%\1/' \
	    -e '1,20s/lstset{inputencoding=utf8x, /lstset{/' 	\
	    $^ > tex/$^
	-cd tex && (pdflatex $^; pdflatex $^)
	cp tex/$@ .

clean:
	/bin/rm -rf tex *.xml *.html *.tex *.pdf

