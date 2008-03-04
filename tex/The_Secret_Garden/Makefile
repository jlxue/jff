SRC=tsg

default: pdflatex


# ------------------------------- dvi
#  使用 gbk2uni 解决书签乱码问题
#
dvi: clean
	latex $(SRC)
#	bibtex $(SRC)
#	latex $(SRC)
	gbk2uni $(SRC).out
	latex $(SRC)


# 用 GSView 看效果很好
ps: dvi
	dvips -P pdf -t A4 -sPAPERSIZE=a4 $(SRC)


# 用 acrobat 看效果很好，使用的 Type 1 字体，拷贝乱码
ps2pdf: ps
	ps2pdf $(SRC).ps


# 用 acrobat 6 看效果很差，truetype 字体，拷贝正常
dvipdfmx: dvi
	dvipdfmx $(SRC)


# ccmap + pdflatex 或者 CJKutf8 + pdflatex，拷贝正常
pdflatex: clean
	pdflatex $(SRC)
#	bibtex $(SRC)
#	pdflatex $(SRC)
	gbk2uni $(SRC).out
	pdflatex $(SRC)

clean:
	-rm -f *.log *.aux *.toc *.out *.out.bak

