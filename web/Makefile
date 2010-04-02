.PHONY: tidy csstidy cssvalidator

TOPDIR := $(dir $(word $(words $(MAKEFILE_LIST)),$(MAKEFILE_LIST)))

HTML_SOURCES := $(shell find $(TOPDIR) -name "*.htm" -o -name "*.html")
CSS_SOURCES  := $(shell find $(TOPDIR) -name "*.css")
JS_SOURCES   := $(shell find $(TOPDIR) -name "*.js")

default: tidy cssvalidator csstidy

tidy: $(HTML_SOURCES)
	tidy -m -i -c -w 120 -q -asxhtml -utf8 -language zh $?

csstidy: $(CSS_SOURCES)

cssvalidator: $(CSS_SOURCES)

