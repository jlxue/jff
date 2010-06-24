.PHONY: all prepare

BUILD_DIR	:= build
CWD_URL		:= $(shell perl -e 'use URI::file; print URI::file->cwd')
MARKIT_URL	?= $(CWD_URL)build/markit.min.js

define compress_js
$(shell java -jar google-closure-compiler/compiler.jar $(addprefix --js , $(2)) --js_output_file $(1))
endef

define js_to_url
$(shell perl -e 'my @a=<>; chomp @a; my $$a=join("", @a); $$a =~ s/;$$//; $$a =~ s/"/%22/g; print "javascript:void(", $$a, ")"' $(2) > $(1))
endef

all: prepare $(addprefix $(BUILD_DIR)/, markit.loader.url.js index.html)

prepare: build

build:
	mkdir $@

############### markit loader ##########################################
$(BUILD_DIR)/markit.loader.url.js: $(BUILD_DIR)/markit.loader.min.js
	$(call js_to_url, $@, $^)

$(BUILD_DIR)/markit.loader.min.js: $(BUILD_DIR)/markit.loader.js
	$(call compress_js, $@, $^)

$(BUILD_DIR)/markit.loader.js: markit.loader.js
	perl -pe 'my $$url="$(MARKIT_URL)"; s/##MARKIT_URL##/$$url/g;' $^ > $@

################ markit index.html #####################################
$(BUILD_DIR)/index.html: $(BUILD_DIR)/markit.loader.url.js index.html
	perl replace.pl "##LOADER_URL##" $^ > $@

$(BUILD_DIR)/markit.min.js: $(BUILD_DIR)/markit.merged.js
	java -jar google-closure-compiler/compiler.jar $(addprefix --js , $^) --js_output_file $@

markit.merged.js: markit.html markit.js
	perl merge.pl $^ > $@

clean:
	rm -rf build

