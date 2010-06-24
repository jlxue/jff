.PHONY: all prepare

BUILD_DIR	:= build
CWD_URL		:= $(shell perl -e 'use URI::file; print URI::file->cwd')

ifeq ($(DEBUG),true)
MARKIT_URL	?= $(CWD_URL)build/markit.merged.js
JQUERY_URL	?= $(CWD_URL)js/jquery-1.4.2.js
JQUERY_UI_URL	?= $(CWD_URL)js/jquery-ui-1.8.2.custom.js
else
MARKIT_URL	?= $(CWD_URL)build/markit.min.js
INFER_MARKIT_URL	?= true

JQUERY_URL	?= $(CWD_URL)js/jquery-1.4.2.min.js
JQUERY_UI_URL	?= $(CWD_URL)js/jquery-ui-1.8.2.custom.min.js
endif


define compress_js
$(shell java -jar google-closure-compiler/compiler.jar \
	$(addprefix --js , $(2)) --js_output_file $(1))
endef

define js_to_url
$(shell perl -e 'my @a=<>; chomp @a; my $$a=join("", @a); $$a =~ s/;$$//; \
	$$a =~ s/"/%22/g; print "javascript:void(", $$a, ")"' $(2) > $(1))
endef

all: prepare $(BUILD_DIR)/index.html

prepare: build

build:
	mkdir $@

############### markit loader ##########################################
$(BUILD_DIR)/markit.loader.url.js: $(BUILD_DIR)/markit.loader.min.js
	$(call js_to_url, $@, $^)

$(BUILD_DIR)/markit.loader.min.js: $(BUILD_DIR)/markit.loader.js
	$(call compress_js, $@, $^)

$(BUILD_DIR)/markit.loader.js: markit.loader.js $(BUILD_DIR)/markit.min.js
ifeq ($(INFER_MARKIT_URL),true)
	perl -pe '' $< > $@
else
	perl -pe 'my $$url="$(MARKIT_URL)"; s/##MARKIT_URL##/$$url/g;' $< > $@
endif


################ markit index.html #####################################
$(BUILD_DIR)/index.html: $(BUILD_DIR)/markit.loader.url.js index.html
	perl replace.pl "##LOADER_URL##" $^ > $@

################ markit js #############################################
$(BUILD_DIR)/markit.min.js: $(BUILD_DIR)/markit.merged.js
	java -jar google-closure-compiler/compiler.jar $(addprefix --js , $^) --js_output_file $@

$(BUILD_DIR)/markit.merged.js: markit.html markit.js
	perl merge.pl $^ > $@
	perl -i -pe 'my $$url="$(JQUERY_URL)"; s/##JQUERY_URL##/$$url/g; $$url="$(JQUERY_UI_URL)"; s/##JQUERY_UI_URL##/$$url/g' $@

clean:
	perl -e 'use File::Path; File::Path::remove_tree("build", { verbose => 1 })'

