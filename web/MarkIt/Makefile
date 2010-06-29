.PHONY: all prepare release deploy

VERSION		:= 1.0
LOADER_VERSION	:= 1
BUILD_DIR	:= build
DEPLOY_DEST_DIR ?= server/t/www
USE_YUI_COMPRESSOR	?= true
# ungzipped 时 YUI 产生的 js 体积小，gzipped 时 Google Closure 产生的 js 体积小。


ifeq ($(USE_YUI_COMPRESSOR),true)
define compress_js
$(shell java -jar yuicompressor-2.4.2.jar --charset UTF-8 --type js $(2) -o $(1))
endef
else
define compress_js
$(shell java -jar google-closure-compiler/compiler.jar \
	--create_source_map $(1).map --charset UTF-8 \
	$(3) $(addprefix --js , $(2)) --js_output_file $(1))
endef
endif

define js_to_url
$(shell perl -e 'my @a=<>; chomp @a; my $$a=join("", @a); $$a =~ s/;$$//; \
	$$a =~ s/\x27/%27/g; print "javascript:void(", $$a, ")"' $(2) > $(1))
endef

all: prepare $(addprefix $(BUILD_DIR)/, index.html ckeditor css js)

prepare: build

build:
	mkdir $@

############### markit loader ##########################################
$(BUILD_DIR)/markit.loader.url.js: $(BUILD_DIR)/markit.loader.min.js
	$(call js_to_url, $@, $^)

$(BUILD_DIR)/markit.loader.min.js: $(BUILD_DIR)/markit.loader.js
	$(call compress_js, $@, $^)
	perl -i -pe 's/\bEVAL\b/eval/g' $@

$(BUILD_DIR)/markit.loader.js: markit.loader.js $(BUILD_DIR)/markit.min.js
	perl -pe 's/##LOADER_VERSION##/$(LOADER_VERSION)/g' $< > $@


################ markit index.html #####################################
$(BUILD_DIR)/index.html: $(BUILD_DIR)/markit.loader.url.js index.html
	perl replace.pl "##LOADER_URL##" $^ > $@


################ markit js #############################################
$(BUILD_DIR)/markit.min.js: $(BUILD_DIR)/markit.js
	$(call compress_js, $@, $^)

$(BUILD_DIR)/markit.js: dialog.html markit.js
	perl merge.pl $^ > $@
	perl -i -pe 's/##LOADER_VERSION##/$(LOADER_VERSION)/g' $@


################ ckeditor/js/css #######################################
$(addprefix $(BUILD_DIR)/, ckeditor css js): ckeditor css js
	mkdir -p $(BUILD_DIR)
	cp -r $^ $(BUILD_DIR)


################ release     ###########################################
release: $(BUILD_DIR)/MarkIt-$(VERSION).tar.gz

$(BUILD_DIR)/MarkIt-$(VERSION).tar.gz: all
	tar czf $@ -C $(BUILD_DIR) --transform 's#^#MarkIt-$(VERSION)/#' \
		index.html markit.min.js  ckeditor css js

################ deploy      ###########################################
deploy: release
	tar xzf $(BUILD_DIR)/MarkIt-$(VERSION).tar.gz -C $(DEPLOY_DEST_DIR) \
		--transform 's#^MarkIt-$(VERSION)/##'

################ clean       ###########################################
clean:
	perl -e 'use File::Path; File::Path::remove_tree("build")'

