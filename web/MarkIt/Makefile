.PHONY: all prepare

BUILD_DIR	:= build


define compress_js
$(shell java -jar google-closure-compiler/compiler.jar \
	$(addprefix --js , $(2)) --js_output_file $(1))
endef

define js_to_url
$(shell perl -e 'my @a=<>; chomp @a; my $$a=join("", @a); $$a =~ s/;$$//; \
	$$a =~ s/"/%22/g; print "javascript:void(", $$a, ")"' $(2) > $(1))
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

$(BUILD_DIR)/markit.loader.js: markit.loader.js $(BUILD_DIR)/markit.min.js
	perl -pe '' $< > $@


################ markit index.html #####################################
$(BUILD_DIR)/index.html: $(BUILD_DIR)/markit.loader.url.js index.html
	perl replace.pl "##LOADER_URL##" $^ > $@


################ markit js #############################################
$(BUILD_DIR)/markit.min.js: $(BUILD_DIR)/markit.js
	java -jar google-closure-compiler/compiler.jar $(addprefix --js , $^) --js_output_file $@

$(BUILD_DIR)/markit.js: markit.html markit.js
	perl merge.pl $^ > $@


################ ckeditor/js/css #######################################
$(addprefix $(BUILD_DIR)/, ckeditor css js): ckeditor css js
	mkdir -p $(BUILD_DIR)
	cp -r $^ $(BUILD_DIR)


################ clean       ###########################################
clean:
	perl -e 'use File::Path; File::Path::remove_tree("build")'

