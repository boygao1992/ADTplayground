ROOT_DIR := $(shell pwd)
FIND_SRC_FILE_ARGS := \( -name '*.purs' -o -name '*.js' \) -type f
NODE_MODULES_DIR := node_modules
OUTPUT ?= output
SRC_DIR ?= src

BUILD ?= $(ROOT_DIR)/.build
NODE_MODULES_STAMP := $(NODE_MODULES_DIR)/.stamp
SRC_SRCS := $(shell find $(SRC_DIR) $(FIND_SRC_FILE_ARGS))

DIST := $(BUILD)/dist
PURS_DEPS ?= $(BUILD)/.deps

BUILD_DEPENDENCIES := $(DIST)/index.html $(DIST)/index.js

PSA_ARGS ?= --censor-lib --stash=$(BUILD)/.psa_stash --strict --is-lib=.spago
RTS_ARGS ?='+RTS -N2 -A800m -RTS'

.DEFAULT_GOAL := build

$(BUILD) $(DIST):
	mkdir -p $@

$(BUILD)/index.js: $(OUTPUT)/Index/index.js | $(DIST)
	yarn purs bundle \
	  $(RTS_ARGS) \
	  '$(OUTPUT)/*/index.js' \
	  '$(OUTPUT)/*/foreign.js' \
	  --main Index \
	  --module Index \
	  --output $@

$(DIST)/index.js: $(BUILD)/index.js
	yarn browserify $< --outfile $@

$(DIST)/index.html: $(SRC_DIR)/index.html | $(DIST)
	cp $< $@

$(NODE_MODULES_STAMP): package.json yarn.lock
	yarn install
	touch $@

$(OUTPUT)/Index/index.js: $(SRC_SRCS) $(PURS_DEPS) | $(BUILD)
	yarn psa $(PSA_ARGS) $(RTS_ARGS) $(shell cat $(PURS_DEPS)) $(SRC_PURS)

$(PURS_DEPS): packages.dhall spago.dhall $(NODE_MODULES_STAMP) | $(BUILD)
	yarn spago install
	yarn --silent spago sources | sed -e "s/\(.*\)/'\1'/" | tr '\n' ' ' > $(PURS_DEPS)

.PHONY: build
build: $(BUILD_DEPENDENCIES)
