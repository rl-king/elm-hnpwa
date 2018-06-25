SHELL := /bin/bash

default: all

.PHONY: test cover

SRC_PATH := ./src
DIST_PATH := ./src
NPM_PATH := ./node_modules/.bin

export PATH := $(NPM_PATH):$(PATH)

all:
	make elm
	make scss

build:
	@sw-precache --config=sw-precache-config.js --root=${DIST_PATH}
	@cp -r ${SRC_PATH}/assets/* ${DIST_PATH}
	@elm-make --optimze --yes ${SRC_PATH}/Main.elm --output ${DIST_PATH}/main.js
	# make scss
	# make minify

clean:
	@rm -Rf ${DIST_PATH}/*

deps:
	@npm install
	@elm-package install --yes

distclean: clean
	@rm -Rf elm-stuff
	@rm -Rf node_modules

elm:
	@elm-make --warn --yes ${SRC_PATH}/Main.elm --output ${DIST_PATH}/main.js

help:
	@echo "Run: make <target> where <target> is one of the following:"
	@echo "  all                    Compile all Elm files"
	@echo "  clean                  Remove 'dist' folder"
	@echo "  deps                   Install build dependencies"
	@echo "  distclean              Remove build dependencies"
	@echo "  help                   Magic"
	@echo "  watch                  Run 'make all' on Elm file change"

livereload:
	@livereload ${DIST_PATH} -e 'js, css'

minify:
	@uglifyjs ${SRC_PATH}/main.js -c --output='${DIST_PATH}/main.js'
	@cleancss -o ${SRC_PATH}/style.css ${DIST_PATH}/style.css

scss:
	@node-sass ${SRC_PATH}/style.scss ${DIST_PATH}/style.css

serve:
	serve --single --listen 5001 ./

watch-elm:
	make livereload & find ${SRC_DIR} -name '*.elm' | entr make elm

watch-scss:
	make livereload & find ${SRC_DIR} -name '*.scss' | entr make scss

watch:
	make livereload & \
	find ${SRC_DIR}/src -name '*.scss' | entr make scss & \
	find ${SRC_DIR}/src -name '*.elm' | entr make elm
