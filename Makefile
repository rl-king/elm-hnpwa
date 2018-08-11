SHELL := /bin/bash

.PHONY: elm watch minify

default: all

NPM_PATH := ./node_modules/.bin
SRC_DIR := ./src
DIST_DIR := ./dist

export PATH := $(NPM_PATH):$(PATH)

all: elm scss

assets:
		@cp -r ${SRC_DIR}/assets/* ${DIST_DIR}
		@cp -r ${SRC_DIR}/index.html ${DIST_DIR}

build: clean assets elmoptimized scss minify serviceworker

clean:
		@rm -Rf ${DIST_DIR}/*

deps:
		@npm install
		@elm-package install --yes

distclean: clean
		@rm -Rf elm-stuff
		@rm -Rf node_modules

elm:
		@./elm make ${SRC_DIR}/Main.elm --output ${DIST_DIR}/main.js

elmoptimized:
		@./elm make --optimize ${SRC_DIR}/Main.elm --output ${DIST_DIR}/main.js

help:
		@echo "Run: make <target> where <target> is one of the following:"
		@echo "  all                    Compile all Elm files"
		@echo "  clean                  Remove 'dist' folder"
		@echo "  deps                   Install build dependencies"
		@echo "  distclean              Remove build dependencies"
		@echo "  help                   Magic"
		@echo "  watch                  Run 'make all' on Elm file change"

livereload:
		@livereload ${DIST_DIR} -e 'js, css'

minify:
		@npx uglify-js ${DIST_DIR}/main.js\
		--output=${DIST_DIR}/main.js\
		--compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe'

scss:
		@node-sass ${SRC_DIR}/style.scss ${DIST_DIR}/style.css

serve:
		serve --single --listen 5001 ./dist

serviceworker:
		@sw-precache --config=sw-precache-config.js --root=${DIST_DIR}

watch-elm:
		make livereload & find ${SRC_DIR} -name '*.elm' | entr make elm

watch-scss:
		make livereload & find ${SRC_DIR} -name '*.scss' | entr make scss

watch:
		make livereload & \
		find ${SRC_DIR} -name '*.scss' | entr make scss & \
		find ${SRC_DIR} -name '*.elm' | entr make elm
