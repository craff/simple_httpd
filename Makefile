
TAGTABLES=./src/chaml/table-1.csv ./src/chaml/table-2.csv ./src/chaml/table-3.csv\
          ./src/chaml/table-4.csv ./src/chaml/table-5.csv ./src/chaml/table-6.csv

.PHONY: all
all: build test

.PHONY: build
build: ./src/field-names.csv ./src/chaml/entities.json ${TAGTABLES}
	@dune build

.PHONY: test
test:
	@dune runtest --no-buffer --force

.PHONY: remove
remove:
	find . \( -name '*~' -o -name '\#*' \) -print -exec rm \{\} \;

.PHONY: clean
clean: remove
	@dune clean

./src/field-names.csv:
	curl https://www.iana.org/assignments/http-fields/field-names.csv -o src/field-names.csv

./src/chaml/entities.json:
	curl https://html.spec.whatwg.org/entities.json -o ./src/chaml/entities.json

${TAGTABLES}:
	cd src/chaml; python3 ../gen/tags.py "https://dev.w3.org/html5/spec-LC/index.html"

.PHONY: doc
doc:
	@dune build @doc

.PHONY: install_doc
install_doc: doc
	rsync -r _build/default/_doc/_html/ ~/WWW2/Raffalli/simple_httpd/
	rsync tests/timings/*.txt ~/WWW2/Raffalli/simple_httpd/bench/
	rsync tests/timings/*.html ~/WWW2/Raffalli/simple_httpd/bench/
	rsync tests/timings/*.svg ~/WWW2/Raffalli/simple_httpd/bench/

.PHONY: install
install: build
	@dune install

.PHONY: watch
watch:
	@dune build @all -w

VERSION=$(shell awk '/^version:/ {print $$2}' simple_httpd.opam)

.PHONY: update_next_tag
update_next_tag:
	@echo "update version to $(VERSION)..."
	sed --follow-symlinks -i "s/NEXT_VERSION/$(VERSION)/g" $(wildcard src/**.ml) $(wildcard src/**.mli) \
			$(wildcard src/**/*.ml) $(wildcard src/**/*.mli)
	sed --follow-symlinks -i "s/NEXT_RELEASE/$(VERSION)/g" $(wildcard src/**.ml) $(wildcard src/**.mli) \
		$(wildcard src/**/*.ml) $(wildcard src/**/*.mli)
