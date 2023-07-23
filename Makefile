
.PHONY: all
all: build test

.PHONY: build
build:
	@dune build

.PHONY: test
test:
	@dune runtest --no-buffer --force

.PHONY: remove
remove:
	find . \( -name *~ -o -name \#* \) -print -exec rm \{\} \;

.PHONY: clean
clean: remove
	@dune clean

.PHONY: entity
entity:
	curl https://html.spec.whatwg.org/entities.json -o src/chaml/entities.json

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
