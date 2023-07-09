
.PHONY: all
all: build test

.PHONY: build
build:
	@dune build

.PHONY: test
test:
	@dune runtest --no-buffer --force

.PHONY: clean
clean:
	@dune clean

.PHONY: doc
doc:
	@dune build @doc

.PHONY: install_doc
install_doc: doc
	rsync -r _build/default/_doc/_html/ ~/WWW2/main/simple_httpd/
	rsync tests/timings/*.txt ~/WWW2/main/simple_httpd/bench/
	rsync tests/timings/*.html ~/WWW2/main/simple_httpd/bench/
	rsync tests/timings/*.svg ~/WWW2/main/simple_httpd/bench/

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
