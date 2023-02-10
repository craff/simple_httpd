
.PHONY: all
all: build test

.PHONY: build
build:
	@dune build @install

.PHONY: test
test:
	@dune runtest --no-buffer --force

.PHONY: clean
clean:
	@dune clean

.PHONY: doc
doc:
	@dune build @doc

.PHONY: send_doc
send_doc: doc
	rsync -r _build/default/_doc/_html/ raffalli:WWW/simple_httpd/
	rsync -r tests/small.png tests/big.png raffalli:WWW/simple_httpd/

.PHONY: install
install:
	@dune build @install
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
