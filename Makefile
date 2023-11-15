DOCKER_COMPOSE ?= docker-compose

export OCAML_REDIS_TEST_SOCKET=$(CURDIR)/socket/redis.sock

all: build test

build:
	@dune build @all

test:
	@$(DOCKER_COMPOSE) up -d
	@(dune runtest --force --no-buffer; EXIT_CODE="$$?"; $(DOCKER_COMPOSE) down; exit $$EXIT_CODE)

clean:
	@dune clean

watch:
	@dune build @all -w

reindent:
	@for dir in src examples tests/ ; do \
	  find $(dir) -name '*.ml*' -exec ocp-indent -i {} \; ; \
	done

.PHONY: all build test clean watch
