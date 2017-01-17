##==============================================================================
##
##               |  o __   _|  _  __  |_   _       _ _   (TM)
##               |_ | | | (_| (/_ | | |_) (_| |_| | | |
##
## Copyright (c) 2017 Lindenbaum GmbH
##
## Permission to use, copy, modify, and/or distribute this software for any
## purpose with or without fee is hereby granted, provided that the above
## copyright notice and this permission notice appear in all copies.
##
## THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
## WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
## MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
## ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
## WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
## ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
## OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
##==============================================================================

.DEFAULT_GOAL := all

REBAR3 := $(shell which rebar3 2> /dev/null)
REBAR3 ?= ./rebar3-3.3.4

VERSION := $(shell cat rebar.config | grep http2smtp | head -1 | cut -d\" -f 2)

REL_DIR := _build/default/rel/http2smtp

.PHONY: all
all:
	$(REBAR3) do dialyzer,eunit,release
	$(RM) -r $(REL_DIR)/bin/
	$(RM) $(REL_DIR)/releases/$(VERSION)/vm.args

.PHONY: test
test:
	$(REBAR3) do dialyzer,eunit

.PHONY: compile
compile:
	$(REBAR3) compile

.PHONY: rpm
rpm: all _build/http2smtp.service
	fpm -s dir -t rpm \
            --name http2smtp \
            --version $(VERSION) --iteration 1 \
            --license "MIT License" \
            --vendor "Lindenbaum GmbH" \
            --config-files /etc/http2smtp.config \
            --maintainer voiceteam@lindenbaum.eu \
            --description "A HTTP-based mail relay service" \
            --url http://lindenbaum.eu \
            config/http2smtp.config=/etc/ \
            config/http2smtp.sh=/etc/profile.d/ \
            _build/http2smtp.service=/usr/lib/systemd/system/ \
            $(REL_DIR)=/usr/lib64

.PHONY: clean
clean:
	$(RM) http2smtp-*.rpm _build/http2smtp.service
	$(REBAR3) clean

.PHONY: distclean
distclean: clean
	$(RM) -r _build/

_build/http2smtp.service: config/http2smtp.service.template
	cat $< \
          | sed "s|%VSN%|$(VERSION)|g" \
          | sed "s|%ERTS%|$(shell ls $(REL_DIR) | grep erts | cut -d- -f2)|g" \
          > $@
