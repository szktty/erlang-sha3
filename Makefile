REBAR?=rebar3

.PHONY: \
	all \
	clean \
	nuke \
	test \
	update \
	dialyzer

all:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

nuke: clean
	@rm -rf _build

test:
	@$(REBAR) eunit

update:
	@$(REBAR) update

dialyzer:
	@$(REBAR) dialyzer

coveralls:
	@${REBAR} coveralls send
