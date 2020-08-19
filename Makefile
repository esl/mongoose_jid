.PHONY: rel deps test

all: deps compile

compile:
	rebar3 compile

deps:
	rebar3 get-deps

clean:
	rebar3 clean

test-deps:
	rebar3 get-deps

test-compile: test-deps
	rebar3 compile

test: test-compile
	rebar3 eunit

coverage-report: _build/test/cover/eunit.coverdata
	rebar3 as test coveralls send

codecov: _build/test/cover/eunit.coverdata
	rebar3 as test codecov analyze

dialyzer:
	rebar3 dialyzer
