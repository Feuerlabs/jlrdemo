.PHONY:	all deps compile setup clean doc


NAME=jlrdemo
export KVDB_BACKENDS=ets

all: deps compile

deps:
	rebar get-deps

compile:
	rebar compile

recomp:
	rebar compile skip_deps=true

setup:
	ERL_LIBS+=":`pwd`/deps" \
	deps/setup/setup_gen $(NAME) priv/setup.config setup -pz `pwd`/ebin

target:
	ERL_LIBS+=":`pwd`/deps" \
	deps/setup/setup_gen $(NAME) priv/setup.config setup -pz `pwd`/ebin \
	-target rel -vsn 0.1

run: setup
	erl -boot setup/start -config setup/sys

doc:
        REBAR_DOC=1 rebar skip_deps=true get-deps doc

clean:
	rebar clean
