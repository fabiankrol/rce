
default: compile

all: deps compile

clean:
	@./rebar clean

compile:
	@./rebar compile

deps:
	@./rebar get-deps

run:
	@erl \
		-args_file etc/vm.args \
		-config etc/sys \
		-pa ebin -pa deps/*/ebin \
		-eval "rce_app:start_dev()."

## shortcuts

c:
	@./rebar compile skip_deps=true
