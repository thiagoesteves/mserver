all:
	rebar3 compile
	rebar3 shell --apps mserver
