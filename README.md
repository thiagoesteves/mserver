# Mserver application #

__Authors:__ Thiago Esteves ([`thiagocalori@gmail.com`](thiagocalori@gmail.com)).

## Note ##

The Mserver application is dependent on GPROC library and rebar3

## Introduction ##

This is just an simple example of how to handle multiple gen_servers that are using the exactly same code and are under the same supervisor.

### Running application ###

In order to execute the application just call make:

```bash
make
```

### Use case: Creating Multiples Mservers ###

The user can create as many gen_servers as needed:

```erlang
1> mserver_sup:create_mserver(0).
{ok,<0.149.0>}
2> mserver_sup:create_mserver(1).
{ok,<0.151.0>}
3> mserver_sup:create_mserver(2).
{ok,<0.153.0>}
4> mserver_sup:create_mserver(3).
{ok,<0.155.0>}
5> mserver:find_me(0).
[CALL] I'm here, State: 0
 ok
6> mserver:show_yourself(0).
[CAST] I'm here, State: 0
 ok
7> mserver_sup:remove_mserver(0).
ok
8> mserver:find_me(0).
{error,invalid_mserver}
```
### Supervisor tree ###

The supervisor tree of all gen_servers created can be easily viewed with the observer, try the sequence of commands below and have a look at the Applications->mserver.

```erlang
1> mserver_sup:create_mserver(0).
{ok,<0.149.0>}
2> mserver_sup:create_mserver(1).
{ok,<0.151.0>}
3> mserver_sup:create_mserver(2).
{ok,<0.153.0>}
4> mserver_sup:create_mserver(3).
{ok,<0.155.0>}
5> observer:start().
ok
```
