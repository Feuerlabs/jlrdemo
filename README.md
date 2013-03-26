exodemo
===========

Demo application using exoport

(The following text is a copy from exoskeleton/README.md)
This is an example of how to build an application on top of `exoport`
and get up and running quickly.

Step 0: Prerequisites
---------------------

* Linux or similar OS
* git installed
* Erlang/OTP R15B02 or later installed

Step 1: Install rebar
---------------------

You can get rebar from https://github.com/rebar/rebar/

Step 2: Use rebar to create a skeleton Erlang application
---------------------------------------------------------

```
$ rebar create-app appid=exoskeleton
==> exoskeleton (create-app)
Writing src/exoskeleton.app.src
Writing src/exoskeleton_app.erl
Writing src/exoskeleton_sup.erl
```

Go to the `rebar` directory and run `make`.
Copy the `rebar` executable to a directory in your path.

Step 3: Edit the rebar.config file
----------------------------------

You need to add a dependency to `exoport` in [`exoskeleton/rebar.config`](rebar.config):

``` erlang
%% -*- erlang -*-

{deps,
 [
  {exoport, ".*", {git, "git@github.com:Feuerlabs/exoport.git", "HEAD"}}
 ]}.
```

Step 4: Fetch dependencies
--------------------------

Exoport has a number of dependencies of its own. Rebar will ensure 
that they all get fetched.

```
$ rebar get-deps
uwair:exoskeleton uwiger$ rebar get-deps
==> exoskeleton (get-deps)
Pulling exoport from {git,"git@github.com:Feuerlabs/exoport.git","HEAD"}
Cloning into exoport...
==> exoport (get-deps)
Pulling bert from {git,"git@github.com:Feuerlabs/bert.git","HEAD"}
Cloning into bert...
Pulling exo from {git,"git@github.com:Feuerlabs/exo.git","HEAD"}
Cloning into exo...
...
```

Step 5: Try compiling
---------------------

```
$ rebar compile
==> edown (compile)
Compiled src/edown_make.erl
Compiled src/edown_lib.erl
Compiled src/edown_xmerl.erl
Compiled src/edown_doclet.erl
Compiled src/edown_layout.erl
==> setup (compile)
... (a fair amount of output)
==> exoskeleton (compile)
Compiled src/exoskeleton_app.erl
Compiled src/exoskeleton_sup.erl
```

At this point, you should have runnable code. We now need to build a system
which is able to start `exoport` and all required applications.

Step 6: Create a setup script
-----------------------------

The following config file (see [`priv/setup.config`](priv/setup.config))
should be enough to build a system with basic settings:

```erlang
%% -*- erlang -*-
[
 %% Add our own app(s)
 {add_apps, [exoskeleton]},
 %% Tell exoport where to find our config file
 {env,
  [{exoport,
    [
     {config, filename:join(CWD, "exoport.config")}
    ]}
  ]},
 %% Put include after settings that you think should override any defaults
 {include_lib, "exoport/priv/setup.config"}
].
```

We also need to create [`priv/exoport.config`](priv/exoport.config) with connection
settings for `exoport` (note that these are dummy settings):

```erlang
%% -*- erlang -*-
{exodm_host, "localhost"}.
{'device-id', "*wiger*x00000001"}.
{'ckey', 1}.
{'skey', 2}.
```

Step 7: Build a runnable system
-------------------------------

Now we need to add a non-trivial step (the first?).
The `setup` application needs to know where to find the applications.
During development, the environment variable ERL_LIBS is a good way
to accomplish this, but now, the applications are in the `deps/`
subdirectory. We set a temporary ERL_LIBS variable, and also add our
own application location as a path option. Let's put it into the 
[`Makefile`](Makefile) once and for all:

```make
NAME=exoskeleton
...
setup:
  ERL_LIBS+=":`pwd`/deps" \
	deps/setup/setup_gen $(NAME) priv/setup.config setup -pz `pwd`/ebin
```

Step 8: Run the system
----------------------

Step 7, resulted in a set of boot scripts under the `setup/` directory.
Let's now create a make target to start our test system.

```make
run: setup
	erl -boot setup/start -config setup/sys
```

Given that we have access to a running Exosense server, and correct
connection settings in our [`priv/exoport.config`](priv/exoport.config),
we can now perform a first connection test:

```
$ make run
ERL_LIBS+=":`pwd`/deps" \
	deps/setup/setup_gen exoskeleton priv/setup.config setup -pz `pwd`/ebin
erl -boot setup/start -config setup/sys
Erlang R15B02 (erts-5.9.2) [source] [64-bit] [smp:4:4] [async-threads:0] [kernel-poll:false]


=PROGRESS REPORT==== 9-Jan-2013::19:27:56 ===
...
19:27:57.232 [info] Application exoport started on node nonode@nohost
19:27:57.239 [info] Application exoskeleton started on node nonode@nohost
Eshell V5.9.2  (abort with ^G)
1> exoport:ping().
handle_request(Socket, {reply,pong}, State)
{reply,pong,[]}
```
