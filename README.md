# dasherl

**Dasherl** - A deployment environmentt and builder for Dash apps running from Erlang/Elixir

[![Hex.pm](https://img.shields.io/hexpm/v/dasherl.svg)](https://hex.pm/packages/dasherl)
[![Build Status](https://travis-ci.org/zgbjgg/dasherl.svg?branch=master)](https://travis-ci.org/zgbjgg/dasherl)
[![Codecov](https://img.shields.io/codecov/c/github/zgbjgg/dasherl.svg)](https://codecov.io/gh/zgbjgg/dasherl)
[![License: MIT](https://img.shields.io/github/license/zgbjgg/dasherl.svg)](https://raw.githubusercontent.com/zgbjgg/dasherl/master/LICENSE)

Dasherl is a deployment environment (using gunicorn for now) for dash apps using Erlang or Elixir, but also it's a builder for the Dash apps so you can write erlang code (just through nested records) and setup the route to use the erlang code then Dasherl can handle the requests to erlang code as serve as a python code (using dash).

This project is under development and should not be used in production, it's not ready for that.

### Creating an isolated environment

To create an environment with dash running on gunicorn just start a single process. In order to start you need define the settings used by gunicorn, for now there are two options: `bind` that defines the ip and port for wsgi server and `workers`:

```erlang
(dasherl@hurakann)1> Settings = [{bind, "127.0.0.1:8000"}, {workers, 3}]
[{bind,"127.0.0.1:8000"},
 {workers,3}]
(dasherl@hurakann)2> {ok, Pid} = dasherl_gunicorn_worker:start_link(Settings).
12:30:02.121 [info] initialized default modules for py pid <0.353.0>
{ok,<0.352.0>}
```

Now there are two tools in order to manage the gunicorn server: run or stop. To run server:

```erlang
(dasherl@hurakann)3> dasherl_gunicorn_worker:run_server(Pid).
ok
```

That's all, if all goes ok then point your browser to http://127.0.0.1:8000 and you will see the error 404 page, since there are not routes defined. To stop server:

```erlang
(dasherl@hurakann)5> dasherl_gunicorn_worker:stop_server(Pid).
ok
```

### Write it like erlang use it like Dash

The routes are defined as an application environment for dasherl, for now the only way to build a layout to render in a specific route is building from erlang code.

The best approach is writing all from erlang and render in python so for that reason there is a interface to write erlang code and use it like python in dash server.

The first piece is adding a new route rendering erlang code, so let's write the erlang code that will render a layout. In the dasherl_components there are many functions as in py to create components (core & html) as dash does in python, for example, lets create a layout with a div and inside a div a single input text.

First create the input text:

```erlang
(dasherl@hurakann)3> Input = dasherl_components:input([{placeholder, 'Write here'}, {value, ''}, {type, 'text'}]).
{dasherl_core_component,'Input',
                        [{placeholder,'Write here'},{value,''},{type,text}]}
```

All core and html components receive a single keywords proplists that represents the arguments of a python dash call.

Now create the div as container of input:

```erlang
(dasherl@hurakann)2> Div = dasherl_components:divc([{children, [Input]}]).
{dasherl_html_component,'Div',
                        [{children,[{dasherl_core_component,'Input',
                                                            [{placeholder,'Write here'},{value,''},{type,text}]}]}]}
```

In the above example the div is an html dash component containing an input text core dash component, so now, how render this in dash server?, the answer is simple: a route.

Let's create a new route and set the div as main layout:

```erlang
(dasherl@hurakann)3> dasherl_gunicorn_worker:setup_route(Pid, <<"/my-dasherl-route">>, Div).
ok
```

Notice that third parameter is the recently div created from erlang. Now navigate to http://127.0.0.1:8000/my-dasherl-route and the input text should be rendered. To check supported components check `dasherl_components` module.

### @TODO

* Support callbacks decorator from Erlang.

#### Authors

@zgbjgg Jorge Garrido <zgbjgg@gmail.com>
