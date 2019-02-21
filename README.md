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

The routes and callbacks are expressed as an erlang terms, so building erlang terms with the dasherl api is enough to build layouts or callbacks for events in the dash app. To simplify work dasherl has a module called compiler so building an entire module (erlang module) with a specific behaviour will be enough to compile that module at runtime and setup layouts and callbacks into dash app.

Let's take the Dash example using python code:

```python
import dash
import dash_core_components as dcc
import dash_html_components as html
from dash.dependencies import Input, Output

external_stylesheets = ['https://codepen.io/chriddyp/pen/bWLwgP.css']

app = dash.Dash(__name__, external_stylesheets=external_stylesheets)

app.layout = html.Div([
    dcc.Input(id='my-id', value='initial value', type='text'),
    html.Div(id='my-div')
])


@app.callback(
    Output(component_id='my-div', component_property='children'),
    [Input(component_id='my-id', component_property='value')]
)
def update_output_div(input_value):
    return 'You\'ve entered "{}"'.format(input_value)


if __name__ == '__main__':
    app.run_server(debug=True)
```

The above app can be translated to dasherl into erlang module as follows:

```erlang
-module(mod_example).

-behaviour(dasherl_handler).

-export([layout/0, callbacks/0, my_callback_handler/1]).

layout() ->
    Input = dasherl_components:input([{value, 'initial value'}, {type, 'text'}, {id, 'my-id'}]),
    Div = dasherl_components:divc([{id, 'my-div'}]),
    dasherl_components:divc([{children, [Input, Div]}]).

callbacks() ->
    Output = dasherl_dependencies:output('my-div', 'children'),
    Input = dasherl_dependencies:input('my-id', 'value'),
    [{Output, [Input], 'update_output_div', ?MODULE, my_callback_handler}].

my_callback_handler({InputValue}) ->
    Retval = "You've entered " ++ InputValue,
    list_to_atom(Retval).
```

Now in the dasherl console, start the release, after that start the gunicorn worker, this not start the wsgi server, only prepare to compile module and setup all the environment:

```erlang
(dasherl@zgbjgg)1> {ok, Server} = dasherl_gunicorn_worker:start_link([]).
{ok,<0.358.0>}
20:16:41.828 [info] gunicorn is up and running at linked process: ok
```

Next compile the module above defined, in this case, dasherl needs an url to serve the content:

```erlang
(dasherl@zgbjgg)2> dasherl:compile(Server, "/my-route", mod_example).
ok
```

Finally run the gunicorn server and go to your browser at: http://127.0.0.1:8000/my-route and you will see the same app as python:

```erlang
(dasherl@zgbjgg)3> dasherl_gunicorn_worker:run_server(Server).
ok
```

Nice eh? enjoy!!.

### @TODO

* More complex examples: how crossfiltering using jun.
* Support states into callbacks.

#### Authors

@zgbjgg Jorge Garrido <zgbjgg@gmail.com>
