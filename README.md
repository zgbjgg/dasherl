# dasherl

**Dasherl** - A deployment environemt and builder for Dash apps running from Erlang/Elixir

[![License: MIT](https://img.shields.io/github/license/zgbjgg/jun.svg)](https://raw.githubusercontent.com/zgbjgg/dasherl/master/LICENSE)

Dasherl is a deployment environment (using gunicorn) for dash apps using Erlang or Elixir, but also it's a builder for the
dash apps so you can write erlang code (just through nested records) and setup the route to use the erlang code then Dasherl can
handle the requests to erlang code as serve as a python code (using dash).

This project is under development and should not be used in production, it's not ready for that.

### Creating an isolated environment

To create an environment with dash running on gunicorn just start a single process. In order to start you need define the settings used
by gunicorn, for now there are two options: bind that defines the ip and port for wsgi server and workers:

```erlang
(dasherl@hurakann)1> Settings = [{bind, "127.0.0.1:8000"}, {workers, 3}]
[{bind,"127.0.0.1:8000"},
 {workers,3}]
(dasherl@hurakann)1> {ok, Pid} = dasherl_gunicorn_worker:start_link(Settings).
12:30:02.121 [info] initialized default modules for py pid <0.353.0>
{ok,<0.352.0>}
12:30:03.181 [info] gunicorn is up and running at linked process: <0.354.0>
```

That's all, if all goes ok then point your browser to http://127.0.0.1:8000 and you will see the error 404 page, since there are not
routes defined.

### Working with routes & render

The routes are defined as an application environment for dasherl, the next is the structure (visit config/sys.config for more info):

```erlang
{routes, [
    {"/apps/app1", app1},
    {"/apps/app2", app2}
]}
```

Routes are defined through a proplists containing the uri for the route (the one when request to dash) and the app py file where code
for layout and callbacks are exposed, by default for now the py files should be placed under priv/dasherl_apps. The files MUST implement
an import for main app object:

```python
from dasherl import app
```

Also check that dasherl_apps directory has a `__init__.py` file and in this file import every py file.

However this is a preloaded method to render layouts, callbacks from dash, but there is a method easier than this, because you depend
on programming py files to do layouts, components, callbacks, etc. The method is prepared to `write and load python code in the fly` so
writing erlang code you can define how a route will work and the layout to be displayed.

This method also allows store all erlang code for example in a file, and voilá, erlang code working as py code. In the next section is
described how create a simple layout from erlang and render via Dash.

### Write it like erlang use it like Dash

In previous section learn about how routes preloaded works, but, the best approach is writing all from erlang and render in python
so for that reason there is a interface to write erlang code and use it like python in dash server.

The first piece is adding a new route rendering erlang code, so let's write the erlang code that will render a layout. In the dasherl_components
there are many functions as in py to create components (core & html) as dash does in python, for example, lets create a layout with a div and inside
a div a single input text.

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

In the above example the div is an html dash component containing an input text core dash component, so now, how render this in dash server?, the answer
is simple: a route.

Let's create a new route and set the div as main layout:

```erlang
(dasherl@hurakann)3> dasherl_router:add_route("/my-dasherl-route", {erl, Div}).
ok
```

Notice that second parameter is a tuple with erl atom and the next is the recently div instead of a single atom as in preloaded apps. Now navigate to
http://127.0.0.1:8000/my-dasherl-route and the input text should be rendered. To check supported components check `dasherl_components` module.

### @TODO

* Support callbacks decorator from Erlang.

#### Authors

@zgbjgg Jorge Garrido <zgbjgg@gmail.com>
