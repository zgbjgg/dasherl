from __future__ import unicode_literals

# import core dash components
import dash_core_components as dcc
import dash_html_components as html
from dash.dependencies import Input, Output

# import for erlang interface
import dasherl_router

# define the behaviour of the main app when launching this file,
# this function defines all the behaviour of the app.
# @CAUTION: Do not touch this code unless you know what you're doing
import dash

# imports used for gunicorn base
import multiprocessing
import gunicorn.app.base
from gunicorn.six import iteritems

# in the state store the key/value as dictionary
# holding the gunicorn process and the dash app
state = {}

def initialize(workers, bind, external_stylesheets, appid):
    # define main app and prepare to run
    app = dash.Dash(__name__, external_stylesheets=external_stylesheets)
    app.config.suppress_callback_exceptions = True

    # the flask app
    server = app.server

    # this dictionary is used globally since can be modified outside
    # the child process. This should be join to the dict in manager
    global routes
    routes = {}

    # define the main layout
    app.layout = html.Div([
        dcc.Location(id='url', refresh=False),
        html.Div(id='page-content')
    ])

    # the main app callback just parses the pathname and then
    # redirect to the correct app represented by an erlang module or a single layout.
    # The layout can be built from erlang side using components and dependencies.
    @app.callback(Output('page-content', 'children'),
                  [Input('url', 'pathname')])
    def display_page(pathname):
        # render = dasherl_router.render_layout(pathname)
        if pathname is not None:
            layout = routes.get(pathname, None)
            if layout is None:
                return '404 NOT FOUND by @dasherl'
            else:
                return layout
        else:
            return '404 NOT FOUND by @dasherl'

    # define the class for standalone application for gunicorn
    class DasherlApplication(gunicorn.app.base.BaseApplication):

        def __init__(self, app, options=None):
            self.options = options or {}
            self.application = app
            super(DasherlApplication, self).__init__()

        def load_config(self):
            config = dict([(key, value) for key, value in iteritems(self.options)
                           if key in self.cfg.settings and value is not None])
            for key, value in iteritems(config):
                self.cfg.set(key.lower(), value)

        def load(self):
            return self.application

    # now run explicit the server
    options = {
        'bind': bind,
        'workers': workers,
        'loglevel': 'critical'
    }

    application = DasherlApplication(server, options)
    state[appid] = application
    return 'ok'

# in order to run the application, keep it into a process,
# check for key stored in state (for appid), and run.
# This will hold a separated process running with gunicorn, for that
# reason share an object to use the routes
def run(appid, pid):
    application = state[appid]

    # create manager and start the dict
    manager = multiprocessing.Manager()

    # maybe some routes was loaded before process start, then copy that
    # to shared object in order to work into process
    global routes
    preloaded_routes = routes
    routes = manager.dict()

    if not isinstance(preloaded_routes, multiprocessing.managers.DictProxy):
        for k, v in preloaded_routes.iteritems():
            routes[k] = v

    # create the process and start without joining!
    process = multiprocessing.Process(name=appid, target=application.run)
    state[pid] = process
    process.start()
    return 'ok'

# stops the running process and clean state
def stop(pid):
    process = state.get(pid, None)
    if process is not None:
        process.terminate()
        state.pop(pid)
        return 'ok'
    else:
        return 'no_proc'

# add a route with content, the content will be a complex
# layout made from erlang side using custom components.
def setup_route(route, layout):
    routes[route] = layout # simple assignment
    return 'ok'

# remove a route already loaded into routes.
def del_route(route):
    layout = routes.get(route, None)
    if layout is not None:
        del routes[route]
        return 'ok'
    else:
        return 'ok'
