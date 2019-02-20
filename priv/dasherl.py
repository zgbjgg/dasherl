from __future__ import unicode_literals

# import core dash components
import dash_core_components as dcc
import dash_html_components as html
from dash.dependencies import Input, Output

# define the behaviour of the main app when launching this file,
# this function defines all the behaviour of the app.
# @CAUTION: Do not touch this code unless you know what you're doing
import dash

# imports used for gunicorn base
import gunicorn.app.base
from gunicorn.six import iteritems

# in the state store the key/value as dictionary
# holding the gunicorn process and the dash app
state = {}

# manage routing to layouts
import dasherl_router

def initialize(workers, bind, external_stylesheets, appid):
    # define main app and prepare to run
    global app
    app = dash.Dash(__name__, external_stylesheets=external_stylesheets)
    app.config.suppress_callback_exceptions = True

    # the flask app
    server = app.server

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
        if pathname is not None:
            layout = dasherl_router.render_layout(pathname)
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

# in order to run the application, keep it into a process from erlang side,
# check for key stored in state (for appid), and run.
def run(appid):
    application = state[appid]
    application.run()
