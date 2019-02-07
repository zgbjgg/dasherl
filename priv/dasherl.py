from __future__ import unicode_literals

# import core dash components
import dash_core_components as dcc
import dash_html_components as html
from dash.dependencies import Input, Output

# import for erlang
from erlport.erlterms import Atom
from erlport.erlang import call

# define the behaviour of the main app when launching this file,
# this function defines all the behaviour of the app.
# @CAUTION: Do not touch this code unless you know what you're doing
import dash

# imports used for gunicorn base
import multiprocessing
import gunicorn.app.base
from gunicorn.six import iteritems

app = None

def initialize(workers, bind, external_stylesheets):
    # define main app and prepare to run
    global app
    app = dash.Dash(__name__, external_stylesheets=external_stylesheets)
    app.config.suppress_callback_exceptions = True

    # the flask app
    server = app.server

    # dynamicall import for a package, as a rule all layouts should be
    # placed under this directory. The directory path can be configured
    # from erlang and from here just get the import working
    import dasherl_apps

    # define the main layout
    app.layout = html.Div([
        dcc.Location(id='url', refresh=False),
        html.Div(id='page-content')
    ])

    # the main app callback just parses the pathname and then
    # redirect to the correct app (from dasherl_apps). This config
    # also is stored from erlang side, so when an input comes just go to erlang
    # ask about config and run dynamicall using getattr of module imported!
    @app.callback(Output('page-content', 'children'),
                  [Input('url', 'pathname')])
    def display_page(pathname):
        render = render_layout(pathname)
        if render is None:
            return '404 NOT FOUND by @dasherl'
        else:
            return render

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
        'daemon': True,
        'loglevel': 'critical'
    }

    DasherlApplication(server, options).run()

# render app from config set in erlang worker
def render_layout(path):
    # simply call to erlang module and ask for app rendering that path
    dapp = call(Atom('dasherl_router'), Atom('render'), [path])
    mod = getattr(dasherl_apps, dapp, None)
    if mod is None:
        return None
    else:
        return getattr(mod, 'layout', None)
