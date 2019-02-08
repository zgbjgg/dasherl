# The code in this py file is separated because there are 
# some errors using the interface with the main app.

# imports from erlport
from erlport.erlterms import Atom
from erlport.erlang import call

# render layout from dasherl_apps configured
def render_layout(path, dasherl_apps):
    # simply call to erlang module and ask for app rendering that path
    dapp = call(Atom("dasherl_router"), Atom("render"), [path])
    mod = getattr(dasherl_apps, dapp, None)
    if mod is None:
        return None
    else:
        return getattr(mod, 'layout', None)
