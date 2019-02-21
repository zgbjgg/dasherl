# The code in this py file is separated because there are 
# some errors using the interface with the main app.

# imports from erlport
from erlport.erlterms import Atom
from erlport.erlang import call

def render_layout(path):
    """Render a layout from dasherl erlang side."""

    # simply call to erlang module and ask for app rendering that path
    layout = call(Atom("dasherl_router"), Atom("render"), [path])

    # maybe route doesnt exists so validate response
    if layout == 'no_such_layout':
        return None
    else:
        return layout
