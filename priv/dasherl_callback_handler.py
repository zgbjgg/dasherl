# The code in this py file is separated because there are 
# some errors using the interface with the main app.

# imports from erlport
from erlport.erlterms import Atom
from erlport.erlang import call

def response(key, args):
    """Process the callback to erlang side and wait for response."""

    response = call(Atom("dasherl_binding"), Atom("callback"), [key, args])

    # maybe response breaks!
    if response == 'error_in_response':
        return None
    else:
        return response
