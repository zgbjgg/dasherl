# The code in this py file is separated because there are 
# some errors using the interface with the main app.

# imports from erlport
from erlport.erlterms import Atom
from erlport.erlang import call

# import utils
import dasherl_utils

def response(key, args):
    """Process the callback to erlang side and wait for response."""

    response = call(Atom("dasherl_binding"), Atom("callback"), [key, args])

    # parse response in order to check if comes a figure in it
    response = _parse_response(response)

    # maybe response breaks!
    if response == 'error_in_response':
        return 'ERROR!'
    else:
        return response

# parse response, for now only figure data are parsed, other
# data keep in the same format
def _parse_response(response):
    if isinstance(response, list):
        response = tuple([_parse_response(r) for r in response])
    if isinstance(response, tuple) and len(response) == 2:
        tag = response[0]
        traces = response[1]
        if tag == 'data':
            traces = [ dasherl_utils.build_kwargs(trace) for trace in traces ]
        response = dict([(response[0], traces)])
    return response
