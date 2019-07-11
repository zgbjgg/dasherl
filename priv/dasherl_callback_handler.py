#!/usr/bin/env python
# -*- coding: utf-8 -*-

# The code in this py file is separated because there are 
# some errors using the interface with the main app.

# imports from erlport
from erlport.erlterms import Atom
from erlport.erlang import call
import pandas as pd
import dash

# import utils
import dasherl_utils

def response(key, args):
    """Process the callback to erlang side and wait for response."""

    try:
        response = call(Atom("dasherl_binding"), Atom("callback"), [key, args])

        # maybe response breaks!
        if response == 'error_in_response':
            raise dash.exceptions.PreventUpdate
        else:
            # parse response in order to check if comes a figure in it
            response = _parse_response(response)
            return response
    except BaseException, ex:
        print(str(ex))
        raise dash.exceptions.PreventUpdate

# parse response, for now only figure data are parsed, other
# data keep in the same format
def _parse_response(response):
    if isinstance(response, list):
        response = tuple([_parse_response(r) for r in response])
    if isinstance(response, pd.core.frame.DataFrame):
        response = response.to_dict('records')
        # format floats nicely since this could be difficult from erlang side
        for row in response:
            for key, value in row.iteritems():
                if isinstance(value, float):
                    value = '{:,.2f}'.format(value)
                row[key] = value
    elif isinstance(response, tuple) and len(response) == 2:
        tag = response[0]
        traces = response[1]
        if tag == 'data':
            traces = [ dasherl_utils.build_kwargs(trace) for trace in traces ]
            response = dict([(response[0], traces)])
    return response

