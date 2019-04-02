#!/usr/bin/env python
# -*- coding: utf-8 -*-

# import for dash components html and core
import dash_core_components as dcc
import dash_html_components as html
import dash_table as ddt
from dash.dependencies import Input, Output

# import erlang helpers
from erlport.erlterms import Atom
from erlport.erlang import set_encoder, set_decoder

# utilities
import dasherl_utils
import pandas as pd

def compile(layout):
    """Simple compile erlterm into pyterm and return as pickled data"""
    return (Atom("ok"), layout)

def setup_dasherl_components_type():
    """Setup a custom encoder for dasherl components, this should be render from erlang side"""
    set_decoder(dasherl_components_decoder)
    return Atom("ok")

# decode the value and check if is a dasherl component
# since a record is a tuple we can handle easily here
def dasherl_components_decoder(value):
    if isinstance(value, tuple) and len(value) == 3:
        value = _parse_component(value)
    elif isinstance(value, list):
        return [ _parse_component(component) for component in value ]
    return value

# parse component iterative
def _parse_component(component):
    if isinstance(component, tuple)  and len(component) == 3:
        if component[0] == "dasherl_core_component" or component[0] == "dasherl_html_component": 
            fn = component[1]
            keywords = component[2]
            # since keywords is a list parse to check if there are any other component
            keywords = [ (k, _parse_component_nested(v)) for (k, v) in keywords ]
            if component[0] == "dasherl_core_component":
                component = _to_dash_core_component(fn, keywords)
            elif component[0] == "dasherl_html_component":
                component = _to_dash_html_component(fn, keywords)
        elif component[0] == "dasherl_output_dependency" or component[0] == "dasherl_input_dependency":
            component = _to_dash_dependency(component[0], component[1], component[2])
    return component

# parse nested components such as children of div html
def _parse_component_nested(component):
    if isinstance(component, tuple):
        return _parse_component(component)
    elif isinstance(component, list):
        return [ _parse_component(v) for v in component ]
    else:
        return _parse_component(component)

# to dash core
def _to_dash_core_component(fn, keywords):
    # datatable is not a core component, check to apply the correct value
    if fn == 'DataTable':
      fn_core = getattr(ddt, fn)
      kwargs = dasherl_utils.build_kwargs(keywords)
      data = kwargs.get("data")
      if isinstance(data, pd.core.frame.DataFrame):
        kwargs["data"] = data.to_dict("rows")
    else:
      fn_core = getattr(dcc, fn)
      kwargs = dasherl_utils.build_kwargs(keywords)
    return fn_core(**kwargs)

# to dash html 
def _to_dash_html_component(fn, keywords):
    fn_html = getattr(html, fn)
    kwargs = dasherl_utils.build_kwargs(keywords)
    return fn_html(**kwargs)

# to dash dependency
def _to_dash_dependency(dep, component, event):
    if dep == "dasherl_output_dependency":
        return _to_dash_output(component, event)
    elif dep == "dasherl_input_dependency":
        return _to_dash_input(component, event)

def _to_dash_output(component, event):
    return Output(component, event)

def _to_dash_input(component, event):
    return Input(component, event)
