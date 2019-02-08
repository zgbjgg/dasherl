# import for dash components html and core
import dash_core_components as dcc
import dash_html_components as html

# import erlang helpers
from erlport.erlterms import Atom
from erlport.erlang import set_encoder, set_decoder

# setup a custom encoder for dasherl components, this
# should be render from erlang side
def setup_dasherl_components_type():
    set_decoder(dasherl_components_decoder)
    return Atom("ok")

# decode the value and check if is a dasherl component
# since a record is a tuple we can handle easily here
def dasherl_components_decoder(value):
    if isinstance(value, tuple) and len(value) == 2:
        if isinstance(value[0], tuple) and len(value[0]) == 3:
            full_component = value[0]
            value = (parse_component(full_component), value[1])
    return value

# parse component iterative
def parse_component(component):
    if isinstance(component, tuple)  and len(component) == 3:
        if component[0] == "dasherl_core_component" or component[0] == "dasherl_html_component": 
            fn = component[1]
            keywords = component[2]
            # since keywords is a list parse to check if there are any other component
            keywords = [ (k, parse_component_nested(v)) for (k, v) in keywords ]
            if component[0] == "dasherl_core_component":
                component = to_dash_core_component(fn, keywords)
            elif component[0] == "dasherl_html_component":
                component = to_dash_html_component(fn, keywords)
    return component

# parse nested components such as children of div html
def parse_component_nested(component):
    if isinstance(component, tuple):
        return parse_component(component)
    elif isinstance(component, list):
        return [ parse_component(v) for v in component ]
    else:
        return parse_component(component)

# to dash core
def to_dash_core_component(fn, keywords):
    fn_core = getattr(dcc, fn)
    kwargs = build_kwargs(keywords)
    return fn_core(**kwargs)

# to dash html 
def to_dash_html_component(fn, keywords):
    fn_html = getattr(html, fn)
    kwargs = build_kwargs(keywords)
    return fn_html(**kwargs)

def build_kwargs(keywords):
    kwargs = dict([ (k, v) for (k, v) in keywords ])
    return kwargs
