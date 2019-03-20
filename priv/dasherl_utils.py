# utilities for dasherl

def build_kwargs(keywords):
    kwargs = dict([ (k, _check_value(v)) for (k,v) in keywords ])
    return kwargs

def _check_value(v):
    if isinstance(v, list):
      kwargs = [ _build_list_kwargs(item) for item in v ]
      return kwargs
    else:
      return v

def _build_list_kwargs(keywords):
    if isinstance(keywords, list):
        kwargs = build_kwargs(keywords)
        return kwargs
    else:
        return keywords
