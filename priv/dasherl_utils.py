# utilities for dasherl

def build_kwargs(keywords):
    if are_keywords(keywords) == True:
        kwargs = dict([ (k, _build_list_kwargs(v)) for (k, v) in keywords ])
        return kwargs
    else:
        return keywords

def _build_list_kwargs(keywords):
    if isinstance(keywords, list) and are_keywords(keywords) == True:
        kwargs = [ build_kwargs(item) for item in keywords ]
        return kwargs
    else:
        return keywords

def are_keywords(keywords):
    for keyword in keywords:
        print(isinstance(keyword, tuple))
        if not isinstance(keyword, tuple) or not len(keyword) == 2:
            return False
    return True
