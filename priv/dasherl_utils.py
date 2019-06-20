#!/usr/bin/env python
# -*- coding: utf-8 -*-

# utilities for dasherl

def build_kwargs(keywords):
    kwargs = dict([ (k, _check_value(k, v)) for (k,v) in keywords ])
    return kwargs

def _check_value(k, v):
    if isinstance(v, list):
        kwargs = [ _build_list_kwargs(item) for item in v ]
        # since other args than children should be dicts
        if k != 'children' and k != 'options' and k != 'columns' and k != 'data':
            return dict(kwargs)
        else:
            return kwargs
    else:
        return v

def _build_list_kwargs(keywords):
    if isinstance(keywords, list):
        kwargs = build_kwargs(keywords)
        return kwargs
    else:
        return keywords
