-module(dasherl_handler).

-type dasherl_component() :: dasherl_html_component | dasherl_core_component.
-type dasherl_dependency() :: dasherl_input_dependency | dasherl_output_dependency.
-type dasherl_keywords() :: [{atom(), atom() | [dasherl_layout(), ...]}, ...].
-type dasherl_layout() :: {dasherl_component(), atom(), dasherl_keywords()}.
-type dasherl_callback() :: {dasherl_dependency() | [dasherl_dependency(), ...],
    dasherl_dependency() | [dasherl_dependency(), ...],
    atom(), atom(), atom()}.    

-callback layout() -> dasherl_layout().

-callback callbacks() -> [dasherl_callback(), ...].
