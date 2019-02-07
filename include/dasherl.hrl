% define for defaults
-define(DEFAULT_WORKERS, 3).
-define(DEFAULT_BIND, "127.0.0.1:8000").
-define(GUNICORN, "gunicorn").

% define the macros for CMD building
-define(BIND(Bind), " --bind=" ++ Bind).
-define(WORKERS(Workers), " --workers=" ++ integer_to_list(Workers)).
-define(CHDIR(Chdir), " --chdir=" ++ Chdir).
-define(PIDFILE(UnixPid), " --pid=" ++ UnixPid).
-define(APP, " dasherl:server").
