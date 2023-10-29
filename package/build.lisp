(proclaim '(optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))
(pushnew (uiop:getcwd) asdf:*central-registry*)
(ql:quickload '(#:mana-break #:deploy))
(asdf:make :mana-break)
