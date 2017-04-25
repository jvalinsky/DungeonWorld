; File for initial loading of gridworld code
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Note: Do not change the loading order, as some definitions in earlier
; files may be needed in later files.
(load "gridworldlib/gridworld-definitions.lisp")
(load "gridworldlib/gridworld-planning.lisp")
(load "gridworldlib/simulation-and-function-evaluation.lisp")
(load "gridworld-worldmap.lisp")
(load "gridworld-dungeonworld.lisp")
(load "implement-effects_web.lisp")
(load "gridworld-exops_web.lisp")
(load "go_web.lisp")
(initialize-state-node)
