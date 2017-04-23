; File for initial loading of gridworld code
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Note: Do not change the loading order, as some definitions in earlier
; files may be needed in later files.
(load "gridworldlib/gridworld-definitions.lisp")
(load "gridworldlib/gridworld-planning.lisp")
(load "gridworldlib/simulation-and-function-evaluation.lisp")
(load "gridworld-worldmap.lisp")
(load "gridworld-dungeonworld.lisp")
(load "implement-effects.lisp")
(load "gridworld-exops.lisp")
(load "go.lisp")
(initialize-state-node)
