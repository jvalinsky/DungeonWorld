; File for initial loading of gridworld code
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Note: Do not change the loading order, as some definitions in earlier
; files may be needed in later files.
(load "gridworldlib/gridworld-definitions.lisp")
(load "gridworldlib/gridworld-planning.lisp")
(load "gridworldlib/go.lisp")
(load "gridworldlib/implement-effects.lisp")
(load "gridworldlib/simulation-and-function-evaluation.lisp")
(load "gridworld-worldmap.lisp")
(load "gridworld-dungeonworld.lisp")
(initialize-state-node)
