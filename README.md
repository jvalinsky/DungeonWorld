# DungeonWorld

CSC 291 Final Project
----------------------

## How to run (terminal)
start allegro common lisp (on the undegrad cycles it is alisp, this will not work in steel bank common lisp)
(load "init.lisp")

use go! and listen! macros to interact with and step through execution of gridworld code

## How to run (Web GUI)

**need quicklisp installed!**
See quicklisp website for installing quicklisp (it's a package manager for handling external common lisp libraries)
Once quicklisp is set up to load everytime the allegro common lisp is started then one can start the server using:
(load "server.lisp")

Using a web browser navigate to localhost:5000 to see the web GUI

The button go runs the go! macro, listen runs the listen! macro and has a text field for input, eval can run arbitrary lisp
code. eval is meant for debugging purposes and is not recommended for use on a public server.


#### `gridworld-worldmap.lisp`
* A room can be represented using points and edges as following:
```
*-*-*-*
| | | |
*-*-*-*
| | | |
*-*-*-*
| | | |
*-*-*-*

```

* `room-pts-rect`
	* Given the coordinate of upper left corner and lower right corner, this function calculate all of the points occupied by this room
	* Input: ?room ?x1 ?x2 ?y1 ?y2
		* ?room: a __symbol__ indicating the room name
		* ?x1: an __integer__ indicating x coordinate of the upper left corner
		* ?x2: an __integer__ indicating x coordinate of the lower right corner
		* ?y1: an __integer__ indicating y coordinate of the upper left corner
		* ?y2: an __integer__ indicating y coordinate of the lower right corner
	* Return: a list of points for a room, each in a form of `?room&?x&?y`

* `room-edges-rect-helper` 
	* helper function for recursively computing the edges in a room that connect each point
* `room-edges-rect`
	* Given a room name and a list of points (assume the points are all in the same room), computes the edges connecting all the points
	* Input: ?room ?points
		* ?room: a __symbol__ indicating the room name
		* ?points: a list of points previously computed using `room-pts-rect`
	* Return: a list of edges, each in the form `?room&?x1&?y1$?room&?x2&?y2`
* `def-room`
	* Function that define a room in the simulated world
	* Input: ?room ?x1 ?x2 ?y1 ?y2
		* ?room: a symbol representing room name 
		* ?x1: an __integer__ indicating x coordinate of the upper left corner
		* ?x2: an __integer__ indicating x coordinate of the lower right corner
		* ?y1: an __integer__ indicating y coordinate of the upper left corner
		* ?y2: an __integer__ indicating y coordinate of the lower right corner
	* Return: a list which has the first element a list of points in the room, and second element representing all the edges in the room
