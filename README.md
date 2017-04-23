# DungeonWorld

CSC 291 Final Project
----------------------

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
	* Return: a list of edges, each in the form `?room&?x1&?y1_?room&?x2&?y2`
* `def-room`
	* Function that define a room in the simulated world
	* Input: ?room ?x1 ?x2 ?y1 ?y2
		* ?room: a symbol representing room name 
		* ?x1: an __integer__ indicating x coordinate of the upper left corner
		* ?x2: an __integer__ indicating x coordinate of the lower right corner
		* ?y1: an __integer__ indicating y coordinate of the upper left corner
		* ?y2: an __integer__ indicating y coordinate of the lower right corner
	* Return: a list which has the first element a list of points in the room, and second element representing all the edges in the room