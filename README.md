Pong Clock
==========

About
-----
This is a clock that uses the game of Pong to tell the time.

It was originally a HTML5 + JS version by The Silvervest Group Labs (see http://www.reddit.com/r/web_design/comments/f6xff/html5js_pong_clock/)

I've ported to Chicken Scheme + GLUT/OpenGL (http://github.com/ZaneA/PongClock)

To run it, you'll need Chicken Scheme, with a couple of Eggs (chicken-install gl and glut).

Then, `cd` to this folder, and run (this is what I used, ymmv): `csc clock.scm -LGLU`

Now you can do `./clock` and see it in all its beauty :)


TODO
----

* Fullscreen option
* Use a proper font
* Sounds?
