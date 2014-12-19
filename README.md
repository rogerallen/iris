# Iris

Iris is a OpenGL 1.0-like graphics pipeline implemented in Clojure for
educational purposes.  Others may find this useful for understanding a
basic graphics pipeline.  I've used it to learn about programming in
Clojure, performance in Clojure, lazy sequences and futures.

My main goals were:

1. To write a simple simulator in Clojure that shows how a very basic
graphics pipeline works in an easy-to-follow manner.

2. To take advantage of lazy sequences and the embarrassing
parallelism of the graphics pipeline to achieve easy speedup via
multiple concurrent threads.

In case it isn't obvious, a non-goal is to actually create a useful
graphics renderer.  :-)

I would be very interested to hear feedback on how well I did with (1)
in your opinion.  I would definitely consider pull requests that add
"FIXME" features, increase readability and make the code more
idiomatic.

I'm pretty happy with (2).  Adding simple parallelism was
straightforward via futures.  I only had to run `jvisualvm` once to
gain almost perfect parallelism speedup on a fullscreen triangle
render.

I was also pleasantly surprised to see that emacs renders ppm files!

## Usage

`lein run configfile.edn` will output a ppm file to stdout based on
the configfile.edn.  See `doc/example1.edn` for the format.

Start with `render-framebuffer` in `src/iris/pipeline.clj` and follow
the rendering...

`lein test` to check basic operation and run some performance tests.

## License

Copyright © 2013-2014 Roger Allen

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
