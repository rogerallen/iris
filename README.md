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

3. To learn about making Clojure performant.

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

Update: I did learn a couple things about clojure math performance
that might be helpful to others.

1. Leiningen DISABLES optimized compilation by default.

Add `:jvm-opts ^:replace []` to your project.clj to enable the
optimizer.  This helped me by 2x immediately.

2. use `*warn-on-reflection` AND the primitive-math library.

As https://github.com/ztellman/primitive-math explains, clojure does
not warn you about basic math reflection.  Using primitive-math is
essential for getting the best perf results!  This got me an
additional 4x speedup.

3. use a disassembler to see the code you are creating.

http://insideclojure.org/2014/12/15/warn-on-boxed/ (note that
unchecked-math doesn't help double math, but can help some integer
math a bit)

4. use `jvisualvm` to profile your code

I bring up jvisualvm via
`/System/Library/Frameworks/JavaVM.framework/Versions/Current/Commands/jvisualvm
& `, select the 2nd clojure process, create a profile that only
searches in the "iris.*" namespace, profile the CPU usage and it
usually points out the issue to focus on.

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
