# Iris

Iris is a OpenGL-like graphics pipeline implemented in Clojure for
educational purposes.  Others may find this useful for understanding a
basic graphics pipeline.  I've used it to learn about programming in
Clojure and taking advantage of lazy sequences and futures.

## Usage

`lein run configfile.edn` will output a ppm file to stdout based on
the configfile.edn.  See `doc/example1.edn` for the format.

Start with `render-framebuffer` in `src/iris/pipeline.clj` and follow
the rendering...

## License

Copyright © 2013 Roger Allen

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
