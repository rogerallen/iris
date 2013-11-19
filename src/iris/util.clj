(ns iris.util)

(defn round255 [x]
  (int (Math/floor (+ 0.5 (* 255 x)))))

(defn print-ppm-data
  "append a framebuffers data for a ppm file to stdout"
  [framebuffer]
  (let [w (:width framebuffer)
        h (:height framebuffer)]
    (doseq [y (range h)]
      (do
        (doseq [x (range w)]
          (let [i (int (+ (* y w) x))
                r (round255 (:r ((:data framebuffer) i)))
                g (round255 (:g ((:data framebuffer) i)))
                b (round255 (:b ((:data framebuffer) i)))]
            (print r g b " ")))
        (println)))))

(defn print-ppm-header
  "print single ppm header for multiple framebuffers"
  [framebuffer]
  (let [iw (:width framebuffer)
        ih (:height framebuffer)]
    (println "P3" iw ih 255)))

(defn print-fb-to-ppm
  "assume framebuffers are sequential in y"
  [framebuffer]
  (print-ppm-header framebuffer)
  (print-ppm-data framebuffer))

(defn round-byte [x]
  (byte (- (Math/floor (+ 0.5 (* 255 x))) 128)))

(defn rgb-byte-array
  [fb]
  (byte-array
   (let [width (:width fb)
         height (:height fb)]
     (for [y (range height)
           x (range width)
           c [:r :g :b]
           :let [i (int (+ (* y width) x))]]
       (round-byte (c ((:data fb) i)))))))

(defn check-crc
  [framebuffer]
  (let [ crc (new java.util.zip.CRC32)
        _ (.update crc (rgb-byte-array framebuffer))]
    (.getValue crc )))
