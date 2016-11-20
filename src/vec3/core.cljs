(ns vec3)


(defn det [[a b c]
           [d e f]
           [g h i]]
  (- 
    (+
      (* a e i)
      (* b f g)
      (* c d h)
    )
    (* a f h)
    (* b d i)
    (* c e g)
    )
  )