(trigger next
         (key :one-of (:page-down :right))
         (button :one-of (:r1)))

(trigger prev
         (key :one-of (:page-up :left))
         (button :one-of (:l1)))

(trigger reload
         (key :one-of (:f5))
         (button :one-of (:select)))

(trigger exit
         (key :one-of (:escape))
         (button :one-of (:home)))
