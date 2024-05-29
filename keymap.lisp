(trigger next
         (key :one-of (:page-down))
         (button :one-of (:r1)))

(trigger prev
         (key :one-of (:page-up))
         (button :one-of (:l1)))

(trigger reload
         (key :one-of (:f5))
         (button :one-of (:select)))

(trigger exit
         (key :one-of (:escape))
         (button :one-of (:home)))
