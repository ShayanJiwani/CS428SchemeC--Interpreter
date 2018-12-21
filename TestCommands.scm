;; fcall test
(let ( ( environment ( eval_def '(fun sq (x) (pexpr * x x)) '() '() '() ) ) )
     (eval_expr '(fcall sq (3)) (car environment) '() (cadr environment)))
     
;; fcall test, mess with it and fix
(fcall '(fcall sq (3)) '((sq @)) '() '((@ (x) (pexpr * x x) ((sq @) ()))
