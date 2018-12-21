(define eval_defs
  (lambda (def env store defs)
    (if (null? def)
      (list env store defs)
      (let ((new-binding (eval_def (car def) env store defs)))
        (let ((merge-list (mergeAll (caar def) new-binding env store defs)))
              (eval_defs (cdr def) (car merge-list) (cadr merge-list) (caddr merge-list))
        )
      )
    )
  )
)

(define mergeAll
  (lambda (def new-binding env store defs)
    (cond
      ((eq? def 'vdef)
        (list
          (cons (car new-binding) env)
          (cons (cadr new-binding) store)
          defs
        )
      )
      ((eq? def 'fdef)
        (list
          (cons (car new-binding) env)
          store
          (cons (cadr new-binding) defs)
        )
      )
      ((eq? def 'pdef)
        (list
          (cons (car new-binding) env)
          store
          (cons (cadr new-binding) defs)
        )
      )
      (else display def)
    )
  )
)



(define merge2
  (lambda (new-binding env store)
    (list
      (cons (car new-binding) env)
      (cons (cadr new-binding) store)
      ()
      )))

(define merge
  (lambda (new-binding env defs)
    (cons
      (cons (cons (car new-binding) (cddr new-binding)) defs)
      (cons (cons (cadr new-binding) (car new-binding)) env))))

(define eval_def
  (lambda (def env store defs)
    (cond
    ((vdef? def)
      (let ((ref (gensym)))
        (let ((new-binding (list (vname def) ref)))
          (let ((new-env (list new-binding env )))
            (list new-binding
              (list ref (eval_expr (caddr def) env store defs)))))))
    ((fdef? def)
        (let ((ref (gensym)))
          (let ((new-binding (list (fname def) ref)))
            (let ((new-env (list new-binding env )))
              (list new-binding
                (list ref (params def) (body def) new-env))))))
    ((pdef? def)
        (let ((ref (gensym)))
          (let ((new-binding (list (pname def) ref)))
            (let ((new-env (list new-binding env )))
              (list new-binding
                (list ref (params def) (body def) new-env))))))
    (else (error ""unknown definition type"" def)))
   )
)

(define fname
  (lambda (def)
    (cadr def)))

(define vname
  (lambda (def)
    (cadr def)))

(define pname
  (lambda (def)
    (cadr def)))

(define params
  (lambda (def)
    (caddr def)))

(define body
  (lambda (def)
    (cadddr def)))

(define fdef?
  (lambda (def)
    (eq? (car def) 'fdef)))

(define vdef?
  (lambda (def)
    (eq? (car def) 'vdef)))

(define pdef?
  (lambda (def)
    (eq? (car def) 'pdef)))






    ;; subexp2 not working
;; reason: subexp1 and subexp are not defined
(define eval_expr
  (lambda (exp env store defs)
    (cond
    ((number? exp) exp)
    ((symbol? exp)
      (findvar (cadr (assoc exp env)) store))
    ((sum? exp)
     (+ (eval_expr (subexp1 exp) env store defs)
        (eval_expr (subexp2 exp) env store defs)))
    ((difference? exp)
     (- (eval_expr (subexp1 exp) env store defs)
        (eval_expr (subexp2 exp) env store defs)))
    ((product? exp)
     (* (eval_expr (subexp1 exp) env store defs)
        (eval_expr (subexp2 exp) env store defs)))
    ((quotient? exp)
     (/ (eval_expr (subexp1 exp) env store defs)
        (eval_expr (subexp2 exp) env store defs)))
    ((remainder? exp)
      (mod (eval_expr (subexp1 exp) env store defs)
        (eval_expr (subexp2 exp) env store defs)))
    ((if_expr? exp)
      (if (eval_bool (cadr exp) env store defs)
        (eval_expr (caddr exp) env store defs)
        (eval_expr (cadddr exp) env store defs)))
   ((fcall? exp)
    (let ((new-fenv (eval_defs (cons (list 'vdef (caar (cadr (assoc (cadr (assoc (cadr exp) env)) defs)))))  )))
      (eval_expr (cadddr (assoc (cadr (assoc (cadr exp) env)) defs))))
    )
    (else (error 'eval_expr "invalid expression ~s" exp)))
  )
)

(define vmap
  (lambda (vlist)
    (map (lambda (x) (list 'vdef x)) vlist)
  )
)


(cadddr (assoc (cadr (assoc (cadr '(fcall sq (3))) '((sq @)))) '((@ sq (x) (pexpr * x x) ((sq @))))))

;;Helpers for eval_expr
;(define fcall
;  (lambda (exp env store defs)
;    (cadr (assoc (cadr (assoc (cadr exp) env)) defs))
;  )
;)

;(define fcall
  ;(lambda (exp env store defs)
    ;(let ((ref (cadr (assoc exp env))))
      ;(let ((closure (cadr (assoc ref defs)))))
        ;(let ((actual_params (map eval_expr (caddr exp))))
        ;  (let ((formal_params (closure_params closure)))))
       ; (eval_expr 
      ;    (closure_body closure) 
     ;     (cons (closure_env closure) env) 
    ;      (cons (closure_params closure) store) 
   ;       defs)
  ;  )
 ; )
;)

(define fcall?
 (lambda (exp)
  (eq? (car exp) 'fcall)
 )
)

;(define fcall
;  (lambda (exp env store defs)
;    (let ((ref (cadr (assoc exp env))))
;      (let ((closure (cadr (assoc ref))))))))

(define subexp1
  (lambda (exp) (caddr exp)))

(define subexp2
  (lambda (exp) (cadddr exp)))

(define findvar
  (lambda (exp store) (cadr (assoc exp store))))

(define value_of
  (lambda(pair) (cadr pair)))

(define sum?
  (lambda (exp)
    (eq? (cadr exp) '+)))

(define difference?
  (lambda (exp)
    (eq? (cadr exp) '-)))

(define product?
  (lambda (exp)
    (eq? (cadr exp) '*)))

(define quotient?
  (lambda (exp)
    (eq? (cadr exp) '/)))

(define remainder?
  (lambda (exp) (eq? (cadr exp) '%)))


(define if_expr?
    (lambda (exp env store defs)
      (eq? car(exp) 'if)))


;; HELPER METHODS

(define assign?
  (lambda (x)
    (eq? (car x) 'assign)))
(define skip?
  (lambda (x)
    (eq? x 'skip)))
(define while?
  (lambda (x)
    (eq? (car x) 'while)))
(define if1?
  (lambda (x)
    (eq? (car x) 'if1)))
(define if2?
  (lambda (x)
    (eq? (car x) 'if2)))
(define print?
  (lambda (x)
    (eq? (car x) 'print)))
(define break?
  (lambda (x)
    (eq? (car x) 'break)))

;; EXEC MAIN STATEMENT
(define exec_stmt
  (lambda (stmt rest env store defs)
    (cond 
      ((null? stmt)                                                 ;; no new statement
          (display "null") (newline) )
      ((assign? stmt)                                               ;; setting variable
        (let nvar())
        (eval_expr (caddr stmt) env store defs) 
      )
      ((if1? stmt)                                                  ;; just if stmt
        (if (eval_bool (cadr stmt) env store defs)                  ;;bool
          (exec_stmt (caddr stmt) rest env store defs)              ;; exp
          (if (null? rest)                                 ;; make sure more things left
            (exec_stmt '() rest env store defs)                     ;; end
            (exec_stmt (car rest) (cdr rest) env store defs))))     ;;continue
      ((if2? stmt)                                                  ;; if/else stmt
        (if (eval_bool (cadr stmt) env store defs)                  ;; bool exp
          (exec_stmt (caddr stmt) rest env store defs)              ;; exp 1
          (exec_stmt (cadddr stmt) rest env store defs)))           ;; exp 2
      ((while? stmt) 
        (if (eval_bool (cadr stmt) env store defs)
            (exec_stmt (cddr stmt) store) 
             (display "false")))
      ((break? stmt) 
        defs store)
      ((print? stmt) 
        (display (cadr stmt)) (newline)
        (if (null? rest)
          (exec_stmt '() rest env store defs)
          (exec_stmt (car rest) (cdr rest) env store defs)))
      (else (error 'exec_stmt ""invalid statment ~s"" stmt))
    )
  )
)
;if 1, if its false, move onto the rest of the statements
;(display (eval_expr (cadr stmt) env store defs)) (newline))

;(exec_stmt '(if2 (comp leq 3 3) (print "if true") (print "if false")) 
 ; '((if2 (comp ge 4 3) (print "2t") (print "2f"))) '() '() '())


(define eval_bool
  (lambda (exp env store defs)
    (cond
      ((lessThan exp)
        (< (eval_expr (subexp1 exp) env store defs)
        (eval_expr (subexp2 exp) env store defs)))
      ((lessThanEqual exp)
        (<= (eval_expr (subexp1 exp) env store defs)
        (eval_expr (subexp2 exp) env store defs)))
      ((equalTo exp)
        (= (eval_expr (subexp1 exp) env store defs)
        (eval_expr (subexp2 exp) env store defs)))
      ((greaterThan exp)
        (> (eval_expr (subexp1 exp) env store defs)
        (eval_expr (subexp2 exp) env store defs)))
      ((greaterThanEqual exp)
        (>= (eval_expr (subexp1 exp) env store defs)
        (eval_expr (subexp2 exp) env store defs)))
      ((notEqual exp)
        (not (= (eval_expr (subexp1 exp) env store defs)
        (eval_expr (subexp2 exp) env store defs))))
      (else (error 'eval_expr ""invalid expression ~s"" exp))
    )
  )
)

 (define lessThan
  (lambda (exp)
    (eq? (cadr exp) 'le)))

 (define lessThanEqual
  (lambda (exp)
    (eq? (cadr exp) 'leq)))

 (define equalTo
  (lambda (exp)
    (eq? (cadr exp) 'eq)))

 (define greaterThan
  (lambda (exp)
    (eq? (cadr exp) 'ge)))

 (define greaterThanEqual
  (lambda (exp)
     (eq? (cadr exp) 'geq)))

 (define notEqual
  (lambda (exp)
     (eq? (cadr exp) 'neq)))


;; test

;(let ( ( environment ( eval_def '(fdef sq (x) (pexpr * x x)) '() '() '() ) ) )
;     (eval_expr '(fcall sq (3)) (car environment) '() (cadr environment)))
