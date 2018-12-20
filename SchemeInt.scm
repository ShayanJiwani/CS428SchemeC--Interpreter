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
    ((fcall? exp)
      (fcall eval_expr(exp) env store defs))
    ((if_expr? exp)
      (if (eval_bool (cadr exp) env store defs)
        (eval_expr (caddr exp) env store defs)
        (eval_expr (cadddr exp) env store defs)))
    (else (error 'eval_expr "invalid expression ~s" exp)))
  )
)
;;Helpers for eval_expr
(define subexp1
  (lambda (exp) (caddr exp)))

(define subexp2
  (lambda (exp) (cadddr exp)))

(define fcall?
  (lambda (exp env store defs)
    (eq? (cadr exp) (car (assoc exp env)))))

(define fcall
  (lambda (exp env store defs)
    (let ((ref (cadr (assoc exp env))))
      (let ((closure (cadr (assoc ref))))))
        )

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


(define if_expr
    (lambda (exp env store defs)
      (eq? car(exp) 'if)))

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
      (else (error 'eval_expr ""invalid expression ~s"" exp)))))

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


;; eval_def creates the binding for a single definition

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

;; eval_defs loops through multiple definitions, calling eval_def
;; for each, and collects all the bindings into a list.
;; add to store in eval defs
;; merge is adding to the different lists
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
      ((eq? def 'var)
        (list
          (cons (car new-binding) env)
          (cons (cadr new-binding) store)
          defs
        )
      )
      ((eq? def 'fun)
        (list
          (cons (car new-binding) env)
          store
          (cons (cadr new-binding) defs)
        )
      )
      ((eq? def 'proc)
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




(define merge
  (lambda (new-binding env defs)
    (cons
      (cons (cons (cadr new-binding) (car new-binding)) env)
      (cons (cons (car new-binding) (cddr new-binding)) defs))))

(define merge2
  (lambda (new-binding env store)
    (cons
      (cons (car new-binding) env)
      (cons (cadr new-binding) store))))


;; where "merge" would take the bindings returned from eval_def
;; and cons the two association lists to the current env store defs

; ****
(define fdef?
  (lambda (def)
    (eq? (car def) 'fun))) ;; function definition

(define vdef?
  (lambda (def)
    (eq? (car def) 'var))) ;; variable definition

(define pdef?
  (lambda (def)
    (eq? (car def) 'proc))) ;; variable definition

;; HELPER METHODS

(define assign?
  (lambda (x)
    (eq? (car x) 'assign)))
(define skip?
  (lambda (x)
    (eq? x 'skip)))
(define sequence?
  (lambda (x)
    (pair? x)))
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
    (eq? (car x) 'print')))
(define break?
  (lambda (x)
    (eq? (car x) 'break)))

;; EXEC MAIN STATEMENT
(define exec_stmt
  (lambda (stmt rest env store defs)
    (cond
      ((null? stmt)
          defs)
      ((assign? stmt)
        (eval_defs (cdr stmt) env store defs))
      ((if1? stmt)
        ((eval_bool (cadr stmt) store) (exec_stmt (cddr stmt) store) defs))
      ((if2? stmt)
        ((eval_bool (cadr stmt) store)
          (exec_stmt (cddr stmt) store)
          defs))
      ((while? stmt)
        ((eval_bool (cadr stmt) store)
          (exec_stmt (cddr stmt) store)
          defs))
      ((break? stmt)
        defs store)
      ((print? stmt)
        (display (list env store defs)) (newline))
      (else (error 'exec_stmt ""invalid statment ~s"" stmt))
    )
  )
)


(findvar ((cadr(assoc 'x '((x 1) (y 2)))) '((1 2))))

; cadr(rest) --> next statement
; car(rest) --> environment
; rest ==> (env (S1, S2, S3...,Sn))
; need to return the env to use after a block
