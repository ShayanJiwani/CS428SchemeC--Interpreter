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
      (fcall exp env store defs))
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
    (eq? (car exp) 'fcall)))

(define param_vals ;;calculates the actual vals of function params
  (lambda (params env store defs)
    (cond
    ((null? (cdr params)) ;;if param list is size 1
      (list(eval_expr (car params) env store defs)))
    (else (cons (eval_expr (car params) env store defs) (param_vals (cdr params) env store defs)))
    )
  )
)

(define temp_refs ;;creates a list of references for each param
  (lambda (params)
    (cond
    ((null? (cdr params))
      (list (gensym)))
    (else (cons (gensym) (temp_refs(cdr params)))))))


;(cadr (assoc (cadr '(fcall sq (3))) '((sq @))))
;(assoc '@ '((@ (x) (pexpr * x x) ((sq @) ()))))

;(param_vals '(x) '((a #) (sq @)) '((# 4)) '(((@ (x) ;(pexpr * x x) ((sq @) ())))))

(define fcall
  (lambda (exp env store defs)
    (let ((ref (cadr (assoc (cadr exp) env))))
      (let ((closure (assoc ref defs)))
        (let ((formal-params (closure_params closure)))
          (let ((param-refs (temp_refs (caddr exp))))
            (let ((param-vals (param_vals (caddr exp) env store defs)))
              (let ((new-env (zip formal-params param-refs)))
                (let ((new-store (zip param-refs param-vals)))
                  (eval_expr (closure_body closure) new-env new-store defs))))))))))

(define ref_list
    (lambda (keys lst)
      (map (lambda (key) (cadr (assoc key lst)))
           keys)))

(define zip
  (lambda (list1 list2)
    (map list list1 list2)))


;;store takes eval_expr applied to actual_params

(define closure_env
  (lambda (closure)
      (cadddr  closure)))

(define closure_body
  (lambda (closure)
    (caddr  closure)))

(define closure_params
  (lambda (closure)
    (cadr  closure)))

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
      (eq? (car exp) 'if)))

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
    (eq? (car def) 'fdef))) ;; function definition

(define vdef?
  (lambda (def)
    (eq? (car def) 'vdef))) ;; variable definition

(define pdef?
  (lambda (def)
    (eq? (car def) 'pdef))) ;; variable definition

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

; cadr(rest) --> next statement
; car(rest) --> environment
; rest ==> (env (S1, S2, S3...,Sn))
; need to return the env to use after a block


(fcall '(fcall add (a, b)) '((sq @) (a #)) '((# 5)) '((@ (x) (pexpr * x x) ((sq @) ()))))
