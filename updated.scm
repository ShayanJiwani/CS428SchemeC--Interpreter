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
        (< (eval_expr (bsubexp1 exp) env store defs)
        (eval_expr (bsubexp2 exp) env store defs)))
      ((lessThanEqual exp)
        (<= (eval_expr (bsubexp1 exp) env store defs)
        (eval_expr (bsubexp2 exp) env store defs)))
      ((equalTo exp)
        (= (eval_expr (bsubexp1 exp) env store defs)
        (eval_expr (bsubexp2 exp) env store defs)))
      ((greaterThan exp)
        (> (eval_expr (bsubexp1 exp) env store defs)
        (eval_expr (bsubexp2 exp) env store defs)))
      ((greaterThanEqual exp)
        (>= (eval_expr (bsubexp1 exp) env store defs)
        (eval_expr (bsubexp2 exp) env store defs)))
      ((notEqual exp)
        (not (= (eval_expr (bsubexp1 exp) env store defs)
        (eval_expr (bsubexp2 exp) env store defs))))
      (else (error 'eval_expr ""invalid expression ~s"" exp)))))

(define bsubexp1
  (lambda (exp) (cadr exp)))

(define bsubexp2
  (lambda (exp) (cadddr exp)))

 (define lessThan
  (lambda (exp)
    (eq? (caddr exp) 'le)))

 (define lessThanEqual
  (lambda (exp)
    (eq? (caddr exp) 'leq)))

 (define equalTo
  (lambda (exp)
    (eq? (caddr exp) 'eq)))

 (define greaterThan
  (lambda (exp)
    (eq? (caddr exp) 'ge)))

 (define greaterThanEqual
  (lambda (exp)
     (eq? (caddr exp) 'geq)))

 (define notEqual
  (lambda (exp)
     (eq? (caddr exp) 'neq)))


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
    (eq? (car x) 'print)))
;(define break?
;  (lambda (x)
;    (eq? (car x) 'break)))
(define block?
  (lambda (stmt)
    (eq? (car stmt) 'block)))
(define pcall?
  (lambda (stmt)
    (eq? (car stmt) 'pcall)))
;; EXEC MAIN STATEMENT
;; EXEC MAIN STATEMENT
(define exec_stmt
  (lambda (stmt rest env store defs)
    (cond 
      ((null? stmt)
      (display ""))
      ((block? stmt)
        (let ((env-store-defs (eval_defs (cadr stmt) env store defs)))
          (exec_stmt (caaddr stmt) (cdaddr stmt) (append env (car env-store-defs)) (append store (cadr env-store-defs)) (append defs (caddr env-store-defs))) 
        )
      )
      ((assign? stmt)
        (let ((value-map (list (cadr stmt) (eval_expr (caddr stmt) env store defs))))
          (let ((ref (cadr (assoc (car value-map) env))))
            (let ((ref-map (assoc ref store)))
              (let ((rstore (remove ref-map store)))
                (let ((new-store (cons (list (car ref-map) (cadr value-map)) rstore)))
                  (if (null? rest)
                      (exec_stmt '() rest env new-store defs)
                      (exec_stmt (car rest) (cdr rest) env new-store defs))))))))
      ((if1? stmt)                                                  ;; just if stmt
        (if (eval_bool (cadr stmt) env store defs)                  ;;bool
          (exec_stmt (caddr stmt) rest env store defs)              ;; exp
          (if (null? rest)                                 ;; make sure more things left
            (exec_stmt '() rest env store defs)                     ;; end
            (exec_stmt (car rest) (cdr rest) env store defs))))     ;;continue
      ((if2? stmt)                                                  ;; if/else stmt
        (if (eval_bool (cadr stmt) env store defs)                  ;; bool exp
          (exec_stmt (caddr stmt) rest env store defs)              ;; exp 1
          (exec_stmt (cadddr stmt) rest env store defs)))                   
      ((while? stmt) 
        (if (eval_bool (cadr stmt) env store defs)
            (exec_stmt (list (caaddr stmt) (car (cdaddr stmt)) (reverse (cons (reverse (cons rest (reverse stmt))) (reverse (caddr (caddr stmt)))))) rest env store defs)
        (if (not (null? stmt))                                
            (if (not (null? (cadddr stmt)))                
              (exec_stmt (car (cadddr stmt)) (cdr (cadddr stmt)) env store defs)))))
      ((pcall? stmt)
        (display "pcall"))
      ((print? stmt) 
        (display (eval_expr (cadr stmt) env store defs)) (newline)
        (if (null? rest)                                 ;; make sure more things left
            (exec_stmt '() rest env store defs)                     ;; end
            (exec_stmt (car rest) (cdr rest) env store defs)))
      (else (error 'exec_stmt ""invalid statment ~s"" stmt))
    )
  )
)
;pdef p ( (val x ) (var result ) )

;; pdef can either have a literal value called val
;; or we have to use the reference when it's var
;; so basically, we have to go through each of the params and see if its val or var 
(define pcall
  (lambda (stmt rest env store defs)
  ;; this ref gets the pdef from the env
    (let ((ref (cadr (assoc (cadr stmt) env))))
    ;; this gets the body of the procedure
      (let ((closure (assoc ref defs)))
        ; this section is where we add any params to our new env
        ; we have to go through each param and see if its val or var
        ; if its var, then we use pass by reference, and have them set to the same
        ; reference as the variable we are passing in, so we can edit its value
        ; if its val, then we just do what we did with fcall, which is below
        ; a procedure doesn't have to have parameters (main is an example)
        ; start section
        (let ((formal-params (closure_params closure)))
        ; only do param refs if we have variables to add
        ; what we could do is have a function that maps all the variables in the param
        ; list to some function
        ; meaning: map getref (*list*)
        ; the getref function that we do to every member of the list is just a function
        ; that has a conditional whether lambda is val or var
        ; if it's val do something, if var do another
        ; the map will properly store etc.
          (if (not (null? formal-params))
            (let ((param-refs (temp_refs (caddr exp))))
              (let ((param-vals (param_vals (caddr exp) env store defs)))
                (let ((new-env (zip formal-params param-refs)))
                ; end section
                  (let ((new-store (zip param-refs param-vals)))
                    (eval_expr (closure_body closure) new-env new-store defs))))))

                  )))))


(define map-all-vars
  (lambda (list-of-vars)
    (map (determine_type (lambda (var) (car var))) list-of-vars)
  )
)

(define determine_type
  (lambda type)
    (cond 
      ((eq? (car type) 'val)
      ;; do fcall type stuff
        
      )
      ((eq? (car type) 'var)
        ;; get the reference and store it
      )
  )


; cadr(rest) --> next statement
; car(rest) --> environment
; rest ==> (env (S1, S2, S3...,Sn))
; need to return the env to use after a block


;(fcall '(fcall add (a b)) '((add @) (a #) (b $)) '((# 5) ($ 2)) '((@ (x y) (pexpr + x y) ((add @) ()))))\



;(block ( ) ((assign sum (expr + sum i)) (assign i (expr - i 1))))



;((assign? stmt)
;  (let value-map (list (cadr stmt) (eval_expr (caddr stmt) env store defs))
;    (let ref (cadr (assoc (car value-map) env))
;      (let ref-map (assoc ref store)
;        (let rstore (remove ref-map store)
;          (let new-store (cons (list (car ref-map) (cadr value-map)) rstore)
;            (if (null? rest)
;                (exec_stmt '() rest env new-store defs)
;                (exec_stmt (car rest) (cdr rest) env new-store defs)))))))
;)

;(list (cadr '(assign sum (expr + sum i))) (eval_expr (caddr '(assign sum (expr + sum i))) '((sum @) (i #)) '((@ 1) (# 2)) '()))


;; say we have '((@ 4) (# 2) ($ 5))

;; updating @

;; we want '((@ 10) (# 2) ($ 5))


;; we pull out '(@ 4)

;(cons (list (car '(@ 1)) (cadr '(sum 3))) '((# 2)))

;(exec_stmt '(assign sum (expr + sum i)) '() '((sum @) (i #)) '((@ 3) (# 2)) '()) 
;(exec_stmt '(print "end") '() '() '() '())
;(exec_stmt '(print (pexpr + 2 3)) '((print (pexpr * 2 3)) (print (pexpr / 10 2))) '() '() '())

;(exec_stmt '(if1 (comp le 2 3) (display "hi")) '() '() '() '())

;(exec_stmt '(assign sum (expr + sum i)) '() '((sum @) (i #)) '((@ 3) (# 2)) '()) 


;(exec_stmt '(assign sum (expr + sum i)) '((print (expr + sum i))) '((sum @) (i #)) '((@ 3) (# 2)) '())

;(exec_stmt '(block ((vdef x 1) (vdef y (expr + x 1 ))) ((if2 (comp x ge y) (print x) (print y)))) '() '() '() '())


;(exec_stmt '(if2 (comp x ge y) (print x) (print y)) '() '((x @) (y #)) '((@ 2) (# 3)) '() )


;(while (comp i ge 0) (block ( ) ((assign sum (expr + sum i)) (assign i (expr - i 1))))) 

;(exec_stmt '(while (comp i ge 0) (block ( ) ((assign sum (expr + sum i)) (assign i (expr - i 1))))) '() '((i @) (sum #)) '((@ 2) (# 3)) '())


;(exec_stmt '(block ((vdef x 1) (vdef y (expr + x 1 ))) ((if2 (comp x ge y) (print x) (print y)))) '((print (pexpr * 2 3))) '() '() '())

;; while --> add while stmt to end of block statements --> do block --> reach while --> repeat

;'(while (comp i ge 0) (block ( ) ((assign sum (expr + sum i)) (assign i (expr - i 1)))))
;'((assign i (expr - i 1)))

;(reverse (cons '(while (comp i ge 0) (block ( ) ((assign sum (expr + sum i)) (assign i ;(expr - i 1))))) '((assign i (expr - i 1)))))

;(reverse (cons '(x y z) (reverse '((a b c) (d e f)))))


;(exec_stmt '(while (comp i ge 0) (block ( ) ((assign sum (expr + sum i)) (assign i (expr - i 1))))) '((print sum)) '((i @) (sum #)) '((@ 2) (# 3)) '())

;(list (caaddr stmt) (car (cdaddr stmt)) (reverse (cons stmt (reverse (caddr (caddr stmt))))))

;(reverse (cons rest (reverse stmt)))


;(reverse (cons '((print sum) (print i)) (reverse '(while (comp i ge 0) (block ( ) ((assign sum (expr + sum i)) (assign i (expr - i 1))))))))


;'(block ((vdef sum 0 ) (vdef i 5)) ((while (comp i ge 0) (block ( ) ((assign sum (expr + sum i)) (assign i (expr - i 1))))) (print sum)))

;(exec_stmt '(block ((vdef sum 0 ) (vdef i 5)) ((while (comp i ge 0) (block ( ) ((assign sum (expr + sum i)) (assign i (expr - i 1))))) (print sum))) '() '() '() '())


;(pdef main ( ) (block ((vdef sum 0 ) (vdef i 5)) ((while (comp i ge 0) (block ( ) ((assign sum (expr + ;sum i)) (assign i (expr - i 1)))))
;(print sum))))
