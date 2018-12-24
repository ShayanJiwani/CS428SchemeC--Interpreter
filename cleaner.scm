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

(define param_vals 
  (lambda (params env store defs)
    (cond
    ((null? (cdr params))
      (list(eval_expr (car params) env store defs)))
    (else (cons (eval_expr (car params) env store defs) (param_vals (cdr params) env store defs)))
    )
  )
)

(define temp_refs
  (lambda (params)
    (cond
    ((null? (cdr params))
      (list (gensym)))
    (else (cons (gensym) (temp_refs(cdr params)))))))


(define fcall
  (lambda (exp env store defs)
    ;(display env) (newline) (newline)
    (let ((ref (cadr (assoc (cadr exp) env))))
      (let ((closure (assoc ref defs)))
        (let ((formal-params (map cadr (closure_params closure))))
          (let ((param-refs (temp_refs (caddr exp))))
            (let ((param-vals (param_vals (caddr exp) env store defs)))
              (let ((new-env (zip formal-params param-refs)))
                (let ((new-store (zip param-refs param-vals)))
                  ;(display (cons (assoc (cadr exp) (cadddr closure)) new-env)) (newline) (newline)
                  (eval_expr (closure_body closure) (cons (assoc (cadr exp) (cadddr closure)) new-env) new-store defs))))))))))

(define ref_list
    (lambda (keys lst)
      (map (lambda (key) (cadr (assoc key lst)))
           keys)))

(define zip
  (lambda (list1 list2)
    (map list list1 list2)))


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
      ;(else display def)
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

(define fdef?
  (lambda (def)
    (eq? (car def) 'fdef))) ;; function definition

(define vdef?
  (lambda (def)
    (eq? (car def) 'vdef))) ;; variable definition

(define pdef?
  (lambda (def)
    (eq? (car def) 'pdef))) ;; variable definition


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
(define block?
  (lambda (stmt)
    (eq? (car stmt) 'block)))
(define pcall?
  (lambda (stmt)
    (eq? (car stmt) 'pcall)))

(define exec_stmt
  (lambda (stmt rest env store defs)
    (cond 
      ((null? stmt) (display "here"))
      ((block? stmt)
        (let ((env-store-defs (eval_defs (cadr stmt) env store defs)))
          (exec_stmt (caaddr stmt) (append (cdaddr stmt) rest)  (car env-store-defs) (cadr env-store-defs) (caddr env-store-defs)))
      )
      ((assign? stmt)
        (let ((value-map (list (cadr stmt) (eval_expr (caddr stmt) env store defs))))
          (let ((ref (cadr (assoc (car value-map) env))))
            (let ((ref-map (assoc ref store)))
              (let ((rstore (remove ref-map store)))
                (let ((new-store (cons (list (car ref-map) (cadr value-map)) rstore)))
                  (if (not (null? rest))
                      (exec_stmt (car rest) (cdr rest) env new-store defs))))))))
      ((if1? stmt)                                                  
        (if (eval_bool (cadr stmt) env store defs)                  
          (exec_stmt (caddr stmt) rest env store defs)           
          (if (not (null? rest))                                
            (exec_stmt (car rest) (cdr rest) env store defs))))    
      ((if2? stmt)                                                
        (if (eval_bool (cadr stmt) env store defs)            
          (exec_stmt (caddr stmt) rest env store defs)        
          (exec_stmt (cadddr stmt) rest env store defs)))                   
      ((while? stmt) 
        (if (eval_bool (cadr stmt) env store defs)
            (exec_stmt (list (caaddr stmt) (car (cdaddr stmt)) (reverse (cons (reverse (cons rest (reverse stmt))) (reverse (caddr (caddr stmt)))))) rest env store defs)
            (if (not (null? stmt))                                
                (if (not (null? (cadddr stmt)))                
                  (exec_stmt (car (cadddr stmt)) (cdr (cadddr stmt)) env store defs)))))
      ((pcall? stmt)
        (pcall stmt rest env store defs))
      ((print? stmt) 
        (display (eval_expr (cadr stmt) env store defs)) (newline)
        (if (not (null? rest))                                   
            (exec_stmt (car rest) (cdr rest) env store defs)))
      (else (error 'exec_stmt ""invalid statment ~s"" stmt))
    )
  )
)

(define pcall
  (lambda (stmt rest env store defs)
    (let ((ref (cadr (assoc (cadr stmt) env))))
      (let ((closure (assoc ref defs)))
        (let ((formal-params (closure_params closure)))
          (if (not (null? formal-params))
            (let ((new-env-store (set_all_vars formal-params (caddr stmt) env store defs)))
              (exec_stmt (closure_body closure) rest (car new-env-store) (cadr new-env-store) defs))
            (exec_stmt (closure_body closure) rest env store defs))
          )))))

(define set_all_vars
  (lambda (list-of-vars passed-in-vars env store defs)
  (let ((new-env-store (determine_type (car list-of-vars) (car passed-in-vars) env store defs)))
      (if (not (null? (cdr list-of-vars))) 
        (set_all_vars (cdr list-of-vars) (cdr passed-in-vars) (car new-env-store) (cadr new-env-store) defs)
        new-env-store))))

(define determine_type
  (lambda (type pvar env store defs)
    (cond 
      ((eq? (car type) 'val)
      (let (( var-map (cons 'vdef (list (cadr type) (eval_expr pvar env store defs)))))
       (eval_defs (list var-map) env store defs)
      ))
      ((eq? (car type) 'var)
        (let ((ref (cadr (assoc pvar env))))
          (let ((new-var (list (cadr type) ref)))
            (let ((new-env (cons new-var env)))
              (list new-env store)))))
    )
  )
)
;(define ptree (quote  (prog ( (fdef ack ( (val x ) (val y ) ) (if (comp x ge 0 ) (if (comp y ge 0 ) (fcall ack ( (expr - x 1 ) (fcall ack ( x (expr - y 1 ) ) ) ) ) (fcall ack ( (expr - x 1 ) 1 ) ) ) (expr + y 1 ) ) ) (pdef main ( ) (block ( (vdef a 3 ) (vdef b 4 ) ) ( (print (fcall ack ( a b ) ) ) ) ) ) ) (pcall main ( ) ) )))

;(define ptree (quote  (prog ( (fdef gcd ( (val p ) (val q ) ) (if (comp q eq 0 ) p (fcall gcd ( q (expr % p q ) ) ) ) ) (pdef inc ( (var y ) ) (block ( ) ( (assign y (expr + y 1 ) ) ) ) ) (pdef main ( ) (block ( (vdef count 0 ) (vdef n 20 ) (vdef i 1 ) ) ( (while (comp i le n ) (block ( (vdef flag (fcall gcd ( i n ) ) ) ) ( (if1 (comp flag eq 1 ) (pcall inc ( count ) ) ) (pcall inc ( i ) ) ) ) ) (print count ) ) ) ) ) (pcall main ( ) ) )))

(define compute
  (lambda ()
    (load "scheme_ast")  ;; << This needs to be consistent
                         ;; << with the file generated by ast.exe
      (interpret ptree))) ;; eval_defs followed by exec_stmt

(define interpret
  (lambda (pt)
    (let ((env-store-defs (eval_defs (cadr pt) '() '() '())))
      (exec_stmt (caddr pt) '() (car env-store-defs) (cadr env-store-defs) (caddr env-store-defs)))))

