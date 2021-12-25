(define map
  (let ((null? null?)
	(car car) (cdr cdr)
	(cons cons) (apply apply))
  (letrec ((map-many
	    (lambda (f lists)
	      (if (null? (car lists))
		  '()
		  (cons
		   (apply f (map-one car lists))
		   (map-many f (map-one cdr lists))))))
	   (map-one
	    (lambda (f s)
	      (if (null? s)
		  '()
		  (cons (f (car s))
			(map-one f (cdr s)))))))
    (lambda (f . args)
      (map-many f args)))))


(define fold-left 
  #;(Add your implementation here
     Note: The file won't compile like this, beacuase your tag-parser requires define to have a second expression.
     This is on purpose, so you don't compile the library without completing this implementation by mistake.))

(define fold-right
  #;(Add your implementation here
     Note: The file won't compile like this, beacuase your tag-parser requires define to have a second expression.
     This is on purpose, so you don't compile the library without completing this implementation by mistake.))

(define cons*
  #;(Add your implementation here
     Note: The file won't compile like this, beacuase your tag-parser requires define to have a second expression.
     This is on purpose, so you don't compile the library without completing this implementation by mistake.))

(define append
  (let ((null? null?)
	(fold-right fold-right)
	(cons cons))
    (lambda args
      (fold-right
       (lambda (e a)
	 (if (null? a)
	     e
	     (fold-right cons a e)))
       '() args))))

(define list (lambda x x))

(define list? 
  (let ((null? null?)
	(pair? pair?)
	(cdr cdr))
    (letrec ((list?-loop
	      (lambda (x)
		(or (null? x)
		    (and (pair? x)
			 (list?-loop (cdr x)))))))
      list?-loop)))

(define make-string
  (let ((null? null?) (car car)
	(make-string make-string))
    (lambda (x . y)
      (if (null? y)
	  (make-string x #\nul)
	  (make-string x (car y))))))

(define not
  (lambda (x) (if x #f #t)))

(let ((flonum? flonum?) (rational? rational?)
      (exact->inexact exact->inexact)
      (fold-left fold-left) (map map)
      (_+ +) (_* *) (_/ /) (_= =) (_< <)
      (car car) (cdr cdr) (null? null?))
  (let ((^numeric-op-dispatcher
	 (lambda (op)
	   (lambda (x y)
	     (cond
	      ((and (flonum? x) (rational? y)) (op x (exact->inexact y)))
	      ((and (rational? x) (flonum? y)) (op (exact->inexact x) y))
	      (else (op x y)))))))
      (set! + (lambda x (fold-left (^numeric-op-dispatcher _+) 0 x)))
      (set! * (lambda x (fold-left (^numeric-op-dispatcher _*) 1 x)))
      (set! / (let ((/ (^numeric-op-dispatcher _/)))
		(lambda (x . y)
		  (if (null? y)
		      (/ 1 x)
		      (fold-left / x y)))))
    (let ((^comparator
	   (lambda (op)
	     (letrec ((comparator
		       (lambda (x ys)
			 (or (null? ys)
			     (and (op x (car ys))
				  (comparator (car ys) (cdr ys)))))))
	       (lambda (x . y)
		 (comparator x y))))))
      (set! = (^comparator (^numeric-op-dispatcher _=)))
      (set! < (^comparator (^numeric-op-dispatcher _<))))))

(define -
  (let ((apply apply)
	(+ +)
	(null? null?))
    (lambda (x . y)
      (if (null? y)
	  (+ 0 (* -1 x))
	  (+ x (* -1 (apply + y)))))))

(define >
  (let ((null? null?) (not not)
        (car car) (cdr cdr)
        (< <) (= =))
    (letrec ((>-loop
	      (lambda (x ys)
	        (or (null? ys)
		    (and (not (< x (car ys))) (not (= x (car ys)))
		         (>-loop (car ys) (cdr ys)))))))
      (lambda (x . y)
        (>-loop x y)))))

(define gcd
  (let ((gcd gcd) (null? null?)
	(car car) (cdr cdr))
    (letrec ((gcd-loop
	      (lambda (x ys)
		(if (null? ys)
		    x
		    (gcd-loop (gcd x (car ys)) (cdr ys))))))
      (lambda x
	(if (null? x)
	    0
	    (gcd-loop (car x) (cdr x)))))))

(define zero? 
  (let ((= =))
    (lambda (x) (= x 0))))

(define integer?
  (let ((rational? rational?)
	(= =)
	(denominator denominator))
    (lambda (x)
      (and (rational? x) (= (denominator x) 1)))))

(define number?
  (let ((flonum? flonum?)
	(rational? rational?))
    (lambda (x)
      (or (flonum? x) (rational? x)))))

(define length
  (let ((fold-left fold-left)
	(+ +))
    (lambda (l)
      (fold-left (lambda (acc e) (+ acc 1)) 0 l))))

(define string->list
  (let ((string-ref string-ref)
	(string-length string-length)
	(< <) (- -) (cons cons))
    (lambda (s)
      (letrec
	  ((s->l-loop
	    (lambda (n a)
	      (if (< n 0)
		  a
		  (s->l-loop (- n 1) (cons (string-ref s n) a))))))
	(s->l-loop (- (string-length s) 1) '())))))

(define equal?
  (let ((= =) (string->list string->list)
	(rational? rational?) (flonum? flonum?)
	(pair? pair?) (char? char?)
	(string? string?) (eq? eq?)
	(car car) (cdr cdr)
	(char->integer char->integer))
    (letrec ((equal?-loop
	      (lambda (x y)
		(cond
		 ((and (rational? x) (rational? y)) (= x y))
		 ((and (flonum? x) (flonum? y)) (= x y))
		 ((and (char? x) (char? y)) (= (char->integer x) (char->integer y)))
		 ((and (pair? x) (pair? y))
		  (and (equal?-loop (car x) (car y)) (equal?-loop (cdr x) (cdr y))))
		 ((and (string? x) (string? y)) (equal?-loop (string->list x) (string->list y)))
		 (else (eq? x y))))))
    equal?-loop)))
