#lang racket

(provide (all-defined-out))

(define-struct gnode (sym list) #:transparent)
(define-struct ident (str) #:transparent)
(define-struct num (val) #:transparent)

(define (combine-cc char1 char2)
(list->string (list char1 char2)))
(define (combine-sc str char)
(list->string (append (string->list str)
(list char))))
(define (combine-cs char str)
(list->string (cons char (string->list str))))
(define (combine-ss str1 str2)
(list->string (append (string->list str1)
(string->list str2))))

(define null-checker
  (lambda (str)
    (let ((l (string-length str)))
       (equal? str (make-string l #\space)))))

(define (helper str c1 c2)
  (define (joker a b d e)
    (cond ((>= a (string-length str)) 'not-applicable)
          ((equal? (string-ref str a) c1) (if (= d 0) (joker (+ a 1) a (+ d 1) e) (joker (+ a 1) b (+ d 1) e)))
          ((equal? (string-ref str a) c2) (if (= (- d e) 1) (cons b a) (joker (+ a 1) b d (+ e 1))))
          (else (joker (+ a 1) b d e))))
  (joker 0 0 0 0))

(define (pred-p p)
  (lambda (str)
    (if (null-checker str) 'fail
    (let ((x (string-ref str 0)))
                  (if (p x) (cons x (substring str 1 (string-length str))) 'fail)))))

(define single-digit-p (lambda (str) ((pred-p (lambda (c) (char-numeric? c))) str)))

(define single-alphabet-p (lambda (str) ((pred-p (lambda (c) (char-alphabetic? c))) str)))

(define (seq p1 p2 f)
  (lambda (str) (let ((substr (substring str 1 (string-length str))))
                  (cond ((or (equal? (p1 str) 'fail) (equal? (p2 substr) 'fail)) 'fail)
                        (else (cons (combine-cc (car (p1 str)) (car (p2 substr))) (cdr (p2 substr))))))))
                        
(define (alt p1 p2) (lambda (str) (if (not (equal? (p1 str) 'fail)) (p1 str) (p2 str))))

(define epsilon-p (lambda (str) (cons "" str)))

(define (zero-or-more p f)
  (lambda (str)
    (if (and (= (string-length str) 1) (not (equal? (p str) 'fail))) (cons str "")  
     (let ((substr (substring str 1 (string-length str))))
        (cond ((equal? (p str) 'fail) (epsilon-p str))
              ((equal? (p substr) 'fail) (cons (f (string-ref str 0) (car (epsilon-p substr))) (cdr (epsilon-p substr))))
              (else (cons (f (string-ref str 0) (car ((zero-or-more p f) substr))) (cdr ((zero-or-more p f) substr)))))))))

(define (one-or-more p f)
  (lambda (str)
    (if (equal? 'fail (p str)) 'fail
        ((zero-or-more p f) str))))
    
(define whitespace-p
  (lambda (str)
    (if (null-checker str) (cons "" "") 
      (let ((substr (substring str 1 (string-length str))))
         (if (char-whitespace? (string-ref str 0)) (whitespace-p substr) (cons "" str))))))

(define number-p
  (lambda (str)
   (let* ((joker ((one-or-more single-digit-p combine-cs) (cdr (whitespace-p str)))))
     (if (equal? 'fail joker) 'fail
         (cons (num (string->number (car joker))) (cdr joker))))))
                 
(define identifier-p
  (lambda (str)
    (let ((wstr (cdr (whitespace-p str))))
      (cond ((null-checker wstr) 'fail)
            ((not (char-alphabetic? (string-ref wstr 0))) 'fail)
            (else (let ((joker ((zero-or-more (alt single-alphabet-p single-digit-p) combine-cs) wstr)))
                    (cons (ident (car joker)) (cdr joker))))))))
        
(define variable-p
  (λ (str)
    (let ((wstr (cdr (whitespace-p str))))
      (cond ((null-checker wstr) 'fail)
            ((equal? 'fail (identifier-p wstr)) 'fail)
            ((equal? "" (cdr (identifier-p wstr))) (identifier-p wstr))
            (else (let ((joker (cdr (whitespace-p (cdr (identifier-p wstr))))))                       
                    (if (not (equal? #\[ (string-ref joker 0))) (identifier-p wstr)
                        (if (equal? 'not-applicable (helper joker #\[ #\])) (identifier-p wstr)
                            (let* ((pair (helper joker #\[ #\]))
                                   (string_for_exp (substring joker (+ (car pair) 1) (cdr pair)))
                                   (string_for_cdr (substring joker (+ (cdr pair) 1) (string-length joker)))
                                   (ident_part (car (identifier-p wstr))))
                              (if (equal? 'fail (expression-p string_for_exp)) (identifier-p wstr)
                                  (cons (gnode 'ARRAY (list ident_part (expression-p string_for_exp))) string_for_cdr)))))))))))

(define term-p
  (λ (str)
    (let ((wstr (cdr (whitespace-p str))))
      (cond ((not (equal? 'fail (number-p wstr))) (number-p wstr))
            ((not (equal? 'fail (variable-p wstr))) (variable-p wstr))
            ((not (equal? #\( (string-ref wstr 0))) 'fail)
            ((equal? 'not-applicable (helper wstr #\( #\))) 'fail)
            (else (let* ((pair (helper wstr #\( #\)))
                        (expression_part (substring wstr (+ (car pair) 1) (cdr pair)))
                        (after_part (substring wstr (+ (cdr pair) 1) (string-length wstr))))
                    (if (equal? 'fail (expression-p expression_part)) 'fail
                             (if (equal? "" after_part) (expression-p expression_part)
                                 (cons (car (expression-p expression_part)) after_part)))))))))

(define expression-p
 (λ (str)
   (let ((wstr (cdr (whitespace-p str))))
     (cond ((equal? 'fail (term-p wstr)) 'fail)
           (else (let* ((t_parser (term-p wstr))
                       (w1str (cdr (whitespace-p (cdr t_parser)))))
                   (if (equal? w1str "") t_parser 
                       (if (not (equal? #\+ (string-ref w1str 0))) t_parser
                           (let ((joker (expression-p (substring w1str 1))))
                             (if (equal? 'fail joker) t_parser
                                 (cons (gnode 'PLUS (list (car t_parser) (car joker))) (cdr joker))))))))))))

(define assignment-p
  (λ (str)
    (let* ((wstr (cdr (whitespace-p str))))
      (if (equal? (variable-p wstr) 'fail) 'fail
          (let ((joker (cdr (whitespace-p (cdr (variable-p wstr))))))
            (if (equal? #\= (string-ref joker 0))
                (let* ((w1str (expression-p (substring joker 1 (string-length joker)))))
                  (if (equal? 'fail w1str) 'fail
                      (cons (gnode 'ASSIGN (list (car (variable-p wstr)) (car w1str))) (cdr w1str))))
                'fail))))))
                      
                  
                
          
    
     
                        
                        
      
                              
                                  
                             
                              
                            
                            
                        
                    
        

     
          
          
          
          
    
    
         
            
          
            
            
            
      
      
     
    
    
    
      
  
                                                                    
                                     
                                      
                               
                            
                             
                                             
                                              
                                          