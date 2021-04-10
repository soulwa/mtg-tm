;; an implementation of a Rogozhin (2,18) UTM

(defconst *alphabet*
  '(a b c d e f g h i j k l m n o p q r))
  
(defconst *even-huh-tag-sys*
  '(2 (a ()) (b (b)) (c (b c)) (c c a a a a)))

(defun utm-symp (x)
  (member x *alphabet*))

;; listof symbol
;; not programmatically different from utm-tape- but this one
;; is used for production functions
(defun utm-losp (x)
  (cond ((endp x) T)
        ((consp x) (and (utm-symp (car x))
                        (utm-losp (cdr x))))))

(defun remove-n (n x)
  (if (zp n)
      x
      (remove-n (- n 1) (cdr x))))

;; because rogozhin TMs "eat" tapes left to right
;; no need for a half-tape

;; a list of symbols
(defun utm-tapep (x)
  (cond ((endp x) T)
        ((consp x) (and (utm-symp (car x))
                        (utm-tapep (cdr x))))))

;; production function
;; a cons of a symp to a los-symp
;; 
(defun utm-funcp (x)
  (and (consp x)
       (= (length x) 2)
       (utm-symp (car x))
       (utm-losp (car (cdr x)))))

;; a list or "set" of prod-funcs
;; I don't use sets for now because they suck
(defun utm-funcsp (x)
  (cond ((endp x) T)
        ((consp x) (and (utm-funcp (car x))
                        (utm-funcsp (cdr x))))))

;; does this utm-func x apply to sym y
(defun utm-func-appliesp (x y)
  (and (utm-funcp x)
       (utm-symp y)
       (equal (car x) y)))

;; apply func
(defun utm-func-apply (x)
  (car (cdr x)))

;; return the applicable func from a set
;; assumes such a func exists
(defun utm-funcs-applicable (x y)
  (cond ((endp x) nil)
        ((consp x) (if (utm-func-appliesp (car x) y)
                       (car x)
                       (utm-funcs-applicable (cdr x) y)))))

;; a tag system has a tape,
;; deletion number, and prod. function
;; Alphabet is included in the defintion of tape
;; '(,deletion-number ,prod-funcs ,tape)
(defun tag-sysp (x)
  (and (consp x)
       (equal (length x) 3)
       (natp (first x))
       (utm-funcsp (second x))
       (utm-tapep (third x))))

;; step the tape
;; find the applicable func in the tag system
;; using the first element of the tape and the set of production functions
;; append the application of that function to the end
;; delete the deletion number from the head
(defun utm-step (m funcs tape)
  (remove-n m (append tape
                      (utm-func-apply
               		  (utm-funcs-applicable funcs (car tape))))))

(defun utm-exec (m funcs tape n)
  (declare (xargs :measure (nfix n)))
  (cond
   ;; both of these are termination conditions
   ((< (length tape) m) tape)
   ((zp n) `(nil ,tape))
   (t (utm-exec m funcs (utm-step m funcs tape) (- n 1)))))

;; wrapper for utm-exec, which works on a tag system construct
(defun utm-tag-sys-exec (tag-sys n)
  (utm-exec (first tag-sys)
            (second tag-sys)
            (third tag-sys)
            n))
