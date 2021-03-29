;; an implementation of a Rogozhin (2,18) UTM
;;
(defconst *alphabet*
  '(a b c d e f g h i j k l m n o p q r))

(defun utm-symp (x)
  (member x *alphabet*))

;; because rogozhin TMs "eat" tapes left to right
;; no need for a half-tape

;; a list of symbols
(defun utm-tapep (x)
  (cond ((endp x) T)
        ((consp x) (and (utm-symp (car x))
                        (utm-tapep (cdr x))))))

;; production function
;; a cons of two utm-symps, left is input
;; we don't lambda-ize cuz A CL2

(defun utm-funcp (x)
  (and (consp x)
       (= (length x) 2)
       (utm-symp (car x))
       (utm-symp (car (cdr x)))))

;; a list or "set" of prod-funcs
;; I don't use sets for now because they suck
(defun utm-funcsp (x)
  (cond ((endp x) T)
        ((consp x) (and (utm-funcp (car x))
                        (utm-funcsp (cdr x))))))

;; a tag system has a tape,
;; deletion number, and prod. function
;; Alphabet is included in the defintion of tape
(defun tag-sysp (x)
  (and (consp x)
       (equal (length x) 3)
       (natp (car x))
       (utm-funcsp (car (cdr x)))
       (utm-tapep (car (cdr (cdr x))))))

;; TODO:
;; write the interpreter
