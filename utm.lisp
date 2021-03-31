;; an implementation of a Rogozhin (2,18) UTM


(set-irrelevant-formals ok t)
(set-ignore-ok :warn)

(defconst *alphabet*
  '(a b c d e f g h i j k l m n o p q r))

(defun utm-symp (x)
  (member x *alphabet*))

;;listof symbol
(defun utm-losp (x)
  (cond ((endp x) T)
        ((consp x) (and (utm-symp (car x))
                        (utm-losp (cdr x))))))
#|
(defun append (x y)
  (cond ((endp x) y)
        ((consp x) (cons (car x) (append (cdr x) y)))))
|#

;; needs a proof
(defun remove-n (n x)
  (if (equal n 0)
      x
      (remove-n (- 1 n) (cdr x))))


;; because rogozhin TMs "eat" tapes left to right
;; no need for a half-tape

;; a list of symbols
(defun utm-tapep (x)
  (cond ((endp x) T)
        ((consp x) (and (utm-symp (car x))
                        (utm-tapep (cdr x))))))

;; production function
;; a cons of a symp to a los-symp

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
  (cond ((endp x) 'ERROR)
        ((consp x) (if (utm-func-appliesp (car x) y)
                       (car x)
                       (utm-funcs-applicable (cdr x) y)))))




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
;; test the bits of the interpreter written
;;

;; x is a tag system
(defun utm-exec (x)
  (if (< (length (car (cdr (cdr x)))) (car x))
      (car (cdr (cdr x))) ;; tape smaller than m (or empty)
      (utm-step x)))

;; step the tape
;; find the applicable func in the tag system
;; using the first element of the tape and the set of production functions
;; append the application of that function to the end
;; delete the deletion number from the head

(defun utm-step (x)
  (remove-n (car x) (append (car (cdr (cdr x))) (utm-apply (utm-funcs-applicable (car (cdr x)) (car (car (cdr (cdr x)))))))))
