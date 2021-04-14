;; An implementation of a UTM using magic the gathering rules.

(defconst *types*
          '(aetherborn basilisk cephalid demon elf faerie giant harpy illusion
                       juggernaut kavu leviathan myr noggle orc pegasus rhino
                       silver))

(defconst *tokencolor*
          '(white green))

(defun ctypep (x)
  (member x *types*))

(defun ccolorp (x)
  (member x *tokencolor*))

(defconst *aetherborn-ex*
  '(aetherborn green 2 2))

;; example rotlung
(defconst *rotlung-1<q1*
  '(cephalid (sliver white 2 2 t)))

;; example xathrid 
;; note that (rotlungp *xathrid-b2q1*) will be true, since
;; they both produce a creature, just that xathrid produces a tapped
;; creature (transitioning the state)
(defconst *xathrid-b2q1*
  '(kavu (leviathan white 2 2 t)))

;; creature cards (or tokens) are of a type, have a color, and have
;; power and toughness, as well as if they are tapped
(defun creaturep (x)
  (and (= (length x) 4)
       (ctypep (first x))
       (ccolorp (second x))
       (natp (third x))
       (natp (fourth x))))

;; a collection of creatures, where order does not matter
(defun creaturesp (x)
  (cond ((endp x) T)
        ((consp x) (and (creaturep (first x))
                       (creaturesp (rest x))))))

;; rotlung renanimator- writes symbols when infest causes
;; the head to die 
(defun rotlungp (x)
  (and (= (length x) 4)
       (booleanp (first x))
       (ctypep (second x))
       (creaturep (third x))
       (booleanp (fourth x))))

(defun rotlungsp (x)
  (cond ((endp x) T)
        ((consp x) (and (rotlungp (first x))
                        (rotlungsp (rest x))))))

(defun applicable (x c st)
  (cond ((endp x) 'error)
        ((consp x) (if (and (equal (second (first x)) c)
                            (equal (first (first x)) st))
                     (third (first x))
                     (applicable (rest x) c st)))))


;; identifies the unique two-two creature from a list of creatures
;; essentially: finds the head of the tape
(defun findhead (x)
  (cond ((endp x) 'error)
        ((consp x) (if (and (= 2 (third (first x)))
                            (= 2 (fourth (first x))))
                     (first x)
                     (findhead (rest x))))))
  

;; move the computation
;; find the appropriate rotlung, spawn its result, killt he creature 
(defun infest (x st)
  (let ((head (findhead x)))
  (remove head (append (list (applicable x (second head) st)) x))))

;; move left or right
;; cast beam on creatures
;; color is to be moved from
(defun beam (x color)
  (cond ((endp x) nil)
        ((consp x) (if (equal color (second (first x)))
                     (cons (list (first (first x))
                                 (second (first x))
                                 (+ 2 (third (first x)))
                                 (+ 2 (fourth (first x))))
                           (beam (rest x) color))
                     (cons (first x) (beam (rest x) color))))))
                   

;; where x and y are states
(defun victoryp (x y)
  (equal x y))

;; cast snuffers, dealing -1 -1 to all
(defun snuffers (x)
  (cond ((endp x) nil)
        ((consp x) (cons (list (first (first x))
                               (second (first x))
                               (- (third (first x)) 1)
                               (- (fourth (first x)) 1))
                         (snuffers (rest x))))))

;; need to prove termination
(defun mtgi (st tape tm n)
  (declare (xargs :measure (nfix n)))
  (let ((advanced (infest (beam tape st) st)))
    (cond ((zp n) tape)
          ((victoryp advanced tape) nil)
          ((applicable tm (findhead tape) st)
           (let ((rlg (applicable tm (findhead tape) st)))
             (mtgi (fourth rlg)
                   advanced
                   tm
                   (- n 1))))
          (t nil))))
    




