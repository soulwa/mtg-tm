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

;; example creature
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
       (natp (fourth x))
       (boolp (fifth x))))

;; a collection of creatures, where order does not matter
(defun creaturesp (x)
  (cond ((endp x) T)
        ((cons x) (and (creaturep (first x))
                       (creaturesp (rest x))))))
                       
;; our functions are done with an Artificial Evolution'd, Glamerdye'd Rotlung Reanimator
;; or xathrid necromancer

;; thus a rr has an input creature (the one that dies, of any color)
;; and an output creature with some color
(defun rotlungp (x)
  (and (= (length x) 2)
       (ctypep (first x))
       (creaturep (second x))
       (boolp (third x))))

;; list of rotlung, list of functions
(defun rotlungsp (x)
  (cond ((endp x) T)
        ((consp x) (and (rotlungp (first x))
                        (rotlungsp (rest x))))))

;; find the rotlung applicable to some creature
(defun applicable (x c)
  (cond ((endp x) 'error)
        ((consp x) (if (equal (first (first x)) c)
                     (second (first x))
                     (applicable (rest x) c)))))


;; identifies the unique two-two creature from a list of creatures
;; essentially: finds the head of the tape
(defun two-two (x)
  (cond ((endp x) 'error)
        ((consp x) (if (and (= 2 (third (first x)))
                            (= 2 (fourth (first x))))
                     (first x)
                     (two-two (rest x))))))
  

;; move the computation
;; find the appropriate rotlung, spawn its result, killt he creature 
(defun infest (x)
  (let ((head (two-two x)))
  (remove head (append (list (applicable x head)) x))))

