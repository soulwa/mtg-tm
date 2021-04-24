;; An ACL2 implementation of an MTGTM.
;; https://arxiv.org/abs/1904.09828 
(include-book "kestrel/lists-light/perm" :dir :system)
(include-book "kestrel/lists-light/memberp" :dir :system)

;; MTGTM creature types.
(defconst *types*
          '(aetherborn basilisk cephalid demon elf faerie giant harpy illusion
                       juggernaut kavu leviathan myr noggle orc pegasus rhino
                       sliver assassin))

;; MTGTM tape-creature colors
(defconst *tokencolor*
          '(white green blue))

;; MTGTM creature type recognizer
(defun ctypep (x)
  (memberp x *types*))

;; MTGTM creature color recognizer
(defun ccolorp (x)
  (memberp x *tokencolor*))

;; Example tape-creature
(defconst *aetherborn-ex*
  '(aetherborn green 2 2))

;; example rotlung
(defconst *rotlung-1<q1*
  `(T cephalid ,*aetherborn-ex* T))

;; example xathrid 
;; note that (rotlungp *xathrid-b2q1*) will be true, since
;; they both produce a creature, just that xathrid produces a tapped
;; creature (transitioning the state)
(defconst *xathrid-b2q1*
  '(nil kavu (leviathan white 2 2) nil))

(defconst *rogozhin-rotlungs*
  ;; state | death proc | new head | new state
  '((t aetherborn (sliver white 2 2)     t)
    (t basilisk   (elf green 2 2)        t)
    (t cephalid   (sliver white 2 2)     t)
    (t demon      (aetherborn green 2 2) t)
    (t elf        (demon white 2 2)      t)
    (t faerie     (harpy green 2 2)      t)
    (t giant      (juggernaut green 2 2) t)
    (t harpy      (faerie white 2 2)     t)
    (t illusion   (faerie green 2 2)     t)
    (t juggernaut (illusion white 2 2)   t)
    (t kavu       (leviathan white 2 2)  nil)
    (t leviathan  (illusion white 2 2)   nil)
    (t myr        (basilisk white 2 2)   nil)
    (t noggle     (orc green 2 2)        t)
    (t orc        (pegasus white 2 2)    t)
    (t pegasus    (rhino green 2 2)      nil)
    (t rhino      (assassin blue 2 2)    t) ;; HALT
    (t sliver     (cephalid green 2 2)   t)

    (nil aetherborn (cephalid green 2 2)   nil)
    (nil basilisk   (cephalid green 2 2)   nil)
    (nil cephalid   (basilisk white 2 2)   nil)
    (nil demon      (elf green 2 2)        nil)
    (nil elf        (aetherborn white 2 2) nil)
    (nil faerie     (kavu green 2 2)       t)
    (nil giant      (harpy green 2 2)      nil)
    (nil harpy      (giant white 2 2)      nil)
    (nil illusion   (juggernaut green 2 2) nil)
    (nil juggernaut (giant white 2 2)      nil)
    (nil kavu       (faerie green 2 2)     t)
    (nil leviathan  (juggernaut green 2 2) nil)
    (nil myr        (orc green 2 2)        nil)
    (nil noggle     (orc green 2 2)        nil)
    (nil orc        (noggle white 2 2)     nil)
    (nil pegasus    (sliver green 2 2)     nil)
    (nil rhino      (sliver white 2 2)     t)
    (nil sliver     (myr white 2 2)        nil)))


;; creature cards (or tokens) are of a type, have a color,
;; and have power and toughness
(defun creaturep (x)
  (and (= (length x) 4)
       (ctypep (first x))
       (ccolorp (second x))
       (natp (third x))
       (>= (third x) 2)
       (natp (fourth x))
       (>= (fourth x) 2)
       (= (third x) (fourth x))))

;; a collection of creatures, where order does not matter
(defun creaturesp (x)
  (cond ((endp x) T)
        ((consp x) (and (creaturep (first x))
                        (creaturesp (rest x))))))

;; rotlung renanimator- writes symbols when infest causes
;; the head to die 
;; two booleans represent state
(defun rotlungp (x)
  (and (= (length x) 4)
       (booleanp (first x))
       (ctypep (second x))
       (creaturep (third x))
       (booleanp (fourth x))))

;; list of rotlungs
(defun rotlungsp (x)
  (cond ((endp x) T)
        ((consp x) (and (rotlungp (first x))
                        (rotlungsp (rest x))))))

;; find the rotlung appropriate rotlung given state and type
(defun find-rotlung (tm typ st)
  (cond ((endp tm) nil)
        ((consp tm) (if (and (equal (first (first tm)) st)
                             (equal (second (first tm)) typ))
                     (first tm)
                     (find-rotlung (rest tm) typ st)))))

;; finds the head creature, of power and toughness 2/2
(defun find-head (tape)
  (cond ((endp tape) '(cephalid white 2 2))
        ((consp tape) (if (and (= 2 (third (first tape)))
                               (= 2 (fourth (first tape))))
                        (first tape)
                        (find-head (rest tape))))))

;; simple remove
(defun infest (head tape)
  (remove-equal head tape))
    
;; cast beam on creatures
(defun vigor-beam (tape color)
  (cond ((endp tape) nil)
        ((consp tape) (if (equal color (second (first tape)))
                          (cons (list (first (first tape))
                                      (second (first tape))
                                      (+ 2 (third (first tape)))
                                      (+ 2 (fourth (first tape))))
                                (vigor-beam (rest tape) color))
                          (cons (first tape) (vigor-beam (rest tape) color))))))

;; cast snuffers, dealing -1 -1 to all
(defun snuffers (tape)
  (cond ((endp tape) nil)
        ((consp tape) (cons (list (first (first tape))
                               (second (first tape))
                               (- (third (first tape)) 1)
                               (- (fourth (first tape)) 1))
                         (snuffers (rest tape))))))

;; combine vigor, beam, and snuffers to move in the desired direction
(defun move (tape color)
  (snuffers (vigor-beam tape color)))

(defun new-tape (new-head head tape)
  (let* ((color (second new-head))
         (headless (infest head tape)))
    (case color
      ('white (move (cons new-head headless) color))
      ('green (move (cons new-head headless) color))
      (t (cons new-head headless)))))

(defun mtgi (st tape tm n)
  (declare (xargs :measure (nfix n)))
  (let* ((head (find-head tape))
         (rlg  (find-rotlung tm (first head) st)))
    (cond 
      ;; done taking steps (doesn't halt in n steps)
      ((zp n) tape)
      ;; produce a new tape with the given production function
      (rlg (mtgi (fourth rlg) (new-tape (third rlg) head tape) tm (- n 1)))
      ;; halted in n steps (no prod. function available, must be halt symbol)
      (t tape))))

(defconst *name-map* 
  '((aetherborn a)
    (basilisk b)
    (cephalid c)
    (demon d)
    (elf e)
    (faerie f)
    (giant g)
    (harpy h)
    (illusion i)
    (juggernaut j)
    (kavu k)
    (leviathan l)
    (myr m)
    (noggle n)
    (orc o)
    (pegasus p)
    (rhino r)
    (sliver s)
    (assassin HALT)))

(defun map-creature-name (creature)
  (second (assoc (first creature) *name-map*)))

(defun map-creature-names (creatures)
  (cond
    ((endp creatures) nil)
    ((consp creatures) (cons (map-creature-name (first creatures))
                             (map-creature-names (rest creatures))))))

;; order this mtg tape 
;; first, we need to define insert: by modelling
;; this similarly to insertion sort, we can prove some fairly
;; simple lemmas easily

;; defines the greater than relation on creatures
(defun le-creature (c1 c2)
  (<= (fourth c1) (fourth c2)))

(defthm le-creature-relation
  (and
    (booleanp (le-creature c1 c2))
    (le-creature c1 c1)
    (implies (and (le-creature c1 c2) (le-creature c2 c3))
             (le-creature c1 c3))))

;; inserts a creature into a list of creatures
(defun insert-creature (creature l) 
  (cond
    ((endp l) (cons creature l))
    ((le-creature creature (first l)) (cons creature l))
    (t (cons (first l) (insert-creature creature (rest l))))))

;; sort a list of creatures
(defun isort-creatures (creatures)
  (cond
    ((endp creatures) nil)
    ((consp creatures)
     (insert-creature (first creatures)
                      (isort-creatures (rest creatures))))))

;; we then need the different "pieces" of the tape, so we 
;; can model the pieces of the half tape

;; this gets us the right side of the half tape
;; we include the head, regardless of color
(defun white-creatures (creatures)
  (cond
    ((endp creatures) nil)
    ((consp creatures)
      (if (or 
            (equal 'white (second (first creatures)))
            (equal 2 (fourth (first creatures))))
          (cons (first creatures) (white-creatures (rest creatures)))
          (white-creatures (rest creatures))))))

;; this gets us the left side of the half tape
;; we exclude the head, regardless of color
;; need to get halt symbol, as well... decide on that behavior
(defun green-creatures (creatures)
  (cond
    ((endp creatures) nil)
    ((consp creatures)
      (if (and 
            (equal 'green (second (first creatures)))
            (not (equal 2 (fourth (first creatures)))))
          (cons (first creatures) (green-creatures (rest creatures)))
          (green-creatures (rest creatures))))))

;; we are now equipped to transform an arbitrary battlefield of creatures
;; into the structure of an mtg tape, preserving the ordering information for now
(defun battlefield-to-tape (creatures)
  (cons (isort-creatures (white-creatures creatures))
        (isort-creatures (green-creatures creatures))))

;; shows that some tape contains a card of a given 
;; Listof Creature Nat -> Boolean
(defun contains-toughness-and-color (creatures hp color)
  (cond
    ((endp creatures) nil)
    ((consp creatures) (or (and
                            (equal hp (third (first creatures)))
                            (equal color (second (first creatures)))
                           (contains-toughness-and-color (rest creatures) hp color))))))

;; we need to prove some lemmas to show that we don't need mtgi anymore
;; notably, we will need to show a new function operating on creatures, 
;; mtgi-ord, will produce the same output as mtgi. interestingly, we don't care about the
;; values of the output aside from the symbols. once we **have** an ordered tape, we
;; can remove all relevant ordering information and operate on it as we wish.
;; but we'll still need to keep the ordering information: this presents an interesting challenge.
;; how can we show that mtgi-ord's output represents the same output as mtgi?

(defun ordered-creatures (creatures)
  (cond
    ((endp creatures) t)
    ((endp (rest creatures)) t)
    (t (and (le-creature (first creatures) (second creatures))
            (ordered-creatures (rest creatures))))))

(defun render-mtg-tape (creatures)
  (cons
    (map-creature-names (isort-creatures (green-creatures creatures)))
    (list (map-creature-names (isort-creatures (white-creatures creatures))))))

;; equivalent to the example-tape in utm.lisp
(defconst *example-mtg-tape*
  '(  (faerie green 3 3)
      (aetherborn green 4 4)
      (faerie green 5 5)
      (aetherborn green 6 6)
      (aetherborn green 7 7)
      (faerie green 8 8)
      (faerie green 9 9)
      (aetherborn green 10 10)
      (aetherborn green 11 11)
      (aetherborn green 12 12)
      (aetherborn green 13 13)
      (aetherborn green 14 14)
      (aetherborn green 15 15)
      (aetherborn green 16 16)
      (aetherborn green 17 17)
      (aetherborn green 18 18)
      (aetherborn green 19 19)
      (aetherborn green 20 20)
      (faerie green 21 21)
      (aetherborn green 22 22)
      (aetherborn green 23 23)
      (aetherborn green 24 24)
      (aetherborn green 25 25)
      (aetherborn green 26 26)
      (faerie green 27 27)
      (faerie green 28 28)
      (aetherborn green 29 29)
      (faerie green 30 30)
      (aetherborn green 31 31)
      (aetherborn green 32 32)
      (aetherborn green 33 33)
      (aetherborn green 34 34)
      (aetherborn green 35 35)
      (aetherborn green 36 36)
      (aetherborn green 37 37)
      (aetherborn green 38 38)
      (aetherborn green 39 39)
      (aetherborn green 40 40)
      (aetherborn green 41 41)
      (aetherborn green 42 42)
      (faerie green 43 43)
      (aetherborn green 44 44)
      (aetherborn green 45 45)
      (aetherborn green 46 46)
      (aetherborn green 47 47)
      (aetherborn green 48 48)
      (aetherborn green 49 49)
      (aetherborn green 50 50)
      (aetherborn green 51 51)
      (faerie green 52 52)
      (faerie green 53 53)
      (rhino green 54 54)
      (rhino green 55 55)

      (faerie white 2 2)
      (aetherborn white 3 3)
      (myr white 4 4)
      (aetherborn white 5 5)
      (aetherborn white 6 6)
      (aetherborn white 7 7)
      (aetherborn white 8 8)
      (myr white 9 9)
      (aetherborn white 10 10)
      (aetherborn white 11 11)
      (aetherborn white 12 12)
      (aetherborn white 13 13)
      (aetherborn white 14 14)
      (aetherborn white 15 15)
      (aetherborn white 16 16)
      (myr white 17 17) ))

(defthm ordered-isort-creatures
  (implies
    (creaturesp creatures)
    (ordered-creatures (isort-creatures creatures))))

;; based on the acl2 insertion sort tutorial, we also need to prove that
;; insertion sort produces a permutation of the input, as it might 
;; always return nil (and still be ordered!)
;; to do so, we need to set up an inductive proof, since it's not quite
;; as trivial to admit for creatures as it is for integers. (why?)

;; used
(defthm insert-is-member
  (implies 
    (and
      (creaturep c)
      (creaturesp creatures))
    (memberp c (insert-creature c creatures))))

;; used
(defthm isort-is-creaturesp
  (implies
    (creaturesp creatures)
    (creaturesp (isort-creatures creatures))))

;; used
(defthm isort-preserves-first
  (implies
    (and
      (consp creatures)
      (creaturesp creatures)
      (creaturesp (rest creatures)))
    (memberp (first creatures) (isort-creatures creatures)))
  :hints 
  (("Goal" :use 
           ((:instance insert-is-member (c (first creatures))
                                        (creatures (isort-creatures (rest creatures)))))
           :expand (isort-creatures creatures))))

;; up to here is proved
;; induction
(defthm isort-is-perm
  (implies 
    (creaturesp creatures)
    (perm creatures (isort-creatures creatures)))
  :hints
  (("Goal" :expand (isort-creatures creatures)
           :induct (perm creatures (isort-creatures creatures)))))

;; determines if the creatures are arranged in progressive order: that is, every creature is either
;; - the head, or
;; - offset by 1 from the head, or
;; - has a card of 1 less toughness and matching power before it
(defun sequential (creatures)
  (cond
    ((endp creatures) t)
    ((endp (rest creatures)) t)
    (t (and
         (or (equal (third (first creatures)) (third (second creatures)))
             (equal (+ 1 (third (first creatures))) (third (second creatures))))
         (sequential (rest creatures))))))

;; a useful function to ensure we have properly ordered tapes
;; this works on all creatures of a given color (ie, green creatures
;; and white creatures, corresponding to the **unordered** half-tapes)
(defun well-formed-battlefield (creatures)
  (and
    (consp creatures)
    (creaturesp creatures)
    (find-head (green-creatures creatures))
    (not (find-head (white-creatures creatures)))
    (sequential (isort-creatures (white-creatures creatures)))
    (sequential (isort-creatures (green-creatures creatures)))))

(defun mtg-haltedp (creatures)
  (cond
    ((endp creatures) nil)
    ((consp creatures) (or (equal (first (first creatures)) 'assassin)
                       (mtg-haltedp (rest creatures))))))

;; need to next show that some operations, once we resort the list, will remain true (ie the 2/2 thing)
(defthm head-exists-and-is-two-two
  (implies
      (well-formed-battlefield creatures)
    (= (third (find-head creatures)) 2)))

(defun mtgi* (st tape tm n)
  (let ((res (mtgi st tape tm n)))
    (cond
      ((endp res) nil)
      ((consp res) (cons (isort-creatures (green-creatures res))
                         (list (isort-creatures (white-creatures res))))))))

;; will need addiitonal lemmas, most likely
(defthm mtgi-is-well-formed
  (implies
    (and
      (booleanp st)
      (well-formed-battlefield creatures)
      (natp n)
      (> n 0)
      (mtg-haltedp (mtgi st creatures *rogozhin-rotlungs* n)))
    (well-formed-battlefield (mtgi st creatures *rogozhin-rotlungs* n))))


;; prove that running mtgi and ordering the input is a permutation -- not bad?
;; we already have the permutation, and are defining the ordered property ourselves


;; prove that mtgi-ord produces the same output as ordering after the fact


;; reduce the output down to just the symbols with a mapper
; (defun map-mtg (creatures)
;   (cond
;     ((endp creatures) nil)
;     ((consp creatures)))

;; prove that the symbols are isomorphic to the rogozhin alphabet

;; prove that mtgi-ord's output, once reduced, is equivalent to utmi.

;; then, need to have a function which actually operates on an ordered battlefield. would look a lot like utmi:

(defconst *rogozhin-assoc* 
  '((aetherborn 1)
    (basilisk 1>)
    (cephalid 1<)
    (demon 1>1)
    (elf 1<1)
    (faerie b)
    (giant b>)
    (harpy b<)
    (illusion b>1)
    (juggernaut b<1)
    (kavu b2)
    (leviathan b3)
    (myr c)
    (noggle c>)
    (orc c<)
    (pegasus c>1)
    (rhino c<1)
    (sliver c2)
    (assassin HALT)))

(defun map-creature (creature)
  (second (assoc (first creature) *rogozhin-assoc*)))

(defun map-tape (creatures)
  (cond
    ((endp creatures) nil)
    ((consp creatures) (cons (map-creature (first creatures))
                             (map-tape (rest creatures))))))

; (defun mtgi-ord (st tape tm n)
;   (declare (xargs :measure (nfix n)))
;   (cond
;     ;; done taking steps
;     ((zp n) nil)
;     ;; symbol appears in the program
;     (())
;     ;; symbol we don't recognize: must be a halt!
;     (t tape)







