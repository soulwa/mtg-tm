;; An ACL2 implementation of an MTGTM.
;; https://arxiv.org/abs/1904.09828 
(include-book "kestrel/lists-light/perm" :dir :system)
(include-book "kestrel/lists-light/memberp" :dir :system)
(include-book "acl2s/check-equal" :dir :system)

;; MTGTM creature types
(defconst *types*
  '(aetherborn basilisk cephalid demon elf faerie giant harpy illusion
	       juggernaut kavu leviathan myr noggle orc pegasus rhino
	       sliver assassin))

;; MTGTM tape-creature colors
(defconst *colors*
  '(white green blue))

;; MTGTM creature type recognizer
(defun ctypep (x)
  (memberp x *types*))

(ACL2S::check= (ctypep 'assassin) t)
(ACL2S::check= (ctypep 'frog) nil)

;; MTGTM creature color recognizer
(defun ccolorp (x)
  (memberp x *colors*))
(ACL2s::check= (ccolorp 'blue) t)
(ACL2s::check= (ccolorp 'black) nil)

;; the representation of a UTM(2, 18) in magic cards
;; consists of rotlungs/xathrids (rotlungs have matching state)
;; which produce the next head for the tape
(defconst *mtg-tm-2-18*
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

;; a creature has a type, color, power, and toughness
;; creatures must have power/toughness greater than 2,
;; and their power/toughness must remain equal
(defun creaturep (x)
  (and (= (length x) 4)
       (ctypep (first x))
       (ccolorp (second x))
       (natp (third x))
       (>= (third x) 2)
       (natp (fourth x))
       (>= (fourth x) 2)
       (= (third x) (fourth x))))
(ACL2s::check= (creaturep '(myr white 2 2)) t)
(ACL2s::check= (creaturep '(myr white 1 1)) nil)

;; a collection of creatures, where order does not matter
(defun creaturesp (x)
  (cond ((endp x) T)
        ((consp x) (and (creaturep (first x))
                        (creaturesp (rest x))))))
(ACL2s::check= (creaturesp '((myr white 2 2) (noggle white 2 2))) t)
(ACL2s::check= (creaturesp '()) t)
(ACL2s::check= (creaturesp '((1 2) (3 4) (5 6))) nil)

;; rotlung-reanimator: creates a new creature when
;; infest causes the specified creature to die
(defun rotlungp (x)
  (and (= (length x) 4)
       (booleanp (first x))
       (ctypep (second x))
       (creaturep (third x))
       (booleanp (fourth x))))
(ACL2s::check= (rotlungp (first *mtg-tm-2-18*)) t)
(ACL2s::check= (rotlungp '(t noggle (kavu) t)) nil)

;; a list of rotlungs
(defun rotlungsp (x)
  (cond ((endp x) T)
        ((consp x) (and (rotlungp (first x))
                        (rotlungsp (rest x))))))
(ACL2s::check= (rotlungsp *mtg-tm-2-18*) t)
(ACL2s::check= (rotlungsp '()) t)

;; find the appropriate rotlung given state, type, and a
;; list of rotlungs (effectively, a turing machine)
(defun find-rotlung (st typ tm)
  (cond ((endp tm) nil)
        ((consp tm) (if (and (equal (first (first tm)) st)
                             (equal (second (first tm)) typ))
			(first tm)
		      (find-rotlung st typ (rest tm))))))
(ACL2s::check= (find-rotlung t 'kavu *mtg-tm-2-18*)
               '(t kavu (leviathan white 2 2) nil))
;; in practice, this is always used with rogozhin-rotlungs
;; we emulated UTM(2,18), not an arbitrary turing machine
(ACL2s::check= (find-rotlung t 'kavu '())
               '())

;; finds the head creature, of power and toughness 2/2
;; otherwise, indicate no such creature exists (nil)
;;  primarily used for lemmas, upholding invariants
(defun find-head (tape)
  (cond ((endp tape) nil)
        ((consp tape) (if (and (= 2 (third (first tape)))
                               (= 2 (fourth (first tape))))
			  (first tape)
                        (find-head (rest tape))))))
(defconst *test-tape1* 
  '((juggernaut green 2 2)
    (kavu green 3 3)
    (kavu green 4 4)))
(ACL2s::check= (find-head *test-tape1*)
	       '(juggernaut green 2 2))
(ACL2s::check= (find-head '())
               '())
(ACL2s::check= (find-head (remove-equal '(juggernaut green 2 2) *test-tape1*))
               '())

;; produces the head of the tape, or a blank symbol otherwise (nothing is on the
;; input tape at the current position, so we read a blank symbol)
(defun head (tape)
  (cond
   ((endp tape) '(cephalid white 2 2))
   ((consp tape) (if (and (= 2 (third (first tape)))
			  (= 2 (fourth (first tape))))
		     (first tape)
		   (head (rest tape))))))
(ACL2s::check= (head *test-tape1*)
               '(juggernaut green 2 2))
(ACL2s::check= (head '())
               '(cephalid white 2 2))

;; removes the head of the creatures
(defun infest (tape)
  (remove-equal (head tape) tape))
(ACL2s::check= (infest *test-tape1*)
               '( (kavu green 3 3)
		  (kavu green 4 4)))

;; cast beam on creatures, giving +2 / +2 to all creatures of the color specified
;; used in combination with snuffers to give +1/+1 to color specified and -1/-1 to other color
(defun vigor-beam (tape color)
  (cond ((endp tape) nil)
        ((consp tape) (if (equal color (second (first tape)))
                          (cons (list (first (first tape))
                                      (second (first tape))
                                      (+ 2 (third (first tape)))
                                      (+ 2 (fourth (first tape))))
				(vigor-beam (rest tape) color))
			(cons (first tape) (vigor-beam (rest tape) color))))))
(ACL2s::check= (vigor-beam *test-tape1* 'green)
               '( (juggernaut green 4 4)
                  (kavu green 5 5)
                  (kavu green 6 6)))
(ACL2s::check= (vigor-beam *test-tape1* 'blue)
               '( (juggernaut green 2 2)
                  (kavu green 3 3)
                  (kavu green 4 4)))

;; cast snuffers, dealing -1 -1 to all creatures
;; used in combination with vigor-beam
(defun snuffers (tape)
  (cond ((endp tape) nil)
        ((consp tape) (cons (list (first (first tape))
				  (second (first tape))
				  (- (third (first tape)) 1)
				  (- (fourth (first tape)) 1))
			    (snuffers (rest tape))))))
(ACL2s::check= (snuffers *test-tape1*)
               '( (juggernaut green 1 1)
                  (kavu green 2 2)
                  (kavu green 3 3)))
(ACL2s::check= (snuffers '())
               '())


;; combine vigor, beam, and snuffers to move in the desired direction
(defun move (tape color)
  (snuffers (vigor-beam tape color)))
(ACL2s::check= (move *test-tape1* 'green)
               '( (juggernaut green 3 3)
                  (kavu green 4 4)
                  (kavu green 5 5)))

;; produce a new tape, given a tape and a new head to write
;; to the current tape
;; we write the head, then extract the directions encoded in the
;; head and move appropriately
(defun new-tape (new-head tape)
  (let* ((color (second new-head))
         (headless (infest tape)))
    (case color
	  ('white (move (cons new-head headless) color))
	  ('green (move (cons new-head headless) color))
	  (t (cons new-head headless)))))
(ACL2s::check= (new-tape '(illusion white 2 2) *test-tape1*)
               (cons '(illusion white 3 3) '(
                                             (kavu green 2 2)
                                             (kavu green 3 3))))
(ACL2s::check= (new-tape '(elf green 2 2) *test-tape1*)
               (cons '(elf green 3 3) '(
					(kavu green 4 4)
					(kavu green 5 5))))

;; mtg interpreter: casts spells on a group of creatures
;; in a deterministic manner such that (theoretically) the creatures
;; embed a UTM(2, 18).
;; takes in an initial state, a tape, and a set of rotlungs as tm,
;; as well as a number of steps n.
;; as ACL2 wouldn't accept a turing machine interpreter which took
;; an arbitrary amount of steps (since it won't prove halting!) we
;; contrain the runtime of the function to guarantee it terminates after n steps
(defun mtgi (st tape tm n)
  (declare (xargs :measure (nfix n)))
  (let* ((head (head tape))
         (rlg  (find-rotlung st (first head) tm)))
    (cond 
     ;; done taking steps (doesn't halt in n steps)
     ((zp n) nil)
     ;; produce a new tape with the given production function
     (rlg (mtgi (fourth rlg) (new-tape (third rlg) tape) tm (- n 1)))
     ;; halted in n steps (no prod. function available, must be halt symbol)
     ((not rlg) tape))))

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

;; tests the behavior of the mtgi
(ACL2s::check= (mtgi t *example-mtg-tape* *mtg-tm-2-18* 0) nil)
(ACL2s::check= (creaturesp (mtgi t *example-mtg-tape* *mtg-tm-2-18* 6003)) t)     

;; in order to order this mtg tape, we want to model
;; insertion sort on our list of creatures

;; defines the less than/equal to relation on creatures
(defun le-creature (c1 c2)
  (<= (fourth c1) (fourth c2)))
(ACL2s::check= (le-creature '(juggernaut green 2 2) '(kavu green 3 3)) t)

;; maintains symmetry/transitivity with less than/equal to relation
(defthm le-creature-relation
  (and
   (booleanp (le-creature c1 c2))
   (le-creature c1 c1)
   (implies (and (le-creature c1 c2) (le-creature c2 c3))
	    (le-creature c1 c3))))

;; inserts a creature into a list of creatures, based on its
;; power/toughness
(defun insert-creature (creature l) 
  (cond
   ((endp l) (cons creature l))
   ((le-creature creature (first l)) (cons creature l))
   (t (cons (first l) (insert-creature creature (rest l))))))
(ACL2s::check= (insert-creature '(leviathan green 2 2) '()) '((leviathan green 2 2)))
(ACL2s::check= (insert-creature '(leviathan green 2 2) '( (kavu green 3 3) (kavu green 4 4)))
               '( (leviathan green 2 2) (kavu green 3 3) (kavu green 4 4)))

;; sort a list of creatures, based on their power/toughness
(defun isort-creatures (creatures)
  (cond
   ((endp creatures) nil)
   ((consp creatures)
    (insert-creature (first creatures)
		     (isort-creatures (rest creatures))))))
(ACL2s::check= (isort-creatures '( (leviathan green 4 4) (assassin blue 3 3) (kavu green 2 2)))
               '( (kavu green 2 2) (assassin blue 3 3) (leviathan green 4 4)))
(ACL2s::check= (isort-creatures '( (leviathan green 2 2) (leviathan green 4 4) (leviathan green 3 3)))
               '( (leviathan green 2 2) (leviathan green 3 3) (leviathan green 4 4)))

;; we then need the different "pieces" of the tape, so we 
;; can model the pieces of the half tape

;; this gets us the left side of the half tape, or all green creatures
;; excluding the head
(defun left-tape (creatures)
  (cond
   ((endp creatures) nil)
   ((and
     (equal (second (first creatures)) 'green)
     (not (equal (third (first creatures)) 2)))
    (cons (first creatures) (left-tape (rest creatures))))
   (t (left-tape (rest creatures)))))
(ACL2s::check= (left-tape '( (leviathan green 4 4) (kavu white 2 2)))
               '( (leviathan green 4 4)))

;; this gets us the right side of the half tape, or all white creatures
;; again excluding the head
(defun right-tape (creatures)
  (cond
   ((endp creatures) nil)
   ((and
     (equal (second (first creatures)) 'white)
     (not (equal (third (first creatures)) 2)))
    (cons (first creatures) (right-tape (rest creatures))))
   (t (right-tape (rest creatures)))))
(ACL2s::check= (right-tape '( (leviathan green 4 4) (kavu white 2 2)))
               '())

;; we define some additional functions, allowing us to test the output of mtgi
;; in a much nicer format
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

;; turn the creature name into an abbreviated form
(defun map-creature-name (creature)
  (second (assoc (first creature) *name-map*)))
(ACL2s::check= (map-creature-name '(juggernaut green 2 2)) 'j)
(ACL2s::check= (map-creature-name '(apple white 2 2)) nil)

;; transform the creatures to appear as symbols representing their names,
;; given that they are already ordered.
(defun map-creature-names (creatures)
  (cond
   ((endp creatures) nil)
   ((consp creatures) (cons (map-creature-name (first creatures))
			    (map-creature-names (rest creatures))))))

;; transform the tape to be like the utm tape
(defun render-mtg-tape (creatures)
  (cons
   (map-creature-names (isort-creatures (left-tape creatures)))
   (list
    (cons (map-creature-name (head creatures))
	  (map-creature-names (isort-creatures (right-tape creatures)))))))


;; ================== LEMMAS ========================

;; we want to be sure that insertion sort actually orders the creatures
;; here we define the ordered check on a group of our creatures
(defun ordered-creatures (creatures)
  (cond
   ((endp creatures) t)
   ((endp (rest creatures)) t)
   (t (and (le-creature (first creatures) (second creatures))
	   (ordered-creatures (rest creatures))))))

;; this lemma ensures that isort-creatures produces an ordered list
;; of creatures (increasing in power/toughness) as its output
(defthm ordered-isort-creatures
  (implies
   (creaturesp creatures)
   (ordered-creatures (isort-creatures creatures))))

;; asserts that inserting a creature into a list of creatures
;; makes it a member of that list
(defthm insert-is-memberp
  (implies 
   (and
    (creaturep c)
    (creaturesp creatures))
   (memberp c (insert-creature c creatures))))

;; asserts that running isort on a list of creatures
;; produces a list of creatures
(defthm isort-is-creaturesp
  (implies
   (creaturesp creatures)
   (creaturesp (isort-creatures creatures))))

;; asserts that isort is a permutation of the original list
;; while we know the list is ordered, we want to ensure it also contains
;; all of the creatures we originally had in our list, and
;; ensure ordered holds true on arbitrarily large inputs
(defthm isort-is-perm
  (perm (isort-creatures creatures) creatures)
  :hints
  (("Goal"
    :induct (isort-creatures creatures))))

;; in order to submit isort-creatures-is-perm to ACL2, we first
;; submit isort-is-perm. ACL2 focused on the implication that
;; the list is of creatures if trying only this lemma, but with
;; the previous lemma it is able to infer, from simpler lemmas
;; we have defined and rewrite rules for memberp/perm, that any
;; list we use insertion-sort on (which doesn't violate its guards, of
;; course) will be a permutation
(defthm isort-creatures-is-perm
  (implies
   (creaturesp creatures)
   (perm (isort-creatures creatures) creatures))
  :hints
  (("Goal"
    :use ((:instance isort-is-perm (creatures creatures))))))

;; determines if the creatures are arranged in progressive order by toughness
(defun sequential (creatures)
  (cond
   ((endp creatures) t)
   ((endp (rest creatures)) t)
   (t (and
       (equal (+ 1 (third (first creatures))) (third (second creatures)))
       (sequential (rest creatures))))))

;; congruence relationship that if two lists are equal, they return
;; the same result when tested to see if they are sequential
(defcong equal equal (sequential creatures) 1)

(defun min-creature-pow-acc (creatures acc)
  (cond
   ((endp creatures) acc)
   ((consp creatures) (min-creature-pow-acc
		       (rest creatures)
		       (min acc (third (first creatures)))))))

(defun min-creature-pow (creatures)
  (min-creature-pow-acc creatures 100000)) 

;; a useful function to ensure we have properly ordered tapes
;; this works on all creatures of a given color (ie, green creatures
;; and white creatures, corresponding to the **unordered** half-tapes)
;; we don't actually care whether we're at a head or not... if we aren't,
;; either the left or right should be empty though
(defun well-formed (creatures)
  (and
   (creaturesp creatures)
   (sequential (isort-creatures (right-tape creatures)))
   (sequential (isort-creatures (left-tape creatures)))
   ;; (xor (endp (left-tape creatures)) (equal (min-creature-pow (left-tape creatures)) 3))
   ;; (xor (endp (right-tape creatures)) (equal (min-creature-pow (right-tape creatures)) 3))
   ;; (xor (find-head creatures)
   ;; 	(xor (endp left-tape creatures)
   ;; 	     (endp right-tape creatures)))
   ))

(defcong equal equal (well-formed creatures) 1)

(defthm empty-tape-is-well-formed
  (well-formed nil))

(defthm head-is-memberp
  (implies
   (and
    (creaturesp creatures)
    (find-head creatures))
   (memberp (find-head creatures) creatures)))

(defthm right-tape-elems
  (implies
   (and
    (creaturesp creatures)
    (memberp c (right-tape creatures)))
   (and
    (equal (second c) 'white)
    (not (equal (third c) 2)))))

(defthm left-tape-elems
  (implies
   (and
    (creaturesp creatures)
    (memberp c (left-tape creatures)))
   (and
    (equal (second c) 'green)
    (not (equal (third c) 2)))))

(defthm left-not-in-right
  (implies
   (and
    (creaturesp creatures)
    (memberp c (right-tape creatures)))
   (not (memberp c (left-tape creatures)))))

(defthm right-not-in-left
  (implies
   (and
    (creaturesp creatures)
    (memberp c (left-tape creatures)))
   (not (memberp c (right-tape creatures)))))

(defthm left-right-disjoint
  (implies
   (and
    (creaturesp creatures)
    (memberp c1 (right-tape creatures))
    (memberp c2 (left-tape creatures)))
   (and
    (not (memberp c2 (right-tape creatures)))
    (not (memberp c1 (left-tape creatures))))))

;; no 2/2 creatures are in either tape (shouldn't this include the head?!)
(defthm no-two-two
  (implies
   (and
    (creaturesp creatures)
    (creaturep c)
    (equal (third c) 2))
   (and
    (not (memberp c (right-tape creatures)))
    (not (memberp c (left-tape creatures))))))

(defthm head-is-two-two
  (implies
   (creaturesp creatures)
   (equal (third (head creatures)) 2)))

(defthm head-not-in-left-right
  (implies
   (creaturesp creatures)
   (and
    (not (memberp (head creatures) (left-tape creatures)))
    (not (memberp (head creatures) (right-tape creatures)))))
  :hints
  (("Goal"
    :use ((:instance no-two-two (creatures creatures) (c (head creatures)))
	  (:instance head-is-two-two))
    :in-theory (disable head-is-two-two))))

(defthm removal-keeps-the-same-left
  (implies
   (not (memberp c (left-tape creatures)))
   (equal
    (left-tape (remove-equal c creatures))
    (left-tape creatures))))

(defthm removal-keeps-the-same-right
  (implies
   (not (memberp c (right-tape creatures)))
   (equal
    (right-tape (remove-equal c creatures))
    (right-tape creatures))))

;; determines if this tape is halted
(defun mtg-haltedp (creatures)
  (cond
   ((endp creatures) nil)
   ((consp creatures) (equal (first (head creatures)) 'assassin))))

;; tape is only halted if the head is assassin
(defthm halted-iff-head-assassin
  (implies
   (well-formed creatures)
   (iff (equal (first (head creatures)) 'assassin)
	(mtg-haltedp creatures))))

;; rlg is nil iff creature is assassin
(defthm assassin-lookup-is-nil
  (implies
   (and
    (booleanp st)
    (ctypep typ))
   (iff (not (find-rotlung st typ *mtg-tm-2-18*))
	(equal typ 'assassin))))

;; rlg isn't nil iff creature isn't assassin
(defthm other-lookups-have-rotlung
  (implies
   (and
    (booleanp st)
    (ctypep typ))
   (iff (find-rotlung st typ *mtg-tm-2-18*)
	(not (equal typ 'assassin)))))

;; mtgi will halt on assassin 
(defthm mtgi-halts-on-assassin
  (implies
   (and
    (booleanp st)
    (creaturesp creatures)
    (natp n)
    (> n 0)
    (equal (first (head creatures)) 'assassin))
   (mtg-haltedp (mtgi st creatures *mtg-tm-2-18* n))))

;; using the above theorem, we can show that mtgi stays halted
;; (note: update to show this is equivalent to running for (+ n m)
(defthm mtgi-stays-halted
  (implies
   (and
    (booleanp st)
    (creaturesp creatures)
    (natp n)
    (natp m)
    (> m 0)
    (mtg-haltedp (mtgi st creatures *mtg-tm-2-18* n)))
   (mtg-haltedp (mtgi st
		      (mtgi st creatures *mtg-tm-2-18* n)
		      *mtg-tm-2-18*
		      m)))
  :hints
  (("Goal" :use ((:instance mtgi-halts-on-assassin
			    (st st)
			    (creatures (mtgi st creatures *mtg-tm-2-18* n))
			    (n m))))))

;; need some additional lemmas to help prove well-formedness
;; essentially: demonstrate effects of infest, move, new-tape
;; on the tape, proving each time that a well-formed tape
;; maintains these properties
;; then, we can show that when mtgi terminates, it does so
;; with a well-formed tape (ie, it can be mapped)

;; (skip-proofs
;;  (defthm new-tape-is-well-formed
;;    (implies
;;     (and
;;      (well-formed creatures)
;;      (creaturep new-head)
;;      (equal (third new-head) 2))
;;     (well-formed (new-tape new-head creatures)))))

(defthm mtgi-base-case-well-formed
  (implies
   (and
    (booleanp st)
    (well-formed creatures)
    (equal n 0))
   (well-formed (mtgi st creatures *mtg-tm-2-18* n))))

(defthm mtgi-other-base-case-well-formed
  (implies
   (and
    (booleanp st)
    (well-formed creatures)
    (natp n)
    (not (find-rotlung st (first (head creatures)) *mtg-tm-2-18*)))
   (well-formed (mtgi st creatures *mtg-tm-2-18* n))))

;; should pass..
;; nil is well formed
;; tape is well formed
;; recursive case is simply new-tape, which we have shown is well-formed
;; (defthm mtgi-is-well-formed
;;   (implies
;;    (and
;;     (booleanp st)
;;     (well-formed creatures)
;;     (natp n)
;;     (> n 0)
;;     (find-rotlung st (first (head creatures)) *mtg-tm-2-18*))
;;    (well-formed (mtgi st creatures *mtg-tm-2-18* n)))
;;   :hints
;;   (("Goal"
;;     :expand (mtgi st creatures *mtg-tm-2-18* n)
;;     :induct (mtgi st creatures *mtg-tm-2-18* n))))

(defthm remove-is-creaturesp
  (implies
   (creaturesp creatures)
   (creaturesp (remove-equal c creatures))))

(defthm infest-is-creaturesp
  (implies
   (creaturesp creatures)
   (creaturesp (infest creatures)))
  :hints
  (("Goal"
    :expand (infest creatures))
   ("Subgoal *1/2.2'"
    :use (:instance remove-is-creaturesp
		    (creatures (cdr creatures))
		    (c (car creatures))))
   ("Subgoal 2"
    :cases ((endp (find-head creatures)) (creaturep (find-head creatures)))
    :use (:instance remove-is-creaturesp
		    (creatures creatures)
		    (c (find-head creatures))))))

(defthm head-change-is-creaturesp
  (implies
   (and
    (creaturesp creatures)
    (creaturep new-head)
    (equal (third new-head) 2))
   (creaturesp (cons new-head (infest creatures)))))

(defthm head-change-sets-new-head
  (implies
   (and
    (creaturesp creatures)
    (creaturep new-head)
    (equal (third new-head) 2))
   (equal (find-head (cons new-head (infest creatures))) new-head)))

;; prove these from the angle of them not containing head to begin with
(defthm infest-preserves-left-tape
  (implies
   (creaturesp creatures)
   (equal (left-tape (infest creatures))
	  (left-tape creatures)))
  :hints
  (("Goal"
    :expand (infest creatures)
    :use ((:instance removal-keeps-the-same-left
		     (creatures creatures)
		     (c (head creatures)))
	  (:instance head-not-in-left-right
		     (creatures creatures))))))

(defthm infest-preserves-right-tape
  (implies
   (creaturesp creatures)
   (equal (right-tape (infest creatures))
	  (right-tape creatures)))
  :hints
  (("Goal"
    :expand (infest creatures)
    :use ((:instance removal-keeps-the-same-right
		     (creatures creatures)
		     (c (head creatures)))
	  (:instance head-not-in-left-right
		     (creatures creatures))))))

(defthm head-change-preserves-left-tape
  (implies
   (and
    (creaturesp creatures)
    (creaturep new-head)
    (equal (third new-head) 2))
   (equal (left-tape (cons new-head (infest creatures)))
	  (left-tape creatures)))
  :hints
  (("Goal"
    :use ((:instance infest-preserves-left-tape (creatures creatures))))))

(defthm head-change-preserves-right-tape
  (implies
   (and
    (creaturesp creatures)
    (creaturep new-head)
    (equal (third new-head) 2))
   (equal (right-tape (cons new-head (infest creatures)))
	  (right-tape creatures)))
  :hints
  (("Goal"
    :use ((:instance infest-preserves-right-tape (creatures creatures))))))

;; solid result here
(defthm head-change-well-formed
  (implies
   (and
    (well-formed creatures)
    (creaturep new-head)
    (equal (third new-head) 2))
   (and
    (well-formed (cons new-head (infest creatures)))
    (find-head (cons new-head (infest creatures)))))
  :hints
  (("Goal"
    :use ((:instance head-change-is-creaturesp (creatures creatures) (new-head new-head))
	  (:instance head-change-preserves-left-tape
		     (creatures creatures)
		     (new-head new-head))
	  (:instance head-change-preserves-right-tape
		     (creatures creatures)
		     (new-head new-head))))))

;; after this, we just want to show that vigor-beam + snuffers is also wellformed,
;; then new-tape + mtgi should "easily" follow.

;; this isn't happening, i don't think...
;; (defthm vigor-beam-stays-sequential
;;   (implies
;;    (and
;;     (well-formed creatures)
;;     (find-head creatures)
;;     (sequential (isort-creatures (left-tape creatures))))
;;    (sequential (isort-creatures
;; 		(left-tape (vigor-beam creatures (second (find-head creatures))))))))

(skip-proofs
 (defthm move-is-well-formed
   (implies
    (and
     (well-formed creatures)
     (creaturep new-head)
     (equal (third new-head) 2))
    (well-formed (move creatures (second new-head))))))

(defthm new-tape-is-well-formed
  (implies
   (and
    (well-formed creatures)
    (creaturep new-head)
    (equal (third new-head) 2))
   (well-formed (new-tape new-head creatures)))
  :hints
  (("Goal"
    :use ((:instance head-change-well-formed (creatures creatures) (new-head new-head))
	  (:instance move-is-well-formed (creatures (cons new-head (infest creatures))) (new-head new-head)))
    :in-theory (disable head-change-well-formed move-is-well-formed)
    :cases ((equal (second new-head) 'white)
	    (equal (second new-head) 'green)
	    (equal (second new-head) 'blue)))))

;; will need additional lemmas, most likely
(defthm mtgi-is-well-formed
  (implies
   (and
    (booleanp st)
    (well-formed creatures)
    (natp n))
   (well-formed (mtgi st creatures *mtg-tm-2-18* n)))
  :hints
  (("Goal"
    :use ((:instance new-tape-is-well-formed
		     (creatures creatures)
		     (new-head (third (find-rotlung st (head creatures) *mtg-tm-2-18*))))
	  (:instance mtgi-base-case-well-formed
		     (st st)
		     (creatures creatures)
		     (n n))
	  (:instance mtgi-other-base-case-well-formed
		     (st st)
		     (creatures creatures)
		     (n n)))
    :in-theory (disable new-tape-is-well-formed
			mtgi-base-case-well-formed
			mtgi-other-base-case-well-formed))))

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
