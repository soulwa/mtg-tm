# Final Project: Proving Turing Completeness of Magic: The Gathering in ACL2
### Authors: Leonid Belyaev and Samuel Lyon

# Introduction
The aim of our project is to demonstrate that the game Magic: The Gathering (henceforth MTG)is capable of embedding a Universal Turing machine, and is thus both Turing complete in its ruleset and undecidable. Our project specifically proves that operations on a sequence of "creature tokens", cards in the game which encode symbols on a Turing machine, leave the sequence of cards **well-formed**: each time the game advances analogously to the UTM(2, 18), the cards describe a properly constructed input tape for a Turing machine.

Our project was completed in ACL2, and requires a great deal of lemmata to get working. We develop an interpreter for a UTM(2, 18), or a Universal Turing machine with 2 states and 18 symbols, as described in Yurii Rogozhin's *Small universal Turing machines*. We use existing interpreters, such as the one found [here](http://www.nearly42.org/misc/tm/tm.html) and on [Replit](https://repl.it/@Quantumplation/UTM218) to ensure the soundness of our interpreter, since it serves as the source of truth for our project.

We also write various lemmata to build up to our final proof, and ensure the soundness of our additional functions. These will be discussed in detail below.

We use the `lists-light` library, which is part of the ACL2 community books. Specifically, we use the `perm` and `memberp` functions, along with their associated lemmata and equivalence/congruence relations to prove our own lemmata and main theorem.

Finally, our work is inspired by the paper *[Magic: The Gathering is Turing Complete](https://arxiv.org/abs/1904.09828)* by Churchill et al. as we model the system laid out in this paper which embeds a UTM(2, 18) in the MTG card game. J Strother Moore's [Proof Peal: Proving a Simple Von Neumann Machine Turing Complete](https://www.cs.utexas.edu/users/moore/publications/turing-completeness.pdf) and its accompanying code in the ACL2 community book `models/jvm/m1` served as the starting point and inspiration for the structure of our project, though we have since pivoted in both direction and scope.
<!-- might mention Rogozhin paper here as well, though we already did kinda mention it -->

# Background

There is no existing implementation of the proof that MTG is Turing complete, in any automated theorem prover, as far as we know. As such, this project serves as a first attempt to implement a working proof (or part of one) of Turing equivalence in a system where it was not intended. The UTM(2, 18) is a popular machine used to show that systems such as video games are Turing equivalent. This project also stands as the first attempt of modelling one of these systems alongside a UTM(2, 18) in an automated theorem prover. 

Our data types begin with a working implementation of a UTM(2, 18). We model the *production functions* for the universal Turing machine, often called *instructions* in other machines, as 5-tuples, with a state *q1* and a symbol *s1* expressing that this function should be read when the machine is in state *q1*, and the interpreter reads *s1* from the input tape. The remaining values are another symbol to write to the tape, a direction for the head position to move in, and a new state to transition the interpreter into. We model the tape with a cons list of symbols, corresponding to those used by Rogozhin in his paper.

Our other data types are analogous to those presented in MTG, with some simplifications made. We model the production functions, or [Rotlung Reanimators](https://scryfall.com/card/ons/164/rotlung-reanimator)/[Xathrid Necromancers](https://scryfall.com/card/c20/141/xathrid-necromancer) similarly to those in the UTM. These are 4-tuples, consisting of a state and creature name expressing that this Rotlung/Xathrid card has its ability triggered, a single *creature* which it produces, and a new state to transition into. *Creatures* represent a symbol on the tape much like the UTM tape, but they also contain positional information regarding where they are on the tape. The creature with 2 power and 2 toughness is always at the head, and other creatures all have power and toughness greater than this, representing how far offset from the head they are. All creatures are either *green*, indicating that they are to the left of the head, *white*, indicating that they are to the right, or *blue*, which is only the case when the creature token which halts the game, the assassin, is written to the tape. A creature's color additionally indicates what direction the tape should move in once the creature is written to the head of the tape.

To move the tape, the players cast spells which deal damage to or empower creatures, raising their *power* or *toughness*, which will shift them along the tape. These are represented by functions which operate on every creature on the tape (`snuffers`) or creatures of a specified color (`vigor-beam`) so that at each movement step, one set of creatures is shifted left and another is shifted right.

Our source code is [available here](https://github.com/soulwa/mtg-tm).

# Walkthrough

We first implement a tag system and UTM(2, 18) in ACL2, to ensure that our domain knowledge of the task ahead was sufficient. Using existing models, we set both of these machines up to terminate on certain example programs. Once these worked correctly, they served as a source of truth for the output of the MTG interpreter.

