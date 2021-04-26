# Final Project: Proving Turing Completeness of Magic: The Gathering in ACL2
### Authors: Leonid Belyaev and Samuel Lyon

# Introduction
The aim of our project is to demonstrate that the game Magic: The Gathering (henceforth MTG)is capable of embedding a Universal Turing machine, and is thus both Turing complete in its ruleset and undecidable. Our project specifically proves that operations on a sequence of "creature tokens", cards in the game which encode symbols on a Turing machine, leave the sequence of cards **well-formed**: each time the game advances analogously to the UTM(2, 18), the cards describe a properly constructed input tape for a Turing machine.

Our project was completed in ACL2, and required a great deal of lemmata to get working. We wrote an interpreter for a UTM(2, 18), or a Universal Turing machine with 2 states and 18 symbols, as described in Yurii Rogozhin's *Small universal Turing machines*. We used existing interpreters, such as the one found [here](http://www.nearly42.org/misc/tm/tm.html) and on [Replit](https://repl.it/@Quantumplation/UTM218) to ensure the soundness of our interpreter, since it serves as the source of truth for our project.

We also wrote various lemmata to build up to our final proof, and ensure the soundness of our additional functions. These will be discussed in detail below.

We made use of the `lists-light` library, which is part of the ACL2 community books. Specifically, we used the `perm` and `memberp` functions, along with their associated lemmata and equivalence/congruence relations to prove our own lemmata and main theorem.

Finally, our work was heavily inspired by the paper *[Magic: The Gathering is Turing Complete](https://arxiv.org/abs/1904.09828)* by Churchill et al. as we model the system laid out in this paper which embeds a UTM(2, 18) in the MTG card game. J Strother Moore's [Proof Peal: Proving a Simple Von Neumann Machine Turing Complete](https://www.cs.utexas.edu/users/moore/publications/turing-completeness.pdf) and its accompanying code in the ACL2 community book `models/jvm/m1` served as the starting point and inspiration for the structure of our project, though we have since pivoted in both direction and scope.
<!-- might mention Rogozhin paper here as well, though we already did kinda mention it -->

# Background



Magic: The Gathering is a widely popular trading card game developed by Wizards of the Coast. In the game, two players battle with a fixed-size deck of cards, consisting entirely of their own choice of cards within the deck. Players have thousands of cards to choose from when they build their deck, lending the game to a deep variety in strategy, and difficulty in choosing an optimal path through the game algorithmically. Churchill et al. showed that optimal play of Magic: The Gathering (MTG from here on) is generally undecidable[1], proving this by demonstrating how two players can embed a Universal Turing Machine (UTM) with 2 states and 18 symbols (UTM(2, 18)) within the rules of the game. Furthermore, the paper demonstrates how this can be done ***deterministically***, without any choices in the game left up to the two players. Thus, the game is forced to execute "instructions" once entering a given state, based on an initial layout of "creature tokens", the cards which serve as the symbols for the embedded UTM.
