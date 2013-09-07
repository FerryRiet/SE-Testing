module Sol2  

where 

import GS
import TAMO

-- Exercise 2.2 (+2.4) (+2.9)
--
-- Question:	Make up the truth table for the exclusive version of or.
--
-- Answer:		+---+-------++-------+----------++----------------+
--				|2.2|  2.2  ||  2.4  |   2.4    ||       2.9      |
--				+---+-------++-------+----------++----------------+
--				|P Q|P XOR Q||P <=> Q|!(P <=> Q)||(P XOR Q) XOR Q)|
--				+---+-------++-------+----------++----------------+
--				|t t|   f   ||   t   |    f     ||        t       |
--				|t f|   t   ||   f   |    t     ||        t       |
--				|f t|   t   ||   f   |    t     ||        f       |
--				|f f|   f   ||   t   |    f     ||        f       |
--				+---+-------++-------+----------++----------------+

-- Exercise 2.4
--
-- Question:	Check that the truth table for exclusive or from Exercise 2.2 is equivalent
--				to the table for !(P <=> Q). Conclude that the Haskell implementation of the
--				function <+> for exclusive or in the frame below is correct.
--
-- Answer:		See exercise 2.2 for the truth table.
--				The Haskell function is implemented to return true if x and y are not the same.
--				This confirms the truth table.

