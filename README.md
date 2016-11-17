# COMP30020 Assignment 2
Assignment 2 for COMP30020 - Declarative Programming, a fill-in puzzle solver written in Prolog.

## Overview
This is completed code for project 2 for the University of Melbourne subject
COMP30020 - Declarative Programming. The aim to is to write prolog code which
can take something called a fill-in puzzle, in its unfilled state, as well as
a list of words for that puzzle, and produce a completed puzzle.
Efficiency is important here, because as the puzzle grows the possibilities
for filling the slots grow vastly, making it unfeasible to brute force it.
The main functionality includes preparing the puzzle by filling it with
logical variables, finding slots in which to put words, and generating words
which could be the correct word for each slot. These pairs are then sorted
and the slot with the fewest possible words is attempted to be filled in.
If the algorithm gets itself into a hole where it can't fill anything in, the
program will backtrack until it gets to a suitable choicepoint to try again. 
This process continues until the puzzle is filled in.

## Usage
1. Load up SWI Prolog and load in the fillin.pl file with: `[fillin].`.
2. If run from within this cloned repo, execute this predicate:
```prolog
main("samples/puzzle1", "samples/words1", "samples/solved1").
```
The solved puzzle will be in `samples/solved1`.

## Directory structure
The root directory contains the code and this readme.

- **provided_files** contains the files given to us to begin with the assignment. The provided predicates are for reading in the files and verifying their correctness.
- **samples** contains a few puzzles/wordlists with their filled-in solutions.
- **other** contains excerpts from the discussion forum, some thoughts on how to progress with the assignment, and code snippets.

## A note on sorting
Originally, sorting happened like this.
The pairs would be sorted in descending order (so sorted and then reversed)
based on length of the slots first. A single insertion sort would then occur
to sort the pairs based on how many words the slots had. This first
insertion sort was likely expensive, up to O(n^2). However, after every time
a word was matched to a slot, the list would be sorted with insertion sort
again. Because the list was mostly already sorted, this would be around O(n),
since insertion sort performs really well on mostly sorted data. Furthermore,
insertion sort is stable, meaning that the long slots that were at the start
of the list due to quicksort would stay around there. This is advantageous
because a long word fills more squares/elements than a short one.

Theoretically this should have been faster. However, this sorting mechanism 
would run out of time on test case 6. After experimenting a bit more, it was
found that using quicksort for the recursive case sorting was somehow faster, 
and indeed made the code pass test case 6. Whether this is due to it being 
genuinely better or just that it does well on these specific cases is hard 
to say, but this quicksorting version has been left in for the purposes of 
passing cases. In my opinion, the original method should be the cheapest 
computationally, especially considering that quicksort doesn't perform outstandingly 
on linked lists in the first place. If the code performs worse on hidden cases, consider
that perhaps this arrangement of sorting algorithms is to blame. Excuse the long read!

## Results
Final results: 95%

Correctness: 70/70 || Code style: 25/30

See `other/porteousd_results.pdf` for a full description of the results.