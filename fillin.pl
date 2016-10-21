/* 
** Skeleton code provided by Peter Schachte / Les Kitchen.
** Additional code written by me:
** Daniel Porteous porteousd 696965
**
** After opening swipl, run the program like this:
** main("samples/puzzle1", "samples/words1", "samples/attempt1").
** Check how you went by comparing attempt1 and filled1.
*/

/* 
** Terminology:
** Element: A single point in the puzzle. Would use Square, but want distinct
            first letters for each thing (E for element, S for Slot).
** Slot:    A > 2 series of squares running horizontally or vertically.
** Row:     A series of squares, which can be '_', '#', or a letter.
** Puzzle:  Entire puzzle, which is a series of rows.
*/

:- ensure_loaded(library(clpfd)).
:- use_module(puzzle_reader).
:- use_module(generic_reader).

/*
** Reads the PuzzleFile and WordlistFile into Puzzle and Wordlist
** Confirms that the puzzle is valid
** Solves the Puzzle for Wordlist into Solved
** Prints the solved puzzle out to SolutionFile
*/
main(PuzzleFile, WordlistFile, SolutionFile) :-
    % Read in Puzzle and Wordlist.
    % We throw away puzzle after checking for validity and read it in again
    % with read_puzzle, which creates logical variables wrapped in functors
    % (e.g. slot("AB") for each slot as it reads in the PuzzleFile.
    read_generic(PuzzleFile, PuzzleVerificationTest),
    read_generic(WordlistFile, Wordlist),
    valid_puzzle(PuzzleVerificationTest),
    !, % Makes the previous stuff final.
    read_puzzle(PuzzleFile, Puzzle),

    solve_puzzle(Puzzle, Wordlist, Solved).
    %print_puzzle(SolutionFile, Solved).

/*
convert_row_to_logical_vars(Row, LogicalRow) :-
    convert_row_to_logical_vars(Row, LogicalRow, "A", "A").

convert_row_to_logical_vars([E|Rest], LogicalRow, RowChar, ColChar) :-
    Char = '\n';
    E = "_"
    ->  

        
    ; 
*/

/*
next_char(Curr, Next) :-
    Code is Curr + 1,
    string_codes(Code,Next).
*/

% Probably need two accumulators, one to hold the slots and one to hold a slot
% as it is being built up square by square.
% Could split the functionality into two predicates and therefore have only
% one accumulator each, but then it would be hard to keep track of where
% we are in the row.
% Don't need a bool to tell if we're reading a slot or not because we can
% just check the length of the AccCurr.
get_slots_from_row(Row, Slots) :-
    % Row, Acc for all slots, Acc for the slot currently being built, output.
    gsfr(Row, [], [], Slots).
% Base case, hit the end of the row with an empty AccCurr.
gsfr([], Acc, [], Acc).
% Second base case, hit the end of the row with AccCurr holding something.
% Need to decide whether to add the slot to the accumulator or not based on
% how long it is (i.e. it must be > 2 in length).
gsfr([], AccSlots, AccCurr, Slots) :-
    length(AccCurr, Len),
(   Len > 2
->  append(AccSlots, AccCurr, AccSlotsNew)
;   AccSlotsNew = AccSlots
),  % Now that we've dealt with the dangling slot, call again with AccCurr = []
    gsfr([], AccSlotsNew, [], Slots).
% Main recursive case. Every time we find a non-# char, add it to AccCurr.
% Once we come across a #, check if AccCurr is long enough (len > 2) to commit 
% to AccSlots. Check for # first, try to fail as soon as possible.
gsfr([E|Es], AccSlots, AccCurr, Slots) :-
(   E = '#'
->  length(AccCurr, Len),
    (   Len > 2
    ->  append(AccSlots, AccCurr, AccSlotsNew)
    ;   AccSlotsNew = AccSlots
    ),
    AccCurrNew = []
;   % E isn't #, so keep building the current slot AccCurr.
    append(AccCurr, E, AccCurrNew)
),  % If E was #, AccCurrNew will be empty [].
    % By building the AccSlotsNew and AccCurrNew args beforehand instead of
    % calling the predicates with the new args immediately after they're
    % decided in a choicepoint, we can get nice tail recursion.
    gsfr(Es, AccSlotsNew, AccCurrNew, Slots).

fill_puzzle_with_vars(Puzzle, FilledPuzzle) :-
    % The accumulator will be for each row in the puzzle.
    fpwv(Puzzle, [], FilledPuzzle).
% Base case, hit the end of the puzzle. Set FilledPuzzle to the accumulator.
fpwv([], Acc, Acc).
% [Row|RemainingRows], Acc (set to [] at the start), FilledPuzzle (returned 
% puzzle).
fpwv([R|Rs], Acc, FilledPuzzle) :-
    fill_row_with_vars(R, NewR),
    append(Acc, NewR, NewAcc),
    fpvw(Rs, NewAcc, FilledPuzzle).


% This magical little snippet creates a free variable:
create_free_var(A) :-
    length(A,1).
% Use like:
% create_free_var(A),
% append(blah, A, blah).

fill_row_with_vars(Row, FilledRow) :-
    % The accumulator will be for each item in the row.
    frwv(Row, [], FilledRow).
% Base case, hit the end of the row. Set FilledRow to the accumulator.
frwv([], Acc, Acc).
% [Slot|RemainingSlots], Acc (set to [] at the start), FilledRow (returned row).
frwv([S|Ss], Acc, FilledRow) :-
(   % If we get an underscore, replace it with a free var.
    S = '_'
->  create_free_var(A),
    % NewAcc has the free variable A in it.
    append(Acc, slot(A), NewAcc)
;   % Otherwise, just leave the current value (# or letter).
    append(Acc, S, NewAcc)
),  frvw(Ss, NewAcc, FilledRow).



% Break up puzzle into rows like this:
% [R|Rs]

print_puzzle(SolutionFile, Puzzle) :-
    open(SolutionFile, write, Stream),
    maplist(print_row(Stream), Puzzle),
    close(Stream).

print_row(Stream, Row) :-
    maplist(put_puzzle_char(Stream), Row),
    nl(Stream).

put_puzzle_char(Stream, Char) :-
    (   var(Char)
    ->  put_char(Stream, '_')
    ;   put_char(Stream, Char)
    ).

valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
    maplist(samelength(Row), Rows).

samelength([], []).
samelength([_|L1], [_|L2]) :-
    same_length(L1, L2).


% solve_puzzle(Puzzle0, WordList, Puzzle)
% should hold when Puzzle is a solved version of Puzzle0, with the
% empty slots filled in with words from WordList.  Puzzle0 and Puzzle
% should be lists of lists of characters (single-character atoms), one
% list per puzzle row.  WordList is also a list of lists of
% characters, one list per word.
%
% This code is obviously wrong: it just gives back the unfilled puzzle
% as result.  You'll need to replace this with a working
% implementation.

solve_puzzle(Puzzle, _, Puzzle).

/*
get_slots(Puzzle, Slots) :-
    fsdfsdfdf.
*/

% Take a list of words and sort them based on frquency:
% E.g. hey, dog, dogs, cat, logs, maths -> maths, dogs, logs, hey, dog, cat
% This allows us to select the next most infrequent word.
/*
sort_words_by_length_frequency(Dasf) :-
    fas.
*/
% Make puzzle into something like this:
% #  AB AC #  AE
% BA BB BC #  BE

