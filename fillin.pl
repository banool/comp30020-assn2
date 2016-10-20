/* 
** Skeleton code provided by Peter Schachte / Les Kitchen.
** Additional code written by me:
** Daniel Porteous porteousd 696965
**
** After opening swipl, run the program like this:
** main("samples/puzzle1", "samples/words1", "samples/attempt1").
** Check how you went by comparing attempt1 and filled1.
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

