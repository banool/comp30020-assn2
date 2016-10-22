/* 
** Skeleton code provided by Peter Schachte / Les Kitchen.
** Additional code written by me:
** Daniel Porteous porteousd 696965
**
** After opening swipl, run the program like this:
** main("samples/puzzle1", "samples/words1", "samples/attempt1").
** Check how you went by comparing attempt1 and filled1.
**
** Efforts are made to implement tail recursion where possible.
** The main way to do this is to build the arguments for the next call in the
** choicepoints and then having a single call at the end, instead of calling
** the predicate in the choicepoints when the arguments become available.
**
** The two readers are kept in separate files so there is no collision between
** the worker functions. Necessary because the handling of chars in the word
** list is different from a standard reader like is used for the puzzle.
** Instead of doing it like this (where the appropriate chars are wrapped in
** slot functors), it could be done in a separate pass, but this is easier and
** more efficient.
*/

/* 
** Terminology:
** Element: A single point in the puzzle. Would use Square, but want distinct
            first letters for each thing (E for element, S for Slot).
** Slot:    A >= 2 series of squares running horizontally or vertically.
** Row:     A list of elements, which can be '_', '#', or a letter.
** Puzzle:  Entire puzzle, which is a series of rows.
*/

:- ensure_loaded(library(clpfd)).
:- use_module(generic_reader).
:- use_module(wordlist_reader).

/*
** Reads the PuzzleFile and WordListFile into Puzzle and WordList
** Confirms that the puzzle is valid
** Solves the Puzzle for WordList into Solved
** Prints the solved puzzle out to SolutionFile
*/
main(PuzzleFile, WordListFile, SolutionFile) :-
    % Read in Puzzle and WordList.
    read_generic(PuzzleFile, Puzzle),
    read_wordlist(WordListFile, WordList),
    %maplist(slot/1, WordListRaw, WordList),
    %into_slots_wordlist(WordListRaw, WordList),
    valid_puzzle(Puzzle),
    % !, % Makes the previous stuff final.
    solve_puzzle(Puzzle, WordList, Solved),
    print_puzzle(SolutionFile, Solved).


% ============================================================================ %
% Preparing the puzzle with logical variables.
% ============================================================================ %

fill_puzzle_with_vars(Puzzle, FilledPuzzle) :-
    % The accumulator will be for each row in the puzzle.
    fpwv(Puzzle, [], FilledPuzzle).
% Base case, hit the end of the puzzle. Set FilledPuzzle to the accumulator.
fpwv([], Acc, Acc).
% [Row|RemainingRows], Acc (set to [] at the start), FilledPuzzle (returned 
% puzzle).
fpwv([R|Rs], Acc, FilledPuzzle) :-
    fill_row_with_vars(R, NewR),
    append(Acc, [NewR], NewAcc),
    fpwv(Rs, NewAcc, FilledPuzzle).

fill_row_with_vars(Row, FilledRow) :-
    % The accumulator will be for each item in the row.
    frwv(Row, [], FilledRow).
% Base case, hit the end of the row. Set FilledRow to the accumulator.
frwv([], Acc, Acc).
% [Slot|RemainingSlots], Acc (set to [] at the start), FilledRow (returned row).
frwv([E|Es], Acc, FilledRow) :-
    (   % Just append hash chars as they are and keep going.
        E = '#'
    ->  append(Acc, [E], NewAcc)
    ;   (   E = '_'
        ->  % If we get an underscore, replace it with a free var.
            % NewAcc has the free variable slot(_) in it.
            InSlot = slot(_)
        ;   % Otherwise, we got a letter, which we wrap in slot().
            InSlot = slot(E)
        ),
        append(Acc, [InSlot], NewAcc)    
    ),
    frwv(Es, NewAcc, FilledRow).


% ============================================================================ %
% Getting the slots from the puzzle.
% ============================================================================ %

/*
** Gets the slots from the entire puzzle by looking through each horizontal row.
** The puzzle is then transposed (rotated 90 degrees) and each column is checked
** for slots. The magic of this is that row and column slots will have shared
** squares/elements, meaning that when you fill in one slot, parts of other
** slots might be filled in. All these slots are then put into one list.
*/
get_slots(FilledPuzzle, Slots) :-
    get_slots_one_orientation(FilledPuzzle, HSlots),
    transpose(FilledPuzzle, VerticalPuzzle),
    get_slots_one_orientation(VerticalPuzzle, VSlots),
    append(HSlots, VSlots, Slots).

/*
** Takes a puzzle that has been filled with logical variables and checks each
** row for slots. This is orientation agnostic, meaning it expects the incoming
** puzzle to have been rotated already if necesasry.
*/
get_slots_one_orientation(FilledPuzzle, Slots) :-
    gsoo(FilledPuzzle, [], Slots).
% Base case. No rows left so set Slots to the accumulator.
gsoo([], Acc, Acc).
% Spent ages looking for a bug which prolog helped 0% with where I was doing
% this [R:Rs] instead of this [R|Rs]. Kill me.
gsoo([R|Rs], Acc, Slots) :-
    get_slots_from_row(R, RSlots),
    % If this row had no slots, RSlots will be [], in which case the call
    % to append won't change Acc (NewAcc will be the same as Acc).
    append(Acc, RSlots, NewAcc),
    gsoo(Rs, NewAcc, Slots).

/* 
** Gets the slots from a single row in a single pass.
** 
** More interesting than the standard format because there are multiple base
** cases (described within the predicates).
**
** Slots will be a list of slots for that row 
*/
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
    ->  append(AccSlots, [AccCurr], AccSlotsNew)
    ;   AccSlotsNew = AccSlots
    ),  
    % Now that we've dealt with the dangling slot, call again with AccCurr = []
    gsfr([], AccSlotsNew, [], Slots).
% Main recursive case. Every time we find a non-# char, add it to AccCurr.
% Once we come across a #, check if AccCurr is long enough (len > 2) to commit 
% to AccSlots. Check for # first, try to fail as soon as possible.
gsfr([E|Es], AccSlots, AccCurr, Slots) :-
    (   E = '#'
    ->  length(AccCurr, Len),
        (   Len >= 2
        ->  append(AccSlots, [AccCurr], AccSlotsNew)
        ;   AccSlotsNew = AccSlots
        ),
        AccCurrNew = []
    ;   % E isn't #, so keep building the current slot AccCurr.
        append(AccCurr, [E], AccCurrNew),
        AccSlotsNew = AccSlots
    ),  
    % If E was #, AccCurrNew will be empty [].
    % By building the AccSlotsNew and AccCurrNew args beforehand instead of
    % calling the predicates with the new args immediately after they're
    % decided in a choicepoint, we can get nice tail recursion.
    gsfr(Es, AccSlotsNew, AccCurrNew, Slots).


% ============================================================================ %
% Enumerating all the Slot -> Wordlist pairs.
% ============================================================================ %

get_slot_words_pairs(Slots, WordList, Pairs) :-
    % acc for all pairs, acc for currently in construction pair
    gswp(Slots, WordList, [], Pairs).
gswp([], _, Acc, Acc).
gswp([S|Ss], WordList, Acc, Pairs) :-
    % This member call will backtrack upon failure in bagof.
    %member(W, WordList),
    %bagof(WordList, W=S, FittingWords).
    get_words_for_slot(S, WordList, Words),
    append(Acc, [pair(S, Words)], NewAcc),
    gswp(Ss, WordList, NewAcc, Pairs).

get_words_for_slot(Slot, WordList, Words) :-
    gwfs(Slot, WordList, [], Words).
gwfs(_, [], Acc, Acc).
gwfs(Slot, [W|Ws], Acc, Words) :-
(   % Checks if they can be unified without actually doing it.
    not(not(Slot = W))
->  append(Acc, [W], NewAcc)
;   NewAcc = Acc
),  gwfs(Slot, Ws, NewAcc, Words).


% ============================================================================ %
% Solving the puzzle, one slot at a time.
% ============================================================================ %

/*
** The general approach here is to get all the slots and all the words that
** could fit in them. This kind of key->value mapping is updated each time a
** slot is filled. Backtracking takes care of situations where the algorithm
** fills itself into a hole. Fortunately, because the pairs are sorted based on
** how many words there are for each slot, this should happen as infrequently
** as possible.
*/
solve_puzzle(Puzzle, WordList, PuzzleSolved) :-
    % Get a puzzle with logical variables where appropriate.
    fill_puzzle_with_vars(Puzzle, FilledPuzzle),
    % Get the slots from this puzzle.
    get_slots(FilledPuzzle, Slots),
    % For each slot collect the words that would fit in it.
    get_slot_words_pairs(Slots, WordList, Pairs),
    % Call insertion_sort once beforehand.
    % In a perfect world this first sort call would be quicksort.
    % Insertion sort is good for later calls because it will be mostly sorted.
    insertion_sort(Pairs, SortedPairs),
    % This predicate does the bulk of the work.
    insert_words_until_filled(SortedPairs),
    % Done! :)
    PuzzleSolved = FilledPuzzle.

/*
** 1. For each slot, find the words that can fit in the slot.
**    This is based on whether they actually fit: Slot = Word.
** 2. Sort these pairs (slot, words) by the number of words that match.
** 3. Select the slot with the least possible matches and try to bind the words.
**    Keep trying to bind words until you get a successful fit.
** 4. Once you get a successful fit, go through all the pairs and remove all 
**    instances of this word (unless there are two of that word in the wordlist, 
**    deal with this edge case). Can't use the same word twice.
** 5. While there, for each slot check that the words for that slot can still
**    unify with the slot. If not, remove them.
** 6. After this successful fit and removal of the word, re-sort the pairs.
**    Use insertion sort because, except for the first time, the list will be
**    mostly sorted, meaning close to O(n) time.
*/
insert_words_until_filled([]).
insert_words_until_filled([pair(Slot, Words)|Ps]) :-
    first_element(Words, WordToTry),
    (   % Fill in a word. This unification shouldn't fail because the words for 
        % each slot are checked to make sure that they can unify with said slot 
        % after every guess.
        Slot = WordToTry,
        % Now that a word has been filled in, we need to clean up all the pairs.
        % This means removing the just-guessed word as well making sure that the
        % words for each slot would still unify with it (since a letter may have
        % been filled in now). We get back nice pairs with unifiable words.
        clean_up_pairs(WordToTry, Ps, CleanPairs)
    ->  % Success, cleaned up well. This means that this slot is totally done
        % and we can move on. So just do nothing.
        _ = _
    ;   % Cleaning up went wrong (a slot ended up with no possible words), so we
        % delete this word from the list of words for this slot and try again.
        delete_single_element(WordToTry, Words, NewWords),
        RevisedPair = pair(Slot, NewWords),
        append([RevisedPair], Ps, CleanPairs)
    ),  
    % The length of the list of words for each pair may have changed, so sort
    % the pairs again so we minimise the space for the next and future runs.
    % Sorting of pairs is based on how many words left for each slot.
    insertion_sort(CleanPairs, SortedPairs),
    % Go into the next run with the remaining pairs.
    insert_words_until_filled(SortedPairs).


% Get the first element of a list. Useful when you don't want to do [H|Hs].
first_element([E|_], E).

% This predicate does two things:
% 1. Removes an instance of the word we've just put into a slot from all the
%    remaining pairs.
% 2. Checks that the remaining words would still unify successfully with the
%    slot. If they don't, remove them.
clean_up_pairs(Word, Pairs, NewPairs) :-
    cup(Word, Pairs, [], NewPairs).
cup(_, [], Acc, Acc).
cup(Word, [pair(Slot,Words)|Ps], Acc, NewPairs) :-
    first_element(Words, CheckWord),
    length(Word, WLen),
    length(CheckWord, CWLen),
    % If the length of Word is different to WordCheck, an element in the word 
    % list, it means that the length of the slot being checked is different to
    % the word we're trying to delete. Since the length of words will all be 
    % the same in the word list, we can just stop checking early.
    (   WLen = CWLen
    ->  (   % Instead of checking whether this returns false, it would really be 
            % better to just have the delete_single_element predicate return the 
            % original list if the element isn't found. This would avoid the 
            % choicepoint here.
            % Could check to see if Words is > 1 before doing 
            % delete_single_element for a tiny, tiny efficiency boost. 
            delete_single_element(Word, Words, WordsMinusMatch)
        ->  _ = _ % Do nothing, the word wasn't in the list.
        ;   WordsMinusMatch = Words
        )
    ;   WordsMinusMatch = Words
    ),  
    % Take the words (minus the matching word) and check whether they can
    % still unify with the slot (now that a letter may have been filled in).
    % Before doing get_words_for_slot, we could check the length of 
    % WordsMinusMatch. If it is 1, there is only one word that could possibly
    % fit the slot, so we may as well unify them for real. As it stands though
    % the code is quite efficient and backtracking handles this well already.
    get_words_for_slot(Slot, WordsMinusMatch, NewWords),
    % If the length of the list of words for this new slot is 0, it is now
    % impossible to fill this slot. This means we followed the wrong branch
    % and have officially failed, so we kill this predicate here.
    length(NewWords, LenNewWords),
    LenNewWords > 0,
    NewPair = pair(Slot, NewWords),
    append(Acc, [NewPair], NewAcc),
    cup(Word, Ps, NewAcc, NewPairs).

% Returns false if nothing changed.
delete_single_element(_, [], []).
delete_single_element(Element, [Element|Es], Es). % Used to have a cut here.
% Where Element and E are not equal.
delete_single_element(Element, [E|Es], [E|Result]) :-
    delete_single_element(Element, Es, Result).


% ============================================================================ %
% Additional helper predicates.
% ============================================================================ %

% Does insertion sort on pairs based on the length of the words for that slot.
% Based on http://kti.mff.cuni.cz/~bartak/prolog/sorting.html
insertion_sort(Pairs, Sorted):-
    i_sort(Pairs, [], Sorted).
% Base case, hit the end of the original list.
i_sort([], Acc, Acc).
% Main functionality of insertion sort.
% Iterate through the list, trying to insert each item into the new list.
% Keep doing until you have inserted all items into the new list.
i_sort([E|Es], Acc, Sorted):-
    insert(E, Acc, NAcc),
    i_sort(Es, NAcc, Sorted).
% This gets us to the right spot in the list. 
insert(X, [Y|T], [Y|NT]):-
    len_pair(X, LenX),
    len_pair(Y, LenY),
    LenX > LenY,
    insert(X, T, NT).
% We found the spot!
insert(X, [Y|T], [X,Y|T]):-
    len_pair(X, LenX),
    len_pair(Y, LenY),
    LenX =< LenY.
insert(X, [], [X]).


% ============================================================================ %
% Preincluded predicates. Thanks!
% ============================================================================ %

% len_pair(Pair, Length)
% Takes a Pair and returns the length of the list of words in the pair.
len_pair(pair(_, Words), Length) :-
    length(Words, Length).

% Opens the file, prints each row to it and closes it.
print_puzzle(SolutionFile, Puzzle) :-
    open(SolutionFile, write, Stream),
    maplist(print_row(Stream), Puzzle),
    close(Stream).

% Maps the chars of a row to the output stream.
% How to handle each char is determined by put_puzzle_char
print_row(Stream, Row) :-
    maplist(put_puzzle_char(Stream), Row),
    nl(Stream).

% Simply gets the value from inside a slot.
char_from_slot(slot(A), A).

% Goes through the 
put_puzzle_char(Stream, Char) :-
(   var(Char)
->  put_char(Stream, '_')
;   (   Char = '#'
    ->  put_char(Stream, Char)
    ;   char_from_slot(Char, CleanChar),
        put_char(Stream, CleanChar)
    )
).

valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
    maplist(samelength(Row), Rows).

samelength([], []).
samelength([_|L1], [_|L2]) :-
    same_length(L1, L2).
