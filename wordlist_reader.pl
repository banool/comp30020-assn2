:-  module(wordlist_reader, [read_wordlist/2]).

% Opens Filename, reads it into Content, and closes it
read_wordlist(Filename, Content) :-
    open(Filename, read, Stream),
    read_lines(Stream, Content),
    close(Stream).

% Predicate to read all the lines of a file.
% Pretty standard. Just build up the final list with repeated calls to
% read_line until you hit the end of file.
read_lines(Stream, Content) :-
    read_line(Stream, Line, Last),
    (   Last = true
    ->  (   Line = []
        ->  Content = []
        ;   Content = [Line]
        )
    ;  Content = [Line|Content1],
        read_lines(Stream, Content1)
    ).

% Reads all the chars in a line. Handles hitting the end of the file as well
% and sets last to true to let read_lines know.
% This is different to the generic read_line because the char is wrapped in a 
% slot(), which allows the words in the wordlist to unify with the slots, which
% will have also been wrapped in slot(), except for # chars.
read_line(Stream, Line, Last) :-
    get_char(Stream, Char),
    (   Char = end_of_file
    ->  Line = [],
        Last = true
    ; Char = '\n'
    ->  Line = [],
        Last = false
    ;   Line = [slot(Char)|Line1],
        read_line(Stream, Line1, Last)
    ).