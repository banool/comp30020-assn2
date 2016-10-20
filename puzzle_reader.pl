:-  module(puzzle_reader, [read_puzzle/2]).

% Opens Filename, reads it into Content, and closes it
read_puzzle(Filename, Content) :-
    open(Filename, read, Stream),
    read_lines(Stream, Content),
    close(Stream).

read_lines(Stream, Content) :-
    read_lines(Stream, Content, 65).

read_lines(Stream, Content, RowChar) :-
    read_line(Stream, Line, Last, RowChar),
    (   Last = true
    ->  (   Line = []
        ->  Content = []
        ;   Content = [Line]
        )
    ;   Content = [Line|Content1],
        RowNew is RowChar + 1,
        read_lines(Stream, Content1, RowNew)
    ).

read_line(Stream, Line, Last, RowChar) :-
    read_line(Stream, Line, Last, RowChar, 65).

read_line(Stream, Line, Last, RowChar, ColChar) :-
    get_char(Stream, Char),
    (   Char = end_of_file
    ->  Line = [],
        Last = true
    ;   Char = '\n'
    ->  Line = [],
        Last = false
    ;   Char = '_'
    ->  (
        string_codes(NewChar, [RowChar, ColChar])
    ;   NewChar = Char
    ),
    Line = [slot(NewChar)|Line1],
    ColNew is ColChar + 1,
    read_line(Stream, Line1, Last, RowChar, ColNew)
    ).