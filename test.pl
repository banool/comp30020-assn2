% THIS is how you "unpack" arguments from a predicate.
% Not like this [X|Y], that is only for lists.
get_args(pair(X,Y), X, Y).

