

list(X) :- X = nil.
list(X) :- X = cons(H, T), list(T).



?list(cons(a, nil)).
