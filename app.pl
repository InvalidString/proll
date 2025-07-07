
app(X, Y, Z) :-
    X = nil,
    Y = Z.

app(X, Y, Z) :-
    X = cons(H, X2),
    Z = cons(H, Z2),
    app (X2, Y, Z2).


?app(
    cons(a, cons(b, nil)),
    cons(c, cons(d, nil)),
    X
).
