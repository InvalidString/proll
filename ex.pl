t(X) :- X = b.

p() :- q(X), t(X).

q(X) :- s(X).

s(X) :- t(X).
s(X) :- X = a.


?p().
