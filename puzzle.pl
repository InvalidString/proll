
noteq(A, B) :- A = B, !, fail.
noteq(A, B).


student(X) :- X = alice.
student(X) :- X = bob.
student(X) :- X = carol.
student(X) :- X = dave.

solution(Java, C, Ocaml, Scala, Linux, Windows, Macos, Openbsd) :-
    student(Java), student(C), student(Ocaml), student(Scala),
    student(Linux), student(Windows), student(Macos), student(Openbsd),
    noteq(Java , C), noteq(Java , Ocaml),
    noteq(Java , Scala), noteq(C , Ocaml),
    noteq(C , Scala), noteq(Ocaml , Scala),
    noteq(Linux , Windows), noteq(Linux , Macos),
    noteq(Linux , Openbsd), noteq(Windows , Macos),
    noteq(Windows , Openbsd), noteq(Macos , Openbsd),
    alice = Java, C = Windows, carol = Macos, noteq(dave , Ocaml), noteq(dave , Linux).




?solution(Java, C, Ocaml, Scala, Linux, Windows, Macos, Openbsd).


