persona(pepe).
persona(juan).
persona(lopez).
soncapos(juan, lopez).
pred(X,suc(X)).
sonanticapos(X,Y) :- soncapos(Y,X).
eof().
