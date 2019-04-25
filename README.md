# pure-prolog
Pure prolog interpreter written in Haskell

# Example run

```
PROMPT$ ./prologInterpreter 
load(test).
Cargando test.pl ...
Se cargo correctamente el archivo test.pl
persona(pepe).
true;
false.
persona(ricardo).
false.
persona(X).
X = pepe;

X = juan;

X = lopez;
false.
soncapos(X,X).
false.
soncapos(X,Y).
X = juan , Y = lopez;
false.
soncanticapos(X,Y).
false.
sonanticapos(X,Y).
X = lopez , Y = juan;
false.
pred(robertito, Y).
Y = suc(robertito);
false.
pred(X,X).
false.
persona(X), persona(Y).
X = pepe , Y = pepe;

X = pepe , Y = juan;

X = pepe , Y = lopez;

X = juan , Y = pepe;

X = juan , Y = juan;

X = juan , Y = lopez;

X = lopez , Y = pepe;

X = lopez , Y = juan;

X = lopez , Y = lopez;
false.
pred(X,Y).
X = X0 , Y = suc(X0);
false.
```
