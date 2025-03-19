% Факти про сімейні зв’язки
father(alex, tina).
father(alex, sara).
father(enn, vlad).
father(enn, margaret).
father(vlad, sam).
father(vlad, boris).
father(mike, josephine).
father(sam, serk).
father(sam, jorge).

mother(natella, tina).
mother(natella, sara).
mother(john, vlad).
mother(john, margaret).
mother(tina, sam).
mother(tina, boris).
mother(olga, josephine).
mother(josephine, serk).
mother(josephine, jorge).

% Визначення родинних зв’язків
parent(X, Y) :- father(X, Y).
parent(X, Y) :- mother(X, Y).

sibling(X, Y) :- parent(P, X), parent(P, Y), X \= Y.

brother(X, Y) :- sibling(X, Y), male(X).
sister(X, Y) :- sibling(X, Y), female(X).

son(X, Y) :- parent(Y, X), male(X).
daughter(X, Y) :- parent(Y, X), female(X).

grandparent(X, Y) :- parent(X, Z), parent(Z, Y).
grandfather(X, Y) :- grandparent(X, Y), male(X).
grandmother(X, Y) :- grandparent(X, Y), female(X).

grandchild(X, Y) :- grandparent(Y, X).
grandson(X, Y) :- grandchild(X, Y), male(X).
granddaughter(X, Y) :- grandchild(X, Y), female(X).

uncle(X, Y) :- brother(X, P), parent(P, Y).
aunt(X, Y) :- sister(X, P), parent(P, Y).

nephew(X, Y) :- uncle(Y, X).
nephew(X, Y) :- aunt(Y, X).
niece(X, Y) :- uncle(Y, X).
niece(X, Y) :- aunt(Y, X).

% Гендерні факти (потрібні для визначення братів/сестер)
male(alex). 
male(enn). 
male(john). 
male(vlad). 
male(mike).
male(sam).
male(boris).
male(serk).
male(jorge).

female(natella).
female(enn).
female(tina).
female(sara).
female(margaret).
female(olga).
female(josephine).
