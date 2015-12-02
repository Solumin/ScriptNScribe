{-# LANGUAGE QuasiQuotes #-}

module BrevePrelude (prelude) where

import Text.RawString.QQ

-- So, funny story: Haskell has no support for multiline strings.
-- Well, not NO support, but certainly no /good/ support. Your default option
-- is something like:
--     "string starts\
--     \ and ends"
-- which evaluates to "string starts and ends".
-- Text.RawString.QQ is a quasiquoter that lets us have pure raw strings -- no
-- escaping necessary! So barring the admittedly weird [r|...|] syntax, we have
-- Breve programs that look exactly how they should.

prelude = [r|
{- Prelude for Breve. Contains useful functions that are included in every
 - program.
 - Categories:
 - List functions: map, foldl, foldr, flatten, length, sum, product, head, tail,
 - init, last, repeat, take, drop, takeWhile, dropWhile, member, filter
 - Music functions: line, chord, arpeggio, transpose, pitchOf, changeOctave
 - Durations: qn, etc.
 - Numbers: floor, ceil, range, abs, sign, remainder, mod, quot, divMod, pi,
 - trig...?, exp, pow
 - Booleans: and, or, not, any, all
 - Functions: id, const, compose, flip
-}

-- General Functions

id = (\ x -> x);

const = (\ a b -> a);

compose = (\ f g x -> f(g(x)));

flip = (\ f y x -> f(x,y));

-- List Functions

map = (\ f ls -> case ls of
        [] -> [];
        (x:xs) -> f(h) : map(f, xs);
    );

foldl = (\ f init ls -> case ls of
        [] -> init;
        (x:xs) -> foldl(f, f(init, x), xs)
    );

foldr = (\ f init ls -> case ls of
        [] -> init;
        (x:xs) -> f(x, foldr(f, init, xs))
    );

length = (\ ls -> foldl((\ len i -> len+1), 0, ls));

sum = (\ ls -> foldl((\ s i -> s + i),0,ls));

product = (\ ls -> foldl((\ p i -> p * i),1,ls));

head = (\ ls -> case ls of (x:_) -> x);

tail = (\ ls -> case ls of (_:xs) -> xs);

repeat = (\ len item -> if len <= 0 then [] else item : repeat(len-1, item));

take = (\ len ls -> case [len, ls] of
        [0, _] -> [];
        [_,[]] -> [];
        [_,(h:t)] -> h : take(len-1, t);
    );

drop = (\ len ls -> case [len, ls] of
        [0, _] -> ls;
        [_,[]] -> [];
        [_,(x:xs)] -> if len <= 0 then ls else drop(len-1,xs)
    );

member = (\ m ls -> case ls of
        [] -> false;
        (x:xs) -> if m == x then true else member(m,xs);
    );

filter = (\ f ls -> case ls of
        [] -> ls;
        (x:xs) -> if f(x) then x : filter(f,xs) else fitler(f,xs);
    );

-- Music Functions

line = (\ ls -> case ls of
        [x] -> x;
        (x:xs) -> x :+: line(xs);
        [] -> (rest 0);
    );

chord = (\ ls -> case ls of
        [x] -> x;
        (x:xs) -> x :=: chord(xs);
        [] -> (rest 0);
    );

changeOctave = (\ note delta -> case note of (p o d) -> (p o+delta d));

pitchOf = (\ note -> case note of (p _ _) -> p);

transpose = (\ note step ->
    newNote = if (step >= 12)
        then transpose(changeOctave(note, 1), step-12)
        else case note of (p o d) -> (p+step o d);
    return if pitchOf(newNote) < pitchOf(note)
        then changeOctave(newNote, 1)
        else newNote;
    );

arpeggio = (\ steps note -> map((\s -> transpose(note,s)), steps));

-- Number Functions

-- Boolean Functions

and = (\ a b -> if a then b else false);

or = (\ a b -> if a then true else b);

any = (\ ls -> foldl(or, false, ls));

all = (\ ls -> foldl(and, true, ls));
|]
