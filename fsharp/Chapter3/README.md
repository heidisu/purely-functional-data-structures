# Chapter 3

## Exercise 3.1

Antar at treet er perfekt, dvs at n = 2^(x + 1) - 1, da har alle veier fra roten lengde x + 1, og også x + 1 noder, og det er maksimalt hva høyre side kan ha av noder.
Hvis  treet er uperfekt har høyreside < x + 1 noder.

n + 1 = 2^(x + 1) => x + 1 = log(n + 1) => antall noder <= log(n + 1)

## Exercise 3.3

log n passes:

antar lista har n = 2^x elementer, da vil parene gå opp perfekt, og man trenger x = log n passes for å merge alle.
hvis lista har n ulik 2^x så vil den bruke like mange pass som neste tall m = 2^x, som er log n rundet oppover.


## Exercise 3.8

Prove that the maximum depth of a node in a red-black tree of
size n is at most 2|log(n + 1)|.

Induksjon? 

maks antall sorte = hvis alle i treet er sorte = log(n + 1). 
Lengste path hvis det er max antall sorte + max antall røde. røde må være annehver gang, 2 * log(n + 1)
maks dype hvis det er max antall røde noder

  r
 / \
b   b  