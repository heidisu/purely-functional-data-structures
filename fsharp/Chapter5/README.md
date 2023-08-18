# Chapter 5

a = t_1 + phi_d - phi_d-1

phi = abs(#f - #r)


hvis lista har n > 2 element, og #f = 1
#f = 1 #r n-1 -> phi_d-1 = n - 2


#f = (n-1)/2 #r = (n-1)/2 => phi_d = 0

t_i = n

a = n + 0 - (n - 2) = 2 = O(1)

## oppgave 5.2

The amortized cost of any operation is defined to
be the actual cost of the operation plus the credits allocated by the operation
minus the credits spent by the operation, i.e.,
a_i = t_i + c_i - c'_i

Hvert tre i heapen allokerer 1 kreditt
bruker når man linker trær

k + 1 steps, k links, hver link bruker k, den ene operasjonen som ikke linker allokerer 1

(k + 1) + c_i - k

a = (K + 1) + 1 - k = 2e

## Oppgave 5.3

A binomial tree of order(= rank) k has 2^{k} nodes  => n noder => log(n) rank

phi = antall trær i heapen

hvis merge tar k links så er 
phi_in = #ts1 + #ts2, 
phi_out = #ts1 + #ts2 - k
a = actual step + sum phi d_i+1 - sum phi d_i
a = actual steps + (#ts1 + #ts2 - k) - (#ts1 + #ts2) = actual steps - k

actual steps av merge er r + k? 

deleteMin

removeMinTree = t steps
deleteMin = t + r + k + () - t = 


