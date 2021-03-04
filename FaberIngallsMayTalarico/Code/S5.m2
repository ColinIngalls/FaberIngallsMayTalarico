restart
loadPackage "PushForward"
loadPackage "InvariantRing"
loadPackage "pushFWDbasis"
kk = QQ
n = 5
S = kk[x_0..x_(n-1)]
U = matrix{{0,1},{1,0}}

-- G = S5
Ggens = apply(0..(n-2), i -> (id_(S^i))++ U ++ id_(S^(n-2-i)))
G = generateGroup(toList Ggens,kk, OrderBound => n!)
#G --Should be 120

-- T invariant in G
Tgens = (invariantRing(S,G))_0
T = kk[f_0..f_(n-1),Degrees=>apply(Tgens,degree)]
injTinS = map(S,T,Tgens)

-- H = S3xS2 
PermVar = {3,2}

PermIns = {0,PermVar_0}
Hgens = {}
m = #PermVar
i = 1
while i < m do(
    PermIns = append(PermIns, PermVar_(i-1)+PermVar_i);
    i = i+1; )
i = 1
while i < m+1 do (
    I = PermIns_(i-1);
    J = PermIns_(i)-2;
    if I == J then H_i = {I} else H_i = I..J;
    if I == J then h_i = {I,I+1} else h_i = I..(J+1);
    i = i+1; )
i = 1; Hset = {};
while i < m+1 do (
    for j in H_i do (
	Hset = append(Hset, j); );i = i+1)
for i in Hset do (
    Hgens = append(Hgens,Ggens_i);)

H = generateGroup(toList Hgens,kk)
#H --Should be 12

--R invariant under H
Rgens = (invariantRing(S,H))_0
R = kk[g_0..g_(n-1), Degrees => apply(Rgens,degree)]
injRinS = map(S,R,Rgens)


-- Code Below creates Zbar given H (PermVar)
use S
Zbar = {}; Z = {}; i = 1; zbar = 1;
while i < m+1 do (
    for j in h_i do (
	for z in h_i do (
	    if j<z then Zbar = append(Zbar,(x_j-x_z)); ); ); i = i+1 )
for h from 0 to n-1 do (
    for g from 0 to n-1 do (
	if h<g then Z = append(Z,(x_h-x_g)); ); );
for k in Zbar do Z = delete(k,Z)
for k in Z do zbar = zbar*k

-- ZbarinR
RS = kk[join(gens S, gens R),MonomialOrder=>Eliminate n]
phi = map(RS,S)
RgensInRS = apply(Rgens,x->(phi x))
graphI = apply(1..n,i->(gens RS)_(n+i-1) - (RgensInRS)_(i-1))
fromTtoR = apply(0..(n-1),i->(phi(Tgens_i) % ideal graphI))
rho = map(R,RS)
injTinR = map(R,T,toList fromTtoR / rho)


RS = kk[join(gens S, gens R),MonomialOrder=>Eliminate 1]
phi = map(RS,S)
RgensInRS = apply(Rgens,x->(phi x))

graphI = apply(1..n,i->(gens RS)_(n+i-1) - (RgensInRS)_(i-1))

zbarinRS = (phi(zbar) % ideal graphI)
mp = map(R,RS) 
zbarinR = mp(zbarinRS)

use Ro
R1 = R^1
rho = map(R1,R1,{{zbarinR}})
pushFwd(injTinR)
zbarinR.degree
