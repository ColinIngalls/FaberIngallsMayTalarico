restart
loadPackage "PushForward"
loadPackage "InvariantRing"
loadPackage "pushFWDbasis"
kk = QQ
n = 5
S = kk[x_0..x_(n-1)]
U = matrix{{0,1},{1,0}}

--Invariant Polinomials
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
    J = PermIns_(i)-1;
    if I == J then H_i = {I} else H_i = I..J;
    if I == J then h_i = {I,I+1} else h_i = I..(J+1);
    i = i+1; )
Tgens = {}
for i from 1 to n do (
    t_i = 0;
    for j from 0 to n-1 do (
	t_i = t_i + x_j^i);
    Tgens = append(Tgens,t_i))
Rgens = {}
for i from 1 to  #PermVar do (
    for j from 1 to #(H_i) do (
	Hg_i = apply(H_i, i->(x_i^j));
	h = 0; N = #(Hg_i);
	for m in Hg_i do (
	    h = h+m;
	    );
	Rgens = append(Rgens,h);
	);
    );

-- T invariant in G
T = kk[f_0..f_(n-1),Degrees=>apply(Tgens,degree)]
injTinS = map(S,T,Tgens)

--R invariant under H
R = kk[g_0..g_(n-1), Degrees => apply(Rgens,degree)]
injRinS = map(S,R,Rgens)


-- Code Below creates Zbar given H (PermVar)
use S
zbar = (x_0-x_3)*(x_0-x_4)*(x_1-x_3)*(x_1-x_4)*(x_2-x_3)*(x_2-x_4) -- Will Generalize This


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
substitute(pushFwd(injTinR,rho), f_0 => 0)

apply(0..9,i->degree((pushFwd(injTinR))_1_i))

    