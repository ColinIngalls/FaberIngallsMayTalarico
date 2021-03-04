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
Zbar = (x_0-x_3)*(x_0-x_4)*(x_1-x_3)*(x_1-x_4)*(x_2-x_3)*(x_2-x_4) -- Will Generalize This


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

zbarinRS = (phi(Zbar) % ideal graphI)
mp = map(R,RS) 
zbarinR = mp(zbarinRS)

use R
R1 = R^1
rho = map(R1,R1,{{zbarinR}})
PFbas = (pushFwd(injTinR))_1
ormatt = substitute(pushFwd(injTinR,rho), f_0 => 0)
BasT1 = matrix{{1,
	        diff(g_2*g_1, zbarinR),
		diff(g_2*g_0,zbarinR),
		diff(g_3*g_4,zbarinR),
		diff(g_1,zbarinR),
		diff(g_2,zbarinR),
		diff(g_0,zbarinR),
		diff(g_3*g_1*g_0,zbarinR),
		diff(g_4,zbarinR),
		zbarinR}}
apply(0..9, i -> degree(BasT1_i))
(ChBasisMat(injTinR,rho,BasT1))_0
kernel (ChBasisMat(injTinR,rho,BasT1))_0
det (ChBasisMat(injTinR,rho,BasT1))_0
matt = substitute(PushFWD(injTinR,rho,BasT1,BasT1), f_0 => 0)

MakeM = (Row,Col) -> (substitute(PushFWD(injTinR,rho,Row,Col), f_0 => 0))

--

c1 = addc(BasT1, matt, {2,0,-(2/15)*f_1,injTinR}) 
c2 = addc(c1_1, c1_0, {3,0, -(1/4)*f_2, injTinR})
c3 = addc(c2_1, c2_0, {4,0, (1/8)*f_1^2, injTinR})
c4 = addc(c3_1, c3_0, {5,0, (5/18)*f_2, injTinR})

matt4 = c4_0
c5 = addc(c4_1, c4_0, {6,0, -((matt4_6)_9), injTinR})
c6 = addc(c5_1, c5_0, {7,0, -((matt4_7)_9), injTinR})
c7 = addc(c6_1, c6_0, {8,0, -((matt4_8)_9), injTinR})
c8 = addc(c7_1, c7_0, {9,0, -((matt4_9)_9), injTinR})

-- check that z^2  =  (det ormatt) ^3 up to scale

r1 = addr(BasT1, c8_0, {4,8,2,injTinR})
r2 = addr(r1_1, r1_0, {2,8,-(30/80)*f_1, injTinR})
r3 = addr(r2_1, r2_0, {3,6, -(2/15)*f_1,injTinR})
r4 = addr(r3_1, r3_0, {5,3, (75/2)*(3/50),injTinR})
r5 = addr(r4_1, r4_0, {0,8, -(f_1^2-4*f_3)*(10/48), injTinR})
r6 = addr(r5_1, r5_0, {1,8, f_2, injTinR})



r7 = mulr(r6_1, r6_0, {1,100,injTinR})
r8 = mulr(r7_1, r7_0, {6, (-10), injTinR})
r9 = mulr(r8_1, r8_0, {8, (10), injTinR})
r10 = addr(r9_1, r9_0, {1, 6, (-1/3)*(3*f_1^2-10*f_3), injTinR})
r11 = addr(r10_1, r10_0, {3, 6, (2/75)*(f_1), injTinR})
r12 = mulr(r11_1, r11_0, {3,(15/2),injTinR})
r13 = addr(r12_1,r12_0, {7,8, (-13/40)*f_1,injTinR})
r14 = mulr(r13_1, r13_0, {7,(-2),injTinR})
r15 = swpr(r14_1, r14_0, {0,1,2,4,5,3,6,7,8,9})
r16 = mulr(r15_1, r15_0, {0,1200,injTinR})
r17 = addr(r16_1, r16_0, {5,4, -2, injTinR})
r18 = addr(r17_1, r17_0, {8,3, (-6),injTinR})
