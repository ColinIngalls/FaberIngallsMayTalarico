-- Here is the function PushFWD, I have included some tests below 




restart
loadPackage "pushFWDbasis"
loadPackage "PushForward"
loadPackage "InvariantRing"
kk=QQ
n=4
S=kk[x_0..x_(n-1)]
U=matrix{{0,1},{1,0}}
Ggens = apply(0..(n-2),i->(id_(S^i)) ++ U ++ id_(S^(n-2-i)))
G = generateGroup(toList Ggens,kk)
#G
-- Modified Tgens
Tgens = {x_0+x_1+x_2+x_3,x_0*x_1+x_0*x_2+x_1*x_2+x_0*x_3+x_1*x_3+x_2*x_3, 
    x_0*x_1*x_2+x_0*x_1*x_3+x_0*x_2*x_3+x_1*x_2*x_3, x_0*x_1*x_2*x_3}
T=kk[f_0..f_(n-1),Degrees=>apply(Tgens,degree)]
injTinS = map(S,T,Tgens)


Hgens = {Ggens_0,Ggens_2}
H = generateGroup(toList Hgens,kk)
#H
-- Modified Rgens
Rgens = {x_2+x_3, x_0+x_1,x_2*x_3,x_0*x_1}
R=kk[g_0..g_(n-1),Degrees=>apply(Rgens,degree)]
injRinS = map(S,R,Rgens)

--We need to do some computation to find the inclusion T into R.

RS = kk[join(gens S, gens R),MonomialOrder=>Eliminate n]
phi = map(RS,S)
RgensInRS = apply(Rgens,x->(phi x))
graphI = apply(1..n,i->(gens RS)_(n+i-1) - (RgensInRS)_(i-1))
fromTtoR = apply(0..(n-1),i->(phi(Tgens_i) % ideal graphI))
rho = map(R,RS)
injTinR = map(R,T,toList fromTtoR / rho)

 (injRinS * injTinR).matrix ==(injTinS).matrix


-- write zbar in terms of R
use S
zbar = (x_0-x_2)*(x_0-x_3)*(x_1-x_2)*(x_1-x_3)

RS = kk[join(gens S, gens R),MonomialOrder=>Eliminate 1]
phi = map(RS,S)
RgensInRS = apply(Rgens,x->(phi x))

graphI = apply(1..n,i->(gens RS)_(n+i-1) - (RgensInRS)_(i-1))


zbarinRS = (phi(zbar) % ideal graphI)
mp = map(R,RS) 
zbarinR = mp(zbarinRS)

--
use R
basisToverR = matrix{{1,g_0-g_1,g_2+g_3,g_2-g_3,g_0*g_3,zbarinR}}
R1 = R^1
rho = map(R1,R1,{{zbarinR}})
pushFwd(injTinR,rho)
print 
use T
matt = substitute(PushFWD(injTinR,rho,basisToverR,basisToverR),f_0=>0)
print matt
MakM = (i,j) -> (substitute(PushFWD(injTinR,rho,i,j),f_0=>0))


M1 = addr(basisToverR,matt,{0,4,(f_2/2),injTinR})
MakM(M1_1,basisToverR)
    
M2 = addc(basisToverR,M1_0, {4,0,-(f_2/2),injTinR})
MakM(M1_1,M2_1)

M3 = addr(M1_1,M2_0,{0,2,(f_1/3),injTinR})
MakM(M3_1,M2_1)

M4 = addc(M2_1,M3_0,{2,0,-(f_1/3),injTinR})

M5 = addc(M4_1,M4_0,{5,0,-(1/3)*f_1^2-4*f_3,injTinR})

M6 = swpr(M3_1,M5_0,{4,1,3,2,0,5})

M7 = swpc(M5_1, M6_0, {0,2,5,1,3,4})

RowBas = M6_1
ColBas = M7_1
NewMatt = M7_0
substitute(PushFWD(injTinR,rho,RowBas,ColBas),f_0=>0)

tbt = matrix{{NewMatt_1_3,NewMatt_2_3},{NewMatt_1_4,NewMatt_2_4}}
det tbt
--Scale f_1 by (27^(1/6)/2^(1/3))
--      f_2 by (1/27)^(1/4)
--      f_3 by (1/256)^(1/3)