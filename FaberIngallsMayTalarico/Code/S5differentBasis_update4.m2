restart
loadPackage "PushForward"
loadPackage "InvariantRing"
loadPackage "pushFWDbasis"
kk = QQ
n = 5
S = kk[x_0..x_(n-1)]
U = matrix{{0,1},{1,0}}

--Invariant Polinomials
PermVar = {3,2} -- this is the partition of n

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

mulr = method()
mulr(Matrix,Matrix, List) := (bas,mat,N) -> 
(
    i := N_0;
    a := (1/N_1);
    b := (N_1);
    f := N_2;
    BAS := mutableMatrix bas;
    MAT := mutableMatrix mat;
    BAS = columnMult(BAS,i, f(a));
    MAT = rowMult(MAT, i, b);
    return {matrix MAT, matrix BAS}
    )
  
mulc = method()
mulc(Matrix,Matrix, List) := (bas,mat,N) -> 
(
    i := N_0;
    a := (1/N_1);
    b := (N_1);
    f := N_2;
    BAS := mutableMatrix bas;
    MAT := mutableMatrix mat;
    BAS = columnMult(BAS,i, f(a));
    MAT = columnMult(MAT, i, b);
    return {matrix MAT, matrix BAS}
    )


--


inIdeal = (x,I) -> (x%(ideal I) == 0 )
coeff = (x,I) -> (
    len = length I;
    M = (coefficients x)_1;
    for i from 0 to len-1 do (
	v = (coefficients(I_i, Monomials => (monomials x)))_1;
	M = M|v);
    return M)
coeMat = (x,I) -> (
    if inIdeal(x,I) == true then (
	len = length I;
	apply(0..len-1, i-> (fact_i = x//(I_i)));
	apply(0..len-1, i-> (prod_i = (I_i)*(fact_i)));
	prodList = toList(apply(0..len-1, i -> prod_i));
	factList = toList(apply(0..len-1, i -> fact_i));
	return {coeff(x,prodList), factList, ker(coeff(x,prodList))}))
colCheck = (mat,m,n) -> (
    for i from 0 to 9 do (
	if (inIdeal(mat_m_n, mat_i_n) and (i != m)) == true then print{(m,n),(i,n)}))
rowCheck = (mat,m,n) -> (
    for i from 0 to 9 do (
	if (inIdeal(mat_m_n, mat_m_i) and (i != n)) == true then print{(m,n),(m,i)}))
idealCheck = (mat,m,n) -> (
    I = {};
    for i from 0 to 9 do (
	if ((i != m) == true) then (I = append(I, mat_i_n)));
    for i from 0 to 9 do (
	if ((i != n) == true) then (I = append(I, mat_m_i)));
    return coeMat(mat_m_n, I))

rDelete = (mat,m,n) -> (
    rowNum = numgens target mat;
    colNum = numgens source mat;
    I = {};
    for i from 0 to colNum-1 do (
	if ((i != m) == true) then (I = append(I, mat_i_n)));
    matInf = coeMat(mat_m_n, I);
    return matInf)

cDelete = (mat,m,n) -> (
    rowNum = numgens target mat;
    colNum = numgens source mat;
    I = {};
    for i from 0 to rowNum-1 do (
	if ((i != n) == true) then (I = append(I, mat_m_i)));
    matInf = coeMat(mat_m_n, I);
    return matInf)

Form = (mat) -> (
    Mat = {};
    for i from 0 to 9 do (
	Col_i = {};
	for j from 0  to 9 do (
	    if mat_i_j != 0 then (
		Col_i = append(Col_i, 1));
	    if mat_i_j == 0 then (
		Col_i = append(Col_i, 0)));
	Mat = append(Mat, Col_i));
    return transpose matrix Mat)

--
adr = (mat, i, j, elt) -> (addr(mat_1, mat_0, {i,j,elt, injTinR}))
adc = (mat, i, j, elt) -> (addc(mat_1, mat_0, {i,j,elt, injTinR}))
mur = (mat, i, elt) -> (mulr(mat_1, mat_0, {i, elt, injTinR}))
muc = (mat, i, elt) -> (mulc(mat_1, mat_0, {i, elt, injTinr}))

c1 = addc(BasT1, matt, {2,0,-(2/15)*f_1,injTinR}) 
c2 = addc(c1_1, c1_0, {3,0, -(1/4)*f_2, injTinR})
c3 = addc(c2_1, c2_0, {4,0, (1/8)*f_1^2, injTinR})
c4 = addc(c3_1, c3_0, {5,0, (5/18)*f_2, injTinR})
c5 = addc(c4_1, c4_0, {6,0, -(((c4_0)_6)_9), injTinR})
c6 = addc(c5_1, c5_0, {7,0, -(((c5_0)_7)_9), injTinR})
c7 = addc(c6_1, c6_0, {8,0, -(((c6_0)_8)_9), injTinR})
c8 = addc(c7_1, c7_0, {9,0, -(((c7_0)_9)_9), injTinR})
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
c9 = addc(c8_1,r18_0, {3,1,26*f_1,injTinR} )
r19 = addr(r18_1, c9_0, {7,3, (-3/2)*(10/3)*f_1 ,injTinR})
r20 = addr(r19_1, r19_0, {2,3, (3/2)*(-9/20)*f_1 ,injTinR})
r21 = addr(r20_1, r20_0, {7,2, (25/6)*(-21/25) ,injTinR})
c10 = swpc(c9_1,r21_0, {0,1,2,7,3,4,5,6,8,9})
c11 = addc(c10_1, c10_0, {3,2, (5/4)*(-14/5),injTinR} )
c12 = addc(c11_1, c11_0, {2,3, 2, injTinR})
r22 = swpr(r21_1, c12_0, {3,4,0,1,2,5,6,7,8,9})
c13 = addc(c11_1, r22_0, {5,3,(-60/13)*(39/80)*f_1,injTinR})
c14 = swpc(c13_1, c13_0, {0,1,2,5,3,4,6,7,8,9})
c15 = addc(c14_1, c14_0, {6,5,(-2/15)*(5/3),injTinR})
c16 = swpc(c15_1, c15_0, {0,1,2,3,6,4,5,7,8,9})
c17 = addc(c16_1, c16_0, {5,2, (-1/4)*(26/9), injTinR})
c18 = addc(c17_1, c17_0, {6,4, (3), injTinR})
r23 = addr(r22_1, c18_0, {7,6, (-1/6)*(371/240), injTinR})
r24 = addr(r23_1, r23_0, {5,1, (8/9),injTinR})
r25 = addr(r24_1, r24_0, {7,4, (15)*(9/10), injTinR})
r26 = addr(r25_1, r25_0, {8,0, 8/3, injTinR})
r27 = addr(r26_1, r26_0, {7,6, 371/1440, injTinR})
c19 = addc(c18_1, r27_0, {6,1, -6*f_1, injTinR})
c20 = addc(c19_1, c19_0, {7,1, -(rDelete(c19_0, 7,6))_1_1  ,injTinR})
c21 = addc(c20_1, c20_0, {7,2,-(rDelete(c19_0,7,6))_1_2,injTinR})
r28 = swpr(r27_1, c21_0, {0,1,2,3,4,7,6,5,8,9})
r29 = addr(r28_1, r28_0, {5,0,(3292/1350)*f_1, injTinR})
r30 = addr(r29_1, r29_0, {5,4, -(14260/1350), injTinR})
r31 = addr(r30_1, r30_0, {5,8, -(987/1350)*f_1, injTinR})
c22 = addc(c21_1, r31_0, {9,1, -(5/9*f_1*f_2-10/3*f_4) ,injTinR})
c23 = addc(c22_1, c22_0, {9,2, -(-1/2*f_1^2) ,injTinR})
c24 = addc(c23_1, c23_0, {9,3, -(-1/3*f_1) ,injTinR})
c25 = addc(c24_1, c24_0, {9,4, -(-1/3*f_2) ,injTinR})
c26 = addc(c25_1, c25_0, {8,1, 17/9*f_2 , injTinR})
c27 = addc(c26_1, c26_0, {8,3, -1/3, injTinR})
r32 = swpr(r31_1, c27_0, {0,1,2,3,4,5,7,8,6,9})
c28 = swpc(c27_1, r32_0, {0,1,2,3,4,7,8,5,6,9})
r33 = addr(r32_1, c28_0, {4,7,(3/40*f_1), injTinR})
r34 = addr(r33_1, r33_0, {4,8,-(3/40*f_2), injTinR})
r35 = addr(r34_1, r34_0, {3,6,-(20/3*f_1), injTinR})
r36 = addr(r35_1, r35_0, {3,8, (20/3*f_3), injTinR})
r37 = addr(r36_1, r36_0, {2,6, 40*f_2, injTinR})
r38 = addr(r37_1, r37_0, {2,7, -(-10*f_1^2+40*f_3), injTinR})
r39 = addr(r38_1, r38_0, {2,8, -10*f_1*f_2, injTinR})

-- Cannot reduce further

Matt = r39_0  -- Last Matrix from operations
Form(Matt)    -- Outputs the form of the matrix
A = Matt^{5,6,7,8}_{1,2,3,4} -- Lower left Block
B = Matt^{0,1,2,3,4}_{5,6,7,8,9} -- Upper right Block
C = Matt^{5,6,7,8}_{5,6,7,8,9} -- Block we wish to reduce

(q1,r1) = quotientRemainder(C,A) -- gives (q1,r1) s.t A*q1+r1 = C    
-- we hope that there exists y such that yB = r1 so that A*q1 + yB = C
(q2,r2) = quotientRemainder'(A*q1-C,B) -- gives (q2,r2) s.t q2*B + r2 = A*q1-R1 (we hope r2 = 0)
