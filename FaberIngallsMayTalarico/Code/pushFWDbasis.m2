newPackage(
    "pushFWDbasis",
    DebuggingMode => false
    )

export{"PushFWD","ChBasisMat","swpr","swpc","addr","addc"}

PushFWD = method()     
PushFWD(RingMap,Matrix,Matrix,Matrix) := (f,d,maT,maT2)->
(
     A:=source f;
     B:=target f;
     pols:=f.matrix;
     pM:=source d;
     pN:=target d;
     
     amn:=intersect(ann pM,ann pN);
     C:=B/amn;
     bc:=map(C,B);
     g:=bc*f;     
     M:=pM**C;
     N:=pN**C;
     
     A1:=source g;
     B1:=target g;
     polsg:=g.matrix;
         
     FlatA:=flattenRing A1;
     FA:=FlatA_0;
     iFA:=ideal FA;
     varsA:=flatten entries FlatA_1^-1 vars FA;
     RA:=try(ring source presentation FA) else FA;
     FlatB:=flattenRing B1;
     FB:=FlatB_0;
     iFB:= ideal FB;
     varsB:=flatten entries FlatB_1^-1 vars FB;
     RB:=try(ring source presentation FB) else FB;
     m:=numgens FA;
     n:=numgens FB;
     
     polsg=polsg_{0..(m-1)};
          
     R := try(tensor(RB, RA, Join => false)) else tensor(RB, RA, Join => true);
     xvars := (gens R)_{n..n+m-1};
     yvars := (gens R)_{0..n-1};
     iA:=sub(ideal FA,matrix{xvars});
     iB:=sub(ideal FB,matrix{yvars});
     iGraph:=ideal(matrix{xvars}-sub(pols,matrix{yvars}));
     I:=iA+iB+iGraph;
     inI:=leadTerm I;
     
     r:=ideal(sub(inI,matrix{yvars | splice{m:0}}));     
     for i from 1 to n do
	   if ideal(sub(gens r,matrix{{(i-1):0,1_R,(m+n-i):0}}))!=ideal(1_R) then
     	         error "map is not finite";

     mat:=sub(maT,R);
     mator:=lift(basis(R/(r+ideal(xvars))),R);
     k:=numgens source mat;
     matB:=maT;

     phi:=map(R,B,matrix{yvars});
     toA:=map(A,R,flatten{n:0_A,varsA});
     mapfor:=(b)->(
	  (mons,cfs):=coefficients((phi b)%I,Monomials=>mator,Variables=>yvars);
	  toA cfs	  
	  );

     MATT := mapfor maT;
     MATT = inverse MATT;
     
     mapf := i -> (MATT*(mapfor(i)));
     
     auxN:=ambient N/image relations N;
     ken:=(numgens ambient N) * (numgens source matB);
     mp:=try(map(auxN,,g,matB**gens N)) else map(auxN,A^ken,g,matB**gens N);
     keN:=kernel mp;
     pushN:=(super keN)/keN;
      
     auxM:=ambient M/image relations M;
     kem:=(numgens ambient M) * (numgens source matB);
     mpm:=try(map(auxM,,f,matB**gens M)) else map(auxM,A^kem,g,matB**gens M);
     keM:=kernel mpm;
     pushM:=(super keM)/keM;
    

     matMap:=symbol matMap;
     gR:=maT2**matrix d;
     c:=numgens source gR;
     l:=numgens target gR;
     matMap=mutableMatrix(A,k*l,c);
     
    
     for i1 from 0 to c-1 do
     	  for i2 from 0 to l-1 do
	  (
       	       e:=mapf(gR_i1_i2);
	       for i3 from 0 to k-1 do matMap_(i2+l*i3,i1)=e_0_i3;	       
	   );
     return map(pushN,pushM,matrix matMap);
     )


ChBasisMat = method()     
ChBasisMat(RingMap,ModuleMap,Matrix) := (f,d,maT)->
(
     A:=source f;
     B:=target f;
     pols:=f.matrix;
     pM:=source d;
     pN:=target d;
     
     amn:=intersect(ann pM,ann pN);
     C:=B/amn;
     bc:=map(C,B);
     g:=bc*f;     
     M:=pM**C;
     N:=pN**C;
     
     A1:=source g;
     B1:=target g;
     polsg:=g.matrix;
         
     FlatA:=flattenRing A1;
     FA:=FlatA_0;
     iFA:=ideal FA;
     varsA:=flatten entries FlatA_1^-1 vars FA;
     RA:=try(ring source presentation FA) else FA;
     FlatB:=flattenRing B1;
     FB:=FlatB_0;
     iFB:= ideal FB;
     varsB:=flatten entries FlatB_1^-1 vars FB;
     RB:=try(ring source presentation FB) else FB;
     m:=numgens FA;
     n:=numgens FB;
     
     polsg=polsg_{0..(m-1)};
          
     R := try(tensor(RB, RA, Join => false)) else tensor(RB, RA, Join => true);
     xvars := (gens R)_{n..n+m-1};
     yvars := (gens R)_{0..n-1};
     iA:=sub(ideal FA,matrix{xvars});
     iB:=sub(ideal FB,matrix{yvars});
     iGraph:=ideal(matrix{xvars}-sub(pols,matrix{yvars}));
     I:=iA+iB+iGraph;
     inI:=leadTerm I;
     
     r:=ideal(sub(inI,matrix{yvars | splice{m:0}}));     
     for i from 1 to n do
	   if ideal(sub(gens r,matrix{{(i-1):0,1_R,(m+n-i):0}}))!=ideal(1_R) then
    	         error "map is not finite";

     mat:=sub(maT,R);
     mator:=lift(basis(R/(r+ideal(xvars))),R);
     k:=numgens source mat;
     matB:=maT;

     phi:=map(R,B,matrix{yvars});
     toA:=map(A,R,flatten{n:0_A,varsA});
     mapfor:=(b)->(
	  (mons,cfs):=coefficients((phi b)%I,Monomials=>mator,Variables=>yvars);
	  toA cfs	  
	  );

     MATT := mapfor maT;
     return {MATT, maT, mator}
     )


swpr = method()
swpr(Matrix,Matrix,List) := (bas,mat,N) ->
(
    M  := mutableMatrix mat;
    BAS := mutableMatrix transpose(bas);
    MAT  := matrix(rowPermute(M,0,N));
    BAS  = matrix(transpose(rowPermute(BAS,0,N)));
    return {MAT,BAS}
    )

swpc = method()
swpc(Matrix,Matrix,List) := (bas,mat,N) ->
(
    M  := mutableMatrix mat;
    BAS := mutableMatrix bas;
    MAT  := matrix(columnPermute(M,0,N));
    BAS  = matrix(columnPermute(BAS,0,N));
    return {MAT,BAS}
    )


addr = method()
addr(Matrix,Matrix,List) := (bas,mat, N) ->
(
    i := N_0;
    j := N_1;
    a := N_2;
    f := N_3;
    fa := -f(a);
    MAT := mutableMatrix mat;
    MAT = matrix rowAdd(MAT,i,a,j);
    b := transpose bas;
    e := (gens(source(bas)))_j;
    e = matrix(fa*e);
    BAS := b + e;
    return {MAT, transpose BAS}
    )

addc = method()
addc(Matrix,Matrix,List) := (bas,mat, N) ->
(
    i := N_0;
    j := N_1;
    a := N_2;
    f := N_3;
    MAT := mutableMatrix mat;
    MAT = matrix columnAdd(MAT,i,a,j);
    b := mutableMatrix bas;
    BAS := columnAdd(b,i,f(a),j);
    return {MAT, matrix BAS}
    )
