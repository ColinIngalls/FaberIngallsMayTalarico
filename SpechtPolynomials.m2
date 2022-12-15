newPackage(
        "SpechtPolynomials",
        Version => "1.0",
        Date => "May 25, 2022",
        Authors => {{Name => "Marco Talarico"}},
        Headline => "Higher Specht Polynomials",
        DebuggingMode => true,
        PackageExports => {
        "SpechtModule"
        }
        )


-- exporting all functions and types

export{"makePar"}
export{"parFromType"}
export{"typeFromPar"}
export{"allTypes"}
export{"allPartitions"}
export{"listToPartition"}
export{"toPartition"}

export{"mTableaux"}
export{"mtab"}
export{"numChar"}
export{"numTab"}
export{"nstWord"}
export{"allWords"}
export{"wordToFunc"}

export{"tabFromPar","NST","Entries"}
export{"shift"}
export{"word"}
export{"charge"}
export{"fillUp"}
export{"rowPermutations"}
export{"colPermutations"}
export{"youngSymmetrizer"}

export{"hspMonomial"}
export{"hsp","Class","GroupType"}
export{"antiSymmetrize"}
export{"definingPolynomial"}

export{"auxOrdList"}
export{"auxortuple"}
export{"orderTuples"}

export{"HigherSpechtPolynomial"}
export{"HSP"}

-- 'm'-Partitions  --

makePar = method(TypicalValue => List)
makePar List := listOfPartitions -> apply(listOfPartitions, p -> new Partition from p);

parFromType = method(TypicalValue => List)
parFromType List := par -> (
    bInd := apply(#par, i-> 0);
    parList := toList apply(par, i-> partitions i);
    bInd = toSequence bInd;
    eInd := {};
    for i in parList do (
	if (#i<2) then (
	    eInd = append(eInd,0);
	    continue;
	    );
	if (#i>1) then (
	    eInd = append(eInd,#i-1);
	    );
	);
    eInd = toSequence eInd;
    pars := new List;
    for indx in bInd..eInd do (
	pars = append(pars, toList apply(0..#par-1, i-> (parList_i)_(indx_i)));
	);
    return pars;
    )

typeFromPar = method(TypicalValue => List);
typeFromPar List := mPar -> toList apply(mPar, p -> sum toList p);


allTypes = method(TypicalValue => List);
allTypes (ZZ,ZZ) := (m,n) -> (
    partList := toList apply(partitions n, p -> toList p);
    newList := {};
    for par in partList do (
	if (#par < m) then (
	    while (#par < m) do (
		par = append(par,0);
		);
	    for perm in (toList set permutations par) do (
		newList = append(newList,perm);
		);
	    continue;
	    );
	if (#par == m) then (
	    for perm in (toList set permutations par) do (
		newList = append(newList,perm);
		);
	    continue;
	    );
	);
    return newList;
    )

allPartitions = method(TypicalValue => List);
allPartitions (ZZ,ZZ) := (m,n) -> (
    types := allTypes(m,n);
    return flatten toList apply(types, t -> parFromType t);
    )

listToPartition = method(TypicalValue => Partition)
listToPartition List := l -> return new Partition from l;

toPartition = method(TypicalValue => List)
toPartition List := l -> return apply(l,i->listToPartition i);


conjugate List := mPar -> (
    return apply(mPar, par -> conjugate par);
    )


--  'm'-Tableaux

mTableaux = new Type from List;

mtab = method(TypicalValue => mTableaux);
mtab List :=  tabs ->(
    tabList := new mTableaux;
    apply(tabs, t -> (tabList = append(tabList, t)));
    return tabList;
    )

net YoungTableau := tableau ->( -- This is Jonathan's Nino's code I needed to modify
    if #values tableau == 0 then return "-";
    if #(tableau#values) == 0 then return "-";
    l := tableauToList tableau;
    corner := #(tableau#partition) ;
    tableauNet:= "|" ;
    for i to corner-2 do tableauNet = tableauNet || "|";
    for i to numcols tableau-1 do (
	column:= tableau_i;
	columnString := " "|column#0;
	for j from 1 to #column-1 do columnString = columnString|| " "|column#j;
	for j from #column to corner -1 do columnString = columnString || " |" ;
    	corner = #column;
	tableauNet = tableauNet|columnString;
	);
    columnString := " |";
    for i to corner-2 do columnString= columnString || " |";
    tableauNet = tableauNet | columnString;
    tableauNet
)

numChar = method(TypicalValue => ZZ)
numChar mTableaux := mtab -> (
    n := 0;
    for i from 0 to #mtab-1 do (
	n = n+(size mtab_i);
	);
    return n
    )

numTab = method(TypicalValue => ZZ)
numTab mTableaux := mtab -> (
    return #mtab;
    )

partition List := mtab -> return apply(mtab, i-> mtab#partition);

nstWord = method(TypicalValue => List);
nstWord List := tp -> ( --given a list of integers creates subsets telling you what numbers can be in which position of an mTableaux?
    n:= sum tp;
    indx:= 0;
    auxList:= new MutableHashTable;
    elts := toList (0..n-1);
    nstword := {};
    for i from 0 to #tp-1 do (
	if tp_i == 0 then (
	    nstword = append(nstword,{});
	    continue;
	    );
	auxList = {};
	for j from 0 to tp_i-1 do (
	    auxList = append(auxList,indx);
	    indx = indx+1;
	    );
	nstword = append(nstword,auxList);
	);
    return nstword;
    )

allWords = method(TypicalValue => List);
allWords List := tp -> ( -- creates a list of all possible entries that can appear in the different positions of an mTableaux
    if #tp == 0 then return {};
    n := sum tp;
    if n==0 then return apply(tp, i-> new MutableHashTable);
    if #tp == 1 then return {nstWord tp};
    m := #tp;
    charSet := toList (0..n-1);
    wrdList := {};
    auxList := {};
    sbList := {};
    auxSet:= {};
    rem := {};
    for i from 0 to #tp-1 do (
        if i==0 then (
	    wrdList = apply(subsets(charSet,tp_i), i-> {sort i});
	    continue;
	    );
	for setList in wrdList do (
	    sbList = flatten setList;
	    auxSet = toList ((set charSet)-(set sbList));
	    auxList = subsets(auxSet,tp_i);
	    for othSet in auxList do (
		wrdList = append(wrdList, append(setList,sort othSet));
		);
	    wrdList = delete(setList,wrdList);
	    );
	);
   return wrdList;
   )

wordToFunc = method(TypicalValue => List);
wordToFunc List := wrd -> (
    func := {};
    h := new MutableHashTable;
    for l in wrd do (
	if #l == 0 then (
	    func = append(func, new MutableHashTable);
	    continue;
	    );
        h = new MutableHashTable;
	apply(#l, n -> (h#n = l_n));
        func = append(func, h);
	);
   return func;
   );

-- comparisons and last letter --

Partition ? Partition := (p,q) -> (
    if (sum toList p) > (sum toList q) then return symbol >;
    if (sum toList p) < (sum toList q) then return symbol <;
    P := toList p;
    Q := toList q;
    if P == Q then return symbol ==;
    n := min(#P,#Q);
    if n==0 then (
	if #P == 0 then return symbol <;
	if #Q == 0 then return symbol >;
	);
    m := null;
    print "not";
    for i from 0 to n-1 do (
	m = P_i - Q_i;
	if m>0 then return symbol <;
	if m<0 then return symbol >;
	);
    )



mTableaux ? mTableaux := (T,V) -> (
    lastLetter := 0;
    if numChar T < numChar V then return symbol <;
    if numChar T > numChar V then return symbol >;
    if numTab T < numTab V then return symbol <;
    if numTab T > numTab V then return symbol >;
    if numChar T == 0 then return symbol ==;
    n := numChar T;
    m := numTab T;
    entrieT := apply(T, i-> entries i);
    entrieV := apply(V, i-> entries i);
    isLL := false;
    while isLL == false do (
	n=n-1;
	if (position(flatten entrieT, i->i==n) != position(flatten entrieV,i->i==n)) then (
	    isLL = true;
	    );
       );
    posT  := position(entrieT, i-> member(n,i));
    posV  := position(entrieV, i-> member(n,i));
    if posT < posV then return symbol <;
    if posT > posV then return symbol >;
    rowT := position(apply(numRows T_posT, i-> (T_posT)^i), j-> member(n,j));
    rowV := position(apply(numRows V_posV, i-> (V_posV)^i), j-> member(n,j));
    if rowT < rowV then return symbol >;
    if rowT > rowV then return symbol <;
    return symbol ==;
    )

-- Generating Natural Standard Tableaux and Standard Tableaux --


tabFromPar = method(TypicalValue => List, Options => {Entries => {-1}, NST => false});
tabFromPar List := o->par -> (
    if #par == 0 then return mtab {};
    tabList := {};
    for p in par do (
	if #p == 0 then tabList = append(tabList, {youngTableau(new Partition from {},{})});
	if #p > 0 then tabList = append(tabList, toListOfTableaux standardTableaux p);
	);
    beg := toSequence apply(tabList, t-> 0);
    fin := toSequence apply(tabList, t -> #t-1);
    runover := beg..fin;
    entr := {};
    tp := typeFromPar par;
    if (o.Entries != {-1}) then (
	if (apply(o.Entries , i->#i) == tp) then  (
	    entr = o.Entries;
	    );
	);
    wrd := {};
    entriesToUse := {};
    if #entr != 0 then entriesToUse = append(entriesToUse,entr);
    if #entr == 0 then (
	if o.NST == true then (
	    entriesToUse = {nstWord tp};
	    );
	if o.NST == false then (
	    entriesToUse = allWords tp;
	    );
	);
    mtabList := {};
    tb := new YoungTableau;
    fn := {};
    newWrd := {};
    for indx in runover do (
	wrd = toList apply(0..#indx-1, i-> (tabList_i)_(indx_i));
	for ent in entriesToUse do (
	    fn = wordToFunc ent;
	    newWrd = {};
	    for i from 0 to #wrd-1 do (
		tb = youngTableau((wrd_i)#partition, apply(entries (wrd_i), j -> (ent_i)_j));
		newWrd = append(newWrd,tb);
		);
	    mtabList = append(mtabList,mtab newWrd);
	    );
    	);
    return sort mtabList;
    )

conjugate YoungTableau := (tab) -> (
    if (entries tab == {}) then (return tab);
    entrie := flatten toList apply(0..numColumns(tab)-1,i->tab_i);
    listPar := conjugate tab#partition;
    par := new Partition from listPar;
    tab = youngTableau(par,entrie);
    sortColumnsTableau youngTableau(par,entrie);
    return tab
    )


conjugate mTableaux := mtab -> return apply(mtab,i-> conjugate i);

shift = method(TypicalValue => List)
shift mTableaux := mtab -> (
    mtabshift := {};
    for i from 0 to #mtab-1 do (
	mtabshift = append(mtabshift,mtab_((i-1)%#mtab));
	);
    return mtabshift;
    )

word = method(TypicalValue => List)
word mTableaux := (tabList) -> (
    w := {};
    for tab in tabList do (
	if entries tab == {} then continue else w = append(w,readingWord tab);
	);
    return flatten w;
    )

charge = method(TypicalValue => List)
charge mTableaux := (tabList) -> (
     w := word(tabList);
     v := w;
     p := 0;
     q := 0;
     for n from 0 to #w-2 do (
	 p = position(w,i-> i == n);
	 q = position(w,i-> i == n+1);
	 if (p < q) then ( v = replace(q,v_p,v););
	 if (q < p) then ( v = replace(q,(v_p)+1,v););
	 );
     return v;
     )


fillUp = method(TypicalValue => List)
fillUp (List,ZZ) := (l,m) -> (
    if isSubset( set(0..m-1),set l) then return l;
    newList := {};
    ind := 0;
    for i from 0 to m-1 do (
	if member(i,l) then (
	    newList = append(newList,l_ind);
	    ind = ind+1;
	    );
	if member(i,l) == false then newList = append(newList,i);
	);
    return newList;
    )

rowPermutations = method(TypicalValue => List);
rowPermutations mTableaux := tab -> (
    if (#tab == 0) then return {};
    n := numChar tab;
    m := numTab tab;
    if n==0 then return {};
    perm := set {};
    r:= 0;
    for t in tab do (
	if entries t == {} then continue;
	r = numrows t;
	perm = perm + set flatten toList apply(0..r-1, i-> apply(permutations t^i, w -> fillUp(w,n)));
	);
    perm2 := perm;
    p := {};
    q := {};
    done := true;
    while done do (
	for s in subsets(perm,2) do (
	    p = (toList s)_0;
	    q = (toList s)_1;
	    perm2 = perm2 + set {q_p};
	    );
        if (#perm == #perm2) then done = false;
	perm = perm+perm2;
	);
    return sort toList perm;
    )


colPermutations = method(TypicalValue => List);
colPermutations mTableaux := tab -> rowPermutations conjugate tab;

youngSymmetrizer = method(TypicalValue => List);
youngSymmetrizer mTableaux := T -> (
    permSet := {};
    for r in rowPermutations T do (
	for c in colPermutations T do (
	    permSet = append(permSet,{permutationSign c,c,r});
	    );
	);
    return permSet;
    )

-- Generating Higher Specht Polynomials --

hspMonomial = method(TypicalValue => List);
hspMonomial (PolynomialRing,mTableaux,mTableaux) := (rng,T,V) -> (
    if (numChar T != numChar V) then error "tableaux lists are not of the same size";
    x := gens rng;
    if (numChar T != numgens rng) then error "tableaux lists and polynomial generators do not match in size";
    w := word T;
    v := charge V;
    return product toList apply(0..numgens rng-1, i-> x_(w_i)^(v_i));
    )

hsp = method(TypicalValue => List, Options => {Class => 1, GroupType => 1});
hsp (PolynomialRing,mTableaux,mTableaux) := o->(rng,T,V)-> (
    if (numChar T != numChar V) then error "tableaux lists are not of the same size";
    x := gens rng;
    if (numChar T != numgens rng) then error "tableaux lists and polynomial generators do not match in size";
    m := hspMonomial (rng,T,V);
    h := (map(rng,ZZ)) 0;
    permSet := youngSymmetrizer(T);
    if (o.Class == 1) then apply(permSet, p -> h = h + (p_0)*permutePolynomial(p_1,permutePolynomial(p_2,m)));
    if (o.Class == 2) then apply(permSet, p -> h = h + (p_0)*permutePolynomial(p_2,permutePolynomial(p_1,m)));
    if (o.GroupType == 1) then return h;
    extraMonomial := product flatten toList apply(0..#T-1, t -> apply(entries (T_t), i-> x_i^t));
    M := numTab T;
    if (o.GroupType == 2) then return extraMonomial*(sub(h,toList apply(x, k -> k=>k^M)));
    )

antiSymmetrize = method(TypicalValue => RingElement)
antiSymmetrize RingElement := p -> (
    R := ring p;
    X := gens R;
    n := numgens R;
    perm := apply( permutations n, i-> (permutationSign i,i));
    h := R#0;
    for g in perm do (
	h = h + (g_0)*permutePolynomial(g_1,p);
	);
    return h;
    )

definingPolynomial = method(TypicalValue => RingElement);
definingPolynomial (Ring,ZZ) :=  (R,m) -> (
    if m<1 then error "please eneter a non-zero number of tableaux";
    X := gens R;
    par := {};
    groupType := 1;
    if m==1 then (
	par = {last partitions numgens R};
	);
    if m>0 then (
	groupType = 2;
	for i from 0 to m-1 do (
	    if i != 0 then par = append(par, new Partition from {});
	    if i == 1 then par = append(par , last partitions numgens R);
	    );
	);
    T := first tabFromPar par;
    return hsp(R,T,T, GroupType => groupType);
    );



-- Ordering From Yamada

auxOrdList = new Type from List;



auxordtuple = method(TypicalValue=> auxOrdList);
auxordtuple (mTableaux,mTableaux) := (T,V) -> new auxOrdList from {T,V};



auxOrdList ? auxOrdList := (T,V) -> (
    A := T_0;
    C := T_1;
    B := V_0;
    D := V_1;
    if A < B then return symbol >;
    if A > B then return symbol <;
    c1 := rsort charge C;
    c2 := rsort charge D;
    if (new Partition from c1) < (new Partition from c2) then return symbol <;
    if (new Partition from c1) > (new Partition from c2) then return symbol >;
    if C < D then return symbol <;
    if C > D then return symbol >;
    return symbol ==;
    )

orderTuples = method(TypicalValue => List);
orderTuples (List,List) := (tabs1,tabs2)-> (
    tupleList := {};
    for t1 in tabs1 do (
	for t2 in tabs2 do (
	    tupleList = append(tupleList, auxordtuple(t1,t2));
	    );
	);
    tupleList = sort tupleList;
    return apply(tupleList, toList)
    )



-- High Specht Functions --

HigherSpechtPolynomial = new Type from MutableHashTable;



HSP = method(TypicalValue=> HigherSpechtPolynomial);
HSP (PolynomialRing, ZZ,ZZ) := (rng,m,category) -> (
     hp := new MutableHashTable;
     hp#ring = rng;
     hp#numgens = numgens rng;
     hp#size = m;
     hp#class = category;
     return new HigherSpechtPolynomial from hp;
     )

net HigherSpechtPolynomial := (hp) -> (
    if (hp#class == 1) then return "Higher Specht Polynomials over G("|(net hp#size)|",1,"|(net hp#numgens)|") of type F";
    if (hp#class == 2) then return "Higher Specht Polynomials over G("|(net hp#size)|",1,"|(net hp#numgens)|") of type H";
    return "";
    )


HigherSpechtPolynomial_mTableaux := (h,T)-> (
    par := toList apply(T, t -> t#partition);
    tabList := apply(orderTuples({T},tabFromPar par), t -> t_1);
    hs := apply(tabList, V -> hsp(h#ring,T,V, Class=> h#class));
    return (tabList, hs);
    )

HigherSpechtPolynomial^mTableaux := (h,T)-> (
    par := toList apply(T, t -> t#partition);
    tabList := apply(orderTuples(tabFromPar par,{T}), t -> t_0);
    hs := apply(tabList, V -> hsp(h#ring,V,T, Class=> h#class));
    return (tabList, hs);
    )

HigherSpechtPolynomial_List := (h,par) -> (
    bTabList := tabFromPar(par, NST => true);
    tTabList := tabFromPar par;
    tabList := orderTuples(bTabList,tTabList);
    return (toList set tabList, apply(tabList, T -> hsp(h#ring,T_0,T_1,Class=>h#class)));
    )

HigherSpechtPolynomial^List := (h,par) -> (
    bTabList := tabFromPar(par);
    tTabList := tabFromPar par;
    tabList := orderTuples(bTabList,tTabList);
    return (toList set tabList, apply(tabList, T -> hsp(h#ring,T_0,T_1,Class=>h#class)));
    )
beginDocumentation()

doc ///
--package
 Node
  Key
   SpechtPolynomials
  Headline
      A package to compute higher Specht Polynomials for the complex reflection groups G(m,p,2)
  Description
   Text
    {\em SpechtPolynomials} computes the .... add Description
  Subnodes
    makePar
    parFromType
    typeFromPar
    allTypes
    allPartitions
    listToPartition
    toPartition
    numChar
    numTab
    mTableaux
    mtab
    tabFromPar
    shift
    word
    charge
    fillUp
    rowPermutations
    colPermutations
    hspMonomial
    hsp
--makePar
 Node
  Key
    (makePar,List)
    makePar
  Headline
    Makes an object that contains a p-tuple of partitions
  Usage
    par = makePar(P)
  Inputs
    P:List
      A list of partitions, including the empty partition, given by lists of positive integers or an empty list.
  Outputs
    par:List
      List of partition objects.
  Description
    Text
      This function takes in a list containing, possibly empty, list of positive integers and output list of Partition objects.
    Example
      makePar({{2,1},{},{1}})
--parFromType
 Node
   Key
     (parFromType,List)
     parFromType
   Headline
     Makes a list of all possible partitions of given type
   Usage
     L = parFromType(type)
   Inputs
     type:List
       A list of positive integers, possibly 0.
   Outputs
     L:List
       List of lists of partitions of given type.
   Description
     Text
       This function takes in a type of partition given as a list containing, possibly 0, integers and outputs a list of all lists of partitions of the given type.
     Example
       parFromType({3,2,2})
--typeFromPar
 Node
   Key
    (typeFromPar,List)
    typeFromPar
   Headline
    Recovers the type of the given list of partitions
   Usage
    t=typeFromPar(P)
   Inputs
    P:List
      A list of partitions
   Outputs
    t:List
      The type of the given list of partition given as a list of integers.
   Description
     Text
      Given a list $\lambda$ of partitions of type $(n_1,...n_m)$, typeFromPar($\lambda$) recovers the type $(n_1,...,n_m)$.
--allTypes
 Node
  Key
    (allTypes,ZZ,ZZ)
    allTypes
  Headline
    A function to return all the possible types of a specific configuration.
  Usage
    allTypes(m,n)
  Inputs
    m:ZZ
      The number of partitions.
    n:ZZ
      The amount of total cells.
  Outputs
    :
      A list of all types of possible $m$-partitions with $n$-cells.
  Description
    Text
      Given two non negative integers $(m,n)$ this function returns a list of all possible types of $m$-partitions with $n$-cells.
--allPartitions
Node
  Key
    allPartitions
    (allPartitions,ZZ,ZZ)
  Headline
    Returns all partitions of a configuration
  Usage
    a=allPartitions(m,n)
  Inputs
    m:ZZ
      The number of partitions.
    n:ZZ
      The amount of total cells.
  Outputs
    :
      A list of all partitions of a configuration
  Description
    Text
      WIP
--listToPartition
Node
  Key
    (listToPartition,List)
    listToPartition
  Headline
    A function to create a partition object from a list of non negative integers.
  Usage
    listToPartition(P)
  Inputs
    P:
      A list of integers
  Outputs
    :Partition
      A Partition object
  Description
    Example
      listToPartition({2,1})
--toPartition
Node
  Key
    (toPartition,List)
    toPartition
  Headline
    Creates a tuple of partition objects from a list of list of non negative integers.
  Usage
    toPartition(L)
  Inputs
    L: List
      A list of lists of non negative integers.
  Outputs
    : List
      A list of partition objects
  Description
    Text
      WIP
--numChar
Node
  Key
    (numChar,mTableaux)
    numChar
  Headline
    Calculates the number of cells of a mTableaux object
  Usage
    numChar(M)
  Inputs
    M:
  Outputs
    n:ZZ


  Description
    Text
--numTab
Node
  Key
    (numTab,mTableaux)
    numTab
  Headline
    Returns the length of an mTableaux objects
  Usage
    numTab(M)
  Inputs
    M: mTableaux
  Outputs
    :ZZ
  Description
    Text
--mTableaux Type
Node
  Key
    (mTableaux)
    mTableaux
  Headline
    the class of m-tuples of Young tableaux
  Description
    Text
      This type represents a m-tuple of Young tableaux, it is a list of the type Young Tableaux from the package "SpechtModule"
--mtab constructor
Node
  Key
    (mtab,List)
    mtab
  Headline
    Constructor method for the mTableaux object
  Usage
    numTab(L)
  Inputs
    L: List
      A list of tableau objects
  Outputs
    :mTableaux
  Description
    Text
      Takes a list of m Tableau objects (from the package "SpechtModule") constructs a m-tableau object.
--tabFromPar
Node
  Key
    (tabFromPar,List)
    tabFromPar
  Headline
    Outputs all mTableaux from a List of Partitions.
  Usage
    tabFromPar(L)
  Inputs
    L: List
      of tableau objects
  Outputs
    :List
      of mTableaux
  Description
    Text
--shift
Node
  Key
    (shift,mTableaux)
    shift
  Headline
    shifts a mTableaux to the right by one.
  Usage
    shift(M)
  Inputs
    M: mTableaux
  Outputs
    :List
  Description
    Text
--word
Node
  Key
    (word,mTableaux)
    word
  Headline
    outputs the word of an mTableaux
  Usage
    word(M)
  Inputs
    M: mTableaux
  Outputs
    :List
      of mTablea

      ux
  Description
    Text
--charge
Node
  Key
    (charge,mTableaux)
    charge
  Headline
    outputs the charge of an mTableaux
  Usage
    charge(M)
  Inputs
    M: mTableaux
  Outputs
    :List

  Description
    Text
--fillUp
Node
  Key
    (fillUp,List,ZZ)
    fillUp
  Headline
    fills up a set ...
  Usage
    a=fillUp(S,n)
  Inputs
    S:List
    n:ZZ
  Outputs
    a:
  Description
    Text
      used to create permutations.
-- rowPermutations
Node
  Key
    (rowPermutations,mTableaux)
    rowPermutations
  Headline
    output a list of all permutations that stabilise rows
  Usage
    colPermutations(M)
  Inputs
    M: mTableaux
  Outputs
    :List
     of permutations
  Description
    Text
--colPermutations
Node
  Key
    (colPermutations,mTableaux)
    colPermutations
  Headline
    output a list of all permutations that stabilise columns
  Usage
    colPermutations(M)
  Inputs
    M: mTableaux
  Outputs
    :List
     of permutations
  Description
    Text
--hspMonomial
Node
  Key
    (hspMonomial,PolynomialRing,mTableaux,mTableaux)
    hspMonomial
  Headline
    Calculates the monomial given two mTableaux
  Usage
    m=hspMonomial(R,N,M)
  Inputs
    R:PolynomialRing
      polynomial ring
    N: mTableaux
    M: mTableaux
  Outputs
    m:
      a monomial in the polynomial ring R
  Description
    Text
--hsp
Node
  Key
    (hsp,PolynomialRing,mTableaux,mTableaux)
    hsp
  Headline
    Calculate the higher Specht polynomials given two mTableaux
  Usage
    p=hsp(R,N,M)
  Inputs
    R: PolynomialRing
    N: mTableaux
    M: mTableaux
  Outputs
    p:
  Description
    Text
///
end--
