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
    {\em SpechtPolynomials} is a package where you can construct {{\tt $m$ }-tuples of young tableaux and use them to compute the (modified) higher Specht polynomials. This is meant to accompany the pre-print "What is the pape called again" found at "arXiv link".  
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
    definingPolynomial
    orderTuples
    nstWord
    wordToFunc
    NST
    allWords
    auxOrdList
    auxordtuple
    youngSymmetrizer
    antiSymmetrize
    Class
    HigherSpechtPolynomial
    Entries
    GroupType
    conjugate
    symbol ^
--makePar
 Node
  Key
    (makePar,List)
    makePar
  Headline
    Makes an object that contains a m-tuple of partitions
  Usage
    par = makePar(P)
  Inputs
    P:
      A list of partitions, given by lists of positive integers. The empty partition is given by an empty set.
  Outputs
    par:List
      List of partition objects.
  Description
    Text
      makePar takes in a list containing, possibly empty, list of positive integers and output list of Partition objects.
    Example
      makePar {{3,2},{},{2}}
--parFromType
 Node
   Key
     (parFromType,List)
     parFromType
   Headline
     Makes a list of all possible m-tuple partitions of given type.
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
       An $m$-tuple $\lambda=(\lambda_1,...,\lambda_m)$ is of type $(n_1,...,n_m})$ where $\lambda_i$ is a partition of size $n_i$. This function takes in a type of partition given as a list containing, possibly 0, integers and outputs a list of all lists of partitions of the given type.
     Example
       parFromType {3,0,1} 
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
      Given a list $\lambda$ of partitions or an mTableaux object, typeFromPar($\lambda$) recovers the type $(n_1,...,n_m)$.
     Example
      P=makePar {{3,2},{},{2}}
      typeFromPar P
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
      Here we show an example.
    Example
      allTypes(3,2)
--allPartitions
Node
  Key
    (allPartitions,ZZ,ZZ)
    allPartitions
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
      A list of all $m$ tuples of partitions with $n$ cells
  Description
    Example
      allPartitions (3,2)
--listToPartition
Node
  Key
    listToPartition
    (listToPartition,List)
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
    Text
      listToPartition creates a Partiition object from the Macaulay2 package SpechtPolynomials
    Example
      listToPartition {2,1}
--toPartition
Node
  Key
    toPartition
    (toPartition,List)
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
      IS THERE SOME REDUNCENY HERE WITH MAKEPAR?
    Example
      toPartition {{3,2},{},{2}}
--numChar
Node
  Key
    (numChar,mTableaux)
    numChar
  Headline
    Resturns the number of cells of a mTableaux object
  Usage
    numChar(P)
  Inputs
    P:
  Outputs
    n:ZZ
  Description
    Example 
      P=(tabFromPar(makePar{{3,2},{},{2}},Entries=>{{0,1,2,3,4},{},{5,6}})) # 0
      numChar P
--numTab
Node
  Key
    numTab
    (numTab,mTableaux)
  Headline
    Returns the length of an mTableaux objects
  Usage
    numTab(M)
  Inputs
    M: mTableaux
  Outputs
    :ZZ
  Description
    Example
      P=(tabFromPar(makePar{{3,2},{},{2}},Entries=>{{0,1,2,3,4},{},{5,6}})) # 0
      numTab P
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
    tabFromPar
    (tabFromPar,List)
  Headline
    Outputs mTableaux from a List of Partitions depending on some optional paramaters.
  Usage
    tabFromPar(P, Entries=>A,NST=>B)
  Inputs
    P: List
      of tableau objects
    A:List  
      of integers that are the entries of the tableaux
    B:Boolean
      which defaults to false and should be changed to true if only NST are desired
      
  Outputs
    :List
      of mTableaux, which depends on the optional paramaters given.
  Description
    Text 
    Example
      P=makePar {{3,2},{},{2}}
      tabFromPar(P)
      tabFromPar(P,NST=>true)
      tabFromPar(P,Entries=>{{0,1,2,3,4},{},{5,6}})
--shift
Node
  Key
    shift
    (shift,mTableaux)
  Headline
    shifts a mTableaux to the right by one.
  Usage
    shift(M)
  Inputs
    M: mTableaux
  Outputs
    :List
  Description
    Example 
      P=(tabFromPar(makePar{{3,2},{},{2}},Entries=>{{0,1,2,3,4},{},{5,6}})) # 0
      shift P
--word
Node
  Key
    (word,mTableaux)
    word
  Headline
    Outputs the word of an mTableaux
  Usage
    word(M)
  Inputs
    M: mTableaux
  Outputs
    :List
      of mTableaux
  Description
    Text 
      The word of a mTableaux object is defined [REF,Yamada] as ...
    Example
      P=(tabFromPar(makePar{{3,2},{},{2}},Entries=>{{0,1,2,3,4},{},{5,6}})) # 0
      word P
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
      The charge of a mTableaux object is defined [REF,Yamada] as ...
    Example
      P=(tabFromPar(makePar{{3,2},{},{2}},Entries=>{{0,1,2,3,4},{},{5,6}})) # 0
      charge P
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
      This function is used to create the row and column statilisers needed in the methods [REF].
    Example
      fillUp ({2,3},4)
--rowPermutations
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
    Example
      P=(tabFromPar(makePar{{3,2},{},{2}},Entries=>{{0,1,2,3,4},{},{5,6}})) # 0
      rowPermutations P
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
    Example
      P=(tabFromPar(makePar{{3,2},{},{2}},Entries=>{{0,1,2,3,4},{},{5,6}})) # 0
      colPermutations P
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
    Example 
      P=(tabFromPar(makePar{{3,2},{},{2}},Entries=>{{0,1,2,3,4},{},{5,6}})) # 0
      hspMonomial(QQ[x1,x2,x3,x4,x5,x6,x7],P,P)
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
    Example
      P=(tabFromPar(makePar{{3,2},{},{2}},Entries=>{{0,1,2,3,4},{},{5,6}})) # 0
      hsp(QQ[x1,x2,x3,x4,x5,x6,x7],P,P)
--definingPolynomial
Node
  Key
    (definingPolynomial,Ring,ZZ)
    definingPolynomial
  Headline
    Outputs the defining polynomial
  Usage
    p=definingPolynomial(R,m)
  Inputs
    R: Ring
    m: ZZ
      a non negative integer, the amount of tableau in the list.
  Outputs
    p:
      Element of the polynomial ring.
  Description
    Text
      What is the definingPolynomial?? is it 
    Example
      definingPolynomial(QQ[x1,x2,x3],3)
--orderTuples
Node
  Key
    (orderTuples,List,List)
    orderTuples
  Headline
    
  Usage
    L=orderTuples(M,N)
  Inputs
    M: List
    N: List
  Outputs
    L: List 
  Description
    Text
    Example
      P1=(tabFromPar(makePar{{3,2},{},{2}},Entries=>{{0,1,2,3,4},{},{5,6}})) # 0
      P2=(tabFromPar(makePar{{3,2},{},{2}},Entries=>{{0,1,2,3,4},{},{5,6}})) # 1
      P3=(tabFromPar(makePar{{3,2},{},{2}},Entries=>{{0,1,2,3,4},{},{5,6}})) # 2
      orderTuples({P1},{P2,P3})
--nstWord
Node
  Key
    (nstWord,List)
    nstWord
  Headline
    Does the thing it's meant to do, this one needs work...
  Usage
    L=nstWord(M)
  Inputs
    M: List
      of integers
  Outputs
    L: List
      list of subsets telling you what numbers can be in which position of an mTableaux of type M, to create a valid Natural Standard Tableau (NST).
  Description
    Text
      Blah Blah insert words here.
    Example
      nstWord {5,0,2}
--allWords
Node
  Key
    (allWords,List)
    allWords
  Headline
    Creates all the possible words of a mTableaux
  Usage
    L=allWords(M)
  Inputs
    M: List
      of integers
  Outputs
    L: List
      of lists.
  Description
    Text
      Creates a list of all possible entries that can appear in the different positions of an mTableaux.
    Example
      allWords {5,0,2}
--wordToFunc
Node
  Key
    (wordToFunc,List)
    wordToFunc
  Headline
    TEXT
  Usage
    L=wordToFunc(M)
  Inputs
    M:List
  Outputs
    L:
  Description
    Text
--auxordtuple
Node
  Key
    (auxordtuple,mTableaux,mTableaux)
    auxordtuple
  Headline
    A method to create a auxOrdList object which has an ordering.
  Usage
    L=auxordtuple(M,N)
  Inputs
    M:
    N:
  Outputs
    L:
  Description
    Text
      Used to create a auxOrdList object.
    Example
      P1=(tabFromPar(makePar{{3,2},{},{2}},Entries=>{{0,1,2,3,4},{},{5,6}})) # 0
      P2=(tabFromPar(makePar{{3,2},{},{2}},Entries=>{{0,1,2,3,4},{},{5,6}})) # 1
      auxordtuple(P1,P2)
--youngSymmetrizer
Node
  Key
    (youngSymmetrizer,mTableaux)
    youngSymmetrizer
  Headline
    A method to create the youngSymmetrizer of a mTableaux
  Usage
    L=youngSymmetrizer(L)
  Inputs
    M:
  Outputs
    L:
  Description
    Example 
       P=(tabFromPar(makePar{{3,2},{},{2}},Entries=>{{0,1,2,3,4},{},{5,6}})) # 0
       youngSymmetrizer P
--antiSymmetrize
Node
  Key
    (antiSymmetrize,RingElement)
    antiSymmetrize
  Headline
    TEXT
  Usage
    l=antiSymmetrize(r)
  Inputs
    r:RingElement
  Outputs
    l:RingElement
  Description
    Text
      takes a ring element and antiSymmetrizes it ""????!?""
    Example
      QQ[x1,x2]
      antiSymmetrize x1
--conjugate
Node
  Key
    (conjugate,mTableaux)
    conjugate
  Headline
    TEXT
  Usage
    l=conjuate(m)
  Inputs
    m:mTableaux
  Outputs
    l:mTableaux
  Description
    Text
      returns the conjuate of an mTableaux
    Example
      P=(tabFromPar(makePar{{3,2},{},{2}},Entries=>{{0,1,2,3,4},{},{5,6}})) # 0
      conjugate P
--HSP
Node
  Key
    (HSP,PolynomialRing,ZZ,ZZ)
    HSP
  Headline
    The function that creates the hgiher SpechtPolynomials
  Description
    Text
    Example
      HSP(QQ[x1,x2,x3],4,1)
      HSP(QQ[x1,x2,x3],4,2)
--Symbols
--NST
Node
  Key
    NST
    [tabFromPar,NST]
  Headline
    An optional argument to force only natural standard tableau to be considered
  Description
    Text
      Used as an optional argument in the ... to only create a list of ..
--Class
Node
  Key
    Class
    [hsp,Class]
  Headline
    An optional argument to create either higher Specht polynomials or modified higher Specht polynomials
  Description
    Text
      Used as an optional argument in the ... to create a list of ..
--GroupType
Node
  Key
    GroupType
    [hsp,GroupType]
  Headline
    An optional argument to create the higher Specht polynomials, for either the young subgroups of the symmetric groups or the groups G(m,1,n)
  Description
    Text
      Used as an optional argument in the ... to create a list of ..
--Entries
Node
  Key
    Entries
    [tabFromPar,Entries]
  Headline
    An optional argument to create a tableau from a partition with inputted entries
  Description
    Text
      Used as an optional argument in the ... to create tableau with specific entries.
--objects
--auxOrdList
Node
  Key
    (auxOrdList)
    auxOrdList
  Headline
    The class of lists of mTableaux, which has an ordering that is defined by the LL ordering.
  Description
    Text
--HigherSpechtPolynomial
Node
  Key
    HigherSpechtPolynomial
    (symbol _, HigherSpechtPolynomial,mTableaux)
  Headline
    The class of HigherSpechtPolynomials
  Description
    Text
    Example
      T=(tabFromPar(makePar{{2,1},{},{1},{}},NST=>true,Entries=>{{0,1,2},{},{3},{}})) # 0
      S=(tabFromPar(makePar{{2,1},{},{1},{}},NST=>true,Entries=>{{0,1,2},{},{3},{}})) # 1
      F=HSP(QQ[x1,x2,x3,x4],4,1)
      F^T
      F_S
      H=HSP(QQ[x1,x2],3,2)
      P=makePar({{},{1},{},{1}})
      H^P
      H_P
--comparisons&
Node
  Key
    (symbol ?, Partition, Partition)
  Headline
    Compare two partitions with respect to the last letter order.
  Description
    Example
      P1=makePar {{3,2},{},{1},{}}
      P2=makePar {{3,1,1},{},{},{}}
      P1?P2
Node
  Key
    (symbol ?, mTableaux, mTableaux)
  Headline  
    Compare two mTableaux with respect to the last letter order.
  Description
    Example
      T=(tabFromPar(makePar{{2,1},{},{1},{}},NST=>true,Entries=>{{0,1,2},{},{3},{}})) # 0
      S=(tabFromPar(makePar{{2,1},{},{1},{}},NST=>true,Entries=>{{0,1,2},{},{3},{}})) # 1
      T?S
Node 
  Key
    (symbol ?, auxOrdList, auxOrdList)
  Headline  
    Compare two mTableaux with respect to the last letter order.
  Description
    Example
      T=(tabFromPar(makePar{{2,1},{},{1},{}},NST=>true,Entries=>{{0,1,2},{},{3},{}})) # 0
      S=(tabFromPar(makePar{{2,1},{},{1},{}},NST=>true,Entries=>{{0,1,2},{},{3},{}})) # 1
      T?S
--operations on HSP
Node
  Key
    (symbol ^, HigherSpechtPolynomial,mTableaux)
  Headline
    Calculates something that I need to write abot.
  Description
    Example
      T=(tabFromPar(makePar{{2,1},{},{1},{}},NST=>true,Entries=>{{0,1,2},{},{3},{}})) # 0
      F=HSP(QQ[x1,x2,x3,x4],4,1)
      F^T
Node
  Key
    (symbol _, HigherSpechtPolynomial,List)
  Headline
    Calculates something that I need to write abot.
  Description
    Example
      T=(tabFromPar(makePar{{2,1},{},{1},{}},NST=>true,Entries=>{{0,1,2},{},{3},{}})) # 0
      F=HSP(QQ[x1,x2,x3,x4],4,1)
      F_T
Node
  Key
    (symbol ^, HigherSpechtPolynomial,List)
  Headline
    Calculates something that I need to write abot.
  Description
    Example
      T=(tabFromPar(makePar{{2,1},{},{1},{}},NST=>true,Entries=>{{0,1,2},{},{3},{}})) # 0
      F=HSP(QQ[x1,x2,x3,x4],4,1)
      F^T
Node
  Key
    (net, HigherSpechtPolynomial)
  Headline
    Calculates something that I need to write abot.
  Description
    Example
      T=(tabFromPar(makePar{{2,1},{},{1},{}},NST=>true,Entries=>{{0,1,2},{},{3},{}})) # 0
      F=HSP(QQ[x1,x2,x3,x4],4,1)
      net(F)

///
