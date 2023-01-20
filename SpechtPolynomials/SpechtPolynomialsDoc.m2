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
--makePar
 Node
  Key
    makePar
    (makePar,auxOrdList)
    (makePar,List)
    (makePar,mTableaux)
  Headline
    Makes an object that contains a p-tuple of partitions
  Usage
    par = makePar(P)
  Inputs
    P:auxOrdList
      A list of partitions, including the empty partition, given by lists of positive integers or an empty list.
  Outputs
    par:List
      List of partition objects.
  Description
    Text
      This function takes in a list containing, possibly empty, list of positive integers and output list of Partition objects.
--parFromType
 Node
   Key
      parFromType
     (parFromType,List)
     (parFromType,auxOrdList)
     (parFromType,mTableaux)
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
       parFromType {3,0,1} 
--typeFromPar
 Node
   Key
    typeFromPar
    (typeFromPar,List)
    (typeFromPar,auxOrdList)
    (typeFromPar,mTableaux)
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
      Here we show an example.
    Example
      allTypes(3,2)
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
    listToPartition
    (listToPartition,List)
    (listToPartition,auxOrdList)
    (listToPartition,mTableaux)
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
    --Example
      --listToPartition({2,1})
--toPartition
Node
  Key
    toPartition
    (toPartition,List)
    (toPartition,auxOrdList)
    (toPartition,mTableaux)
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
    (mtab,auxOrdList)
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
    (tabFromPar,auxOrdList)
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
--definingPolynomial
Node
  Key
    (definingPolynomial,Ring,ZZ)
    definingPolynomial
  Headline
    Outputs the defining polynomial
  Usage
    p=hsp(R,m)
  Inputs
    R: Ring
    m: ZZ
      a non negative integer, the amount of tableau in the list.
  Outputs
    p:
      Element of the polynomial ring.
  Description
    Text
--orderTuples
Node
  Key
    (orderTuples,List,List)
    orderTuples
  Headline
    Orders two tuples, according to the LL Ordering
  Usage
    L=orderTuples(M,N)
  Inputs
    M: List
    N: List
  Outputs
    L: List
      of the two mTableaux in increasing order according to the LL ordering.
  Description
    Text
      Orders two tuples, according to the LL Ordering ... Add Description of LL here

--nstWord
Node
  Key
    (nstWord,List)
    (nstWord,auxOrdList)
    nstWord
  Headline
    Does the thing it's meant to do, this one needs work...
  Usage
    L=orderTuples(M)
  Inputs
    M: List
      of integers
  Outputs
    L: List
      list of subsets telling you what numbers can be in which position of an mTableaux of shape of M, to create a valid Natural Standard Tableau (NST).
  Description
    Text
      Blah Blah insert words here.
--allWords
Node
  Key
    (allWords,List)
    (allWords,auxOrdList)
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
--wordToFunc
Node
  Key
    (wordToFunc,List)
    (wordToFunc,auxOrdList)
    wordToFunc
  Headline
    TEXT
  Usage
    L=wordToFunc(M)
  Inputs
    M:List
      TEXT
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
      --auxordtuple
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
    Text
      Used to create a auxOrdList object.
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
--HSP

Node
  Key
    (HSP,PolynomialRing,ZZ,ZZ)
    HSP
  Headline
    The function that creates the hgiher SpechtPolynomials
  Description
    Text
--Symbols
--NST
Node
  Key
    NST
  Headline
    An optional argument to force only natural standard tableau to be considered
  Description
    Text
      Used as an optional argument in the ... to only create a list of ..
--Class
Node
  Key
    Class
  Headline
    An optional argument to create either higher Specht polynomials or modified higher Specht polynomials
  Description
    Text
      Used as an optional argument in the ... to create a list of ..
--GroupType
Node
  Key
    GroupType
  Headline
    An optional argument to create the higher Specht polynomials, for either the young subgroups of the symmetric groups or the groups G(m,1,n)
  Description
    Text
      Used as an optional argument in the ... to create a list of ..
--Entries
Node
  Key
    Entries
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
    The class of lists of mTableaux, which has an ordering
  Description
    Text
--HigherSpechtPolynomial
Node
  Key
    (HigherSpechtPolynomial)
    HigherSpechtPolynomial
  Headline
    The class of HigherSpechtPolynomials
  Description
    Text
///
