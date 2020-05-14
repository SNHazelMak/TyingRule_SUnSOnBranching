# TyingRule_SUnSOnBranching

This set of codes is dedicated to calculating the SU(n) to SO(n) branching rules via a set of graphical rules called "tying rules" as introduced in the work by S. J. Gates, Jr., Yangrui Hu and S. N. Hazel Mak (arXiv:2005.xxxxx). 


## Installation

Download the [LieART](https://lieart.hepforge.org/) Mathematica application written by Robert Feger, Thomas W. Kephart, and Robert J. Saskowski (see [arXiv:1206.6379](https://arxiv.org/abs/1206.6379) and [arXiv:1912.10969](https://arxiv.org/abs/1912.10969)). Also download the TyingRule.m file in the repository. 

In Mathematica, to install these two packages, click "File -> Install...", choose the options "Type of Item to Install: Package" and "Source: From File...", and choose the corresponding files. Click "OK" to finish installation. 

To call the packages in a Mathematica notebook, type

```bash
<< LieART`
<< TyingRule`
```

Note that the LieART package must be called first.


## Usage

### Apply Tying Rule

First, input a SU(n) irreducible representation as a Young Tableau shape. The format would be a list of non-strictly decreasing positive integers, where the i-th entry indicates the number of boxes in the i-th column of the Young Tableau. 

Another information needed is the maximum number of rows in the SO(n) algebra, which we call m.

```bash
YTshape = {5,3,2};
m = 5;
```

To decompose the SU(n) irreducible representation represented by the above Young Tableau shape into a list of SO(n) irreducible representations also in Young Tableau shapes,

```bash
TyingRule[YTshape, m]
```

To convert them into irreducible representations of B_m = SO(2m+1), 

```bash
TyingDisplayAllB[YTshape, m]
```

or D_m = SO(2m), 

```bash
TyingDisplayAllB[YTshape, m]
```


### Dimension Checks for Branching Rules

To check the tying rules give the correct decomposition of SU(n) irreducible representation in SO(n), a metric is the matching of dimensions. The following function works for SU(2m+1) to SO(2m+1) branching rules and it returns the Boolean value "True" if the dimensions match up.

```bash
BranchingA2nBnDimCheck[YTshapes, m]
```

A similar function below works for SU(2m) to SO(2m) branching rules.

```bash
BranchingA2n1DnDimCheck[YTshapes, m]
```

To check that this condition satisfies for all of the Young Tableau shapes with less than or equal to 3 columns, first generate a list that contains all of them,

```bash
allYTshapes = GenerateAllYTShapes[m, 3];
```

Then run the following lines and see if they return "True".

```
And[##] & @@ Array[BranchingA2nBnDimCheck[allYTshapes[[#]], rowmaxno] &, Length[allYTshapes]] == True
And[##] & @@ Array[BranchingA2n1DnDimCheck[allYTshapes[[#]], rowmaxno] &, Length[allYTshapes]] == True
```


## Remarks

1. Tying rules work for Young Tableaux with less than or equal to three columns. 

2. In these codes, we perform the calculations of SU(n) to SO(n) branching rules for n > 6 cases.

