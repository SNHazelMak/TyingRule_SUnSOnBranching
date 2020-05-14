(* ::Package:: *)

(* ::Subsection:: *)
(*Generate All YT Shapes*)


(* ::Input::Initialization:: *)
ElementRepeat[m_,n_Integer?Positive]:=Sequence@@ConstantArray[m,n]


(* ::Input::Initialization:: *)
GenerateAllYTShapes[rowmaxno_Integer?Positive,colmaxno_Integer?Positive]:=ReverseSort[#] &/@ DeleteDuplicates[Subsets[Array[ElementRepeat[#,colmaxno]&,rowmaxno],colmaxno]];


(* ::Subsection:: *)
(*Small Functions For Tying Rule*)


(* ::Input::Initialization:: *)
YTPad[YTshape_List,rowmaxno_Integer?Positive,columnno_Integer?Positive]:= Transpose[Array[ArrayPad[Table[1,YTshape[[#]]],{0,(rowmaxno-YTshape[[#]])}]&,columnno]];


(* ::Input::Initialization:: *)
ArrayPadLeftRigid[list_List,length_Integer?Positive,padding_] :=  Piecewise[{{ArrayPad[list,{length-Length[list],0},padding],length>Length[list]},{list,length==Length[list]},{Table[padding,length],length<Length[list]}}];


(* ::Input::Initialization:: *)
TieComboAll[YTshape_List,rowmaxno_Integer?Positive,columnno_Integer?Positive]:=Module[
{tietype,tieendposition,tiemaxno,tieall,tiecomboall,out},
tietype = Permutations[ArrayPadLeftRigid[{1,1},columnno,0]];
tieendposition = Array[Max@Position[tietype[[#]],1]&,Length[tietype]];
tiemaxno = Array[YTshape[[tieendposition[[#]]]]&,Length[tieendposition]];
tieall = ArrayReshape[Array[ConstantArray[tietype[[#]],tiemaxno[[#]]]&,columnno],{Total[tiemaxno],columnno}];
tiecomboall = DeleteDuplicates[Subsets[tieall,rowmaxno]];
out = Array[ArrayReshape[tiecomboall[[#]],{rowmaxno,columnno},0] &,Length[tiecomboall]];
out
]


(* ::Input::Initialization:: *)
InArrayQ[list_,element_]:=If[Length@Position[list,x_/;x==element]==0,False,True];


(* ::Input::Initialization:: *)
NoSymTestAdjColumn[YTshape_List,YTremnant_List, col_Integer?Positive]:=Module[
{nosymneed,nosymquota,out},
nosymneed = Count[YTremnant[[1;;YTshape[[col+1]],col;;(col+1)]],{0,1}];
nosymquota = Count[YTremnant[[(YTshape[[col+1]]+1);;YTshape[[col]],1]] ,1];
out = nosymneed>nosymquota;
out
]


(* ::Input::Initialization:: *)
NoSymTest[YTshape_List,YTremnant_List,columnno_Integer?Positive]:=Or[##]& @@Array[NoSymTestAdjColumn[YTshape,YTremnant,#]&,(columnno-1)];


(* ::Subsection:: *)
(*Tying Rule Full Function*)


(* ::Input::Initialization:: *)
TyingRuleNoTie[YTshape_List]:={YTshape};


(* ::Input::Initialization:: *)
TyingRuleTwoColumn[YTshape_List,rowmaxno_Integer?Positive]:=Module[
{YTstruct,tiemaxno,tiecomboall,YTremnant,out},
YTstruct = YTPad[YTshape,rowmaxno,2];
tiemaxno = Min[YTshape];
tiecomboall = Array[ArrayReshape[Table[{1,1},#],{rowmaxno,2}]&,(tiemaxno+1),0];
YTremnant = Array[YTstruct - tiecomboall[[#]]&,Length[tiecomboall]];
out = Array[Total[YTremnant[[#]],{1}]&,Length[YTremnant]];
out
]


(* ::Input::Initialization:: *)
TyingRuleThreeColumn[YTshape_List,rowmaxno_Integer?Positive]:=Module[
{YTstruct,tiecomboall,YTremnant,tieoutsideQ,YTremnantshape,irregularQ,violatesymQ,out},
YTstruct = YTPad[YTshape,rowmaxno,3];
tiecomboall = TieComboAll[YTshape,rowmaxno,3];
YTremnant = Array[YTstruct - tiecomboall[[#]]&,Length[tiecomboall]];
tieoutsideQ = Array[InArrayQ[YTremnant[[#]],-1]&,Length[YTremnant]];
YTremnant = Delete[YTremnant,Position[tieoutsideQ,True]];
YTremnantshape = Array[Total[YTremnant[[#]],{1}]&,Length[YTremnant]];
irregularQ = Array[!OrderedQ[YTremnantshape[[#]],GreaterEqual] &,Length[YTremnantshape]];
YTremnant = Delete[YTremnant,Position[irregularQ,True]];
violatesymQ = Array[NoSymTest[YTshape,YTremnant[[#]],3]&,Length[YTremnant]];
YTremnant =  Delete[YTremnant,Position[violatesymQ,True]];
out = Array[Total[YTremnant[[#]],{1}]&,Length[YTremnant]];
out
]


(* ::Input::Initialization:: *)
TyingRule[YTshaperaw_List,rowmaxno_Integer?Positive]:=Module[
{columnno,YTshape,tiecomboall,YTremnant,tieoutsideQ,YTremnantshape,irregularQ,violatesymQ,out},
If[rowmaxno<Max[YTshaperaw],Abort[]];
If[!OrderedQ[YTshaperaw,GreaterEqual],Abort[]];
YTshape = DeleteCases[YTshaperaw,0];
columnno = Length[YTshape];
out = Piecewise[{{TyingRuleNoTie[YTshape],columnno<=1},{TyingRuleTwoColumn[YTshape,rowmaxno],columnno==2},{TyingRuleThreeColumn[YTshape,rowmaxno],columnno==3},{Abort[],columnno>=4}}];
out
]


(* ::Subsection:: *)
(*SU(n) to SO(n) Projection Matrix Input*)


(* ::Input::Initialization:: *)
ProjAn1Bn[n_]:=Module[
{out},
If[n>=3,
out = Table[KroneckerDelta[i,j]+KroneckerDelta[2n+1-i,j],{i,1,n},{j,1,2n}];
out[[n,n]]=2;
out[[n,n+1]]=2,
Abort[]];
out
]


(* ::Input::Initialization:: *)
ProjAn1Dn[n_]:=Module[
{out},
If[n>=4,
out = Table[KroneckerDelta[i,j]+KroneckerDelta[2n-i,j],{i,1,n},{j,1,2n-1}];
out[[n,n-1]]=1;
out[[n,n+1]]=1,
Abort[]];
out
]


(* ::Subsection:: *)
(*Convert YTs to Dynkin Labels*)


(* ::Input::Initialization:: *)
BYTToDynkinAn[YTshape_List,rowmaxno_Integer?Positive]:=Module[
{out},
If[rowmaxno<Max[YTshape],Abort[]];
If[!OrderedQ[YTshape,GreaterEqual],Abort[]];
out = Array[Count[YTshape,#]&,rowmaxno];
out
]


(* ::Input::Initialization:: *)
BYTToDynkinBn[YTshape_List,rowmaxno_Integer?Positive]:=Module[
{dynkinAn,out},
If[rowmaxno<3,Abort[]];
dynkinAn = BYTToDynkinAn[YTshape,rowmaxno];
out = ProjAn1Bn[rowmaxno][[All,1;;rowmaxno]].dynkinAn;
out
]


(* ::Input::Initialization:: *)
BYTToDynkinDn[YTshape_List,rowmaxno_Integer?Positive]:=Module[
{dynkinAn,proj,conjugate,out},
If[rowmaxno<4,Abort[]];
dynkinAn = BYTToDynkinAn[YTshape,rowmaxno];
proj = ProjAn1Dn[rowmaxno][[All,1;;rowmaxno]].dynkinAn;
If[proj[[rowmaxno-1]]==proj[[rowmaxno]],
out = proj,
conjugate = proj;
conjugate[[rowmaxno]]=proj[[rowmaxno-1]];
conjugate[[rowmaxno-1]]=proj[[rowmaxno]];
out=Sequence[proj,conjugate]
];
out
]


(* ::Subsection:: *)
(*Convert Dynkin Labels to Irreps*)


(* ::Input::Initialization:: *)
DynkinAnToIrrep[dynkin_List]:= Irrep[A][##]& @@ dynkin;


(* ::Input::Initialization:: *)
DynkinBnToIrrep[dynkin_List]:= Irrep[B][##]& @@ dynkin;


(* ::Input::Initialization:: *)
DynkinDnToIrrep[dynkin_List]:= Irrep[D][##]& @@ dynkin;


(* ::Subsection:: *)
(*Tying Rule Display YTs, Dynkin Labels and Irreps*)


(* ::Input::Initialization:: *)
TyingDisplayAllB[YTshape_List,rowmaxno_Integer?Positive]:=Module[
{tying,YTdisplay,dynkin,irrep,out},
tying = TyingRule[YTshape,rowmaxno];
YTdisplay = Subscript[YoungTableau[Irrep[A][##]]&@@#,"IR"]&/@Array[BYTToDynkinAn[tying[[#]],rowmaxno]&,Length[tying]];
dynkin = Array[BYTToDynkinBn[tying[[#]],rowmaxno]&,Length[tying]];
irrep = Array[DynkinBnToIrrep[dynkin[[#]]]&,Length[dynkin]];
out = Transpose[Array[{YTdisplay[[#]],dynkin[[#]],irrep[[#]]}&,Length[tying]]];
out
]


(* ::Input::Initialization:: *)
TyingDisplayAllD[YTshape_List,rowmaxno_Integer?Positive]:=Module[
{tying,YTdisplay,dynkin,irrep,out},
tying = TyingRule[YTshape,rowmaxno];
YTdisplay = Subscript[YoungTableau[Irrep[A][##]]&@@#,"IR"]&/@Array[BYTToDynkinAn[tying[[#]],rowmaxno]&,Length[tying]];
dynkin = Array[{BYTToDynkinDn[tying[[#]],rowmaxno]}&,Length[tying]];
irrep = Array[If[Length[dynkin[[#]]]==2,{DynkinDnToIrrep[dynkin[[#]][[1]]],DynkinDnToIrrep[dynkin[[#]][[2]]]},DynkinDnToIrrep[dynkin[[#]][[1]]]]&,Length[dynkin]];
out = Transpose[Array[{YTdisplay[[#]],dynkin[[#]],irrep[[#]]}&,Length[tying]]];
out
]


(* ::Subsection:: *)
(*SU(n) to SO(n) Branching Rule Dimension Checks*)


(* ::Input::Initialization:: *)
BranchingA2nBnDimCheck[YTshape_List,rowmaxno_Integer?Positive]:=Module[
{tying,dynkin,irrep,dimBn,dynkinAn,dimAn,out},
tying = TyingRule[YTshape,rowmaxno];
dynkin = Array[BYTToDynkinBn[tying[[#]],rowmaxno]&,Length[tying]];
irrep = Array[DynkinBnToIrrep[dynkin[[#]]]&,Length[dynkin]];
dimBn = Total[Dim[irrep]];
dynkinAn = BYTToDynkinAn[YTshape,2rowmaxno];
dimAn = Dim[DynkinAnToIrrep[dynkinAn]];
out = (dimBn==dimAn);
out
]


(* ::Input::Initialization:: *)
BranchingA2n1DnDimCheck[YTshape_List,rowmaxno_Integer?Positive]:=Module[
{tying,dynkin,irrep,dimDn,dynkinAn,dimAn,out},
tying = TyingRule[YTshape,rowmaxno];
dynkin = Array[BYTToDynkinDn[tying[[#]],rowmaxno]&,Length[tying]];
irrep = Array[DynkinDnToIrrep[dynkin[[#]]]&,Length[dynkin]];
dimDn = Total[Dim[irrep]];
dynkinAn = BYTToDynkinAn[YTshape,2rowmaxno-1];
dimAn = Dim[DynkinAnToIrrep[dynkinAn]];
out = (dimDn==dimAn);
out
]
