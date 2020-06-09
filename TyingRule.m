(* ::Package:: *)



(* Generate All YT Shapes *)

ElementRepeat[m_,n_Integer?Positive] := Sequence@@ConstantArray[m,n]

GenerateAllYTShapes[rowmaxno_Integer?Positive,colmaxno_Integer?Positive] := 
  ReverseSort[#] &/@ DeleteDuplicates[Subsets[Array[ElementRepeat[#,colmaxno]&,rowmaxno],colmaxno]];



(* Small Functions For Tying Rule *)

YTPad[YTshape_List,rowmaxno_Integer?Positive,columnno_Integer?Positive] := 
  Transpose[Array[ArrayPad[Table[1,YTshape[[#]]],{0,(rowmaxno-YTshape[[#]])}]&,columnno]];

ArrayPadLeftRigid[list_List,length_Integer?Positive,padding_] := 
  Piecewise[{{ArrayPad[list,{length-Length[list],0},padding],length>Length[list]},
    {list,length==Length[list]},
    {Table[padding,length],length<Length[list]}}];

TieComboAll[YTshape_List,rowmaxno_Integer?Positive,columnno_Integer?Positive] := Module[
  {tietype,tieendposition,tiemaxno,tieall,tiecomboall,out},
  tietype = Permutations[ArrayPadLeftRigid[{1,1},columnno,0]];
  tieendposition = Array[Max@Position[tietype[[#]],1]&,Length[tietype]];
  tiemaxno = Array[YTshape[[tieendposition[[#]]]]&,Length[tieendposition]];
  tieall = ArrayReshape[Array[ConstantArray[tietype[[#]],tiemaxno[[#]]]&,columnno],{Total[tiemaxno],columnno}];
  tiecomboall = DeleteDuplicates[Subsets[tieall,rowmaxno]];
  out = Array[ArrayReshape[tiecomboall[[#]],{rowmaxno,columnno},0] &,Length[tiecomboall]];
  out
 ]

InArrayQ[list_,element_] := If[Length@Position[list,x_/;x==element]==0,False,True];

NoSymTestAdjColumn[YTshape_List,YTremnant_List, col_Integer?Positive] := Module[
  {nosymneed,nosymquota,out},
  nosymneed = Count[YTremnant[[1;;YTshape[[col+1]],col;;(col+1)]],{0,1}];
  nosymquota = Count[YTremnant[[(YTshape[[col+1]]+1);;YTshape[[col]],1]] ,1];
  out = nosymneed>nosymquota;
  out
 ]

NoSymTest[YTshape_List,YTremnant_List,columnno_Integer?Positive] := 
  Or[##]& @@Array[NoSymTestAdjColumn[YTshape,YTremnant,#]&,(columnno-1)];



(* Tying Rule Full Function *)

TyingRuleNoTie[YTshape_List] := {YTshape};

TyingRuleTwoColumn[YTshape_List,rowmaxno_Integer?Positive] := Module[
  {YTstruct,tiemaxno,tiecomboall,YTremnant,out},
  YTstruct = YTPad[YTshape,rowmaxno,2];
  tiemaxno = Min[YTshape];
  tiecomboall = Array[ArrayReshape[Table[{1,1},#],{rowmaxno,2}]&,(tiemaxno+1),0];
  YTremnant = Array[YTstruct - tiecomboall[[#]]&,Length[tiecomboall]];
  out = Array[Total[YTremnant[[#]],{1}]&,Length[YTremnant]];
  out
 ]

TyingRuleThreeColumn[YTshape_List,rowmaxno_Integer?Positive] := Module[
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

TyingRule[YTshaperaw_List,rowmaxno_Integer?Positive] := Module[
  {columnno,YTshape,tiecomboall,YTremnant,tieoutsideQ,YTremnantshape,irregularQ,violatesymQ,out},
  If[rowmaxno<Max[YTshaperaw],Abort[]];
  If[!OrderedQ[YTshaperaw,GreaterEqual],Abort[]];
  YTshape = DeleteCases[YTshaperaw,0];
  columnno = Length[YTshape];
  out = Piecewise[{{TyingRuleNoTie[YTshape],columnno<=1},
  	{TyingRuleTwoColumn[YTshape,rowmaxno],columnno==2},
  	{TyingRuleThreeColumn[YTshape,rowmaxno],columnno==3},
  	{Abort[],columnno>=4}}];
  out
 ]



(* SU(n) to SO(n) Projection Matrix Input *)

ProjAn1Bn[n_] := Module[
  {out},
  If[n>=3,
    out = Table[KroneckerDelta[i,j]+KroneckerDelta[2n+1-i,j],{i,1,n},{j,1,2n}];
    out[[n,n]]=2;
    out[[n,n+1]]=2,
    Abort[]];
  out
 ]

ProjAn1Dn[n_] := Module[
  {out},
  If[n>=4,
    out = Table[KroneckerDelta[i,j]+KroneckerDelta[2n-i,j],{i,1,n},{j,1,2n-1}];
    out[[n,n-1]]=1;
    out[[n,n+1]]=1,
    Abort[]];
  out
 ]



(* Convert YTs to Dynkin Labels *)

BYTToDynkinAn[YTshape_List,rowmaxno_Integer?Positive] := Module[
  {out},
  If[rowmaxno<Max[YTshape],Abort[]];
  If[!OrderedQ[YTshape,GreaterEqual],Abort[]];
  out = Array[Count[YTshape,#]&,rowmaxno];
  out
 ]

BYTToDynkinBn[YTshape_List,rowmaxno_Integer?Positive] := Module[
  {dynkinAn,out},
  If[rowmaxno<3,Abort[]];
  dynkinAn = BYTToDynkinAn[YTshape,rowmaxno];
  out = ProjAn1Bn[rowmaxno][[All,1;;rowmaxno]].dynkinAn;
  out
 ]

BYTToDynkinDn[YTshape_List,rowmaxno_Integer?Positive] := Module[
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



(* Convert Dynkin Labels to Irreps *)

DynkinAnToIrrep[dynkin_List] := Irrep[A][##]& @@ dynkin;

DynkinBnToIrrep[dynkin_List] := Irrep[B][##]& @@ dynkin;

DynkinDnToIrrep[dynkin_List] := Irrep[D][##]& @@ dynkin;



(* Tying Rule Display YTs, Dynkin Labels and Irreps *)

TyingDisplayYT[YTshape_List,rowmaxno_Integer?Positive] := Module[
  {tying,tyingdynkin,out},
  tying = TyingRule[YTshape,rowmaxno];
  tyingdynkin = Array[BYTToDynkinAn[tying[[#]],rowmaxno]&,Length[tying]];
  out = Subscript[YoungTableau[Irrep[A][##]]&@@#,"IR"]&/@tyingdynkin;
  out
 ]

TyingDisplayAllB[YTshape_List,rowmaxno_Integer?Positive] := Module[
  {tying,YTdisplay,dynkin,irrep,out},
  tying = TyingRule[YTshape,rowmaxno];
  YTdisplay = Subscript[YoungTableau[Irrep[A][##]]&@@#,"IR"]&/@Array[BYTToDynkinAn[tying[[#]],rowmaxno]&,Length[tying]];
  dynkin = Array[BYTToDynkinBn[tying[[#]],rowmaxno]&,Length[tying]];
  irrep = Array[DynkinBnToIrrep[dynkin[[#]]]&,Length[dynkin]];
  out = Transpose[Array[{YTdisplay[[#]],dynkin[[#]],irrep[[#]]}&,Length[tying]]];
  out
 ]

TyingDisplayAllD[YTshape_List,rowmaxno_Integer?Positive] := Module[
  {tying,YTdisplay,dynkin,irrep,out},
  tying = TyingRule[YTshape,rowmaxno];
  YTdisplay = Subscript[YoungTableau[Irrep[A][##]]&@@#,"IR"]&/@Array[BYTToDynkinAn[tying[[#]],rowmaxno]&,Length[tying]];
  dynkin = Array[{BYTToDynkinDn[tying[[#]],rowmaxno]}&,Length[tying]];
  irrep = Array[If[Length[dynkin[[#]]]==2,
    {DynkinDnToIrrep[dynkin[[#]][[1]]],DynkinDnToIrrep[dynkin[[#]][[2]]]},
    DynkinDnToIrrep[dynkin[[#]][[1]]]]&,Length[dynkin]];
  out = Transpose[Array[{YTdisplay[[#]],dynkin[[#]],irrep[[#]]}&,Length[tying]]];
  out
 ]



(* SU(n) to SO(n) Branching Rule Dimension Checks *)

BranchingA2nBnDimCheck[YTshape_List,rowmaxno_Integer?Positive] := Module[
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

BranchingA2n1DnDimCheck[YTshape_List,rowmaxno_Integer?Positive] := Module[
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
