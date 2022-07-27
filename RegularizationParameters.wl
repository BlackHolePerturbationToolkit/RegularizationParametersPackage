(* ::Package:: *)

(* ::Section::Closed:: *)
(*Acknowledgements*)


(************************************************************************)
(* This package will generate the first two regularisation parameters,  *)
(* for the self-force and scalar field of a scalar charged particle on  *)
(* a generic orbit in Kerr spacetime. That is,                          *)
(*      Subscript[A, a] or Subscript[F, a[-1]] and Subscript[B, a] or Subscript[F, a[0]] for the scalar self-force,           *)
(*      B or Subscript[F, [0]] and D or Subscript[F, [2]] for the scalar singular field.           *)
(*                                                                      *)
(* Please cite the following if you make use of the citation function   *)
(* included in this package, in particular the following papers were used *)
(*  - L.Barack, A.Ori, Phys.Rev. D66 (2002) 084022, arXiv:gr-qc/0204093 *)          
(*      Calculation of Subscript[A, a] or Subscript[F, a[-1]] and Subscript[B, a] or Subscript[F, a[0]] for scalar self-force *)
(*  - A.Heffernan, arXiv:2107.14750                                     *)
(*      Calculation of Subscript[D, a] or Subscript[F, a[2]] for scalar self force *)
(*   - A.Heffernan, doi:10.5281/zenodo.6282572                          *)
(*	  Development and Zenado repository for this code                 *)
(*  - bhptoolkit.org as outlined on https://bhptoolkit.org/index.html   *)
(*  - Regularisation pacakage on Zenado: doi:10.5281/zenodo.6282572     *)
(*      Development and availability of this code                       *)
(*                                                                      *)
(* The creation of this package was supported by The European Union\[CloseCurlyQuote]s   *)
(* Horizon 2020 research and innovation programme under grant agreement *)
(* 661705-GravityWaveWindow                                             *)
(************************************************************************)


(* ::Section::Closed:: *)
(*Set-up*)


(* ::Subsubsection::Closed:: *)
(*Package name and function*)


BeginPackage["RegularizationParameters`"];


ElFactor::usage="Factor of \[ScriptL] that comes before A, B and D.";
RPscalarA::usage="Function to return first regularisation parameters for all 4 components {t,r,\[Theta],\[Phi]}. 
It requires 10 input parameters: 
		{Energy, Angular Momentum, CarterConstant, spin, mass, radius, inclination, sign of \[CapitalDelta]r, sign of \!\(\*SuperscriptBox[\(u\), \(r\)]\),sign of \!\(\*SuperscriptBox[\(u\), \(\[Theta]\)]\)}.";
RPscalarB::usage="Function to return second regularisation parameters \!\(\*SubscriptBox[\(F\), \(a[0]\)]\) or \!\(\*SubscriptBox[\(B\), \(a\)]\) for all 4 components {t,r,\[Theta],\[Phi]}. 
It requires 10 input parameters: 
		{Energy, Angular Momentum, CarterConstant, spin, mass, radius, inclination, sign of \[CapitalDelta]r, sign of \!\(\*SuperscriptBox[\(u\), \(r\)]\),sign of \!\(\*SuperscriptBox[\(u\), \(\[Theta]\)]\)}.";
RPscalarD::usage="Function to return fourth regularisation parameters \!\(\*SubscriptBox[\(F\), \(a[2]\)]\) or \!\(\*SubscriptBox[\(D\), \(a\)]\) for all 4 components {t,r,\[Theta],\[Phi]}. 
It requires 10 input parameters: 
		{Energy, Angular Momentum, CarterConstant, spin, mass, radius, inclination, sign of \[CapitalDelta]r, sign of \!\(\*SuperscriptBox[\(u\), \(r\)]\),sign of \!\(\*SuperscriptBox[\(u\), \(\[Theta]\)]\)}.";
citeScalarSF::usage="Function that returns string with necessary citations based on what self-force parameters used.
		Input is {spin (0 or not 0), eccentricity (0 or not 0), inclination (0 for equatorial, non zero otherwise), number of non-zero parameters used}.";


Begin["`Private`"];


(* ::Section::Closed:: *)
(*Regularisation Parameter Functions*)


(* ::Subsection::Closed:: *)
(*\[ScriptL] function*)


ElFactor[kk_,l_]:=((2 l +1))/Product[(2l-2n),{n,-(1/2)(kk+1),1/2 (kk-1)}];


(* ::Subsection::Closed:: *)
(*Functions for scalar parameters*)


SetDirectory["~/Documents/Research/Code/RegularizationParameters"];


Get["RPscalarA.wl"];
Get["RPscalarB.wl"];
Get["RPscalarD.wl"];


(* ::Section::Closed:: *)
(*Citations*)


(* ::Subsection::Closed:: *)
(*general code*)


citeCode[1]="@misc{BHPToolkit,
  title = {{Black Hole Perturbation Toolkit}},
  howpublished = {(\\href{http://bhptoolkit.org/}{bhptoolkit.org})},
}

";


citeCode[2]="@electronic{Heffernan:2022,
  author        = \"Heffenran, Anna\",
  title         = \"{Self-Force Regularisation Parameters Package}\",
  year          = \"2022\",
  publisher     = \"Zenodo\",
  doi           = \"10.5281/zenodo.6282572\",
  note          = \"Published May, 2022\"
}

";


citeCodeT=Table[citeCode[i],{i,2}];


(* ::Subsection::Closed:: *)
(*Scalar self-force*)


(* ::Subsubsection::Closed:: *)
(*Kerr generic*)


citeScalarKerrGen[1]="@article{Barack:2002mh,
    author = \"Barack, Leor and Ori, Amos\",
    title = \"{Gravitational self-force on a particle orbiting a Kerr black hole}\",
    eprint = \"gr-qc/0212103\",
    archivePrefix = \"arXiv\",
    doi = \"10.1103/PhysRevLett.90.111101\",
    journal = \"Phys. Rev. Lett.\",
    volume = \"90\",
    pages = \"111101\",
    year = \"2003\"
}

";


citeScalarKerrGen[2]="@article{Heffernan:2021olv,
    author = \"Heffernan, Anna\",
    title = \"{Regularization of a scalar charged particle for generic orbits in Kerr spacetime}\",
    eprint = \"2107.14750\",
    archivePrefix = \"arXiv\",
    primaryClass = \"gr-qc\",
    month = \"7\",
    year = \"2021\"
}

";


citeScalKerrGen={citeScalarKerrGen[1],citeScalarKerrGen[2]};


(* ::Subsubsection::Closed:: *)
(*Kerr eccentric equatorial*)


citeScalarKerrEccEq[2]="@article{Heffernan:2012vj,
    author = \"Heffernan, Anna and Ottewill, Adrian and Wardell, Barry\",
    title = \"{High-order expansions of the Detweiler-Whiting singular field in Kerr spacetime}\",
    eprint = \"1211.6446\",
    archivePrefix = \"arXiv\",
    primaryClass = \"gr-qc\",
    doi = \"10.1103/PhysRevD.89.024030\",
    journal = \"Phys. Rev. D\",
    volume = \"89\",
    number = \"2\",
    pages = \"024030\",
    year = \"2014\"
}

";


citeScalKerrEccEq={citeScalarKerrGen[1],citeScalarKerrEccEq[2]};


(* ::Subsubsection::Closed:: *)
(*Schwarzschild generic (eccentric)*)


citeScalarSchwGen[1]="@article{Barack:2002mha,
    author = \"Barack, Leor and Ori, Amos\",
    title = \"{Regularization parameters for the selfforce in Schwarzschild space-time. 1. Scalar case}\",
    eprint = \"gr-qc/0204093\",
    archivePrefix = \"arXiv\",
    doi = \"10.1103/PhysRevD.66.084022\",
    journal = \"Phys. Rev. D\",
    volume = \"66\",
    pages = \"084022\",
    year = \"2002\"
}

";


citeScalarSchwGen[2]="@article{Haas:2006ne,
    author = \"Haas, Roland and Poisson, Eric\",
    title = \"{Mode-sum regularization of the scalar self-force: Formulation in terms of a tetrad decomposition of the singular field}\",
    eprint = \"gr-qc/0605077\",
    archivePrefix = \"arXiv\",
    doi = \"10.1103/PhysRevD.74.044009\",
    journal = \"Phys. Rev. D\",
    volume = \"74\",
    pages = \"044009\",
    year = \"2006\"
}

";


citeScalarSchwGen[3]="@article{Heffernan:2012su,
    author = \"Heffernan, Anna and Ottewill, Adrian and Wardell, Barry\",
    title = \"{High-order expansions of the Detweiler-Whiting singular field in Schwarzschild spacetime}\",
    eprint = \"1204.0794\",
    archivePrefix = \"arXiv\",
    primaryClass = \"gr-qc\",
    doi = \"10.1103/PhysRevD.86.104023\",
    journal = \"Phys. Rev. D\",
    volume = \"86\",
    pages = \"104023\",
    year = \"2012\"
}
";


citeScalSchwGen=Table[citeScalarSchwGen[i],{i,3}];


(* ::Subsubsection::Closed:: *)
(*Schwarzschild circular*)


citeScalarSchwCirc[1]="@article{Barack:1999zw,
    author = \"Barack, Leor and Ori, Amos\",
    title = \"{Calculating the selfforce in Schwarzschild space-time by mode sum regularization}\",
    eprint = \"gr-qc/9911040\",
    archivePrefix = \"arXiv\",
    month = \"11\",
    year = \"1999\"
}

";


citeScalarSchwCirc[2]="@article{Detweiler:2002gi,
    author = \"Detweiler, Steven L. and Messaritaki, Eirini and Whiting, Bernard F.\",
    title = \"{Selfforce of a scalar field for circular orbits about a Schwarzschild black hole}\",
    eprint = \"gr-qc/0205079\",
    archivePrefix = \"arXiv\",
    doi = \"10.1103/PhysRevD.67.104016\",
    journal = \"Phys. Rev. D\",
    volume = \"67\",
    pages = \"104016\",
    year = \"2003\"
}

";


citeScalSchwCirc={citeScalarSchwCirc[1],citeScalarSchwCirc[2],citeScalarSchwGen[3]};


(* ::Text:: *)
(**)


(* ::Subsection::Closed:: *)
(*Citation output*)


(* ::Subsubsection::Closed:: *)
(*Self-force*)


citeScalarSF[spin_,ecc_,theta_,n_]:=Module[{tab="",citns},
If[spin==0,
If[ecc!=0,
tab=If[n-1>Length[citeScalSchwGen],citeScalSchwGen,citeScalSchwGen[[1;;n-1]]];
,
tab=If[n-1>Length[citeScalSchwCirc],citeScalSchwCirc,citeScalSchwCirc[[1;;n-1]]];
],
If[theta==0,
tab=If[n-1>Length[citeScalKerrEccEq],citeScalKerrEccEq,citeScalKerrEccEq[[1;;n-1]]];,
tab=If[n-1>Length[citeScalKerrGen],citeScalKerrGen,citeScalKerrGen[[1;;n-1]]];
]
];

citns=Flatten[Join[citeCodeT,tab]];
StringJoin[citns]
]


(* ::Section::Closed:: *)
(*End Package*)


(* ::Subsection::Closed:: *)
(*The End*)


(* ::Subsubsection::Closed:: *)
(*Package end*)


End[]


EndPackage[]
