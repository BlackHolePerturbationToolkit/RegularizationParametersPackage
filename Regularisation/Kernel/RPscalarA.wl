(* ::Package:: *)

RPscalarA=Compile[{{EE,_Real},{L,_Real},{Q,_Real},{a,_Real},{m,_Real},{r,_Real},{\[Theta],_Real},{s\[CapitalDelta]r,_Integer},{sR,_Integer},{s\[Theta],_Integer}},
	Block[
		{\[CapitalSigma],\[CapitalDelta],ut,ur,urd,u\[Theta]d,g\[Phi]\[Phi],V},
	
		\[CapitalSigma]=r^2+a^2 Cos[\[Theta]]^2;
		\[CapitalDelta]=a^2-2 m r +r^2;
	
		ut=EE ((a^2+r^2)^2/(\[CapitalDelta] \[CapitalSigma])-(a^2+r^2-\[CapitalSigma])/\[CapitalSigma])-(2 a L m r)/(\[CapitalDelta] \[CapitalSigma]);
		ur=sR/\[CapitalSigma] Sqrt[(EE (r^2+a^2)-a L)^2-(r^2-2 m r +a^2) (r^2+ (L-a EE)^2+Q)];

		urd=sR/\[CapitalDelta] Sqrt[(EE (r^2+a^2)-a L)^2-(r^2-2 m r +a^2) (r^2+ (L-a EE)^2+Q)];
		u\[Theta]d=s\[Theta] Sqrt[Q-Cot[\[Theta]]^2 L^2-a^2 Cos[\[Theta]]^2 (1-EE^2)];
	
		g\[Phi]\[Phi]=(\[CapitalDelta]+2 m r (r^2+a^2)/\[CapitalSigma]) Sin[\[Theta]]^2;
		V=1+u\[Theta]d^2/\[CapitalSigma]+L^2/g\[Phi]\[Phi];

		{-ur/ut 1/2 (-s\[CapitalDelta]r 1/V (Sin[\[Theta]]^2/(\[CapitalDelta] g\[Phi]\[Phi]))^(1/2) (V+(\[CapitalDelta] urd^2)/\[CapitalSigma])^(1/2)),
		1/2 (-s\[CapitalDelta]r 1/V (Sin[\[Theta]]^2/(\[CapitalDelta] g\[Phi]\[Phi]))^(1/2) (V+(\[CapitalDelta] urd^2)/\[CapitalSigma])^(1/2)),0,0}
	],
	CompilationTarget->"C",RuntimeAttributes->{Listable},Parallelization->True]


RPscalarAt=Compile[{{EE,_Real},{L,_Real},{Q,_Real},{a,_Real},{m,_Real},{r,_Real},{\[Theta],_Real},{s\[CapitalDelta]r,_Integer},{sR,_Integer},{s\[Theta],_Integer}},
	Block[
		{\[CapitalSigma],\[CapitalDelta],ut,ur,urd,u\[Theta]d,g\[Phi]\[Phi],V},
	
		\[CapitalSigma]=r^2+a^2 Cos[\[Theta]]^2;
		\[CapitalDelta]=a^2-2 m r +r^2;
	
		ut=EE ((a^2+r^2)^2/(\[CapitalDelta] \[CapitalSigma])-(a^2+r^2-\[CapitalSigma])/\[CapitalSigma])-(2 a L m r)/(\[CapitalDelta] \[CapitalSigma]);
		ur=sR/\[CapitalSigma] Sqrt[(EE (r^2+a^2)-a L)^2-(r^2-2 m r +a^2) (r^2+ (L-a EE)^2+Q)];

		urd=sR/\[CapitalDelta] Sqrt[(EE (r^2+a^2)-a L)^2-(r^2-2 m r +a^2) (r^2+ (L-a EE)^2+Q)];
		u\[Theta]d=s\[Theta] Sqrt[Q-Cot[\[Theta]]^2 L^2-a^2 Cos[\[Theta]]^2 (1-EE^2)];
	
		g\[Phi]\[Phi]=(\[CapitalDelta]+2 m r (r^2+a^2)/\[CapitalSigma]) Sin[\[Theta]]^2;
		V=1+u\[Theta]d^2/\[CapitalSigma]+L^2/g\[Phi]\[Phi];

		-ur/ut 1/2 (-s\[CapitalDelta]r 1/V (Sin[\[Theta]]^2/(\[CapitalDelta] g\[Phi]\[Phi]))^(1/2) (V+(\[CapitalDelta] urd^2)/\[CapitalSigma])^(1/2))
	],
	CompilationTarget->"C",RuntimeAttributes->{Listable},Parallelization->True]


RPscalarAr=Compile[{{EE,_Real},{L,_Real},{Q,_Real},{a,_Real},{m,_Real},{r,_Real},{\[Theta],_Real},{s\[CapitalDelta]r,_Integer},{sR,_Integer},{s\[Theta],_Integer}},
	Block[
		{\[CapitalSigma],\[CapitalDelta],ut,ur,urd,u\[Theta]d,g\[Phi]\[Phi],V},
	
		\[CapitalSigma]=r^2+a^2 Cos[\[Theta]]^2;
		\[CapitalDelta]=a^2-2 m r +r^2;
	
		ut=EE ((a^2+r^2)^2/(\[CapitalDelta] \[CapitalSigma])-(a^2+r^2-\[CapitalSigma])/\[CapitalSigma])-(2 a L m r)/(\[CapitalDelta] \[CapitalSigma]);
		ur=sR/\[CapitalSigma] Sqrt[(EE (r^2+a^2)-a L)^2-(r^2-2 m r +a^2) (r^2+ (L-a EE)^2+Q)];

		urd=sR/\[CapitalDelta] Sqrt[(EE (r^2+a^2)-a L)^2-(r^2-2 m r +a^2) (r^2+ (L-a EE)^2+Q)];
		u\[Theta]d=s\[Theta] Sqrt[Q-Cot[\[Theta]]^2 L^2-a^2 Cos[\[Theta]]^2 (1-EE^2)];
	
		g\[Phi]\[Phi]=(\[CapitalDelta]+2 m r (r^2+a^2)/\[CapitalSigma]) Sin[\[Theta]]^2;
		V=1+u\[Theta]d^2/\[CapitalSigma]+L^2/g\[Phi]\[Phi];

		1/2 (-s\[CapitalDelta]r 1/V (Sin[\[Theta]]^2/(\[CapitalDelta] g\[Phi]\[Phi]))^(1/2) (V+(\[CapitalDelta] urd^2)/\[CapitalSigma])^(1/2))
	],
	CompilationTarget->"C",RuntimeAttributes->{Listable},Parallelization->True]
