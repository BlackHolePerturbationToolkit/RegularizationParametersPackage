(* ::Package:: *)

(* ::Subsection:: *)
(*Subscript[B, a]*)


RPscalarB=Compile[{{EE,_Real},{L,_Real},{Q,_Real},{a,_Real},{m,_Real},{r,_Real},{\[Theta],_Real},{s\[CapitalDelta]r,_Integer},{sR,_Integer},{s\[Theta],_Integer}},
	Block[
		{\[CapitalSigma],\[CapitalDelta],ur,u\[Theta],uw1d,uw2d,\[Zeta]p\[Xi]Sq,\[Zeta]m\[Xi]Sq,k,\[Mu],cos2\[Beta],sin2\[Beta],cos4\[Beta],sin4\[Beta],fa\[Rho]\[CapitalDelta]x1302,fa\[Rho]\[CapitalDelta]x1311,fa\[Rho]\[CapitalDelta]x1320,fa\[Rho]\[CapitalDelta]x1504,fa\[Rho]\[CapitalDelta]x1513,fa\[Rho]\[CapitalDelta]x1522,fa\[Rho]\[CapitalDelta]x1531,fa\[Rho]\[CapitalDelta]x1540,faEB,faKB},
	
		\[CapitalSigma]=r^2+a^2 Cos[\[Theta]]^2;
		\[CapitalDelta]=a^2-2 m r +r^2;
	
		ur=sR/\[CapitalSigma] Sqrt[(EE (r^2+a^2)-a L)^2-(r^2-2 m r +a^2) (r^2+ (L-a EE)^2+Q)];
		u\[Theta]=s\[Theta]/\[CapitalSigma] Sqrt[Q-Cot[\[Theta]]^2 L^2-a^2 Cos[\[Theta]]^2 (1-EE^2)];
	
		uw1d=(L ((-2 m r+\[CapitalSigma]) (2 m r (a^2+r^2)+\[CapitalDelta] \[CapitalSigma]) Csc[\[Theta]]+4 a^2 m^2 r^2 Sin[\[Theta]]))/(\[CapitalDelta] \[CapitalSigma]^2);
		uw2d=-s\[Theta] Sqrt[Q-Cot[\[Theta]]^2 L^2-a^2 Cos[\[Theta]]^2 (1-EE^2)];

		\[Eta]=1/\[CapitalSigma] \[Sqrt](a^4 (2 m r+\[CapitalSigma])^2+((L^2+Q) \[CapitalSigma]-(r^2-\[CapitalSigma]) (2 m r+EE^2 \[CapitalSigma]))^2+2 a^2 (2 m r+\[CapitalSigma]) ((L^2-Q) \[CapitalSigma]+(r^2-\[CapitalSigma]) (2 m r+EE^2 \[CapitalSigma])));
		\[Zeta]p\[Xi]Sq=a^2+L^2+Q-r (2 m+(-2+EE^2) r)+(2 m r (a^2+r^2))/\[CapitalSigma]+EE^2 \[CapitalSigma];
		\[Zeta]m\[Xi]Sq= a^2-Q-L^2 (1-2 Csc[\[Theta]]^2) -  2 m r+(2 m r (a^2+ r^2))/\[CapitalSigma] - EE^2 (-r^2+\[CapitalSigma]);
		k=(2 \[Eta])/(\[Zeta]p\[Xi]Sq+\[Eta]);
		\[Mu]=uw1d uw2d;
	
		cos2\[Beta]=\[Zeta]m\[Xi]Sq/\[Eta];
		sin2\[Beta]=-((2 \[Mu])/\[Eta]);
	
		cos4\[Beta]=cos2\[Beta]^2-sin2\[Beta]^2;
		sin4\[Beta]=2 sin2\[Beta] cos2\[Beta];
	
		fa\[Rho]\[CapitalDelta]x1302={-(1/2) EE r ur-(a u\[Theta] (-8 L m r Cot[\[Theta]]+a EE (4 m r+\[CapitalSigma]) Sin[2 \[Theta]]))/(4 \[CapitalSigma]),(-r (\[CapitalDelta]-ur^2 \[CapitalSigma]+2 u\[Theta]^2 \[CapitalDelta] \[CapitalSigma])+3 a^2 ur u\[Theta] \[CapitalSigma] Cos[\[Theta]] Sin[\[Theta]])/(2 \[CapitalDelta]),3/4 (2 r ur u\[Theta] \[CapitalSigma]+a^2 (1+u\[Theta]^2 \[CapitalSigma]) Sin[2 \[Theta]]),(L r ur \[CapitalSigma]+L u\[Theta] (-4 m r (a^2+r^2)+(a^2+r (4 m+r)) \[CapitalSigma]-3 \[CapitalSigma]^2) Cot[\[Theta]]+2 a EE m r u\[Theta] (a^2+r^2-\[CapitalSigma]) Sin[2 \[Theta]])/(2 \[CapitalSigma])};
		fa\[Rho]\[CapitalDelta]x1311={1/\[CapitalSigma]^2 a m (2 (-1+EE^2) r (a^2+r^2-\[CapitalSigma]) Cos[\[Theta]]+2 r (a^2+r^2) u\[Theta]^2 \[CapitalSigma] Cos[\[Theta]]-2 L^2 r Cot[\[Theta]] Csc[\[Theta]]+ur u\[Theta] \[CapitalSigma] (-2 r^2+\[CapitalSigma]) Sin[\[Theta]]),1/(\[CapitalDelta] \[CapitalSigma]) (-a ur (2 a^2 EE m r+2 EE m r (r^2-\[CapitalSigma])+a L (-2 m r+\[CapitalSigma])) Cos[\[Theta]]+u\[Theta] Csc[\[Theta]] (-2 L m r^2 (a^2+r^2)+L \[CapitalSigma] (a^2 (m+r)-(m-r) (r^2+\[CapitalSigma]))+a EE m (a^2 (2 r^2-\[CapitalSigma])+r^2 (2 r^2+\[CapitalSigma])) Sin[\[Theta]]^2)),-((4 a EE m r u\[Theta] (a^2+r^2-\[CapitalSigma]) Cos[\[Theta]])/\[CapitalSigma])+L (-r ur+(u\[Theta] (4 m r (a^2+r^2)-(a^2+r (4 m+r)) \[CapitalSigma]+2 \[CapitalSigma]^2) Cot[\[Theta]])/\[CapitalSigma]) Csc[\[Theta]],1/\[CapitalSigma]^2 (-4 a EE L m r (a^2+r^2-\[CapitalSigma]) Cos[\[Theta]]+(2 m r (a^2+r^2) (a^2+r^2-\[CapitalSigma])-u\[Theta]^2 \[CapitalSigma] (2 m r (a^2+r^2)^2+\[CapitalDelta] \[CapitalSigma]^2)) Cos[\[Theta]]+L^2 (4 m r (a^2+r^2)-4 m r \[CapitalSigma]+\[CapitalSigma]^2) Cot[\[Theta]] Csc[\[Theta]]+ur u\[Theta] \[CapitalSigma] (m (a^2+r^2-\[CapitalSigma]) (2 r^2-\[CapitalSigma])-r \[CapitalSigma]^2) Sin[\[Theta]])};
		fa\[Rho]\[CapitalDelta]x1320={1/(2 \[CapitalSigma]^2) (ur (2 m r^2 (a^2 EE+2 a L+EE r^2)-m (a^2 EE+2 a L+3 EE r^2) \[CapitalSigma]+EE (m-r) \[CapitalSigma]^2)-a u\[Theta] (4 L m r (a^2+r^2) Cot[\[Theta]]+a EE (2 m r (a^2+r^2)+2 m r \[CapitalSigma]+\[CapitalSigma]^2) Cos[\[Theta]] Sin[\[Theta]])),1/(2 \[CapitalDelta] \[CapitalSigma]^2) (-2 m r^2 (a^2+r^2) (2 a EE L-\[CapitalDelta])-2 m (a EE L (-a^2+r^2)+r^2 (a^2+r^2) ur^2) \[CapitalSigma]+\[CapitalSigma] (a^2 m+3 m r^2-m \[CapitalSigma]+r \[CapitalSigma]) (-\[CapitalDelta]+ur^2 \[CapitalSigma])+2 L^2 (2 m r^2 (a^2+r^2)-m (a^2+r^2) \[CapitalSigma]+(m-r) \[CapitalSigma]^2) Csc[\[Theta]]^2+a^2 ur u\[Theta] \[CapitalSigma] (2 m r (a^2+r^2)+2 m r \[CapitalSigma]+\[CapitalSigma]^2) Cos[\[Theta]] Sin[\[Theta]]),1/(4 \[CapitalSigma]^2) ((2 m r (a^2+r^2)-2 m r \[CapitalSigma]+\[CapitalSigma]^2) (-a^2-4 L^2-r^2-\[CapitalSigma]+(a^2+r^2+\[CapitalSigma]) Cos[2 \[Theta]]) Cot[\[Theta]] Csc[\[Theta]]^2+4 a^3 EE L m r Sin[2 \[Theta]]+u\[Theta] \[CapitalSigma] (-4 m r^2 (a^2+r^2) ur+2 ur \[CapitalSigma] (a^2 m+3 m r^2-m \[CapitalSigma]+r \[CapitalSigma])+a^2 u\[Theta] (2 a^2 m r+\[CapitalSigma]^2+2 m r (r^2+\[CapitalSigma])) Sin[2 \[Theta]])),1/2 L ((3 ur (-2 m r^2 (a^2+r^2)+m (a^2+3 r^2) \[CapitalSigma]+(-m+r) \[CapitalSigma]^2))/\[CapitalSigma]^2+u\[Theta] (3 \[CapitalDelta]+(6 m r (a^2+r^2)^2)/\[CapitalSigma]^2-\[CapitalSigma]) Cot[\[Theta]])};
		fa\[Rho]\[CapitalDelta]x1504={3/4 EE u\[Theta] \[CapitalSigma] (2 r ur u\[Theta] \[CapitalSigma]+a^2 (1+u\[Theta]^2 \[CapitalSigma]) Sin[2 \[Theta]]),-((3 ur u\[Theta] \[CapitalSigma]^2 (2 r ur u\[Theta] \[CapitalSigma]+a^2 (1+u\[Theta]^2 \[CapitalSigma]) Sin[2 \[Theta]]))/(4 \[CapitalDelta])),-(3/4) \[CapitalSigma] (1+u\[Theta]^2 \[CapitalSigma]) (2 r ur u\[Theta] \[CapitalSigma]+a^2 (1+u\[Theta]^2 \[CapitalSigma]) Sin[2 \[Theta]]),-(3/4) L u\[Theta] \[CapitalSigma] (2 r ur u\[Theta] \[CapitalSigma]+a^2 (1+u\[Theta]^2 \[CapitalSigma]) Sin[2 \[Theta]])};
		fa\[Rho]\[CapitalDelta]x1513={-((3 a (a EE L \[CapitalSigma] (1+2 u\[Theta]^2 (-2 m r+\[CapitalSigma]))+2 a^2 m r (1+u\[Theta]^2 (\[CapitalSigma]+2 EE^2 \[CapitalSigma]))+2 m r (r^2-\[CapitalSigma]) (1+u\[Theta]^2 (\[CapitalSigma]+2 EE^2 \[CapitalSigma]))) Cos[\[Theta]])/(2 \[CapitalSigma]))-3 r ur u\[Theta] (a m r+EE L \[CapitalSigma] Csc[\[Theta]]^2) Sin[\[Theta]],(3 ur \[CapitalSigma] (a (4 a^2 EE m r u\[Theta]^2+4 EE m r u\[Theta]^2 (r^2-\[CapitalSigma])+a L (1+2 u\[Theta]^2 (-2 m r+\[CapitalSigma]))) Cos[\[Theta]]+2 L r ur u\[Theta] \[CapitalSigma] Csc[\[Theta]]))/(2 \[CapitalDelta]),3 a u\[Theta] (1+u\[Theta]^2 \[CapitalSigma]) (2 a^2 EE m r+2 EE m r (r^2-\[CapitalSigma])+a L (-2 m r+\[CapitalSigma])) Cos[\[Theta]]+3/2 L r ur \[CapitalSigma] (1+2 u\[Theta]^2 \[CapitalSigma]) Csc[\[Theta]],3/2 (4 a EE L m r u\[Theta]^2 (a^2+r^2-\[CapitalSigma]) Cos[\[Theta]]+(a^2 L^2 (1+2 u\[Theta]^2 (-2 m r+\[CapitalSigma]))+(u\[Theta]^2+1/\[CapitalSigma]) (a^2+r^2-\[CapitalSigma]) (2 m r^3+r (-2 m+r) \[CapitalSigma]+a^2 (2 m r+\[CapitalSigma]))) Cos[\[Theta]]+r ur u\[Theta] (2 m r (a^2+r^2)+\[CapitalDelta] \[CapitalSigma]+2 L^2 \[CapitalSigma] Csc[\[Theta]]^2) Sin[\[Theta]])};
		fa\[Rho]\[CapitalDelta]x1522={3/4 (2 EE ur u\[Theta]^2 (-2 m r^2 (a^2+r^2)+m (a^2+3 r^2) \[CapitalSigma]+(-m+r) \[CapitalSigma]^2)+(2 L r ur (2 a m r+EE L \[CapitalSigma] Csc[\[Theta]]^2))/\[CapitalSigma]+a^2 EE u\[Theta]^3 (2 a^2 m r+\[CapitalSigma]^2+2 m r (r^2+\[CapitalSigma])) Sin[2 \[Theta]]+1/\[CapitalSigma]^2 2 a^2 u\[Theta] (EE \[CapitalSigma] (L^2 (-8 m r+\[CapitalSigma]) Cot[\[Theta]]+\[CapitalSigma]^2 Cos[\[Theta]] Sin[\[Theta]])+m r (a^2 EE (4 m r-\[CapitalSigma])+EE (4 m r-\[CapitalSigma]) (r^2-\[CapitalSigma])+a L (-4 m r+\[CapitalSigma]+4 EE^2 \[CapitalSigma])) Sin[2 \[Theta]])),1/(4 \[CapitalDelta]) 3 ur (2 a^2 L^2 u\[Theta] (8 m r-\[CapitalSigma]) Cot[\[Theta]]+2 ur \[CapitalSigma] (u\[Theta]^2 (2 m r^2 (a^2+r^2)-m (a^2+3 r^2) \[CapitalSigma]+(m-r) \[CapitalSigma]^2)-L^2 r Csc[\[Theta]]^2)-a^2 u\[Theta] (-2 m r (a^2-4 a EE L+r^2)+2 m r (1+(a^2+r^2) u\[Theta]^2) \[CapitalSigma]+(1+2 m r u\[Theta]^2) \[CapitalSigma]^2+u\[Theta]^2 \[CapitalSigma]^3) Sin[2 \[Theta]]),-(1/(4 \[CapitalSigma]))3 (2 a^2 L^2 (u\[Theta]^2 \[CapitalSigma]^2-4 m (r+2 r u\[Theta]^2 \[CapitalSigma])) Cot[\[Theta]]+2 ur u\[Theta] \[CapitalSigma] ((1+u\[Theta]^2 \[CapitalSigma]) (-2 m r^2 (a^2+r^2)+m (a^2+3 r^2) \[CapitalSigma]+(-m+r) \[CapitalSigma]^2)+L^2 r \[CapitalSigma] Csc[\[Theta]]^2)+a^2 (4 a EE L m r (1+2 u\[Theta]^2 \[CapitalSigma])+2 a^2 m r (-1+u\[Theta]^4 \[CapitalSigma]^2)+(1+u\[Theta]^2 \[CapitalSigma]) (\[CapitalSigma]^2 (1+u\[Theta]^2 \[CapitalSigma])+2 m r (\[CapitalSigma]+u\[Theta]^2 \[CapitalSigma]^2+r^2 (-1+u\[Theta]^2 \[CapitalSigma])))) Sin[2 \[Theta]]),1/(2 \[CapitalSigma]^2) 3 (-L (ur \[CapitalSigma] (a^2 r \[CapitalSigma]+r \[CapitalSigma] (r^2+u\[Theta]^2 \[CapitalSigma]^2)+m (a^2+r^2-\[CapitalSigma]) (u\[Theta]^2 \[CapitalSigma]^2+r^2 (2-2 u\[Theta]^2 \[CapitalSigma]))+L^2 r \[CapitalSigma] Csc[\[Theta]]^2)+a^2 u\[Theta] (L^2 \[CapitalSigma] (-8 m r+\[CapitalSigma]) Cot[\[Theta]]+(-8 m^2 r^2 (a^2+r^2)-4 m r \[CapitalDelta] \[CapitalSigma]+(a^2+r^2) (1+2 m r u\[Theta]^2) \[CapitalSigma]^2+(1+2 m r u\[Theta]^2) \[CapitalSigma]^3+u\[Theta]^2 \[CapitalSigma]^4) Cos[\[Theta]] Sin[\[Theta]]))+4 a EE m r u\[Theta] (-((a^2+r^2-\[CapitalSigma]) (2 m r^3+r (-2 m+r) \[CapitalSigma]+a^2 (2 m r+\[CapitalSigma])) Cos[\[Theta]] Sin[\[Theta]])-a^2 L^2 \[CapitalSigma] Sin[2 \[Theta]]))};
		fa\[Rho]\[CapitalDelta]x1531={1/(2 \[CapitalSigma]^3) 3 (-2 a u\[Theta]^2 \[CapitalSigma] (a^2 m r+m r (r^2-\[CapitalSigma])+a EE L \[CapitalSigma]) (2 a^2 m r+\[CapitalSigma]^2+2 m r (r^2+\[CapitalSigma])) Cos[\[Theta]]+a (8 a^2 L^2 m^2 r^2 Cos[\[Theta]]-4 a^2 EE^2 L^2 m r \[CapitalSigma] Cos[\[Theta]]+2 m r (a^2+r^2-\[CapitalSigma]) (2 m r (a^2+r^2)-2 m r \[CapitalSigma]-\[CapitalSigma]^2) Cos[\[Theta]]+a EE (-L (8 m^2 r^2 (a^2+r^2)-2 m r (a^2+r (4 m+r)) \[CapitalSigma]+2 m r \[CapitalSigma]^2+\[CapitalSigma]^3) Cos[\[Theta]]+4 L^3 m r \[CapitalSigma] Cot[\[Theta]] Csc[\[Theta]]))+2 ur u\[Theta] \[CapitalSigma] (2 m r^2 (a^2+r^2)-m (a^2+3 r^2) \[CapitalSigma]+(m-r) \[CapitalSigma]^2) (a m r+EE L \[CapitalSigma] Csc[\[Theta]]^2) Sin[\[Theta]]),1/(2 \[CapitalDelta] \[CapitalSigma]) 3 L ur (a^2 (-2 m r (a^2-2 a EE L+r^2)+2 m r (1+2 (a^2+r^2) u\[Theta]^2) \[CapitalSigma]+(1+4 m r u\[Theta]^2) \[CapitalSigma]^2+2 u\[Theta]^2 \[CapitalSigma]^3) Cos[\[Theta]]+2 (ur u\[Theta] \[CapitalSigma] (-2 m r^2 (a^2+r^2)+m (a^2+3 r^2) \[CapitalSigma]+(-m+r) \[CapitalSigma]^2)-2 a^2 L^2 m r Cot[\[Theta]]) Csc[\[Theta]]),3/2 L ((4 a^3 EE L m r u\[Theta] Cos[\[Theta]])/\[CapitalSigma]+(ur (1+2 u\[Theta]^2 \[CapitalSigma]) (-2 m r^2 (a^2+r^2)+m (a^2+3 r^2) \[CapitalSigma]+(-m+r) \[CapitalSigma]^2) Csc[\[Theta]])/\[CapitalSigma]+2 a^2 u\[Theta] ((2 m r+\[CapitalSigma]+u\[Theta]^2 (\[CapitalSigma]^2+2 m r (a^2+r^2+\[CapitalSigma]))) Cos[\[Theta]]-(2 L^2 m r Cot[\[Theta]] Csc[\[Theta]])/\[CapitalSigma])),1/(2 \[CapitalSigma]^3) 3 ((a^2+r^2-\[CapitalSigma]) (-2 m r (a^2+r^2)+2 m r (1+(a^2+r^2) u\[Theta]^2) \[CapitalSigma]+(1+2 m r u\[Theta]^2) \[CapitalSigma]^2+u\[Theta]^2 \[CapitalSigma]^3) (2 m r^3+r (-2 m+r) \[CapitalSigma]+a^2 (2 m r+\[CapitalSigma])) Cos[\[Theta]]+4 a EE L m r (a^2 L^2 \[CapitalSigma]+(a^2+r^2-\[CapitalSigma]) (2 m r^3+r (-2 m+r) \[CapitalSigma]+a^2 (2 m r+\[CapitalSigma]))) Cos[\[Theta]]+a^2 L^2 (8 m^2 r^2 (-r^2+\[CapitalSigma])+\[CapitalSigma]^3 (1+2 u\[Theta]^2 \[CapitalSigma])-2 a^2 m r (4 m r+\[CapitalSigma] (3-2 u\[Theta]^2 \[CapitalSigma]))+2 m r \[CapitalSigma] (\[CapitalSigma]+2 u\[Theta]^2 \[CapitalSigma]^2+r^2 (-3+2 u\[Theta]^2 \[CapitalSigma]))) Cos[\[Theta]]-4 a^2 L^4 m r \[CapitalSigma] Cot[\[Theta]] Csc[\[Theta]]-ur u\[Theta] \[CapitalSigma] (2 m r^2 (a^2+r^2)-m (a^2+3 r^2) \[CapitalSigma]+(m-r) \[CapitalSigma]^2) (2 m r (a^2+r^2)+\[CapitalDelta] \[CapitalSigma]+2 L^2 \[CapitalSigma] Csc[\[Theta]]^2) Sin[\[Theta]])};
		fa\[Rho]\[CapitalDelta]x1540={1/(2 \[CapitalSigma]^3) 3 L (ur (-2 m r^2 (a^2+r^2)+m (a^2+3 r^2) \[CapitalSigma]+(-m+r) \[CapitalSigma]^2) (2 a m r+EE L \[CapitalSigma] Csc[\[Theta]]^2)+a^2 u\[Theta] (2 a^2 m r+\[CapitalSigma]^2+2 m r (r^2+\[CapitalSigma])) (EE L \[CapitalSigma] Cot[\[Theta]]+a m r Sin[2 \[Theta]])),1/(2 \[CapitalDelta] \[CapitalSigma]) 3 L^2 ur Csc[\[Theta]] (-a^2 u\[Theta] (2 m r (a^2+r^2)+2 m r \[CapitalSigma]+\[CapitalSigma]^2) Cos[\[Theta]]+ur (2 m r^2 (a^2+r^2)-m (a^2+3 r^2) \[CapitalSigma]+(m-r) \[CapitalSigma]^2) Csc[\[Theta]]),1/(2 \[CapitalSigma]) 3 L^2 u\[Theta] Csc[\[Theta]] (-a^2 u\[Theta] (2 m r (a^2+r^2)+2 m r \[CapitalSigma]+\[CapitalSigma]^2) Cos[\[Theta]]+ur (2 m r^2 (a^2+r^2)-m (a^2+3 r^2) \[CapitalSigma]+(m-r) \[CapitalSigma]^2) Csc[\[Theta]]),1/(2 \[CapitalSigma]^3) 3 L (ur (2 m r^2 (a^2+r^2)-m (a^2+3 r^2) \[CapitalSigma]+(m-r) \[CapitalSigma]^2) (2 m r (a^2+r^2)+\[CapitalDelta] \[CapitalSigma]+L^2 \[CapitalSigma] Csc[\[Theta]]^2)-a^2 u\[Theta] (2 a^2 m r+\[CapitalSigma]^2+2 m r (r^2+\[CapitalSigma])) (L^2 \[CapitalSigma] Cot[\[Theta]]+\[CapitalDelta] \[CapitalSigma] Cos[\[Theta]] Sin[\[Theta]]+m r (a^2+r^2) Sin[2 \[Theta]]))};
	
		faEB=cos4\[Beta] (fa\[Rho]\[CapitalDelta]x1504-fa\[Rho]\[CapitalDelta]x1522+fa\[Rho]\[CapitalDelta]x1540) (2-k) (-4+k (4+k))+(fa\[Rho]\[CapitalDelta]x1513-fa\[Rho]\[CapitalDelta]x1531) (2-k) (-4+k (4+k)) sin4\[Beta]+k (-((3 fa\[Rho]\[CapitalDelta]x1504+fa\[Rho]\[CapitalDelta]x1522+3 fa\[Rho]\[CapitalDelta]x1540) (-2+k) k)-6 (fa\[Rho]\[CapitalDelta]x1302+fa\[Rho]\[CapitalDelta]x1320) (-1+k) \[Eta])+sin2\[Beta] (2 (fa\[Rho]\[CapitalDelta]x1513+fa\[Rho]\[CapitalDelta]x1531) k (1+(-1+k) k)+6 fa\[Rho]\[CapitalDelta]x1311 (-2+k) (-1+k) \[Eta])+cos2\[Beta] (4 (fa\[Rho]\[CapitalDelta]x1504-fa\[Rho]\[CapitalDelta]x1540) k (1+(-1+k) k)+6 (fa\[Rho]\[CapitalDelta]x1302-fa\[Rho]\[CapitalDelta]x1320) (-2+k) (-1+k) \[Eta]);
	
		faKB=(3 fa\[Rho]\[CapitalDelta]x1504+fa\[Rho]\[CapitalDelta]x1522+3 fa\[Rho]\[CapitalDelta]x1540) k^2+cos4\[Beta] (fa\[Rho]\[CapitalDelta]x1504-fa\[Rho]\[CapitalDelta]x1522+fa\[Rho]\[CapitalDelta]x1540) (-16+k (16+k))+(fa\[Rho]\[CapitalDelta]x1513-fa\[Rho]\[CapitalDelta]x1531) (-16+k (16+k)) sin4\[Beta]+sin2\[Beta] (-2 (fa\[Rho]\[CapitalDelta]x1513+fa\[Rho]\[CapitalDelta]x1531) (-2+k) k-24 fa\[Rho]\[CapitalDelta]x1311 (-1+k) \[Eta])+cos2\[Beta] (4 (-fa\[Rho]\[CapitalDelta]x1504+fa\[Rho]\[CapitalDelta]x1540) (-2+k) k+24 (-fa\[Rho]\[CapitalDelta]x1302+fa\[Rho]\[CapitalDelta]x1320) (-1+k) \[Eta]);

		(Sqrt[k] (2 EllipticE[k] faEB +(-1+k) EllipticK[k] faKB))/(12 (-1+k)^2 \[Pi] \[Eta]^(5/2))
	],
	CompilationTarget->"C",RuntimeAttributes->{Listable},Parallelization->True]


(* ::Subsection:: *)
(*Subscript[B, t]*)


RPscalarBt=Compile[{{EE,_Real},{L,_Real},{Q,_Real},{a,_Real},{m,_Real},{r,_Real},{\[Theta],_Real},{s\[CapitalDelta]r,_Integer},{sR,_Integer},{s\[Theta],_Integer}},
	Block[
		{\[CapitalSigma],\[CapitalDelta],ur,u\[Theta],uw1d,uw2d,\[Zeta]p\[Xi]Sq,\[Zeta]m\[Xi]Sq,k,\[Mu],cos2\[Beta],sin2\[Beta],cos4\[Beta],sin4\[Beta],fa\[Rho]\[CapitalDelta]x1302,fa\[Rho]\[CapitalDelta]x1311,fa\[Rho]\[CapitalDelta]x1320,fa\[Rho]\[CapitalDelta]x1504,fa\[Rho]\[CapitalDelta]x1513,fa\[Rho]\[CapitalDelta]x1522,fa\[Rho]\[CapitalDelta]x1531,fa\[Rho]\[CapitalDelta]x1540,faEB,faKB},
	
		\[CapitalSigma]=r^2+a^2 Cos[\[Theta]]^2;
		\[CapitalDelta]=a^2-2 m r +r^2;
	
		ur=sR/\[CapitalSigma] Sqrt[(EE (r^2+a^2)-a L)^2-(r^2-2 m r +a^2) (r^2+ (L-a EE)^2+Q)];
		u\[Theta]=s\[Theta]/\[CapitalSigma] Sqrt[Q-Cot[\[Theta]]^2 L^2-a^2 Cos[\[Theta]]^2 (1-EE^2)];
	
		uw1d=(L ((-2 m r+\[CapitalSigma]) (2 m r (a^2+r^2)+\[CapitalDelta] \[CapitalSigma]) Csc[\[Theta]]+4 a^2 m^2 r^2 Sin[\[Theta]]))/(\[CapitalDelta] \[CapitalSigma]^2);
		uw2d=-s\[Theta] Sqrt[Q-Cot[\[Theta]]^2 L^2-a^2 Cos[\[Theta]]^2 (1-EE^2)];

		\[Eta]=1/\[CapitalSigma] \[Sqrt](a^4 (2 m r+\[CapitalSigma])^2+((L^2+Q) \[CapitalSigma]-(r^2-\[CapitalSigma]) (2 m r+EE^2 \[CapitalSigma]))^2+2 a^2 (2 m r+\[CapitalSigma]) ((L^2-Q) \[CapitalSigma]+(r^2-\[CapitalSigma]) (2 m r+EE^2 \[CapitalSigma])));
		\[Zeta]p\[Xi]Sq=a^2+L^2+Q-r (2 m+(-2+EE^2) r)+(2 m r (a^2+r^2))/\[CapitalSigma]+EE^2 \[CapitalSigma];
		\[Zeta]m\[Xi]Sq= a^2-Q-L^2 (1-2 Csc[\[Theta]]^2) -  2 m r+(2 m r (a^2+ r^2))/\[CapitalSigma] - EE^2 (-r^2+\[CapitalSigma]);
		k=(2 \[Eta])/(\[Zeta]p\[Xi]Sq+\[Eta]);
		\[Mu]=uw1d uw2d;
	
		cos2\[Beta]=\[Zeta]m\[Xi]Sq/\[Eta];
		sin2\[Beta]=-((2 \[Mu])/\[Eta]);
	
		cos4\[Beta]=cos2\[Beta]^2-sin2\[Beta]^2;
		sin4\[Beta]=2 sin2\[Beta] cos2\[Beta];
	
		fa\[Rho]\[CapitalDelta]x1302=-(1/2) EE r ur-(a u\[Theta] (-8 L m r Cot[\[Theta]]+a EE (4 m r+\[CapitalSigma]) Sin[2 \[Theta]]))/(4 \[CapitalSigma]);
		fa\[Rho]\[CapitalDelta]x1311=1/\[CapitalSigma]^2 a m (2 (-1+EE^2) r (a^2+r^2-\[CapitalSigma]) Cos[\[Theta]]+2 r (a^2+r^2) u\[Theta]^2 \[CapitalSigma] Cos[\[Theta]]-2 L^2 r Cot[\[Theta]] Csc[\[Theta]]+ur u\[Theta] \[CapitalSigma] (-2 r^2+\[CapitalSigma]) Sin[\[Theta]]);
		fa\[Rho]\[CapitalDelta]x1320=1/(2 \[CapitalSigma]^2) (ur (2 m r^2 (a^2 EE+2 a L+EE r^2)-m (a^2 EE+2 a L+3 EE r^2) \[CapitalSigma]+EE (m-r) \[CapitalSigma]^2)-a u\[Theta] (4 L m r (a^2+r^2) Cot[\[Theta]]+a EE (2 m r (a^2+r^2)+2 m r \[CapitalSigma]+\[CapitalSigma]^2) Cos[\[Theta]] Sin[\[Theta]]));
		fa\[Rho]\[CapitalDelta]x1504=3/4 EE u\[Theta] \[CapitalSigma] (2 r ur u\[Theta] \[CapitalSigma]+a^2 (1+u\[Theta]^2 \[CapitalSigma]) Sin[2 \[Theta]]);
		fa\[Rho]\[CapitalDelta]x1513=-((3 a (a EE L \[CapitalSigma] (1+2 u\[Theta]^2 (-2 m r+\[CapitalSigma]))+2 a^2 m r (1+u\[Theta]^2 (\[CapitalSigma]+2 EE^2 \[CapitalSigma]))+2 m r (r^2-\[CapitalSigma]) (1+u\[Theta]^2 (\[CapitalSigma]+2 EE^2 \[CapitalSigma]))) Cos[\[Theta]])/(2 \[CapitalSigma]))-3 r ur u\[Theta] (a m r+EE L \[CapitalSigma] Csc[\[Theta]]^2) Sin[\[Theta]];
		fa\[Rho]\[CapitalDelta]x1522=3/4 (2 EE ur u\[Theta]^2 (-2 m r^2 (a^2+r^2)+m (a^2+3 r^2) \[CapitalSigma]+(-m+r) \[CapitalSigma]^2)+(2 L r ur (2 a m r+EE L \[CapitalSigma] Csc[\[Theta]]^2))/\[CapitalSigma]+a^2 EE u\[Theta]^3 (2 a^2 m r+\[CapitalSigma]^2+2 m r (r^2+\[CapitalSigma])) Sin[2 \[Theta]]+1/\[CapitalSigma]^2 2 a^2 u\[Theta] (EE \[CapitalSigma] (L^2 (-8 m r+\[CapitalSigma]) Cot[\[Theta]]+\[CapitalSigma]^2 Cos[\[Theta]] Sin[\[Theta]])+m r (a^2 EE (4 m r-\[CapitalSigma])+EE (4 m r-\[CapitalSigma]) (r^2-\[CapitalSigma])+a L (-4 m r+\[CapitalSigma]+4 EE^2 \[CapitalSigma])) Sin[2 \[Theta]]));
		fa\[Rho]\[CapitalDelta]x1531=1/(2 \[CapitalSigma]^3) 3 (-2 a u\[Theta]^2 \[CapitalSigma] (a^2 m r+m r (r^2-\[CapitalSigma])+a EE L \[CapitalSigma]) (2 a^2 m r+\[CapitalSigma]^2+2 m r (r^2+\[CapitalSigma])) Cos[\[Theta]]+a (8 a^2 L^2 m^2 r^2 Cos[\[Theta]]-4 a^2 EE^2 L^2 m r \[CapitalSigma] Cos[\[Theta]]+2 m r (a^2+r^2-\[CapitalSigma]) (2 m r (a^2+r^2)-2 m r \[CapitalSigma]-\[CapitalSigma]^2) Cos[\[Theta]]+a EE (-L (8 m^2 r^2 (a^2+r^2)-2 m r (a^2+r (4 m+r)) \[CapitalSigma]+2 m r \[CapitalSigma]^2+\[CapitalSigma]^3) Cos[\[Theta]]+4 L^3 m r \[CapitalSigma] Cot[\[Theta]] Csc[\[Theta]]))+2 ur u\[Theta] \[CapitalSigma] (2 m r^2 (a^2+r^2)-m (a^2+3 r^2) \[CapitalSigma]+(m-r) \[CapitalSigma]^2) (a m r+EE L \[CapitalSigma] Csc[\[Theta]]^2) Sin[\[Theta]]);
		fa\[Rho]\[CapitalDelta]x1540=1/(2 \[CapitalSigma]^3) 3 L (ur (-2 m r^2 (a^2+r^2)+m (a^2+3 r^2) \[CapitalSigma]+(-m+r) \[CapitalSigma]^2) (2 a m r+EE L \[CapitalSigma] Csc[\[Theta]]^2)+a^2 u\[Theta] (2 a^2 m r+\[CapitalSigma]^2+2 m r (r^2+\[CapitalSigma])) (EE L \[CapitalSigma] Cot[\[Theta]]+a m r Sin[2 \[Theta]]));
	
		faEB=cos4\[Beta] (fa\[Rho]\[CapitalDelta]x1504-fa\[Rho]\[CapitalDelta]x1522+fa\[Rho]\[CapitalDelta]x1540) (2-k) (-4+k (4+k))+(fa\[Rho]\[CapitalDelta]x1513-fa\[Rho]\[CapitalDelta]x1531) (2-k) (-4+k (4+k)) sin4\[Beta]+k (-((3 fa\[Rho]\[CapitalDelta]x1504+fa\[Rho]\[CapitalDelta]x1522+3 fa\[Rho]\[CapitalDelta]x1540) (-2+k) k)-6 (fa\[Rho]\[CapitalDelta]x1302+fa\[Rho]\[CapitalDelta]x1320) (-1+k) \[Eta])+sin2\[Beta] (2 (fa\[Rho]\[CapitalDelta]x1513+fa\[Rho]\[CapitalDelta]x1531) k (1+(-1+k) k)+6 fa\[Rho]\[CapitalDelta]x1311 (-2+k) (-1+k) \[Eta])+cos2\[Beta] (4 (fa\[Rho]\[CapitalDelta]x1504-fa\[Rho]\[CapitalDelta]x1540) k (1+(-1+k) k)+6 (fa\[Rho]\[CapitalDelta]x1302-fa\[Rho]\[CapitalDelta]x1320) (-2+k) (-1+k) \[Eta]);
	
		faKB=(3 fa\[Rho]\[CapitalDelta]x1504+fa\[Rho]\[CapitalDelta]x1522+3 fa\[Rho]\[CapitalDelta]x1540) k^2+cos4\[Beta] (fa\[Rho]\[CapitalDelta]x1504-fa\[Rho]\[CapitalDelta]x1522+fa\[Rho]\[CapitalDelta]x1540) (-16+k (16+k))+(fa\[Rho]\[CapitalDelta]x1513-fa\[Rho]\[CapitalDelta]x1531) (-16+k (16+k)) sin4\[Beta]+sin2\[Beta] (-2 (fa\[Rho]\[CapitalDelta]x1513+fa\[Rho]\[CapitalDelta]x1531) (-2+k) k-24 fa\[Rho]\[CapitalDelta]x1311 (-1+k) \[Eta])+cos2\[Beta] (4 (-fa\[Rho]\[CapitalDelta]x1504+fa\[Rho]\[CapitalDelta]x1540) (-2+k) k+24 (-fa\[Rho]\[CapitalDelta]x1302+fa\[Rho]\[CapitalDelta]x1320) (-1+k) \[Eta]);

		(Sqrt[k] (2 EllipticE[k] faEB +(-1+k) EllipticK[k] faKB))/(12 (-1+k)^2 \[Pi] \[Eta]^(5/2))
	],
	CompilationTarget->"C",RuntimeAttributes->{Listable},Parallelization->True]


(* ::Subsection:: *)
(*Subscript[B, r]*)


RPscalarBr=Compile[{{EE,_Real},{L,_Real},{Q,_Real},{a,_Real},{m,_Real},{r,_Real},{\[Theta],_Real},{s\[CapitalDelta]r,_Integer},{sR,_Integer},{s\[Theta],_Integer}},
	Block[
		{\[CapitalSigma],\[CapitalDelta],ur,u\[Theta],uw1d,uw2d,\[Zeta]p\[Xi]Sq,\[Zeta]m\[Xi]Sq,k,\[Mu],cos2\[Beta],sin2\[Beta],cos4\[Beta],sin4\[Beta],fa\[Rho]\[CapitalDelta]x1302,fa\[Rho]\[CapitalDelta]x1311,fa\[Rho]\[CapitalDelta]x1320,fa\[Rho]\[CapitalDelta]x1504,fa\[Rho]\[CapitalDelta]x1513,fa\[Rho]\[CapitalDelta]x1522,fa\[Rho]\[CapitalDelta]x1531,fa\[Rho]\[CapitalDelta]x1540,faEB,faKB},
	
		\[CapitalSigma]=r^2+a^2 Cos[\[Theta]]^2;
		\[CapitalDelta]=a^2-2 m r +r^2;
	
		ur=sR/\[CapitalSigma] Sqrt[(EE (r^2+a^2)-a L)^2-(r^2-2 m r +a^2) (r^2+ (L-a EE)^2+Q)];
		u\[Theta]=s\[Theta]/\[CapitalSigma] Sqrt[Q-Cot[\[Theta]]^2 L^2-a^2 Cos[\[Theta]]^2 (1-EE^2)];
	
		uw1d=(L ((-2 m r+\[CapitalSigma]) (2 m r (a^2+r^2)+\[CapitalDelta] \[CapitalSigma]) Csc[\[Theta]]+4 a^2 m^2 r^2 Sin[\[Theta]]))/(\[CapitalDelta] \[CapitalSigma]^2);
		uw2d=-s\[Theta] Sqrt[Q-Cot[\[Theta]]^2 L^2-a^2 Cos[\[Theta]]^2 (1-EE^2)];

		\[Eta]=1/\[CapitalSigma] \[Sqrt](a^4 (2 m r+\[CapitalSigma])^2+((L^2+Q) \[CapitalSigma]-(r^2-\[CapitalSigma]) (2 m r+EE^2 \[CapitalSigma]))^2+2 a^2 (2 m r+\[CapitalSigma]) ((L^2-Q) \[CapitalSigma]+(r^2-\[CapitalSigma]) (2 m r+EE^2 \[CapitalSigma])));
		\[Zeta]p\[Xi]Sq=a^2+L^2+Q-r (2 m+(-2+EE^2) r)+(2 m r (a^2+r^2))/\[CapitalSigma]+EE^2 \[CapitalSigma];
		\[Zeta]m\[Xi]Sq= a^2-Q-L^2 (1-2 Csc[\[Theta]]^2) -  2 m r+(2 m r (a^2+ r^2))/\[CapitalSigma] - EE^2 (-r^2+\[CapitalSigma]);
		k=(2 \[Eta])/(\[Zeta]p\[Xi]Sq+\[Eta]);
		\[Mu]=uw1d uw2d;
	
		cos2\[Beta]=\[Zeta]m\[Xi]Sq/\[Eta];
		sin2\[Beta]=-((2 \[Mu])/\[Eta]);
	
		cos4\[Beta]=cos2\[Beta]^2-sin2\[Beta]^2;
		sin4\[Beta]=2 sin2\[Beta] cos2\[Beta];
	
		fa\[Rho]\[CapitalDelta]x1302=(-r (\[CapitalDelta]-ur^2 \[CapitalSigma]+2 u\[Theta]^2 \[CapitalDelta] \[CapitalSigma])+3 a^2 ur u\[Theta] \[CapitalSigma] Cos[\[Theta]] Sin[\[Theta]])/(2 \[CapitalDelta]);
		fa\[Rho]\[CapitalDelta]x1311=1/(\[CapitalDelta] \[CapitalSigma]) (-a ur (2 a^2 EE m r+2 EE m r (r^2-\[CapitalSigma])+a L (-2 m r+\[CapitalSigma])) Cos[\[Theta]]+u\[Theta] Csc[\[Theta]] (-2 L m r^2 (a^2+r^2)+L \[CapitalSigma] (a^2 (m+r)-(m-r) (r^2+\[CapitalSigma]))+a EE m (a^2 (2 r^2-\[CapitalSigma])+r^2 (2 r^2+\[CapitalSigma])) Sin[\[Theta]]^2));
		fa\[Rho]\[CapitalDelta]x1320=1/(2 \[CapitalDelta] \[CapitalSigma]^2) (-2 m r^2 (a^2+r^2) (2 a EE L-\[CapitalDelta])-2 m (a EE L (-a^2+r^2)+r^2 (a^2+r^2) ur^2) \[CapitalSigma]+\[CapitalSigma] (a^2 m+3 m r^2-m \[CapitalSigma]+r \[CapitalSigma]) (-\[CapitalDelta]+ur^2 \[CapitalSigma])+2 L^2 (2 m r^2 (a^2+r^2)-m (a^2+r^2) \[CapitalSigma]+(m-r) \[CapitalSigma]^2) Csc[\[Theta]]^2+a^2 ur u\[Theta] \[CapitalSigma] (2 m r (a^2+r^2)+2 m r \[CapitalSigma]+\[CapitalSigma]^2) Cos[\[Theta]] Sin[\[Theta]]);
		fa\[Rho]\[CapitalDelta]x1504=-((3 ur u\[Theta] \[CapitalSigma]^2 (2 r ur u\[Theta] \[CapitalSigma]+a^2 (1+u\[Theta]^2 \[CapitalSigma]) Sin[2 \[Theta]]))/(4 \[CapitalDelta]));
		fa\[Rho]\[CapitalDelta]x1513=(3 ur \[CapitalSigma] (a (4 a^2 EE m r u\[Theta]^2+4 EE m r u\[Theta]^2 (r^2-\[CapitalSigma])+a L (1+2 u\[Theta]^2 (-2 m r+\[CapitalSigma]))) Cos[\[Theta]]+2 L r ur u\[Theta] \[CapitalSigma] Csc[\[Theta]]))/(2 \[CapitalDelta]);
		fa\[Rho]\[CapitalDelta]x1522=1/(4 \[CapitalDelta]) 3 ur (2 a^2 L^2 u\[Theta] (8 m r-\[CapitalSigma]) Cot[\[Theta]]+2 ur \[CapitalSigma] (u\[Theta]^2 (2 m r^2 (a^2+r^2)-m (a^2+3 r^2) \[CapitalSigma]+(m-r) \[CapitalSigma]^2)-L^2 r Csc[\[Theta]]^2)-a^2 u\[Theta] (-2 m r (a^2-4 a EE L+r^2)+2 m r (1+(a^2+r^2) u\[Theta]^2) \[CapitalSigma]+(1+2 m r u\[Theta]^2) \[CapitalSigma]^2+u\[Theta]^2 \[CapitalSigma]^3) Sin[2 \[Theta]]);
		fa\[Rho]\[CapitalDelta]x1531=1/(2 \[CapitalDelta] \[CapitalSigma]) 3 L ur (a^2 (-2 m r (a^2-2 a EE L+r^2)+2 m r (1+2 (a^2+r^2) u\[Theta]^2) \[CapitalSigma]+(1+4 m r u\[Theta]^2) \[CapitalSigma]^2+2 u\[Theta]^2 \[CapitalSigma]^3) Cos[\[Theta]]+2 (ur u\[Theta] \[CapitalSigma] (-2 m r^2 (a^2+r^2)+m (a^2+3 r^2) \[CapitalSigma]+(-m+r) \[CapitalSigma]^2)-2 a^2 L^2 m r Cot[\[Theta]]) Csc[\[Theta]]);
		fa\[Rho]\[CapitalDelta]x1540=1/(2 \[CapitalDelta] \[CapitalSigma]) 3 L^2 ur Csc[\[Theta]] (-a^2 u\[Theta] (2 m r (a^2+r^2)+2 m r \[CapitalSigma]+\[CapitalSigma]^2) Cos[\[Theta]]+ur (2 m r^2 (a^2+r^2)-m (a^2+3 r^2) \[CapitalSigma]+(m-r) \[CapitalSigma]^2) Csc[\[Theta]]);
	
		faEB=cos4\[Beta] (fa\[Rho]\[CapitalDelta]x1504-fa\[Rho]\[CapitalDelta]x1522+fa\[Rho]\[CapitalDelta]x1540) (2-k) (-4+k (4+k))+(fa\[Rho]\[CapitalDelta]x1513-fa\[Rho]\[CapitalDelta]x1531) (2-k) (-4+k (4+k)) sin4\[Beta]+k (-((3 fa\[Rho]\[CapitalDelta]x1504+fa\[Rho]\[CapitalDelta]x1522+3 fa\[Rho]\[CapitalDelta]x1540) (-2+k) k)-6 (fa\[Rho]\[CapitalDelta]x1302+fa\[Rho]\[CapitalDelta]x1320) (-1+k) \[Eta])+sin2\[Beta] (2 (fa\[Rho]\[CapitalDelta]x1513+fa\[Rho]\[CapitalDelta]x1531) k (1+(-1+k) k)+6 fa\[Rho]\[CapitalDelta]x1311 (-2+k) (-1+k) \[Eta])+cos2\[Beta] (4 (fa\[Rho]\[CapitalDelta]x1504-fa\[Rho]\[CapitalDelta]x1540) k (1+(-1+k) k)+6 (fa\[Rho]\[CapitalDelta]x1302-fa\[Rho]\[CapitalDelta]x1320) (-2+k) (-1+k) \[Eta]);
	
		faKB=(3 fa\[Rho]\[CapitalDelta]x1504+fa\[Rho]\[CapitalDelta]x1522+3 fa\[Rho]\[CapitalDelta]x1540) k^2+cos4\[Beta] (fa\[Rho]\[CapitalDelta]x1504-fa\[Rho]\[CapitalDelta]x1522+fa\[Rho]\[CapitalDelta]x1540) (-16+k (16+k))+(fa\[Rho]\[CapitalDelta]x1513-fa\[Rho]\[CapitalDelta]x1531) (-16+k (16+k)) sin4\[Beta]+sin2\[Beta] (-2 (fa\[Rho]\[CapitalDelta]x1513+fa\[Rho]\[CapitalDelta]x1531) (-2+k) k-24 fa\[Rho]\[CapitalDelta]x1311 (-1+k) \[Eta])+cos2\[Beta] (4 (-fa\[Rho]\[CapitalDelta]x1504+fa\[Rho]\[CapitalDelta]x1540) (-2+k) k+24 (-fa\[Rho]\[CapitalDelta]x1302+fa\[Rho]\[CapitalDelta]x1320) (-1+k) \[Eta]);

		(Sqrt[k] (2 EllipticE[k] faEB +(-1+k) EllipticK[k] faKB))/(12 (-1+k)^2 \[Pi] \[Eta]^(5/2))
	],
	CompilationTarget->"C",RuntimeAttributes->{Listable},Parallelization->True]


(* ::Subsection:: *)
(*Subscript[B, \[Theta]]*)


RPscalarB\[Theta]=Compile[{{EE,_Real},{L,_Real},{Q,_Real},{a,_Real},{m,_Real},{r,_Real},{\[Theta],_Real},{s\[CapitalDelta]r,_Integer},{sR,_Integer},{s\[Theta],_Integer}},
	Block[
		{\[CapitalSigma],\[CapitalDelta],ur,u\[Theta],uw1d,uw2d,\[Zeta]p\[Xi]Sq,\[Zeta]m\[Xi]Sq,k,\[Mu],cos2\[Beta],sin2\[Beta],cos4\[Beta],sin4\[Beta],fa\[Rho]\[CapitalDelta]x1302,fa\[Rho]\[CapitalDelta]x1311,fa\[Rho]\[CapitalDelta]x1320,fa\[Rho]\[CapitalDelta]x1504,fa\[Rho]\[CapitalDelta]x1513,fa\[Rho]\[CapitalDelta]x1522,fa\[Rho]\[CapitalDelta]x1531,fa\[Rho]\[CapitalDelta]x1540,faEB,faKB},
	
		\[CapitalSigma]=r^2+a^2 Cos[\[Theta]]^2;
		\[CapitalDelta]=a^2-2 m r +r^2;
	
		ur=sR/\[CapitalSigma] Sqrt[(EE (r^2+a^2)-a L)^2-(r^2-2 m r +a^2) (r^2+ (L-a EE)^2+Q)];
		u\[Theta]=s\[Theta]/\[CapitalSigma] Sqrt[Q-Cot[\[Theta]]^2 L^2-a^2 Cos[\[Theta]]^2 (1-EE^2)];
	
		uw1d=(L ((-2 m r+\[CapitalSigma]) (2 m r (a^2+r^2)+\[CapitalDelta] \[CapitalSigma]) Csc[\[Theta]]+4 a^2 m^2 r^2 Sin[\[Theta]]))/(\[CapitalDelta] \[CapitalSigma]^2);
		uw2d=-s\[Theta] Sqrt[Q-Cot[\[Theta]]^2 L^2-a^2 Cos[\[Theta]]^2 (1-EE^2)];

		\[Eta]=1/\[CapitalSigma] \[Sqrt](a^4 (2 m r+\[CapitalSigma])^2+((L^2+Q) \[CapitalSigma]-(r^2-\[CapitalSigma]) (2 m r+EE^2 \[CapitalSigma]))^2+2 a^2 (2 m r+\[CapitalSigma]) ((L^2-Q) \[CapitalSigma]+(r^2-\[CapitalSigma]) (2 m r+EE^2 \[CapitalSigma])));
		\[Zeta]p\[Xi]Sq=a^2+L^2+Q-r (2 m+(-2+EE^2) r)+(2 m r (a^2+r^2))/\[CapitalSigma]+EE^2 \[CapitalSigma];
		\[Zeta]m\[Xi]Sq= a^2-Q-L^2 (1-2 Csc[\[Theta]]^2) -  2 m r+(2 m r (a^2+ r^2))/\[CapitalSigma] - EE^2 (-r^2+\[CapitalSigma]);
		k=(2 \[Eta])/(\[Zeta]p\[Xi]Sq+\[Eta]);
		\[Mu]=uw1d uw2d;
	
		cos2\[Beta]=\[Zeta]m\[Xi]Sq/\[Eta];
		sin2\[Beta]=-((2 \[Mu])/\[Eta]);
	
		cos4\[Beta]=cos2\[Beta]^2-sin2\[Beta]^2;
		sin4\[Beta]=2 sin2\[Beta] cos2\[Beta];
	
		fa\[Rho]\[CapitalDelta]x1302=3/4 (2 r ur u\[Theta] \[CapitalSigma]+a^2 (1+u\[Theta]^2 \[CapitalSigma]) Sin[2 \[Theta]]);
		fa\[Rho]\[CapitalDelta]x1311=-((4 a EE m r u\[Theta] (a^2+r^2-\[CapitalSigma]) Cos[\[Theta]])/\[CapitalSigma])+L (-r ur+(u\[Theta] (4 m r (a^2+r^2)-(a^2+r (4 m+r)) \[CapitalSigma]+2 \[CapitalSigma]^2) Cot[\[Theta]])/\[CapitalSigma]) Csc[\[Theta]];
		fa\[Rho]\[CapitalDelta]x1320=1/(4 \[CapitalSigma]^2) ((2 m r (a^2+r^2)-2 m r \[CapitalSigma]+\[CapitalSigma]^2) (-a^2-4 L^2-r^2-\[CapitalSigma]+(a^2+r^2+\[CapitalSigma]) Cos[2 \[Theta]]) Cot[\[Theta]] Csc[\[Theta]]^2+4 a^3 EE L m r Sin[2 \[Theta]]+u\[Theta] \[CapitalSigma] (-4 m r^2 (a^2+r^2) ur+2 ur \[CapitalSigma] (a^2 m+3 m r^2-m \[CapitalSigma]+r \[CapitalSigma])+a^2 u\[Theta] (2 a^2 m r+\[CapitalSigma]^2+2 m r (r^2+\[CapitalSigma])) Sin[2 \[Theta]]));
		fa\[Rho]\[CapitalDelta]x1504=-(3/4) \[CapitalSigma] (1+u\[Theta]^2 \[CapitalSigma]) (2 r ur u\[Theta] \[CapitalSigma]+a^2 (1+u\[Theta]^2 \[CapitalSigma]) Sin[2 \[Theta]]);
		fa\[Rho]\[CapitalDelta]x1513=3 a u\[Theta] (1+u\[Theta]^2 \[CapitalSigma]) (2 a^2 EE m r+2 EE m r (r^2-\[CapitalSigma])+a L (-2 m r+\[CapitalSigma])) Cos[\[Theta]]+3/2 L r ur \[CapitalSigma] (1+2 u\[Theta]^2 \[CapitalSigma]) Csc[\[Theta]];
		fa\[Rho]\[CapitalDelta]x1522=-(1/(4 \[CapitalSigma]))3 (2 a^2 L^2 (u\[Theta]^2 \[CapitalSigma]^2-4 m (r+2 r u\[Theta]^2 \[CapitalSigma])) Cot[\[Theta]]+2 ur u\[Theta] \[CapitalSigma] ((1+u\[Theta]^2 \[CapitalSigma]) (-2 m r^2 (a^2+r^2)+m (a^2+3 r^2) \[CapitalSigma]+(-m+r) \[CapitalSigma]^2)+L^2 r \[CapitalSigma] Csc[\[Theta]]^2)+a^2 (4 a EE L m r (1+2 u\[Theta]^2 \[CapitalSigma])+2 a^2 m r (-1+u\[Theta]^4 \[CapitalSigma]^2)+(1+u\[Theta]^2 \[CapitalSigma]) (\[CapitalSigma]^2 (1+u\[Theta]^2 \[CapitalSigma])+2 m r (\[CapitalSigma]+u\[Theta]^2 \[CapitalSigma]^2+r^2 (-1+u\[Theta]^2 \[CapitalSigma])))) Sin[2 \[Theta]]);
		fa\[Rho]\[CapitalDelta]x1531=3/2 L ((4 a^3 EE L m r u\[Theta] Cos[\[Theta]])/\[CapitalSigma]+(ur (1+2 u\[Theta]^2 \[CapitalSigma]) (-2 m r^2 (a^2+r^2)+m (a^2+3 r^2) \[CapitalSigma]+(-m+r) \[CapitalSigma]^2) Csc[\[Theta]])/\[CapitalSigma]+2 a^2 u\[Theta] ((2 m r+\[CapitalSigma]+u\[Theta]^2 (\[CapitalSigma]^2+2 m r (a^2+r^2+\[CapitalSigma]))) Cos[\[Theta]]-(2 L^2 m r Cot[\[Theta]] Csc[\[Theta]])/\[CapitalSigma]));
		fa\[Rho]\[CapitalDelta]x1540=1/(2 \[CapitalSigma]) 3 L^2 u\[Theta] Csc[\[Theta]] (-a^2 u\[Theta] (2 m r (a^2+r^2)+2 m r \[CapitalSigma]+\[CapitalSigma]^2) Cos[\[Theta]]+ur (2 m r^2 (a^2+r^2)-m (a^2+3 r^2) \[CapitalSigma]+(m-r) \[CapitalSigma]^2) Csc[\[Theta]]);
	
		faEB=cos4\[Beta] (fa\[Rho]\[CapitalDelta]x1504-fa\[Rho]\[CapitalDelta]x1522+fa\[Rho]\[CapitalDelta]x1540) (2-k) (-4+k (4+k))+(fa\[Rho]\[CapitalDelta]x1513-fa\[Rho]\[CapitalDelta]x1531) (2-k) (-4+k (4+k)) sin4\[Beta]+k (-((3 fa\[Rho]\[CapitalDelta]x1504+fa\[Rho]\[CapitalDelta]x1522+3 fa\[Rho]\[CapitalDelta]x1540) (-2+k) k)-6 (fa\[Rho]\[CapitalDelta]x1302+fa\[Rho]\[CapitalDelta]x1320) (-1+k) \[Eta])+sin2\[Beta] (2 (fa\[Rho]\[CapitalDelta]x1513+fa\[Rho]\[CapitalDelta]x1531) k (1+(-1+k) k)+6 fa\[Rho]\[CapitalDelta]x1311 (-2+k) (-1+k) \[Eta])+cos2\[Beta] (4 (fa\[Rho]\[CapitalDelta]x1504-fa\[Rho]\[CapitalDelta]x1540) k (1+(-1+k) k)+6 (fa\[Rho]\[CapitalDelta]x1302-fa\[Rho]\[CapitalDelta]x1320) (-2+k) (-1+k) \[Eta]);
	
		faKB=(3 fa\[Rho]\[CapitalDelta]x1504+fa\[Rho]\[CapitalDelta]x1522+3 fa\[Rho]\[CapitalDelta]x1540) k^2+cos4\[Beta] (fa\[Rho]\[CapitalDelta]x1504-fa\[Rho]\[CapitalDelta]x1522+fa\[Rho]\[CapitalDelta]x1540) (-16+k (16+k))+(fa\[Rho]\[CapitalDelta]x1513-fa\[Rho]\[CapitalDelta]x1531) (-16+k (16+k)) sin4\[Beta]+sin2\[Beta] (-2 (fa\[Rho]\[CapitalDelta]x1513+fa\[Rho]\[CapitalDelta]x1531) (-2+k) k-24 fa\[Rho]\[CapitalDelta]x1311 (-1+k) \[Eta])+cos2\[Beta] (4 (-fa\[Rho]\[CapitalDelta]x1504+fa\[Rho]\[CapitalDelta]x1540) (-2+k) k+24 (-fa\[Rho]\[CapitalDelta]x1302+fa\[Rho]\[CapitalDelta]x1320) (-1+k) \[Eta]);

		(Sqrt[k] (2 EllipticE[k] faEB +(-1+k) EllipticK[k] faKB))/(12 (-1+k)^2 \[Pi] \[Eta]^(5/2))
	],
	CompilationTarget->"C",RuntimeAttributes->{Listable},Parallelization->True]


(* ::Subsection:: *)
(*Subscript[B, \[Phi]]*)


RPscalarB\[Phi]=Compile[{{EE,_Real},{L,_Real},{Q,_Real},{a,_Real},{m,_Real},{r,_Real},{\[Theta],_Real},{s\[CapitalDelta]r,_Integer},{sR,_Integer},{s\[Theta],_Integer}},
	Block[
		{\[CapitalSigma],\[CapitalDelta],ur,u\[Theta],uw1d,uw2d,\[Zeta]p\[Xi]Sq,\[Zeta]m\[Xi]Sq,k,\[Mu],cos2\[Beta],sin2\[Beta],cos4\[Beta],sin4\[Beta],fa\[Rho]\[CapitalDelta]x1302,fa\[Rho]\[CapitalDelta]x1311,fa\[Rho]\[CapitalDelta]x1320,fa\[Rho]\[CapitalDelta]x1504,fa\[Rho]\[CapitalDelta]x1513,fa\[Rho]\[CapitalDelta]x1522,fa\[Rho]\[CapitalDelta]x1531,fa\[Rho]\[CapitalDelta]x1540,faEB,faKB},
	
		\[CapitalSigma]=r^2+a^2 Cos[\[Theta]]^2;
		\[CapitalDelta]=a^2-2 m r +r^2;
	
		ur=sR/\[CapitalSigma] Sqrt[(EE (r^2+a^2)-a L)^2-(r^2-2 m r +a^2) (r^2+ (L-a EE)^2+Q)];
		u\[Theta]=s\[Theta]/\[CapitalSigma] Sqrt[Q-Cot[\[Theta]]^2 L^2-a^2 Cos[\[Theta]]^2 (1-EE^2)];
	
		uw1d=(L ((-2 m r+\[CapitalSigma]) (2 m r (a^2+r^2)+\[CapitalDelta] \[CapitalSigma]) Csc[\[Theta]]+4 a^2 m^2 r^2 Sin[\[Theta]]))/(\[CapitalDelta] \[CapitalSigma]^2);
		uw2d=-s\[Theta] Sqrt[Q-Cot[\[Theta]]^2 L^2-a^2 Cos[\[Theta]]^2 (1-EE^2)];

		\[Eta]=1/\[CapitalSigma] \[Sqrt](a^4 (2 m r+\[CapitalSigma])^2+((L^2+Q) \[CapitalSigma]-(r^2-\[CapitalSigma]) (2 m r+EE^2 \[CapitalSigma]))^2+2 a^2 (2 m r+\[CapitalSigma]) ((L^2-Q) \[CapitalSigma]+(r^2-\[CapitalSigma]) (2 m r+EE^2 \[CapitalSigma])));
		\[Zeta]p\[Xi]Sq=a^2+L^2+Q-r (2 m+(-2+EE^2) r)+(2 m r (a^2+r^2))/\[CapitalSigma]+EE^2 \[CapitalSigma];
		\[Zeta]m\[Xi]Sq= a^2-Q-L^2 (1-2 Csc[\[Theta]]^2) -  2 m r+(2 m r (a^2+ r^2))/\[CapitalSigma] - EE^2 (-r^2+\[CapitalSigma]);
		k=(2 \[Eta])/(\[Zeta]p\[Xi]Sq+\[Eta]);
		\[Mu]=uw1d uw2d;
	
		cos2\[Beta]=\[Zeta]m\[Xi]Sq/\[Eta];
		sin2\[Beta]=-((2 \[Mu])/\[Eta]);
	
		cos4\[Beta]=cos2\[Beta]^2-sin2\[Beta]^2;
		sin4\[Beta]=2 sin2\[Beta] cos2\[Beta];
	
		fa\[Rho]\[CapitalDelta]x1302=(L r ur \[CapitalSigma]+L u\[Theta] (-4 m r (a^2+r^2)+(a^2+r (4 m+r)) \[CapitalSigma]-3 \[CapitalSigma]^2) Cot[\[Theta]]+2 a EE m r u\[Theta] (a^2+r^2-\[CapitalSigma]) Sin[2 \[Theta]])/(2 \[CapitalSigma]);
		fa\[Rho]\[CapitalDelta]x1311=1/\[CapitalSigma]^2 (-4 a EE L m r (a^2+r^2-\[CapitalSigma]) Cos[\[Theta]]+(2 m r (a^2+r^2) (a^2+r^2-\[CapitalSigma])-u\[Theta]^2 \[CapitalSigma] (2 m r (a^2+r^2)^2+\[CapitalDelta] \[CapitalSigma]^2)) Cos[\[Theta]]+L^2 (4 m r (a^2+r^2)-4 m r \[CapitalSigma]+\[CapitalSigma]^2) Cot[\[Theta]] Csc[\[Theta]]+ur u\[Theta] \[CapitalSigma] (m (a^2+r^2-\[CapitalSigma]) (2 r^2-\[CapitalSigma])-r \[CapitalSigma]^2) Sin[\[Theta]]);
		fa\[Rho]\[CapitalDelta]x1320=1/2 L ((3 ur (-2 m r^2 (a^2+r^2)+m (a^2+3 r^2) \[CapitalSigma]+(-m+r) \[CapitalSigma]^2))/\[CapitalSigma]^2+u\[Theta] (3 \[CapitalDelta]+(6 m r (a^2+r^2)^2)/\[CapitalSigma]^2-\[CapitalSigma]) Cot[\[Theta]]);
		fa\[Rho]\[CapitalDelta]x1504=-(3/4) L u\[Theta] \[CapitalSigma] (2 r ur u\[Theta] \[CapitalSigma]+a^2 (1+u\[Theta]^2 \[CapitalSigma]) Sin[2 \[Theta]]);
		fa\[Rho]\[CapitalDelta]x1513=3/2 (4 a EE L m r u\[Theta]^2 (a^2+r^2-\[CapitalSigma]) Cos[\[Theta]]+(a^2 L^2 (1+2 u\[Theta]^2 (-2 m r+\[CapitalSigma]))+(u\[Theta]^2+1/\[CapitalSigma]) (a^2+r^2-\[CapitalSigma]) (2 m r^3+r (-2 m+r) \[CapitalSigma]+a^2 (2 m r+\[CapitalSigma]))) Cos[\[Theta]]+r ur u\[Theta] (2 m r (a^2+r^2)+\[CapitalDelta] \[CapitalSigma]+2 L^2 \[CapitalSigma] Csc[\[Theta]]^2) Sin[\[Theta]]);
		fa\[Rho]\[CapitalDelta]x1522=1/(2 \[CapitalSigma]^2) 3 (-L (ur \[CapitalSigma] (a^2 r \[CapitalSigma]+r \[CapitalSigma] (r^2+u\[Theta]^2 \[CapitalSigma]^2)+m (a^2+r^2-\[CapitalSigma]) (u\[Theta]^2 \[CapitalSigma]^2+r^2 (2-2 u\[Theta]^2 \[CapitalSigma]))+L^2 r \[CapitalSigma] Csc[\[Theta]]^2)+a^2 u\[Theta] (L^2 \[CapitalSigma] (-8 m r+\[CapitalSigma]) Cot[\[Theta]]+(-8 m^2 r^2 (a^2+r^2)-4 m r \[CapitalDelta] \[CapitalSigma]+(a^2+r^2) (1+2 m r u\[Theta]^2) \[CapitalSigma]^2+(1+2 m r u\[Theta]^2) \[CapitalSigma]^3+u\[Theta]^2 \[CapitalSigma]^4) Cos[\[Theta]] Sin[\[Theta]]))+4 a EE m r u\[Theta] (-((a^2+r^2-\[CapitalSigma]) (2 m r^3+r (-2 m+r) \[CapitalSigma]+a^2 (2 m r+\[CapitalSigma])) Cos[\[Theta]] Sin[\[Theta]])-a^2 L^2 \[CapitalSigma] Sin[2 \[Theta]]));
		fa\[Rho]\[CapitalDelta]x1531=1/(2 \[CapitalSigma]^3) 3 ((a^2+r^2-\[CapitalSigma]) (-2 m r (a^2+r^2)+2 m r (1+(a^2+r^2) u\[Theta]^2) \[CapitalSigma]+(1+2 m r u\[Theta]^2) \[CapitalSigma]^2+u\[Theta]^2 \[CapitalSigma]^3) (2 m r^3+r (-2 m+r) \[CapitalSigma]+a^2 (2 m r+\[CapitalSigma])) Cos[\[Theta]]+4 a EE L m r (a^2 L^2 \[CapitalSigma]+(a^2+r^2-\[CapitalSigma]) (2 m r^3+r (-2 m+r) \[CapitalSigma]+a^2 (2 m r+\[CapitalSigma]))) Cos[\[Theta]]+a^2 L^2 (8 m^2 r^2 (-r^2+\[CapitalSigma])+\[CapitalSigma]^3 (1+2 u\[Theta]^2 \[CapitalSigma])-2 a^2 m r (4 m r+\[CapitalSigma] (3-2 u\[Theta]^2 \[CapitalSigma]))+2 m r \[CapitalSigma] (\[CapitalSigma]+2 u\[Theta]^2 \[CapitalSigma]^2+r^2 (-3+2 u\[Theta]^2 \[CapitalSigma]))) Cos[\[Theta]]-4 a^2 L^4 m r \[CapitalSigma] Cot[\[Theta]] Csc[\[Theta]]-ur u\[Theta] \[CapitalSigma] (2 m r^2 (a^2+r^2)-m (a^2+3 r^2) \[CapitalSigma]+(m-r) \[CapitalSigma]^2) (2 m r (a^2+r^2)+\[CapitalDelta] \[CapitalSigma]+2 L^2 \[CapitalSigma] Csc[\[Theta]]^2) Sin[\[Theta]]);
		fa\[Rho]\[CapitalDelta]x1540=1/(2 \[CapitalSigma]^3) 3 L (ur (2 m r^2 (a^2+r^2)-m (a^2+3 r^2) \[CapitalSigma]+(m-r) \[CapitalSigma]^2) (2 m r (a^2+r^2)+\[CapitalDelta] \[CapitalSigma]+L^2 \[CapitalSigma] Csc[\[Theta]]^2)-a^2 u\[Theta] (2 a^2 m r+\[CapitalSigma]^2+2 m r (r^2+\[CapitalSigma])) (L^2 \[CapitalSigma] Cot[\[Theta]]+\[CapitalDelta] \[CapitalSigma] Cos[\[Theta]] Sin[\[Theta]]+m r (a^2+r^2) Sin[2 \[Theta]]));
	
		faEB=cos4\[Beta] (fa\[Rho]\[CapitalDelta]x1504-fa\[Rho]\[CapitalDelta]x1522+fa\[Rho]\[CapitalDelta]x1540) (2-k) (-4+k (4+k))+(fa\[Rho]\[CapitalDelta]x1513-fa\[Rho]\[CapitalDelta]x1531) (2-k) (-4+k (4+k)) sin4\[Beta]+k (-((3 fa\[Rho]\[CapitalDelta]x1504+fa\[Rho]\[CapitalDelta]x1522+3 fa\[Rho]\[CapitalDelta]x1540) (-2+k) k)-6 (fa\[Rho]\[CapitalDelta]x1302+fa\[Rho]\[CapitalDelta]x1320) (-1+k) \[Eta])+sin2\[Beta] (2 (fa\[Rho]\[CapitalDelta]x1513+fa\[Rho]\[CapitalDelta]x1531) k (1+(-1+k) k)+6 fa\[Rho]\[CapitalDelta]x1311 (-2+k) (-1+k) \[Eta])+cos2\[Beta] (4 (fa\[Rho]\[CapitalDelta]x1504-fa\[Rho]\[CapitalDelta]x1540) k (1+(-1+k) k)+6 (fa\[Rho]\[CapitalDelta]x1302-fa\[Rho]\[CapitalDelta]x1320) (-2+k) (-1+k) \[Eta]);
	
		faKB=(3 fa\[Rho]\[CapitalDelta]x1504+fa\[Rho]\[CapitalDelta]x1522+3 fa\[Rho]\[CapitalDelta]x1540) k^2+cos4\[Beta] (fa\[Rho]\[CapitalDelta]x1504-fa\[Rho]\[CapitalDelta]x1522+fa\[Rho]\[CapitalDelta]x1540) (-16+k (16+k))+(fa\[Rho]\[CapitalDelta]x1513-fa\[Rho]\[CapitalDelta]x1531) (-16+k (16+k)) sin4\[Beta]+sin2\[Beta] (-2 (fa\[Rho]\[CapitalDelta]x1513+fa\[Rho]\[CapitalDelta]x1531) (-2+k) k-24 fa\[Rho]\[CapitalDelta]x1311 (-1+k) \[Eta])+cos2\[Beta] (4 (-fa\[Rho]\[CapitalDelta]x1504+fa\[Rho]\[CapitalDelta]x1540) (-2+k) k+24 (-fa\[Rho]\[CapitalDelta]x1302+fa\[Rho]\[CapitalDelta]x1320) (-1+k) \[Eta]);

		(Sqrt[k] (2 EllipticE[k] faEB +(-1+k) EllipticK[k] faKB))/(12 (-1+k)^2 \[Pi] \[Eta]^(5/2))
	],
	CompilationTarget->"C",RuntimeAttributes->{Listable},Parallelization->True]
