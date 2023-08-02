#!/usr/local/bin/MathematicaScript -script
(* generate paintworthy ge.m *)

a = ToExpression[$ScriptCommandLine[[2]]];
b = ToExpression[$ScriptCommandLine[[3]]];
c = ToExpression[$ScriptCommandLine[[4]]];
d = ToExpression[$ScriptCommandLine[[5]]];
par[x_][r_][u_,v_] := Module[{xu,xv,n1,n2,n3,n4},
    xu = D[x[uu,vv],uu];
    xv = D[x[uu,vv],vv];
    n1 = cross[xu,xv]//Simplify;
    n2 = Simplify[Factor[n1.n1]];
    n3 = PowerExpand[Sqrt[n2]];
    n4 = n1/n3
    Simplify[Together[x[uu,vv] + 
        r n4]]] /. {uu->u,vv->v}

hel[Rw_,Rz_, Ry_, Rx_][u_,v_] := {
	Rz (3-2 Cos[u/2]) JacobiSN[u,Rw] Sin[u/2] Cos[Ry v/2],
	Rz (3-2 Cos[u/2]) JacobiCN[u,Ry] Rz Cos[Rw v/2] Cos[u/2],
	JacobiSN[u,Rx] (1+2 Cos[Rw v/2])
}

S01 = hel[a,b,c,d][u,v]
S02 = par[hel[a,b,c,d]][u,v]
S03 = par[hel[-a,-b,-c,-d]][u,v]
S04 = par[hel[a/b,b/a,c/a,-d/c]][u,v]
S05 = par[hel[a/c,b/c,c/b,c/d]][u,v]
S06 = par[hel[-a/b,-b/a,-c/a,-a/c]][u,v]
S07 = par[hel[-a/c,-b/c,-c/b,a/c]][u,v]
S08 = par[hel[c,b,a,d]][u,v]
S09 = par[hel[b,a,c,d]][u,v]
S10 = par[hel[-b,-a,-c,-d]][u,v]
S11 = par[hel[-c,-b,-a,-d]][u,v]
S12 = par[hel[a*a,b*b,c*c,d*d]][u,v]
S13 = par[hel[a*b,c*b,c*a,d*b]][u,v]
S14 = par[hel[-c*a,-b*c,-a*b,d*a]][u,v]
S15 = par[hel[-c*c,-b*b,-a*a,a*c]][u,v]
S16 = par[hel[-c*b,-b*a,-a*c,b*d]][u,v]

plot = 
    ParametricPlot3D[
    {
    
    S01,S02,S03,S04,S05,S06,S07,S08,S09,S10,S11,S12,S13,S14,S15,S16

    }
    //Evaluate,
    {u,-26 Pi,26 Pi},{v,-26 Pi,26 Pi}
    ];
Export["name.obj",plot];
