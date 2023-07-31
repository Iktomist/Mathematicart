#!/usr/local/bin/MathematicaScript -script
(* generate paintworthy ge.m *)

a = ToExpression[$ScriptCommandLine[[2]]];
b = ToExpression[$ScriptCommandLine[[3]]];
c = ToExpression[$ScriptCommandLine[[4]]];
d = ToExpression[$ScriptCommandLine[[5]]];
par[x_][r_][space1_,space2_] := Module[{xu,xv,n1,n2,n3,n4},
    xu = D[x[uu,vv],uu];
    xv = D[x[uu,vv],vv];
    n1 = cross[xu,xv]//Simplify;
    n2 = Simplify[Factor[n1.n1]];
    n3 = PowerExpand[Sqrt[n2]];
    n4 = n1/n3
    Simplify[Together[x[uu,vv] + 
        r n4]]] /. {uu->space1,vv->space2}

hel[Rz_, Rw_, Rl_, k_][space1_,space2_] := 
{k (1+ 2 Sin[JacobiSN[space1 - 2,k]^2 - 2]) + Rl (1+ 2 Sin[.5 k space2 - 2]*2) Rw JacobiSN[space2,k] Rl Sec[space1] JacobiSN[space2,k],
 k (1+ 2 JacobiSN[.5 space1 - 2, k]*2) + Rl (1+ 2 Sin[.5 k space2 - 2]*2) JacobiSN[space2,k] + Rw Cos[space1/2] JacobiSN[space2,k],
 Rw (1+ 2 JacobiSN[.5 space1, k]) Sec[k space2/2]}

S01 = hel[a,b,c,d][space1,space2]
S02 = par[hel[a,b,c,d]][space1,space2]
S03 = par[hel[-a,-b,-c,-d]][space1,space2]
S04 = par[hel[a/b,b/a,c/a,-d/c]][space1,space2]
S05 = par[hel[a/c,b/c,c/b,c/d]][space1,space2]
S06 = par[hel[-a/b,-b/a,-c/a,-a/c]][space1,space2]
S07 = par[hel[-a/c,-b/c,-c/b,a/c]][space1,space2]
S08 = par[hel[c,b,a,d]][space1,space2]
S09 = par[hel[b,a,c,d]][space1,space2]
S10 = par[hel[-b,-a,-c,-d]][space1,space2]
S11 = par[hel[-c,-b,-a,-d]][space1,space2]
S12 = par[hel[a*a,b*b,c*c,d*d]][space1,space2]
S13 = par[hel[a*b,c*b,c*a,d*b]][space1,space2]
S14 = par[hel[-c*a,-b*c,-a*b,d*a]][space1,space2]
S15 = par[hel[-c*c,-b*b,-a*a,a*c]][space1,space2]
S16 = par[hel[-c*b,-b*a,-a*c,b*d]][space1,space2]

plot = 
    ParametricPlot3D[
    {
    
    S01,S02,S03,S04,S05,S06,S07,S08,S09,S10,S11,S12,S13,S14,S15,S16

    }
    //Evaluate,
    {space1,-Pi,Pi},{space2,-Pi,Pi}
    ];
Export["name.obj",plot];

