       subroutine t3A100100_update(N0,N1,N2,N3,HT3A,shift,
     & M1,M2,K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,
     & FAHH,FAHP,FAPP,FBHH,FBHP,FBPP,
     & VAHHHH,VAHHHP,VAHHPP,VAHPHP,VAHPPP,
     & VBHHHH,VBHHHP,VBHHPH,VBHHPP,VBHPHP,VBHPPH,
     & VBPHPH,VBHPPP,VBPHPP,
     & VCHHHH,VCHHHP,VCHHPP,VCHPHP,VCHPPP,
     & VAAPPP,VBAPPP,VBPAPP,VCAPPP,
     & t1A,t1B,t2A,t2B,t2C,
     & t3A,t3B1,t3B2,t3B3,t3B4,t3C1,t3C2,t3C3,t3C4,t3D)
C

       integer a,b,c,e,f,g,h,i,j,k,m,n,o,p
       real*8 CoeLeft,shift,PP
       real*8 FAHH(N0+1:N1,N0+1:N1)
       real*8 FAHP(N1+1:N3,N0+1:N1)
       real*8 FAPP(N1+1:N3,N1+1:N3)
       real*8 FBHH(N0+1:N2,N0+1:N2)
       real*8 FBHP(N2+1:N3,N0+1:N2)
       real*8 FBPP(N2+1:N3,N2+1:N3)
       real*8 VAHHHH(N0+1:N1,N0+1:N1,N0+1:N1,N0+1:N1)
       real*8 VAHHHP(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1)
       real*8 VAHHPP(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
       real*8 VAHPHP(N1+1:N3,N0+1:N1,N1+1:N3,N0+1:N1)
       real*8 VAHPPP(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1)
       real*8 VBHHHH(N0+1:N2,N0+1:N1,N0+1:N2,N0+1:N1)
       real*8 VBHHHP(N2+1:N3,N0+1:N1,N0+1:N2,N0+1:N1)
       real*8 VBHHPH(N0+1:N2,N1+1:N3,N0+1:N2,N0+1:N1)
       real*8 VBHHPP(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
       real*8 VBHPHP(N2+1:N3,N0+1:N1,N2+1:N3,N0+1:N1)
       real*8 VBHPPH(N0+1:N2,N1+1:N3,N2+1:N3,N0+1:N1)
       real*8 VBPHPH(N0+1:N2,N1+1:N3,N0+1:N2,N1+1:N3)
       real*8 VBHPPP(N2+1:N3,N1+1:N3,N2+1:N3,N0+1:N1)
       real*8 VBPHPP(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:N3)
       real*8 VCHHHH(N0+1:N2,N0+1:N2,N0+1:N2,N0+1:N2)
       real*8 VCHHHP(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2)
       real*8 VCHHPP(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
       real*8 VCHPHP(N2+1:N3,N0+1:N2,N2+1:N3,N0+1:N2)
       real*8 VCHPPP(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2)
       real*8 VAAPPP(N1+1:N3,N1+1:N3,N1+1:N3,N1+1:M2)
       real*8 VBAPPP(N2+1:N3,N1+1:N3,N2+1:N3,N1+1:M2)
       real*8 VBPAPP(N2+1:N3,N1+1:N3,N2+1:M2,N1+1:N3)
       real*8 VCAPPP(N2+1:N3,N2+1:N3,N2+1:N3,N2+1:M2)
       real*8 t1A(N1+1:N3,N0+1:N1)
       real*8 t1B(N2+1:N3,N0+1:N2)
       real*8 t2A(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1)
       real*8 t2B(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1)
       real*8 t2C(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2)
       real*8 t3A(N1+1:N3,N1+1:N3,N1+1:M2,N0+1:N1,N0+1:N1,M1+1:N1)
       real*8 t3B1(N2+1:N3,N1+1:N3,N1+1:M2,N0+1:N2,N0+1:N1,M1+1:N1)
       real*8 t3B2(N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,N0+1:M1,N0+1:M1)
       real*8 t3B3(N2+1:M2,M2+1:N3,M2+1:N3,N0+1:N2,N0+1:N1,M1+1:N1)
       real*8 t3B4(N2+1:N3,N1+1:N3,N1+1:M2,M1+1:N2,N0+1:M1,N0+1:M1)
       real*8 t3C1(N2+1:N3,N2+1:M2,N1+1:N3,N0+1:N2,M1+1:N2,N0+1:N1)
       real*8 t3C2(M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1)
       real*8 t3C3(M2+1:N3,M2+1:N3,N1+1:M2,N0+1:N2,M1+1:N2,N0+1:N1)
       real*8 t3C4(N2+1:N3,N2+1:M2,N1+1:N3,N0+1:M1,N0+1:M1,M1+1:N1)
       real*8 t3D(N2+1:N3,N2+1:N3,N2+1:M2,N0+1:N2,N0+1:N2,M1+1:N2)
       real*8 HT3A(N1+1:N3,N1+1:N3,N1+1:M2,N0+1:N1,N0+1:N1,M1+1:N1)
C
       real*8,allocatable::V3A(:,:,:,:,:,:)
       real*8,allocatable::B1(:,:)
       real*8,allocatable::B2(:,:)
       real*8,allocatable::C1(:,:,:)
       real*8,allocatable::C2(:,:,:)
       real*8,allocatable::D1(:,:,:,:)
       real*8,allocatable::D2(:,:,:,:)
       real*8,allocatable::F1(:,:,:,:,:,:)
       real*8,allocatable::F2(:,:,:,:,:,:)
       real*8,allocatable::S1(:,:,:,:)
       real*8,allocatable::S2(:,:,:,:)
       real*8,allocatable::S3(:,:,:,:)
       real*8,allocatable::S4(:,:,:,:)
       real*8,allocatable::S5(:,:,:,:)
       real*8,allocatable::S6(:,:,:,:)
       real*8,allocatable::S7(:,:,:,:)
       real*8,allocatable::S8(:,:,:,:)
       real*8,allocatable::S9(:,:,:,:)
       real*8,allocatable::S10(:,:,:,:)
       real*8,allocatable::S11(:,:,:,:)
       real*8,allocatable::S12(:,:,:,:)
       real*8,allocatable::S13(:,:,:,:)
       real*8,allocatable::S14(:,:,:,:)
       real*8,allocatable::S15(:,:,:,:)
       real*8,allocatable::S16(:,:,:,:)
       real*8,allocatable::S17(:,:,:,:)
       real*8,allocatable::S18(:,:,:,:)
       real*8,allocatable::Q1(:,:)
       real*8,allocatable::Q2(:,:)
       real*8,allocatable::Q3(:,:)
       real*8,allocatable::Q4(:,:)
       real*8,allocatable::Q5(:,:)
       real*8,allocatable::Q6(:,:)
       real*8,allocatable::S19(:,:,:,:)
       real*8,allocatable::S20(:,:,:,:)
       real*8,allocatable::S21(:,:,:,:)
       real*8,allocatable::S22(:,:,:,:)
       real*8,allocatable::S23(:,:,:,:)
       real*8,allocatable::Q7(:,:)
       real*8,allocatable::S24(:,:,:,:)
       real*8,allocatable::S25(:,:,:,:)
       real*8,allocatable::S26(:,:,:,:)
       real*8,allocatable::S27(:,:,:,:)
       real*8,allocatable::S28(:,:,:,:)
       real*8,allocatable::S29(:,:,:,:)
       real*8,allocatable::S30(:,:,:,:)
       real*8,allocatable::S31(:,:,:,:)
       real*8,allocatable::S32(:,:,:,:)
       real*8,allocatable::S33(:,:,:,:)
       real*8,allocatable::S34(:,:,:,:)
       real*8,allocatable::S35(:,:,:,:)
       real*8,allocatable::S36(:,:,:,:)
       real*8,allocatable::S37(:,:,:,:)
       real*8,allocatable::Q8(:,:)
       real*8,allocatable::Q9(:,:)
       real*8,allocatable::S38(:,:,:,:)
       real*8,allocatable::S39(:,:,:,:)
       real*8,allocatable::S40(:,:,:,:)
       real*8,allocatable::S41(:,:,:,:)
       real*8,allocatable::S42(:,:,:,:)
       real*8,allocatable::S43(:,:,:,:)
       real*8,allocatable::U94(:,:,:,:,:,:)
       real*8,allocatable::S44(:,:,:,:)
       real*8,allocatable::S45(:,:,:,:)
       real*8,allocatable::S46(:,:,:,:)
       real*8,allocatable::S47(:,:,:,:)
       real*8,allocatable::Q10(:,:)
       real*8,allocatable::Q11(:,:)
       real*8,allocatable::S48(:,:,:,:)
       real*8,allocatable::S49(:,:,:,:)
       real*8,allocatable::Q12(:,:)
       real*8,allocatable::S50(:,:,:,:)
       real*8,allocatable::S51(:,:,:,:)
       real*8,allocatable::S52(:,:,:,:)
       real*8,allocatable::S53(:,:,:,:)
       real*8,allocatable::S54(:,:,:,:)
       real*8,allocatable::S55(:,:,:,:)
       real*8,allocatable::S56(:,:,:,:)
       real*8,allocatable::S57(:,:,:,:)
       real*8,allocatable::S58(:,:,:,:)
       real*8,allocatable::S59(:,:,:,:)
       real*8,allocatable::S60(:,:,:,:)
       real*8,allocatable::S61(:,:,:,:)
       real*8,allocatable::S62(:,:,:,:)
       real*8,allocatable::S63(:,:,:,:)
       real*8,allocatable::S64(:,:,:,:)
       real*8,allocatable::S65(:,:,:,:)
       real*8,allocatable::S66(:,:,:,:)
       real*8,allocatable::S67(:,:,:,:)
       real*8,allocatable::Q13(:,:)
       real*8,allocatable::Q14(:,:)
       real*8,allocatable::Q15(:,:)
       real*8,allocatable::Q16(:,:)
       real*8,allocatable::Q17(:,:)
       real*8,allocatable::Q18(:,:)
       real*8,allocatable::S68(:,:,:,:)
       real*8,allocatable::S69(:,:,:,:)
       real*8,allocatable::S70(:,:,:,:)
       real*8,allocatable::S71(:,:,:,:)
       real*8,allocatable::S72(:,:,:,:)
       real*8,allocatable::S73(:,:,:,:)
       real*8,allocatable::S74(:,:,:,:)
       real*8,allocatable::S75(:,:,:,:)
       real*8,allocatable::S76(:,:,:,:)
       real*8,allocatable::S77(:,:,:,:)
       real*8,allocatable::S78(:,:,:,:)
       real*8,allocatable::S79(:,:,:,:)
       real*8,allocatable::S80(:,:,:,:)
       real*8,allocatable::S81(:,:,:,:)
       real*8,allocatable::S82(:,:,:,:)
       real*8,allocatable::S83(:,:,:,:)
       real*8,allocatable::S84(:,:,:,:)
       real*8,allocatable::S85(:,:,:,:)
       real*8,allocatable::S86(:,:,:,:)
       real*8,allocatable::S87(:,:,:,:)
       real*8,allocatable::S88(:,:,:,:)
       real*8,allocatable::S89(:,:,:,:)
       real*8,allocatable::S90(:,:,:,:)
       real*8,allocatable::S91(:,:,:,:)
       real*8,allocatable::S92(:,:,:,:)
       real*8,allocatable::S93(:,:,:,:)
       real*8,allocatable::S94(:,:,:,:)
       real*8,allocatable::S95(:,:,:,:)
       real*8,allocatable::S96(:,:,:,:)
       real*8,allocatable::S97(:,:,:,:)
       real*8,allocatable::S98(:,:,:,:)
       real*8,allocatable::S99(:,:,:,:)
       real*8,allocatable::S100(:,:,:,:)
       real*8,allocatable::S101(:,:,:,:)
       real*8,allocatable::S102(:,:,:,:)
       real*8,allocatable::S103(:,:,:,:)
       real*8,allocatable::S104(:,:,:,:)
       real*8,allocatable::S105(:,:,:,:)
       real*8,allocatable::S106(:,:,:,:)
       real*8,allocatable::S107(:,:,:,:)
       real*8,allocatable::S108(:,:,:,:)
       real*8,allocatable::S109(:,:,:,:)
       real*8,allocatable::S110(:,:,:,:)
       real*8,allocatable::S111(:,:,:,:)
       real*8,allocatable::S112(:,:,:,:)
       real*8,allocatable::S113(:,:,:,:)
       real*8,allocatable::S114(:,:,:,:)
       real*8,allocatable::S115(:,:,:,:)
       real*8,allocatable::S116(:,:,:,:)
       real*8,allocatable::S117(:,:,:,:)
       real*8,allocatable::S118(:,:,:,:)
       real*8,allocatable::S119(:,:,:,:)
       real*8,allocatable::Q19(:,:)
       real*8,allocatable::S120(:,:,:,:)
       real*8,allocatable::S121(:,:,:,:)
       real*8,allocatable::S122(:,:,:,:)
       real*8,allocatable::S123(:,:,:,:)
       real*8,allocatable::S124(:,:,:,:)
       real*8,allocatable::S125(:,:,:,:)
       real*8,allocatable::S126(:,:,:,:)
       real*8,allocatable::S127(:,:,:,:)
       real*8,allocatable::S128(:,:,:,:)
       real*8,allocatable::S129(:,:,:,:)
       real*8,allocatable::S130(:,:,:,:)
       real*8,allocatable::S131(:,:,:,:)
       real*8,allocatable::S132(:,:,:,:)
       real*8,allocatable::S133(:,:,:,:)
       real*8,allocatable::S134(:,:,:,:)
       real*8,allocatable::S135(:,:,:,:)
       real*8,allocatable::S136(:,:,:,:)
       real*8,allocatable::S137(:,:,:,:)
       real*8,allocatable::S138(:,:,:,:)
       real*8,allocatable::S139(:,:,:,:)
       real*8,allocatable::S140(:,:,:,:)
       real*8,allocatable::S141(:,:,:,:)
       real*8,allocatable::S142(:,:,:,:)
       real*8,allocatable::S143(:,:,:,:)
       real*8,allocatable::S144(:,:,:,:)
       real*8,allocatable::S145(:,:,:,:)
       real*8,allocatable::S146(:,:,:,:)
       real*8,allocatable::S147(:,:,:,:)
       real*8,allocatable::Q20(:,:)
       real*8,allocatable::Q21(:,:)
       real*8,allocatable::U212(:,:,:,:,:,:)
       real*8,allocatable::U333(:,:,:,:,:,:)
       real*8,allocatable::S149(:,:,:,:)
       real*8,allocatable::S150(:,:,:,:)
       real*8,allocatable::S151(:,:,:,:)
       real*8,allocatable::Q22(:,:)
       real*8,allocatable::Q23(:,:)
       real*8,allocatable::Q24(:,:)
       real*8,allocatable::S152(:,:,:,:)
       real*8,allocatable::S153(:,:,:,:)
       real*8,allocatable::S154(:,:,:,:)
       real*8,allocatable::S155(:,:,:,:)
       real*8,allocatable::S156(:,:,:,:)
       real*8,allocatable::S157(:,:,:,:)
       real*8,allocatable::S158(:,:,:,:)
       real*8,allocatable::S159(:,:,:,:)
       real*8,allocatable::S160(:,:,:,:)
       real*8,allocatable::S161(:,:,:,:)
       real*8,allocatable::S162(:,:,:,:)
       real*8,allocatable::S163(:,:,:,:)
       real*8,allocatable::S164(:,:,:,:)
       real*8,allocatable::S165(:,:,:,:)
       real*8,allocatable::S166(:,:,:,:)
       real*8,allocatable::S167(:,:,:,:)
       real*8,allocatable::S168(:,:,:,:)
       real*8,allocatable::S169(:,:,:,:)
       real*8,allocatable::S170(:,:,:,:)
       real*8,allocatable::S171(:,:,:,:)
       real*8,allocatable::S172(:,:,:,:)
       real*8,allocatable::S173(:,:,:,:)
       real*8,allocatable::S174(:,:,:,:)
       real*8,allocatable::S175(:,:,:,:)
       real*8,allocatable::S176(:,:,:,:)
       real*8,allocatable::S177(:,:,:,:)
       real*8,allocatable::S178(:,:,:,:)
       real*8,allocatable::S179(:,:,:,:)
       real*8,allocatable::S180(:,:,:,:)
       real*8,allocatable::S181(:,:,:,:)
       real*8,allocatable::S182(:,:,:,:)
       real*8,allocatable::S183(:,:,:,:)
       real*8,allocatable::S184(:,:,:,:)
       real*8,allocatable::S185(:,:,:,:)
       real*8,allocatable::S186(:,:,:,:)
       real*8,allocatable::S187(:,:,:,:)
       real*8,allocatable::S188(:,:,:,:)
       real*8,allocatable::S189(:,:,:,:)
       real*8,allocatable::S190(:,:,:,:)
       real*8,allocatable::S191(:,:,:,:)
       real*8,allocatable::S192(:,:,:,:)
       real*8,allocatable::S193(:,:,:,:)
       real*8,allocatable::S194(:,:,:,:)
       real*8,allocatable::S195(:,:,:,:)
       real*8,allocatable::S196(:,:,:,:)
       real*8,allocatable::S197(:,:,:,:)
       real*8,allocatable::S198(:,:,:,:)
       real*8,allocatable::S199(:,:,:,:)
       real*8,allocatable::S200(:,:,:,:)
       real*8,allocatable::S201(:,:,:,:)
       real*8,allocatable::S202(:,:,:,:)
       real*8,allocatable::S203(:,:,:,:)
       real*8,allocatable::S204(:,:,:,:)
       real*8,allocatable::S205(:,:,:,:)
       real*8,allocatable::Q25(:,:)
       real*8,allocatable::S206(:,:,:,:)
       real*8,allocatable::S207(:,:,:,:)
       real*8,allocatable::S208(:,:,:,:)
       real*8,allocatable::S209(:,:,:,:)
       real*8,allocatable::S210(:,:,:,:)
       real*8,allocatable::S211(:,:,:,:)
       real*8,allocatable::Q26(:,:)
       real*8,allocatable::Q27(:,:)
       real*8,allocatable::Q28(:,:)
       real*8,allocatable::Q29(:,:)
       real*8,allocatable::Q30(:,:)
       real*8,allocatable::S212(:,:,:,:)
       real*8,allocatable::S213(:,:,:,:)
       real*8,allocatable::S214(:,:,:,:)
       real*8,allocatable::S215(:,:,:,:)
       real*8,allocatable::S216(:,:,:,:)
       real*8,allocatable::S217(:,:,:,:)
       real*8,allocatable::S218(:,:,:,:)
       real*8,allocatable::S219(:,:,:,:)
       real*8,allocatable::S220(:,:,:,:)
       real*8,allocatable::S221(:,:,:,:)
       real*8,allocatable::S223(:,:,:,:)
       real*8,allocatable::S222(:,:,:,:)
       real*8,allocatable::S224(:,:,:,:)
       real*8,allocatable::S226(:,:,:,:)
       real*8,allocatable::S225(:,:,:,:)
       real*8,allocatable::S227(:,:,:,:)
       real*8,allocatable::S229(:,:,:,:)
       real*8,allocatable::S228(:,:,:,:)
       real*8,allocatable::S230(:,:,:,:)
       real*8,allocatable::S234(:,:,:,:)
       real*8,allocatable::S235(:,:,:,:)
       real*8,allocatable::S231(:,:,:,:)
       real*8,allocatable::S232(:,:,:,:)
       real*8,allocatable::S236(:,:,:,:)
       real*8,allocatable::S233(:,:,:,:)
       real*8,allocatable::S237(:,:,:,:)
       real*8,allocatable::S239(:,:,:,:)
       real*8,allocatable::S238(:,:,:,:)
       real*8,allocatable::S240(:,:,:,:)
       real*8,allocatable::S242(:,:,:,:)
       real*8,allocatable::S241(:,:,:,:)
       real*8,allocatable::S243(:,:,:,:)
       real*8,allocatable::S247(:,:,:,:)
       real*8,allocatable::S248(:,:,:,:)
       real*8,allocatable::S244(:,:,:,:)
       real*8,allocatable::S245(:,:,:,:)
       real*8,allocatable::S249(:,:,:,:)
       real*8,allocatable::S246(:,:,:,:)
       real*8,allocatable::S250(:,:,:,:)
       real*8,allocatable::S251(:,:,:,:)
       real*8,allocatable::S252(:,:,:,:)
       real*8,allocatable::S253(:,:,:,:)
       real*8,allocatable::S254(:,:,:,:)
       real*8,allocatable::S255(:,:,:,:)
       real*8,allocatable::S256(:,:,:,:)
       real*8,allocatable::S257(:,:,:,:)
       real*8,allocatable::S258(:,:,:,:)
       real*8,allocatable::S259(:,:,:,:)
       real*8,allocatable::Q31(:,:)
       real*8,allocatable::Q35(:,:)
       real*8,allocatable::Q32(:,:)
       real*8,allocatable::S262(:,:,:,:)
       real*8,allocatable::S263(:,:,:,:)
       real*8,allocatable::S264(:,:,:,:)
       real*8,allocatable::S265(:,:,:,:)
       real*8,allocatable::S266(:,:,:,:)
       real*8,allocatable::S267(:,:,:,:)
       real*8,allocatable::S268(:,:,:,:)
       real*8,allocatable::S269(:,:,:,:)
       real*8,allocatable::S270(:,:,:,:)
       real*8,allocatable::S271(:,:,:,:)
       real*8,allocatable::S272(:,:,:,:)
       real*8,allocatable::S273(:,:,:,:)
       real*8,allocatable::S274(:,:,:,:)
       real*8,allocatable::S275(:,:,:,:)
       real*8,allocatable::S276(:,:,:,:)
       real*8,allocatable::S277(:,:,:,:)
       real*8,allocatable::Q33(:,:)
       real*8,allocatable::Q34(:,:)
       real*8,allocatable::S260(:,:,:,:)
       real*8,allocatable::S261(:,:,:,:)
       real*8,allocatable::S148(:,:,:,:)
       real*8,allocatable::S278(:,:,:,:)
       real*8,allocatable::S282(:,:,:,:)
       real*8,allocatable::S279(:,:,:,:)
       real*8,allocatable::S280(:,:,:,:)
       real*8,allocatable::S283(:,:,:,:)
       real*8,allocatable::S281(:,:,:,:)
       real*8,allocatable::Q36(:,:)
       real*8,allocatable::Q37(:,:)
       real*8,allocatable::Q38(:,:)
       real*8,allocatable::Q40(:,:)
       real*8,allocatable::Q39(:,:)
       real*8,allocatable::S284(:,:,:,:)
       real*8,allocatable::S285(:,:,:,:)
       real*8,allocatable::S286(:,:,:,:)
       real*8,allocatable::S288(:,:,:,:)
       real*8,allocatable::S287(:,:,:,:)
       real*8,allocatable::S289(:,:,:,:)
       real*8,allocatable::S290(:,:,:,:)
       real*8,allocatable::S291(:,:,:,:)
       real*8,allocatable::S292(:,:,:,:)
       real*8,allocatable::S293(:,:,:,:)
       real*8,allocatable::S297(:,:,:,:)
       real*8,allocatable::S294(:,:,:,:)
       real*8,allocatable::S295(:,:,:,:)
       real*8,allocatable::S298(:,:,:,:)
       real*8,allocatable::S296(:,:,:,:)
       real*8,allocatable::Q41(:,:)
       real*8,allocatable::Q45(:,:)
       real*8,allocatable::Q42(:,:)
       real*8,allocatable::Q43(:,:)
       real*8,allocatable::Q44(:,:)
       real*8,allocatable::Q46(:,:)
       real*8,allocatable::Q47(:,:)
       real*8,allocatable::Q48(:,:)
       real*8,allocatable::Q50(:,:)
       real*8,allocatable::Q49(:,:)
       real*8,allocatable::S299(:,:,:,:)
       real*8,allocatable::S301(:,:,:,:)
       real*8,allocatable::S302(:,:,:,:)
       real*8,allocatable::S303(:,:,:,:)
       real*8,allocatable::S353(:,:,:,:)
       real*8,allocatable::S354(:,:,:,:)
       real*8,allocatable::S356(:,:,:,:)
       real*8,allocatable::S357(:,:,:,:)
       real*8,allocatable::S358(:,:,:,:)
       real*8,allocatable::S359(:,:,:,:)
       real*8,allocatable::S300(:,:,:,:)
       real*8,allocatable::S355(:,:,:,:)
       real*8,allocatable::S304(:,:,:,:)
       real*8,allocatable::S310(:,:,:,:)
       real*8,allocatable::S305(:,:,:,:)
       real*8,allocatable::S306(:,:,:,:)
       real*8,allocatable::S307(:,:,:,:)
       real*8,allocatable::S308(:,:,:,:)
       real*8,allocatable::S360(:,:,:,:)
       real*8,allocatable::S361(:,:,:,:)
       real*8,allocatable::S363(:,:,:,:)
       real*8,allocatable::S364(:,:,:,:)
       real*8,allocatable::S365(:,:,:,:)
       real*8,allocatable::S366(:,:,:,:)
       real*8,allocatable::S309(:,:,:,:)
       real*8,allocatable::S362(:,:,:,:)
       real*8,allocatable::S323(:,:,:,:)
       real*8,allocatable::S329(:,:,:,:)
       real*8,allocatable::S324(:,:,:,:)
       real*8,allocatable::S321(:,:,:,:)
       real*8,allocatable::S322(:,:,:,:)
       real*8,allocatable::S327(:,:,:,:)
       real*8,allocatable::S325(:,:,:,:)
       real*8,allocatable::S326(:,:,:,:)
       real*8,allocatable::Q51(:,:)
       real*8,allocatable::S318(:,:,:,:)
       real*8,allocatable::S320(:,:,:,:)
       real*8,allocatable::S328(:,:,:,:)
       real*8,allocatable::S330(:,:,:,:)
       real*8,allocatable::S334(:,:,:,:)
       real*8,allocatable::S342(:,:,:,:)
       real*8,allocatable::S343(:,:,:,:)
       real*8,allocatable::S335(:,:,:,:)
       real*8,allocatable::S336(:,:,:,:)
       real*8,allocatable::S337(:,:,:,:)
       real*8,allocatable::S338(:,:,:,:)
       real*8,allocatable::S339(:,:,:,:)
       real*8,allocatable::S341(:,:,:,:)
       real*8,allocatable::S340(:,:,:,:)
       real*8,allocatable::S346(:,:,:,:)
       real*8,allocatable::S348(:,:,:,:)
       real*8,allocatable::S347(:,:,:,:)
       real*8,allocatable::Q52(:,:)
       real*8,allocatable::S350(:,:,:,:)
       real*8,allocatable::S351(:,:,:,:)
       real*8,allocatable::S352(:,:,:,:)
       real*8,allocatable::S349(:,:,:,:)
       real*8,allocatable::S311(:,:,:,:)
       real*8,allocatable::S312(:,:,:,:)
       real*8,allocatable::S313(:,:,:,:)
       real*8,allocatable::S317(:,:,:,:)
       real*8,allocatable::S314(:,:,:,:)
       real*8,allocatable::S315(:,:,:,:)
       real*8,allocatable::S319(:,:,:,:)
       real*8,allocatable::S316(:,:,:,:)
       real*8,allocatable::S331(:,:,:,:)
       real*8,allocatable::S333(:,:,:,:)
       real*8,allocatable::S332(:,:,:,:)
       real*8,allocatable::S344(:,:,:,:)
       real*8,allocatable::S345(:,:,:,:)
       real*8,allocatable::X1(:,:,:,:)
       real*8,allocatable::Z1(:,:,:,:,:,:)
       real*8,allocatable::X2(:,:,:,:)
       real*8,allocatable::Z2(:,:,:,:,:,:)
       real*8,allocatable::X3(:,:,:,:)
       real*8,allocatable::Z3(:,:,:,:,:,:)
       real*8,allocatable::X4(:,:,:,:)
       real*8,allocatable::Z4(:,:,:,:,:,:)
       real*8,allocatable::X5(:,:,:,:)
       real*8,allocatable::Z5(:,:,:,:,:,:)
       real*8,allocatable::X6(:,:,:,:)
       real*8,allocatable::Z6(:,:,:,:,:,:)
       real*8,allocatable::X7(:,:,:,:)
       real*8,allocatable::Z7(:,:,:,:,:,:)
       real*8,allocatable::X8(:,:,:,:)
       real*8,allocatable::Z8(:,:,:,:,:,:)
       real*8,allocatable::X9(:,:)
       real*8,allocatable::Z9(:,:,:,:,:,:)
       real*8,allocatable::X10(:,:)
       real*8,allocatable::Z10(:,:,:,:,:,:)
       real*8,allocatable::X11(:,:)
       real*8,allocatable::Z11(:,:,:,:,:,:)
       real*8,allocatable::X12(:,:)
       real*8,allocatable::Z12(:,:,:,:,:,:)
       real*8,allocatable::X13(:,:)
       real*8,allocatable::Z13(:,:,:,:,:,:)
       real*8,allocatable::X14(:,:)
       real*8,allocatable::Z14(:,:,:,:,:,:)
       real*8,allocatable::X15(:,:,:,:)
       real*8,allocatable::Z15(:,:,:,:,:,:)
       real*8,allocatable::X16(:,:,:,:)
       real*8,allocatable::Z16(:,:,:,:,:,:)
       real*8,allocatable::X17(:,:,:,:)
       real*8,allocatable::Z17(:,:,:,:,:,:)
       real*8,allocatable::X18(:,:,:,:)
       real*8,allocatable::Z18(:,:,:,:,:,:)
       real*8,allocatable::X19(:,:,:,:)
       real*8,allocatable::Z19(:,:,:,:,:,:)
       real*8,allocatable::X20(:,:,:,:)
       real*8,allocatable::Z20(:,:,:,:,:,:)
       real*8,allocatable::X21(:,:,:,:)
       real*8,allocatable::Z21(:,:,:,:,:,:)
       real*8,allocatable::X22(:,:,:,:)
       real*8,allocatable::Z22(:,:,:,:,:,:)
       real*8,allocatable::X23(:,:,:,:)
       real*8,allocatable::Z23(:,:,:,:,:,:)
       real*8,allocatable::X24(:,:,:,:)
       real*8,allocatable::Z24(:,:,:,:,:,:)
       real*8,allocatable::X25(:,:,:,:)
       real*8,allocatable::Z25(:,:,:,:,:,:)
       real*8,allocatable::X26(:,:,:,:)
       real*8,allocatable::Z26(:,:,:,:,:,:)
       real*8,allocatable::X27(:,:,:,:)
       real*8,allocatable::Z27(:,:,:,:,:,:)
       real*8,allocatable::X28(:,:,:,:)
       real*8,allocatable::Z28(:,:,:,:,:,:)
       real*8,allocatable::X29(:,:,:,:)
       real*8,allocatable::Z29(:,:,:,:,:,:)
       real*8,allocatable::X30(:,:,:,:)
       real*8,allocatable::Z30(:,:,:,:,:,:)
       real*8,allocatable::X31(:,:,:,:)
       real*8,allocatable::Z31(:,:,:,:,:,:)
       real*8,allocatable::X32(:,:,:,:)
       real*8,allocatable::Z32(:,:,:,:,:,:)
       real*8,allocatable::X33(:,:,:,:)
       real*8,allocatable::Z33(:,:,:,:,:,:)
       real*8,allocatable::X34(:,:,:,:)
       real*8,allocatable::Z34(:,:,:,:,:,:)
       real*8,allocatable::Z35(:,:,:,:,:,:)
       real*8,allocatable::X35(:,:,:,:)
       real*8,allocatable::Z36(:,:,:,:,:,:)
       real*8,allocatable::X36(:,:,:,:)
       real*8,allocatable::Z37(:,:,:,:,:,:)
       real*8,allocatable::X37(:,:,:,:)
       real*8,allocatable::Z38(:,:,:,:,:,:)
       real*8,allocatable::X38(:,:,:,:)
       real*8,allocatable::Z39(:,:,:,:,:,:)
       real*8,allocatable::Z40(:,:,:,:,:,:)
       real*8,allocatable::Z41(:,:,:,:,:,:)
       real*8,allocatable::X39(:,:,:,:)
       real*8,allocatable::Z46(:,:,:,:,:,:)
       real*8,allocatable::X40(:,:,:,:)
       real*8,allocatable::Z54(:,:,:,:,:,:)
       real*8,allocatable::X41(:,:,:,:)
       real*8,allocatable::Z55(:,:,:,:,:,:)
       real*8,allocatable::X42(:,:,:,:)
       real*8,allocatable::Z56(:,:,:,:,:,:)
       real*8,allocatable::X43(:,:,:,:)
       real*8,allocatable::Z70(:,:,:,:,:,:)
       real*8,allocatable::Z77(:,:,:,:,:,:)
       real*8,allocatable::Z78(:,:,:,:,:,:)
       real*8,allocatable::Z79(:,:,:,:,:,:)
       real*8,allocatable::X44(:,:,:,:)
       real*8,allocatable::Z84(:,:,:,:,:,:)
       real*8,allocatable::X45(:,:,:,:)
       real*8,allocatable::Z85(:,:,:,:,:,:)
       real*8,allocatable::Z95(:,:,:,:,:,:)
       real*8,allocatable::Z96(:,:,:,:,:,:)
       real*8,allocatable::Z97(:,:,:,:,:,:)
       real*8,allocatable::Z105(:,:,:,:,:,:)
       real*8,allocatable::Z106(:,:,:,:,:,:)
       real*8,allocatable::Z107(:,:,:,:,:,:)
       real*8,allocatable::Z108(:,:,:,:,:,:)
       real*8,allocatable::Z109(:,:,:,:,:,:)
       real*8,allocatable::Z110(:,:,:,:,:,:)
       real*8,allocatable::Z111(:,:,:,:,:,:)
       real*8,allocatable::Z112(:,:,:,:,:,:)
       real*8,allocatable::Z113(:,:,:,:,:,:)
       real*8,allocatable::Z114(:,:,:,:,:,:)
       real*8,allocatable::Z115(:,:,:,:,:,:)
       real*8,allocatable::Z116(:,:,:,:,:,:)
       real*8,allocatable::Z117(:,:,:,:,:,:)
       real*8,allocatable::Z118(:,:,:,:,:,:)
       real*8,allocatable::Z119(:,:,:,:,:,:)
       real*8,allocatable::Z120(:,:,:,:,:,:)
       real*8,allocatable::Z121(:,:,:,:,:,:)
       real*8,allocatable::Z122(:,:,:,:,:,:)
       real*8,allocatable::Z180(:,:,:,:,:,:)
       real*8,allocatable::Z208(:,:,:,:,:,:)
       real*8,allocatable::Z209(:,:,:,:,:,:)
       real*8,allocatable::Z213(:,:,:,:,:,:)
       real*8,allocatable::Z334(:,:,:,:,:,:)
       real*8,allocatable::Z220(:,:,:,:,:,:)
       real*8,allocatable::X46(:,:,:,:)
       real*8,allocatable::Z237(:,:,:,:,:,:)
       real*8,allocatable::X47(:,:,:,:)
       real*8,allocatable::Z238(:,:,:,:,:,:)
       real*8,allocatable::X48(:,:,:,:)
       real*8,allocatable::Z239(:,:,:,:,:,:)
       real*8,allocatable::X49(:,:,:,:)
       real*8,allocatable::Z266(:,:,:,:,:,:)
       real*8,allocatable::X50(:,:,:,:)
       real*8,allocatable::Z267(:,:,:,:,:,:)
       real*8,allocatable::X51(:,:,:,:)
       real*8,allocatable::Z268(:,:,:,:,:,:)
       real*8,allocatable::X52(:,:,:,:)
       real*8,allocatable::Z269(:,:,:,:,:,:)
       real*8,allocatable::X53(:,:,:,:)
       real*8,allocatable::Z270(:,:,:,:,:,:)
       real*8,allocatable::X54(:,:,:,:)
       real*8,allocatable::Z271(:,:,:,:,:,:)
C
       allocate(V3A(M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       V3A=0.0d0
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder3412(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,M1,M1,N1,VAHHHH,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S1(M2+1:N3,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S1)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X1(N0+1:N1,M2+1:N3,N0+1:M1,M1+1:N1))
       X1=0.0d0
       call
     & sum2134(N0,N1,M2,N3,N0,M1,M1,N1,X1,S1,-1.000)
       deallocate(S1)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder4312(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,M1,M1,N1,VAHHHH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S2(N1+1:M2,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S2)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X2(N0+1:N1,N1+1:M2,N0+1:M1,M1+1:N1))
       X2=0.0d0
       call
     & sum2134(N0,N1,N1,M2,N0,M1,M1,N1,X2,S2, 1.000)
       deallocate(S2)
C
       allocate(D1(N1+1:N3,N0+1:N1,M2+1:N3,M1+1:N1))
       call reorder1432(N1,N3,N0,N1,N1,N3,N0,N1,
     & N1,N3,N0,N1,M2,N3,M1,N1,VAHPHP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S3(N0+1:M1,N0+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S3)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N1,M2,N3,N0,M1,M1,N1,X1,S3, 1.000)
       deallocate(S3)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:M2,M1+1:N1))
       call reorder1432(N1,N3,N0,N1,N1,N3,N0,N1,
     & N1,N3,N0,N1,N1,M2,M1,N1,VAHPHP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S4(N0+1:M1,N0+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S4)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N1,N1,M2,N0,M1,M1,N1,X2,S4, 1.000)
       deallocate(S4)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder4132(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N1,VAHPHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S5(M2+1:N3,N1+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S5)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X39(N1+1:N3,M2+1:N3,M2+1:N3,M1+1:N1))
       X39=0.0d0
       call
     & sum3124(N1,N3,M2,N3,M2,N3,M1,N1,X39,S5, 1.000)
       deallocate(S5)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder4132(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N1,VAHPHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S6(N1+1:M2,N1+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S6)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X4(N1+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       X4=0.0d0
       call
     & sum3124(N1,N3,M2,N3,N1,M2,M1,N1,X4,S6,-1.000)
       deallocate(S6)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder4132(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,VAHPHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S7(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S7)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,N1,M2,M1,N1,X4,S7, 1.000)
       deallocate(S7)
C
       allocate(D1(N1+1:N3,N0+1:N1,M2+1:N3,N0+1:M1))
       call reorder1432(N1,N3,N0,N1,N1,N3,N0,N1,
     & N1,N3,N0,N1,M2,N3,N0,M1,VAHPHP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S8(M1+1:N1,N0+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S8)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,N1,M2,N3,N0,M1,M1,N1,X1,S8,-1.000)
       deallocate(S8)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:M2,N0+1:M1))
       call reorder1432(N1,N3,N0,N1,N1,N3,N0,N1,
     & N1,N3,N0,N1,N1,M2,N0,M1,VAHPHP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S9(M1+1:N1,N0+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S9)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,N1,N1,M2,N0,M1,M1,N1,X2,S9,-1.000)
       deallocate(S9)
C
       allocate(D1(N1+1:N3,N1+1:N3,M2+1:N3,N1+1:M2))
       call reorder1234(N1,N3,N1,N3,N1,N3,N1,M2,
     & N1,N3,N1,N3,M2,N3,N1,M2,VAAPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S10(M1+1:N1,N1+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6*K3
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S10)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N1,N3,M2,N3,N1,M2,M1,N1,X4,S10,-1.000)
       deallocate(S10)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,N0+1:M1))
       call reorder3412(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,M1,N0,M1,VAHHHH,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S11(M2+1:N3,N0+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S11)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X5(N0+1:N1,M2+1:N3,N0+1:M1,N0+1:M1))
       X5=0.0d0
       call
     & sum2134(N0,N1,M2,N3,N0,M1,N0,M1,X5,S11,-1.000)
       deallocate(S11)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,N0+1:M1))
       call reorder4312(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,M1,N0,M1,VAHHHH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S12(N1+1:M2,N0+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S12)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X6(N0+1:N1,N1+1:M2,N0+1:M1,N0+1:M1))
       X6=0.0d0
       call
     & sum2134(N0,N1,N1,M2,N0,M1,N0,M1,X6,S12, 1.000)
       deallocate(S12)
C
       allocate(D1(N1+1:N3,N0+1:N1,M2+1:N3,N0+1:M1))
       call reorder1432(N1,N3,N0,N1,N1,N3,N0,N1,
     & N1,N3,N0,N1,M2,N3,N0,M1,VAHPHP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S13(N0+1:M1,N0+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S13)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X40(N0+1:N1,M2+1:N3,N0+1:M1,N0+1:M1))
       X40=0.0d0
       call
     & sum3124(N0,N1,M2,N3,N0,M1,N0,M1,X40,S13, 1.000)
       deallocate(S13)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:M2,N0+1:M1))
       call reorder1432(N1,N3,N0,N1,N1,N3,N0,N1,
     & N1,N3,N0,N1,N1,M2,N0,M1,VAHPHP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S14(N0+1:M1,N0+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S14)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X41(N0+1:N1,N1+1:M2,N0+1:M1,N0+1:M1))
       X41=0.0d0
       call
     & sum3124(N0,N1,N1,M2,N0,M1,N0,M1,X41,S14, 1.000)
       deallocate(S14)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder4132(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,VAHPHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S15(M2+1:N3,N1+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S15)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X42(N1+1:N3,M2+1:N3,M2+1:N3,N0+1:M1))
       X42=0.0d0
       call
     & sum3124(N1,N3,M2,N3,M2,N3,N0,M1,X42,S15, 1.000)
       deallocate(S15)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder4132(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,VAHPHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S16(N1+1:M2,N1+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S16)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X8(N1+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       X8=0.0d0
       call
     & sum3124(N1,N3,M2,N3,N1,M2,N0,M1,X8,S16,-1.000)
       deallocate(S16)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder4132(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,VAHPHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S17(M2+1:N3,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S17)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,N1,M2,N0,M1,X8,S17, 1.000)
       deallocate(S17)
C
       allocate(D1(N1+1:N3,N1+1:N3,M2+1:N3,N1+1:M2))
       call reorder1234(N1,N3,N1,N3,N1,N3,N1,M2,
     & N1,N3,N1,N3,M2,N3,N1,M2,VAAPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S18(N0+1:M1,N1+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6*K3
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S18)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N1,N3,M2,N3,N1,M2,N0,M1,X8,S18,-1.000)
       deallocate(S18)
C
       allocate(B1(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,FAHP,B1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(Q1(M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q1)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X9(M1+1:N1,M1+1:N1))
       X9=0.0d0
       call
     & sum21(M1,N1,M1,N1,X9,Q1, 1.000)
       deallocate(Q1)
C
       allocate(B1(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,FAHP,B1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(Q2(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q2)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X10(N0+1:M1,N0+1:M1))
       X10=0.0d0
       call
     & sum21(N0,M1,N0,M1,X10,Q2, 1.000)
       deallocate(Q2)
C
       allocate(B1(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,FAHP,B1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(Q3(N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q3)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X11(M1+1:N1,N0+1:M1))
       X11=0.0d0
       call
     & sum21(M1,N1,N0,M1,X11,Q3, 1.000)
       deallocate(Q3)
C
       allocate(B1(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,FAHP,B1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(Q4(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,B1,B2,Q4)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X12(M2+1:N3,M2+1:N3))
       X12=0.0d0
       call
     & sum21(M2,N3,M2,N3,X12,Q4,-1.000)
       deallocate(Q4)
C
       allocate(B1(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,FAHP,B1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(Q5(M2+1:N3,N1+1:M2))
       I1=K9
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,B1,B2,Q5)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X13(N1+1:M2,M2+1:N3))
       X13=0.0d0
       call
     & sum21(N1,M2,M2,N3,X13,Q5,-1.000)
       deallocate(Q5)
C
       allocate(B1(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,FAHP,B1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(Q6(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,B1,B2,Q6)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X14(N1+1:M2,N1+1:M2))
       X14=0.0d0
       call
     & sum21(N1,M2,N1,M2,X14,Q6,-1.000)
       deallocate(Q6)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,M1+1:N1))
       call reorder1342(N1,N3,N0,N1,N0,N1,N0,N1,
     & N1,N3,N0,M1,M1,N1,M1,N1,VAHHHP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S19(N0+1:M1,N0+1:M1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S19)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X15(N0+1:M1,M1+1:N1,N0+1:M1,M1+1:N1))
       X15=0.0d0
       call
     & sum3124(N0,M1,M1,N1,N0,M1,M1,N1,X15,S19, 1.000)
       deallocate(S19)
C
       allocate(D1(N1+1:N3,M1+1:N1,M1+1:N1,M1+1:N1))
       call reorder1342(N1,N3,N0,N1,N0,N1,N0,N1,
     & N1,N3,M1,N1,M1,N1,M1,N1,VAHHHP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S20(N0+1:M1,M1+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S20)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X16(M1+1:N1,M1+1:N1,N0+1:M1,M1+1:N1))
       X16=0.0d0
       call
     & sum3124(M1,N1,M1,N1,N0,M1,M1,N1,X16,S20, 1.000)
       deallocate(S20)
C
       allocate(D1(N0+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,M1,N1,M2,N3,M1,N1,VAHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S21(M2+1:N3,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K7
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S21)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X17(M1+1:N1,M2+1:N3,M2+1:N3,M1+1:N1))
       X17=0.0d0
       call
     & sum3124(M1,N1,M2,N3,M2,N3,M1,N1,X17,S21,-1.000)
       deallocate(S21)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       call reorder3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,M1,N1,N1,M2,M1,N1,VAHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S22(M2+1:N3,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K7
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S22)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X18(M1+1:N1,N1+1:M2,M2+1:N3,M1+1:N1))
       X18=0.0d0
       call
     & sum3124(M1,N1,N1,M2,M2,N3,M1,N1,X18,S22,-1.000)
       deallocate(S22)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       call reorder4312(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,M1,N1,N1,M2,M1,N1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S23(N1+1:M2,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S23)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X43(M1+1:N1,N1+1:M2,N1+1:M2,M1+1:N1))
       X43=0.0d0
       call
     & sum3124(M1,N1,N1,M2,N1,M2,M1,N1,X43,S23, 1.000)
       deallocate(S23)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M1+1:N1))
       call reorder3142(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,M1,N1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q7(M1+1:N1,M1+1:N1))
       I1=K7*K7
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q7)
       deallocate(D1)
       deallocate(B2)
C
       X9=X9+Q7
       deallocate(Q7)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,N0+1:M1))
       call reorder1342(N1,N3,N0,N1,N0,N1,N0,N1,
     & N1,N3,N0,M1,M1,N1,N0,M1,VAHHHP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S24(M1+1:N1,N0+1:M1,M1+1:N1,N0+1:M1))
       I1=K5*K7*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S24)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,M1,N1,N0,M1,M1,N1,X15,S24,-1.000)
       deallocate(S24)
C
       allocate(D1(N1+1:N3,M1+1:N1,M1+1:N1,N0+1:M1))
       call reorder1342(N1,N3,N0,N1,N0,N1,N0,N1,
     & N1,N3,M1,N1,M1,N1,N0,M1,VAHHHP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S25(M1+1:N1,M1+1:N1,M1+1:N1,N0+1:M1))
       I1=K5*K7*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S25)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,M1,N1,N0,M1,M1,N1,X16,S25,-1.000)
       deallocate(S25)
C
       allocate(D1(N1+1:N3,M1+1:N1,M2+1:N3,M2+1:N3))
       call reorder1423(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,M1,N1,M2,N3,M2,N3,VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S26(M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S26)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,M2,N3,M2,N3,M1,N1,X17,S26,-1.000)
       deallocate(S26)
C
       allocate(D1(N1+1:N3,M1+1:N1,N1+1:M2,M2+1:N3))
       call reorder1423(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,M1,N1,N1,M2,M2,N3,VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S27(M1+1:N1,M1+1:N1,N1+1:M2,M2+1:N3))
       I1=K6*K9*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S27)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,N1,M2,M2,N3,M1,N1,X18,S27,-1.000)
       deallocate(S27)
C
       allocate(D1(N1+1:N3,M1+1:N1,N1+1:M2,N1+1:M2))
       call reorder1423(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,M1,N1,N1,M2,N1,M2,VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S28(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S28)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X19(M1+1:N1,N1+1:M2,N1+1:M2,M1+1:N1))
       X19=0.0d0
       call
     & sum4123(M1,N1,N1,M2,N1,M2,M1,N1,X19,S28,-1.000)
       deallocate(S28)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:M1,N0+1:M1))
       call reorder1342(N1,N3,N0,N1,N0,N1,N0,N1,
     & N1,N3,N0,M1,N0,M1,N0,M1,VAHHHP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S29(N0+1:M1,N0+1:M1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S29)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N0+1:M1,N0+1:M1,N0+1:M1))
       call reorder2314(N0,M1,N0,M1,N0,M1,N0,M1,
     & N0,M1,N0,M1,N0,M1,N0,M1,S29,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder451236(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N0,M1,M2,N3,M2,N3,N1,M2,M1,N1,t3A,F2)
       allocate(Z77(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K7*K9*K6*K6
       I3=K5*K5
       call EGEMM(I1,I2,I3,D1,F2,Z77)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum123645(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z77, 0.500)
       call
     & sum123654(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z77,-0.500)
       deallocate(Z77)
       deallocate(S29)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,N0+1:M1))
       call reorder1342(N1,N3,N0,N1,N0,N1,N0,N1,
     & N1,N3,N0,M1,M1,N1,N0,M1,VAHHHP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S30(N0+1:M1,N0+1:M1,M1+1:N1,N0+1:M1))
       I1=K5*K7*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S30)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M1+1:N1,N0+1:M1,N0+1:M1))
       call reorder2314(N0,M1,N0,M1,M1,N1,N0,M1,
     & N0,M1,M1,N1,N0,M1,N0,M1,S30,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder451236(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,M2,N3,N1,M2,M1,N1,t3A,F2)
       allocate(Z78(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K7*K9*K6*K6
       I3=K7*K5
       call EGEMM(I1,I2,I3,D1,F2,Z78)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum123645(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z78, 1.000)
       call
     & sum123654(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z78,-1.000)
       deallocate(Z78)
       deallocate(S30)
C
       allocate(D1(N1+1:N3,M1+1:N1,M1+1:N1,N0+1:M1))
       call reorder1342(N1,N3,N0,N1,N0,N1,N0,N1,
     & N1,N3,M1,N1,M1,N1,N0,M1,VAHHHP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S31(N0+1:M1,M1+1:N1,M1+1:N1,N0+1:M1))
       I1=K5*K7*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S31)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N1,M1+1:N1,N0+1:M1,N0+1:M1))
       call reorder2314(N0,M1,M1,N1,M1,N1,N0,M1,
     & M1,N1,M1,N1,N0,M1,N0,M1,S31,D1)
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder451236(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M1,N1,M2,N3,M2,N3,N1,M2,M1,N1,t3A,F2)
       allocate(Z79(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K7*K9*K6*K6
       I3=K7*K7
       call EGEMM(I1,I2,I3,D1,F2,Z79)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum123645(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z79, 0.500)
       call
     & sum123654(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z79,-0.500)
       deallocate(Z79)
       deallocate(S31)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,M1,M2,N3,N0,M1,VAHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S32(M2+1:N3,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S32)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X23(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:M1))
       X23=0.0d0
       call
     & sum3124(N0,M1,M2,N3,M2,N3,N0,M1,X23,S32,-1.000)
       deallocate(S32)
C
       allocate(D1(N0+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,M1,N1,M2,N3,N0,M1,VAHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S33(M2+1:N3,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S33)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X24(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:M1))
       X24=0.0d0
       call
     & sum3124(M1,N1,M2,N3,M2,N3,N0,M1,X24,S33,-1.000)
       deallocate(S33)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,M1,N1,M2,N0,M1,VAHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S34(M2+1:N3,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S34)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X25(N0+1:M1,N1+1:M2,M2+1:N3,N0+1:M1))
       X25=0.0d0
       call
     & sum3124(N0,M1,N1,M2,M2,N3,N0,M1,X25,S34,-1.000)
       deallocate(S34)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,M1,N1,N1,M2,N0,M1,VAHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S35(M2+1:N3,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S35)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X26(M1+1:N1,N1+1:M2,M2+1:N3,N0+1:M1))
       X26=0.0d0
       call
     & sum3124(M1,N1,N1,M2,M2,N3,N0,M1,X26,S35,-1.000)
       deallocate(S35)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder4312(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,M1,N1,M2,N0,M1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S36(N1+1:M2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S36)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X44(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:M1))
       X44=0.0d0
       call
     & sum3124(N0,M1,N1,M2,N1,M2,N0,M1,X44,S36, 1.000)
       deallocate(S36)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder4312(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,M1,N1,N1,M2,N0,M1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S37(N1+1:M2,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S37)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X45(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:M1))
       X45=0.0d0
       call
     & sum3124(M1,N1,N1,M2,N1,M2,N0,M1,X45,S37, 1.000)
       deallocate(S37)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder3142(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,N0,M1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q8(N0+1:M1,N0+1:M1))
       I1=K5*K5
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q8)
       deallocate(D1)
       deallocate(B2)
C
       X10=X10+Q8
       deallocate(Q8)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N0+1:M1))
       call reorder3142(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,N0,M1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q9(M1+1:N1,N0+1:M1))
       I1=K5*K7
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q9)
       deallocate(D1)
       deallocate(B2)
C
       X11=X11+Q9
       deallocate(Q9)
C
       allocate(D1(N1+1:N3,N0+1:M1,M2+1:N3,M2+1:N3))
       call reorder1423(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N0,M1,M2,N3,M2,N3,VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S38(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S38)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,M2,N3,M2,N3,N0,M1,X23,S38,-1.000)
       deallocate(S38)
C
       allocate(D1(N1+1:N3,M1+1:N1,M2+1:N3,M2+1:N3))
       call reorder1423(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,M1,N1,M2,N3,M2,N3,VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S39(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S39)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,M2,N3,M2,N3,N0,M1,X24,S39,-1.000)
       deallocate(S39)
C
       allocate(D1(N1+1:N3,N0+1:M1,N1+1:M2,M2+1:N3))
       call reorder1423(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N0,M1,N1,M2,M2,N3,VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S40(N0+1:M1,N0+1:M1,N1+1:M2,M2+1:N3))
       I1=K6*K9*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S40)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,N1,M2,M2,N3,N0,M1,X25,S40,-1.000)
       deallocate(S40)
C
       allocate(D1(N1+1:N3,M1+1:N1,N1+1:M2,M2+1:N3))
       call reorder1423(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,M1,N1,N1,M2,M2,N3,VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S41(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3))
       I1=K6*K9*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S41)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,N1,M2,M2,N3,N0,M1,X26,S41,-1.000)
       deallocate(S41)
C
       allocate(D1(N1+1:N3,N0+1:M1,N1+1:M2,N1+1:M2))
       call reorder1423(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N0,M1,N1,M2,N1,M2,VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S42(N0+1:M1,N0+1:M1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S42)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X27(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:M1))
       X27=0.0d0
       call
     & sum4123(N0,M1,N1,M2,N1,M2,N0,M1,X27,S42,-1.000)
       deallocate(S42)
C
       allocate(D1(N1+1:N3,M1+1:N1,N1+1:M2,N1+1:M2))
       call reorder1423(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,M1,N1,N1,M2,N1,M2,VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S43(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S43)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X28(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:M1))
       X28=0.0d0
       call
     & sum4123(M1,N1,N1,M2,N1,M2,N0,M1,X28,S43,-1.000)
       deallocate(S43)
C
       allocate(D1(M2+1:N3,M2+1:N3,N0+1:N1,M2+1:N3))
       call reorder1243(N1,N3,N1,N3,N1,N3,N0,N1,
     & M2,N3,M2,N3,N0,N1,M2,N3,VAHPPP,D1)
       allocate(F2(M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder123456(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,t3A,F2)
       allocate(U94(N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,N0+1:N1,M2+1:N3))
       I1=K6*K1
       I2=K7*K5*K5*K9
       I3=K6*K6
       call EGEMM(I1,I2,I3,D1,F2,U94)
       deallocate(D1)
       deallocate(F2)
C
       allocate(F1(N0+1:N1,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder561234(N1,M2,N0,M1,N0,M1,M1,N1,N0,N1,M2,N3,
     & N0,N1,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,U94,F1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(Z95(M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K5*K9*K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,F1,B2,Z95)
       deallocate(F1)
       deallocate(B2)
C
       call
     & sum213456(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z95,-0.500)
       V3A=V3A+0.500*Z95
       deallocate(Z95)
       deallocate(U94)
C
       allocate(D1(N0+1:N1,M2+1:N3,N1+1:M2,M2+1:N3))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,M2,N3,N1,M2,M2,N3,VAHPPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S44(M2+1:N3,M2+1:N3,N1+1:M2,M2+1:N3))
       I1=K6*K9*K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S44)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M2+1:N3,N1+1:M2,M2+1:N3,M2+1:N3))
       call reorder2341(M2,N3,M2,N3,N1,M2,M2,N3,
     & M2,N3,N1,M2,M2,N3,M2,N3,S44,D1)
       allocate(F2(M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder123456(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M2,N3,N1,M2,N1,M2,N0,M1,N0,M1,M1,N1,t3A,F2)
       allocate(Z96(N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3))
       I1=K6*K6
       I2=K7*K5*K5*K9
       I3=K9*K6
       call EGEMM(I1,I2,I3,D1,F2,Z96)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum345612(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z96,-1.000)
       call
     & sum345621(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z96, 1.000)
       deallocate(Z96)
       deallocate(S44)
C
       allocate(D1(N0+1:N1,N1+1:M2,N1+1:M2,M2+1:N3))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,M2,N1,M2,M2,N3,VAHPPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S45(M2+1:N3,N1+1:M2,N1+1:M2,M2+1:N3))
       I1=K6*K9*K9
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S45)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N1+1:M2,N1+1:M2,M2+1:N3,M2+1:N3))
       call reorder2341(M2,N3,N1,M2,N1,M2,M2,N3,
     & N1,M2,N1,M2,M2,N3,M2,N3,S45,D1)
       allocate(F2(N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder123456(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N1,M2,N1,M2,N1,M2,N0,M1,N0,M1,M1,N1,t3A,F2)
       allocate(Z97(N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3))
       I1=K6*K6
       I2=K7*K5*K5*K9
       I3=K9*K9
       call EGEMM(I1,I2,I3,D1,F2,Z97)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum345612(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z97,-0.500)
       call
     & sum345621(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z97, 0.500)
       deallocate(Z97)
       deallocate(S45)
C
       allocate(D1(N0+1:N1,M2+1:N3,N1+1:M2,M2+1:N3))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,M2,N3,N1,M2,M2,N3,VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S46(N1+1:M2,M2+1:N3,N1+1:M2,M2+1:N3))
       I1=K6*K9*K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S46)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X31(M2+1:N3,N1+1:M2,M2+1:N3,N1+1:M2))
       X31=0.0d0
       call
     & sum4123(M2,N3,N1,M2,M2,N3,N1,M2,X31,S46,-1.000)
       deallocate(S46)
C
       allocate(D1(N0+1:N1,N1+1:M2,N1+1:M2,M2+1:N3))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,M2,N1,M2,M2,N3,VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S47(N1+1:M2,N1+1:M2,N1+1:M2,M2+1:N3))
       I1=K6*K9*K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S47)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X32(N1+1:M2,N1+1:M2,M2+1:N3,N1+1:M2))
       X32=0.0d0
       call
     & sum4123(N1,M2,N1,M2,M2,N3,N1,M2,X32,S47,-1.000)
       deallocate(S47)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M2+1:N3))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,M2,N3,M2,N3,VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q10(M2+1:N3,M2+1:N3))
       I1=K6*K6
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q10)
       deallocate(D1)
       deallocate(B2)
C
       X12=X12-Q10
       deallocate(Q10)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M2+1:N3))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,M2,M2,N3,VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q11(N1+1:M2,M2+1:N3))
       I1=K6*K9
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q11)
       deallocate(D1)
       deallocate(B2)
C
       X13=X13-Q11
       deallocate(Q11)
C
       allocate(D1(N0+1:N1,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,M2,N3,N1,M2,N1,M2,VAHPPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S48(M2+1:N3,M2+1:N3,N1+1:M2,N1+1:M2))
       I1=K9*K9*K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S48)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,N1,M2,M2,N3,N1,M2,X31,S48, 1.000)
       deallocate(S48)
C
       allocate(D1(N0+1:N1,N1+1:M2,N1+1:M2,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,M2,N1,M2,N1,M2,VAHPPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S49(M2+1:N3,N1+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9*K9
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S49)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,M2,N1,M2,M2,N3,N1,M2,X32,S49, 1.000)
       deallocate(S49)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,M2,N1,M2,VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q12(N1+1:M2,N1+1:M2))
       I1=K9*K9
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q12)
       deallocate(D1)
       deallocate(B2)
C
       X14=X14-Q12
       deallocate(Q12)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,M1+1:N1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,M1,N2,M2,N3,M1,N1,VBHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S50(M2+1:N3,M1+1:N2,M2+1:N3,M1+1:N1))
       I1=K7*K6*K8
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S50)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder2314(M2,N3,M1,N2,M2,N3,M1,N1,
     & M1,N2,M2,N3,M2,N3,M1,N1,S50,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder412356(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,t3B4,F2)
       allocate(Z105(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K5*K5*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z105)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z105,-1.000)
       call
     & sum134526(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z105, 1.000)
       deallocate(Z105)
       deallocate(S50)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,M1+1:N1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,M1,N2,N2,M2,M1,N1,VBHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S51(M2+1:N3,M1+1:N2,N2+1:M2,M1+1:N1))
       I1=K7*K0*K8
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S51)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,M1+1:N1))
       call reorder2314(M2,N3,M1,N2,N2,M2,M1,N1,
     & M1,N2,N2,M2,M2,N3,M1,N1,S51,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder412356(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,N2,M2,M2,N3,N1,M2,N0,M1,N0,M1,t3B4,F2)
       allocate(Z106(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K5*K5*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z106)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z106,-1.000)
       call
     & sum134526(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z106, 1.000)
       deallocate(Z106)
       deallocate(S51)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,M1+1:N1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,M1,N2,N2,M2,M1,N1,VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S52(N1+1:M2,M1+1:N2,N2+1:M2,M1+1:N1))
       I1=K7*K0*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S52)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder2314(N1,M2,M1,N2,N2,M2,M1,N1,
     & M1,N2,N2,M2,N1,M2,M1,N1,S52,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder412356(N2,M2,M2,N3,M2,N3,M1,N2,N0,M1,N0,M1,
     & M1,N2,N2,M2,M2,N3,M2,N3,N0,M1,N0,M1,t3B2,F2)
       allocate(Z107(M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K6*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z107)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z107,-1.000)
       deallocate(Z107)
       deallocate(S52)
C
       allocate(D1(N1+1:N3,M1+1:N2,M2+1:N3,M2+1:N3))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,M1,N2,M2,N3,M2,N3,VBPHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S53(M1+1:N1,M1+1:N2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S53)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder2341(M1,N1,M1,N2,M2,N3,M2,N3,
     & M1,N2,M2,N3,M2,N3,M1,N1,S53,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder412356(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,t3B4,F2)
       allocate(Z108(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K5*K5*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z108)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z108, 1.000)
       call
     & sum134526(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z108,-1.000)
       deallocate(Z108)
       deallocate(S53)
C
       allocate(D1(N1+1:N3,M1+1:N2,N2+1:M2,M2+1:N3))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,M1,N2,N2,M2,M2,N3,VBPHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S54(M1+1:N1,M1+1:N2,N2+1:M2,M2+1:N3))
       I1=K6*K0*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S54)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,M1+1:N1))
       call reorder2341(M1,N1,M1,N2,N2,M2,M2,N3,
     & M1,N2,N2,M2,M2,N3,M1,N1,S54,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder412356(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,N2,M2,M2,N3,N1,M2,N0,M1,N0,M1,t3B4,F2)
       allocate(Z109(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K5*K5*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z109)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z109, 1.000)
       call
     & sum134526(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z109,-1.000)
       deallocate(Z109)
       deallocate(S54)
C
       allocate(D1(N1+1:N3,M1+1:N2,N2+1:M2,N1+1:M2))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,M1,N2,N2,M2,N1,M2,VBPHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S55(M1+1:N1,M1+1:N2,N2+1:M2,N1+1:M2))
       I1=K9*K0*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S55)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder2341(M1,N1,M1,N2,N2,M2,N1,M2,
     & M1,N2,N2,M2,N1,M2,M1,N1,S55,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder412356(N2,M2,M2,N3,M2,N3,M1,N2,N0,M1,N0,M1,
     & M1,N2,N2,M2,M2,N3,M2,N3,N0,M1,N0,M1,t3B2,F2)
       allocate(Z110(M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K6*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z110)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z110, 1.000)
       deallocate(Z110)
       deallocate(S55)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N0,M1,M2,N3,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S56(M2+1:N3,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S56)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder2314(M2,N3,N0,M1,M2,N3,N0,M1,
     & N0,M1,M2,N3,M2,N3,N0,M1,S56,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z111(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K5*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z111)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z111, 1.000)
       call
     & sum134625(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z111,-1.000)
       call
     & sum235614(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z111,-1.000)
       call
     & sum135624(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z111, 1.000)
       deallocate(Z111)
       deallocate(S56)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,M1,N2,M2,N3,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S57(M2+1:N3,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S57)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder2314(M2,N3,M1,N2,M2,N3,N0,M1,
     & M1,N2,M2,N3,M2,N3,N0,M1,S57,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z112(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K5*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z112)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z112, 1.000)
       call
     & sum134625(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z112,-1.000)
       call
     & sum235614(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z112,-1.000)
       call
     & sum135624(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z112, 1.000)
       deallocate(Z112)
       deallocate(S57)
C
       allocate(D1(N0+1:N1,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N0,M1,N2,M2,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S58(M2+1:N3,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S58)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder2314(M2,N3,N0,M1,N2,M2,N0,M1,
     & N0,M1,N2,M2,M2,N3,N0,M1,S58,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z113(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K5*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z113)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z113, 1.000)
       call
     & sum134625(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z113,-1.000)
       call
     & sum235614(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z113,-1.000)
       call
     & sum135624(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z113, 1.000)
       deallocate(Z113)
       deallocate(S58)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,M1,N2,N2,M2,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S59(M2+1:N3,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S59)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder2314(M2,N3,M1,N2,N2,M2,N0,M1,
     & M1,N2,N2,M2,M2,N3,N0,M1,S59,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z114(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K5*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z114)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z114, 1.000)
       call
     & sum134625(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z114,-1.000)
       call
     & sum235614(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z114,-1.000)
       call
     & sum135624(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z114, 1.000)
       deallocate(Z114)
       deallocate(S59)
C
       allocate(D1(N0+1:N1,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N0,M1,N2,M2,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S60(N1+1:M2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S60)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder2314(N1,M2,N0,M1,N2,M2,N0,M1,
     & N0,M1,N2,M2,N1,M2,N0,M1,S60,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder412356(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,M2,N3,N0,M1,M1,N1,t3B3,F2)
       allocate(Z115(M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K6*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z115)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124635(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z115, 1.000)
       call
     & sum125634(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z115,-1.000)
       deallocate(Z115)
       deallocate(S60)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,M1,N2,N2,M2,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S61(N1+1:M2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S61)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder2314(N1,M2,M1,N2,N2,M2,N0,M1,
     & M1,N2,N2,M2,N1,M2,N0,M1,S61,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder412356(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,M2,N3,N0,M1,M1,N1,t3B3,F2)
       allocate(Z116(M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K6*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z116)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124635(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z116, 1.000)
       call
     & sum125634(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z116,-1.000)
       deallocate(Z116)
       deallocate(S61)
C
       allocate(D1(N1+1:N3,N0+1:M1,M2+1:N3,M2+1:N3))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,N0,M1,M2,N3,M2,N3,VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S62(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S62)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,M1,M2,N3,M2,N3,
     & N0,M1,M2,N3,M2,N3,N0,M1,S62,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z117(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K5*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z117)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z117,-1.000)
       call
     & sum134625(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z117, 1.000)
       call
     & sum235614(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z117, 1.000)
       call
     & sum135624(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z117,-1.000)
       deallocate(Z117)
       deallocate(S62)
C
       allocate(D1(N1+1:N3,M1+1:N2,M2+1:N3,M2+1:N3))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,M1,N2,M2,N3,M2,N3,VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S63(N0+1:M1,M1+1:N2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K8
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S63)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,M1,N2,M2,N3,M2,N3,
     & M1,N2,M2,N3,M2,N3,N0,M1,S63,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z118(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K5*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z118)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z118,-1.000)
       call
     & sum134625(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z118, 1.000)
       call
     & sum235614(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z118, 1.000)
       call
     & sum135624(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z118,-1.000)
       deallocate(Z118)
       deallocate(S63)
C
       allocate(D1(N1+1:N3,N0+1:M1,N2+1:M2,M2+1:N3))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,N0,M1,N2,M2,M2,N3,VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S64(N0+1:M1,N0+1:M1,N2+1:M2,M2+1:N3))
       I1=K6*K0*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S64)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,M1,N2,M2,M2,N3,
     & N0,M1,N2,M2,M2,N3,N0,M1,S64,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z119(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K5*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z119)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z119,-1.000)
       call
     & sum134625(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z119, 1.000)
       call
     & sum235614(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z119, 1.000)
       call
     & sum135624(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z119,-1.000)
       deallocate(Z119)
       deallocate(S64)
C
       allocate(D1(N1+1:N3,M1+1:N2,N2+1:M2,M2+1:N3))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,M1,N2,N2,M2,M2,N3,VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S65(N0+1:M1,M1+1:N2,N2+1:M2,M2+1:N3))
       I1=K6*K0*K8
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S65)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,M1,N2,N2,M2,M2,N3,
     & M1,N2,N2,M2,M2,N3,N0,M1,S65,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z120(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K5*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z120)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z120,-1.000)
       call
     & sum134625(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z120, 1.000)
       call
     & sum235614(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z120, 1.000)
       call
     & sum135624(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z120,-1.000)
       deallocate(Z120)
       deallocate(S65)
C
       allocate(D1(N1+1:N3,N0+1:M1,N2+1:M2,N1+1:M2))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,N0,M1,N2,M2,N1,M2,VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S66(N0+1:M1,N0+1:M1,N2+1:M2,N1+1:M2))
       I1=K9*K0*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S66)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,M1,N2,M2,N1,M2,
     & N0,M1,N2,M2,N1,M2,N0,M1,S66,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder412356(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,M2,N3,N0,M1,M1,N1,t3B3,F2)
       allocate(Z121(M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K6*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z121)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124635(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z121,-1.000)
       call
     & sum125634(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z121, 1.000)
       deallocate(Z121)
       deallocate(S66)
C
       allocate(D1(N1+1:N3,M1+1:N2,N2+1:M2,N1+1:M2))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,M1,N2,N2,M2,N1,M2,VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S67(N0+1:M1,M1+1:N2,N2+1:M2,N1+1:M2))
       I1=K9*K0*K8
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S67)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder2341(N0,M1,M1,N2,N2,M2,N1,M2,
     & M1,N2,N2,M2,N1,M2,N0,M1,S67,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder412356(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,M2,N3,N0,M1,M1,N1,t3B3,F2)
       allocate(Z122(M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K6*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z122)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124635(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z122,-1.000)
       call
     & sum125634(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z122, 1.000)
       deallocate(Z122)
       deallocate(S67)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M1+1:N1))
       call reorder3142(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,M1,N1,VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q13(M1+1:N1,M1+1:N1))
       I1=K7*K7
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q13)
       deallocate(D1)
       deallocate(B2)
C
       X9=X9+Q13
       deallocate(Q13)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N0+1:M1))
       call reorder3142(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q14(N0+1:M1,N0+1:M1))
       I1=K5*K5
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q14)
       deallocate(D1)
       deallocate(B2)
C
       X10=X10+Q14
       deallocate(Q14)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N0+1:M1))
       call reorder3142(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q15(M1+1:N1,N0+1:M1))
       I1=K5*K7
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q15)
       deallocate(D1)
       deallocate(B2)
C
       X11=X11+Q15
       deallocate(Q15)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,M2+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,N3,M2,N3,M2,N3,VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q16(M2+1:N3,M2+1:N3))
       I1=K6*K6
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q16)
       deallocate(D1)
       deallocate(B2)
C
       X12=X12+Q16
       deallocate(Q16)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,M2+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,N3,N1,M2,M2,N3,VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q17(N1+1:M2,M2+1:N3))
       I1=K6*K9
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q17)
       deallocate(D1)
       deallocate(B2)
C
       X13=X13+Q17
       deallocate(Q17)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,N3,N1,M2,N1,M2,VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q18(N1+1:M2,N1+1:M2))
       I1=K9*K9
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q18)
       deallocate(D1)
       deallocate(B2)
C
       X14=X14+Q18
       deallocate(Q18)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,FAHP,B1)
       allocate(D2(N1+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,M2,N3,N0,M1,M1,N1,t2A,D2)
       allocate(S68(M2+1:N3,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K6
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S68)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,M2,N3,N0,M1,M1,N1,X1,S68,-1.000)
       deallocate(S68)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,FAHP,B1)
       allocate(D2(N1+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,M2,N0,M1,M1,N1,t2A,D2)
       allocate(S69(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S69)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X2,S69,-1.000)
       deallocate(S69)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,FAHP,B1)
       allocate(D2(N1+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,M2,N0,M1,N0,M1,t2A,D2)
       allocate(S70(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S70)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,N0,M1,X6,S70,-1.000)
       deallocate(S70)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,FAHP,B1)
       allocate(D2(N1+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,M2,N3,N0,M1,N0,M1,t2A,D2)
       allocate(S71(M2+1:N3,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K6
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S71)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,M2,N3,N0,M1,N0,M1,X5,S71,-1.000)
       deallocate(S71)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M1+1:N1))
       call reorder3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,M1,N1,VAHHHP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,M2,N3,N1,M2,t2A,D2)
       allocate(S72(M2+1:N3,N1+1:M2,N1+1:N3,M1+1:N1))
       I1=K7*K3
       I2=K9*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S72)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N1,N3,M2,N3,N1,M2,M1,N1,X4,S72, 0.500)
       deallocate(S72)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M1+1:N1))
       call reorder3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,M1,N1,VAHHHP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,M2+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,M2,N3,M2,N3,t2A,D2)
       allocate(S73(M2+1:N3,M2+1:N3,N1+1:N3,M1+1:N1))
       I1=K7*K3
       I2=K6*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S73)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X3(N1+1:N3,M2+1:N3,M2+1:N3,M1+1:N1))
       X3=0.0d0
       call
     & sum2314(N1,N3,M2,N3,M2,N3,M1,N1,X3,S73, 0.500)
       deallocate(S73)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M1+1:N1))
       call reorder4132(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,M1,N1,VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S74(N1+1:M2,N0+1:M1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S74)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N1,M2,N0,M1,M1,N1,X2,S74,-1.000)
       deallocate(S74)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M1+1:N1))
       call reorder3142(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,M1,N1,VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2A,D2)
       allocate(S75(M2+1:N3,N0+1:M1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S75)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,M2,N3,N0,M1,M1,N1,X1,S75, 1.000)
       deallocate(S75)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:M1))
       call reorder3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N0,M1,VAHHHP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,M2,N3,N1,M2,t2A,D2)
       allocate(S76(M2+1:N3,N1+1:M2,N1+1:N3,N0+1:M1))
       I1=K5*K3
       I2=K9*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S76)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N1,N3,M2,N3,N1,M2,N0,M1,X8,S76, 0.500)
       deallocate(S76)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:M1))
       call reorder3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N0,M1,VAHHHP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,M2+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,M2,N3,M2,N3,t2A,D2)
       allocate(S77(M2+1:N3,M2+1:N3,N1+1:N3,N0+1:M1))
       I1=K5*K3
       I2=K6*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S77)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X7(N1+1:N3,M2+1:N3,M2+1:N3,N0+1:M1))
       X7=0.0d0
       call
     & sum2314(N1,N3,M2,N3,M2,N3,N0,M1,X7,S77, 0.500)
       deallocate(S77)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder4132(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N0,M1,VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S78(N1+1:M2,N0+1:M1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S78)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N1,M2,N0,M1,N0,M1,X41,S78,-1.000)
       deallocate(S78)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder3142(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N0,M1,VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2A,D2)
       allocate(S79(M2+1:N3,N0+1:M1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S79)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,M2,N3,N0,M1,N0,M1,X40,S79, 1.000)
       deallocate(S79)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder3142(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N0,M1,VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N1,t2A,D2)
       allocate(S80(M2+1:N3,M1+1:N1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S80)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N1,M2,N3,N0,M1,M1,N1,X1,S80,-1.000)
       deallocate(S80)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder4132(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N0,M1,VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S81(N1+1:M2,M1+1:N1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S81)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N1,N1,M2,N0,M1,M1,N1,X2,S81, 1.000)
       deallocate(S81)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,M2+1:N3))
       call reorder4213(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,M2,N3,VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S82(N1+1:M2,N0+1:M1,N1+1:N3,M2+1:N3))
       I1=K6*K3
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S82)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N1,N3,M2,N3,N1,M2,N0,M1,X8,S82, 1.000)
       deallocate(S82)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,M2+1:N3))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,M2,N3,VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2A,D2)
       allocate(S83(M2+1:N3,N0+1:M1,N1+1:N3,M2+1:N3))
       I1=K6*K3
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S83)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N1,N3,M2,N3,M2,N3,N0,M1,X42,S83, 1.000)
       deallocate(S83)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,M2+1:N3))
       call reorder1243(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N1,N3,N0,N1,M2,N3,VAHPPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,M1,M1,N1,t2A,D2)
       allocate(S84(N0+1:M1,M1+1:N1,N0+1:N1,M2+1:N3))
       I1=K6*K1
       I2=K7*K5
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S84)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,N1,M2,N3,N0,M1,M1,N1,X1,S84, 0.500)
       deallocate(S84)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder4213(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,M2,VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2A,D2)
       allocate(S85(M2+1:N3,N0+1:M1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S85)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N1,N3,M2,N3,N1,M2,N0,M1,X8,S85,-1.000)
       deallocate(S85)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:M2))
       call reorder1243(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N1,N3,N0,N1,N1,M2,VAHPPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,M1,M1,N1,t2A,D2)
       allocate(S86(N0+1:M1,M1+1:N1,N0+1:N1,N1+1:M2))
       I1=K9*K1
       I2=K7*K5
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S86)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,N1,N1,M2,N0,M1,M1,N1,X2,S86, 0.500)
       deallocate(S86)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,M2+1:N3))
       call reorder1243(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N1,N3,N0,N1,M2,N3,VAHPPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,M1,N0,M1,t2A,D2)
       allocate(S87(N0+1:M1,N0+1:M1,N0+1:N1,M2+1:N3))
       I1=K6*K1
       I2=K5*K5
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S87)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,N1,M2,N3,N0,M1,N0,M1,X5,S87, 0.500)
       deallocate(S87)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,M2+1:N3))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,M2,N3,VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N1,t2A,D2)
       allocate(S88(M2+1:N3,M1+1:N1,N1+1:N3,M2+1:N3))
       I1=K6*K3
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S88)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N1,N3,M2,N3,M2,N3,M1,N1,X39,S88, 1.000)
       deallocate(S88)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,M2+1:N3))
       call reorder4213(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,M2,N3,VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S89(N1+1:M2,M1+1:N1,N1+1:N3,M2+1:N3))
       I1=K6*K3
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S89)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N1,N3,M2,N3,N1,M2,M1,N1,X4,S89, 1.000)
       deallocate(S89)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:M2))
       call reorder1243(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N1,N3,N0,N1,N1,M2,VAHPPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,M1,N0,M1,t2A,D2)
       allocate(S90(N0+1:M1,N0+1:M1,N0+1:N1,N1+1:M2))
       I1=K9*K1
       I2=K5*K5
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S90)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,N1,N1,M2,N0,M1,N0,M1,X6,S90, 0.500)
       deallocate(S90)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,M2,VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N1,t2A,D2)
       allocate(S91(M2+1:N3,M1+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S91)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N1,N3,M2,N3,N1,M2,M1,N1,X4,S91, 1.000)
       deallocate(S91)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M1+1:N1))
       call reorder3142(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,M1,N1,VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S92(N1+1:M2,N0+1:M1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S92)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N1,M2,N0,M1,M1,N1,X2,S92, 1.000)
       deallocate(S92)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M1+1:N1))
       call reorder3142(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,M1,N1,VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S93(M2+1:N3,N0+1:M1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S93)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,M2,N3,N0,M1,M1,N1,X1,S93, 1.000)
       deallocate(S93)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder3142(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N0,M1,VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S94(N1+1:M2,N0+1:M1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S94)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N1,M2,N0,M1,N0,M1,X41,S94, 1.000)
       deallocate(S94)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder3142(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N0,M1,VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S95(M2+1:N3,N0+1:M1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S95)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,M2,N3,N0,M1,N0,M1,X40,S95, 1.000)
       deallocate(S95)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,M2+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,N3,N1,N3,M2,N3,VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S96(N1+1:M2,N0+1:M1,N1+1:N3,M2+1:N3))
       I1=K6*K3
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S96)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N1,N3,M2,N3,N1,M2,N0,M1,X8,S96, 1.000)
       deallocate(S96)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,M2+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,N3,N1,N3,M2,N3,VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S97(M2+1:N3,N0+1:M1,N1+1:N3,M2+1:N3))
       I1=K6*K3
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S97)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N1,N3,M2,N3,M2,N3,N0,M1,X42,S97,-1.000)
       deallocate(S97)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,N3,N1,N3,N1,M2,VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S98(M2+1:N3,N0+1:M1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S98)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N1,N3,M2,N3,N1,M2,N0,M1,X8,S98,-1.000)
       deallocate(S98)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder3142(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N0,M1,VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S99(N1+1:M2,M1+1:N1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S99)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N1,N1,M2,N0,M1,M1,N1,X2,S99,-1.000)
       deallocate(S99)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder3142(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N0,M1,VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,M1,N1,t2B,D2)
       allocate(S100(M2+1:N3,M1+1:N1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S100)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N1,M2,N3,N0,M1,M1,N1,X1,S100,-1.000)
       deallocate(S100)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,M2+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,N3,N1,N3,M2,N3,VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S101(N1+1:M2,M1+1:N1,N1+1:N3,M2+1:N3))
       I1=K6*K3
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S101)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N1,N3,M2,N3,N1,M2,M1,N1,X4,S101, 1.000)
       deallocate(S101)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,M2+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,N3,N1,N3,M2,N3,VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,M1,N1,t2B,D2)
       allocate(S102(M2+1:N3,M1+1:N1,N1+1:N3,M2+1:N3))
       I1=K6*K3
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S102)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N1,N3,M2,N3,M2,N3,M1,N1,X39,S102,-1.000)
       deallocate(S102)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,N3,N1,N3,N1,M2,VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,M1,N1,t2B,D2)
       allocate(S103(M2+1:N3,M1+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S103)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N1,N3,M2,N3,N1,M2,M1,N1,X4,S103,-1.000)
       deallocate(S103)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M1,N1,M2,N3,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder461235(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,M2,N3,N1,M2,N0,M1,t3A,F2)
       allocate(S104(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S104)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,N0,M1,X8,S104,-1.000)
       deallocate(S104)
C
       allocate(D1(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M1,N1,M2,N3,N1,N3,VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder561234(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M1,N1,M2,N3,M2,N3,N1,M2,N0,M1,t3A,F2)
       allocate(S105(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K6*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S105)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,N0,M1,X8,S105, 0.500)
       deallocate(S105)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M1,N1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder462135(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M1,N1,N1,M2,M2,N3,N1,M2,N0,M1,t3A,F2)
       allocate(S106(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S106)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,N0,M1,X8,S106, 1.000)
       deallocate(S106)
C
       allocate(D1(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M1,N1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder562134(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M1,N1,N1,M2,M2,N3,N1,M2,N0,M1,t3A,F2)
       allocate(S107(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K9*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S107)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,N0,M1,X8,S107,-0.500)
       deallocate(S107)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M1,N1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder463125(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M1,N1,N1,M2,M2,N3,M2,N3,N0,M1,t3A,F2)
       allocate(S108(M2+1:N3,M2+1:N3,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K6*K6
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S108)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,M2,N3,N0,M1,X7,S108, 1.000)
       deallocate(S108)
C
       allocate(D1(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M1,N1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder563124(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M1,N1,N1,M2,M2,N3,M2,N3,N0,M1,t3A,F2)
       allocate(S109(M2+1:N3,M2+1:N3,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K6*K6
       I3=K9*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S109)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,M2,N3,N0,M1,X7,S109,-0.500)
       deallocate(S109)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,M1,M1,N1,VAHHPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,M1,M1,N1,t2A,D2)
       allocate(S110(N0+1:M1,M1+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5
       I2=K7*K5
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S110)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M1,N1,N0,M1,M1,N1,X15,S110, 0.500)
       deallocate(S110)
C
       allocate(D1(N1+1:N3,N1+1:N3,M1+1:N1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,M1,N1,M1,N1,VAHHPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,M1,M1,N1,t2A,D2)
       allocate(S111(N0+1:M1,M1+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7
       I2=K7*K5
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S111)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,M1,N1,N0,M1,M1,N1,X16,S111, 0.500)
       deallocate(S111)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder4123(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,M2,N3,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder612345(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,t3A,F2)
       allocate(S112(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K6*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S112)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,N0,M1,X6,S112, 0.500)
       deallocate(S112)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder4123(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder612345(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,N1,M2,N0,M1,N0,M1,t3A,F2)
       allocate(S113(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S113)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,N0,M1,X6,S113, 1.000)
       deallocate(S113)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder4123(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,N1,M2,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder612345(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,N1,M2,N1,M2,N0,M1,N0,M1,t3A,F2)
       allocate(S114(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K9*K9*K7
       call EGEMM(I1,I2,I3,D1,F2,S114)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,N0,M1,X6,S114, 0.500)
       deallocate(S114)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder613245(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,M2,N3,N0,M1,N0,M1,t3A,F2)
       allocate(S115(M2+1:N3,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K6
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S115)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,N0,M1,N0,M1,X5,S115, 1.000)
       deallocate(S115)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,N1,M2,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder623145(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,N1,M2,M2,N3,N0,M1,N0,M1,t3A,F2)
       allocate(S116(M2+1:N3,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K6
       I3=K9*K9*K7
       call EGEMM(I1,I2,I3,D1,F2,S116)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,N0,M1,N0,M1,X5,S116,-0.500)
       deallocate(S116)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N1,t2A,D2)
       allocate(S117(M2+1:N3,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S117)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,M2,N3,M2,N3,M1,N1,X17,S117,-1.000)
       deallocate(S117)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N1,t2A,D2)
       allocate(S118(M2+1:N3,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S118)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,N1,M2,M2,N3,M1,N1,X18,S118,-1.000)
       deallocate(S118)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S119(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S119)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder3412(N1,M2,M1,N1,M1,N1,N1,M2,
     & M1,N1,N1,M2,N1,M2,M1,N1,S119,D1)
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder631245(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,M2,N3,M2,N3,N0,M1,N0,M1,t3A,F2)
       allocate(Z180(M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K6*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,D1,F2,Z180)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z180, 1.000)
       deallocate(Z180)
       deallocate(S119)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,M1,N1,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,M1,N1,t2A,D2)
       allocate(Q19(M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7
       I3=K3*K3*K1
       call EGEMM(I1,I2,I3,D1,D2,Q19)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M1,N1,M1,N1,X9,Q19, 0.500)
       deallocate(Q19)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,N0,M1,M2,N3,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder451236(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N0,M1,M2,N3,M2,N3,N1,M2,M1,N1,t3A,F2)
       allocate(S120(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S120)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N1,X4,S120, 0.500)
       deallocate(S120)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M1,N1,M2,N3,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder451236(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,M2,N3,N1,M2,M1,N1,t3A,F2)
       allocate(S121(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S121)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N1,X4,S121, 1.000)
       deallocate(S121)
C
       allocate(D1(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M1,N1,M2,N3,N1,N3,VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder451236(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M1,N1,M2,N3,M2,N3,N1,M2,M1,N1,t3A,F2)
       allocate(S122(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S122)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N1,X4,S122, 0.500)
       deallocate(S122)
C
       allocate(D1(N0+1:M1,N0+1:M1,N1+1:M2,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,N0,M1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder452136(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N0,M1,N1,M2,M2,N3,N1,M2,M1,N1,t3A,F2)
       allocate(S123(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K9*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S123)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N1,X4,S123,-0.500)
       deallocate(S123)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M1,N1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder452136(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M1,N1,N1,M2,M2,N3,N1,M2,M1,N1,t3A,F2)
       allocate(S124(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S124)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N1,X4,S124,-1.000)
       deallocate(S124)
C
       allocate(D1(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M1,N1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder452136(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M1,N1,N1,M2,M2,N3,N1,M2,M1,N1,t3A,F2)
       allocate(S125(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K9*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S125)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N1,X4,S125,-0.500)
       deallocate(S125)
C
       allocate(D1(N0+1:M1,N0+1:M1,N1+1:M2,N1+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,N0,M1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder453126(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N0,M1,N1,M2,M2,N3,M2,N3,M1,N1,t3A,F2)
       allocate(S126(M2+1:N3,M2+1:N3,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K6*K6
       I3=K9*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S126)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,M2,N3,M1,N1,X3,S126,-0.500)
       deallocate(S126)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M1,N1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder453126(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M1,N1,N1,M2,M2,N3,M2,N3,M1,N1,t3A,F2)
       allocate(S127(M2+1:N3,M2+1:N3,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K6*K6
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S127)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,M2,N3,M1,N1,X3,S127,-1.000)
       deallocate(S127)
C
       allocate(D1(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M1,N1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder453126(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M1,N1,N1,M2,M2,N3,M2,N3,M1,N1,t3A,F2)
       allocate(S128(M2+1:N3,M2+1:N3,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K6*K6
       I3=K9*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S128)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,M2,N3,M1,N1,X3,S128,-0.500)
       deallocate(S128)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,M1,N0,M1,VAHHPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,M1,N0,M1,t2A,D2)
       allocate(S129(N0+1:M1,N0+1:M1,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K5*K5
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S129)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X20(N0+1:M1,N0+1:M1,N0+1:M1,N0+1:M1))
       X20=0.0d0
       call
     & sum3412(N0,M1,N0,M1,N0,M1,N0,M1,X20,S129, 0.500)
       deallocate(S129)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,M1,M1,N1,VAHHPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,M1,N0,M1,t2A,D2)
       allocate(S130(N0+1:M1,N0+1:M1,N0+1:M1,M1+1:N1))
       I1=K7*K5
       I2=K5*K5
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S130)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X21(N0+1:M1,M1+1:N1,N0+1:M1,N0+1:M1))
       X21=0.0d0
       call
     & sum3412(N0,M1,M1,N1,N0,M1,N0,M1,X21,S130, 0.500)
       deallocate(S130)
C
       allocate(D1(N1+1:N3,N1+1:N3,M1+1:N1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,M1,N1,M1,N1,VAHHPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,M1,N0,M1,t2A,D2)
       allocate(S131(N0+1:M1,N0+1:M1,M1+1:N1,M1+1:N1))
       I1=K7*K7
       I2=K5*K5
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S131)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X22(M1+1:N1,M1+1:N1,N0+1:M1,N0+1:M1))
       X22=0.0d0
       call
     & sum3412(M1,N1,M1,N1,N0,M1,N0,M1,X22,S131, 0.500)
       deallocate(S131)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder4123(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(S132(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S132)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X2,S132, 0.500)
       deallocate(S132)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder4123(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,M2,N3,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder512346(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(S133(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K6*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S133)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X2,S133,-0.500)
       deallocate(S133)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder4123(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(S134(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S134)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X2,S134, 1.000)
       deallocate(S134)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder4123(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder512346(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(S135(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S135)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X2,S135,-1.000)
       deallocate(S135)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder4123(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,N1,M2,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N1,M2,N1,M2,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(S136(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K9*K5
       call EGEMM(I1,I2,I3,D1,F2,S136)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X2,S136, 0.500)
       deallocate(S136)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder4123(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,N1,M2,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder512346(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,N1,M2,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(S137(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K9*K7
       call EGEMM(I1,I2,I3,D1,F2,S137)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X2,S137,-0.500)
       deallocate(S137)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder413256(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,M2,N3,N0,M1,M1,N1,t3A,F2)
       allocate(S138(M2+1:N3,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K6
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S138)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,N0,M1,M1,N1,X1,S138, 1.000)
       deallocate(S138)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder513246(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,M2,N3,N0,M1,M1,N1,t3A,F2)
       allocate(S139(M2+1:N3,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K6
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S139)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,N0,M1,M1,N1,X1,S139,-1.000)
       deallocate(S139)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,N1,M2,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder423156(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N1,M2,N1,M2,M2,N3,N0,M1,M1,N1,t3A,F2)
       allocate(S140(M2+1:N3,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K6
       I3=K9*K9*K5
       call EGEMM(I1,I2,I3,D1,F2,S140)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,N0,M1,M1,N1,X1,S140,-0.500)
       deallocate(S140)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,N1,M2,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder523146(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,N1,M2,M2,N3,N0,M1,M1,N1,t3A,F2)
       allocate(S141(M2+1:N3,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K6
       I3=K9*K9*K7
       call EGEMM(I1,I2,I3,D1,F2,S141)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,N0,M1,M1,N1,X1,S141, 0.500)
       deallocate(S141)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2A,D2)
       allocate(S142(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S142)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,M2,N3,N0,M1,X23,S142,-1.000)
       deallocate(S142)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2A,D2)
       allocate(S143(M2+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S143)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,M2,N3,M2,N3,N0,M1,X24,S143,-1.000)
       deallocate(S143)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2A,D2)
       allocate(S144(M2+1:N3,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S144)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N1,M2,M2,N3,N0,M1,X25,S144,-1.000)
       deallocate(S144)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2A,D2)
       allocate(S145(M2+1:N3,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S145)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,N1,M2,M2,N3,N0,M1,X26,S145,-1.000)
       deallocate(S145)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S146(N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S146)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder3412(N1,M2,N0,M1,N0,M1,N1,M2,
     & N0,M1,N1,M2,N1,M2,N0,M1,S146,D1)
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder431256(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N1,M2,M2,N3,M2,N3,N0,M1,M1,N1,t3A,F2)
       allocate(Z208(M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K6*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,D1,F2,Z208)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124635(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z208,-1.000)
       call
     & sum125634(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z208, 1.000)
       deallocate(Z208)
       deallocate(S146)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S147(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S147)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder3412(N1,M2,N0,M1,M1,N1,N1,M2,
     & M1,N1,N1,M2,N1,M2,N0,M1,S147,D1)
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder531246(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,M2,N3,M2,N3,N0,M1,M1,N1,t3A,F2)
       allocate(Z209(M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K6*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,D1,F2,Z209)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124635(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z209, 1.000)
       call
     & sum125634(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z209,-1.000)
       deallocate(Z209)
       deallocate(S147)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,M1,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,M1,t2A,D2)
       allocate(Q20(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K3*K3*K1
       call EGEMM(I1,I2,I3,D1,D2,Q20)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N0,M1,N0,M1,X10,Q20, 0.500)
       deallocate(Q20)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,M1,N1,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,M1,t2A,D2)
       allocate(Q21(N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5
       I3=K3*K3*K1
       call EGEMM(I1,I2,I3,D1,D2,Q21)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M1,N1,N0,M1,X11,Q21, 0.500)
       deallocate(Q21)
C
       allocate(D1(M2+1:N3,M2+1:N3,N0+1:N1,N0+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & M2,N3,M2,N3,N0,N1,N0,N1,VAHHPP,D1)
       allocate(F2(M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder123456(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,t3A,F2)
       allocate(U212(N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K7*K5*K5*K9
       I3=K6*K6
       call EGEMM(I1,I2,I3,D1,F2,U212)
       deallocate(D1)
       deallocate(F2)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder561234(N1,M2,N0,M1,N0,M1,M1,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,M2,N0,M1,N0,M1,M1,N1,U212,F1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,M2+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,M2,N3,M2,N3,t2A,D2)
       allocate(Z213(M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K5*K9
       I2=K6*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,F1,D2,Z213)
       deallocate(F1)
       deallocate(D2)
C
       V3A=V3A+0.250*Z213
       deallocate(Z213)
C
       allocate(F1(N0+1:N1,N0+1:N1,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder651234(N1,M2,N0,M1,N0,M1,M1,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,M2,N0,M1,N0,M1,M1,N1,U212,F1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(U333(M2+1:N3,N0+1:N1,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K5*K9*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,F1,B2,U333)
       deallocate(F1)
       deallocate(B2)
C
       allocate(F1(N0+1:N1,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder213456(M2,N3,N0,N1,N1,M2,N0,M1,N0,M1,M1,N1,
     & N0,N1,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,U333,F1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(Z334(M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K5*K9*K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,F1,B2,Z334)
       deallocate(F1)
       deallocate(B2)
C
       V3A=V3A+0.500*Z334
       deallocate(Z334)
       deallocate(U333)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:M2,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,M2,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,M2+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,M2,N3,M2,N3,t2A,D2)
       allocate(S149(M2+1:N3,M2+1:N3,N1+1:M2,N1+1:M2))
       I1=K9*K9
       I2=K6*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S149)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X30(N1+1:M2,N1+1:M2,M2+1:N3,M2+1:N3))
       X30=0.0d0
       call
     & sum3412(N1,M2,N1,M2,M2,N3,M2,N3,X30,S149, 0.500)
       deallocate(S149)
C
       allocate(D1(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,M2,N3,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,M2,N3,N1,M2,t2A,D2)
       allocate(S150(M2+1:N3,N1+1:M2,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K9*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S150)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M2,N3,N1,M2,M2,N3,N1,M2,X31,S150, 0.500)
       deallocate(S150)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:M2,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,M2,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,M2,N3,N1,M2,t2A,D2)
       allocate(S151(M2+1:N3,N1+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9
       I2=K9*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S151)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N1,M2,N1,M2,M2,N3,N1,M2,X32,S151, 0.500)
       deallocate(S151)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,M2,N3,t2A,D2)
       allocate(Q22(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K3*K1*K1
       call EGEMM(I1,I2,I3,D1,D2,Q22)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M2,N3,M2,N3,X12,Q22,-0.500)
       deallocate(Q22)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,M2,N3,t2A,D2)
       allocate(Q23(M2+1:N3,N1+1:M2))
       I1=K9
       I2=K6
       I3=K3*K1*K1
       call EGEMM(I1,I2,I3,D1,D2,Q23)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N1,M2,M2,N3,X13,Q23,-0.500)
       deallocate(Q23)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,M2,t2A,D2)
       allocate(Q24(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K3*K1*K1
       call EGEMM(I1,I2,I3,D1,D2,Q24)
       deallocate(D1)
       deallocate(D2)
C
       allocate(B1(N1+1:M2,N1+1:M2))
       call reorder21(N1,M2,N1,M2,
     & N1,M2,N1,M2,Q24,B1)
       allocate(F2(N1+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder312456(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N1,M2,M2,N3,M2,N3,N0,M1,N0,M1,M1,N1,t3A,F2)
       allocate(Z220(M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K5*K6*K6
       I3=K9
       call EGEMM(I1,I2,I3,B1,F2,Z220)
       deallocate(B1)
       deallocate(F2)
C
       call
     & sum124563(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z220, 0.500)
       deallocate(Z220)
       deallocate(Q24)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder451236(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,N0,M1,M2,N3,M2,N3,N1,M2,N0,M1,t3B4,F2)
       allocate(S152(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S152)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,N0,M1,X8,S152,-1.000)
       deallocate(S152)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,M2,N3,N1,M2,N0,M1,t3B1,F2)
       allocate(S153(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S153)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,N0,M1,X8,S153, 1.000)
       deallocate(S153)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,M2,N3,M2,N3,N1,M2,N0,M1,t3B1,F2)
       allocate(S154(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S154)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,N0,M1,X8,S154, 1.000)
       deallocate(S154)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder451236(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,N0,M1,N2,M2,M2,N3,N1,M2,N0,M1,t3B4,F2)
       allocate(S155(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K0*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S155)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,N0,M1,X8,S155,-1.000)
       deallocate(S155)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,N2,M2,M2,N3,N1,M2,N0,M1,t3B1,F2)
       allocate(S156(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K0*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S156)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,N0,M1,X8,S156, 1.000)
       deallocate(S156)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,N2,M2,M2,N3,N1,M2,N0,M1,t3B1,F2)
       allocate(S157(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K0*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S157)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,N0,M1,X8,S157, 1.000)
       deallocate(S157)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder451236(N2,M2,M2,N3,M2,N3,M1,N2,N0,M1,N0,M1,
     & M1,N2,N0,M1,N2,M2,M2,N3,M2,N3,N0,M1,t3B2,F2)
       allocate(S158(M2+1:N3,M2+1:N3,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K6*K6
       I3=K0*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S158)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,M2,N3,N0,M1,X7,S158,-1.000)
       deallocate(S158)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder461235(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,N2,M2,M2,N3,M2,N3,N0,M1,t3B3,F2)
       allocate(S159(M2+1:N3,M2+1:N3,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K6*K6
       I3=K0*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S159)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,M2,N3,N0,M1,X7,S159, 1.000)
       deallocate(S159)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder461235(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,N2,M2,M2,N3,M2,N3,N0,M1,t3B3,F2)
       allocate(S160(M2+1:N3,M2+1:N3,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K6*K6
       I3=K0*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S160)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,M2,N3,N0,M1,X7,S160, 1.000)
       deallocate(S160)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder412356(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,t3B4,F2)
       allocate(S161(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S161)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,N0,M1,X6,S161,-1.000)
       deallocate(S161)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder412356(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,N2,M2,M2,N3,N1,M2,N0,M1,N0,M1,t3B4,F2)
       allocate(S162(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K6*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S162)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,N0,M1,X6,S162,-1.000)
       deallocate(S162)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder412356(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,M2,N3,N1,M2,N1,M2,N0,M1,N0,M1,t3B4,F2)
       allocate(S163(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K9*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S163)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,N0,M1,X6,S163,-1.000)
       deallocate(S163)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder412356(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,N2,M2,N1,M2,N1,M2,N0,M1,N0,M1,t3B4,F2)
       allocate(S164(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K9*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S164)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,N0,M1,X6,S164,-1.000)
       deallocate(S164)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder412356(N2,M2,M2,N3,M2,N3,M1,N2,N0,M1,N0,M1,
     & M1,N2,N2,M2,M2,N3,M2,N3,N0,M1,N0,M1,t3B2,F2)
       allocate(S165(M2+1:N3,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K6
       I3=K6*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S165)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,N0,M1,N0,M1,X5,S165,-1.000)
       deallocate(S165)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder413256(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,M2,N3,N1,M2,M2,N3,N0,M1,N0,M1,t3B4,F2)
       allocate(S166(M2+1:N3,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K6
       I3=K9*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S166)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,N0,M1,N0,M1,X5,S166, 1.000)
       deallocate(S166)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder413256(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,N2,M2,N1,M2,M2,N3,N0,M1,N0,M1,t3B4,F2)
       allocate(S167(M2+1:N3,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K6
       I3=K9*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S167)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,N0,M1,N0,M1,X5,S167, 1.000)
       deallocate(S167)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N1,t2A,D2)
       allocate(S168(M2+1:N3,M1+1:N1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S168)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X46(M1+1:N2,M2+1:N3,M2+1:N3,M1+1:N1))
       X46=0.0d0
       call
     & sum3412(M1,N2,M2,N3,M2,N3,M1,N1,X46,S168, 1.000)
       deallocate(S168)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N1,t2A,D2)
       allocate(S169(M2+1:N3,M1+1:N1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S169)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X47(M1+1:N2,N2+1:M2,M2+1:N3,M1+1:N1))
       X47=0.0d0
       call
     & sum3412(M1,N2,N2,M2,M2,N3,M1,N1,X47,S169, 1.000)
       deallocate(S169)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S170(N1+1:M2,M1+1:N1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S170)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X48(M1+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       X48=0.0d0
       call
     & sum3412(M1,N2,N2,M2,N1,M2,M1,N1,X48,S170, 1.000)
       deallocate(S170)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N0,M1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N0,M1,M2,N3,M2,N3,N1,M2,M1,N1,t3B1,F2)
       allocate(S171(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S171)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N1,X4,S171,-1.000)
       deallocate(S171)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N0,M1,M2,N3,M2,N3,N1,M2,M1,N1,t3B1,F2)
       allocate(S172(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S172)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N1,X4,S172,-1.000)
       deallocate(S172)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,M2,N3,N1,M2,M1,N1,t3B1,F2)
       allocate(S173(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S173)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N1,X4,S173,-1.000)
       deallocate(S173)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,M2,N3,M2,N3,N1,M2,M1,N1,t3B1,F2)
       allocate(S174(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S174)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N1,X4,S174,-1.000)
       deallocate(S174)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N0,M1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N0,M1,N2,M2,M2,N3,N1,M2,M1,N1,t3B1,F2)
       allocate(S175(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S175)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N1,X4,S175,-1.000)
       deallocate(S175)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N0,M1,N2,M2,M2,N3,N1,M2,M1,N1,t3B1,F2)
       allocate(S176(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K0*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S176)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N1,X4,S176,-1.000)
       deallocate(S176)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,N2,M2,M2,N3,N1,M2,M1,N1,t3B1,F2)
       allocate(S177(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K0*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S177)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N1,X4,S177,-1.000)
       deallocate(S177)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,N2,M2,M2,N3,N1,M2,M1,N1,t3B1,F2)
       allocate(S178(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K0*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S178)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N1,X4,S178,-1.000)
       deallocate(S178)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N0,M1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder451236(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & N0,M1,N0,M1,N2,M2,M2,N3,M2,N3,M1,N1,t3B3,F2)
       allocate(S179(M2+1:N3,M2+1:N3,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K6*K6
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S179)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,M2,N3,M1,N1,X3,S179,-1.000)
       deallocate(S179)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder451236(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N2,N0,M1,N2,M2,M2,N3,M2,N3,M1,N1,t3B3,F2)
       allocate(S180(M2+1:N3,M2+1:N3,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K6*K6
       I3=K0*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S180)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,M2,N3,M1,N1,X3,S180,-1.000)
       deallocate(S180)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder451236(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,N2,M2,M2,N3,M2,N3,M1,N1,t3B3,F2)
       allocate(S181(M2+1:N3,M2+1:N3,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K6*K6
       I3=K0*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S181)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,M2,N3,M1,N1,X3,S181,-1.000)
       deallocate(S181)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder451236(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,N2,M2,M2,N3,M2,N3,M1,N1,t3B3,F2)
       allocate(S182(M2+1:N3,M2+1:N3,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K6*K6
       I3=K0*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S182)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,M2,N3,M1,N1,X3,S182,-1.000)
       deallocate(S182)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(S183(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S183)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X2,S183,-1.000)
       deallocate(S183)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(S184(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S184)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X2,S184,-1.000)
       deallocate(S184)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(S185(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S185)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X2,S185,-1.000)
       deallocate(S185)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(S186(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K6*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S186)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X2,S186,-1.000)
       deallocate(S186)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(S187(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S187)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X2,S187,-1.000)
       deallocate(S187)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(S188(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S188)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X2,S188,-1.000)
       deallocate(S188)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(S189(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S189)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X2,S189,-1.000)
       deallocate(S189)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(S190(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S190)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X2,S190,-1.000)
       deallocate(S190)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder412356(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,M2,N3,N0,M1,M1,N1,t3B3,F2)
       allocate(S191(M2+1:N3,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K6
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S191)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,N0,M1,M1,N1,X1,S191,-1.000)
       deallocate(S191)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder412356(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,M2,N3,N0,M1,M1,N1,t3B3,F2)
       allocate(S192(M2+1:N3,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K6
       I3=K6*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S192)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,N0,M1,M1,N1,X1,S192,-1.000)
       deallocate(S192)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder413256(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,M2,N3,N0,M1,M1,N1,t3B1,F2)
       allocate(S193(M2+1:N3,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K6
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S193)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,N0,M1,M1,N1,X1,S193, 1.000)
       deallocate(S193)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder413256(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,N1,M2,M2,N3,N0,M1,M1,N1,t3B1,F2)
       allocate(S194(M2+1:N3,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K6
       I3=K9*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S194)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,N0,M1,M1,N1,X1,S194, 1.000)
       deallocate(S194)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder413256(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,N1,M2,M2,N3,N0,M1,M1,N1,t3B1,F2)
       allocate(S195(M2+1:N3,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K6
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S195)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,N0,M1,M1,N1,X1,S195, 1.000)
       deallocate(S195)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder413256(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,N1,M2,M2,N3,N0,M1,M1,N1,t3B1,F2)
       allocate(S196(M2+1:N3,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K6
       I3=K9*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S196)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,N0,M1,M1,N1,X1,S196, 1.000)
       deallocate(S196)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2A,D2)
       allocate(S197(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S197)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X49(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:M1))
       X49=0.0d0
       call
     & sum3412(N0,M1,M2,N3,M2,N3,N0,M1,X49,S197, 1.000)
       deallocate(S197)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2A,D2)
       allocate(S198(M2+1:N3,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S198)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X50(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:M1))
       X50=0.0d0
       call
     & sum3412(M1,N2,M2,N3,M2,N3,N0,M1,X50,S198, 1.000)
       deallocate(S198)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2A,D2)
       allocate(S199(M2+1:N3,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S199)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X51(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:M1))
       X51=0.0d0
       call
     & sum3412(N0,M1,N2,M2,M2,N3,N0,M1,X51,S199, 1.000)
       deallocate(S199)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2A,D2)
       allocate(S200(M2+1:N3,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S200)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X52(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:M1))
       X52=0.0d0
       call
     & sum3412(M1,N2,N2,M2,M2,N3,N0,M1,X52,S200, 1.000)
       deallocate(S200)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S201(N1+1:M2,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S201)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X53(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:M1))
       X53=0.0d0
       call
     & sum3412(N0,M1,N2,M2,N1,M2,N0,M1,X53,S201, 1.000)
       deallocate(S201)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S202(N1+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S202)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X54(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:M1))
       X54=0.0d0
       call
     & sum3412(M1,N2,N2,M2,N1,M2,N0,M1,X54,S202, 1.000)
       deallocate(S202)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,M1,N1,t2B,D2)
       allocate(S203(M2+1:N3,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S203)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,M2,N3,M2,N3,M1,N1,X17,S203,-1.000)
       deallocate(S203)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,M1,N1,t2B,D2)
       allocate(S204(M2+1:N3,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S204)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,N1,M2,M2,N3,M1,N1,X18,S204,-1.000)
       deallocate(S204)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S205(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S205)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,N1,M2,N1,M2,M1,N1,X19,S205,-1.000)
       deallocate(S205)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,N1,M2,N1,M2,M1,N1,X19,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder631245(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,M2,N3,M2,N3,N0,M1,N0,M1,t3A,F2)
       allocate(Z19(M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K6*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X19,F2,Z19)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z19,-1.000)
       deallocate(Z19)
       deallocate(X19)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,M1,N1,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,M1,N1,t2B,D2)
       allocate(Q25(M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7
       I3=K3*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q25)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M1,N1,M1,N1,X9,Q25, 1.000)
       deallocate(Q25)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S206(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S206)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,M2,N3,N0,M1,X23,S206,-1.000)
       deallocate(S206)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S207(M2+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S207)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,M2,N3,M2,N3,N0,M1,X24,S207,-1.000)
       deallocate(S207)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S208(M2+1:N3,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S208)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N1,M2,M2,N3,N0,M1,X25,S208,-1.000)
       deallocate(S208)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S209(M2+1:N3,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S209)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,N1,M2,M2,N3,N0,M1,X26,S209,-1.000)
       deallocate(S209)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S210(N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S210)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N1,M2,N1,M2,N0,M1,X27,S210,-1.000)
       deallocate(S210)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,M1,N1,M2,N1,M2,N0,M1,X27,VAHPHP, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder431256(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N1,M2,M2,N3,M2,N3,N0,M1,M1,N1,t3A,F2)
       allocate(Z27(M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K6*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X27,F2,Z27)
       deallocate(F2)
C
       call
     & sum124635(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z27, 1.000)
       call
     & sum125634(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z27,-1.000)
       deallocate(Z27)
       deallocate(X27)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S211(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S211)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,N1,M2,N1,M2,N0,M1,X28,S211,-1.000)
       deallocate(S211)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,N1,M2,N1,M2,N0,M1,X28,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder531246(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,M2,N3,M2,N3,N0,M1,M1,N1,t3A,F2)
       allocate(Z28(M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K6*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X28,F2,Z28)
       deallocate(F2)
C
       call
     & sum124635(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z28,-1.000)
       call
     & sum125634(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z28, 1.000)
       deallocate(Z28)
       deallocate(X28)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,M1,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,M1,t2B,D2)
       allocate(Q26(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K3*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q26)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N0,M1,N0,M1,X10,Q26, 1.000)
       deallocate(Q26)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,M1,N1,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,M1,t2B,D2)
       allocate(Q27(N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5
       I3=K3*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q27)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M1,N1,N0,M1,X11,Q27, 1.000)
       deallocate(Q27)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,M2,N3,t2B,D2)
       allocate(Q28(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K4*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q28)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M2,N3,M2,N3,X12,Q28,-1.000)
       deallocate(Q28)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,M2,N3,t2B,D2)
       allocate(Q29(M2+1:N3,N1+1:M2))
       I1=K9
       I2=K6
       I3=K4*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q29)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N1,M2,M2,N3,X13,Q29,-1.000)
       deallocate(Q29)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,M2,t2B,D2)
       allocate(Q30(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K4*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q30)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N1,M2,N1,M2,X14,Q30,-1.000)
       deallocate(Q30)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,M1,N1,t2B,D2)
       allocate(S212(M2+1:N3,M1+1:N1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S212)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X33(M1+1:N2,M2+1:N3,M2+1:N3,M1+1:N1))
       X33=0.0d0
       call
     & sum3412(M1,N2,M2,N3,M2,N3,M1,N1,X33,S212, 1.000)
       deallocate(S212)
C
       call sumx1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N2,M2,N3,M2,N3,M1,N1,X33,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder412356(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,t3B4,F2)
       allocate(Z33(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K5*K5*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X33,F2,Z33)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z33, 1.000)
       call
     & sum134526(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z33,-1.000)
       deallocate(Z33)
       deallocate(X33)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,M1,N1,t2B,D2)
       allocate(S213(M2+1:N3,M1+1:N1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S213)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X34(M1+1:N2,N2+1:M2,M2+1:N3,M1+1:N1))
       X34=0.0d0
       call
     & sum3412(M1,N2,N2,M2,M2,N3,M1,N1,X34,S213, 1.000)
       deallocate(S213)
C
       call sumx1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N2,N2,M2,M2,N3,M1,N1,X34,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder412356(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,N2,M2,M2,N3,N1,M2,N0,M1,N0,M1,t3B4,F2)
       allocate(Z34(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K5*K5*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X34,F2,Z34)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z34, 1.000)
       call
     & sum134526(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z34,-1.000)
       deallocate(Z34)
       deallocate(X34)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S214(N1+1:M2,M1+1:N1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S214)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,N2,M2,N1,M2,M1,N1,X48,S214, 1.000)
       deallocate(S214)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S215(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S215)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X35(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:M1))
       X35=0.0d0
       call
     & sum3412(N0,M1,M2,N3,M2,N3,N0,M1,X35,S215, 1.000)
       deallocate(S215)
C
       call sumx1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,M1,X35,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z36(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K5*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X35,F2,Z36)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z36,-1.000)
       call
     & sum134625(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z36, 1.000)
       call
     & sum235614(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z36, 1.000)
       call
     & sum135624(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z36,-1.000)
       deallocate(Z36)
       deallocate(X35)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S216(M2+1:N3,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S216)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X36(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:M1))
       X36=0.0d0
       call
     & sum3412(M1,N2,M2,N3,M2,N3,N0,M1,X36,S216, 1.000)
       deallocate(S216)
C
       call sumx1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N2,M2,N3,M2,N3,N0,M1,X36,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z37(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K5*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X36,F2,Z37)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z37,-1.000)
       call
     & sum134625(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z37, 1.000)
       call
     & sum235614(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z37, 1.000)
       call
     & sum135624(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z37,-1.000)
       deallocate(Z37)
       deallocate(X36)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S217(M2+1:N3,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S217)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X37(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:M1))
       X37=0.0d0
       call
     & sum3412(N0,M1,N2,M2,M2,N3,N0,M1,X37,S217, 1.000)
       deallocate(S217)
C
       call sumx1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,N2,M2,M2,N3,N0,M1,X37,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z38(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K5*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,X37,F2,Z38)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z38,-1.000)
       call
     & sum134625(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z38, 1.000)
       call
     & sum235614(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z38, 1.000)
       call
     & sum135624(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z38,-1.000)
       deallocate(Z38)
       deallocate(X37)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S218(M2+1:N3,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S218)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X38(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:M1))
       X38=0.0d0
       call
     & sum3412(M1,N2,N2,M2,M2,N3,N0,M1,X38,S218, 1.000)
       deallocate(S218)
C
       call sumx1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N2,N2,M2,M2,N3,N0,M1,X38,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z39(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K5*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X38,F2,Z39)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z39,-1.000)
       call
     & sum134625(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z39, 1.000)
       call
     & sum235614(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z39, 1.000)
       call
     & sum135624(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z39,-1.000)
       deallocate(Z39)
       deallocate(X38)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S219(N1+1:M2,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S219)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N2,M2,N1,M2,N0,M1,X53,S219, 1.000)
       deallocate(S219)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S220(N1+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S220)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,N2,M2,N1,M2,N0,M1,X54,S220, 1.000)
       deallocate(S220)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,M1+1:N1))
       call reorder1342(N1,N3,N0,N1,N0,N1,N0,N1,
     & N1,N3,N0,N1,N0,N1,M1,N1,VAHHHP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S221(N0+1:M1,N0+1:N1,N0+1:N1,M1+1:N1))
       I1=K7*K1*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S221)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder3214(N0,M1,N0,N1,N0,N1,M1,N1,
     & N0,N1,N0,N1,N0,M1,M1,N1,S221,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S223(N1+1:M2,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S223)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N1,M2,N0,M1,M1,N1,X2,S223, 1.000)
       deallocate(S223)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder2314(N0,M1,N0,N1,N0,N1,M1,N1,
     & N0,N1,N0,N1,N0,M1,M1,N1,S221,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S222(M2+1:N3,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S222)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,M2,N3,N0,M1,M1,N1,X1,S222,-1.000)
       deallocate(S222)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M1+1:N1))
       call reorder4312(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,M1,N1,VAHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S224(M2+1:N3,N0+1:N1,N1+1:N3,M1+1:N1))
       I1=K7*K3*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S224)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder2314(M2,N3,N0,N1,N1,N3,M1,N1,
     & N0,N1,N1,N3,M2,N3,M1,N1,S224,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S226(N1+1:M2,N1+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S226)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,N3,M2,N3,N1,M2,M1,N1,X4,S226,-1.000)
       deallocate(S226)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder2314(M2,N3,N0,N1,N1,N3,M1,N1,
     & N0,N1,N1,N3,M2,N3,M1,N1,S224,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S225(M2+1:N3,N1+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S225)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,M2,N3,M1,N1,X3,S225, 1.000)
       deallocate(S225)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:M1))
       call reorder1342(N1,N3,N0,N1,N0,N1,N0,N1,
     & N1,N3,N0,N1,N0,N1,N0,M1,VAHHHP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S227(M1+1:N1,N0+1:N1,N0+1:N1,N0+1:M1))
       I1=K5*K1*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S227)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder3241(M1,N1,N0,N1,N0,N1,N0,M1,
     & N0,N1,N0,N1,N0,M1,M1,N1,S227,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S229(N1+1:M2,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S229)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N1,M2,N0,M1,M1,N1,X2,S229,-1.000)
       deallocate(S229)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder2341(M1,N1,N0,N1,N0,N1,N0,M1,
     & N0,N1,N0,N1,N0,M1,M1,N1,S227,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S228(M2+1:N3,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S228)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,M2,N3,N0,M1,M1,N1,X1,S228, 1.000)
       deallocate(S228)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder2413(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N0,N1,N1,N3,M2,N3,VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S230(M1+1:N1,N0+1:N1,N1+1:N3,M2+1:N3))
       I1=K6*K3*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S230)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder2341(M1,N1,N0,N1,N1,N3,M2,N3,
     & N0,N1,N1,N3,M2,N3,M1,N1,S230,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S234(M2+1:N3,N1+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S234)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,N3,M2,N3,M2,N3,M1,N1,X39,S234, 1.000)
       deallocate(S234)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder2341(M1,N1,N0,N1,N1,N3,M2,N3,
     & N0,N1,N1,N3,M2,N3,M1,N1,S230,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S235(N1+1:M2,N1+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S235)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,N3,M2,N3,N1,M2,M1,N1,X4,S235,-1.000)
       deallocate(S235)
C
       allocate(D1(N1+1:N3,N0+1:N1,M2+1:N3,M1+1:N1))
       call reorder3241(M1,N1,N0,N1,N1,N3,M2,N3,
     & N1,N3,N0,N1,M2,N3,M1,N1,S230,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S231(N0+1:M1,N0+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S231)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N1,M2,N3,N0,M1,M1,N1,X1,S231, 1.000)
       deallocate(S231)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder2413(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N0,N1,N1,N3,N1,M2,VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S232(M1+1:N1,N0+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S232)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder2341(M1,N1,N0,N1,N1,N3,N1,M2,
     & N0,N1,N1,N3,N1,M2,M1,N1,S232,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S236(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S236)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,N1,M2,M1,N1,X4,S236, 1.000)
       deallocate(S236)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:M2,M1+1:N1))
       call reorder3241(M1,N1,N0,N1,N1,N3,N1,M2,
     & N1,N3,N0,N1,N1,M2,M1,N1,S232,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S233(N0+1:M1,N0+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S233)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N1,N1,M2,N0,M1,M1,N1,X2,S233, 1.000)
       deallocate(S233)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:M1))
       call reorder1342(N1,N3,N0,N1,N0,N1,N0,N1,
     & N1,N3,N0,N1,N0,N1,N0,M1,VAHHHP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S237(N0+1:M1,N0+1:N1,N0+1:N1,N0+1:M1))
       I1=K5*K1*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S237)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,N0+1:M1))
       call reorder3214(N0,M1,N0,N1,N0,N1,N0,M1,
     & N0,N1,N0,N1,N0,M1,N0,M1,S237,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S239(N1+1:M2,N0+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S239)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N1,M2,N0,M1,N0,M1,X41,S239, 1.000)
       deallocate(S239)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,N0+1:M1))
       call reorder2314(N0,M1,N0,N1,N0,N1,N0,M1,
     & N0,N1,N0,N1,N0,M1,N0,M1,S237,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S238(M2+1:N3,N0+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S238)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,M2,N3,N0,M1,N0,M1,X40,S238,-1.000)
       deallocate(S238)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:M1))
       call reorder4312(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N0,M1,VAHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S240(M2+1:N3,N0+1:N1,N1+1:N3,N0+1:M1))
       I1=K5*K3*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S240)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder2314(M2,N3,N0,N1,N1,N3,N0,M1,
     & N0,N1,N1,N3,M2,N3,N0,M1,S240,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S242(N1+1:M2,N1+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S242)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,N3,M2,N3,N1,M2,N0,M1,X8,S242,-1.000)
       deallocate(S242)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder2314(M2,N3,N0,N1,N1,N3,N0,M1,
     & N0,N1,N1,N3,M2,N3,N0,M1,S240,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S241(M2+1:N3,N1+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S241)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,M2,N3,N0,M1,X7,S241, 1.000)
       deallocate(S241)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder2413(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N0,N1,N1,N3,M2,N3,VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S243(N0+1:M1,N0+1:N1,N1+1:N3,M2+1:N3))
       I1=K6*K3*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S243)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N1,N1,N3,M2,N3,
     & N0,N1,N1,N3,M2,N3,N0,M1,S243,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S247(M2+1:N3,N1+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S247)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,N3,M2,N3,M2,N3,N0,M1,X42,S247, 1.000)
       deallocate(S247)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N1,N1,N3,M2,N3,
     & N0,N1,N1,N3,M2,N3,N0,M1,S243,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S248(N1+1:M2,N1+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S248)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,N3,M2,N3,N1,M2,N0,M1,X8,S248,-1.000)
       deallocate(S248)
C
       allocate(D1(N1+1:N3,N0+1:N1,M2+1:N3,N0+1:M1))
       call reorder3241(N0,M1,N0,N1,N1,N3,M2,N3,
     & N1,N3,N0,N1,M2,N3,N0,M1,S243,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S244(N0+1:M1,N0+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S244)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N1,M2,N3,N0,M1,N0,M1,X5,S244, 1.000)
       deallocate(S244)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder2413(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N0,N1,N1,N3,N1,M2,VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S245(N0+1:M1,N0+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S245)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N1,N1,N3,N1,M2,
     & N0,N1,N1,N3,N1,M2,N0,M1,S245,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S249(M2+1:N3,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S249)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,N1,M2,N0,M1,X8,S249, 1.000)
       deallocate(S249)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:M2,N0+1:M1))
       call reorder3241(N0,M1,N0,N1,N1,N3,N1,M2,
     & N1,N3,N0,N1,N1,M2,N0,M1,S245,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S246(N0+1:M1,N0+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S246)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N1,N1,M2,N0,M1,N0,M1,X6,S246, 1.000)
       deallocate(S246)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,N1+1:N3))
       call reorder2341(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,M1,M1,N1,N1,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S250(M1+1:N1,N0+1:M1,M1+1:N1,N1+1:N3))
       I1=K3*K7*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S250)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,M1+1:N1))
       call reorder4231(M1,N1,N0,M1,M1,N1,N1,N3,
     & N1,N3,N0,M1,M1,N1,M1,N1,S250,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S251(N0+1:M1,N0+1:M1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S251)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M1,N1,N0,M1,M1,N1,X15,S251, 1.000)
       deallocate(S251)
C
       call sumx3412(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,M1,M1,N1,N0,M1,M1,N1,X15,VAHHHH, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder461235(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,M2,N3,N1,M2,N0,M1,t3A,F2)
       allocate(Z15(M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       I1=K7*K5
       I2=K5*K9*K6*K6
       I3=K7*K5
       call EGEMM(I1,I2,I3,X15,F2,Z15)
       deallocate(F2)
C
       V3A=V3A-Z15
       call
     & sum123546(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z15, 1.000)
       deallocate(Z15)
       deallocate(X15)
C
       allocate(D1(N1+1:N3,M1+1:N1,M1+1:N1,N1+1:N3))
       call reorder2341(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,M1,N1,M1,N1,N1,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S252(M1+1:N1,M1+1:N1,M1+1:N1,N1+1:N3))
       I1=K3*K7*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S252)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N1+1:N3,M1+1:N1,M1+1:N1,M1+1:N1))
       call reorder4231(M1,N1,M1,N1,M1,N1,N1,N3,
     & N1,N3,M1,N1,M1,N1,M1,N1,S252,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S253(N0+1:M1,M1+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S253)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,M1,N1,N0,M1,M1,N1,X16,S253, 1.000)
       deallocate(S253)
C
       call sumx3412(N0,N1,N0,N1,N0,N1,N0,N1,
     & M1,N1,M1,N1,N0,M1,M1,N1,X16,VAHHHH, 1.000)
C
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder561234(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M1,N1,M2,N3,M2,N3,N1,M2,N0,M1,t3A,F2)
       allocate(Z16(M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       I1=K7*K5
       I2=K5*K9*K6*K6
       I3=K7*K7
       call EGEMM(I1,I2,I3,X16,F2,Z16)
       deallocate(F2)
C
       V3A=V3A+0.500*Z16
       call
     & sum123546(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z16,-0.500)
       deallocate(Z16)
       deallocate(X16)
C
       allocate(D1(N1+1:N3,N0+1:N1,M1+1:N1,M2+1:N3))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,N1,M1,N1,M2,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S254(M1+1:N1,N0+1:N1,M1+1:N1,M2+1:N3))
       I1=K6*K7*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S254)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder2341(M1,N1,N0,N1,M1,N1,M2,N3,
     & N0,N1,M1,N1,M2,N3,M1,N1,S254,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S255(M2+1:N3,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K7
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S255)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,M2,N3,M2,N3,M1,N1,X17,S255, 1.000)
       deallocate(S255)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,M2,N3,M2,N3,M1,N1,X17,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder612345(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,t3A,F2)
       allocate(Z17(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K5*K5*K9*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X17,F2,Z17)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z17,-1.000)
       call
     & sum134526(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z17, 1.000)
       deallocate(Z17)
       deallocate(X17)
C
       allocate(D1(N1+1:N3,N0+1:N1,M1+1:N1,N1+1:M2))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,N1,M1,N1,N1,M2,VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S256(M1+1:N1,N0+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S256)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       call reorder2341(M1,N1,N0,N1,M1,N1,N1,M2,
     & N0,N1,M1,N1,N1,M2,M1,N1,S256,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S257(M2+1:N3,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K7
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S257)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,N1,M2,M2,N3,M1,N1,X18,S257, 1.000)
       deallocate(S257)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,N1,M2,M2,N3,M1,N1,X18,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder621345(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,M2,N3,N1,M2,N0,M1,N0,M1,t3A,F2)
       allocate(Z18(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K5*K5*K9*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X18,F2,Z18)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z18, 1.000)
       call
     & sum134526(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z18,-1.000)
       deallocate(Z18)
       deallocate(X18)
C
       allocate(D1(N1+1:N3,M1+1:N1,N0+1:N1,N1+1:M2))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,M1,N1,N0,N1,N1,M2,VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S258(M1+1:N1,M1+1:N1,N0+1:N1,N1+1:M2))
       I1=K9*K1*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S258)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       call reorder3241(M1,N1,M1,N1,N0,N1,N1,M2,
     & N0,N1,M1,N1,N1,M2,M1,N1,S258,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S259(N1+1:M2,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S259)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,N1,M2,N1,M2,M1,N1,X43,S259,-1.000)
       deallocate(S259)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder631245(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,M2,N3,M2,N3,N0,M1,N0,M1,t3A,F2)
       allocate(Z70(M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K6*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X43,F2,Z70)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z70,-1.000)
       deallocate(Z70)
       deallocate(X43)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,N1,N3,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q31(M1+1:N1,N1+1:N3))
       I1=K3*K7
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q31)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,M1+1:N1))
       call reorder21(M1,N1,N1,N3,
     & N1,N3,M1,N1,Q31,B1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(Q35(N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q35)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(M1,N1,N0,M1,X11,Q35, 1.000)
       deallocate(Q35)
C
       allocate(B1(N1+1:N3,M1+1:N1))
       call reorder21(M1,N1,N1,N3,
     & N1,N3,M1,N1,Q31,B1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(Q32(M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q32)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(M1,N1,M1,N1,X9,Q32, 1.000)
       deallocate(Q32)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,N1+1:N3))
       call reorder2341(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,M1,M1,N1,N1,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S262(N0+1:M1,N0+1:M1,M1+1:N1,N1+1:N3))
       I1=K3*K7*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S262)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,N0+1:M1))
       call reorder4231(N0,M1,N0,M1,M1,N1,N1,N3,
     & N1,N3,N0,M1,M1,N1,N0,M1,S262,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S263(N0+1:M1,N0+1:M1,M1+1:N1,N0+1:M1))
       I1=K5*K7*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S263)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M1,N1,N0,M1,N0,M1,X21,S263, 1.000)
       deallocate(S263)
C
       call sumx3412(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,M1,M1,N1,N0,M1,N0,M1,X21,VAHHHH, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder451236(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,M2,N3,N1,M2,M1,N1,t3A,F2)
       allocate(Z21(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K7*K9*K6*K6
       I3=K7*K5
       call EGEMM(I1,I2,I3,X21,F2,Z21)
       deallocate(F2)
C
       call
     & sum123645(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z21, 1.000)
       deallocate(Z21)
       deallocate(X21)
C
       allocate(D1(N1+1:N3,M1+1:N1,M1+1:N1,N1+1:N3))
       call reorder2341(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,M1,N1,M1,N1,N1,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S264(N0+1:M1,M1+1:N1,M1+1:N1,N1+1:N3))
       I1=K3*K7*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S264)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N1+1:N3,M1+1:N1,M1+1:N1,N0+1:M1))
       call reorder4231(N0,M1,M1,N1,M1,N1,N1,N3,
     & N1,N3,M1,N1,M1,N1,N0,M1,S264,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S265(N0+1:M1,M1+1:N1,M1+1:N1,N0+1:M1))
       I1=K5*K7*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S265)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,M1,N1,N0,M1,N0,M1,X22,S265, 1.000)
       deallocate(S265)
C
       call sumx3412(N0,N1,N0,N1,N0,N1,N0,N1,
     & M1,N1,M1,N1,N0,M1,N0,M1,X22,VAHHHH, 1.000)
C
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder451236(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M1,N1,M2,N3,M2,N3,N1,M2,M1,N1,t3A,F2)
       allocate(Z22(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K7*K9*K6*K6
       I3=K7*K7
       call EGEMM(I1,I2,I3,X22,F2,Z22)
       deallocate(F2)
C
       call
     & sum123645(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z22, 0.500)
       deallocate(Z22)
       deallocate(X22)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:M1,M2+1:N3))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,N1,N0,M1,M2,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S266(N0+1:M1,N0+1:N1,N0+1:M1,M2+1:N3))
       I1=K6*K5*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S266)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N1,N0,M1,M2,N3,
     & N0,N1,N0,M1,M2,N3,N0,M1,S266,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S267(M2+1:N3,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S267)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,M2,N3,N0,M1,X23,S267, 1.000)
       deallocate(S267)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,M1,X23,VAHPHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z23(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K5*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X23,F2,Z23)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z23, 1.000)
       call
     & sum134625(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z23,-1.000)
       call
     & sum235614(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z23,-1.000)
       call
     & sum135624(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z23, 1.000)
       deallocate(Z23)
       deallocate(X23)
C
       allocate(D1(N1+1:N3,N0+1:N1,M1+1:N1,M2+1:N3))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,N1,M1,N1,M2,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S268(N0+1:M1,N0+1:N1,M1+1:N1,M2+1:N3))
       I1=K6*K7*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S268)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N1,M1,N1,M2,N3,
     & N0,N1,M1,N1,M2,N3,N0,M1,S268,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S269(M2+1:N3,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S269)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,M2,N3,M2,N3,N0,M1,X24,S269, 1.000)
       deallocate(S269)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,M2,N3,M2,N3,N0,M1,X24,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder512346(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z24(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K5*K9*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X24,F2,Z24)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z24,-1.000)
       call
     & sum134625(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z24, 1.000)
       call
     & sum235614(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z24, 1.000)
       call
     & sum135624(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z24,-1.000)
       deallocate(Z24)
       deallocate(X24)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:M1,N1+1:M2))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,N1,N0,M1,N1,M2,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S270(N0+1:M1,N0+1:N1,N0+1:M1,N1+1:M2))
       I1=K9*K5*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S270)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N1,N0,M1,N1,M2,
     & N0,N1,N0,M1,N1,M2,N0,M1,S270,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S271(M2+1:N3,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S271)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N1,M2,M2,N3,N0,M1,X25,S271, 1.000)
       deallocate(S271)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,M1,N1,M2,M2,N3,N0,M1,X25,VAHPHP, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder421356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N1,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z25(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K5*K9*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X25,F2,Z25)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z25,-1.000)
       call
     & sum134625(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z25, 1.000)
       call
     & sum235614(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z25, 1.000)
       call
     & sum135624(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z25,-1.000)
       deallocate(Z25)
       deallocate(X25)
C
       allocate(D1(N1+1:N3,N0+1:N1,M1+1:N1,N1+1:M2))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,N1,M1,N1,N1,M2,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S272(N0+1:M1,N0+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S272)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N1,M1,N1,N1,M2,
     & N0,N1,M1,N1,N1,M2,N0,M1,S272,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S273(M2+1:N3,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S273)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,N1,M2,M2,N3,N0,M1,X26,S273, 1.000)
       deallocate(S273)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,N1,M2,M2,N3,N0,M1,X26,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder521346(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z26(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K5*K9*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X26,F2,Z26)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z26, 1.000)
       call
     & sum134625(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z26,-1.000)
       call
     & sum235614(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z26,-1.000)
       call
     & sum135624(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z26, 1.000)
       deallocate(Z26)
       deallocate(X26)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:N1,N1+1:M2))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,M1,N0,N1,N1,M2,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S274(N0+1:M1,N0+1:M1,N0+1:N1,N1+1:M2))
       I1=K9*K1*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S274)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder3241(N0,M1,N0,M1,N0,N1,N1,M2,
     & N0,N1,N0,M1,N1,M2,N0,M1,S274,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S275(N1+1:M2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S275)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N1,M2,N1,M2,N0,M1,X44,S275,-1.000)
       deallocate(S275)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder431256(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N1,M2,M2,N3,M2,N3,N0,M1,M1,N1,t3A,F2)
       allocate(Z84(M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K6*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X44,F2,Z84)
       deallocate(F2)
C
       call
     & sum124635(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z84, 1.000)
       call
     & sum125634(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z84,-1.000)
       deallocate(Z84)
       deallocate(X44)
C
       allocate(D1(N1+1:N3,M1+1:N1,N0+1:N1,N1+1:M2))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,M1,N1,N0,N1,N1,M2,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S276(N0+1:M1,M1+1:N1,N0+1:N1,N1+1:M2))
       I1=K9*K1*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S276)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder3241(N0,M1,M1,N1,N0,N1,N1,M2,
     & N0,N1,M1,N1,N1,M2,N0,M1,S276,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S277(N1+1:M2,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S277)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,N1,M2,N1,M2,N0,M1,X45,S277,-1.000)
       deallocate(S277)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder531246(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,M2,N3,M2,N3,N0,M1,M1,N1,t3A,F2)
       allocate(Z85(M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K6*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X45,F2,Z85)
       deallocate(F2)
C
       call
     & sum124635(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z85,-1.000)
       call
     & sum125634(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z85, 1.000)
       deallocate(Z85)
       deallocate(X45)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,N1,N3,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q33(N0+1:M1,N1+1:N3))
       I1=K3*K5
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q33)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,N0+1:M1))
       call reorder21(N0,M1,N1,N3,
     & N1,N3,N0,M1,Q33,B1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(Q34(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q34)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(N0,M1,N0,M1,X10,Q34, 1.000)
       deallocate(Q34)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:M1,N1+1:N3))
       call reorder2341(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,M1,N0,M1,N1,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S260(N0+1:M1,N0+1:M1,N0+1:M1,N1+1:N3))
       I1=K3*K5*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S260)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:M1,N0+1:M1))
       call reorder4231(N0,M1,N0,M1,N0,M1,N1,N3,
     & N1,N3,N0,M1,N0,M1,N0,M1,S260,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S261(N0+1:M1,N0+1:M1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S261)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N0,M1,N0,M1,N0,M1,X20,S261, 1.000)
       deallocate(S261)
C
       call sumx3412(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,M1,N0,M1,N0,M1,N0,M1,X20,VAHHHH, 1.000)
C
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder451236(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N0,M1,M2,N3,M2,N3,N1,M2,M1,N1,t3A,F2)
       allocate(Z20(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K7*K9*K6*K6
       I3=K5*K5
       call EGEMM(I1,I2,I3,X20,F2,Z20)
       deallocate(F2)
C
       call
     & sum123645(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z20, 0.500)
       deallocate(Z20)
       deallocate(X20)
C
       allocate(D1(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,M2,N3,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,M2+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,M2,N3,M2,N3,t2A,D2)
       allocate(S148(M2+1:N3,M2+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K6*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S148)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X29(M2+1:N3,N1+1:M2,M2+1:N3,M2+1:N3))
       X29=0.0d0
       call
     & sum3412(M2,N3,N1,M2,M2,N3,M2,N3,X29,S148, 0.500)
       deallocate(S148)
C
       allocate(D1(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder4312(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,M2,N3,N1,M2,VAHHPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S278(M2+1:N3,N0+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S278)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M2+1:N3,N1+1:M2,M2+1:N3))
       call reorder2341(M2,N3,N0,N1,M2,N3,N1,M2,
     & N0,N1,M2,N3,N1,M2,M2,N3,S278,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S282(N1+1:M2,M2+1:N3,N1+1:M2,M2+1:N3))
       I1=K6*K9*K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S282)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M2,N3,N1,M2,M2,N3,N1,M2,X31,S282,-1.000)
       deallocate(S282)
C
       call sumx1234(N1,N3,N1,N3,N1,N3,N1,M2,
     & M2,N3,N1,M2,M2,N3,N1,M2,X31,VAAPPP, 1.000)
C
       allocate(F2(M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder132456(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M2,N3,N1,M2,M2,N3,N0,M1,N0,M1,M1,N1,t3A,F2)
       allocate(Z31(M2+1:N3,N0+1:M1,N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K7*K5*K5*K6
       I3=K9*K6
       call EGEMM(I1,I2,I3,X31,F2,Z31)
       deallocate(F2)
C
       call
     & sum245613(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z31, 1.000)
       call
     & sum145623(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z31,-1.000)
       deallocate(Z31)
       deallocate(X31)
C
       allocate(D1(N0+1:N1,M2+1:N3,N1+1:M2,M2+1:N3))
       call reorder2341(M2,N3,N0,N1,M2,N3,N1,M2,
     & N0,N1,M2,N3,N1,M2,M2,N3,S278,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S279(M2+1:N3,M2+1:N3,N1+1:M2,M2+1:N3))
       I1=K6*K9*K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S279)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,N1,M2,M2,N3,M2,N3,X29,S279, 1.000)
       deallocate(S279)
C
       call sumx3412(N1,N3,N1,N3,N1,N3,N1,M2,
     & M2,N3,N1,M2,M2,N3,M2,N3,X29,VAAPPP, 1.000)
C
       allocate(F2(M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder123456(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M2,N3,N1,M2,N1,M2,N0,M1,N0,M1,M1,N1,t3A,F2)
       allocate(Z29(N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3))
       I1=K6*K6
       I2=K7*K5*K5*K9
       I3=K9*K6
       call EGEMM(I1,I2,I3,X29,F2,Z29)
       deallocate(F2)
C
       call
     & sum345612(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z29, 1.000)
       deallocate(Z29)
       deallocate(X29)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:M2,N1+1:M2))
       call reorder4312(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,M2,N1,M2,VAHHPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S280(M2+1:N3,N0+1:N1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S280)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:M2,N1+1:M2,M2+1:N3))
       call reorder2341(M2,N3,N0,N1,N1,M2,N1,M2,
     & N0,N1,N1,M2,N1,M2,M2,N3,S280,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S283(N1+1:M2,N1+1:M2,N1+1:M2,M2+1:N3))
       I1=K6*K9*K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S283)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N1,M2,N1,M2,M2,N3,N1,M2,X32,S283,-1.000)
       deallocate(S283)
C
       call sumx1234(N1,N3,N1,N3,N1,N3,N1,M2,
     & N1,M2,N1,M2,M2,N3,N1,M2,X32,VAAPPP, 1.000)
C
       allocate(F2(N1+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder231456(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N1,M2,N1,M2,M2,N3,N0,M1,N0,M1,M1,N1,t3A,F2)
       allocate(Z32(M2+1:N3,N0+1:M1,N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K7*K5*K5*K6
       I3=K9*K9
       call EGEMM(I1,I2,I3,X32,F2,Z32)
       deallocate(F2)
C
       call
     & sum245613(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z32,-0.500)
       call
     & sum145623(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z32, 0.500)
       deallocate(Z32)
       deallocate(X32)
C
       allocate(D1(N0+1:N1,N1+1:M2,N1+1:M2,M2+1:N3))
       call reorder2341(M2,N3,N0,N1,N1,M2,N1,M2,
     & N0,N1,N1,M2,N1,M2,M2,N3,S280,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S281(M2+1:N3,N1+1:M2,N1+1:M2,M2+1:N3))
       I1=K6*K9*K9
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S281)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,M2,N1,M2,M2,N3,M2,N3,X30,S281, 1.000)
       deallocate(S281)
C
       call sumx3412(N1,N3,N1,N3,N1,N3,N1,M2,
     & N1,M2,N1,M2,M2,N3,M2,N3,X30,VAAPPP, 1.000)
C
       allocate(F2(N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder123456(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N1,M2,N1,M2,N1,M2,N0,M1,N0,M1,M1,N1,t3A,F2)
       allocate(Z30(N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3))
       I1=K6*K6
       I2=K7*K5*K5*K9
       I3=K9*K9
       call EGEMM(I1,I2,I3,X30,F2,Z30)
       deallocate(F2)
C
       call
     & sum345612(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z30, 0.500)
       deallocate(Z30)
       deallocate(X30)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M2+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,M2,N3,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q36(N0+1:N1,M2+1:N3))
       I1=K6*K1
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q36)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(Q37(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,Q36,B2,Q37)
       deallocate(B2)
C
       call
     & sum21(M2,N3,M2,N3,X12,Q37,-1.000)
       deallocate(Q37)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:M2))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N1,M2,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q38(N0+1:N1,N1+1:M2))
       I1=K9*K1
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q38)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(Q40(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,Q38,B2,Q40)
       deallocate(B2)
C
       call
     & sum21(N1,M2,N1,M2,X14,Q40,-1.000)
       deallocate(Q40)
C
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(Q39(M2+1:N3,N1+1:M2))
       I1=K9
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,Q38,B2,Q39)
       deallocate(B2)
C
       call
     & sum21(N1,M2,M2,N3,X13,Q39,-1.000)
       deallocate(Q39)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:N1,M2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,N0,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S284(M1+1:N1,M1+1:N2,N0+1:N1,M2+1:N3))
       I1=K6*K1*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S284)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,M1+1:N1))
       call reorder3241(M1,N1,M1,N2,N0,N1,M2,N3,
     & N0,N1,M1,N2,M2,N3,M1,N1,S284,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S285(M2+1:N3,M1+1:N2,M2+1:N3,M1+1:N1))
       I1=K7*K6*K8
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S285)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M2,N3,M2,N3,M1,N1,X46,S285,-1.000)
       deallocate(S285)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder412356(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,t3B4,F2)
       allocate(Z237(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K5*K5*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X46,F2,Z237)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z237, 1.000)
       call
     & sum134526(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z237,-1.000)
       deallocate(Z237)
       deallocate(X46)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:N1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,N0,N1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S286(M1+1:N1,M1+1:N2,N0+1:N1,N2+1:M2))
       I1=K0*K1*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S286)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,M1+1:N1))
       call reorder3241(M1,N1,M1,N2,N0,N1,N2,M2,
     & N0,N1,M1,N2,N2,M2,M1,N1,S286,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S288(N1+1:M2,M1+1:N2,N2+1:M2,M1+1:N1))
       I1=K7*K0*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S288)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,N2,M2,N1,M2,M1,N1,X48,S288,-1.000)
       deallocate(S288)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder412356(N2,M2,M2,N3,M2,N3,M1,N2,N0,M1,N0,M1,
     & M1,N2,N2,M2,M2,N3,M2,N3,N0,M1,N0,M1,t3B2,F2)
       allocate(Z239(M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K6*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X48,F2,Z239)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z239, 1.000)
       deallocate(Z239)
       deallocate(X48)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,M1+1:N1))
       call reorder3241(M1,N1,M1,N2,N0,N1,N2,M2,
     & N0,N1,M1,N2,N2,M2,M1,N1,S286,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S287(M2+1:N3,M1+1:N2,N2+1:M2,M1+1:N1))
       I1=K7*K0*K8
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S287)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,N2,M2,M2,N3,M1,N1,X47,S287,-1.000)
       deallocate(S287)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder412356(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,N2,M2,M2,N3,N1,M2,N0,M1,N0,M1,t3B4,F2)
       allocate(Z238(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K5*K5*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X47,F2,Z238)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z238, 1.000)
       call
     & sum134526(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z238,-1.000)
       deallocate(Z238)
       deallocate(X47)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:N1,M2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,N0,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S289(N0+1:M1,N0+1:M1,N0+1:N1,M2+1:N3))
       I1=K6*K1*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S289)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder3241(N0,M1,N0,M1,N0,N1,M2,N3,
     & N0,N1,N0,M1,M2,N3,N0,M1,S289,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S290(M2+1:N3,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S290)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,M2,N3,N0,M1,X49,S290,-1.000)
       deallocate(S290)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z266(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K5*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X49,F2,Z266)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z266,-1.000)
       call
     & sum134625(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z266, 1.000)
       call
     & sum235614(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z266, 1.000)
       call
     & sum135624(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z266,-1.000)
       deallocate(Z266)
       deallocate(X49)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:N1,M2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,N0,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S291(N0+1:M1,M1+1:N2,N0+1:N1,M2+1:N3))
       I1=K6*K1*K8
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S291)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder3241(N0,M1,M1,N2,N0,N1,M2,N3,
     & N0,N1,M1,N2,M2,N3,N0,M1,S291,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S292(M2+1:N3,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S292)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M2,N3,M2,N3,N0,M1,X50,S292,-1.000)
       deallocate(S292)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z267(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K5*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X50,F2,Z267)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z267,-1.000)
       call
     & sum134625(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z267, 1.000)
       call
     & sum235614(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z267, 1.000)
       call
     & sum135624(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z267,-1.000)
       deallocate(Z267)
       deallocate(X50)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:N1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,N0,N1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S293(N0+1:M1,N0+1:M1,N0+1:N1,N2+1:M2))
       I1=K0*K1*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S293)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder3241(N0,M1,N0,M1,N0,N1,N2,M2,
     & N0,N1,N0,M1,N2,M2,N0,M1,S293,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S297(N1+1:M2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S297)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N2,M2,N1,M2,N0,M1,X53,S297,-1.000)
       deallocate(S297)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder412356(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,M2,N3,N0,M1,M1,N1,t3B3,F2)
       allocate(Z270(M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K6*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,X53,F2,Z270)
       deallocate(F2)
C
       call
     & sum124635(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z270,-1.000)
       call
     & sum125634(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z270, 1.000)
       deallocate(Z270)
       deallocate(X53)
C
       allocate(D1(N0+1:N1,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder3241(N0,M1,N0,M1,N0,N1,N2,M2,
     & N0,N1,N0,M1,N2,M2,N0,M1,S293,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S294(M2+1:N3,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S294)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N2,M2,M2,N3,N0,M1,X51,S294,-1.000)
       deallocate(S294)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z268(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K5*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,X51,F2,Z268)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z268,-1.000)
       call
     & sum134625(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z268, 1.000)
       call
     & sum235614(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z268, 1.000)
       call
     & sum135624(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z268,-1.000)
       deallocate(Z268)
       deallocate(X51)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:N1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,N0,N1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S295(N0+1:M1,M1+1:N2,N0+1:N1,N2+1:M2))
       I1=K0*K1*K8
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S295)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder3241(N0,M1,M1,N2,N0,N1,N2,M2,
     & N0,N1,M1,N2,N2,M2,N0,M1,S295,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S298(N1+1:M2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S298)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,N2,M2,N1,M2,N0,M1,X54,S298,-1.000)
       deallocate(S298)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder412356(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,M2,N3,N0,M1,M1,N1,t3B3,F2)
       allocate(Z271(M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K6*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X54,F2,Z271)
       deallocate(F2)
C
       call
     & sum124635(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z271,-1.000)
       call
     & sum125634(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z271, 1.000)
       deallocate(Z271)
       deallocate(X54)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder3241(N0,M1,M1,N2,N0,N1,N2,M2,
     & N0,N1,M1,N2,N2,M2,N0,M1,S295,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S296(M2+1:N3,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S296)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,N2,M2,M2,N3,N0,M1,X52,S296,-1.000)
       deallocate(S296)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z269(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K5*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X52,F2,Z269)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z269,-1.000)
       call
     & sum134625(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z269, 1.000)
       call
     & sum235614(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z269, 1.000)
       call
     & sum135624(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z269,-1.000)
       deallocate(Z269)
       deallocate(X52)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,N1,N3,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q41(M1+1:N1,N1+1:N3))
       I1=K3*K7
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q41)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,M1+1:N1))
       call reorder21(M1,N1,N1,N3,
     & N1,N3,M1,N1,Q41,B1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(Q45(N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q45)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(M1,N1,N0,M1,X11,Q45, 1.000)
       deallocate(Q45)
C
       call sumx21(N0,N1,N0,N1,
     & M1,N1,N0,M1,X11,FAHH, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder512346(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z11(M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7*K5*K9*K6*K6
       I3=K7
       call EGEMM(I1,I2,I3,X11,F2,Z11)
       deallocate(F2)
C
       call
     & sum123465(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z11,-1.000)
       call
     & sum123564(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z11, 1.000)
       deallocate(Z11)
       deallocate(X11)
C
       allocate(B1(N1+1:N3,M1+1:N1))
       call reorder21(M1,N1,N1,N3,
     & N1,N3,M1,N1,Q41,B1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(Q42(M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q42)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(M1,N1,M1,N1,X9,Q42, 1.000)
       deallocate(Q42)
C
       call sumx21(N0,N1,N0,N1,
     & M1,N1,M1,N1,X9,FAHH, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder612345(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,t3A,F2)
       allocate(Z9(M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5*K5*K9*K6*K6
       I3=K7
       call EGEMM(I1,I2,I3,X9,F2,Z9)
       deallocate(F2)
C
       V3A=V3A-Z9
       deallocate(Z9)
       deallocate(X9)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,N1,N3,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q43(N0+1:M1,N1+1:N3))
       I1=K3*K5
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q43)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,N0+1:M1))
       call reorder21(N0,M1,N1,N3,
     & N1,N3,N0,M1,Q43,B1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(Q44(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q44)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(N0,M1,N0,M1,X10,Q44, 1.000)
       deallocate(Q44)
C
       call sumx21(N0,N1,N0,N1,
     & N0,M1,N0,M1,X10,FAHH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z10(M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7*K5*K9*K6*K6
       I3=K5
       call EGEMM(I1,I2,I3,X10,F2,Z10)
       deallocate(F2)
C
       call
     & sum123465(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z10, 1.000)
       call
     & sum123564(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z10,-1.000)
       deallocate(Z10)
       deallocate(X10)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q46(N0+1:N1,M2+1:N3))
       I1=K6*K1
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q46)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(Q47(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,Q46,B2,Q47)
       deallocate(B2)
C
       call
     & sum21(M2,N3,M2,N3,X12,Q47,-1.000)
       deallocate(Q47)
C
       call sumx12(N1,N3,N1,N3,
     & M2,N3,M2,N3,X12,FAPP, 1.000)
C
       allocate(F2(M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder123456(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,t3A,F2)
       allocate(Z12(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K5*K5*K9*K6
       I3=K6
       call EGEMM(I1,I2,I3,X12,F2,Z12)
       deallocate(F2)
C
       call
     & sum234561(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z12, 1.000)
       call
     & sum134562(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z12,-1.000)
       deallocate(Z12)
       deallocate(X12)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q48(N0+1:N1,N1+1:M2))
       I1=K9*K1
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q48)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(Q50(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,Q48,B2,Q50)
       deallocate(B2)
C
       call
     & sum21(N1,M2,N1,M2,X14,Q50,-1.000)
       deallocate(Q50)
C
       call sumx12(N1,N3,N1,N3,
     & N1,M2,N1,M2,X14,FAPP, 1.000)
C
       allocate(F2(N1+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder312456(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N1,M2,M2,N3,M2,N3,N0,M1,N0,M1,M1,N1,t3A,F2)
       allocate(Z14(M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K5*K5*K6*K6
       I3=K9
       call EGEMM(I1,I2,I3,X14,F2,Z14)
       deallocate(F2)
C
       call
     & sum124563(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z14, 1.000)
       deallocate(Z14)
       deallocate(X14)
C
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(Q49(M2+1:N3,N1+1:M2))
       I1=K9
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,Q48,B2,Q49)
       deallocate(B2)
C
       call
     & sum21(N1,M2,M2,N3,X13,Q49,-1.000)
       deallocate(Q49)
C
       call sumx12(N1,N3,N1,N3,
     & N1,M2,M2,N3,X13,FAPP, 1.000)
C
       allocate(F2(N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder213456(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N1,M2,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,t3A,F2)
       allocate(Z13(M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K5*K5*K9*K6
       I3=K9
       call EGEMM(I1,I2,I3,X13,F2,Z13)
       deallocate(F2)
C
       call
     & sum234561(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z13,-1.000)
       call
     & sum134562(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z13, 1.000)
       deallocate(Z13)
       deallocate(X13)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,N1,N0,N1,N1,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S299(M1+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S299)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M1+1:N1))
       call reorder2341(M1,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N0,N1,N1,N3,M1,N1,S299,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,M2+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,M2,N3,M2,N3,t2A,D2)
       allocate(S301(M2+1:N3,M2+1:N3,N1+1:N3,M1+1:N1))
       I1=K7*K3
       I2=K6*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S301)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N1,N3,M2,N3,M2,N3,M1,N1,X3,S301,-0.500)
       deallocate(S301)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M1+1:N1))
       call reorder3421(M1,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N1,M1,N1,S299,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S302(N1+1:M2,N0+1:M1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S302)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N1,M2,N0,M1,M1,N1,X2,S302, 1.000)
       deallocate(S302)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M1+1:N1))
       call reorder2431(M1,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N1,M1,N1,S299,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2A,D2)
       allocate(S303(M2+1:N3,N0+1:M1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S303)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,M2,N3,N0,M1,M1,N1,X1,S303,-1.000)
       deallocate(S303)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,M1+1:N1))
       call reorder4231(M1,N1,N0,N1,N0,N1,N1,N3,
     & N1,N3,N0,N1,N0,N1,M1,N1,S299,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S353(N0+1:M1,N0+1:N1,N0+1:N1,M1+1:N1))
       I1=K7*K1*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S353)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder2314(N0,M1,N0,N1,N0,N1,M1,N1,
     & N0,N1,N0,N1,N0,M1,M1,N1,S353,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S354(M2+1:N3,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S354)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,M2,N3,N0,M1,M1,N1,X1,S354, 1.000)
       deallocate(S354)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M1+1:N1))
       call reorder3241(M1,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N0,N1,N1,N3,M1,N1,S299,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S356(M2+1:N3,N0+1:N1,N1+1:N3,M1+1:N1))
       I1=K7*K3*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S356)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder2314(M2,N3,N0,N1,N1,N3,M1,N1,
     & N0,N1,N1,N3,M2,N3,M1,N1,S356,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S357(M2+1:N3,N1+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S357)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,M2,N3,M1,N1,X3,S357,-1.000)
       deallocate(S357)
C
       call sumx2314(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,M2,N3,M2,N3,M1,N1,X3,VAHPPP, 1.000)
C
       allocate(D2(N1+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,M2,N0,M1,N0,M1,t2A,D2)
       allocate(Z3(N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K6
       I2=K5*K5*K9
       I3=K3
       call EGEMM(I1,I2,I3,X3,D2,Z3)
       deallocate(D2)
C
       call
     & sum345126(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z3,-1.000)
       deallocate(Z3)
       deallocate(X3)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M1+1:N1))
       call reorder3241(M1,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N0,N1,N1,N3,M1,N1,S299,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S358(N1+1:M2,N0+1:N1,N1+1:N3,M1+1:N1))
       I1=K7*K3*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S358)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder2314(N1,M2,N0,N1,N1,N3,M1,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,S358,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S359(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S359)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,N1,M2,M1,N1,X4,S359,-1.000)
       deallocate(S359)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M1+1:N1))
       call reorder2341(M1,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N0,N1,N1,N3,M1,N1,S299,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,M2,N3,N1,M2,t2A,D2)
       allocate(S300(M2+1:N3,N1+1:M2,N1+1:N3,M1+1:N1))
       I1=K7*K3
       I2=K9*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S300)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N1,N3,M2,N3,N1,M2,M1,N1,X4,S300,-0.500)
       deallocate(S300)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder3214(N0,M1,N0,N1,N0,N1,M1,N1,
     & N0,N1,N0,N1,N0,M1,M1,N1,S353,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S355(N1+1:M2,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S355)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N1,M2,N0,M1,M1,N1,X2,S355,-1.000)
       deallocate(S355)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,N1,N0,N1,N1,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S304(N0+1:M1,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S304)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder3421(N0,M1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N1,N0,M1,S304,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S310(N1+1:M2,M1+1:N1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S310)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N1,N1,M2,N0,M1,M1,N1,X2,S310,-1.000)
       deallocate(S310)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N0,N1,N1,N3,N0,M1,S304,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,M2,N3,N1,M2,t2A,D2)
       allocate(S305(M2+1:N3,N1+1:M2,N1+1:N3,N0+1:M1))
       I1=K5*K3
       I2=K9*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S305)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N1,N3,M2,N3,N1,M2,N0,M1,X8,S305,-0.500)
       deallocate(S305)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N0,N1,N1,N3,N0,M1,S304,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,M2+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,M2,N3,M2,N3,t2A,D2)
       allocate(S306(M2+1:N3,M2+1:N3,N1+1:N3,N0+1:M1))
       I1=K5*K3
       I2=K6*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S306)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N1,N3,M2,N3,M2,N3,N0,M1,X7,S306,-0.500)
       deallocate(S306)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder3421(N0,M1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N1,N0,M1,S304,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S307(N1+1:M2,N0+1:M1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S307)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N1,M2,N0,M1,N0,M1,X41,S307, 1.000)
       deallocate(S307)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder2431(N0,M1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N1,N0,M1,S304,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2A,D2)
       allocate(S308(M2+1:N3,N0+1:M1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S308)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,M2,N3,N0,M1,N0,M1,X40,S308,-1.000)
       deallocate(S308)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:M1))
       call reorder4231(N0,M1,N0,N1,N0,N1,N1,N3,
     & N1,N3,N0,N1,N0,N1,N0,M1,S304,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S360(N0+1:M1,N0+1:N1,N0+1:N1,N0+1:M1))
       I1=K5*K1*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S360)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,N0+1:M1))
       call reorder2314(N0,M1,N0,N1,N0,N1,N0,M1,
     & N0,N1,N0,N1,N0,M1,N0,M1,S360,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S361(M2+1:N3,N0+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S361)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,M2,N3,N0,M1,N0,M1,X5,S361, 1.000)
       deallocate(S361)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:M1))
       call reorder3241(N0,M1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N0,N1,N1,N3,N0,M1,S304,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S363(M2+1:N3,N0+1:N1,N1+1:N3,N0+1:M1))
       I1=K5*K3*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S363)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder2314(M2,N3,N0,N1,N1,N3,N0,M1,
     & N0,N1,N1,N3,M2,N3,N0,M1,S363,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S364(M2+1:N3,N1+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S364)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,M2,N3,N0,M1,X7,S364,-1.000)
       deallocate(S364)
C
       call sumx2314(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,M2,N3,M2,N3,N0,M1,X7,VAHPPP, 1.000)
C
       allocate(D2(N1+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,M2,N0,M1,M1,N1,t2A,D2)
       allocate(Z7(N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K6
       I2=K7*K5*K9
       I3=K3
       call EGEMM(I1,I2,I3,X7,D2,Z7)
       deallocate(D2)
C
       call
     & sum346125(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z7, 1.000)
       call
     & sum356124(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z7,-1.000)
       deallocate(Z7)
       deallocate(X7)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:M1))
       call reorder3241(N0,M1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N0,N1,N1,N3,N0,M1,S304,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S365(N1+1:M2,N0+1:N1,N1+1:N3,N0+1:M1))
       I1=K5*K3*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S365)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder2314(N1,M2,N0,N1,N1,N3,N0,M1,
     & N0,N1,N1,N3,N1,M2,N0,M1,S365,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S366(M2+1:N3,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S366)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,N1,M2,N0,M1,X8,S366,-1.000)
       deallocate(S366)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder2431(N0,M1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N1,N0,M1,S304,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N1,t2A,D2)
       allocate(S309(M2+1:N3,M1+1:N1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S309)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N1,M2,N3,N0,M1,M1,N1,X1,S309, 1.000)
       deallocate(S309)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,N0+1:M1))
       call reorder3214(N0,M1,N0,N1,N0,N1,N0,M1,
     & N0,N1,N0,N1,N0,M1,N0,M1,S360,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S362(N1+1:M2,N0+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S362)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N1,M2,N0,M1,N0,M1,X6,S362,-1.000)
       deallocate(S362)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder4132(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N1,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N1,t2A,D2)
       allocate(S323(M2+1:N3,M1+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S323)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder3412(M2,N3,M1,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,M2,N3,M1,N1,S323,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S329(N1+1:M2,N1+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S329)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,N3,M2,N3,N1,M2,M1,N1,X4,S329,-1.000)
       deallocate(S329)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder3412(M2,N3,M1,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,M2,N3,M1,N1,S323,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S324(M2+1:N3,N1+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S324)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,M2,N3,M1,N1,X39,S324,-1.000)
       deallocate(S324)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,VAHHPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,M1,N0,M1,t2A,D2)
       allocate(S321(N0+1:M1,N0+1:M1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K5*K5
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S321)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,N0+1:M1))
       call reorder3412(N0,M1,N0,M1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,M1,N0,M1,S321,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S322(M2+1:N3,N0+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S322)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,M2,N3,N0,M1,N0,M1,X5,S322,-0.500)
       deallocate(S322)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,N0+1:M1))
       call reorder4312(N0,M1,N0,M1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,M1,N0,M1,S321,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S327(N1+1:M2,N0+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S327)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N1,M2,N0,M1,N0,M1,X6,S327, 0.500)
       deallocate(S327)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N1,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S325(N1+1:M2,M1+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S325)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3412(N1,M2,M1,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N1,M2,M1,N1,S325,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S326(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S326)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,N1,M2,M1,N1,X4,S326,-1.000)
       deallocate(S326)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N1,N3,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q51(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q51)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q51,B1)
       allocate(D2(N1+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,M2,N3,N0,M1,M1,N1,t2A,D2)
       allocate(S318(M2+1:N3,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K6
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S318)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,M2,N3,N0,M1,M1,N1,X1,S318,-1.000)
       deallocate(S318)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q51,B1)
       allocate(D2(N1+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,M2,N0,M1,M1,N1,t2A,D2)
       allocate(S320(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S320)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X2,S320,-1.000)
       deallocate(S320)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q51,B1)
       allocate(D2(N1+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,M2,N0,M1,N0,M1,t2A,D2)
       allocate(S328(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S328)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,N0,M1,X6,S328,-1.000)
       deallocate(S328)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q51,B1)
       allocate(D2(N1+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,M2,N3,N0,M1,N0,M1,t2A,D2)
       allocate(S330(M2+1:N3,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K6
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S330)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,M2,N3,N0,M1,N0,M1,X5,S330,-1.000)
       deallocate(S330)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,N2,N0,N1,N2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S334(N0+1:M1,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4*K1*K2
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S334)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder2431(N0,M1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N2,N3,N0,N1,N0,M1,S334,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S342(N1+1:M2,M1+1:N1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S342)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N1,N1,M2,N0,M1,M1,N1,X2,S342,-1.000)
       deallocate(S342)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder2431(N0,M1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N2,N3,N0,N1,N0,M1,S334,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,M1,N1,t2B,D2)
       allocate(S343(M2+1:N3,M1+1:N1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S343)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N1,M2,N3,N0,M1,M1,N1,X1,S343,-1.000)
       deallocate(S343)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder2431(N0,M1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N2,N3,N0,N1,N0,M1,S334,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S335(N1+1:M2,N0+1:M1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S335)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N1,M2,N0,M1,N0,M1,X41,S335, 1.000)
       deallocate(S335)
C
       allocate(D2(N0+1:N1,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,M2,N3,M2,N3,M1,N1,t2A,D2)
       allocate(Z55(M2+1:N3,M2+1:N3,M1+1:N1,N1+1:M2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K9
       I2=K7*K6*K6
       I3=K1
       call EGEMM(I1,I2,I3,X41,D2,Z55)
       deallocate(D2)
C
       call
     & sum126345(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z55,-1.000)
       call
     & sum126354(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z55, 1.000)
       deallocate(Z55)
       deallocate(X41)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder2431(N0,M1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N2,N3,N0,N1,N0,M1,S334,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S336(M2+1:N3,N0+1:M1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S336)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,M2,N3,N0,M1,N0,M1,X40,S336, 1.000)
       deallocate(S336)
C
       allocate(D2(N0+1:N1,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,M2,N3,N1,M2,M1,N1,t2A,D2)
       allocate(Z54(M2+1:N3,N1+1:M2,M1+1:N1,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5*K5*K6
       I2=K7*K9*K6
       I3=K1
       call EGEMM(I1,I2,I3,X40,D2,Z54)
       deallocate(D2)
C
       call
     & sum236145(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z54,-1.000)
       call
     & sum136245(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z54, 1.000)
       call
     & sum236154(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z54, 1.000)
       call
     & sum136254(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z54,-1.000)
       deallocate(Z54)
       deallocate(X40)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N1,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S337(N1+1:M2,N0+1:M1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S337)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3412(N1,M2,N0,M1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N1,M2,N0,M1,S337,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S338(M2+1:N3,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S338)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,N1,M2,N0,M1,X8,S338,-1.000)
       deallocate(S338)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N1,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S339(M2+1:N3,N0+1:M1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S339)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3412(M2,N3,N0,M1,N0,N1,N1,N3,
     & N0,N1,N1,N3,M2,N3,N0,M1,S339,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S341(N1+1:M2,N1+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S341)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,N3,M2,N3,N1,M2,N0,M1,X8,S341, 1.000)
       deallocate(S341)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3412(M2,N3,N0,M1,N0,N1,N1,N3,
     & N0,N1,N1,N3,M2,N3,N0,M1,S339,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S340(M2+1:N3,N1+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S340)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,M2,N3,N0,M1,X42,S340, 1.000)
       deallocate(S340)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N1,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,M1,N1,t2B,D2)
       allocate(S346(M2+1:N3,M1+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S346)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder3412(M2,N3,M1,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,M2,N3,M1,N1,S346,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S348(N1+1:M2,N1+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S348)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,N3,M2,N3,N1,M2,M1,N1,X4,S348, 1.000)
       deallocate(S348)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder3412(M2,N3,M1,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,M2,N3,M1,N1,S346,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S347(M2+1:N3,N1+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S347)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,M2,N3,M1,N1,X39,S347, 1.000)
       deallocate(S347)
C
       allocate(D2(N1+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,M2,N0,M1,N0,M1,t2A,D2)
       allocate(Z46(N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K6
       I2=K5*K5*K9
       I3=K3
       call EGEMM(I1,I2,I3,X39,D2,Z46)
       deallocate(D2)
C
       call
     & sum345126(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z46, 1.000)
       call
     & sum345216(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z46,-1.000)
       deallocate(Z46)
       deallocate(X39)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N1,N3,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q52(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q52)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q52,B1)
       allocate(D2(N1+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,M2,N0,M1,M1,N1,t2A,D2)
       allocate(S350(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S350)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X2,S350,-1.000)
       deallocate(S350)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q52,B1)
       allocate(D2(N1+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,M2,N0,M1,N0,M1,t2A,D2)
       allocate(S351(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K9
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S351)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,N0,M1,X6,S351,-1.000)
       deallocate(S351)
C
       call sumx2134(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,M2,N0,M1,N0,M1,X6,VAHHHP, 1.000)
C
       allocate(D2(N0+1:N1,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,M2,N3,M2,N3,M1,N1,t2A,D2)
       allocate(Z6(M2+1:N3,M2+1:N3,M1+1:N1,N1+1:M2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K9
       I2=K7*K6*K6
       I3=K1
       call EGEMM(I1,I2,I3,X6,D2,Z6)
       deallocate(D2)
C
       call
     & sum126345(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z6,-1.000)
       deallocate(Z6)
       deallocate(X6)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q52,B1)
       allocate(D2(N1+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,M2,N3,N0,M1,N0,M1,t2A,D2)
       allocate(S352(M2+1:N3,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K6
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S352)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,M2,N3,N0,M1,N0,M1,X5,S352,-1.000)
       deallocate(S352)
C
       call sumx2134(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,M2,N3,N0,M1,N0,M1,X5,VAHHHP, 1.000)
C
       allocate(D2(N0+1:N1,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,M2,N3,N1,M2,M1,N1,t2A,D2)
       allocate(Z5(M2+1:N3,N1+1:M2,M1+1:N1,M2+1:N3,N0+1:M1,N0+1:M1))
       I1=K5*K5*K6
       I2=K7*K9*K6
       I3=K1
       call EGEMM(I1,I2,I3,X5,D2,Z5)
       deallocate(D2)
C
       call
     & sum236145(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z5,-1.000)
       call
     & sum136245(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z5, 1.000)
       deallocate(Z5)
       deallocate(X5)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q52,B1)
       allocate(D2(N1+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,M2,N3,N0,M1,M1,N1,t2A,D2)
       allocate(S349(M2+1:N3,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K6
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S349)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,M2,N3,N0,M1,M1,N1,X1,S349,-1.000)
       deallocate(S349)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N1,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S311(N1+1:M2,N0+1:M1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S311)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3412(N1,M2,N0,M1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N1,M2,N0,M1,S311,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S312(M2+1:N3,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S312)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,N1,M2,N0,M1,X8,S312,-1.000)
       deallocate(S312)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder4132(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N1,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2A,D2)
       allocate(S313(M2+1:N3,N0+1:M1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S313)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3412(M2,N3,N0,M1,N0,N1,N1,N3,
     & N0,N1,N1,N3,M2,N3,N0,M1,S313,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S317(N1+1:M2,N1+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S317)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,N3,M2,N3,N1,M2,N0,M1,X8,S317,-1.000)
       deallocate(S317)
C
       call sumx2314(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,M2,N3,N1,M2,N0,M1,X8,VAHPPP, 1.000)
C
       allocate(D2(N1+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,M2,N3,N0,M1,M1,N1,t2A,D2)
       allocate(Z8(M2+1:N3,N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K6
       I2=K7*K5*K6
       I3=K3
       call EGEMM(I1,I2,I3,X8,D2,Z8)
       deallocate(D2)
C
       call
     & sum246135(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z8,-1.000)
       call
     & sum146235(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z8, 1.000)
       call
     & sum256134(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z8, 1.000)
       call
     & sum156234(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z8,-1.000)
       deallocate(Z8)
       deallocate(X8)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3412(M2,N3,N0,M1,N0,N1,N1,N3,
     & N0,N1,N1,N3,M2,N3,N0,M1,S313,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S314(M2+1:N3,N1+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S314)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,M2,N3,N0,M1,X42,S314,-1.000)
       deallocate(S314)
C
       allocate(D2(N1+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,M2,N0,M1,M1,N1,t2A,D2)
       allocate(Z56(N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K6
       I2=K7*K5*K9
       I3=K3
       call EGEMM(I1,I2,I3,X42,D2,Z56)
       deallocate(D2)
C
       call
     & sum346125(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z56,-1.000)
       call
     & sum346215(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z56, 1.000)
       call
     & sum356124(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z56, 1.000)
       call
     & sum356214(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z56,-1.000)
       deallocate(Z56)
       deallocate(X42)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,VAHHPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,M1,M1,N1,t2A,D2)
       allocate(S315(N0+1:M1,M1+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K7*K5
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S315)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder4312(N0,M1,M1,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,M1,M1,N1,S315,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S319(N1+1:M2,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S319)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N1,M2,N0,M1,M1,N1,X2,S319, 0.500)
       deallocate(S319)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder3412(N0,M1,M1,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,M1,M1,N1,S315,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S316(M2+1:N3,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S316)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,M2,N3,N0,M1,M1,N1,X1,S316,-0.500)
       deallocate(S316)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,N2,N0,N1,N2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S331(M1+1:N1,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4*K1*K2
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S331)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M1+1:N1))
       call reorder2431(M1,N1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N2,N3,N0,N1,M1,N1,S331,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S333(M2+1:N3,N0+1:M1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S333)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,M2,N3,N0,M1,M1,N1,X1,S333, 1.000)
       deallocate(S333)
C
       call sumx2134(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,M2,N3,N0,M1,M1,N1,X1,VAHHHP, 1.000)
C
       allocate(D2(N0+1:N1,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,M2,N3,N1,M2,N0,M1,t2A,D2)
       allocate(Z1(M2+1:N3,N1+1:M2,N0+1:M1,M2+1:N3,N0+1:M1,M1+1:N1))
       I1=K7*K5*K6
       I2=K5*K9*K6
       I3=K1
       call EGEMM(I1,I2,I3,X1,D2,Z1)
       deallocate(D2)
C
       call
     & sum234156(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z1,-1.000)
       call
     & sum134256(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z1, 1.000)
       call
     & sum235146(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z1, 1.000)
       call
     & sum135246(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z1,-1.000)
       deallocate(Z1)
       deallocate(X1)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M1+1:N1))
       call reorder2431(M1,N1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N2,N3,N0,N1,M1,N1,S331,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S332(N1+1:M2,N0+1:M1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S332)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N1,M2,N0,M1,M1,N1,X2,S332, 1.000)
       deallocate(S332)
C
       call sumx2134(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,M2,N0,M1,M1,N1,X2,VAHHHP, 1.000)
C
       allocate(D2(N0+1:N1,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,M2,N3,M2,N3,N0,M1,t2A,D2)
       allocate(Z2(M2+1:N3,M2+1:N3,N0+1:M1,N1+1:M2,N0+1:M1,M1+1:N1))
       I1=K7*K5*K9
       I2=K5*K6*K6
       I3=K1
       call EGEMM(I1,I2,I3,X2,D2,Z2)
       deallocate(D2)
C
       call
     & sum124356(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z2,-1.000)
       call
     & sum125346(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z2, 1.000)
       deallocate(Z2)
       deallocate(X2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N1,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S344(N1+1:M2,M1+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S344)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3412(N1,M2,M1,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N1,M2,M1,N1,S344,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S345(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S345)
       deallocate(D1)
       deallocate(B2)
       deallocate(S344)
C
       call
     & sum2134(N1,N3,M2,N3,N1,M2,M1,N1,X4,S345,-1.000)
       deallocate(S345)
C
       call sumx2314(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,M2,N3,N1,M2,M1,N1,X4,VAHPPP, 1.000)
C
       allocate(D2(N1+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,M2,N3,N0,M1,N0,M1,t2A,D2)
       allocate(Z4(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K6
       I2=K5*K5*K6
       I3=K3
       call EGEMM(I1,I2,I3,X4,D2,Z4)
       deallocate(D2)
C
       call
     & sum245136(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z4, 1.000)
       call
     & sum145236(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z4,-1.000)
       deallocate(Z4)
       deallocate(X4)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N2,N2,M2,N1,M2,M1,N1,VBHPPH,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1))
       call reorder412356(N2,M2,M2,N3,M2,N3,M1,N2,N0,M1,N0,M1,
     & M1,N2,N2,M2,M2,N3,M2,N3,N0,M1,N0,M1,t3B2,F2)
       allocate(Z35(M2+1:N3,M2+1:N3,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K5*K5*K6*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z35)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z35, 1.000)
       deallocate(Z35)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,N2,M2,N1,M2,N0,M1,VBHPPH,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder412356(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,M2,N3,N0,M1,M1,N1,t3B3,F2)
       allocate(Z40(M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K6*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z40)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124635(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z40,-1.000)
       call
     & sum125634(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z40, 1.000)
       deallocate(Z40)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N2,N2,M2,N1,M2,N0,M1,VBHPPH,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder412356(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,M2,N3,N0,M1,M1,N1,t3B3,F2)
       allocate(Z41(M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K5*K6*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z41)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124635(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z41,-1.000)
       call
     & sum125634(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,V3A,Z41, 1.000)
       deallocate(Z41)
C
       call sumx3(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,HT3A,V3A,1.0)
       deallocate(V3A)
C
       end
