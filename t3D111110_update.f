       subroutine t3D111110_update(N0,N1,N2,N3,HT3D,shift,
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
       real*8 HT3D(N2+1:N3,N2+1:N3,N2+1:M2,N0+1:N2,N0+1:N2,M1+1:N2)
C
       real*8,allocatable::V3D(:,:,:,:,:,:)
       real*8,allocatable::B1(:,:)
       real*8,allocatable::B2(:,:)
       real*8,allocatable::C1(:,:,:)
       real*8,allocatable::C2(:,:,:)
       real*8,allocatable::D1(:,:,:,:)
       real*8,allocatable::D2(:,:,:,:)
       real*8,allocatable::F1(:,:,:,:,:,:)
       real*8,allocatable::F2(:,:,:,:,:,:)
       real*8,allocatable::Q1(:,:)
       real*8,allocatable::Q2(:,:)
       real*8,allocatable::Q3(:,:)
       real*8,allocatable::Q4(:,:)
       real*8,allocatable::Q5(:,:)
       real*8,allocatable::Q6(:,:)
       real*8,allocatable::Q7(:,:)
       real*8,allocatable::Q8(:,:)
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
       real*8,allocatable::S19(:,:,:,:)
       real*8,allocatable::S20(:,:,:,:)
       real*8,allocatable::S21(:,:,:,:)
       real*8,allocatable::S22(:,:,:,:)
       real*8,allocatable::S23(:,:,:,:)
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
       real*8,allocatable::S38(:,:,:,:)
       real*8,allocatable::S39(:,:,:,:)
       real*8,allocatable::Q9(:,:)
       real*8,allocatable::Q10(:,:)
       real*8,allocatable::Q11(:,:)
       real*8,allocatable::Q12(:,:)
       real*8,allocatable::Q13(:,:)
       real*8,allocatable::Q14(:,:)
       real*8,allocatable::Q15(:,:)
       real*8,allocatable::Q16(:,:)
       real*8,allocatable::S40(:,:,:,:)
       real*8,allocatable::S41(:,:,:,:)
       real*8,allocatable::S42(:,:,:,:)
       real*8,allocatable::S43(:,:,:,:)
       real*8,allocatable::S44(:,:,:,:)
       real*8,allocatable::S45(:,:,:,:)
       real*8,allocatable::S46(:,:,:,:)
       real*8,allocatable::S47(:,:,:,:)
       real*8,allocatable::S48(:,:,:,:)
       real*8,allocatable::S49(:,:,:,:)
       real*8,allocatable::S50(:,:,:,:)
       real*8,allocatable::S51(:,:,:,:)
       real*8,allocatable::S52(:,:,:,:)
       real*8,allocatable::S53(:,:,:,:)
       real*8,allocatable::S54(:,:,:,:)
       real*8,allocatable::Q17(:,:)
       real*8,allocatable::Q18(:,:)
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
       real*8,allocatable::S68(:,:,:,:)
       real*8,allocatable::S69(:,:,:,:)
       real*8,allocatable::S70(:,:,:,:)
       real*8,allocatable::S71(:,:,:,:)
       real*8,allocatable::S72(:,:,:,:)
       real*8,allocatable::Q19(:,:)
       real*8,allocatable::Q20(:,:)
       real*8,allocatable::S73(:,:,:,:)
       real*8,allocatable::S74(:,:,:,:)
       real*8,allocatable::S75(:,:,:,:)
       real*8,allocatable::S76(:,:,:,:)
       real*8,allocatable::S77(:,:,:,:)
       real*8,allocatable::S78(:,:,:,:)
       real*8,allocatable::S79(:,:,:,:)
       real*8,allocatable::S80(:,:,:,:)
       real*8,allocatable::Q21(:,:)
       real*8,allocatable::Q22(:,:)
       real*8,allocatable::S81(:,:,:,:)
       real*8,allocatable::S82(:,:,:,:)
       real*8,allocatable::Q23(:,:)
       real*8,allocatable::Q24(:,:)
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
       real*8,allocatable::Q25(:,:)
       real*8,allocatable::Q26(:,:)
       real*8,allocatable::Q27(:,:)
       real*8,allocatable::Q28(:,:)
       real*8,allocatable::Q29(:,:)
       real*8,allocatable::Q30(:,:)
       real*8,allocatable::Q31(:,:)
       real*8,allocatable::Q32(:,:)
       real*8,allocatable::S116(:,:,:,:)
       real*8,allocatable::S117(:,:,:,:)
       real*8,allocatable::S118(:,:,:,:)
       real*8,allocatable::S119(:,:,:,:)
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
       real*8,allocatable::S148(:,:,:,:)
       real*8,allocatable::S149(:,:,:,:)
       real*8,allocatable::S150(:,:,:,:)
       real*8,allocatable::S151(:,:,:,:)
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
       real*8,allocatable::S206(:,:,:,:)
       real*8,allocatable::S207(:,:,:,:)
       real*8,allocatable::S208(:,:,:,:)
       real*8,allocatable::S209(:,:,:,:)
       real*8,allocatable::S210(:,:,:,:)
       real*8,allocatable::S211(:,:,:,:)
       real*8,allocatable::S212(:,:,:,:)
       real*8,allocatable::S213(:,:,:,:)
       real*8,allocatable::S214(:,:,:,:)
       real*8,allocatable::Q33(:,:)
       real*8,allocatable::Q34(:,:)
       real*8,allocatable::S215(:,:,:,:)
       real*8,allocatable::S216(:,:,:,:)
       real*8,allocatable::S217(:,:,:,:)
       real*8,allocatable::Q35(:,:)
       real*8,allocatable::Q36(:,:)
       real*8,allocatable::S218(:,:,:,:)
       real*8,allocatable::S219(:,:,:,:)
       real*8,allocatable::Q37(:,:)
       real*8,allocatable::Q38(:,:)
       real*8,allocatable::Q39(:,:)
       real*8,allocatable::Q40(:,:)
       real*8,allocatable::Q41(:,:)
       real*8,allocatable::Q42(:,:)
       real*8,allocatable::Q43(:,:)
       real*8,allocatable::Q44(:,:)
       real*8,allocatable::Q45(:,:)
       real*8,allocatable::Q49(:,:)
       real*8,allocatable::Q51(:,:)
       real*8,allocatable::Q46(:,:)
       real*8,allocatable::Q47(:,:)
       real*8,allocatable::Q48(:,:)
       real*8,allocatable::Q52(:,:)
       real*8,allocatable::Q50(:,:)
       real*8,allocatable::Q53(:,:)
       real*8,allocatable::S221(:,:,:,:)
       real*8,allocatable::S220(:,:,:,:)
       real*8,allocatable::S222(:,:,:,:)
       real*8,allocatable::S224(:,:,:,:)
       real*8,allocatable::S225(:,:,:,:)
       real*8,allocatable::S223(:,:,:,:)
       real*8,allocatable::S226(:,:,:,:)
       real*8,allocatable::S230(:,:,:,:)
       real*8,allocatable::S227(:,:,:,:)
       real*8,allocatable::S228(:,:,:,:)
       real*8,allocatable::S229(:,:,:,:)
       real*8,allocatable::S231(:,:,:,:)
       real*8,allocatable::S237(:,:,:,:)
       real*8,allocatable::S238(:,:,:,:)
       real*8,allocatable::S232(:,:,:,:)
       real*8,allocatable::S233(:,:,:,:)
       real*8,allocatable::S239(:,:,:,:)
       real*8,allocatable::S241(:,:,:,:)
       real*8,allocatable::S234(:,:,:,:)
       real*8,allocatable::S235(:,:,:,:)
       real*8,allocatable::S236(:,:,:,:)
       real*8,allocatable::S242(:,:,:,:)
       real*8,allocatable::S240(:,:,:,:)
       real*8,allocatable::S243(:,:,:,:)
       real*8,allocatable::S251(:,:,:,:)
       real*8,allocatable::S255(:,:,:,:)
       real*8,allocatable::S244(:,:,:,:)
       real*8,allocatable::S249(:,:,:,:)
       real*8,allocatable::S254(:,:,:,:)
       real*8,allocatable::S258(:,:,:,:)
       real*8,allocatable::S250(:,:,:,:)
       real*8,allocatable::S247(:,:,:,:)
       real*8,allocatable::S248(:,:,:,:)
       real*8,allocatable::S257(:,:,:,:)
       real*8,allocatable::S253(:,:,:,:)
       real*8,allocatable::S245(:,:,:,:)
       real*8,allocatable::S246(:,:,:,:)
       real*8,allocatable::S252(:,:,:,:)
       real*8,allocatable::S256(:,:,:,:)
       real*8,allocatable::S259(:,:,:,:)
       real*8,allocatable::S260(:,:,:,:)
       real*8,allocatable::S261(:,:,:,:)
       real*8,allocatable::S262(:,:,:,:)
       real*8,allocatable::S263(:,:,:,:)
       real*8,allocatable::S264(:,:,:,:)
       real*8,allocatable::S265(:,:,:,:)
       real*8,allocatable::S273(:,:,:,:)
       real*8,allocatable::S266(:,:,:,:)
       real*8,allocatable::S269(:,:,:,:)
       real*8,allocatable::S275(:,:,:,:)
       real*8,allocatable::S270(:,:,:,:)
       real*8,allocatable::S267(:,:,:,:)
       real*8,allocatable::S274(:,:,:,:)
       real*8,allocatable::S268(:,:,:,:)
       real*8,allocatable::S271(:,:,:,:)
       real*8,allocatable::S276(:,:,:,:)
       real*8,allocatable::S272(:,:,:,:)
       real*8,allocatable::S277(:,:,:,:)
       real*8,allocatable::S278(:,:,:,:)
       real*8,allocatable::S279(:,:,:,:)
       real*8,allocatable::S280(:,:,:,:)
       real*8,allocatable::S281(:,:,:,:)
       real*8,allocatable::S282(:,:,:,:)
       real*8,allocatable::S283(:,:,:,:)
       real*8,allocatable::S284(:,:,:,:)
       real*8,allocatable::Q54(:,:)
       real*8,allocatable::Q55(:,:)
       real*8,allocatable::Q56(:,:)
       real*8,allocatable::Q57(:,:)
       real*8,allocatable::S285(:,:,:,:)
       real*8,allocatable::S286(:,:,:,:)
       real*8,allocatable::S287(:,:,:,:)
       real*8,allocatable::S297(:,:,:,:)
       real*8,allocatable::S288(:,:,:,:)
       real*8,allocatable::S291(:,:,:,:)
       real*8,allocatable::S292(:,:,:,:)
       real*8,allocatable::S293(:,:,:,:)
       real*8,allocatable::S294(:,:,:,:)
       real*8,allocatable::S295(:,:,:,:)
       real*8,allocatable::S296(:,:,:,:)
       real*8,allocatable::Q58(:,:)
       real*8,allocatable::Q62(:,:)
       real*8,allocatable::Q64(:,:)
       real*8,allocatable::Q59(:,:)
       real*8,allocatable::S289(:,:,:,:)
       real*8,allocatable::S290(:,:,:,:)
       real*8,allocatable::S298(:,:,:,:)
       real*8,allocatable::Q60(:,:)
       real*8,allocatable::Q61(:,:)
       real*8,allocatable::Q65(:,:)
       real*8,allocatable::Q63(:,:)
       real*8,allocatable::S299(:,:,:,:)
       real*8,allocatable::S301(:,:,:,:)
       real*8,allocatable::S302(:,:,:,:)
       real*8,allocatable::S300(:,:,:,:)
       real*8,allocatable::S303(:,:,:,:)
       real*8,allocatable::S312(:,:,:,:)
       real*8,allocatable::S304(:,:,:,:)
       real*8,allocatable::S307(:,:,:,:)
       real*8,allocatable::S308(:,:,:,:)
       real*8,allocatable::S309(:,:,:,:)
       real*8,allocatable::S310(:,:,:,:)
       real*8,allocatable::S305(:,:,:,:)
       real*8,allocatable::S306(:,:,:,:)
       real*8,allocatable::S311(:,:,:,:)
       real*8,allocatable::S313(:,:,:,:)
       real*8,allocatable::S315(:,:,:,:)
       real*8,allocatable::S316(:,:,:,:)
       real*8,allocatable::S317(:,:,:,:)
       real*8,allocatable::S318(:,:,:,:)
       real*8,allocatable::S334(:,:,:,:)
       real*8,allocatable::S335(:,:,:,:)
       real*8,allocatable::S338(:,:,:,:)
       real*8,allocatable::S339(:,:,:,:)
       real*8,allocatable::S340(:,:,:,:)
       real*8,allocatable::S341(:,:,:,:)
       real*8,allocatable::S314(:,:,:,:)
       real*8,allocatable::S336(:,:,:,:)
       real*8,allocatable::S337(:,:,:,:)
       real*8,allocatable::S342(:,:,:,:)
       real*8,allocatable::S327(:,:,:,:)
       real*8,allocatable::S328(:,:,:,:)
       real*8,allocatable::S322(:,:,:,:)
       real*8,allocatable::S333(:,:,:,:)
       real*8,allocatable::S323(:,:,:,:)
       real*8,allocatable::S329(:,:,:,:)
       real*8,allocatable::S330(:,:,:,:)
       real*8,allocatable::S331(:,:,:,:)
       real*8,allocatable::S319(:,:,:,:)
       real*8,allocatable::S321(:,:,:,:)
       real*8,allocatable::S324(:,:,:,:)
       real*8,allocatable::S325(:,:,:,:)
       real*8,allocatable::S320(:,:,:,:)
       real*8,allocatable::Q66(:,:)
       real*8,allocatable::S326(:,:,:,:)
       real*8,allocatable::S332(:,:,:,:)
       real*8,allocatable::X1(:,:,:,:)
       real*8,allocatable::Z1(:,:,:,:,:,:)
       real*8,allocatable::X2(:,:,:,:)
       real*8,allocatable::Z2(:,:,:,:,:,:)
       real*8,allocatable::Z3(:,:,:,:,:,:)
       real*8,allocatable::X3(:,:,:,:)
       real*8,allocatable::Z4(:,:,:,:,:,:)
       real*8,allocatable::X4(:,:,:,:)
       real*8,allocatable::Z5(:,:,:,:,:,:)
       real*8,allocatable::X5(:,:,:,:)
       real*8,allocatable::Z6(:,:,:,:,:,:)
       real*8,allocatable::X6(:,:,:,:)
       real*8,allocatable::Z7(:,:,:,:,:,:)
       real*8,allocatable::X7(:,:,:,:)
       real*8,allocatable::Z8(:,:,:,:,:,:)
       real*8,allocatable::X8(:,:,:,:)
       real*8,allocatable::Z9(:,:,:,:,:,:)
       real*8,allocatable::X9(:,:,:,:)
       real*8,allocatable::Z10(:,:,:,:,:,:)
       real*8,allocatable::X10(:,:,:,:)
       real*8,allocatable::Z11(:,:,:,:,:,:)
       real*8,allocatable::X11(:,:,:,:)
       real*8,allocatable::Z12(:,:,:,:,:,:)
       real*8,allocatable::X12(:,:)
       real*8,allocatable::Z13(:,:,:,:,:,:)
       real*8,allocatable::X13(:,:)
       real*8,allocatable::Z14(:,:,:,:,:,:)
       real*8,allocatable::X14(:,:)
       real*8,allocatable::Z15(:,:,:,:,:,:)
       real*8,allocatable::X15(:,:)
       real*8,allocatable::Z16(:,:,:,:,:,:)
       real*8,allocatable::X16(:,:)
       real*8,allocatable::Z17(:,:,:,:,:,:)
       real*8,allocatable::X17(:,:)
       real*8,allocatable::Z18(:,:,:,:,:,:)
       real*8,allocatable::X18(:,:)
       real*8,allocatable::Z19(:,:,:,:,:,:)
       real*8,allocatable::X19(:,:)
       real*8,allocatable::Z20(:,:,:,:,:,:)
       real*8,allocatable::X20(:,:,:,:)
       real*8,allocatable::Z21(:,:,:,:,:,:)
       real*8,allocatable::X21(:,:,:,:)
       real*8,allocatable::Z22(:,:,:,:,:,:)
       real*8,allocatable::X22(:,:,:,:)
       real*8,allocatable::Z23(:,:,:,:,:,:)
       real*8,allocatable::X23(:,:,:,:)
       real*8,allocatable::Z24(:,:,:,:,:,:)
       real*8,allocatable::X24(:,:,:,:)
       real*8,allocatable::Z25(:,:,:,:,:,:)
       real*8,allocatable::X25(:,:,:,:)
       real*8,allocatable::Z26(:,:,:,:,:,:)
       real*8,allocatable::X26(:,:,:,:)
       real*8,allocatable::Z27(:,:,:,:,:,:)
       real*8,allocatable::Z28(:,:,:,:,:,:)
       real*8,allocatable::Z29(:,:,:,:,:,:)
       real*8,allocatable::Z30(:,:,:,:,:,:)
       real*8,allocatable::Z31(:,:,:,:,:,:)
       real*8,allocatable::X27(:,:,:,:)
       real*8,allocatable::Z32(:,:,:,:,:,:)
       real*8,allocatable::X28(:,:,:,:)
       real*8,allocatable::Z33(:,:,:,:,:,:)
       real*8,allocatable::X29(:,:,:,:)
       real*8,allocatable::Z34(:,:,:,:,:,:)
       real*8,allocatable::X30(:,:,:,:)
       real*8,allocatable::Z35(:,:,:,:,:,:)
       real*8,allocatable::X31(:,:,:,:)
       real*8,allocatable::Z36(:,:,:,:,:,:)
       real*8,allocatable::X32(:,:)
       real*8,allocatable::Z37(:,:,:,:,:,:)
       real*8,allocatable::X33(:,:)
       real*8,allocatable::Z38(:,:,:,:,:,:)
       real*8,allocatable::X34(:,:)
       real*8,allocatable::Z39(:,:,:,:,:,:)
       real*8,allocatable::X35(:,:)
       real*8,allocatable::Z40(:,:,:,:,:,:)
       real*8,allocatable::X36(:,:)
       real*8,allocatable::Z41(:,:,:,:,:,:)
       real*8,allocatable::X37(:,:)
       real*8,allocatable::Z42(:,:,:,:,:,:)
       real*8,allocatable::X38(:,:)
       real*8,allocatable::Z43(:,:,:,:,:,:)
       real*8,allocatable::X39(:,:)
       real*8,allocatable::Z44(:,:,:,:,:,:)
       real*8,allocatable::X40(:,:,:,:)
       real*8,allocatable::Z46(:,:,:,:,:,:)
       real*8,allocatable::X41(:,:,:,:)
       real*8,allocatable::Z47(:,:,:,:,:,:)
       real*8,allocatable::X42(:,:,:,:)
       real*8,allocatable::Z48(:,:,:,:,:,:)
       real*8,allocatable::X43(:,:,:,:)
       real*8,allocatable::Z49(:,:,:,:,:,:)
       real*8,allocatable::X44(:,:,:,:)
       real*8,allocatable::Z50(:,:,:,:,:,:)
       real*8,allocatable::X45(:,:,:,:)
       real*8,allocatable::Z51(:,:,:,:,:,:)
       real*8,allocatable::X46(:,:,:,:)
       real*8,allocatable::Z52(:,:,:,:,:,:)
       real*8,allocatable::X47(:,:,:,:)
       real*8,allocatable::Z64(:,:,:,:,:,:)
       real*8,allocatable::X48(:,:,:,:)
       real*8,allocatable::Z65(:,:,:,:,:,:)
       real*8,allocatable::X49(:,:,:,:)
       real*8,allocatable::Z66(:,:,:,:,:,:)
       real*8,allocatable::X50(:,:,:,:)
       real*8,allocatable::Z67(:,:,:,:,:,:)
       real*8,allocatable::X51(:,:,:,:)
       real*8,allocatable::Z68(:,:,:,:,:,:)
       real*8,allocatable::X52(:,:,:,:)
       real*8,allocatable::Z69(:,:,:,:,:,:)
       real*8,allocatable::X53(:,:,:,:)
       real*8,allocatable::Z70(:,:,:,:,:,:)
       real*8,allocatable::X54(:,:,:,:)
       real*8,allocatable::Z71(:,:,:,:,:,:)
       real*8,allocatable::Z92(:,:,:,:,:,:)
       real*8,allocatable::Z93(:,:,:,:,:,:)
       real*8,allocatable::Z94(:,:,:,:,:,:)
       real*8,allocatable::X55(:,:,:,:)
       real*8,allocatable::Z99(:,:,:,:,:,:)
       real*8,allocatable::X56(:,:,:,:)
       real*8,allocatable::Z100(:,:,:,:,:,:)
       real*8,allocatable::X57(:,:,:,:)
       real*8,allocatable::Z101(:,:,:,:,:,:)
       real*8,allocatable::X58(:,:,:,:)
       real*8,allocatable::Z102(:,:,:,:,:,:)
       real*8,allocatable::X59(:,:,:,:)
       real*8,allocatable::Z103(:,:,:,:,:,:)
       real*8,allocatable::X60(:,:,:,:)
       real*8,allocatable::Z104(:,:,:,:,:,:)
       real*8,allocatable::X61(:,:,:,:)
       real*8,allocatable::Z105(:,:,:,:,:,:)
       real*8,allocatable::X62(:,:,:,:)
       real*8,allocatable::Z106(:,:,:,:,:,:)
       real*8,allocatable::Z117(:,:,:,:,:,:)
       real*8,allocatable::Z118(:,:,:,:,:,:)
       real*8,allocatable::Z119(:,:,:,:,:,:)
       real*8,allocatable::Z120(:,:,:,:,:,:)
       real*8,allocatable::X63(:,:,:,:)
       real*8,allocatable::Z121(:,:,:,:,:,:)
       real*8,allocatable::X64(:,:,:,:)
       real*8,allocatable::Z122(:,:,:,:,:,:)
       real*8,allocatable::X65(:,:,:,:)
       real*8,allocatable::Z123(:,:,:,:,:,:)
       real*8,allocatable::X66(:,:,:,:)
       real*8,allocatable::Z124(:,:,:,:,:,:)
       real*8,allocatable::X67(:,:,:,:)
       real*8,allocatable::Z125(:,:,:,:,:,:)
       real*8,allocatable::X68(:,:,:,:)
       real*8,allocatable::Z126(:,:,:,:,:,:)
       real*8,allocatable::Z152(:,:,:,:,:,:)
       real*8,allocatable::Z153(:,:,:,:,:,:)
       real*8,allocatable::Z154(:,:,:,:,:,:)
       real*8,allocatable::Z155(:,:,:,:,:,:)
       real*8,allocatable::Z156(:,:,:,:,:,:)
       real*8,allocatable::Z157(:,:,:,:,:,:)
       real*8,allocatable::Z158(:,:,:,:,:,:)
       real*8,allocatable::Z159(:,:,:,:,:,:)
       real*8,allocatable::Z160(:,:,:,:,:,:)
       real*8,allocatable::Z161(:,:,:,:,:,:)
       real*8,allocatable::Z162(:,:,:,:,:,:)
       real*8,allocatable::Z163(:,:,:,:,:,:)
       real*8,allocatable::Z164(:,:,:,:,:,:)
       real*8,allocatable::Z165(:,:,:,:,:,:)
       real*8,allocatable::Z166(:,:,:,:,:,:)
       real*8,allocatable::Z167(:,:,:,:,:,:)
       real*8,allocatable::Z168(:,:,:,:,:,:)
       real*8,allocatable::Z169(:,:,:,:,:,:)
       real*8,allocatable::Z170(:,:,:,:,:,:)
       real*8,allocatable::Z171(:,:,:,:,:,:)
       real*8,allocatable::X69(:,:,:,:)
       real*8,allocatable::Z172(:,:,:,:,:,:)
       real*8,allocatable::X70(:,:,:,:)
       real*8,allocatable::Z173(:,:,:,:,:,:)
       real*8,allocatable::X71(:,:,:,:)
       real*8,allocatable::Z174(:,:,:,:,:,:)
       real*8,allocatable::X72(:,:,:,:)
       real*8,allocatable::Z175(:,:,:,:,:,:)
       real*8,allocatable::X73(:,:,:,:)
       real*8,allocatable::Z184(:,:,:,:,:,:)
       real*8,allocatable::X74(:,:,:,:)
       real*8,allocatable::Z188(:,:,:,:,:,:)
       real*8,allocatable::Z189(:,:,:,:,:,:)
       real*8,allocatable::X75(:,:,:,:)
       real*8,allocatable::Z191(:,:,:,:,:,:)
       real*8,allocatable::Z193(:,:,:,:,:,:)
       real*8,allocatable::Z194(:,:,:,:,:,:)
       real*8,allocatable::X76(:,:,:,:)
       real*8,allocatable::Z212(:,:,:,:,:,:)
       real*8,allocatable::X77(:,:,:,:)
       real*8,allocatable::Z224(:,:,:,:,:,:)
       real*8,allocatable::X78(:,:,:,:)
       real*8,allocatable::Z240(:,:,:,:,:,:)
       real*8,allocatable::X79(:,:,:,:)
       real*8,allocatable::Z246(:,:,:,:,:,:)
       real*8,allocatable::X80(:,:,:,:)
       real*8,allocatable::Z255(:,:,:,:,:,:)
       real*8,allocatable::X81(:,:,:,:)
       real*8,allocatable::Z261(:,:,:,:,:,:)
       real*8,allocatable::Z375(:,:,:,:,:,:)
       real*8,allocatable::Z376(:,:,:,:,:,:)
C
       allocate(V3D(M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       V3D=0.0d0
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M1+1:N2))
       call reorder4231(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q1(N0+1:M1,M1+1:N2))
       I1=K8*K5
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q1)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X32(N0+1:M1,M1+1:N2))
       X32=0.0d0
       X32=X32+Q1
       deallocate(Q1)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M1+1:N2))
       call reorder4231(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q2(M1+1:N2,M1+1:N2))
       I1=K8*K8
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q2)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X33(M1+1:N2,M1+1:N2))
       X33=0.0d0
       X33=X33+Q2
       deallocate(Q2)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M2+1:N3))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,M2,N3,M2,N3,VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q3(M2+1:N3,M2+1:N3))
       I1=K6*K6
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q3)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X34(M2+1:N3,M2+1:N3))
       X34=0.0d0
       X34=X34+Q3
       deallocate(Q3)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,M2+1:N3))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,M2,M2,N3,VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q4(N2+1:M2,M2+1:N3))
       I1=K6*K0
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q4)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X35(N2+1:M2,M2+1:N3))
       X35=0.0d0
       X35=X35+Q4
       deallocate(Q4)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,N2+1:M2))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,M2,N3,N2,M2,VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q5(M2+1:N3,N2+1:M2))
       I1=K0*K6
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q5)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X36(M2+1:N3,N2+1:M2))
       X36=0.0d0
       X36=X36+Q5
       deallocate(Q5)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,N2+1:M2))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,M2,N2,M2,VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q6(N2+1:M2,N2+1:M2))
       I1=K0*K0
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q6)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X37(N2+1:M2,N2+1:M2))
       X37=0.0d0
       X37=X37+Q6
       deallocate(Q6)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,N2+1:M2))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,M2,N3,N2,M2,VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q7(M2+1:N3,N2+1:M2))
       I1=K0*K6
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q7)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X38(M2+1:N3,N2+1:M2))
       X38=0.0d0
       X38=X38+Q7
       deallocate(Q7)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,N2+1:M2))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,M2,N2,M2,VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q8(N2+1:M2,N2+1:M2))
       I1=K0*K0
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q8)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X39(N2+1:M2,N2+1:M2))
       X39=0.0d0
       X39=X39+Q8
       deallocate(Q8)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,M1,N2,M1,N2,VCHHHH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S1(M2+1:N3,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S1)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X1(N0+1:N2,M2+1:N3,M1+1:N2,M1+1:N2))
       X1=0.0d0
       call
     & sum2134(N0,N2,M2,N3,M1,N2,M1,N2,X1,S1,-1.000)
       deallocate(S1)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,M1,N2,M1,N2,VCHHHH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S2(N2+1:M2,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S2)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X40(N0+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       X40=0.0d0
       call
     & sum2134(N0,N2,N2,M2,M1,N2,M1,N2,X40,S2, 1.000)
       deallocate(S2)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder4312(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,M1,N2,M1,N2,VCHHHH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S3(N2+1:M2,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S3)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X41(N0+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       X41=0.0d0
       call
     & sum2134(N0,N2,N2,M2,M1,N2,M1,N2,X41,S3, 1.000)
       deallocate(S3)
C
       allocate(D1(N2+1:N3,N0+1:N2,M2+1:N3,M1+1:N2))
       call reorder1432(N2,N3,N0,N2,N2,N3,N0,N2,
     & N2,N3,N0,N2,M2,N3,M1,N2,VCHPHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S4(M1+1:N2,N0+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S4)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X42(N0+1:N2,M2+1:N3,M1+1:N2,M1+1:N2))
       X42=0.0d0
       call
     & sum3124(N0,N2,M2,N3,M1,N2,M1,N2,X42,S4, 1.000)
       deallocate(S4)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:M2,M1+1:N2))
       call reorder1432(N2,N3,N0,N2,N2,N3,N0,N2,
     & N2,N3,N0,N2,N2,M2,M1,N2,VCHPHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S5(M1+1:N2,N0+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S5)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X43(N0+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       X43=0.0d0
       call
     & sum3124(N0,N2,N2,M2,M1,N2,M1,N2,X43,S5, 1.000)
       deallocate(S5)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:M2,M1+1:N2))
       call reorder1432(N2,N3,N0,N2,N2,N3,N0,N2,
     & N2,N3,N0,N2,N2,M2,M1,N2,VCHPHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S6(M1+1:N2,N0+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S6)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X44(N0+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       X44=0.0d0
       call
     & sum3124(N0,N2,N2,M2,M1,N2,M1,N2,X44,S6, 1.000)
       deallocate(S6)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder4132(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,VCHPHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S7(N2+1:M2,N2+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S7)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X45(N2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       X45=0.0d0
       call
     & sum3124(N2,N3,M2,N3,N2,M2,M1,N2,X45,S7, 1.000)
       deallocate(S7)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder4132(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,VCHPHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S8(N2+1:M2,N2+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S8)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X46(N2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       X46=0.0d0
       call
     & sum3124(N2,N3,M2,N3,N2,M2,M1,N2,X46,S8, 1.000)
       deallocate(S8)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder4132(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,VCHPHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S9(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S9)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,M1,N2,X45,S9,-1.000)
       deallocate(S9)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder4132(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,VCHPHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S10(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S10)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,M1,N2,X46,S10,-1.000)
       deallocate(S10)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder4132(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,VCHPHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S11(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S11)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X3(N2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       X3=0.0d0
       call
     & sum3124(N2,N3,N2,M2,N2,M2,M1,N2,X3,S11,-1.000)
       deallocate(S11)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder4132(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,VCHPHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S12(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S12)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,N2,M2,M1,N2,X3,S12, 1.000)
       deallocate(S12)
C
       allocate(D1(N2+1:N3,N2+1:N3,M2+1:N3,N2+1:M2))
       call reorder1234(N2,N3,N2,N3,N2,N3,N2,M2,
     & N2,N3,N2,N3,M2,N3,N2,M2,VCAPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S13(M1+1:N2,N2+1:N3,M2+1:N3,N2+1:M2))
       I1=K0*K6*K4
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S13)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N2,N3,M2,N3,N2,M2,M1,N2,X45,S13, 1.000)
       deallocate(S13)
C
       allocate(D1(N2+1:N3,N2+1:N3,M2+1:N3,N2+1:M2))
       call reorder1234(N2,N3,N2,N3,N2,N3,N2,M2,
     & N2,N3,N2,N3,M2,N3,N2,M2,VCAPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S14(M1+1:N2,N2+1:N3,M2+1:N3,N2+1:M2))
       I1=K0*K6*K4
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S14)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N2,N3,M2,N3,N2,M2,M1,N2,X46,S14, 1.000)
       deallocate(S14)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:M2,N2+1:M2))
       call reorder1234(N2,N3,N2,N3,N2,N3,N2,M2,
     & N2,N3,N2,N3,N2,M2,N2,M2,VCAPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S15(M1+1:N2,N2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K4
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S15)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N2,N3,N2,M2,N2,M2,M1,N2,X3,S15,-1.000)
       deallocate(S15)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,M1,M2,N3,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S16(M2+1:N3,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S16)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X4(N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N2))
       X4=0.0d0
       call
     & sum3124(N0,M1,M2,N3,M2,N3,M1,N2,X4,S16,-1.000)
       deallocate(S16)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,M2,N3,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S17(M2+1:N3,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S17)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X5(M1+1:N1,M2+1:N3,M2+1:N3,M1+1:N2))
       X5=0.0d0
       call
     & sum3124(M1,N1,M2,N3,M2,N3,M1,N2,X5,S17,-1.000)
       deallocate(S17)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,M1,N1,M2,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S18(M2+1:N3,N0+1:M1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S18)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X6(N0+1:M1,N1+1:M2,M2+1:N3,M1+1:N2))
       X6=0.0d0
       call
     & sum3124(N0,M1,N1,M2,M2,N3,M1,N2,X6,S18,-1.000)
       deallocate(S18)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,N1,M2,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S19(M2+1:N3,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S19)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X7(M1+1:N1,N1+1:M2,M2+1:N3,M1+1:N2))
       X7=0.0d0
       call
     & sum3124(M1,N1,N1,M2,M2,N3,M1,N2,X7,S19,-1.000)
       deallocate(S19)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,M1,M2,N3,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S20(N2+1:M2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S20)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X47(N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       X47=0.0d0
       call
     & sum3124(N0,M1,M2,N3,N2,M2,M1,N2,X47,S20, 1.000)
       deallocate(S20)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,M2,N3,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S21(N2+1:M2,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S21)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X48(M1+1:N1,M2+1:N3,N2+1:M2,M1+1:N2))
       X48=0.0d0
       call
     & sum3124(M1,N1,M2,N3,N2,M2,M1,N2,X48,S21, 1.000)
       deallocate(S21)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,M1,N1,M2,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S22(N2+1:M2,N0+1:M1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S22)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X49(N0+1:M1,N1+1:M2,N2+1:M2,M1+1:N2))
       X49=0.0d0
       call
     & sum3124(N0,M1,N1,M2,N2,M2,M1,N2,X49,S22, 1.000)
       deallocate(S22)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,N1,M2,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S23(N2+1:M2,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S23)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X50(M1+1:N1,N1+1:M2,N2+1:M2,M1+1:N2))
       X50=0.0d0
       call
     & sum3124(M1,N1,N1,M2,N2,M2,M1,N2,X50,S23, 1.000)
       deallocate(S23)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,M1,M2,N3,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S24(N2+1:M2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S24)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X51(N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       X51=0.0d0
       call
     & sum3124(N0,M1,M2,N3,N2,M2,M1,N2,X51,S24, 1.000)
       deallocate(S24)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,M2,N3,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S25(N2+1:M2,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S25)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X52(M1+1:N1,M2+1:N3,N2+1:M2,M1+1:N2))
       X52=0.0d0
       call
     & sum3124(M1,N1,M2,N3,N2,M2,M1,N2,X52,S25, 1.000)
       deallocate(S25)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,M1,N1,M2,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S26(N2+1:M2,N0+1:M1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S26)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X53(N0+1:M1,N1+1:M2,N2+1:M2,M1+1:N2))
       X53=0.0d0
       call
     & sum3124(N0,M1,N1,M2,N2,M2,M1,N2,X53,S26, 1.000)
       deallocate(S26)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,N1,M2,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S27(N2+1:M2,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S27)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X54(M1+1:N1,N1+1:M2,N2+1:M2,M1+1:N2))
       X54=0.0d0
       call
     & sum3124(M1,N1,N1,M2,N2,M2,M1,N2,X54,S27, 1.000)
       deallocate(S27)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,M2+1:N3))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,M1,M2,N3,M2,N3,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S28(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S28)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,M2,N3,M2,N3,M1,N2,X4,S28, 1.000)
       deallocate(S28)
C
       allocate(D1(N2+1:N3,M1+1:N1,M2+1:N3,M2+1:N3))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,M1,N1,M2,N3,M2,N3,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S29(M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K7
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S29)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,M2,N3,M2,N3,M1,N2,X5,S29, 1.000)
       deallocate(S29)
C
       allocate(D1(N2+1:N3,N0+1:M1,N1+1:M2,M2+1:N3))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,M1,N1,M2,M2,N3,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S30(M1+1:N2,N0+1:M1,N1+1:M2,M2+1:N3))
       I1=K6*K9*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S30)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,N1,M2,M2,N3,M1,N2,X6,S30, 1.000)
       deallocate(S30)
C
       allocate(D1(N2+1:N3,M1+1:N1,N1+1:M2,M2+1:N3))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,M1,N1,N1,M2,M2,N3,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S31(M1+1:N2,M1+1:N1,N1+1:M2,M2+1:N3))
       I1=K6*K9*K7
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S31)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,N1,M2,M2,N3,M1,N2,X7,S31, 1.000)
       deallocate(S31)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,M1,M2,N3,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S32(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S32)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,M2,N3,N2,M2,M1,N2,X47,S32,-1.000)
       deallocate(S32)
C
       allocate(D1(N2+1:N3,M1+1:N1,M2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,M1,N1,M2,N3,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S33(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K7
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S33)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,M2,N3,N2,M2,M1,N2,X48,S33,-1.000)
       deallocate(S33)
C
       allocate(D1(N2+1:N3,N0+1:M1,N1+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,M1,N1,M2,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S34(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:M2))
       I1=K0*K9*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S34)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,N1,M2,N2,M2,M1,N2,X49,S34,-1.000)
       deallocate(S34)
C
       allocate(D1(N2+1:N3,M1+1:N1,N1+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,M1,N1,N1,M2,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S35(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:M2))
       I1=K0*K9*K7
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S35)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,N1,M2,N2,M2,M1,N2,X50,S35,-1.000)
       deallocate(S35)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,M1,M2,N3,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S36(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S36)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,M2,N3,N2,M2,M1,N2,X51,S36,-1.000)
       deallocate(S36)
C
       allocate(D1(N2+1:N3,M1+1:N1,M2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,M1,N1,M2,N3,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S37(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K7
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S37)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,M2,N3,N2,M2,M1,N2,X52,S37,-1.000)
       deallocate(S37)
C
       allocate(D1(N2+1:N3,N0+1:M1,N1+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,M1,N1,M2,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S38(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:M2))
       I1=K0*K9*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S38)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,N1,M2,N2,M2,M1,N2,X53,S38,-1.000)
       deallocate(S38)
C
       allocate(D1(N2+1:N3,M1+1:N1,N1+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,M1,N1,N1,M2,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S39(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:M2))
       I1=K0*K9*K7
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S39)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,N1,M2,N2,M2,M1,N2,X54,S39,-1.000)
       deallocate(S39)
C
       allocate(B1(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,FBHP,B1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(Q9(M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q9)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X12(N0+1:M1,M1+1:N2))
       X12=0.0d0
       call
     & sum21(N0,M1,M1,N2,X12,Q9, 1.000)
       deallocate(Q9)
C
       allocate(B1(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,FBHP,B1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(Q10(M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q10)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X13(M1+1:N2,M1+1:N2))
       X13=0.0d0
       call
     & sum21(M1,N2,M1,N2,X13,Q10, 1.000)
       deallocate(Q10)
C
       allocate(B1(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,FBHP,B1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(Q11(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,B1,B2,Q11)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X14(M2+1:N3,M2+1:N3))
       X14=0.0d0
       call
     & sum21(M2,N3,M2,N3,X14,Q11,-1.000)
       deallocate(Q11)
C
       allocate(B1(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,FBHP,B1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(Q12(M2+1:N3,N2+1:M2))
       I1=K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,B1,B2,Q12)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X15(N2+1:M2,M2+1:N3))
       X15=0.0d0
       call
     & sum21(N2,M2,M2,N3,X15,Q12,-1.000)
       deallocate(Q12)
C
       allocate(B1(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,FBHP,B1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(Q13(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,B1,B2,Q13)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X16(M2+1:N3,N2+1:M2))
       X16=0.0d0
       call
     & sum21(M2,N3,N2,M2,X16,Q13,-1.000)
       deallocate(Q13)
C
       allocate(B1(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,FBHP,B1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(Q14(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,B1,B2,Q14)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X17(N2+1:M2,N2+1:M2))
       X17=0.0d0
       call
     & sum21(N2,M2,N2,M2,X17,Q14,-1.000)
       deallocate(Q14)
C
       allocate(B1(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,FBHP,B1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(Q15(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,B1,B2,Q15)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X18(M2+1:N3,N2+1:M2))
       X18=0.0d0
       call
     & sum21(M2,N3,N2,M2,X18,Q15,-1.000)
       deallocate(Q15)
C
       allocate(B1(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,FBHP,B1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(Q16(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,B1,B2,Q16)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X19(N2+1:M2,N2+1:M2))
       X19=0.0d0
       call
     & sum21(N2,M2,N2,M2,X19,Q16,-1.000)
       deallocate(Q16)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:M1,M1+1:N2))
       call reorder1342(N2,N3,N0,N2,N0,N2,N0,N2,
     & N2,N3,N0,M1,N0,M1,M1,N2,VCHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S40(M1+1:N2,N0+1:M1,N0+1:M1,M1+1:N2))
       I1=K8*K5*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S40)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder2314(M1,N2,N0,M1,N0,M1,M1,N2,
     & N0,M1,N0,M1,M1,N2,M1,N2,S40,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N0,M1,M2,N3,N2,M2,N2,M2,M1,N2,t3D,F2)
       allocate(Z92(M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8
       I2=K8*K0*K0*K6
       I3=K5*K5
       call EGEMM(I1,I2,I3,D1,F2,Z92)
       deallocate(D1)
       deallocate(F2)
C
       V3D=V3D+0.500*Z92
       call
     & sum123546(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z92,-0.500)
       call
     & sum123465(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z92,-0.500)
       call
     & sum123564(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z92, 0.500)
       call
     & sum123645(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z92, 0.500)
       call
     & sum123654(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z92,-0.500)
       deallocate(Z92)
       deallocate(S40)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder1342(N2,N3,N0,N2,N0,N2,N0,N2,
     & N2,N3,N0,M1,M1,N2,M1,N2,VCHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S41(M1+1:N2,N0+1:M1,M1+1:N2,M1+1:N2))
       I1=K8*K8*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S41)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder2314(M1,N2,N0,M1,M1,N2,M1,N2,
     & N0,M1,M1,N2,M1,N2,M1,N2,S41,D1)
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M1,N2,M2,N3,N2,M2,N2,M2,M1,N2,t3D,F2)
       allocate(Z93(M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8
       I2=K8*K0*K0*K6
       I3=K8*K5
       call EGEMM(I1,I2,I3,D1,F2,Z93)
       deallocate(D1)
       deallocate(F2)
C
       V3D=V3D+Z93
       call
     & sum123546(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z93,-1.000)
       call
     & sum123465(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z93,-1.000)
       call
     & sum123564(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z93, 1.000)
       call
     & sum123645(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z93, 1.000)
       call
     & sum123654(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z93,-1.000)
       deallocate(Z93)
       deallocate(S41)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder1342(N2,N3,N0,N2,N0,N2,N0,N2,
     & N2,N3,M1,N2,M1,N2,M1,N2,VCHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S42(M1+1:N2,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S42)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder2314(M1,N2,M1,N2,M1,N2,M1,N2,
     & M1,N2,M1,N2,M1,N2,M1,N2,S42,D1)
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M1,N2,M2,N3,N2,M2,N2,M2,M1,N2,t3D,F2)
       allocate(Z94(M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8
       I2=K8*K0*K0*K6
       I3=K8*K8
       call EGEMM(I1,I2,I3,D1,F2,Z94)
       deallocate(D1)
       deallocate(F2)
C
       V3D=V3D+0.500*Z94
       call
     & sum123546(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z94,-0.500)
       call
     & sum123465(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z94,-0.500)
       call
     & sum123564(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z94, 0.500)
       call
     & sum123645(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z94, 0.500)
       call
     & sum123654(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z94,-0.500)
       deallocate(Z94)
       deallocate(S42)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,M1,M2,N3,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S43(M2+1:N3,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S43)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X23(N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N2))
       X23=0.0d0
       call
     & sum3124(N0,M1,M2,N3,M2,N3,M1,N2,X23,S43,-1.000)
       deallocate(S43)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,M1,N2,M2,N3,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S44(M2+1:N3,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S44)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X24(M1+1:N2,M2+1:N3,M2+1:N3,M1+1:N2))
       X24=0.0d0
       call
     & sum3124(M1,N2,M2,N3,M2,N3,M1,N2,X24,S44,-1.000)
       deallocate(S44)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,M1+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,M1,N2,M2,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S45(M2+1:N3,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S45)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X25(N0+1:M1,N2+1:M2,M2+1:N3,M1+1:N2))
       X25=0.0d0
       call
     & sum3124(N0,M1,N2,M2,M2,N3,M1,N2,X25,S45,-1.000)
       deallocate(S45)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,M1,N2,N2,M2,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S46(M2+1:N3,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K8
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S46)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X26(M1+1:N2,N2+1:M2,M2+1:N3,M1+1:N2))
       X26=0.0d0
       call
     & sum3124(M1,N2,N2,M2,M2,N3,M1,N2,X26,S46,-1.000)
       deallocate(S46)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,M1,M2,N3,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S47(N2+1:M2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S47)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X55(N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       X55=0.0d0
       call
     & sum3124(N0,M1,M2,N3,N2,M2,M1,N2,X55,S47, 1.000)
       deallocate(S47)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,M1,N2,M2,N3,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S48(N2+1:M2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S48)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X56(M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       X56=0.0d0
       call
     & sum3124(M1,N2,M2,N3,N2,M2,M1,N2,X56,S48, 1.000)
       deallocate(S48)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,M1+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,M1,N2,M2,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S49(N2+1:M2,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S49)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X57(N0+1:M1,N2+1:M2,N2+1:M2,M1+1:N2))
       X57=0.0d0
       call
     & sum3124(N0,M1,N2,M2,N2,M2,M1,N2,X57,S49, 1.000)
       deallocate(S49)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,M1,N2,N2,M2,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S50(N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S50)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X58(M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       X58=0.0d0
       call
     & sum3124(M1,N2,N2,M2,N2,M2,M1,N2,X58,S50, 1.000)
       deallocate(S50)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder4312(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,M1,M2,N3,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S51(N2+1:M2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S51)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X59(N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       X59=0.0d0
       call
     & sum3124(N0,M1,M2,N3,N2,M2,M1,N2,X59,S51, 1.000)
       deallocate(S51)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder4312(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,M1,N2,M2,N3,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S52(N2+1:M2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S52)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X60(M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       X60=0.0d0
       call
     & sum3124(M1,N2,M2,N3,N2,M2,M1,N2,X60,S52, 1.000)
       deallocate(S52)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,M1+1:N2))
       call reorder4312(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,M1,N2,M2,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S53(N2+1:M2,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S53)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X61(N0+1:M1,N2+1:M2,N2+1:M2,M1+1:N2))
       X61=0.0d0
       call
     & sum3124(N0,M1,N2,M2,N2,M2,M1,N2,X61,S53, 1.000)
       deallocate(S53)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       call reorder4312(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,M1,N2,N2,M2,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S54(N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S54)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X62(M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       X62=0.0d0
       call
     & sum3124(M1,N2,N2,M2,N2,M2,M1,N2,X62,S54, 1.000)
       deallocate(S54)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M1+1:N2))
       call reorder3142(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q17(N0+1:M1,M1+1:N2))
       I1=K8*K5
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q17)
       deallocate(D1)
       deallocate(B2)
C
       X12=X12+Q17
       deallocate(Q17)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder3142(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q18(M1+1:N2,M1+1:N2))
       I1=K8*K8
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q18)
       deallocate(D1)
       deallocate(B2)
C
       X13=X13+Q18
       deallocate(Q18)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,M2+1:N3))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,M1,M2,N3,M2,N3,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S55(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S55)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,M2,N3,M2,N3,M1,N2,X23,S55,-1.000)
       deallocate(S55)
C
       allocate(D1(N2+1:N3,M1+1:N2,M2+1:N3,M2+1:N3))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,M1,N2,M2,N3,M2,N3,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S56(M1+1:N2,M1+1:N2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S56)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N2,M2,N3,M2,N3,M1,N2,X24,S56,-1.000)
       deallocate(S56)
C
       allocate(D1(N2+1:N3,N0+1:M1,N2+1:M2,M2+1:N3))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,M1,N2,M2,M2,N3,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S57(M1+1:N2,N0+1:M1,N2+1:M2,M2+1:N3))
       I1=K6*K0*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S57)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,N2,M2,M2,N3,M1,N2,X25,S57,-1.000)
       deallocate(S57)
C
       allocate(D1(N2+1:N3,M1+1:N2,N2+1:M2,M2+1:N3))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,M1,N2,N2,M2,M2,N3,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S58(M1+1:N2,M1+1:N2,N2+1:M2,M2+1:N3))
       I1=K6*K0*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S58)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N2,N2,M2,M2,N3,M1,N2,X26,S58,-1.000)
       deallocate(S58)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,M1,M2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S59(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S59)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,M2,N3,N2,M2,M1,N2,X55,S59, 1.000)
       deallocate(S59)
C
       allocate(D1(N2+1:N3,M1+1:N2,M2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,M1,N2,M2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S60(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S60)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N2,M2,N3,N2,M2,M1,N2,X56,S60, 1.000)
       deallocate(S60)
C
       allocate(D1(N2+1:N3,N0+1:M1,N2+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,M1,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S61(M1+1:N2,N0+1:M1,N2+1:M2,N2+1:M2))
       I1=K0*K0*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S61)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,N2,M2,N2,M2,M1,N2,X57,S61, 1.000)
       deallocate(S61)
C
       allocate(D1(N2+1:N3,M1+1:N2,N2+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,M1,N2,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S62(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S62)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N2,N2,M2,N2,M2,M1,N2,X58,S62, 1.000)
       deallocate(S62)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,M1,M2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S63(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S63)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,M1,M2,N3,N2,M2,
     & N0,M1,M2,N3,N2,M2,M1,N2,S63,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z117(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z117)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z117, 1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z117,-1.000)
       call
     & sum125634(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z117, 1.000)
       deallocate(Z117)
       deallocate(S63)
C
       allocate(D1(N2+1:N3,M1+1:N2,M2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,M1,N2,M2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S64(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S64)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder2341(M1,N2,M1,N2,M2,N3,N2,M2,
     & M1,N2,M2,N3,N2,M2,M1,N2,S64,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z118(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z118)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z118, 1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z118,-1.000)
       call
     & sum125634(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z118, 1.000)
       deallocate(Z118)
       deallocate(S64)
C
       allocate(D1(N2+1:N3,N0+1:M1,N2+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,M1,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S65(M1+1:N2,N0+1:M1,N2+1:M2,N2+1:M2))
       I1=K0*K0*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S65)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,M1,N2,M2,N2,M2,
     & N0,M1,N2,M2,N2,M2,M1,N2,S65,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z119(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z119)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z119,-1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z119, 1.000)
       call
     & sum125634(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z119,-1.000)
       deallocate(Z119)
       deallocate(S65)
C
       allocate(D1(N2+1:N3,M1+1:N2,N2+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,M1,N2,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S66(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S66)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder2341(M1,N2,M1,N2,N2,M2,N2,M2,
     & M1,N2,N2,M2,N2,M2,M1,N2,S66,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z120(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z120)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z120,-1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z120, 1.000)
       call
     & sum125634(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z120,-1.000)
       deallocate(Z120)
       deallocate(S66)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,M2+1:N3))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,M2,N3,M2,N3,M2,N3,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S67(N2+1:M2,M2+1:N3,M2+1:N3,M2+1:N3))
       I1=K6*K6*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S67)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X63(M2+1:N3,M2+1:N3,M2+1:N3,N2+1:M2))
       X63=0.0d0
       call
     & sum4123(M2,N3,M2,N3,M2,N3,N2,M2,X63,S67, 1.000)
       deallocate(S67)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,M2+1:N3))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,M2,N3,N2,M2,M2,N3,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S68(N2+1:M2,M2+1:N3,N2+1:M2,M2+1:N3))
       I1=K6*K0*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S68)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X64(M2+1:N3,N2+1:M2,M2+1:N3,N2+1:M2))
       X64=0.0d0
       call
     & sum4123(M2,N3,N2,M2,M2,N3,N2,M2,X64,S68, 1.000)
       deallocate(S68)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,M2+1:N3))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,M2,N2,M2,M2,N3,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S69(N2+1:M2,N2+1:M2,N2+1:M2,M2+1:N3))
       I1=K6*K0*K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S69)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X65(N2+1:M2,N2+1:M2,M2+1:N3,N2+1:M2))
       X65=0.0d0
       call
     & sum4123(N2,M2,N2,M2,M2,N3,N2,M2,X65,S69, 1.000)
       deallocate(S69)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,M2+1:N3))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,M2,N3,M2,N3,M2,N3,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S70(N2+1:M2,M2+1:N3,M2+1:N3,M2+1:N3))
       I1=K6*K6*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S70)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X66(M2+1:N3,M2+1:N3,M2+1:N3,N2+1:M2))
       X66=0.0d0
       call
     & sum4123(M2,N3,M2,N3,M2,N3,N2,M2,X66,S70, 1.000)
       deallocate(S70)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,M2+1:N3))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,M2,N3,N2,M2,M2,N3,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S71(N2+1:M2,M2+1:N3,N2+1:M2,M2+1:N3))
       I1=K6*K0*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S71)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X67(M2+1:N3,N2+1:M2,M2+1:N3,N2+1:M2))
       X67=0.0d0
       call
     & sum4123(M2,N3,N2,M2,M2,N3,N2,M2,X67,S71, 1.000)
       deallocate(S71)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,M2+1:N3))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,M2,N2,M2,M2,N3,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S72(N2+1:M2,N2+1:M2,N2+1:M2,M2+1:N3))
       I1=K6*K0*K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S72)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X68(N2+1:M2,N2+1:M2,M2+1:N3,N2+1:M2))
       X68=0.0d0
       call
     & sum4123(N2,M2,N2,M2,M2,N3,N2,M2,X68,S72, 1.000)
       deallocate(S72)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,M2+1:N3))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,M2,N3,M2,N3,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q19(M2+1:N3,M2+1:N3))
       I1=K6*K6
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q19)
       deallocate(D1)
       deallocate(B2)
C
       X14=X14-Q19
       deallocate(Q19)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M2+1:N3))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,M2,M2,N3,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q20(N2+1:M2,M2+1:N3))
       I1=K6*K0
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q20)
       deallocate(D1)
       deallocate(B2)
C
       X15=X15-Q20
       deallocate(Q20)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,M2,N3,M2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S73(M2+1:N3,M2+1:N3,M2+1:N3,N2+1:M2))
       I1=K0*K6*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S73)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,M2,N3,M2,N3,N2,M2,X63,S73,-1.000)
       deallocate(S73)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,M2,N3,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S74(M2+1:N3,M2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S74)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,N2,M2,M2,N3,N2,M2,X64,S74,-1.000)
       deallocate(S74)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,M2,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S75(M2+1:N3,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S75)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,M2,N2,M2,M2,N3,N2,M2,X65,S75,-1.000)
       deallocate(S75)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,M2,N3,M2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S76(M2+1:N3,M2+1:N3,M2+1:N3,N2+1:M2))
       I1=K0*K6*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S76)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,M2,N3,M2,N3,N2,M2,X66,S76,-1.000)
       deallocate(S76)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,M2,N3,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S77(M2+1:N3,M2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S77)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,N2,M2,M2,N3,N2,M2,X67,S77,-1.000)
       deallocate(S77)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,M2,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S78(M2+1:N3,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S78)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,M2,N2,M2,M2,N3,N2,M2,X68,S78,-1.000)
       deallocate(S78)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,M2,N3,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S79(N2+1:M2,M2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S79)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X30(M2+1:N3,N2+1:M2,N2+1:M2,N2+1:M2))
       X30=0.0d0
       call
     & sum4123(M2,N3,N2,M2,N2,M2,N2,M2,X30,S79,-1.000)
       deallocate(S79)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,M2,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S80(N2+1:M2,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S80)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X31(N2+1:M2,N2+1:M2,N2+1:M2,N2+1:M2))
       X31=0.0d0
       call
     & sum4123(N2,M2,N2,M2,N2,M2,N2,M2,X31,S80,-1.000)
       deallocate(S80)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,M2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q21(M2+1:N3,N2+1:M2))
       I1=K0*K6
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q21)
       deallocate(D1)
       deallocate(B2)
C
       X16=X16-Q21
       deallocate(Q21)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q22(N2+1:M2,N2+1:M2))
       I1=K0*K0
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q22)
       deallocate(D1)
       deallocate(B2)
C
       X17=X17-Q22
       deallocate(Q22)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,M2,N3,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S81(N2+1:M2,M2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S81)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,N2,M2,N2,M2,N2,M2,X30,S81, 1.000)
       deallocate(S81)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,M2,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S82(N2+1:M2,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S82)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,M2,N2,M2,N2,M2,N2,M2,X31,S82, 1.000)
       deallocate(S82)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,M2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q23(M2+1:N3,N2+1:M2))
       I1=K0*K6
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q23)
       deallocate(D1)
       deallocate(B2)
C
       X18=X18-Q23
       deallocate(Q23)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q24(N2+1:M2,N2+1:M2))
       I1=K0*K0
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q24)
       deallocate(D1)
       deallocate(B2)
C
       X19=X19-Q24
       deallocate(Q24)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder4231(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,M1,N2,VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S83(M2+1:N3,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S83)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,M2,N3,M1,N2,M1,N2,X42,S83, 1.000)
       deallocate(S83)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder4231(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,M1,N2,VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S84(N2+1:M2,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S84)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,N2,M2,M1,N2,M1,N2,X43,S84, 1.000)
       deallocate(S84)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder4231(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,M1,N2,VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S85(N2+1:M2,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S85)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,N2,M2,M1,N2,M1,N2,X44,S85, 1.000)
       deallocate(S85)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,M2+1:N3))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,N3,M2,N3,VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S86(N2+1:M2,M1+1:N2,N2+1:N3,M2+1:N3))
       I1=K6*K4
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S86)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,N3,M2,N3,N2,M2,M1,N2,X45,S86,-1.000)
       deallocate(S86)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,M2+1:N3))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,N3,M2,N3,VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S87(N2+1:M2,M1+1:N2,N2+1:N3,M2+1:N3))
       I1=K6*K4
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S87)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,N3,M2,N3,N2,M2,M1,N2,X46,S87,-1.000)
       deallocate(S87)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,N3,N2,M2,VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S88(M2+1:N3,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S88)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,M2,N3,N2,M2,M1,N2,X45,S88, 1.000)
       deallocate(S88)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,N3,N2,M2,VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S89(M2+1:N3,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S89)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,M2,N3,N2,M2,M1,N2,X46,S89, 1.000)
       deallocate(S89)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,N3,N2,M2,VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S90(N2+1:M2,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S90)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,N3,N2,M2,N2,M2,M1,N2,X3,S90, 1.000)
       deallocate(S90)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,N3,N2,M2,VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S91(N2+1:M2,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S91)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,N2,M2,N2,M2,M1,N2,X3,S91,-1.000)
       deallocate(S91)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S92(M2+1:N3,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S92)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3412(M2,N3,M1,N2,N0,M1,M2,N3,
     & N0,M1,M2,N3,M2,N3,M1,N2,S92,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z152(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K8*K8*K0*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z152)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z152, 1.000)
       call
     & sum234615(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z152,-1.000)
       call
     & sum235614(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z152, 1.000)
       deallocate(Z152)
       deallocate(S92)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S93(M2+1:N3,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S93)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3412(M2,N3,M1,N2,M1,N1,M2,N3,
     & M1,N1,M2,N3,M2,N3,M1,N2,S93,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z153(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K8*K8*K0*K0
       I3=K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z153)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z153, 1.000)
       call
     & sum234615(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z153,-1.000)
       call
     & sum235614(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z153, 1.000)
       deallocate(Z153)
       deallocate(S93)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S94(M2+1:N3,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S94)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,N1+1:M2,M2+1:N3,M1+1:N2))
       call reorder3412(M2,N3,M1,N2,N0,M1,N1,M2,
     & N0,M1,N1,M2,M2,N3,M1,N2,S94,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N1,M2,N2,M2,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z154(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K8*K8*K0*K0
       I3=K9*K5
       call EGEMM(I1,I2,I3,D1,F2,Z154)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z154, 1.000)
       call
     & sum234615(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z154,-1.000)
       call
     & sum235614(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z154, 1.000)
       deallocate(Z154)
       deallocate(S94)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S95(M2+1:N3,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S95)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,N1+1:M2,M2+1:N3,M1+1:N2))
       call reorder3412(M2,N3,M1,N2,M1,N1,N1,M2,
     & M1,N1,N1,M2,M2,N3,M1,N2,S95,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N1,M2,N2,M2,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z155(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K8*K8*K0*K0
       I3=K9*K7
       call EGEMM(I1,I2,I3,D1,F2,Z155)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z155, 1.000)
       call
     & sum234615(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z155,-1.000)
       call
     & sum235614(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z155, 1.000)
       deallocate(Z155)
       deallocate(S95)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S96(N2+1:M2,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S96)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,M1,M2,N3,
     & N0,M1,M2,N3,N2,M2,M1,N2,S96,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z156(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z156)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z156,-1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z156, 1.000)
       call
     & sum135624(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z156,-1.000)
       deallocate(Z156)
       deallocate(S96)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S97(N2+1:M2,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S97)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,M1,N1,M2,N3,
     & M1,N1,M2,N3,N2,M2,M1,N2,S97,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z157(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z157)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z157,-1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z157, 1.000)
       call
     & sum135624(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z157,-1.000)
       deallocate(Z157)
       deallocate(S97)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S98(N2+1:M2,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S98)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,N1+1:M2,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,M1,N1,M2,
     & N0,M1,N1,M2,N2,M2,M1,N2,S98,D1)
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N1,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z158(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,D1,F2,Z158)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z158,-1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z158, 1.000)
       call
     & sum135624(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z158,-1.000)
       deallocate(Z158)
       deallocate(S98)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S99(N2+1:M2,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S99)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,N1+1:M2,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,M1,N1,N1,M2,
     & M1,N1,N1,M2,N2,M2,M1,N2,S99,D1)
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N1,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z159(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,D1,F2,Z159)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z159,-1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z159, 1.000)
       call
     & sum135624(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z159,-1.000)
       deallocate(Z159)
       deallocate(S99)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S100(N2+1:M2,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S100)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,M1,M2,N3,
     & N0,M1,M2,N3,N2,M2,M1,N2,S100,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z160(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z160)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z160, 1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z160,-1.000)
       call
     & sum125634(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z160, 1.000)
       deallocate(Z160)
       deallocate(S100)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S101(N2+1:M2,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S101)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,M1,N1,M2,N3,
     & M1,N1,M2,N3,N2,M2,M1,N2,S101,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z161(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z161)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z161, 1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z161,-1.000)
       call
     & sum125634(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z161, 1.000)
       deallocate(Z161)
       deallocate(S101)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S102(N2+1:M2,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S102)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,N1+1:M2,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,M1,N1,M2,
     & N0,M1,N1,M2,N2,M2,M1,N2,S102,D1)
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N1,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z162(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,D1,F2,Z162)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z162, 1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z162,-1.000)
       call
     & sum125634(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z162, 1.000)
       deallocate(Z162)
       deallocate(S102)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S103(N2+1:M2,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S103)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,N1+1:M2,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,M1,N1,N1,M2,
     & M1,N1,N1,M2,N2,M2,M1,N2,S103,D1)
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N1,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z163(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,D1,F2,Z163)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z163, 1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z163,-1.000)
       call
     & sum125634(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z163, 1.000)
       deallocate(Z163)
       deallocate(S103)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S104(M2+1:N3,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S104)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3412(M2,N3,M1,N2,N0,M1,M2,N3,
     & N0,M1,M2,N3,M2,N3,M1,N2,S104,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z164(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K8*K8*K0*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z164)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z164, 1.000)
       call
     & sum234615(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z164,-1.000)
       call
     & sum235614(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z164, 1.000)
       deallocate(Z164)
       deallocate(S104)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S105(M2+1:N3,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S105)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3412(M2,N3,M1,N2,M1,N2,M2,N3,
     & M1,N2,M2,N3,M2,N3,M1,N2,S105,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z165(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K8*K8*K0*K0
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z165)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z165, 1.000)
       call
     & sum234615(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z165,-1.000)
       call
     & sum235614(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z165, 1.000)
       deallocate(Z165)
       deallocate(S105)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S106(M2+1:N3,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S106)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder3412(M2,N3,M1,N2,N0,M1,N2,M2,
     & N0,M1,N2,M2,M2,N3,M1,N2,S106,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z166(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K8*K8*K0*K0
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z166)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z166, 1.000)
       call
     & sum234615(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z166,-1.000)
       call
     & sum235614(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z166, 1.000)
       deallocate(Z166)
       deallocate(S106)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S107(M2+1:N3,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S107)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder3412(M2,N3,M1,N2,M1,N2,N2,M2,
     & M1,N2,N2,M2,M2,N3,M1,N2,S107,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z167(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K8*K8*K0*K0
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z167)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z167, 1.000)
       call
     & sum234615(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z167,-1.000)
       call
     & sum235614(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z167, 1.000)
       deallocate(Z167)
       deallocate(S107)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S108(N2+1:M2,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S108)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,M1,M2,N3,
     & N0,M1,M2,N3,N2,M2,M1,N2,S108,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z168(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z168)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z168,-1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z168, 1.000)
       call
     & sum135624(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z168,-1.000)
       deallocate(Z168)
       deallocate(S108)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S109(N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S109)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,M1,N2,M2,N3,
     & M1,N2,M2,N3,N2,M2,M1,N2,S109,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z169(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z169)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z169,-1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z169, 1.000)
       call
     & sum135624(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z169,-1.000)
       deallocate(Z169)
       deallocate(S109)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S110(N2+1:M2,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S110)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,M1,N2,M2,
     & N0,M1,N2,M2,N2,M2,M1,N2,S110,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z170(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z170)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z170, 1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z170,-1.000)
       call
     & sum135624(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z170, 1.000)
       deallocate(Z170)
       deallocate(S110)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S111(N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S111)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,M1,N2,N2,M2,
     & M1,N2,N2,M2,N2,M2,M1,N2,S111,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z171(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z171)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z171, 1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z171,-1.000)
       call
     & sum135624(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z171, 1.000)
       deallocate(Z171)
       deallocate(S111)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S112(N2+1:M2,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S112)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X69(N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       X69=0.0d0
       call
     & sum3412(N0,M1,M2,N3,N2,M2,M1,N2,X69,S112, 1.000)
       deallocate(S112)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S113(N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S113)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X70(M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       X70=0.0d0
       call
     & sum3412(M1,N2,M2,N3,N2,M2,M1,N2,X70,S113, 1.000)
       deallocate(S113)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S114(N2+1:M2,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S114)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X71(N0+1:M1,N2+1:M2,N2+1:M2,M1+1:N2))
       X71=0.0d0
       call
     & sum3412(N0,M1,N2,M2,N2,M2,M1,N2,X71,S114, 1.000)
       deallocate(S114)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S115(N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S115)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X72(M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       X72=0.0d0
       call
     & sum3412(M1,N2,N2,M2,N2,M2,M1,N2,X72,S115, 1.000)
       deallocate(S115)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:M1))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,M1,VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,M1,N2,t2B,D2)
       allocate(Q25(M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8
       I3=K3*K4*K1
       call EGEMM(I1,I2,I3,D1,D2,Q25)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N0,M1,M1,N2,X32,Q25, 1.000)
       deallocate(Q25)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,M1,N2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,M1,N2,t2B,D2)
       allocate(Q26(M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8
       I3=K3*K4*K1
       call EGEMM(I1,I2,I3,D1,D2,Q26)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M1,N2,M1,N2,X33,Q26, 1.000)
       deallocate(Q26)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,M2,N3,t2B,D2)
       allocate(Q27(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q27)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M2,N3,M2,N3,X34,Q27,-1.000)
       deallocate(Q27)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,M2,N3,t2B,D2)
       allocate(Q28(M2+1:N3,N2+1:M2))
       I1=K0
       I2=K6
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q28)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N2,M2,M2,N3,X35,Q28,-1.000)
       deallocate(Q28)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,M2,t2B,D2)
       allocate(Q29(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q29)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M2,N3,N2,M2,X36,Q29,-1.000)
       deallocate(Q29)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,M2,t2B,D2)
       allocate(Q30(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q30)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N2,M2,N2,M2,X37,Q30,-1.000)
       deallocate(Q30)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,M2,t2B,D2)
       allocate(Q31(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q31)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M2,N3,N2,M2,X38,Q31,-1.000)
       deallocate(Q31)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,M2,t2B,D2)
       allocate(Q32(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q32)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N2,M2,N2,M2,X39,Q32,-1.000)
       deallocate(Q32)
C
       allocate(B1(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,FBHP,B1)
       allocate(D2(N0+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,M2,N2,M2,M1,N2,t2C,D2)
       allocate(S116(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K2
       call EGEMM(I1,I2,I3,B1,D2,S116)
       deallocate(B1)
       deallocate(D2)
C
       allocate(X73(N2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       X73=0.0d0
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X73,S116, 1.000)
       deallocate(S116)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,FBHP,B1)
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,M1,N2,M1,N2,t2C,D2)
       allocate(S117(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S117)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X40,S117, 1.000)
       deallocate(S117)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,FBHP,B1)
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,M1,N2,M1,N2,t2C,D2)
       allocate(S118(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S118)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X41,S118,-1.000)
       deallocate(S118)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,M1,N2,VCHHHP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,M2,N2,M2,t2C,D2)
       allocate(S119(N2+1:M2,N2+1:M2,N2+1:N3,M1+1:N2))
       I1=K8*K4
       I2=K0*K0
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S119)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N2,N3,N2,M2,N2,M2,M1,N2,X3,S119, 0.500)
       deallocate(S119)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,M1,N2,VCHHHP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,N2,M2,t2C,D2)
       allocate(S120(M2+1:N3,N2+1:M2,N2+1:N3,M1+1:N2))
       I1=K8*K4
       I2=K0*K6
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S120)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X74(N2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       X74=0.0d0
       call
     & sum2314(N2,N3,M2,N3,N2,M2,M1,N2,X74,S120, 1.000)
       deallocate(S120)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder4132(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,M1,N2,VCHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S121(N2+1:M2,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S121)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder3124(N2,M2,M1,N2,N0,N2,M1,N2,
     & N0,N2,N2,M2,M1,N2,M1,N2,S121,D1)
       allocate(D2(N0+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,M2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(Z189(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K0
       I2=K8*K0*K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,D2,Z189)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum125346(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z189,-1.000)
       call
     & sum135246(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z189, 1.000)
       call
     & sum134256(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z189,-1.000)
       call
     & sum124356(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z189, 1.000)
       call
     & sum126345(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z189, 1.000)
       call
     & sum136245(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z189,-1.000)
       call
     & sum134265(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z189, 1.000)
       call
     & sum124365(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z189,-1.000)
       call
     & sum126354(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z189,-1.000)
       call
     & sum136254(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z189, 1.000)
       call
     & sum135264(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z189,-1.000)
       call
     & sum125364(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z189, 1.000)
       deallocate(Z189)
       deallocate(S121)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder3142(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,M1,N2,VCHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S122(M2+1:N3,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S122)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,M2,N3,M1,N2,M1,N2,X42,S122, 1.000)
       deallocate(S122)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,M2+1:N3))
       call reorder4213(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,M2,N3,VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S123(N2+1:M2,M1+1:N2,N2+1:N3,M2+1:N3))
       I1=K6*K4
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S123)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X75(N2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       X75=0.0d0
       call
     & sum3412(N2,N3,M2,N3,N2,M2,M1,N2,X75,S123, 1.000)
       deallocate(S123)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,M2+1:N3))
       call reorder1243(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N2,N3,N0,N2,M2,N3,VCHPPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,M1,N2,M1,N2,t2C,D2)
       allocate(S124(M1+1:N2,M1+1:N2,N0+1:N2,M2+1:N3))
       I1=K6*K2
       I2=K8*K8
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S124)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,N2,M2,N3,M1,N2,M1,N2,X1,S124, 0.500)
       deallocate(S124)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder4213(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,M2,VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S125(N2+1:M2,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S125)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N2,N3,N2,M2,
     & N2,N3,N2,M2,N2,M2,M1,N2,S125,D1)
       allocate(D2(N2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,M2,N3,M1,N2,M1,N2,t2C,D2)
       allocate(Z193(M2+1:N3,M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K0
       I2=K8*K8*K6
       I3=K4
       call EGEMM(I1,I2,I3,D1,D2,Z193)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum156234(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z193,-1.000)
       call
     & sum156324(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z193, 1.000)
       call
     & sum146235(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z193, 1.000)
       call
     & sum146325(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z193,-1.000)
       call
     & sum145236(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z193,-1.000)
       call
     & sum145326(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z193, 1.000)
       deallocate(Z193)
       deallocate(S125)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,M2,VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S126(M2+1:N3,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S126)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3142(M2,N3,M1,N2,N2,N3,N2,M2,
     & N2,N3,M2,N3,N2,M2,M1,N2,S126,D1)
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,M1,N2,M1,N2,t2C,D2)
       allocate(Z194(N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K6
       I2=K8*K8*K0
       I3=K4
       call EGEMM(I1,I2,I3,D1,D2,Z194)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum356124(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z194,-1.000)
       call
     & sum256134(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z194, 1.000)
       call
     & sum346125(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z194, 1.000)
       call
     & sum246135(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z194,-1.000)
       call
     & sum345126(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z194,-1.000)
       call
     & sum245136(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z194, 1.000)
       deallocate(Z194)
       deallocate(S126)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:M2))
       call reorder1243(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N2,N3,N0,N2,N2,M2,VCHPPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,M1,N2,M1,N2,t2C,D2)
       allocate(S127(M1+1:N2,M1+1:N2,N0+1:N2,N2+1:M2))
       I1=K0*K2
       I2=K8*K8
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S127)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X2(N0+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       X2=0.0d0
       call
     & sum3412(N0,N2,N2,M2,M1,N2,M1,N2,X2,S127, 0.500)
       deallocate(S127)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N0,M1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N0,M1,M2,N3,N2,M2,N2,M2,M1,N2,t3C1,F2)
       allocate(S128(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S128)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X73,S128, 1.000)
       deallocate(S128)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N2,M2,N2,M2,M1,N2,t3C1,F2)
       allocate(S129(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S129)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X73,S129, 1.000)
       deallocate(S129)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N2,M2,N2,M2,M1,N2,t3C1,F2)
       allocate(S130(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S130)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X73,S130, 1.000)
       deallocate(S130)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N2,M2,N2,M2,M1,N2,t3C1,F2)
       allocate(S131(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S131)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X73,S131, 1.000)
       deallocate(S131)
C
       allocate(D1(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N0,M1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N0,M1,N1,M2,N2,M2,N2,M2,M1,N2,t3C1,F2)
       allocate(S132(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K9*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S132)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X73,S132, 1.000)
       deallocate(S132)
C
       allocate(D1(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,N1,M2,N2,M2,N2,M2,M1,N2,t3C1,F2)
       allocate(S133(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K9*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S133)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X73,S133, 1.000)
       deallocate(S133)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N1,N1,M2,N2,M2,N2,M2,M1,N2,t3C1,F2)
       allocate(S134(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S134)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X73,S134, 1.000)
       deallocate(S134)
C
       allocate(D1(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,N1,M2,N2,M2,N2,M2,M1,N2,t3C1,F2)
       allocate(S135(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K9*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S135)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X73,S135, 1.000)
       deallocate(S135)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N0,M1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,t3C1,F2)
       allocate(S136(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S136)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X75,S136,-1.000)
       deallocate(S136)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,t3C1,F2)
       allocate(S137(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S137)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X75,S137,-1.000)
       deallocate(S137)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,M2,N3,N2,M2,M1,N2,t3C1,F2)
       allocate(S138(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S138)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X75,S138,-1.000)
       deallocate(S138)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,M2,N3,N2,M2,M1,N2,t3C1,F2)
       allocate(S139(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S139)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X75,S139,-1.000)
       deallocate(S139)
C
       allocate(D1(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N0,M1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N0,M1,N1,M2,M2,N3,N2,M2,M1,N2,t3C1,F2)
       allocate(S140(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K9*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S140)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X75,S140,-1.000)
       deallocate(S140)
C
       allocate(D1(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,N1,M2,M2,N3,N2,M2,M1,N2,t3C1,F2)
       allocate(S141(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K9*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S141)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X75,S141,-1.000)
       deallocate(S141)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N1,N1,M2,M2,N3,N2,M2,M1,N2,t3C1,F2)
       allocate(S142(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S142)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X75,S142,-1.000)
       deallocate(S142)
C
       allocate(D1(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,N1,M2,M2,N3,N2,M2,M1,N2,t3C1,F2)
       allocate(S143(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K9*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S143)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X75,S143,-1.000)
       deallocate(S143)
C
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,M1,N2,M1,N2,t2C,D2)
       allocate(Z191(N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K6
       I2=K8*K8*K0
       I3=K4
       call EGEMM(I1,I2,I3,X75,D2,Z191)
       deallocate(D2)
C
       call
     & sum256134(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z191, 1.000)
       call
     & sum356124(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z191,-1.000)
       call
     & sum246135(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z191,-1.000)
       call
     & sum346125(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z191, 1.000)
       call
     & sum345126(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z191,-1.000)
       call
     & sum245136(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z191, 1.000)
       deallocate(Z191)
       deallocate(X75)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(S144(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S144)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X76(N0+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       X76=0.0d0
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X76,S144, 1.000)
       deallocate(S144)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(S145(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K6*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S145)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X76,S145, 1.000)
       deallocate(S145)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(S146(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S146)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X76,S146, 1.000)
       deallocate(S146)
C
       allocate(D1(M1+1:N1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,N2,M2,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(S147(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K6*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S147)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X76,S147, 1.000)
       deallocate(S147)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(S148(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S148)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X76,S148, 1.000)
       deallocate(S148)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,N1,M2,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(S149(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S149)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X76,S149, 1.000)
       deallocate(S149)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(S150(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S150)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X76,S150, 1.000)
       deallocate(S150)
C
       allocate(D1(M1+1:N1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,N2,M2,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,N1,M2,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(S151(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K9*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S151)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X76,S151, 1.000)
       deallocate(S151)
C
       allocate(D2(N0+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,M2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(Z212(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K0
       I2=K8*K0*K6
       I3=K2
       call EGEMM(I1,I2,I3,X76,D2,Z212)
       deallocate(D2)
C
       call
     & sum126345(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z212, 1.000)
       call
     & sum136245(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z212,-1.000)
       call
     & sum125346(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z212,-1.000)
       call
     & sum135246(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z212, 1.000)
       call
     & sum124356(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z212, 1.000)
       call
     & sum134256(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z212,-1.000)
       deallocate(Z212)
       deallocate(X76)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S152(M2+1:N3,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S152)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,M2,N3,M1,N2,X4,S152, 1.000)
       deallocate(S152)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S153(M2+1:N3,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S153)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,M2,N3,M2,N3,M1,N2,X5,S153, 1.000)
       deallocate(S153)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S154(M2+1:N3,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S154)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N1,M2,M2,N3,M1,N2,X6,S154, 1.000)
       deallocate(S154)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S155(M2+1:N3,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S155)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,N1,M2,M2,N3,M1,N2,X7,S155, 1.000)
       deallocate(S155)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder623145(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,M2,N3,M1,N2,M1,N2,t3C1,F2)
       allocate(S156(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S156)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X77(N0+1:N2,M2+1:N3,M1+1:N2,M1+1:N2))
       X77=0.0d0
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X77,S156, 1.000)
       deallocate(S156)
C
       allocate(D1(M1+1:N1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,N2,M2,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder623145(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,M2,N3,M2,N3,M1,N2,M1,N2,t3C1,F2)
       allocate(S157(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K6*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S157)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X77,S157, 1.000)
       deallocate(S157)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder613245(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,M2,N3,M1,N2,M1,N2,t3C3,F2)
       allocate(S158(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S158)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X77,S158,-1.000)
       deallocate(S158)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder613245(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,N1,M2,M2,N3,M1,N2,M1,N2,t3C3,F2)
       allocate(S159(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S159)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X77,S159,-1.000)
       deallocate(S159)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder623145(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,M2,N3,M1,N2,M1,N2,t3C1,F2)
       allocate(S160(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S160)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X77,S160, 1.000)
       deallocate(S160)
C
       allocate(D1(M1+1:N1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,N2,M2,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder623145(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,N1,M2,M2,N3,M1,N2,M1,N2,t3C1,F2)
       allocate(S161(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K9*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S161)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X77,S161, 1.000)
       deallocate(S161)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S162(N2+1:M2,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S162)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X8(N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       X8=0.0d0
       call
     & sum3412(N0,M1,M2,N3,N2,M2,M1,N2,X8,S162, 1.000)
       deallocate(S162)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,M2,N3,N2,M2,M1,N2,X8,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z9(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X8,F2,Z9)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z9,-1.000)
       call
     & sum124536(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z9, 1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z9, 1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z9,-1.000)
       call
     & sum135624(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z9,-1.000)
       call
     & sum125634(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z9, 1.000)
       deallocate(Z9)
       deallocate(X8)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S163(N2+1:M2,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S163)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X9(M1+1:N1,M2+1:N3,N2+1:M2,M1+1:N2))
       X9=0.0d0
       call
     & sum3412(M1,N1,M2,N3,N2,M2,M1,N2,X9,S163, 1.000)
       deallocate(S163)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,M2,N3,N2,M2,M1,N2,X9,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z10(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X9,F2,Z10)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z10,-1.000)
       call
     & sum124536(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z10, 1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z10, 1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z10,-1.000)
       call
     & sum135624(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z10,-1.000)
       call
     & sum125634(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z10, 1.000)
       deallocate(Z10)
       deallocate(X9)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S164(N2+1:M2,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S164)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X10(N0+1:M1,N1+1:M2,N2+1:M2,M1+1:N2))
       X10=0.0d0
       call
     & sum3412(N0,M1,N1,M2,N2,M2,M1,N2,X10,S164, 1.000)
       deallocate(S164)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,N1,M2,N2,M2,M1,N2,X10,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N1,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z11(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X10,F2,Z11)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z11,-1.000)
       call
     & sum124536(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z11, 1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z11, 1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z11,-1.000)
       call
     & sum135624(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z11,-1.000)
       call
     & sum125634(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z11, 1.000)
       deallocate(Z11)
       deallocate(X10)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S165(N2+1:M2,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S165)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X11(M1+1:N1,N1+1:M2,N2+1:M2,M1+1:N2))
       X11=0.0d0
       call
     & sum3412(M1,N1,N1,M2,N2,M2,M1,N2,X11,S165, 1.000)
       deallocate(S165)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,N1,M2,N2,M2,M1,N2,X11,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N1,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z12(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X11,F2,Z12)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z12,-1.000)
       call
     & sum124536(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z12, 1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z12, 1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z12,-1.000)
       call
     & sum135624(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z12,-1.000)
       call
     & sum125634(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z12, 1.000)
       deallocate(Z12)
       deallocate(X11)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,N0,M1,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N0,M1,M2,N3,N2,M2,N2,M2,M1,N2,t3D,F2)
       allocate(S166(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S166)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X73,S166,-0.500)
       deallocate(S166)
C
       allocate(D1(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M1,N2,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M1,N2,M2,N3,N2,M2,N2,M2,M1,N2,t3D,F2)
       allocate(S167(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K6*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S167)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X73,S167,-1.000)
       deallocate(S167)
C
       allocate(D1(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M1,N2,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M1,N2,M2,N3,N2,M2,N2,M2,M1,N2,t3D,F2)
       allocate(S168(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K6*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S168)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X73,S168,-0.500)
       deallocate(S168)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,N0,M1,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N0,M1,N2,M2,N2,M2,N2,M2,M1,N2,t3D,F2)
       allocate(S169(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S169)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X73,S169,-0.500)
       deallocate(S169)
C
       allocate(D1(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M1,N2,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M1,N2,N2,M2,N2,M2,N2,M2,M1,N2,t3D,F2)
       allocate(S170(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K0*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S170)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X73,S170,-1.000)
       deallocate(S170)
C
       allocate(D1(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M1,N2,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M1,N2,N2,M2,N2,M2,N2,M2,M1,N2,t3D,F2)
       allocate(S171(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K0*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S171)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X73,S171,-0.500)
       deallocate(S171)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,N0,M1,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,t3D,F2)
       allocate(S172(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S172)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X78(N2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       X78=0.0d0
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X78,S172, 1.000)
       deallocate(S172)
C
       allocate(D1(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M1,N2,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,t3D,F2)
       allocate(S173(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S173)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X78,S173, 2.000)
       deallocate(S173)
C
       allocate(D1(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M1,N2,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,t3D,F2)
       allocate(S174(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S174)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X78,S174, 1.000)
       deallocate(S174)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,N0,M1,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder452136(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N0,M1,N2,M2,M2,N3,N2,M2,M1,N2,t3D,F2)
       allocate(S175(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S175)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X78,S175,-1.000)
       deallocate(S175)
C
       allocate(D1(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M1,N2,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder452136(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M1,N2,N2,M2,M2,N3,N2,M2,M1,N2,t3D,F2)
       allocate(S176(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K0*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S176)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X78,S176,-2.000)
       deallocate(S176)
C
       allocate(D1(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M1,N2,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder452136(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M1,N2,N2,M2,M2,N3,N2,M2,M1,N2,t3D,F2)
       allocate(S177(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K0*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S177)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X78,S177,-1.000)
       deallocate(S177)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,N0,M1,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,t3D,F2)
       allocate(S178(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S178)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X79(N2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       X79=0.0d0
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X79,S178, 1.000)
       deallocate(S178)
C
       allocate(D1(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M1,N2,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,t3D,F2)
       allocate(S179(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S179)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X79,S179, 2.000)
       deallocate(S179)
C
       allocate(D1(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M1,N2,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,t3D,F2)
       allocate(S180(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S180)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X79,S180, 1.000)
       deallocate(S180)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,N0,M1,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder452136(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N0,M1,N2,M2,M2,N3,N2,M2,M1,N2,t3D,F2)
       allocate(S181(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S181)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X79,S181,-1.000)
       deallocate(S181)
C
       allocate(D1(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M1,N2,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder452136(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M1,N2,N2,M2,M2,N3,N2,M2,M1,N2,t3D,F2)
       allocate(S182(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K0*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S182)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X79,S182,-2.000)
       deallocate(S182)
C
       allocate(D1(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M1,N2,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder452136(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M1,N2,N2,M2,M2,N3,N2,M2,M1,N2,t3D,F2)
       allocate(S183(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K0*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S183)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X79,S183,-1.000)
       deallocate(S183)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:M1,N0+1:M1))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,M1,N0,M1,VCHHPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,M1,N2,M1,N2,t2C,D2)
       allocate(S184(M1+1:N2,M1+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K8*K8
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S184)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X20(N0+1:M1,N0+1:M1,M1+1:N2,M1+1:N2))
       X20=0.0d0
       call
     & sum3412(N0,M1,N0,M1,M1,N2,M1,N2,X20,S184, 0.500)
       deallocate(S184)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:M1,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,M1,M1,N2,VCHHPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,M1,N2,M1,N2,t2C,D2)
       allocate(S185(M1+1:N2,M1+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5
       I2=K8*K8
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S185)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X21(N0+1:M1,M1+1:N2,M1+1:N2,M1+1:N2))
       X21=0.0d0
       call
     & sum3412(N0,M1,M1,N2,M1,N2,M1,N2,X21,S185, 0.500)
       deallocate(S185)
C
       allocate(D1(N2+1:N3,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,M1,N2,M1,N2,VCHHPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,M1,N2,M1,N2,t2C,D2)
       allocate(S186(M1+1:N2,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8
       I2=K8*K8
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S186)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X22(M1+1:N2,M1+1:N2,M1+1:N2,M1+1:N2))
       X22=0.0d0
       call
     & sum3412(M1,N2,M1,N2,M1,N2,M1,N2,X22,S186, 0.500)
       deallocate(S186)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M2,N3,M2,N3,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(S187(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S187)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X80(N0+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       X80=0.0d0
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X80,S187, 1.000)
       deallocate(S187)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M2,N3,M2,N3,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(S188(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S188)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X80,S188, 1.000)
       deallocate(S188)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(S189(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K0*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S189)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X80,S189, 2.000)
       deallocate(S189)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(S190(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K0*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S190)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X80,S190, 2.000)
       deallocate(S190)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,N2,M2,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(S191(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K0*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S191)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X80,S191, 1.000)
       deallocate(S191)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,N2,M2,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(S192(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K0*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S192)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X80,S192, 1.000)
       deallocate(S192)
C
       allocate(D2(N0+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,M2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(Z255(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K0
       I2=K8*K0*K6
       I3=K2
       call EGEMM(I1,I2,I3,X80,D2,Z255)
       deallocate(D2)
C
       call
     & sum126345(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z255,-0.500)
       call
     & sum125346(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z255, 0.500)
       call
     & sum124356(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z255,-0.500)
       deallocate(Z255)
       deallocate(X80)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M2,N3,M2,N3,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(S193(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S193)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X81(N0+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       X81=0.0d0
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X81,S193, 1.000)
       deallocate(S193)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M2,N3,M2,N3,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(S194(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S194)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X81,S194, 1.000)
       deallocate(S194)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(S195(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K0*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S195)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X81,S195, 2.000)
       deallocate(S195)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(S196(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K0*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S196)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X81,S196, 2.000)
       deallocate(S196)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,N2,M2,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(S197(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K0*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S197)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X81,S197, 1.000)
       deallocate(S197)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,N2,M2,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(S198(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K0*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S198)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X81,S198, 1.000)
       deallocate(S198)
C
       allocate(D2(N0+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,M2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(Z261(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K0
       I2=K8*K0*K6
       I3=K2
       call EGEMM(I1,I2,I3,X81,D2,Z261)
       deallocate(D2)
C
       call
     & sum136245(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z261,-0.500)
       call
     & sum135246(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z261, 0.500)
       call
     & sum134256(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z261,-0.500)
       deallocate(Z261)
       deallocate(X81)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S199(M2+1:N3,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S199)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,M2,N3,M1,N2,X23,S199,-1.000)
       deallocate(S199)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S200(M2+1:N3,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S200)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,M2,N3,M2,N3,M1,N2,X24,S200,-1.000)
       deallocate(S200)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S201(M2+1:N3,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S201)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N2,M2,M2,N3,M1,N2,X25,S201,-1.000)
       deallocate(S201)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S202(M2+1:N3,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S202)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,N2,M2,M2,N3,M1,N2,X26,S202,-1.000)
       deallocate(S202)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder413256(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,t3D,F2)
       allocate(S203(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K0*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S203)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X77,S203, 1.000)
       deallocate(S203)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder413256(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,t3D,F2)
       allocate(S204(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K0*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S204)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X77,S204, 1.000)
       deallocate(S204)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,N2,M2,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder423156(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,N2,M2,M2,N3,M1,N2,M1,N2,t3D,F2)
       allocate(S205(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K0*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S205)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X77,S205,-0.500)
       deallocate(S205)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,N2,M2,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder423156(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,N2,M2,M2,N3,M1,N2,M1,N2,t3D,F2)
       allocate(S206(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K0*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S206)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X77,S206,-0.500)
       deallocate(S206)
C
       allocate(D2(N0+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,M2,N2,M2,M1,N2,t2C,D2)
       allocate(Z224(N2+1:M2,N2+1:M2,M1+1:N2,M2+1:N3,M1+1:N2,M1+1:N2))
       I1=K8*K8*K6
       I2=K8*K0*K0
       I3=K2
       call EGEMM(I1,I2,I3,X77,D2,Z224)
       deallocate(D2)
C
       call
     & sum236145(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z224,-1.000)
       call
     & sum235146(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z224, 1.000)
       call
     & sum234156(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z224,-1.000)
       deallocate(Z224)
       deallocate(X77)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S207(N2+1:M2,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S207)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,N2,M2,M1,N2,X55,S207, 1.000)
       deallocate(S207)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S208(N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S208)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,M2,N3,N2,M2,M1,N2,X56,S208, 1.000)
       deallocate(S208)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S209(N2+1:M2,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S209)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N2,M2,N2,M2,M1,N2,X57,S209, 1.000)
       deallocate(S209)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S210(N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S210)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,N2,M2,N2,M2,M1,N2,X58,S210, 1.000)
       deallocate(S210)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S211(N2+1:M2,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S211)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,N2,M2,M1,N2,X69,S211, 1.000)
       deallocate(S211)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z172(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X69,F2,Z172)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z172, 1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z172,-1.000)
       call
     & sum125634(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z172, 1.000)
       deallocate(Z172)
       deallocate(X69)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S212(N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S212)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,M2,N3,N2,M2,M1,N2,X70,S212, 1.000)
       deallocate(S212)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z173(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X70,F2,Z173)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z173, 1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z173,-1.000)
       call
     & sum125634(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z173, 1.000)
       deallocate(Z173)
       deallocate(X70)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S213(N2+1:M2,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S213)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N2,M2,N2,M2,M1,N2,X71,S213, 1.000)
       deallocate(S213)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z174(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,X71,F2,Z174)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z174,-1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z174, 1.000)
       call
     & sum125634(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z174,-1.000)
       deallocate(Z174)
       deallocate(X71)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S214(N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S214)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,N2,M2,N2,M2,M1,N2,X72,S214, 1.000)
       deallocate(S214)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z175(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X72,F2,Z175)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z175,-1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z175, 1.000)
       call
     & sum125634(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z175,-1.000)
       deallocate(Z175)
       deallocate(X72)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,M1,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,M1,N2,t2C,D2)
       allocate(Q33(M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8
       I3=K4*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q33)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N0,M1,M1,N2,X12,Q33, 0.500)
       deallocate(Q33)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,M1,N2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,M1,N2,t2C,D2)
       allocate(Q34(M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8
       I3=K4*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q34)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M1,N2,M1,N2,X13,Q34, 0.500)
       deallocate(Q34)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,N2,M2,t2C,D2)
       allocate(S215(M2+1:N3,N2+1:M2,M2+1:N3,M2+1:N3))
       I1=K6*K6
       I2=K0*K6
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S215)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X27(M2+1:N3,M2+1:N3,M2+1:N3,N2+1:M2))
       X27=0.0d0
       call
     & sum3412(M2,N3,M2,N3,M2,N3,N2,M2,X27,S215, 0.500)
       deallocate(S215)
C
       call sumx1234(N2,N3,N2,N3,N2,N3,N2,M2,
     & M2,N3,M2,N3,M2,N3,N2,M2,X27,VCAPPP, 1.000)
C
       allocate(F2(M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder123456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,M1,N2,t3D,F2)
       allocate(Z32(N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K8*K8*K8*K0
       I3=K6*K6
       call EGEMM(I1,I2,I3,X27,F2,Z32)
       deallocate(F2)
C
       call
     & sum345612(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z32, 0.500)
       call
     & sum245613(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z32,-0.500)
       deallocate(Z32)
       deallocate(X27)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,N2,M2,t2C,D2)
       allocate(S216(M2+1:N3,N2+1:M2,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K0*K6
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S216)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X28(M2+1:N3,N2+1:M2,M2+1:N3,N2+1:M2))
       X28=0.0d0
       call
     & sum3412(M2,N3,N2,M2,M2,N3,N2,M2,X28,S216, 0.500)
       deallocate(S216)
C
       call sumx1234(N2,N3,N2,N3,N2,N3,N2,M2,
     & M2,N3,N2,M2,M2,N3,N2,M2,X28,VCAPPP, 1.000)
C
       allocate(F2(M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder123456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,t3D,F2)
       allocate(Z33(N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K8*K8*K8*K0
       I3=K0*K6
       call EGEMM(I1,I2,I3,X28,F2,Z33)
       deallocate(F2)
C
       call
     & sum345612(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z33, 1.000)
       call
     & sum245613(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z33,-1.000)
       deallocate(Z33)
       deallocate(X28)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,M2,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,N2,M2,t2C,D2)
       allocate(S217(M2+1:N3,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0
       I2=K0*K6
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S217)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X29(N2+1:M2,N2+1:M2,M2+1:N3,N2+1:M2))
       X29=0.0d0
       call
     & sum3412(N2,M2,N2,M2,M2,N3,N2,M2,X29,S217, 0.500)
       deallocate(S217)
C
       call sumx1234(N2,N3,N2,N3,N2,N3,N2,M2,
     & N2,M2,N2,M2,M2,N3,N2,M2,X29,VCAPPP, 1.000)
C
       allocate(F2(N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder123456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N2,M2,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,t3D,F2)
       allocate(Z34(N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K8*K8*K8*K0
       I3=K0*K0
       call EGEMM(I1,I2,I3,X29,F2,Z34)
       deallocate(F2)
C
       call
     & sum345612(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z34, 0.500)
       call
     & sum245613(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z34,-0.500)
       deallocate(Z34)
       deallocate(X29)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,M2,N3,t2C,D2)
       allocate(Q35(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,Q35)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M2,N3,M2,N3,X14,Q35,-0.500)
       deallocate(Q35)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,M2,N3,t2C,D2)
       allocate(Q36(M2+1:N3,N2+1:M2))
       I1=K0
       I2=K6
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,Q36)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N2,M2,M2,N3,X15,Q36,-0.500)
       deallocate(Q36)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,M2,N2,M2,t2C,D2)
       allocate(S218(N2+1:M2,N2+1:M2,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K0*K0
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S218)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M2,N3,N2,M2,N2,M2,N2,M2,X30,S218, 0.500)
       deallocate(S218)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,M2,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,M2,N2,M2,t2C,D2)
       allocate(S219(N2+1:M2,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0
       I2=K0*K0
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S219)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,M2,N2,M2,N2,M2,N2,M2,X31,S219, 0.500)
       deallocate(S219)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,M2,t2C,D2)
       allocate(Q37(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,Q37)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M2,N3,N2,M2,X16,Q37,-0.500)
       deallocate(Q37)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,M2,t2C,D2)
       allocate(Q38(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,Q38)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N2,M2,N2,M2,X17,Q38,-0.500)
       deallocate(Q38)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,M2,t2C,D2)
       allocate(Q39(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,Q39)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M2,N3,N2,M2,X38,Q39, 0.500)
       deallocate(Q39)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,M2,t2C,D2)
       allocate(Q40(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,Q40)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N2,M2,N2,M2,X39,Q40, 0.500)
       deallocate(Q40)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,N2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q41(N0+1:M1,N2+1:N3))
       I1=K4*K5
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q41)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:M1))
       call reorder21(N0,M1,N2,N3,
     & N2,N3,N0,M1,Q41,B1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(Q42(M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q42)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(N0,M1,M1,N2,X32,Q42, 1.000)
       deallocate(Q42)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z37(M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8*K8*K0*K0*K6
       I3=K5
       call EGEMM(I1,I2,I3,X32,F2,Z37)
       deallocate(F2)
C
       V3D=V3D-Z37
       call
     & sum123465(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z37, 1.000)
       call
     & sum123564(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z37,-1.000)
       deallocate(Z37)
       deallocate(X32)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,N2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q43(M1+1:N2,N2+1:N3))
       I1=K4*K8
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q43)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,M1+1:N2))
       call reorder21(M1,N2,N2,N3,
     & N2,N3,M1,N2,Q43,B1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(Q44(M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q44)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(M1,N2,M1,N2,X33,Q44, 1.000)
       deallocate(Q44)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z38(M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8*K8*K0*K0*K6
       I3=K8
       call EGEMM(I1,I2,I3,X33,F2,Z38)
       deallocate(F2)
C
       V3D=V3D-Z38
       call
     & sum123465(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z38, 1.000)
       call
     & sum123564(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z38,-1.000)
       deallocate(Z38)
       deallocate(X33)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,M2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q45(N0+1:N2,M2+1:N3))
       I1=K6*K2
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q45)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(Q49(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q45,B2,Q49)
       deallocate(B2)
C
       call
     & sum21(M2,N3,N2,M2,X36,Q49,-1.000)
       deallocate(Q49)
C
       allocate(F2(M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder123456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,M1,N2,t3D,F2)
       allocate(Z41(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K8*K8*K0*K6
       I3=K6
       call EGEMM(I1,I2,I3,X36,F2,Z41)
       deallocate(F2)
C
       call
     & sum134562(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z41,-1.000)
       deallocate(Z41)
       deallocate(X36)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(Q51(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q45,B2,Q51)
       deallocate(B2)
C
       call
     & sum21(M2,N3,N2,M2,X38,Q51,-1.000)
       deallocate(Q51)
C
       allocate(F2(M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder123456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,M1,N2,t3D,F2)
       allocate(Z43(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K8*K8*K0*K6
       I3=K6
       call EGEMM(I1,I2,I3,X38,F2,Z43)
       deallocate(F2)
C
       call
     & sum124563(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z43, 1.000)
       deallocate(Z43)
       deallocate(X38)
C
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(Q46(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,Q45,B2,Q46)
       deallocate(B2)
C
       call
     & sum21(M2,N3,M2,N3,X34,Q46,-1.000)
       deallocate(Q46)
C
       allocate(F2(M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder123456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,t3D,F2)
       allocate(Z39(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6
       I2=K8*K8*K8*K0*K0
       I3=K6
       call EGEMM(I1,I2,I3,X34,F2,Z39)
       deallocate(F2)
C
       call
     & sum234561(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z39, 1.000)
       deallocate(Z39)
       deallocate(X34)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N2,M2,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q47(N0+1:N2,N2+1:M2))
       I1=K0*K2
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q47)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(Q48(M2+1:N3,N2+1:M2))
       I1=K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,Q47,B2,Q48)
       deallocate(B2)
C
       call
     & sum21(N2,M2,M2,N3,X35,Q48,-1.000)
       deallocate(Q48)
C
       allocate(F2(N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder123456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N2,M2,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,t3D,F2)
       allocate(Z40(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6
       I2=K8*K8*K8*K0*K0
       I3=K0
       call EGEMM(I1,I2,I3,X35,F2,Z40)
       deallocate(F2)
C
       call
     & sum234561(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z40, 1.000)
       deallocate(Z40)
       deallocate(X35)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(Q52(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q47,B2,Q52)
       deallocate(B2)
C
       call
     & sum21(N2,M2,N2,M2,X39,Q52,-1.000)
       deallocate(Q52)
C
       allocate(F2(N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder213456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,M1,N2,t3D,F2)
       allocate(Z44(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K8*K8*K0*K6
       I3=K0
       call EGEMM(I1,I2,I3,X39,F2,Z44)
       deallocate(F2)
C
       call
     & sum124563(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z44,-1.000)
       deallocate(Z44)
       deallocate(X39)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(Q50(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q47,B2,Q50)
       deallocate(B2)
C
       call
     & sum21(N2,M2,N2,M2,X37,Q50,-1.000)
       deallocate(Q50)
C
       allocate(F2(N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder213456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,M1,N2,t3D,F2)
       allocate(Z42(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K8*K8*K0*K6
       I3=K0
       call EGEMM(I1,I2,I3,X37,F2,Z42)
       deallocate(F2)
C
       call
     & sum134562(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z42, 1.000)
       deallocate(Z42)
       deallocate(X37)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q53(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q53)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q53,B1)
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,M1,N2,M1,N2,t2C,D2)
       allocate(S221(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S221)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X2,S221,-1.000)
       deallocate(S221)
C
       allocate(D2(N0+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,M2,N2,M2,M1,N2,t2C,D2)
       allocate(S220(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K2
       call EGEMM(I1,I2,I3,Q53,D2,S220)
       deallocate(D2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X73,S220, 1.000)
       deallocate(S220)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,M1+1:N2))
       call reorder1342(N2,N3,N0,N2,N0,N2,N0,N2,
     & N2,N3,N0,N2,N0,N2,M1,N2,VCHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S222(M1+1:N2,N0+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S222)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder2314(M1,N2,N0,N2,N0,N2,M1,N2,
     & N0,N2,N0,N2,M1,N2,M1,N2,S222,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S224(N2+1:M2,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S224)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,N2,M2,M1,N2,M1,N2,X43,S224,-1.000)
       deallocate(S224)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder3214(M1,N2,N0,N2,N0,N2,M1,N2,
     & N0,N2,N0,N2,M1,N2,M1,N2,S222,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S225(N2+1:M2,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S225)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,N2,M2,M1,N2,M1,N2,X44,S225, 1.000)
       deallocate(S225)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder2314(M1,N2,N0,N2,N0,N2,M1,N2,
     & N0,N2,N0,N2,M1,N2,M1,N2,S222,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S223(M2+1:N3,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S223)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,M2,N3,M1,N2,M1,N2,X42,S223,-1.000)
       deallocate(S223)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder4312(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S226(N2+1:M2,N0+1:N2,N2+1:N3,M1+1:N2))
       I1=K8*K4*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S226)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder2314(N2,M2,N0,N2,N2,N3,M1,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,S226,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S230(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S230)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,N3,N2,M2,N2,M2,M1,N2,X3,S230,-1.000)
       deallocate(S230)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder2314(N2,M2,N0,N2,N2,N3,M1,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,S226,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S227(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S227)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,M1,N2,X45,S227,-1.000)
       deallocate(S227)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder4312(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S228(N2+1:M2,N0+1:N2,N2+1:N3,M1+1:N2))
       I1=K8*K4*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S228)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder2314(N2,M2,N0,N2,N2,N3,M1,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,S228,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S229(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S229)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,M1,N2,X46,S229,-1.000)
       deallocate(S229)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder2413(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,N2,N2,N3,M2,N3,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S231(M1+1:N2,N0+1:N2,N2+1:N3,M2+1:N3))
       I1=K6*K4*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S231)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N2,N3,M2,N3,
     & N0,N2,N2,N3,M2,N3,M1,N2,S231,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S237(N2+1:M2,N2+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S237)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,N3,M2,N3,N2,M2,M1,N2,X45,S237, 1.000)
       deallocate(S237)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N2,N3,M2,N3,
     & N0,N2,N2,N3,M2,N3,M1,N2,S231,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S238(N2+1:M2,N2+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S238)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,N3,M2,N3,N2,M2,M1,N2,X46,S238, 1.000)
       deallocate(S238)
C
       allocate(D1(N2+1:N3,N0+1:N2,M2+1:N3,M1+1:N2))
       call reorder3241(M1,N2,N0,N2,N2,N3,M2,N3,
     & N2,N3,N0,N2,M2,N3,M1,N2,S231,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S232(M1+1:N2,N0+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S232)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N2,M2,N3,M1,N2,M1,N2,X1,S232, 1.000)
       deallocate(S232)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder2413(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,N2,N2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S233(M1+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S233)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N2,N3,N2,M2,
     & N0,N2,N2,N3,N2,M2,M1,N2,S233,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S239(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S239)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,M1,N2,X45,S239,-1.000)
       deallocate(S239)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N2,N3,N2,M2,
     & N0,N2,N2,N3,N2,M2,M1,N2,S233,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S241(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S241)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,N3,N2,M2,N2,M2,M1,N2,X3,S241,-1.000)
       deallocate(S241)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:M2,M1+1:N2))
       call reorder3241(M1,N2,N0,N2,N2,N3,N2,M2,
     & N2,N3,N0,N2,N2,M2,M1,N2,S233,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S234(M1+1:N2,N0+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S234)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N2,N2,M2,M1,N2,M1,N2,X40,S234,-1.000)
       deallocate(S234)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder2413(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,N2,N2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S235(M1+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S235)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:M2,M1+1:N2))
       call reorder3241(M1,N2,N0,N2,N2,N3,N2,M2,
     & N2,N3,N0,N2,N2,M2,M1,N2,S235,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S236(M1+1:N2,N0+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S236)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N2,N2,M2,M1,N2,M1,N2,X41,S236, 1.000)
       deallocate(S236)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N2,N3,N2,M2,
     & N0,N2,N2,N3,N2,M2,M1,N2,S235,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S242(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S242)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,N2,M2,M1,N2,X3,S242, 1.000)
       deallocate(S242)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N2,N3,N2,M2,
     & N0,N2,N2,N3,N2,M2,M1,N2,S235,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S240(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S240)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,M1,N2,X46,S240,-1.000)
       deallocate(S240)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,M2+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,N2,N0,M1,M2,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S243(M1+1:N2,N0+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S243)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,M1,M2,N3,
     & N0,N2,N0,M1,M2,N3,M1,N2,S243,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S251(N2+1:M2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S251)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,N2,M2,M1,N2,X47,S251, 1.000)
       deallocate(S251)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z64(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X47,F2,Z64)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z64, 1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z64,-1.000)
       call
     & sum135624(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z64, 1.000)
       deallocate(Z64)
       deallocate(X47)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,M1,M2,N3,
     & N0,N2,N0,M1,M2,N3,M1,N2,S243,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S255(N2+1:M2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S255)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,N2,M2,M1,N2,X51,S255, 1.000)
       deallocate(S255)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z68(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X51,F2,Z68)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z68,-1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z68, 1.000)
       call
     & sum125634(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z68,-1.000)
       deallocate(Z68)
       deallocate(X51)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,M1,M2,N3,
     & N0,N2,N0,M1,M2,N3,M1,N2,S243,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S244(M2+1:N3,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S244)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,M2,N3,M1,N2,X4,S244,-1.000)
       deallocate(S244)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,M2,N3,M2,N3,M1,N2,X4,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z5(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K8*K8*K0*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,X4,F2,Z5)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z5, 1.000)
       call
     & sum234615(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z5,-1.000)
       call
     & sum235614(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z5, 1.000)
       deallocate(Z5)
       deallocate(X4)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,N2,M1,N1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S249(M1+1:N2,N0+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S249)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,M1,N1,N1,M2,
     & N0,N2,M1,N1,N1,M2,M1,N2,S249,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S254(N2+1:M2,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S254)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,N1,M2,N2,M2,M1,N2,X50,S254, 1.000)
       deallocate(S254)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N1,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z67(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X50,F2,Z67)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z67, 1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z67,-1.000)
       call
     & sum135624(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z67, 1.000)
       deallocate(Z67)
       deallocate(X50)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,M1,N1,N1,M2,
     & N0,N2,M1,N1,N1,M2,M1,N2,S249,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S258(N2+1:M2,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S258)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,N1,M2,N2,M2,M1,N2,X54,S258, 1.000)
       deallocate(S258)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N1,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z71(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X54,F2,Z71)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z71,-1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z71, 1.000)
       call
     & sum125634(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z71,-1.000)
       deallocate(Z71)
       deallocate(X54)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,M1,N1,N1,M2,
     & N0,N2,M1,N1,N1,M2,M1,N2,S249,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S250(M2+1:N3,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S250)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,N1,M2,M2,N3,M1,N2,X7,S250,-1.000)
       deallocate(S250)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,N1,M2,M2,N3,M1,N2,X7,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N1,M2,N2,M2,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z8(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K8*K8*K0*K0
       I3=K9*K7
       call EGEMM(I1,I2,I3,X7,F2,Z8)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z8, 1.000)
       call
     & sum234615(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z8,-1.000)
       call
     & sum235614(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z8, 1.000)
       deallocate(Z8)
       deallocate(X7)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,N2,N0,M1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S247(M1+1:N2,N0+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S247)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,M1,N1,M2,
     & N0,N2,N0,M1,N1,M2,M1,N2,S247,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S248(M2+1:N3,N0+1:M1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S248)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N1,M2,M2,N3,M1,N2,X6,S248,-1.000)
       deallocate(S248)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,N1,M2,M2,N3,M1,N2,X6,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N1,M2,N2,M2,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z7(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K8*K8*K0*K0
       I3=K9*K5
       call EGEMM(I1,I2,I3,X6,F2,Z7)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z7, 1.000)
       call
     & sum234615(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z7,-1.000)
       call
     & sum235614(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z7, 1.000)
       deallocate(Z7)
       deallocate(X6)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,M1,N1,M2,
     & N0,N2,N0,M1,N1,M2,M1,N2,S247,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S257(N2+1:M2,N0+1:M1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S257)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N1,M2,N2,M2,M1,N2,X53,S257, 1.000)
       deallocate(S257)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N1,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z70(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X53,F2,Z70)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z70,-1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z70, 1.000)
       call
     & sum125634(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z70,-1.000)
       deallocate(Z70)
       deallocate(X53)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,M1,N1,M2,
     & N0,N2,N0,M1,N1,M2,M1,N2,S247,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S253(N2+1:M2,N0+1:M1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S253)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N1,M2,N2,M2,M1,N2,X49,S253, 1.000)
       deallocate(S253)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N1,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z66(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X49,F2,Z66)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z66, 1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z66,-1.000)
       call
     & sum135624(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z66, 1.000)
       deallocate(Z66)
       deallocate(X49)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N1,M2+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,N2,M1,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S245(M1+1:N2,N0+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S245)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,M1,N1,M2,N3,
     & N0,N2,M1,N1,M2,N3,M1,N2,S245,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S246(M2+1:N3,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S246)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,M2,N3,M2,N3,M1,N2,X5,S246,-1.000)
       deallocate(S246)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,M2,N3,M2,N3,M1,N2,X5,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z6(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K8*K8*K0*K0
       I3=K6*K7
       call EGEMM(I1,I2,I3,X5,F2,Z6)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z6, 1.000)
       call
     & sum234615(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z6,-1.000)
       call
     & sum235614(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z6, 1.000)
       deallocate(Z6)
       deallocate(X5)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,M1,N1,M2,N3,
     & N0,N2,M1,N1,M2,N3,M1,N2,S245,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S252(N2+1:M2,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S252)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,M2,N3,N2,M2,M1,N2,X48,S252, 1.000)
       deallocate(S252)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z65(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X48,F2,Z65)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z65, 1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z65,-1.000)
       call
     & sum135624(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z65, 1.000)
       deallocate(Z65)
       deallocate(X48)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,M1,N1,M2,N3,
     & N0,N2,M1,N1,M2,N3,M1,N2,S245,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S256(N2+1:M2,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S256)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,M2,N3,N2,M2,M1,N2,X52,S256, 1.000)
       deallocate(S256)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z69(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X52,F2,Z69)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z69,-1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z69, 1.000)
       call
     & sum125634(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z69,-1.000)
       deallocate(Z69)
       deallocate(X52)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:M1,N2+1:N3))
       call reorder2341(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,M1,N0,M1,N2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S259(M1+1:N2,N0+1:M1,N0+1:M1,N2+1:N3))
       I1=K4*K5*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S259)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:M1,M1+1:N2))
       call reorder4231(M1,N2,N0,M1,N0,M1,N2,N3,
     & N2,N3,N0,M1,N0,M1,M1,N2,S259,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S260(M1+1:N2,N0+1:M1,N0+1:M1,M1+1:N2))
       I1=K8*K5*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S260)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N0,M1,M1,N2,M1,N2,X20,S260, 1.000)
       deallocate(S260)
C
       call sumx3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,M1,N0,M1,M1,N2,M1,N2,X20,VCHHHH, 1.000)
C
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N0,M1,M2,N3,N2,M2,N2,M2,M1,N2,t3D,F2)
       allocate(Z21(M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8
       I2=K8*K0*K0*K6
       I3=K5*K5
       call EGEMM(I1,I2,I3,X20,F2,Z21)
       deallocate(F2)
C
       V3D=V3D+0.500*Z21
       call
     & sum123546(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z21,-0.500)
       call
     & sum123645(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z21, 0.500)
       deallocate(Z21)
       deallocate(X20)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,N2+1:N3))
       call reorder2341(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,M1,M1,N2,N2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S261(M1+1:N2,N0+1:M1,M1+1:N2,N2+1:N3))
       I1=K4*K8*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S261)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder4231(M1,N2,N0,M1,M1,N2,N2,N3,
     & N2,N3,N0,M1,M1,N2,M1,N2,S261,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S262(M1+1:N2,N0+1:M1,M1+1:N2,M1+1:N2))
       I1=K8*K8*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S262)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M1,N2,M1,N2,M1,N2,X21,S262, 1.000)
       deallocate(S262)
C
       call sumx3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,M1,M1,N2,M1,N2,M1,N2,X21,VCHHHH, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M1,N2,M2,N3,N2,M2,N2,M2,M1,N2,t3D,F2)
       allocate(Z22(M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8
       I2=K8*K0*K0*K6
       I3=K8*K5
       call EGEMM(I1,I2,I3,X21,F2,Z22)
       deallocate(F2)
C
       V3D=V3D+Z22
       call
     & sum123546(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z22,-1.000)
       call
     & sum123645(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z22, 1.000)
       deallocate(Z22)
       deallocate(X21)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,N2+1:N3))
       call reorder2341(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,M1,N2,M1,N2,N2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S263(M1+1:N2,M1+1:N2,M1+1:N2,N2+1:N3))
       I1=K4*K8*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S263)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder4231(M1,N2,M1,N2,M1,N2,N2,N3,
     & N2,N3,M1,N2,M1,N2,M1,N2,S263,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S264(M1+1:N2,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S264)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M1,N2,M1,N2,M1,N2,X22,S264, 1.000)
       deallocate(S264)
C
       call sumx3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & M1,N2,M1,N2,M1,N2,M1,N2,X22,VCHHHH, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M1,N2,M2,N3,N2,M2,N2,M2,M1,N2,t3D,F2)
       allocate(Z23(M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8
       I2=K8*K0*K0*K6
       I3=K8*K8
       call EGEMM(I1,I2,I3,X22,F2,Z23)
       deallocate(F2)
C
       V3D=V3D+0.500*Z23
       call
     & sum123546(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z23,-0.500)
       call
     & sum123645(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z23, 0.500)
       deallocate(Z23)
       deallocate(X22)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,M2+1:N3))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,N2,N0,M1,M2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S265(M1+1:N2,N0+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S265)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,M1,M2,N3,
     & N0,N2,N0,M1,M2,N3,M1,N2,S265,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S273(N2+1:M2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S273)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,N2,M2,M1,N2,X55,S273,-1.000)
       deallocate(S273)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z99(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X55,F2,Z99)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z99,-1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z99, 1.000)
       call
     & sum135624(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z99,-1.000)
       deallocate(Z99)
       deallocate(X55)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,M1,M2,N3,
     & N0,N2,N0,M1,M2,N3,M1,N2,S265,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S266(M2+1:N3,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S266)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,M2,N3,M1,N2,X23,S266, 1.000)
       deallocate(S266)
C
       call sumx2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,M1,M2,N3,M2,N3,M1,N2,X23,VCHPHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z24(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K8*K8*K0*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,X23,F2,Z24)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z24,-1.000)
       call
     & sum234615(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z24, 1.000)
       call
     & sum235614(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z24,-1.000)
       deallocate(Z24)
       deallocate(X23)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,N2+1:M2))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,N2,N0,M1,N2,M2,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S269(M1+1:N2,N0+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S269)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,M1,N2,M2,
     & N0,N2,N0,M1,N2,M2,M1,N2,S269,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S275(N2+1:M2,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S275)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N2,M2,N2,M2,M1,N2,X57,S275,-1.000)
       deallocate(S275)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z101(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,X57,F2,Z101)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z101, 1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z101,-1.000)
       call
     & sum135624(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z101, 1.000)
       deallocate(Z101)
       deallocate(X57)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,M1,N2,M2,
     & N0,N2,N0,M1,N2,M2,M1,N2,S269,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S270(M2+1:N3,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S270)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N2,M2,M2,N3,M1,N2,X25,S270, 1.000)
       deallocate(S270)
C
       call sumx2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,M1,N2,M2,M2,N3,M1,N2,X25,VCHPHP, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z26(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K8*K8*K0*K0
       I3=K0*K5
       call EGEMM(I1,I2,I3,X25,F2,Z26)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z26,-1.000)
       call
     & sum234615(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z26, 1.000)
       call
     & sum235614(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z26,-1.000)
       deallocate(Z26)
       deallocate(X25)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N2,M2+1:N3))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,N2,M1,N2,M2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S267(M1+1:N2,N0+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S267)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,M1,N2,M2,N3,
     & N0,N2,M1,N2,M2,N3,M1,N2,S267,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S274(N2+1:M2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S274)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M2,N3,N2,M2,M1,N2,X56,S274,-1.000)
       deallocate(S274)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z100(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X56,F2,Z100)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z100,-1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z100, 1.000)
       call
     & sum135624(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z100,-1.000)
       deallocate(Z100)
       deallocate(X56)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,M1,N2,M2,N3,
     & N0,N2,M1,N2,M2,N3,M1,N2,S267,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S268(M2+1:N3,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S268)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M2,N3,M2,N3,M1,N2,X24,S268, 1.000)
       deallocate(S268)
C
       call sumx2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,M2,N3,M2,N3,M1,N2,X24,VCHPHP, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z25(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K8*K8*K0*K0
       I3=K6*K8
       call EGEMM(I1,I2,I3,X24,F2,Z25)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z25,-1.000)
       call
     & sum234615(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z25, 1.000)
       call
     & sum235614(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z25,-1.000)
       deallocate(Z25)
       deallocate(X24)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N2,N2+1:M2))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,N2,M1,N2,N2,M2,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S271(M1+1:N2,N0+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S271)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,M1,N2,N2,M2,
     & N0,N2,M1,N2,N2,M2,M1,N2,S271,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S276(N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S276)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,N2,M2,N2,M2,M1,N2,X58,S276,-1.000)
       deallocate(S276)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z102(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X58,F2,Z102)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z102, 1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z102,-1.000)
       call
     & sum135624(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z102, 1.000)
       deallocate(Z102)
       deallocate(X58)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,M1,N2,N2,M2,
     & N0,N2,M1,N2,N2,M2,M1,N2,S271,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S272(M2+1:N3,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K8
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S272)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,N2,M2,M2,N3,M1,N2,X26,S272, 1.000)
       deallocate(S272)
C
       call sumx2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,N2,M2,M2,N3,M1,N2,X26,VCHPHP, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z27(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K8*K8*K0*K0
       I3=K0*K8
       call EGEMM(I1,I2,I3,X26,F2,Z27)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z27,-1.000)
       call
     & sum234615(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z27, 1.000)
       call
     & sum235614(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z27,-1.000)
       deallocate(Z27)
       deallocate(X26)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:N2,M2+1:N3))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,M1,N0,N2,M2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S277(M1+1:N2,N0+1:M1,N0+1:N2,M2+1:N3))
       I1=K6*K2*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S277)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder3241(M1,N2,N0,M1,N0,N2,M2,N3,
     & N0,N2,N0,M1,M2,N3,M1,N2,S277,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S278(N2+1:M2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S278)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,N2,M2,M1,N2,X59,S278,-1.000)
       deallocate(S278)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z103(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X59,F2,Z103)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z103,-1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z103, 1.000)
       call
     & sum125634(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z103,-1.000)
       deallocate(Z103)
       deallocate(X59)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:N2,M2+1:N3))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,M1,N2,N0,N2,M2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S279(M1+1:N2,M1+1:N2,N0+1:N2,M2+1:N3))
       I1=K6*K2*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S279)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder3241(M1,N2,M1,N2,N0,N2,M2,N3,
     & N0,N2,M1,N2,M2,N3,M1,N2,S279,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S280(N2+1:M2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S280)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M2,N3,N2,M2,M1,N2,X60,S280,-1.000)
       deallocate(S280)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z104(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X60,F2,Z104)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z104,-1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z104, 1.000)
       call
     & sum125634(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z104,-1.000)
       deallocate(Z104)
       deallocate(X60)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:N2,N2+1:M2))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,M1,N0,N2,N2,M2,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S281(M1+1:N2,N0+1:M1,N0+1:N2,N2+1:M2))
       I1=K0*K2*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S281)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,M1+1:N2))
       call reorder3241(M1,N2,N0,M1,N0,N2,N2,M2,
     & N0,N2,N0,M1,N2,M2,M1,N2,S281,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S282(N2+1:M2,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S282)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N2,M2,N2,M2,M1,N2,X61,S282,-1.000)
       deallocate(S282)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z105(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,X61,F2,Z105)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z105, 1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z105,-1.000)
       call
     & sum125634(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z105, 1.000)
       deallocate(Z105)
       deallocate(X61)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:N2,N2+1:M2))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,M1,N2,N0,N2,N2,M2,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S283(M1+1:N2,M1+1:N2,N0+1:N2,N2+1:M2))
       I1=K0*K2*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S283)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       call reorder3241(M1,N2,M1,N2,N0,N2,N2,M2,
     & N0,N2,M1,N2,N2,M2,M1,N2,S283,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S284(N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S284)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,N2,M2,N2,M2,M1,N2,X62,S284,-1.000)
       deallocate(S284)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z106(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X62,F2,Z106)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z106, 1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z106,-1.000)
       call
     & sum125634(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z106, 1.000)
       deallocate(Z106)
       deallocate(X62)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,N2,N3,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q54(N0+1:M1,N2+1:N3))
       I1=K4*K5
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q54)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:M1))
       call reorder21(N0,M1,N2,N3,
     & N2,N3,N0,M1,Q54,B1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(Q55(M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q55)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(N0,M1,M1,N2,X12,Q55, 1.000)
       deallocate(Q55)
C
       call sumx21(N0,N2,N0,N2,
     & N0,M1,M1,N2,X12,FBHH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z13(M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8*K8*K0*K0*K6
       I3=K5
       call EGEMM(I1,I2,I3,X12,F2,Z13)
       deallocate(F2)
C
       V3D=V3D-Z13
       call
     & sum123465(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z13, 1.000)
       call
     & sum123564(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z13,-1.000)
       deallocate(Z13)
       deallocate(X12)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,N2,N3,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q56(M1+1:N2,N2+1:N3))
       I1=K4*K8
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q56)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,M1+1:N2))
       call reorder21(M1,N2,N2,N3,
     & N2,N3,M1,N2,Q56,B1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(Q57(M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q57)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(M1,N2,M1,N2,X13,Q57, 1.000)
       deallocate(Q57)
C
       call sumx21(N0,N2,N0,N2,
     & M1,N2,M1,N2,X13,FBHH, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z14(M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8*K8*K0*K0*K6
       I3=K8
       call EGEMM(I1,I2,I3,X13,F2,Z14)
       deallocate(F2)
C
       V3D=V3D-Z14
       call
     & sum123465(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z14, 1.000)
       call
     & sum123564(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z14,-1.000)
       deallocate(Z14)
       deallocate(X13)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,M2+1:N3))
       call reorder4312(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,M2,N3,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S285(N2+1:M2,N0+1:N2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S285)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,N2+1:M2))
       call reorder2341(N2,M2,N0,N2,M2,N3,M2,N3,
     & N0,N2,M2,N3,M2,N3,N2,M2,S285,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S286(M2+1:N3,M2+1:N3,M2+1:N3,N2+1:M2))
       I1=K0*K6*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S286)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,M2,N3,M2,N3,N2,M2,X63,S286,-1.000)
       deallocate(S286)
C
       allocate(F2(M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder123456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,M1,N2,t3D,F2)
       allocate(Z121(N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K8*K8*K8*K0
       I3=K6*K6
       call EGEMM(I1,I2,I3,X63,F2,Z121)
       deallocate(F2)
C
       call
     & sum345612(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z121,-0.500)
       deallocate(Z121)
       deallocate(X63)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder4312(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,N2,M2,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S287(N2+1:M2,N0+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S287)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder2341(N2,M2,N0,N2,M2,N3,N2,M2,
     & N0,N2,M2,N3,N2,M2,N2,M2,S287,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S297(N2+1:M2,M2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S297)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M2,N3,N2,M2,N2,M2,N2,M2,X30,S297,-1.000)
       deallocate(S297)
C
       call sumx1234(N2,N3,N2,N3,N2,N3,N2,M2,
     & M2,N3,N2,M2,N2,M2,N2,M2,X30,VCAPPP, 1.000)
C
       allocate(F2(M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder132456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,M1,N2,t3D,F2)
       allocate(Z35(M2+1:N3,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0
       I2=K8*K8*K8*K6
       I3=K0*K6
       call EGEMM(I1,I2,I3,X30,F2,Z35)
       deallocate(F2)
C
       call
     & sum145623(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z35,-1.000)
       deallocate(Z35)
       deallocate(X30)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder2341(N2,M2,N0,N2,M2,N3,N2,M2,
     & N0,N2,M2,N3,N2,M2,N2,M2,S287,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S288(M2+1:N3,M2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S288)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,N2,M2,M2,N3,N2,M2,X64,S288,-1.000)
       deallocate(S288)
C
       allocate(F2(M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder123456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,t3D,F2)
       allocate(Z122(N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K8*K8*K8*K0
       I3=K0*K6
       call EGEMM(I1,I2,I3,X64,F2,Z122)
       deallocate(F2)
C
       call
     & sum345612(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z122,-1.000)
       deallocate(Z122)
       deallocate(X64)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,M2+1:N3))
       call reorder4312(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,M2,N3,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S291(N2+1:M2,N0+1:N2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S291)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,N2+1:M2))
       call reorder2341(N2,M2,N0,N2,M2,N3,M2,N3,
     & N0,N2,M2,N3,M2,N3,N2,M2,S291,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S292(M2+1:N3,M2+1:N3,M2+1:N3,N2+1:M2))
       I1=K0*K6*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S292)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,M2,N3,M2,N3,N2,M2,X66,S292,-1.000)
       deallocate(S292)
C
       allocate(F2(M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder123456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,M1,N2,t3D,F2)
       allocate(Z124(N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K8*K8*K8*K0
       I3=K6*K6
       call EGEMM(I1,I2,I3,X66,F2,Z124)
       deallocate(F2)
C
       call
     & sum245613(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z124, 0.500)
       deallocate(Z124)
       deallocate(X66)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder4312(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,N2,M2,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S293(N2+1:M2,N0+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S293)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder2341(N2,M2,N0,N2,M2,N3,N2,M2,
     & N0,N2,M2,N3,N2,M2,N2,M2,S293,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S294(M2+1:N3,M2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S294)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,N2,M2,M2,N3,N2,M2,X67,S294,-1.000)
       deallocate(S294)
C
       allocate(F2(M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder123456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,t3D,F2)
       allocate(Z125(N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K8*K8*K8*K0
       I3=K0*K6
       call EGEMM(I1,I2,I3,X67,F2,Z125)
       deallocate(F2)
C
       call
     & sum245613(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z125, 1.000)
       deallocate(Z125)
       deallocate(X67)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder4312(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,M2,N2,M2,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S295(N2+1:M2,N0+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S295)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder2341(N2,M2,N0,N2,N2,M2,N2,M2,
     & N0,N2,N2,M2,N2,M2,N2,M2,S295,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S296(M2+1:N3,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S296)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,M2,N2,M2,M2,N3,N2,M2,X68,S296,-1.000)
       deallocate(S296)
C
       allocate(F2(N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder123456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N2,M2,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,t3D,F2)
       allocate(Z126(N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K8*K8*K8*K0
       I3=K0*K0
       call EGEMM(I1,I2,I3,X68,F2,Z126)
       deallocate(F2)
C
       call
     & sum245613(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z126, 0.500)
       deallocate(Z126)
       deallocate(X68)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,M2,N3,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q58(N0+1:N2,M2+1:N3))
       I1=K6*K2
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q58)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(Q62(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q58,B2,Q62)
       deallocate(B2)
C
       call
     & sum21(M2,N3,N2,M2,X16,Q62,-1.000)
       deallocate(Q62)
C
       call sumx12(N2,N3,N2,N3,
     & M2,N3,N2,M2,X16,FBPP, 1.000)
C
       allocate(F2(M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder123456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,M1,N2,t3D,F2)
       allocate(Z17(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K8*K8*K0*K6
       I3=K6
       call EGEMM(I1,I2,I3,X16,F2,Z17)
       deallocate(F2)
C
       call
     & sum134562(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z17,-1.000)
       deallocate(Z17)
       deallocate(X16)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(Q64(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q58,B2,Q64)
       deallocate(B2)
C
       call
     & sum21(M2,N3,N2,M2,X18,Q64,-1.000)
       deallocate(Q64)
C
       call sumx12(N2,N3,N2,N3,
     & M2,N3,N2,M2,X18,FBPP, 1.000)
C
       allocate(F2(M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder123456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,M1,N2,t3D,F2)
       allocate(Z19(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K8*K8*K0*K6
       I3=K6
       call EGEMM(I1,I2,I3,X18,F2,Z19)
       deallocate(F2)
C
       call
     & sum124563(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z19, 1.000)
       deallocate(Z19)
       deallocate(X18)
C
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(Q59(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,Q58,B2,Q59)
       deallocate(B2)
C
       call
     & sum21(M2,N3,M2,N3,X14,Q59,-1.000)
       deallocate(Q59)
C
       call sumx12(N2,N3,N2,N3,
     & M2,N3,M2,N3,X14,FBPP, 1.000)
C
       allocate(F2(M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder123456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,t3D,F2)
       allocate(Z15(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6
       I2=K8*K8*K8*K0*K0
       I3=K6
       call EGEMM(I1,I2,I3,X14,F2,Z15)
       deallocate(F2)
C
       call
     & sum234561(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z15, 1.000)
       deallocate(Z15)
       deallocate(X14)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder4312(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,M2,N2,M2,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S289(N2+1:M2,N0+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S289)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder2341(N2,M2,N0,N2,N2,M2,N2,M2,
     & N0,N2,N2,M2,N2,M2,N2,M2,S289,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S290(M2+1:N3,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S290)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,M2,N2,M2,M2,N3,N2,M2,X65,S290,-1.000)
       deallocate(S290)
C
       allocate(F2(N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder123456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N2,M2,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,t3D,F2)
       allocate(Z123(N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K8*K8*K8*K0
       I3=K0*K0
       call EGEMM(I1,I2,I3,X65,F2,Z123)
       deallocate(F2)
C
       call
     & sum345612(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z123,-0.500)
       deallocate(Z123)
       deallocate(X65)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder2341(N2,M2,N0,N2,N2,M2,N2,M2,
     & N0,N2,N2,M2,N2,M2,N2,M2,S289,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S298(N2+1:M2,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S298)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N2,M2,N2,M2,N2,M2,N2,M2,X31,S298,-1.000)
       deallocate(S298)
C
       call sumx1234(N2,N3,N2,N3,N2,N3,N2,M2,
     & N2,M2,N2,M2,N2,M2,N2,M2,X31,VCAPPP, 1.000)
C
       allocate(F2(N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder231456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N2,M2,N2,M2,M2,N3,M1,N2,M1,N2,M1,N2,t3D,F2)
       allocate(Z36(M2+1:N3,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0
       I2=K8*K8*K8*K6
       I3=K0*K0
       call EGEMM(I1,I2,I3,X31,F2,Z36)
       deallocate(F2)
C
       call
     & sum145623(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z36, 0.500)
       deallocate(Z36)
       deallocate(X31)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N2,M2,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q60(N0+1:N2,N2+1:M2))
       I1=K0*K2
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q60)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(Q61(M2+1:N3,N2+1:M2))
       I1=K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,Q60,B2,Q61)
       deallocate(B2)
C
       call
     & sum21(N2,M2,M2,N3,X15,Q61,-1.000)
       deallocate(Q61)
C
       call sumx12(N2,N3,N2,N3,
     & N2,M2,M2,N3,X15,FBPP, 1.000)
C
       allocate(F2(N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder123456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N2,M2,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,t3D,F2)
       allocate(Z16(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6
       I2=K8*K8*K8*K0*K0
       I3=K0
       call EGEMM(I1,I2,I3,X15,F2,Z16)
       deallocate(F2)
C
       call
     & sum234561(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z16, 1.000)
       deallocate(Z16)
       deallocate(X15)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(Q65(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q60,B2,Q65)
       deallocate(B2)
C
       call
     & sum21(N2,M2,N2,M2,X19,Q65,-1.000)
       deallocate(Q65)
C
       call sumx12(N2,N3,N2,N3,
     & N2,M2,N2,M2,X19,FBPP, 1.000)
C
       allocate(F2(N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder213456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,M1,N2,t3D,F2)
       allocate(Z20(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K8*K8*K0*K6
       I3=K0
       call EGEMM(I1,I2,I3,X19,F2,Z20)
       deallocate(F2)
C
       call
     & sum124563(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z20,-1.000)
       deallocate(Z20)
       deallocate(X19)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(Q63(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q60,B2,Q63)
       deallocate(B2)
C
       call
     & sum21(N2,M2,N2,M2,X17,Q63,-1.000)
       deallocate(Q63)
C
       call sumx12(N2,N3,N2,N3,
     & N2,M2,N2,M2,X17,FBPP, 1.000)
C
       allocate(F2(N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder213456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,M1,N2,t3D,F2)
       allocate(Z18(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K8*K8*K0*K6
       I3=K0
       call EGEMM(I1,I2,I3,X17,F2,Z18)
       deallocate(F2)
C
       call
     & sum134562(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z18, 1.000)
       deallocate(Z18)
       deallocate(X17)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,N2,N0,N1,N1,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S299(M1+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S299)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder3421(M1,N2,N0,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N2,M1,N2,S299,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S301(N2+1:M2,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S301)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,N2,M2,M1,N2,M1,N2,X43,S301, 1.000)
       deallocate(S301)
C
       allocate(D2(N0+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,M2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(Z49(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K0
       I2=K8*K0*K6
       I3=K2
       call EGEMM(I1,I2,I3,X43,D2,Z49)
       deallocate(D2)
C
       call
     & sum134256(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z49, 1.000)
       call
     & sum135246(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z49,-1.000)
       call
     & sum134265(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z49,-1.000)
       call
     & sum135264(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z49, 1.000)
       call
     & sum136245(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z49, 1.000)
       call
     & sum136254(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z49,-1.000)
       deallocate(Z49)
       deallocate(X43)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder3421(M1,N2,N0,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N2,M1,N2,S299,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S302(N2+1:M2,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S302)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,N2,M2,M1,N2,M1,N2,X44,S302, 1.000)
       deallocate(S302)
C
       allocate(D2(N0+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,M2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(Z50(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K0
       I2=K8*K0*K6
       I3=K2
       call EGEMM(I1,I2,I3,X44,D2,Z50)
       deallocate(D2)
C
       call
     & sum124356(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z50,-1.000)
       call
     & sum125346(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z50, 1.000)
       call
     & sum124365(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z50, 1.000)
       call
     & sum125364(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z50,-1.000)
       call
     & sum126345(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z50,-1.000)
       call
     & sum126354(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z50, 1.000)
       deallocate(Z50)
       deallocate(X44)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder3421(M1,N2,N0,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N2,M1,N2,S299,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S300(M2+1:N3,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S300)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,M2,N3,M1,N2,M1,N2,X42,S300, 1.000)
       deallocate(S300)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S303(N2+1:M2,M1+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S303)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N2,M2,M1,N2,S303,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S312(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S312)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,N3,N2,M2,N2,M2,M1,N2,X3,S312, 1.000)
       deallocate(S312)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N2,M2,M1,N2,S303,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S304(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S304)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,M1,N2,X45,S304, 1.000)
       deallocate(S304)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S307(N2+1:M2,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4*K1
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S307)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder2431(N2,M2,N0,N1,N2,N3,N1,N3,
     & N0,N1,N1,N3,N2,N3,N2,M2,S307,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S308(M2+1:N3,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S308)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,M2,N3,N2,M2,M1,N2,X45,S308,-1.000)
       deallocate(S308)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S309(N2+1:M2,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4*K1
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S309)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder2431(N2,M2,N0,N1,N2,N3,N1,N3,
     & N0,N1,N1,N3,N2,N3,N2,M2,S309,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S310(M2+1:N3,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S310)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,M2,N3,N2,M2,M1,N2,X46,S310,-1.000)
       deallocate(S310)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S305(N2+1:M2,M1+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S305)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N2,M2,M1,N2,S305,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S306(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S306)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,M1,N2,X46,S306, 1.000)
       deallocate(S306)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N2,M2,M1,N2,S305,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S311(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S311)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,N2,M2,M1,N2,X3,S311,-1.000)
       deallocate(S311)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,N2,N0,N2,N2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S313(M1+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S313)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,M1,N2,S313,D1)
       allocate(D2(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,N2,M2,t2C,D2)
       allocate(S315(M2+1:N3,N2+1:M2,N2+1:N3,M1+1:N2))
       I1=K8*K4
       I2=K0*K6
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S315)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N2,N3,M2,N3,N2,M2,M1,N2,X74,S315,-1.000)
       deallocate(S315)
C
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,M1,N2,M1,N2,t2C,D2)
       allocate(Z188(N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K6
       I2=K8*K8*K0
       I3=K4
       call EGEMM(I1,I2,I3,X74,D2,Z188)
       deallocate(D2)
C
       call
     & sum245136(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z188, 0.500)
       call
     & sum345126(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z188,-0.500)
       call
     & sum246135(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z188,-0.500)
       call
     & sum346125(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z188, 0.500)
       call
     & sum256134(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z188, 0.500)
       call
     & sum356124(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z188,-0.500)
       deallocate(Z188)
       deallocate(X74)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder3421(M1,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N0,N2,M1,N2,S313,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S316(N2+1:M2,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S316)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder3124(N2,M2,M1,N2,N0,N2,M1,N2,
     & N0,N2,N2,M2,M1,N2,M1,N2,S316,D1)
       allocate(D2(N0+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,M2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(Z375(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K0
       I2=K8*K0*K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,D2,Z375)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum125346(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z375, 1.000)
       call
     & sum124356(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z375,-1.000)
       call
     & sum126345(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z375,-1.000)
       call
     & sum124365(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z375, 1.000)
       call
     & sum126354(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z375, 1.000)
       call
     & sum125364(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z375,-1.000)
       deallocate(Z375)
       deallocate(S316)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder2431(M1,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N0,N2,M1,N2,S313,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S317(N2+1:M2,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S317)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder3124(N2,M2,M1,N2,N0,N2,M1,N2,
     & N0,N2,N2,M2,M1,N2,M1,N2,S317,D1)
       allocate(D2(N0+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,M2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(Z376(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K0
       I2=K8*K0*K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,D2,Z376)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum135246(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z376, 1.000)
       call
     & sum134256(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z376,-1.000)
       call
     & sum136245(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z376,-1.000)
       call
     & sum134265(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z376, 1.000)
       call
     & sum136254(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z376, 1.000)
       call
     & sum135264(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z376,-1.000)
       deallocate(Z376)
       deallocate(S317)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder2431(M1,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N0,N2,M1,N2,S313,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S318(M2+1:N3,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S318)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,M2,N3,M1,N2,M1,N2,X42,S318,-1.000)
       deallocate(S318)
C
       allocate(D2(N0+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,M2,N2,M2,M1,N2,t2C,D2)
       allocate(Z48(N2+1:M2,N2+1:M2,M1+1:N2,M2+1:N3,M1+1:N2,M1+1:N2))
       I1=K8*K8*K6
       I2=K8*K0*K0
       I3=K2
       call EGEMM(I1,I2,I3,X42,D2,Z48)
       deallocate(D2)
C
       call
     & sum234156(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z48,-1.000)
       call
     & sum235146(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z48, 1.000)
       call
     & sum234165(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z48, 1.000)
       call
     & sum235164(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z48,-1.000)
       call
     & sum236145(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z48,-1.000)
       call
     & sum236154(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z48, 1.000)
       deallocate(Z48)
       deallocate(X42)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,M1+1:N2))
       call reorder4231(M1,N2,N0,N2,N0,N2,N2,N3,
     & N2,N3,N0,N2,N0,N2,M1,N2,S313,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S334(M1+1:N2,N0+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S334)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder2314(M1,N2,N0,N2,N0,N2,M1,N2,
     & N0,N2,N0,N2,M1,N2,M1,N2,S334,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S335(M2+1:N3,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S335)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,M2,N3,M1,N2,M1,N2,X1,S335, 1.000)
       deallocate(S335)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder3241(M1,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,M1,N2,S313,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S338(N2+1:M2,N0+1:N2,N2+1:N3,M1+1:N2))
       I1=K8*K4*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S338)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder2314(N2,M2,N0,N2,N2,N3,M1,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,S338,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S339(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S339)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,M1,N2,X45,S339, 1.000)
       deallocate(S339)
C
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,M1,N2,M1,N2,t2C,D2)
       allocate(Z51(N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K6
       I2=K8*K8*K0
       I3=K4
       call EGEMM(I1,I2,I3,X45,D2,Z51)
       deallocate(D2)
C
       call
     & sum345126(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z51, 1.000)
       call
     & sum346125(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z51,-1.000)
       call
     & sum356124(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z51, 1.000)
       deallocate(Z51)
       deallocate(X45)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder3241(M1,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,M1,N2,S313,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S340(N2+1:M2,N0+1:N2,N2+1:N3,M1+1:N2))
       I1=K8*K4*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S340)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder2314(N2,M2,N0,N2,N2,N3,M1,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,S340,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S341(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S341)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,M1,N2,X46,S341, 1.000)
       deallocate(S341)
C
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,M1,N2,M1,N2,t2C,D2)
       allocate(Z52(N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K6
       I2=K8*K8*K0
       I3=K4
       call EGEMM(I1,I2,I3,X46,D2,Z52)
       deallocate(D2)
C
       call
     & sum245136(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z52,-1.000)
       call
     & sum246135(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z52, 1.000)
       call
     & sum256134(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z52,-1.000)
       deallocate(Z52)
       deallocate(X46)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,M1,N2,S313,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,M2,N2,M2,t2C,D2)
       allocate(S314(N2+1:M2,N2+1:M2,N2+1:N3,M1+1:N2))
       I1=K8*K4
       I2=K0*K0
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S314)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N2,N3,N2,M2,N2,M2,M1,N2,X3,S314,-0.500)
       deallocate(S314)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder2314(M1,N2,N0,N2,N0,N2,M1,N2,
     & N0,N2,N0,N2,M1,N2,M1,N2,S334,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S336(N2+1:M2,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S336)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,N2,M2,M1,N2,M1,N2,X40,S336,-1.000)
       deallocate(S336)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder3214(M1,N2,N0,N2,N0,N2,M1,N2,
     & N0,N2,N0,N2,M1,N2,M1,N2,S334,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S337(N2+1:M2,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S337)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,N2,M2,M1,N2,M1,N2,X41,S337,-1.000)
       deallocate(S337)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder2314(N2,M2,N0,N2,N2,N3,M1,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,S340,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S342(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S342)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,N2,M2,M1,N2,X3,S342,-1.000)
       deallocate(S342)
C
       call sumx2314(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N2,M2,N2,M2,M1,N2,X3,VCHPPP, 1.000)
C
       allocate(D2(N2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,M2,N3,M1,N2,M1,N2,t2C,D2)
       allocate(Z4(M2+1:N3,M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K0
       I2=K8*K8*K6
       I3=K4
       call EGEMM(I1,I2,I3,X3,D2,Z4)
       deallocate(D2)
C
       call
     & sum145236(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z4,-1.000)
       call
     & sum146235(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z4, 1.000)
       call
     & sum156234(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z4,-1.000)
       deallocate(Z4)
       deallocate(X3)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder4312(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S327(N2+1:M2,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S327)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder2341(N2,M2,N0,N2,N2,N3,N2,N3,
     & N0,N2,N2,N3,N2,N3,N2,M2,S327,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S328(M2+1:N3,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S328)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,M2,N3,N2,M2,M1,N2,X79,S328,-2.000)
       deallocate(S328)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,VCHHPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,M1,N2,M1,N2,t2C,D2)
       allocate(S322(M1+1:N2,M1+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K8*K8
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S322)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder4312(M1,N2,M1,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,M1,N2,M1,N2,S322,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S333(N2+1:M2,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S333)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,N2,M2,M1,N2,M1,N2,X41,S333, 0.500)
       deallocate(S333)
C
       allocate(D2(N0+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,M2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(Z47(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K0
       I2=K8*K0*K6
       I3=K2
       call EGEMM(I1,I2,I3,X41,D2,Z47)
       deallocate(D2)
C
       call
     & sum124356(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z47,-1.000)
       call
     & sum125346(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z47, 1.000)
       call
     & sum126345(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z47,-1.000)
       deallocate(Z47)
       deallocate(X41)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder3412(M1,N2,M1,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,M1,N2,M1,N2,S322,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S323(M2+1:N3,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S323)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,M2,N3,M1,N2,M1,N2,X1,S323,-0.500)
       deallocate(S323)
C
       call sumx2134(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,M2,N3,M1,N2,M1,N2,X1,VCHHHP, 1.000)
C
       allocate(D2(N0+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,M2,N2,M2,M1,N2,t2C,D2)
       allocate(Z1(N2+1:M2,N2+1:M2,M1+1:N2,M2+1:N3,M1+1:N2,M1+1:N2))
       I1=K8*K8*K6
       I2=K8*K0*K0
       I3=K2
       call EGEMM(I1,I2,I3,X1,D2,Z1)
       deallocate(D2)
C
       call
     & sum234156(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z1,-1.000)
       call
     & sum235146(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z1, 1.000)
       call
     & sum236145(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z1,-1.000)
       deallocate(Z1)
       deallocate(X1)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder3412(M1,N2,M1,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,M1,N2,M1,N2,S322,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S329(N2+1:M2,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S329)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,N2,M2,M1,N2,M1,N2,X40,S329, 0.500)
       deallocate(S329)
C
       allocate(D2(N0+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,M2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(Z46(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K0
       I2=K8*K0*K6
       I3=K2
       call EGEMM(I1,I2,I3,X40,D2,Z46)
       deallocate(D2)
C
       call
     & sum134256(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z46,-1.000)
       call
     & sum135246(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z46, 1.000)
       call
     & sum136245(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z46,-1.000)
       deallocate(Z46)
       deallocate(X40)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder4312(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S330(N2+1:M2,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S330)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder2341(N2,M2,N0,N2,N2,N3,N2,N3,
     & N0,N2,N2,N3,N2,N3,N2,M2,S330,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S331(M2+1:N3,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S331)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,M2,N3,N2,M2,M1,N2,X78,S331, 2.000)
       deallocate(S331)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S319(N2+1:M2,M1+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S319)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N2,M2,M1,N2,S319,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S321(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S321)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,M1,N2,X79,S321, 2.000)
       deallocate(S321)
C
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,M1,N2,M1,N2,t2C,D2)
       allocate(Z246(N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K6
       I2=K8*K8*K0
       I3=K4
       call EGEMM(I1,I2,I3,X79,D2,Z246)
       deallocate(D2)
C
       call
     & sum356124(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z246, 0.500)
       call
     & sum346125(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z246,-0.500)
       call
     & sum345126(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z246, 0.500)
       deallocate(Z246)
       deallocate(X79)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N2,M2,M1,N2,S319,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S324(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S324)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,N2,M2,M1,N2,X73,S324, 1.000)
       deallocate(S324)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N2,M2,M1,N2,S319,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S325(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S325)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,N3,N2,M2,N2,M2,M1,N2,X73,S325,-1.000)
       deallocate(S325)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N2,M2,M1,N2,S319,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S320(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S320)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,M1,N2,X78,S320,-2.000)
       deallocate(S320)
C
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,M1,N2,M1,N2,t2C,D2)
       allocate(Z240(N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K6
       I2=K8*K8*K0
       I3=K4
       call EGEMM(I1,I2,I3,X78,D2,Z240)
       deallocate(D2)
C
       call
     & sum256134(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z240, 0.500)
       call
     & sum246135(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z240,-0.500)
       call
     & sum245136(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z240, 0.500)
       deallocate(Z240)
       deallocate(X78)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N2,N3,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q66(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q66)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D2(N0+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,M2,N2,M2,M1,N2,t2C,D2)
       allocate(S326(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K2
       call EGEMM(I1,I2,I3,Q66,D2,S326)
       deallocate(D2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X73,S326, 1.000)
       deallocate(S326)
C
       allocate(D2(N2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,M2,N3,M1,N2,M1,N2,t2C,D2)
       allocate(Z184(M2+1:N3,M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K0
       I2=K8*K8*K6
       I3=K4
       call EGEMM(I1,I2,I3,X73,D2,Z184)
       deallocate(D2)
C
       call
     & sum156234(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z184, 1.000)
       call
     & sum146235(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z184,-1.000)
       call
     & sum145236(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z184, 1.000)
       deallocate(Z184)
       deallocate(X73)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q66,B1)
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,M1,N2,M1,N2,t2C,D2)
       allocate(S332(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S332)
       deallocate(B1)
       deallocate(D2)
       deallocate(Q66)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X2,S332,-1.000)
       deallocate(S332)
C
       call sumx2134(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,M2,M1,N2,M1,N2,X2,VCHHHP, 1.000)
C
       allocate(D2(N0+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,M2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(Z2(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K0
       I2=K8*K0*K6
       I3=K2
       call EGEMM(I1,I2,I3,X2,D2,Z2)
       deallocate(D2)
C
       call
     & sum134256(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z2, 1.000)
       call
     & sum124356(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z2,-1.000)
       call
     & sum135246(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z2,-1.000)
       call
     & sum125346(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z2, 1.000)
       call
     & sum136245(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z2, 1.000)
       call
     & sum126345(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z2,-1.000)
       deallocate(Z2)
       deallocate(X2)
C
       allocate(D1(N2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,M2,N3,N2,M2,M1,N2,VCHPPP,D1)
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,M1,N2,M1,N2,t2C,D2)
       allocate(Z3(N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K6
       I2=K8*K8*K0
       I3=K4
       call EGEMM(I1,I2,I3,D1,D2,Z3)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum345126(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z3,-1.000)
       call
     & sum245136(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z3, 1.000)
       call
     & sum346125(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z3, 1.000)
       call
     & sum246135(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z3,-1.000)
       call
     & sum356124(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z3,-1.000)
       call
     & sum256134(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z3, 1.000)
       deallocate(Z3)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder4132(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,M1,M2,N3,N2,M2,M1,N2,VCHPHP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z28(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z28)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z28, 1.000)
       call
     & sum124536(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z28,-1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z28,-1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z28, 1.000)
       call
     & sum135624(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z28, 1.000)
       call
     & sum125634(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z28,-1.000)
       deallocate(Z28)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder4132(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,M2,N3,N2,M2,M1,N2,VCHPHP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z29(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z29)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z29, 1.000)
       call
     & sum124536(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z29,-1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z29,-1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z29, 1.000)
       call
     & sum135624(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z29, 1.000)
       call
     & sum125634(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z29,-1.000)
       deallocate(Z29)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder4132(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,M1,N2,M2,N2,M2,M1,N2,VCHPHP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z30(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z30)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z30,-1.000)
       call
     & sum124536(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z30, 1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z30, 1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z30,-1.000)
       call
     & sum135624(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z30,-1.000)
       call
     & sum125634(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z30, 1.000)
       deallocate(Z30)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder4132(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,N2,M2,N2,M2,M1,N2,VCHPHP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z31(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K8*K0*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z31)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z31,-1.000)
       call
     & sum124536(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z31, 1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z31, 1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z31,-1.000)
       call
     & sum135624(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z31,-1.000)
       call
     & sum125634(M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,V3D,Z31, 1.000)
       deallocate(Z31)
C
       call sumx3(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,M1,N2,HT3D,V3D,1.0)
       deallocate(V3D)
C
       end
