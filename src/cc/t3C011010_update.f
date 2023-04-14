       subroutine t3C011010_update(N0,N1,N2,N3,HT3C1,shift,
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
       real*8 HT3C1(N2+1:N3,N2+1:M2,N1+1:N3,N0+1:N2,M1+1:N2,N0+1:N1)
C
       real*8,allocatable::V3C(:,:,:,:,:,:)
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
       real*8,allocatable::Q1(:,:)
       real*8,allocatable::Q2(:,:)
       real*8,allocatable::Q3(:,:)
       real*8,allocatable::Q4(:,:)
       real*8,allocatable::S10(:,:,:,:)
       real*8,allocatable::S11(:,:,:,:)
       real*8,allocatable::S12(:,:,:,:)
       real*8,allocatable::S13(:,:,:,:)
       real*8,allocatable::Q5(:,:)
       real*8,allocatable::Q6(:,:)
       real*8,allocatable::S14(:,:,:,:)
       real*8,allocatable::S15(:,:,:,:)
       real*8,allocatable::S16(:,:,:,:)
       real*8,allocatable::S17(:,:,:,:)
       real*8,allocatable::Q7(:,:)
       real*8,allocatable::Q8(:,:)
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
       real*8,allocatable::Q9(:,:)
       real*8,allocatable::Q10(:,:)
       real*8,allocatable::U105(:,:,:,:,:,:)
       real*8,allocatable::S32(:,:,:,:)
       real*8,allocatable::S33(:,:,:,:)
       real*8,allocatable::S34(:,:,:,:)
       real*8,allocatable::Q11(:,:)
       real*8,allocatable::Q12(:,:)
       real*8,allocatable::S35(:,:,:,:)
       real*8,allocatable::S36(:,:,:,:)
       real*8,allocatable::S37(:,:,:,:)
       real*8,allocatable::Q13(:,:)
       real*8,allocatable::S38(:,:,:,:)
       real*8,allocatable::S39(:,:,:,:)
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
       real*8,allocatable::S73(:,:,:,:)
       real*8,allocatable::S74(:,:,:,:)
       real*8,allocatable::Q14(:,:)
       real*8,allocatable::Q15(:,:)
       real*8,allocatable::Q16(:,:)
       real*8,allocatable::Q17(:,:)
       real*8,allocatable::Q18(:,:)
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
       real*8,allocatable::Q19(:,:)
       real*8,allocatable::Q20(:,:)
       real*8,allocatable::S85(:,:,:,:)
       real*8,allocatable::S86(:,:,:,:)
       real*8,allocatable::S87(:,:,:,:)
       real*8,allocatable::S88(:,:,:,:)
       real*8,allocatable::U174(:,:,:,:,:,:)
       real*8,allocatable::S89(:,:,:,:)
       real*8,allocatable::S90(:,:,:,:)
       real*8,allocatable::S91(:,:,:,:)
       real*8,allocatable::S92(:,:,:,:)
       real*8,allocatable::S93(:,:,:,:)
       real*8,allocatable::S94(:,:,:,:)
       real*8,allocatable::Q21(:,:)
       real*8,allocatable::Q22(:,:)
       real*8,allocatable::S95(:,:,:,:)
       real*8,allocatable::S96(:,:,:,:)
       real*8,allocatable::S97(:,:,:,:)
       real*8,allocatable::S98(:,:,:,:)
       real*8,allocatable::S99(:,:,:,:)
       real*8,allocatable::S100(:,:,:,:)
       real*8,allocatable::S101(:,:,:,:)
       real*8,allocatable::S102(:,:,:,:)
       real*8,allocatable::Q23(:,:)
       real*8,allocatable::Q24(:,:)
       real*8,allocatable::S103(:,:,:,:)
       real*8,allocatable::S104(:,:,:,:)
       real*8,allocatable::S105(:,:,:,:)
       real*8,allocatable::S106(:,:,:,:)
       real*8,allocatable::S107(:,:,:,:)
       real*8,allocatable::S108(:,:,:,:)
       real*8,allocatable::S109(:,:,:,:)
       real*8,allocatable::S110(:,:,:,:)
       real*8,allocatable::Q25(:,:)
       real*8,allocatable::Q26(:,:)
       real*8,allocatable::S111(:,:,:,:)
       real*8,allocatable::S112(:,:,:,:)
       real*8,allocatable::Q27(:,:)
       real*8,allocatable::S113(:,:,:,:)
       real*8,allocatable::S114(:,:,:,:)
       real*8,allocatable::S115(:,:,:,:)
       real*8,allocatable::S116(:,:,:,:)
       real*8,allocatable::S117(:,:,:,:)
       real*8,allocatable::S118(:,:,:,:)
       real*8,allocatable::S119(:,:,:,:)
       real*8,allocatable::Q28(:,:)
       real*8,allocatable::Q29(:,:)
       real*8,allocatable::Q30(:,:)
       real*8,allocatable::Q31(:,:)
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
       real*8,allocatable::S215(:,:,:,:)
       real*8,allocatable::S216(:,:,:,:)
       real*8,allocatable::S217(:,:,:,:)
       real*8,allocatable::S218(:,:,:,:)
       real*8,allocatable::S219(:,:,:,:)
       real*8,allocatable::S220(:,:,:,:)
       real*8,allocatable::S221(:,:,:,:)
       real*8,allocatable::S222(:,:,:,:)
       real*8,allocatable::S223(:,:,:,:)
       real*8,allocatable::S224(:,:,:,:)
       real*8,allocatable::S225(:,:,:,:)
       real*8,allocatable::S226(:,:,:,:)
       real*8,allocatable::S227(:,:,:,:)
       real*8,allocatable::S228(:,:,:,:)
       real*8,allocatable::S229(:,:,:,:)
       real*8,allocatable::S230(:,:,:,:)
       real*8,allocatable::S231(:,:,:,:)
       real*8,allocatable::S232(:,:,:,:)
       real*8,allocatable::S233(:,:,:,:)
       real*8,allocatable::S234(:,:,:,:)
       real*8,allocatable::S235(:,:,:,:)
       real*8,allocatable::S236(:,:,:,:)
       real*8,allocatable::S237(:,:,:,:)
       real*8,allocatable::Q32(:,:)
       real*8,allocatable::Q33(:,:)
       real*8,allocatable::S238(:,:,:,:)
       real*8,allocatable::S239(:,:,:,:)
       real*8,allocatable::S240(:,:,:,:)
       real*8,allocatable::S241(:,:,:,:)
       real*8,allocatable::S242(:,:,:,:)
       real*8,allocatable::S243(:,:,:,:)
       real*8,allocatable::S244(:,:,:,:)
       real*8,allocatable::S245(:,:,:,:)
       real*8,allocatable::S246(:,:,:,:)
       real*8,allocatable::S247(:,:,:,:)
       real*8,allocatable::S248(:,:,:,:)
       real*8,allocatable::S249(:,:,:,:)
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
       real*8,allocatable::S260(:,:,:,:)
       real*8,allocatable::S261(:,:,:,:)
       real*8,allocatable::Q34(:,:)
       real*8,allocatable::Q35(:,:)
       real*8,allocatable::U364(:,:,:,:,:,:)
       real*8,allocatable::U490(:,:,:,:,:,:)
       real*8,allocatable::S263(:,:,:,:)
       real*8,allocatable::S264(:,:,:,:)
       real*8,allocatable::Q36(:,:)
       real*8,allocatable::Q37(:,:)
       real*8,allocatable::S265(:,:,:,:)
       real*8,allocatable::S266(:,:,:,:)
       real*8,allocatable::S267(:,:,:,:)
       real*8,allocatable::Q38(:,:)
       real*8,allocatable::Q39(:,:)
       real*8,allocatable::Q40(:,:)
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
       real*8,allocatable::S278(:,:,:,:)
       real*8,allocatable::S279(:,:,:,:)
       real*8,allocatable::S280(:,:,:,:)
       real*8,allocatable::S281(:,:,:,:)
       real*8,allocatable::S282(:,:,:,:)
       real*8,allocatable::S283(:,:,:,:)
       real*8,allocatable::S284(:,:,:,:)
       real*8,allocatable::S285(:,:,:,:)
       real*8,allocatable::S286(:,:,:,:)
       real*8,allocatable::S287(:,:,:,:)
       real*8,allocatable::S288(:,:,:,:)
       real*8,allocatable::S289(:,:,:,:)
       real*8,allocatable::S290(:,:,:,:)
       real*8,allocatable::S291(:,:,:,:)
       real*8,allocatable::S292(:,:,:,:)
       real*8,allocatable::S293(:,:,:,:)
       real*8,allocatable::S294(:,:,:,:)
       real*8,allocatable::S295(:,:,:,:)
       real*8,allocatable::S296(:,:,:,:)
       real*8,allocatable::S297(:,:,:,:)
       real*8,allocatable::S298(:,:,:,:)
       real*8,allocatable::S299(:,:,:,:)
       real*8,allocatable::S300(:,:,:,:)
       real*8,allocatable::S301(:,:,:,:)
       real*8,allocatable::S302(:,:,:,:)
       real*8,allocatable::S303(:,:,:,:)
       real*8,allocatable::S304(:,:,:,:)
       real*8,allocatable::S305(:,:,:,:)
       real*8,allocatable::S306(:,:,:,:)
       real*8,allocatable::S307(:,:,:,:)
       real*8,allocatable::S308(:,:,:,:)
       real*8,allocatable::S309(:,:,:,:)
       real*8,allocatable::S310(:,:,:,:)
       real*8,allocatable::S311(:,:,:,:)
       real*8,allocatable::S312(:,:,:,:)
       real*8,allocatable::S313(:,:,:,:)
       real*8,allocatable::S314(:,:,:,:)
       real*8,allocatable::S315(:,:,:,:)
       real*8,allocatable::S316(:,:,:,:)
       real*8,allocatable::S317(:,:,:,:)
       real*8,allocatable::S318(:,:,:,:)
       real*8,allocatable::S319(:,:,:,:)
       real*8,allocatable::S320(:,:,:,:)
       real*8,allocatable::S321(:,:,:,:)
       real*8,allocatable::S322(:,:,:,:)
       real*8,allocatable::S323(:,:,:,:)
       real*8,allocatable::S324(:,:,:,:)
       real*8,allocatable::S325(:,:,:,:)
       real*8,allocatable::S326(:,:,:,:)
       real*8,allocatable::Q41(:,:)
       real*8,allocatable::Q42(:,:)
       real*8,allocatable::S327(:,:,:,:)
       real*8,allocatable::S328(:,:,:,:)
       real*8,allocatable::Q43(:,:)
       real*8,allocatable::Q44(:,:)
       real*8,allocatable::Q45(:,:)
       real*8,allocatable::S329(:,:,:,:)
       real*8,allocatable::S351(:,:,:,:)
       real*8,allocatable::S352(:,:,:,:)
       real*8,allocatable::S330(:,:,:,:)
       real*8,allocatable::S335(:,:,:,:)
       real*8,allocatable::S336(:,:,:,:)
       real*8,allocatable::S337(:,:,:,:)
       real*8,allocatable::S338(:,:,:,:)
       real*8,allocatable::S339(:,:,:,:)
       real*8,allocatable::S340(:,:,:,:)
       real*8,allocatable::S341(:,:,:,:)
       real*8,allocatable::S342(:,:,:,:)
       real*8,allocatable::Q46(:,:)
       real*8,allocatable::Q47(:,:)
       real*8,allocatable::Q48(:,:)
       real*8,allocatable::Q49(:,:)
       real*8,allocatable::Q50(:,:)
       real*8,allocatable::Q51(:,:)
       real*8,allocatable::Q52(:,:)
       real*8,allocatable::Q53(:,:)
       real*8,allocatable::S343(:,:,:,:)
       real*8,allocatable::S344(:,:,:,:)
       real*8,allocatable::S345(:,:,:,:)
       real*8,allocatable::S346(:,:,:,:)
       real*8,allocatable::S347(:,:,:,:)
       real*8,allocatable::S348(:,:,:,:)
       real*8,allocatable::S349(:,:,:,:)
       real*8,allocatable::S350(:,:,:,:)
       real*8,allocatable::S331(:,:,:,:)
       real*8,allocatable::S353(:,:,:,:)
       real*8,allocatable::S332(:,:,:,:)
       real*8,allocatable::S333(:,:,:,:)
       real*8,allocatable::S354(:,:,:,:)
       real*8,allocatable::S334(:,:,:,:)
       real*8,allocatable::S355(:,:,:,:)
       real*8,allocatable::S357(:,:,:,:)
       real*8,allocatable::S356(:,:,:,:)
       real*8,allocatable::S358(:,:,:,:)
       real*8,allocatable::S359(:,:,:,:)
       real*8,allocatable::S360(:,:,:,:)
       real*8,allocatable::S361(:,:,:,:)
       real*8,allocatable::S362(:,:,:,:)
       real*8,allocatable::S434(:,:,:,:)
       real*8,allocatable::S435(:,:,:,:)
       real*8,allocatable::S363(:,:,:,:)
       real*8,allocatable::S367(:,:,:,:)
       real*8,allocatable::S369(:,:,:,:)
       real*8,allocatable::S370(:,:,:,:)
       real*8,allocatable::S368(:,:,:,:)
       real*8,allocatable::S371(:,:,:,:)
       real*8,allocatable::S372(:,:,:,:)
       real*8,allocatable::S373(:,:,:,:)
       real*8,allocatable::S374(:,:,:,:)
       real*8,allocatable::S375(:,:,:,:)
       real*8,allocatable::S376(:,:,:,:)
       real*8,allocatable::S377(:,:,:,:)
       real*8,allocatable::S378(:,:,:,:)
       real*8,allocatable::S379(:,:,:,:)
       real*8,allocatable::S380(:,:,:,:)
       real*8,allocatable::S381(:,:,:,:)
       real*8,allocatable::S382(:,:,:,:)
       real*8,allocatable::S383(:,:,:,:)
       real*8,allocatable::S387(:,:,:,:)
       real*8,allocatable::S384(:,:,:,:)
       real*8,allocatable::S385(:,:,:,:)
       real*8,allocatable::S388(:,:,:,:)
       real*8,allocatable::S386(:,:,:,:)
       real*8,allocatable::Q54(:,:)
       real*8,allocatable::Q55(:,:)
       real*8,allocatable::Q56(:,:)
       real*8,allocatable::Q57(:,:)
       real*8,allocatable::S389(:,:,:,:)
       real*8,allocatable::S390(:,:,:,:)
       real*8,allocatable::S391(:,:,:,:)
       real*8,allocatable::S392(:,:,:,:)
       real*8,allocatable::S393(:,:,:,:)
       real*8,allocatable::S394(:,:,:,:)
       real*8,allocatable::S395(:,:,:,:)
       real*8,allocatable::S396(:,:,:,:)
       real*8,allocatable::Q58(:,:)
       real*8,allocatable::Q59(:,:)
       real*8,allocatable::Q60(:,:)
       real*8,allocatable::Q61(:,:)
       real*8,allocatable::S262(:,:,:,:)
       real*8,allocatable::S397(:,:,:,:)
       real*8,allocatable::S403(:,:,:,:)
       real*8,allocatable::S398(:,:,:,:)
       real*8,allocatable::S401(:,:,:,:)
       real*8,allocatable::S405(:,:,:,:)
       real*8,allocatable::S402(:,:,:,:)
       real*8,allocatable::Q64(:,:)
       real*8,allocatable::Q66(:,:)
       real*8,allocatable::Q65(:,:)
       real*8,allocatable::S399(:,:,:,:)
       real*8,allocatable::S400(:,:,:,:)
       real*8,allocatable::S404(:,:,:,:)
       real*8,allocatable::Q62(:,:)
       real*8,allocatable::Q63(:,:)
       real*8,allocatable::Q67(:,:)
       real*8,allocatable::Q68(:,:)
       real*8,allocatable::Q69(:,:)
       real*8,allocatable::Q70(:,:)
       real*8,allocatable::S406(:,:,:,:)
       real*8,allocatable::S408(:,:,:,:)
       real*8,allocatable::S407(:,:,:,:)
       real*8,allocatable::S409(:,:,:,:)
       real*8,allocatable::S410(:,:,:,:)
       real*8,allocatable::Q71(:,:)
       real*8,allocatable::S414(:,:,:,:)
       real*8,allocatable::S411(:,:,:,:)
       real*8,allocatable::S412(:,:,:,:)
       real*8,allocatable::S413(:,:,:,:)
       real*8,allocatable::S415(:,:,:,:)
       real*8,allocatable::S417(:,:,:,:)
       real*8,allocatable::S418(:,:,:,:)
       real*8,allocatable::S419(:,:,:,:)
       real*8,allocatable::S420(:,:,:,:)
       real*8,allocatable::S518(:,:,:,:)
       real*8,allocatable::S519(:,:,:,:)
       real*8,allocatable::S520(:,:,:,:)
       real*8,allocatable::S521(:,:,:,:)
       real*8,allocatable::S416(:,:,:,:)
       real*8,allocatable::S523(:,:,:,:)
       real*8,allocatable::S524(:,:,:,:)
       real*8,allocatable::S522(:,:,:,:)
       real*8,allocatable::Q72(:,:)
       real*8,allocatable::S433(:,:,:,:)
       real*8,allocatable::S427(:,:,:,:)
       real*8,allocatable::S430(:,:,:,:)
       real*8,allocatable::S364(:,:,:,:)
       real*8,allocatable::S366(:,:,:,:)
       real*8,allocatable::S365(:,:,:,:)
       real*8,allocatable::S436(:,:,:,:)
       real*8,allocatable::S438(:,:,:,:)
       real*8,allocatable::S437(:,:,:,:)
       real*8,allocatable::S439(:,:,:,:)
       real*8,allocatable::S441(:,:,:,:)
       real*8,allocatable::S440(:,:,:,:)
       real*8,allocatable::S442(:,:,:,:)
       real*8,allocatable::S443(:,:,:,:)
       real*8,allocatable::S444(:,:,:,:)
       real*8,allocatable::S448(:,:,:,:)
       real*8,allocatable::S445(:,:,:,:)
       real*8,allocatable::S446(:,:,:,:)
       real*8,allocatable::S449(:,:,:,:)
       real*8,allocatable::S447(:,:,:,:)
       real*8,allocatable::S450(:,:,:,:)
       real*8,allocatable::S451(:,:,:,:)
       real*8,allocatable::S452(:,:,:,:)
       real*8,allocatable::S453(:,:,:,:)
       real*8,allocatable::S454(:,:,:,:)
       real*8,allocatable::S458(:,:,:,:)
       real*8,allocatable::S455(:,:,:,:)
       real*8,allocatable::S456(:,:,:,:)
       real*8,allocatable::S459(:,:,:,:)
       real*8,allocatable::S457(:,:,:,:)
       real*8,allocatable::S460(:,:,:,:)
       real*8,allocatable::S461(:,:,:,:)
       real*8,allocatable::S462(:,:,:,:)
       real*8,allocatable::S463(:,:,:,:)
       real*8,allocatable::S464(:,:,:,:)
       real*8,allocatable::S465(:,:,:,:)
       real*8,allocatable::S466(:,:,:,:)
       real*8,allocatable::S467(:,:,:,:)
       real*8,allocatable::S468(:,:,:,:)
       real*8,allocatable::S472(:,:,:,:)
       real*8,allocatable::S469(:,:,:,:)
       real*8,allocatable::S470(:,:,:,:)
       real*8,allocatable::S473(:,:,:,:)
       real*8,allocatable::S471(:,:,:,:)
       real*8,allocatable::Q73(:,:)
       real*8,allocatable::Q74(:,:)
       real*8,allocatable::Q75(:,:)
       real*8,allocatable::Q76(:,:)
       real*8,allocatable::S474(:,:,:,:)
       real*8,allocatable::S475(:,:,:,:)
       real*8,allocatable::S476(:,:,:,:)
       real*8,allocatable::S477(:,:,:,:)
       real*8,allocatable::Q77(:,:)
       real*8,allocatable::Q78(:,:)
       real*8,allocatable::Q79(:,:)
       real*8,allocatable::Q81(:,:)
       real*8,allocatable::Q80(:,:)
       real*8,allocatable::S478(:,:,:,:)
       real*8,allocatable::S483(:,:,:,:)
       real*8,allocatable::S484(:,:,:,:)
       real*8,allocatable::S485(:,:,:,:)
       real*8,allocatable::S487(:,:,:,:)
       real*8,allocatable::S525(:,:,:,:)
       real*8,allocatable::S526(:,:,:,:)
       real*8,allocatable::S479(:,:,:,:)
       real*8,allocatable::S527(:,:,:,:)
       real*8,allocatable::S480(:,:,:,:)
       real*8,allocatable::S481(:,:,:,:)
       real*8,allocatable::S482(:,:,:,:)
       real*8,allocatable::S489(:,:,:,:)
       real*8,allocatable::S494(:,:,:,:)
       real*8,allocatable::S490(:,:,:,:)
       real*8,allocatable::S425(:,:,:,:)
       real*8,allocatable::S498(:,:,:,:)
       real*8,allocatable::S426(:,:,:,:)
       real*8,allocatable::S493(:,:,:,:)
       real*8,allocatable::S496(:,:,:,:)
       real*8,allocatable::S497(:,:,:,:)
       real*8,allocatable::S491(:,:,:,:)
       real*8,allocatable::S492(:,:,:,:)
       real*8,allocatable::Q82(:,:)
       real*8,allocatable::S495(:,:,:,:)
       real*8,allocatable::S499(:,:,:,:)
       real*8,allocatable::S500(:,:,:,:)
       real*8,allocatable::S503(:,:,:,:)
       real*8,allocatable::S504(:,:,:,:)
       real*8,allocatable::S501(:,:,:,:)
       real*8,allocatable::S528(:,:,:,:)
       real*8,allocatable::S529(:,:,:,:)
       real*8,allocatable::S531(:,:,:,:)
       real*8,allocatable::S532(:,:,:,:)
       real*8,allocatable::S502(:,:,:,:)
       real*8,allocatable::S530(:,:,:,:)
       real*8,allocatable::S512(:,:,:,:)
       real*8,allocatable::S513(:,:,:,:)
       real*8,allocatable::S507(:,:,:,:)
       real*8,allocatable::S516(:,:,:,:)
       real*8,allocatable::S508(:,:,:,:)
       real*8,allocatable::S514(:,:,:,:)
       real*8,allocatable::Q83(:,:)
       real*8,allocatable::S515(:,:,:,:)
       real*8,allocatable::S509(:,:,:,:)
       real*8,allocatable::S517(:,:,:,:)
       real*8,allocatable::S505(:,:,:,:)
       real*8,allocatable::S506(:,:,:,:)
       real*8,allocatable::S431(:,:,:,:)
       real*8,allocatable::S488(:,:,:,:)
       real*8,allocatable::S432(:,:,:,:)
       real*8,allocatable::S428(:,:,:,:)
       real*8,allocatable::S486(:,:,:,:)
       real*8,allocatable::S429(:,:,:,:)
       real*8,allocatable::S421(:,:,:,:)
       real*8,allocatable::S422(:,:,:,:)
       real*8,allocatable::S423(:,:,:,:)
       real*8,allocatable::S424(:,:,:,:)
       real*8,allocatable::S510(:,:,:,:)
       real*8,allocatable::S511(:,:,:,:)
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
       real*8,allocatable::X9(:,:,:,:)
       real*8,allocatable::Z9(:,:,:,:,:,:)
       real*8,allocatable::X10(:,:,:,:)
       real*8,allocatable::Z10(:,:,:,:,:,:)
       real*8,allocatable::X11(:,:,:,:)
       real*8,allocatable::Z11(:,:,:,:,:,:)
       real*8,allocatable::X12(:,:,:,:)
       real*8,allocatable::Z12(:,:,:,:,:,:)
       real*8,allocatable::X13(:,:,:,:)
       real*8,allocatable::Z13(:,:,:,:,:,:)
       real*8,allocatable::X14(:,:,:,:)
       real*8,allocatable::Z14(:,:,:,:,:,:)
       real*8,allocatable::X15(:,:,:,:)
       real*8,allocatable::Z15(:,:,:,:,:,:)
       real*8,allocatable::X16(:,:,:,:)
       real*8,allocatable::Z16(:,:,:,:,:,:)
       real*8,allocatable::X17(:,:)
       real*8,allocatable::Z17(:,:,:,:,:,:)
       real*8,allocatable::X18(:,:)
       real*8,allocatable::Z18(:,:,:,:,:,:)
       real*8,allocatable::X19(:,:)
       real*8,allocatable::Z19(:,:,:,:,:,:)
       real*8,allocatable::X20(:,:)
       real*8,allocatable::Z20(:,:,:,:,:,:)
       real*8,allocatable::X21(:,:)
       real*8,allocatable::Z21(:,:,:,:,:,:)
       real*8,allocatable::X22(:,:)
       real*8,allocatable::Z22(:,:,:,:,:,:)
       real*8,allocatable::X23(:,:)
       real*8,allocatable::Z23(:,:,:,:,:,:)
       real*8,allocatable::X24(:,:)
       real*8,allocatable::Z24(:,:,:,:,:,:)
       real*8,allocatable::X25(:,:)
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
       real*8,allocatable::X35(:,:,:,:)
       real*8,allocatable::Z35(:,:,:,:,:,:)
       real*8,allocatable::X36(:,:,:,:)
       real*8,allocatable::Z36(:,:,:,:,:,:)
       real*8,allocatable::X37(:,:,:,:)
       real*8,allocatable::Z37(:,:,:,:,:,:)
       real*8,allocatable::X38(:,:,:,:)
       real*8,allocatable::Z38(:,:,:,:,:,:)
       real*8,allocatable::X39(:,:,:,:)
       real*8,allocatable::Z39(:,:,:,:,:,:)
       real*8,allocatable::X40(:,:,:,:)
       real*8,allocatable::Z40(:,:,:,:,:,:)
       real*8,allocatable::X41(:,:,:,:)
       real*8,allocatable::Z41(:,:,:,:,:,:)
       real*8,allocatable::X42(:,:,:,:)
       real*8,allocatable::Z42(:,:,:,:,:,:)
       real*8,allocatable::X43(:,:,:,:)
       real*8,allocatable::Z43(:,:,:,:,:,:)
       real*8,allocatable::X44(:,:,:,:)
       real*8,allocatable::Z44(:,:,:,:,:,:)
       real*8,allocatable::X45(:,:,:,:)
       real*8,allocatable::Z45(:,:,:,:,:,:)
       real*8,allocatable::X46(:,:,:,:)
       real*8,allocatable::Z46(:,:,:,:,:,:)
       real*8,allocatable::X47(:,:,:,:)
       real*8,allocatable::Z47(:,:,:,:,:,:)
       real*8,allocatable::X48(:,:,:,:)
       real*8,allocatable::Z48(:,:,:,:,:,:)
       real*8,allocatable::X49(:,:,:,:)
       real*8,allocatable::Z49(:,:,:,:,:,:)
       real*8,allocatable::X50(:,:,:,:)
       real*8,allocatable::Z50(:,:,:,:,:,:)
       real*8,allocatable::X51(:,:,:,:)
       real*8,allocatable::Z51(:,:,:,:,:,:)
       real*8,allocatable::X52(:,:,:,:)
       real*8,allocatable::Z52(:,:,:,:,:,:)
       real*8,allocatable::X53(:,:,:,:)
       real*8,allocatable::Z53(:,:,:,:,:,:)
       real*8,allocatable::X54(:,:,:,:)
       real*8,allocatable::Z54(:,:,:,:,:,:)
       real*8,allocatable::X55(:,:,:,:)
       real*8,allocatable::Z55(:,:,:,:,:,:)
       real*8,allocatable::X56(:,:,:,:)
       real*8,allocatable::Z56(:,:,:,:,:,:)
       real*8,allocatable::X57(:,:,:,:)
       real*8,allocatable::Z57(:,:,:,:,:,:)
       real*8,allocatable::X58(:,:,:,:)
       real*8,allocatable::Z58(:,:,:,:,:,:)
       real*8,allocatable::X59(:,:,:,:)
       real*8,allocatable::Z59(:,:,:,:,:,:)
       real*8,allocatable::Z60(:,:,:,:,:,:)
       real*8,allocatable::Z61(:,:,:,:,:,:)
       real*8,allocatable::Z62(:,:,:,:,:,:)
       real*8,allocatable::Z63(:,:,:,:,:,:)
       real*8,allocatable::Z77(:,:,:,:,:,:)
       real*8,allocatable::Z78(:,:,:,:,:,:)
       real*8,allocatable::Z79(:,:,:,:,:,:)
       real*8,allocatable::Z80(:,:,:,:,:,:)
       real*8,allocatable::Z83(:,:,:,:,:,:)
       real*8,allocatable::Z84(:,:,:,:,:,:)
       real*8,allocatable::Z85(:,:,:,:,:,:)
       real*8,allocatable::Z86(:,:,:,:,:,:)
       real*8,allocatable::X60(:,:,:,:)
       real*8,allocatable::Z93(:,:,:,:,:,:)
       real*8,allocatable::X61(:,:,:,:)
       real*8,allocatable::Z94(:,:,:,:,:,:)
       real*8,allocatable::X62(:,:,:,:)
       real*8,allocatable::Z95(:,:,:,:,:,:)
       real*8,allocatable::X63(:,:,:,:)
       real*8,allocatable::Z96(:,:,:,:,:,:)
       real*8,allocatable::X64(:,:,:,:)
       real*8,allocatable::Z97(:,:,:,:,:,:)
       real*8,allocatable::X65(:,:,:,:)
       real*8,allocatable::Z98(:,:,:,:,:,:)
       real*8,allocatable::X66(:,:,:,:)
       real*8,allocatable::Z99(:,:,:,:,:,:)
       real*8,allocatable::X67(:,:,:,:)
       real*8,allocatable::Z100(:,:,:,:,:,:)
       real*8,allocatable::X68(:,:,:,:)
       real*8,allocatable::Z101(:,:,:,:,:,:)
       real*8,allocatable::X69(:,:,:,:)
       real*8,allocatable::Z102(:,:,:,:,:,:)
       real*8,allocatable::X70(:,:)
       real*8,allocatable::Z103(:,:,:,:,:,:)
       real*8,allocatable::X71(:,:)
       real*8,allocatable::Z104(:,:,:,:,:,:)
       real*8,allocatable::Z106(:,:,:,:,:,:)
       real*8,allocatable::X72(:,:)
       real*8,allocatable::Z110(:,:,:,:,:,:)
       real*8,allocatable::X73(:,:)
       real*8,allocatable::Z111(:,:,:,:,:,:)
       real*8,allocatable::X74(:,:)
       real*8,allocatable::Z115(:,:,:,:,:,:)
       real*8,allocatable::Z116(:,:,:,:,:,:)
       real*8,allocatable::Z117(:,:,:,:,:,:)
       real*8,allocatable::Z118(:,:,:,:,:,:)
       real*8,allocatable::Z119(:,:,:,:,:,:)
       real*8,allocatable::Z120(:,:,:,:,:,:)
       real*8,allocatable::Z121(:,:,:,:,:,:)
       real*8,allocatable::Z122(:,:,:,:,:,:)
       real*8,allocatable::Z123(:,:,:,:,:,:)
       real*8,allocatable::X75(:,:,:,:)
       real*8,allocatable::Z133(:,:,:,:,:,:)
       real*8,allocatable::X76(:,:,:,:)
       real*8,allocatable::Z134(:,:,:,:,:,:)
       real*8,allocatable::X77(:,:,:,:,:,:)
       real*8,allocatable::Z175(:,:,:,:,:,:)
       real*8,allocatable::Z184(:,:,:,:,:,:)
       real*8,allocatable::Z185(:,:,:,:,:,:)
       real*8,allocatable::X78(:,:,:,:)
       real*8,allocatable::Z210(:,:,:,:,:,:)
       real*8,allocatable::X79(:,:,:,:)
       real*8,allocatable::Z211(:,:,:,:,:,:)
       real*8,allocatable::X80(:,:,:,:)
       real*8,allocatable::Z212(:,:,:,:,:,:)
       real*8,allocatable::X81(:,:,:,:)
       real*8,allocatable::Z213(:,:,:,:,:,:)
       real*8,allocatable::Z216(:,:,:,:,:,:)
       real*8,allocatable::Z217(:,:,:,:,:,:)
       real*8,allocatable::X82(:,:,:,:)
       real*8,allocatable::Z218(:,:,:,:,:,:)
       real*8,allocatable::X83(:,:,:,:)
       real*8,allocatable::Z219(:,:,:,:,:,:)
       real*8,allocatable::X84(:,:,:,:)
       real*8,allocatable::Z220(:,:,:,:,:,:)
       real*8,allocatable::X85(:,:,:,:)
       real*8,allocatable::Z221(:,:,:,:,:,:)
       real*8,allocatable::X86(:,:,:,:)
       real*8,allocatable::Z224(:,:,:,:,:,:)
       real*8,allocatable::X87(:,:,:,:)
       real*8,allocatable::Z226(:,:,:,:,:,:)
       real*8,allocatable::X88(:,:,:,:)
       real*8,allocatable::Z227(:,:,:,:,:,:)
       real*8,allocatable::X89(:,:,:,:)
       real*8,allocatable::Z235(:,:,:,:,:,:)
       real*8,allocatable::X90(:,:,:,:)
       real*8,allocatable::Z244(:,:,:,:,:,:)
       real*8,allocatable::Z346(:,:,:,:,:,:)
       real*8,allocatable::Z347(:,:,:,:,:,:)
       real*8,allocatable::Z348(:,:,:,:,:,:)
       real*8,allocatable::Z349(:,:,:,:,:,:)
       real*8,allocatable::Z356(:,:,:,:,:,:)
       real*8,allocatable::Z357(:,:,:,:,:,:)
       real*8,allocatable::Z365(:,:,:,:,:,:)
       real*8,allocatable::X91(:,:,:,:)
       real*8,allocatable::Z406(:,:,:,:,:,:)
C
       allocate(V3C(M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2,N0+1:M1))
       V3C=0.0d0
C
       allocate(D1(N1+1:N3,N0+1:N1,M2+1:N3,M1+1:N2))
       call reorder2431(N0,N2,N1,N3,N2,N3,N0,N1,
     & N1,N3,N0,N1,M2,N3,M1,N2,VBHPPH,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S1(N0+1:M1,N0+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K1
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S1)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X1(N0+1:N1,M2+1:N3,M1+1:N2,N0+1:M1))
       X1=0.0d0
       call
     & sum4123(N0,N1,M2,N3,M1,N2,N0,M1,X1,S1, 1.000)
       deallocate(S1)
C
       allocate(D1(N1+1:N3,N0+1:N1,N2+1:M2,M1+1:N2))
       call reorder2431(N0,N2,N1,N3,N2,N3,N0,N1,
     & N1,N3,N0,N1,N2,M2,M1,N2,VBHPPH,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S2(N0+1:M1,N0+1:N1,N2+1:M2,M1+1:N2))
       I1=K8*K0*K1
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S2)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X2(N0+1:N1,N2+1:M2,M1+1:N2,N0+1:M1))
       X2=0.0d0
       call
     & sum4123(N0,N1,N2,M2,M1,N2,N0,M1,X2,S2, 1.000)
       deallocate(S2)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,VBHPPH,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S3(M2+1:N3,N1+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K3
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S3)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X3(N1+1:N3,M2+1:N3,M2+1:N3,M1+1:N2))
       X3=0.0d0
       call
     & sum3124(N1,N3,M2,N3,M2,N3,M1,N2,X3,S3,-1.000)
       deallocate(S3)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,VBHPPH,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S4(M2+1:N3,N1+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K3
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S4)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X4(N1+1:N3,N2+1:M2,M2+1:N3,M1+1:N2))
       X4=0.0d0
       call
     & sum3124(N1,N3,N2,M2,M2,N3,M1,N2,X4,S4,-1.000)
       deallocate(S4)
C
       allocate(D1(N0+1:N1,N0+1:N2,M1+1:N2,N0+1:M1))
       call reorder4312(N0,N2,N0,N1,N0,N2,N0,N1,
     & N0,N1,N0,N2,M1,N2,N0,M1,VBHHHH,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S5(M2+1:N3,N0+1:N2,M1+1:N2,N0+1:M1))
       I1=K5*K8*K2
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S5)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X8(N0+1:N2,M2+1:N3,M1+1:N2,N0+1:M1))
       X8=0.0d0
       call
     & sum2134(N0,N2,M2,N3,M1,N2,N0,M1,X8,S5,-1.000)
       deallocate(S5)
C
       allocate(D1(N0+1:N1,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder4132(N2,N3,N0,N1,N2,N3,N0,N1,
     & N0,N1,N2,N3,M2,N3,N0,M1,VBHPHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S6(M2+1:N3,N2+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K4
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S6)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X9(N2+1:N3,M2+1:N3,M2+1:N3,N0+1:M1))
       X9=0.0d0
       call
     & sum3124(N2,N3,M2,N3,M2,N3,N0,M1,X9,S6,-1.000)
       deallocate(S6)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder4132(N2,N3,N0,N1,N2,N3,N0,N1,
     & N0,N1,N2,N3,N2,M2,N0,M1,VBHPHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S7(M2+1:N3,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S7)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X10(N2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1))
       X10=0.0d0
       call
     & sum3124(N2,N3,N2,M2,M2,N3,N0,M1,X10,S7,-1.000)
       deallocate(S7)
C
       allocate(D1(N1+1:N3,N0+1:N2,M2+1:N3,M1+1:N2))
       call reorder2341(N0,N2,N1,N3,N0,N2,N1,N3,
     & N1,N3,N0,N2,M2,N3,M1,N2,VBPHPH,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S8(N0+1:M1,N0+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K2
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S8)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,N2,M2,N3,M1,N2,N0,M1,X8,S8, 1.000)
       deallocate(S8)
C
       allocate(D1(N1+1:N3,N2+1:N3,N2+1:M2,M2+1:N3))
       call reorder2134(N2,N3,N1,N3,N2,M2,N1,N3,
     & N1,N3,N2,N3,N2,M2,M2,N3,VBPAPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S9(N0+1:M1,N2+1:N3,N2+1:M2,M2+1:N3))
       I1=K6*K0*K4
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S9)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N2,N3,N2,M2,M2,N3,N0,M1,X10,S9, 1.000)
       deallocate(S9)
C
       allocate(B1(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,FAHP,B1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(Q1(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,B1,B2,Q1)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X17(N0+1:M1,N0+1:M1))
       X17=0.0d0
       call
     & sum21(N0,M1,N0,M1,X17,Q1, 1.000)
       deallocate(Q1)
C
       allocate(B1(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,FAHP,B1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(Q2(N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,B1,B2,Q2)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X18(M1+1:N1,N0+1:M1))
       X18=0.0d0
       call
     & sum21(M1,N1,N0,M1,X18,Q2, 1.000)
       deallocate(Q2)
C
       allocate(B1(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,FAHP,B1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(Q3(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,B1,B2,Q3)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X19(M2+1:N3,M2+1:N3))
       X19=0.0d0
       call
     & sum21(M2,N3,M2,N3,X19,Q3,-1.000)
       deallocate(Q3)
C
       allocate(B1(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,FAHP,B1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(Q4(M2+1:N3,N1+1:M2))
       I1=K9
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,B1,B2,Q4)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X20(N1+1:M2,M2+1:N3))
       X20=0.0d0
       call
     & sum21(N1,M2,M2,N3,X20,Q4,-1.000)
       deallocate(Q4)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder4312(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,M1,M2,N3,N0,M1,VAHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S10(M2+1:N3,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S10)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder2314(M2,N3,N0,M1,M2,N3,N0,M1,
     & N0,M1,M2,N3,M2,N3,N0,M1,S10,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z77(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K6
       I3=K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z77)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z77,-1.000)
       deallocate(Z77)
       deallocate(S10)
C
       allocate(D1(N0+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder4312(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,M1,N1,M2,N3,N0,M1,VAHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S11(M2+1:N3,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S11)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder2314(M2,N3,M1,N1,M2,N3,N0,M1,
     & M1,N1,M2,N3,M2,N3,N0,M1,S11,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z78(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K6
       I3=K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,Z78)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z78,-1.000)
       deallocate(Z78)
       deallocate(S11)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder4312(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,M1,N1,M2,N0,M1,VAHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S12(M2+1:N3,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S12)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N1+1:M2,M2+1:N3,N0+1:M1))
       call reorder2314(M2,N3,N0,M1,N1,M2,N0,M1,
     & N0,M1,N1,M2,M2,N3,N0,M1,S12,D1)
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N1,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z79(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K6
       I3=K9*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z79)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z79,-1.000)
       deallocate(Z79)
       deallocate(S12)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder4312(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,M1,N1,N1,M2,N0,M1,VAHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S13(M2+1:N3,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S13)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N1,N1+1:M2,M2+1:N3,N0+1:M1))
       call reorder2314(M2,N3,M1,N1,N1,M2,N0,M1,
     & M1,N1,N1,M2,M2,N3,N0,M1,S13,D1)
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N1,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z80(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K6
       I3=K9*K7
       call DMATMAT(I1,I2,I3,D1,F2,Z80)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z80,-1.000)
       deallocate(Z80)
       deallocate(S13)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder3142(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,N0,M1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q5(N0+1:M1,N0+1:M1))
       I1=K5*K5
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q5)
       deallocate(D1)
       deallocate(B2)
C
       X17=X17+Q5
       deallocate(Q5)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N0+1:M1))
       call reorder3142(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,N0,M1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q6(M1+1:N1,N0+1:M1))
       I1=K5*K7
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q6)
       deallocate(D1)
       deallocate(B2)
C
       X18=X18+Q6
       deallocate(Q6)
C
       allocate(D1(N1+1:N3,N0+1:M1,M2+1:N3,M2+1:N3))
       call reorder2413(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N0,M1,M2,N3,M2,N3,VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S14(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K5
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S14)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,M1,M2,N3,M2,N3,
     & N0,M1,M2,N3,M2,N3,N0,M1,S14,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z83(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K6
       I3=K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z83)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z83,-1.000)
       deallocate(Z83)
       deallocate(S14)
C
       allocate(D1(N1+1:N3,M1+1:N1,M2+1:N3,M2+1:N3))
       call reorder2413(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,M1,N1,M2,N3,M2,N3,VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S15(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K7
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S15)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,M1,N1,M2,N3,M2,N3,
     & M1,N1,M2,N3,M2,N3,N0,M1,S15,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z84(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K6
       I3=K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,Z84)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z84,-1.000)
       deallocate(Z84)
       deallocate(S15)
C
       allocate(D1(N1+1:N3,N0+1:M1,N1+1:M2,M2+1:N3))
       call reorder2413(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N0,M1,N1,M2,M2,N3,VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S16(N0+1:M1,N0+1:M1,N1+1:M2,M2+1:N3))
       I1=K6*K9*K5
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S16)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N1+1:M2,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,M1,N1,M2,M2,N3,
     & N0,M1,N1,M2,M2,N3,N0,M1,S16,D1)
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N1,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z85(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K6
       I3=K9*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z85)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z85,-1.000)
       deallocate(Z85)
       deallocate(S16)
C
       allocate(D1(N1+1:N3,M1+1:N1,N1+1:M2,M2+1:N3))
       call reorder2413(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,M1,N1,N1,M2,M2,N3,VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S17(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3))
       I1=K6*K9*K7
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S17)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N1,N1+1:M2,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,M1,N1,N1,M2,M2,N3,
     & M1,N1,N1,M2,M2,N3,N0,M1,S17,D1)
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N1,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z86(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K6
       I3=K9*K7
       call DMATMAT(I1,I2,I3,D1,F2,Z86)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z86,-1.000)
       deallocate(Z86)
       deallocate(S17)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M2+1:N3))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,M2,N3,M2,N3,VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q7(M2+1:N3,M2+1:N3))
       I1=K6*K6
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q7)
       deallocate(D1)
       deallocate(B2)
C
       X19=X19-Q7
       deallocate(Q7)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M2+1:N3))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,M2,M2,N3,VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q8(N1+1:M2,M2+1:N3))
       I1=K6*K9
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q8)
       deallocate(D1)
       deallocate(B2)
C
       X20=X20-Q8
       deallocate(Q8)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:M1,M1+1:N2))
       call reorder2341(N0,N2,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,N0,M1,M1,N2,VBHHPH,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S18(N0+1:M1,N0+1:M1,N0+1:M1,M1+1:N2))
       I1=K8*K5*K5
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S18)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X30(N0+1:M1,N0+1:M1,M1+1:N2,N0+1:M1))
       X30=0.0d0
       call
     & sum4123(N0,M1,N0,M1,M1,N2,N0,M1,X30,S18, 1.000)
       deallocate(S18)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:M1,M1+1:N2))
       call reorder2341(N0,N2,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,N0,M1,M1,N2,VBHHPH,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S19(N0+1:M1,M1+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K8
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S19)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X31(M1+1:N2,N0+1:M1,M1+1:N2,N0+1:M1))
       X31=0.0d0
       call
     & sum4123(M1,N2,N0,M1,M1,N2,N0,M1,X31,S19, 1.000)
       deallocate(S19)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,M1+1:N2))
       call reorder2341(N0,N2,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,M1,N1,M1,N2,VBHHPH,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S20(N0+1:M1,N0+1:M1,M1+1:N1,M1+1:N2))
       I1=K8*K7*K5
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S20)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X32(N0+1:M1,M1+1:N1,M1+1:N2,N0+1:M1))
       X32=0.0d0
       call
     & sum4123(N0,M1,M1,N1,M1,N2,N0,M1,X32,S20, 1.000)
       deallocate(S20)
C
       allocate(D1(N1+1:N3,M1+1:N2,M1+1:N1,M1+1:N2))
       call reorder2341(N0,N2,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,M1,N1,M1,N2,VBHHPH,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S21(N0+1:M1,M1+1:N2,M1+1:N1,M1+1:N2))
       I1=K8*K7*K8
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S21)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X33(M1+1:N2,M1+1:N1,M1+1:N2,N0+1:M1))
       X33=0.0d0
       call
     & sum4123(M1,N2,M1,N1,M1,N2,N0,M1,X33,S21, 1.000)
       deallocate(S21)
C
       allocate(D1(N1+1:N3,N0+1:M1,M2+1:N3,M2+1:N3))
       call reorder2413(N2,N3,N1,N3,N2,N3,N0,N1,
     & N1,N3,N0,M1,M2,N3,M2,N3,VBHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S22(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K5
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S22)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X60(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:M1))
       X60=0.0d0
       call
     & sum4123(N0,M1,M2,N3,M2,N3,N0,M1,X60,S22, 1.000)
       deallocate(S22)
C
       allocate(D1(N1+1:N3,M1+1:N1,M2+1:N3,M2+1:N3))
       call reorder2413(N2,N3,N1,N3,N2,N3,N0,N1,
     & N1,N3,M1,N1,M2,N3,M2,N3,VBHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S23(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K7
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S23)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X61(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:M1))
       X61=0.0d0
       call
     & sum4123(M1,N1,M2,N3,M2,N3,N0,M1,X61,S23, 1.000)
       deallocate(S23)
C
       allocate(D1(N1+1:N3,N0+1:M1,N2+1:M2,M2+1:N3))
       call reorder2413(N2,N3,N1,N3,N2,N3,N0,N1,
     & N1,N3,N0,M1,N2,M2,M2,N3,VBHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S24(N0+1:M1,N0+1:M1,N2+1:M2,M2+1:N3))
       I1=K6*K0*K5
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S24)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X62(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:M1))
       X62=0.0d0
       call
     & sum4123(N0,M1,N2,M2,M2,N3,N0,M1,X62,S24, 1.000)
       deallocate(S24)
C
       allocate(D1(N1+1:N3,M1+1:N1,N2+1:M2,M2+1:N3))
       call reorder2413(N2,N3,N1,N3,N2,N3,N0,N1,
     & N1,N3,M1,N1,N2,M2,M2,N3,VBHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S25(N0+1:M1,M1+1:N1,N2+1:M2,M2+1:N3))
       I1=K6*K0*K7
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S25)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X63(M1+1:N1,N2+1:M2,M2+1:N3,N0+1:M1))
       X63=0.0d0
       call
     & sum4123(M1,N1,N2,M2,M2,N3,N0,M1,X63,S25, 1.000)
       deallocate(S25)
C
       allocate(D1(N1+1:N3,N0+1:M1,N2+1:M2,N2+1:M2))
       call reorder2413(N2,N3,N1,N3,N2,N3,N0,N1,
     & N1,N3,N0,M1,N2,M2,N2,M2,VBHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S26(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:M2))
       I1=K0*K0*K5
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S26)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X64(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:M1))
       X64=0.0d0
       call
     & sum4123(N0,M1,N2,M2,N2,M2,N0,M1,X64,S26, 1.000)
       deallocate(S26)
C
       allocate(D1(N1+1:N3,M1+1:N1,N2+1:M2,N2+1:M2))
       call reorder2413(N2,N3,N1,N3,N2,N3,N0,N1,
     & N1,N3,M1,N1,N2,M2,N2,M2,VBHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S27(N0+1:M1,M1+1:N1,N2+1:M2,N2+1:M2))
       I1=K0*K0*K7
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S27)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X65(M1+1:N1,N2+1:M2,N2+1:M2,N0+1:M1))
       X65=0.0d0
       call
     & sum4123(M1,N1,N2,M2,N2,M2,N0,M1,X65,S27, 1.000)
       deallocate(S27)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder4321(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N0,M1,M2,N3,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S28(M2+1:N3,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S28)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X66(N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N2))
       X66=0.0d0
       call
     & sum3124(N0,M1,M2,N3,M2,N3,M1,N2,X66,S28, 1.000)
       deallocate(S28)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder4321(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,M1,N2,M2,N3,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S29(M2+1:N3,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S29)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X67(M1+1:N2,M2+1:N3,M2+1:N3,M1+1:N2))
       X67=0.0d0
       call
     & sum3124(M1,N2,M2,N3,M2,N3,M1,N2,X67,S29, 1.000)
       deallocate(S29)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,M1+1:N2))
       call reorder4321(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N0,M1,N1,M2,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S30(M2+1:N3,N0+1:M1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K5
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S30)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X68(N0+1:M1,N1+1:M2,M2+1:N3,M1+1:N2))
       X68=0.0d0
       call
     & sum3124(N0,M1,N1,M2,M2,N3,M1,N2,X68,S30, 1.000)
       deallocate(S30)
C
       allocate(D1(N0+1:N1,M1+1:N2,N1+1:M2,M1+1:N2))
       call reorder4321(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,M1,N2,N1,M2,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S31(M2+1:N3,M1+1:N2,N1+1:M2,M1+1:N2))
       I1=K8*K9*K8
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S31)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X69(M1+1:N2,N1+1:M2,M2+1:N3,M1+1:N2))
       X69=0.0d0
       call
     & sum3124(M1,N2,N1,M2,M2,N3,M1,N2,X69,S31, 1.000)
       deallocate(S31)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M1+1:N2))
       call reorder4231(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q9(N0+1:M1,M1+1:N2))
       I1=K8*K5
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q9)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X70(N0+1:M1,M1+1:N2))
       X70=0.0d0
       X70=X70+Q9
       deallocate(Q9)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M1+1:N2))
       call reorder4231(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q10(M1+1:N2,M1+1:N2))
       I1=K8*K8
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q10)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X71(M1+1:N2,M1+1:N2))
       X71=0.0d0
       X71=X71+Q10
       deallocate(Q10)
C
       allocate(D1(M2+1:N3,M2+1:N3,N0+1:N1,M2+1:N3))
       call reorder1243(N2,N3,N1,N3,N2,N3,N0,N1,
     & M2,N3,M2,N3,N0,N1,M2,N3,VBHPPP,D1)
       allocate(F2(M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N0+1:M1))
       call reorder132456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,N0,M1,t3C1,F2)
       allocate(U105(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:M1,N0+1:N1,M2+1:N3))
       I1=K6*K1
       I2=K5*K8*K8*K0
       I3=K6*K6
       call DMATMAT(I1,I2,I3,D1,F2,U105)
       deallocate(D1)
       deallocate(F2)
C
       allocate(F1(N0+1:N1,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N0+1:M1))
       call reorder561234(N2,M2,M1,N2,M1,N2,N0,M1,N0,N1,M2,N3,
     & N0,N1,M2,N3,N2,M2,M1,N2,M1,N2,N0,M1,U105,F1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(Z106(M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N0+1:M1))
       I1=K5*K8*K8*K0*K6
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,F1,B2,Z106)
       deallocate(F1)
       deallocate(B2)
C
       call
     & sum312456(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z106,-1.000)
       deallocate(Z106)
       deallocate(U105)
C
       allocate(D1(N0+1:N1,N2+1:M2,M2+1:N3,M2+1:N3))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N2,M2,M2,N3,M2,N3,VBHPPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S32(M2+1:N3,N2+1:M2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K0
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S32)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X44(N2+1:M2,M2+1:N3,M2+1:N3,M2+1:N3))
       X44=0.0d0
       call
     & sum4123(N2,M2,M2,N3,M2,N3,M2,N3,X44,S32,-1.000)
       deallocate(S32)
C
       allocate(D1(N0+1:N1,M2+1:N3,N1+1:M2,M2+1:N3))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,M2,N3,N1,M2,M2,N3,VBHPPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S33(M2+1:N3,M2+1:N3,N1+1:M2,M2+1:N3))
       I1=K6*K9*K6
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S33)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X45(M2+1:N3,N1+1:M2,M2+1:N3,M2+1:N3))
       X45=0.0d0
       call
     & sum4123(M2,N3,N1,M2,M2,N3,M2,N3,X45,S33,-1.000)
       deallocate(S33)
C
       allocate(D1(N0+1:N1,N2+1:M2,N1+1:M2,M2+1:N3))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N2,M2,N1,M2,M2,N3,VBHPPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S34(M2+1:N3,N2+1:M2,N1+1:M2,M2+1:N3))
       I1=K6*K9*K0
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S34)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X46(N2+1:M2,N1+1:M2,M2+1:N3,M2+1:N3))
       X46=0.0d0
       call
     & sum4123(N2,M2,N1,M2,M2,N3,M2,N3,X46,S34,-1.000)
       deallocate(S34)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M2+1:N3))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,M2,N3,M2,N3,VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q11(M2+1:N3,M2+1:N3))
       I1=K6*K6
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q11)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X72(M2+1:N3,M2+1:N3))
       X72=0.0d0
       X72=X72+Q11
       deallocate(Q11)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,M2+1:N3))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,M2,M2,N3,VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q12(N2+1:M2,M2+1:N3))
       I1=K6*K0
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q12)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X73(N2+1:M2,M2+1:N3))
       X73=0.0d0
       X73=X73+Q12
       deallocate(Q12)
C
       allocate(D1(N0+1:N1,N2+1:M2,M2+1:N3,N2+1:M2))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N2,M2,M2,N3,N2,M2,VBHPPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S35(M2+1:N3,N2+1:M2,M2+1:N3,N2+1:M2))
       I1=K0*K6*K0
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S35)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X47(N2+1:M2,M2+1:N3,N2+1:M2,M2+1:N3))
       X47=0.0d0
       call
     & sum4123(N2,M2,M2,N3,N2,M2,M2,N3,X47,S35,-1.000)
       deallocate(S35)
C
       allocate(D1(N0+1:N1,M2+1:N3,N1+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,M2,N3,N1,M2,N2,M2,VBHPPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S36(M2+1:N3,M2+1:N3,N1+1:M2,N2+1:M2))
       I1=K0*K9*K6
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S36)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X48(M2+1:N3,N1+1:M2,N2+1:M2,M2+1:N3))
       X48=0.0d0
       call
     & sum4123(M2,N3,N1,M2,N2,M2,M2,N3,X48,S36,-1.000)
       deallocate(S36)
C
       allocate(D1(N0+1:N1,N2+1:M2,N1+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N2,M2,N1,M2,N2,M2,VBHPPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S37(M2+1:N3,N2+1:M2,N1+1:M2,N2+1:M2))
       I1=K0*K9*K0
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S37)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X49(N2+1:M2,N1+1:M2,N2+1:M2,M2+1:N3))
       X49=0.0d0
       call
     & sum4123(N2,M2,N1,M2,N2,M2,M2,N3,X49,S37,-1.000)
       deallocate(S37)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,N2+1:M2))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,M2,N2,M2,VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q13(N2+1:M2,N2+1:M2))
       I1=K0*K0
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q13)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X74(N2+1:M2,N2+1:M2))
       X74=0.0d0
       X74=X74+Q13
       deallocate(Q13)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N0,M1,M2,N3,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S38(M2+1:N3,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S38)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder2314(M2,N3,N0,M1,M2,N3,N0,M1,
     & N0,M1,M2,N3,M2,N3,N0,M1,S38,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z116(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K6
       I3=K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z116)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z116,-1.000)
       deallocate(Z116)
       deallocate(S38)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,M1,N2,M2,N3,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S39(M2+1:N3,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S39)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder2314(M2,N3,M1,N2,M2,N3,N0,M1,
     & M1,N2,M2,N3,M2,N3,N0,M1,S39,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z117(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K6
       I3=K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z117)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z117,-1.000)
       deallocate(Z117)
       deallocate(S39)
C
       allocate(D1(N0+1:N1,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N0,M1,N2,M2,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S40(M2+1:N3,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S40)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder2314(M2,N3,N0,M1,N2,M2,N0,M1,
     & N0,M1,N2,M2,M2,N3,N0,M1,S40,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z118(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K6
       I3=K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z118)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z118, 1.000)
       deallocate(Z118)
       deallocate(S40)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,M1,N2,N2,M2,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S41(M2+1:N3,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S41)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder2314(M2,N3,M1,N2,N2,M2,N0,M1,
     & M1,N2,N2,M2,M2,N3,N0,M1,S41,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z119(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K6
       I3=K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z119)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z119, 1.000)
       deallocate(Z119)
       deallocate(S41)
C
       allocate(D1(N1+1:N3,N0+1:M1,M2+1:N3,M2+1:N3))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,N0,M1,M2,N3,M2,N3,VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S42(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K5
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S42)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,M1,M2,N3,M2,N3,
     & N0,M1,M2,N3,M2,N3,N0,M1,S42,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z120(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K6
       I3=K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z120)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z120, 1.000)
       deallocate(Z120)
       deallocate(S42)
C
       allocate(D1(N1+1:N3,M1+1:N2,M2+1:N3,M2+1:N3))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,M1,N2,M2,N3,M2,N3,VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S43(N0+1:M1,M1+1:N2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K8
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S43)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,M1,N2,M2,N3,M2,N3,
     & M1,N2,M2,N3,M2,N3,N0,M1,S43,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z121(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K6
       I3=K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z121)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z121, 1.000)
       deallocate(Z121)
       deallocate(S43)
C
       allocate(D1(N1+1:N3,N0+1:M1,N2+1:M2,M2+1:N3))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,N0,M1,N2,M2,M2,N3,VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S44(N0+1:M1,N0+1:M1,N2+1:M2,M2+1:N3))
       I1=K6*K0*K5
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S44)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,M1,N2,M2,M2,N3,
     & N0,M1,N2,M2,M2,N3,N0,M1,S44,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z122(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K6
       I3=K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z122)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z122,-1.000)
       deallocate(Z122)
       deallocate(S44)
C
       allocate(D1(N1+1:N3,M1+1:N2,N2+1:M2,M2+1:N3))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,M1,N2,N2,M2,M2,N3,VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S45(N0+1:M1,M1+1:N2,N2+1:M2,M2+1:N3))
       I1=K6*K0*K8
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S45)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,M1,N2,N2,M2,M2,N3,
     & M1,N2,N2,M2,M2,N3,N0,M1,S45,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z123(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K6
       I3=K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z123)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z123,-1.000)
       deallocate(Z123)
       deallocate(S45)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,N0+1:M1))
       call reorder3412(N0,N2,N0,N1,N0,N2,N0,N1,
     & N0,N2,N0,N1,M1,N2,N0,M1,VBHHHH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S46(M2+1:N3,N0+1:N1,M1+1:N2,N0+1:M1))
       I1=K5*K8*K1
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S46)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,M2,N3,M1,N2,N0,M1,X1,S46,-1.000)
       deallocate(S46)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,N0+1:M1))
       call reorder3412(N0,N2,N0,N1,N0,N2,N0,N1,
     & N0,N2,N0,N1,M1,N2,N0,M1,VBHHHH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S47(N2+1:M2,N0+1:N1,M1+1:N2,N0+1:M1))
       I1=K5*K8*K1
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S47)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N2,M2,M1,N2,N0,M1,X2,S47,-1.000)
       deallocate(S47)
C
       allocate(D1(N2+1:N3,N0+1:N1,M2+1:N3,N0+1:M1))
       call reorder1432(N2,N3,N0,N1,N2,N3,N0,N1,
     & N2,N3,N0,N1,M2,N3,N0,M1,VBHPHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S48(M1+1:N2,N0+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K1
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S48)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N1,M2,N3,M1,N2,N0,M1,X1,S48, 1.000)
       deallocate(S48)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:M2,N0+1:M1))
       call reorder1432(N2,N3,N0,N1,N2,N3,N0,N1,
     & N2,N3,N0,N1,N2,M2,N0,M1,VBHPHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S49(M1+1:N2,N0+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K1
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S49)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N1,N2,M2,M1,N2,N0,M1,X2,S49, 1.000)
       deallocate(S49)
C
       allocate(D1(N0+1:N2,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder3241(N0,N2,N1,N3,N0,N2,N1,N3,
     & N0,N2,N1,N3,M2,N3,M1,N2,VBPHPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S50(M2+1:N3,N1+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K3
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S50)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,M2,N3,M1,N2,X3,S50,-1.000)
       deallocate(S50)
C
       allocate(D1(N0+1:N2,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder3241(N0,N2,N1,N3,N0,N2,N1,N3,
     & N0,N2,N1,N3,M2,N3,M1,N2,VBPHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S51(N2+1:M2,N1+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K3
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S51)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,N2,M2,M2,N3,M1,N2,X4,S51,-1.000)
       deallocate(S51)
C
       allocate(D1(N2+1:N3,N1+1:N3,N2+1:M2,M2+1:N3))
       call reorder1234(N2,N3,N1,N3,N2,M2,N1,N3,
     & N2,N3,N1,N3,N2,M2,M2,N3,VBPAPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S52(M1+1:N2,N1+1:N3,N2+1:M2,M2+1:N3))
       I1=K6*K0*K3
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S52)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N1,N3,N2,M2,M2,N3,M1,N2,X4,S52, 1.000)
       deallocate(S52)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,M1,N2,M1,N2,VCHHHH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S53(M2+1:N3,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S53)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X5(N0+1:N2,M2+1:N3,M1+1:N2,M1+1:N2))
       X5=0.0d0
       call
     & sum2134(N0,N2,M2,N3,M1,N2,M1,N2,X5,S53,-1.000)
       deallocate(S53)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,M1,N2,M1,N2,VCHHHH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S54(N2+1:M2,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S54)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X6(N0+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       X6=0.0d0
       call
     & sum2134(N0,N2,N2,M2,M1,N2,M1,N2,X6,S54,-1.000)
       deallocate(S54)
C
       allocate(D1(N2+1:N3,N0+1:N2,M2+1:N3,M1+1:N2))
       call reorder1432(N2,N3,N0,N2,N2,N3,N0,N2,
     & N2,N3,N0,N2,M2,N3,M1,N2,VCHPHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S55(M1+1:N2,N0+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K2
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S55)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X75(N0+1:N2,M2+1:N3,M1+1:N2,M1+1:N2))
       X75=0.0d0
       call
     & sum3124(N0,N2,M2,N3,M1,N2,M1,N2,X75,S55, 1.000)
       deallocate(S55)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:M2,M1+1:N2))
       call reorder1432(N2,N3,N0,N2,N2,N3,N0,N2,
     & N2,N3,N0,N2,N2,M2,M1,N2,VCHPHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S56(M1+1:N2,N0+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K2
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S56)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X76(N0+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       X76=0.0d0
       call
     & sum3124(N0,N2,N2,M2,M1,N2,M1,N2,X76,S56, 1.000)
       deallocate(S56)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder4132(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,VCHPHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S57(N2+1:M2,N2+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K4
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S57)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X7(N2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       X7=0.0d0
       call
     & sum3124(N2,N3,M2,N3,N2,M2,M1,N2,X7,S57,-1.000)
       deallocate(S57)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder4132(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,VCHPHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S58(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S58)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,M1,N2,X7,S58, 1.000)
       deallocate(S58)
C
       allocate(D1(N2+1:N3,N2+1:N3,M2+1:N3,N2+1:M2))
       call reorder1234(N2,N3,N2,N3,N2,N3,N2,M2,
     & N2,N3,N2,N3,M2,N3,N2,M2,VCAPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S59(M1+1:N2,N2+1:N3,M2+1:N3,N2+1:M2))
       I1=K0*K6*K4
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S59)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N2,N3,M2,N3,N2,M2,M1,N2,X7,S59,-1.000)
       deallocate(S59)
C
       allocate(D1(N2+1:N3,N0+1:N2,M2+1:N3,N0+1:M1))
       call reorder3124(N0,N2,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,N2,M2,N3,N0,M1,VBHPPH,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S60(M1+1:N2,N0+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K2
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S60)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N2,M2,N3,M1,N2,N0,M1,X8,S60, 1.000)
       deallocate(S60)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,VBHPPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S61(M2+1:N3,N2+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K4
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S61)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,M2,N3,N0,M1,X9,S61,-1.000)
       deallocate(S61)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,VBHPPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S62(N2+1:M2,N2+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K4
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S62)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,M2,N3,N0,M1,X10,S62,-1.000)
       deallocate(S62)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,M1,M2,N3,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S63(M2+1:N3,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S63)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X11(N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N2))
       X11=0.0d0
       call
     & sum3124(N0,M1,M2,N3,M2,N3,M1,N2,X11,S63,-1.000)
       deallocate(S63)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,M2,N3,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S64(M2+1:N3,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K7
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S64)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X12(M1+1:N1,M2+1:N3,M2+1:N3,M1+1:N2))
       X12=0.0d0
       call
     & sum3124(M1,N1,M2,N3,M2,N3,M1,N2,X12,S64,-1.000)
       deallocate(S64)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,M1,N1,M2,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S65(M2+1:N3,N0+1:M1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K5
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S65)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X13(N0+1:M1,N1+1:M2,M2+1:N3,M1+1:N2))
       X13=0.0d0
       call
     & sum3124(N0,M1,N1,M2,M2,N3,M1,N2,X13,S65,-1.000)
       deallocate(S65)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,N1,M2,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S66(M2+1:N3,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K7
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S66)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X14(M1+1:N1,N1+1:M2,M2+1:N3,M1+1:N2))
       X14=0.0d0
       call
     & sum3124(M1,N1,N1,M2,M2,N3,M1,N2,X14,S66,-1.000)
       deallocate(S66)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,M1,N1,M2,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S67(N2+1:M2,N0+1:M1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K5
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S67)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X15(N0+1:M1,N1+1:M2,N2+1:M2,M1+1:N2))
       X15=0.0d0
       call
     & sum3124(N0,M1,N1,M2,N2,M2,M1,N2,X15,S67,-1.000)
       deallocate(S67)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,N1,M2,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S68(N2+1:M2,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K7
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S68)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X16(M1+1:N1,N1+1:M2,N2+1:M2,M1+1:N2))
       X16=0.0d0
       call
     & sum3124(M1,N1,N1,M2,N2,M2,M1,N2,X16,S68,-1.000)
       deallocate(S68)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,M2+1:N3))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,M1,M2,N3,M2,N3,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S69(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K5
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S69)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,M2,N3,M2,N3,M1,N2,X11,S69, 1.000)
       deallocate(S69)
C
       allocate(D1(N2+1:N3,M1+1:N1,M2+1:N3,M2+1:N3))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,M1,N1,M2,N3,M2,N3,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S70(M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K7
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S70)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,M2,N3,M2,N3,M1,N2,X12,S70, 1.000)
       deallocate(S70)
C
       allocate(D1(N2+1:N3,N0+1:M1,N1+1:M2,M2+1:N3))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,M1,N1,M2,M2,N3,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S71(M1+1:N2,N0+1:M1,N1+1:M2,M2+1:N3))
       I1=K6*K9*K5
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S71)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,N1,M2,M2,N3,M1,N2,X13,S71, 1.000)
       deallocate(S71)
C
       allocate(D1(N2+1:N3,M1+1:N1,N1+1:M2,M2+1:N3))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,M1,N1,N1,M2,M2,N3,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S72(M1+1:N2,M1+1:N1,N1+1:M2,M2+1:N3))
       I1=K6*K9*K7
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S72)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,N1,M2,M2,N3,M1,N2,X14,S72, 1.000)
       deallocate(S72)
C
       allocate(D1(N2+1:N3,N0+1:M1,N1+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,M1,N1,M2,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S73(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:M2))
       I1=K0*K9*K5
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S73)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,N1,M2,N2,M2,M1,N2,X15,S73, 1.000)
       deallocate(S73)
C
       allocate(D1(N2+1:N3,M1+1:N1,N1+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,M1,N1,N1,M2,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S74(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:M2))
       I1=K0*K9*K7
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S74)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,N1,M2,N2,M2,M1,N2,X16,S74, 1.000)
       deallocate(S74)
C
       allocate(B1(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,FBHP,B1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(Q14(M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,B1,B2,Q14)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X21(N0+1:M1,M1+1:N2))
       X21=0.0d0
       call
     & sum21(N0,M1,M1,N2,X21,Q14, 1.000)
       deallocate(Q14)
C
       allocate(B1(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,FBHP,B1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(Q15(M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,B1,B2,Q15)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X22(M1+1:N2,M1+1:N2))
       X22=0.0d0
       call
     & sum21(M1,N2,M1,N2,X22,Q15, 1.000)
       deallocate(Q15)
C
       allocate(B1(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,FBHP,B1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(Q16(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,B1,B2,Q16)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X23(M2+1:N3,M2+1:N3))
       X23=0.0d0
       call
     & sum21(M2,N3,M2,N3,X23,Q16,-1.000)
       deallocate(Q16)
C
       allocate(B1(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,FBHP,B1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(Q17(M2+1:N3,N2+1:M2))
       I1=K0
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,B1,B2,Q17)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X24(N2+1:M2,M2+1:N3))
       X24=0.0d0
       call
     & sum21(N2,M2,M2,N3,X24,Q17,-1.000)
       deallocate(Q17)
C
       allocate(B1(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,FBHP,B1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(Q18(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,B1,B2,Q18)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X25(N2+1:M2,N2+1:M2))
       X25=0.0d0
       call
     & sum21(N2,M2,N2,M2,X25,Q18,-1.000)
       deallocate(Q18)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:M1,N0+1:M1))
       call reorder1342(N2,N3,N0,N1,N0,N2,N0,N1,
     & N2,N3,N0,M1,N0,M1,N0,M1,VBHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S75(M1+1:N2,N0+1:M1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K5
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S75)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N0,M1,M1,N2,N0,M1,X30,S75, 1.000)
       deallocate(S75)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:M1,N0+1:M1))
       call reorder1342(N2,N3,N0,N1,N0,N2,N0,N1,
     & N2,N3,M1,N2,N0,M1,N0,M1,VBHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S76(M1+1:N2,M1+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K8
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S76)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,N0,M1,M1,N2,N0,M1,X31,S76, 1.000)
       deallocate(S76)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N1,N0+1:M1))
       call reorder1342(N2,N3,N0,N1,N0,N2,N0,N1,
     & N2,N3,N0,M1,M1,N1,N0,M1,VBHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S77(M1+1:N2,N0+1:M1,M1+1:N1,N0+1:M1))
       I1=K5*K7*K5
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S77)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M1,N1,M1,N2,N0,M1,X32,S77, 1.000)
       deallocate(S77)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N1,N0+1:M1))
       call reorder1342(N2,N3,N0,N1,N0,N2,N0,N1,
     & N2,N3,M1,N2,M1,N1,N0,M1,VBHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S78(M1+1:N2,M1+1:N2,M1+1:N1,N0+1:M1))
       I1=K5*K7*K8
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S78)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M1,N1,M1,N2,N0,M1,X33,S78, 1.000)
       deallocate(S78)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N0,M1,M2,N3,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S79(M2+1:N3,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S79)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X34(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:M1))
       X34=0.0d0
       call
     & sum3124(N0,M1,M2,N3,M2,N3,N0,M1,X34,S79,-1.000)
       deallocate(S79)
C
       call sumx2431(N2,N3,N0,N1,N2,N3,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,M1,X34,VBHPHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder612345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,t3C1,F2)
       allocate(Z34(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K6*K0
       I3=K6*K5
       call DMATMAT(I1,I2,I3,X34,F2,Z34)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z34,-1.000)
       deallocate(Z34)
       deallocate(X34)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,M1,N1,M2,N3,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S80(M2+1:N3,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S80)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X35(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:M1))
       X35=0.0d0
       call
     & sum3124(M1,N1,M2,N3,M2,N3,N0,M1,X35,S80,-1.000)
       deallocate(S80)
C
       call sumx2431(N2,N3,N0,N1,N2,N3,N0,N1,
     & M1,N1,M2,N3,M2,N3,N0,M1,X35,VBHPHP, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder612345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,t3C1,F2)
       allocate(Z35(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K6*K0
       I3=K6*K7
       call DMATMAT(I1,I2,I3,X35,F2,Z35)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z35,-1.000)
       deallocate(Z35)
       deallocate(X35)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N0,M1,N2,M2,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S81(M2+1:N3,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S81)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X36(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:M1))
       X36=0.0d0
       call
     & sum3124(N0,M1,N2,M2,M2,N3,N0,M1,X36,S81,-1.000)
       deallocate(S81)
C
       call sumx2431(N2,N3,N0,N1,N2,N3,N0,N1,
     & N0,M1,N2,M2,M2,N3,N0,M1,X36,VBHPHP, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder612345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N2,M2,M2,N3,M1,N2,M1,N2,t3C1,F2)
       allocate(Z36(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K6*K0
       I3=K0*K5
       call DMATMAT(I1,I2,I3,X36,F2,Z36)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z36,-1.000)
       deallocate(Z36)
       deallocate(X36)
C
       allocate(D1(N0+1:N2,M1+1:N1,N2+1:M2,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,M1,N1,N2,M2,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S82(M2+1:N3,M1+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K7
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S82)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X37(M1+1:N1,N2+1:M2,M2+1:N3,N0+1:M1))
       X37=0.0d0
       call
     & sum3124(M1,N1,N2,M2,M2,N3,N0,M1,X37,S82,-1.000)
       deallocate(S82)
C
       call sumx2431(N2,N3,N0,N1,N2,N3,N0,N1,
     & M1,N1,N2,M2,M2,N3,N0,M1,X37,VBHPHP, 1.000)
C
       allocate(F2(M1+1:N1,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder612345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,N2,M2,M2,N3,M1,N2,M1,N2,t3C1,F2)
       allocate(Z37(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K6*K0
       I3=K0*K7
       call DMATMAT(I1,I2,I3,X37,F2,Z37)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z37,-1.000)
       deallocate(Z37)
       deallocate(X37)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N0,M1,N2,M2,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S83(N2+1:M2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S83)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X38(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:M1))
       X38=0.0d0
       call
     & sum3124(N0,M1,N2,M2,N2,M2,N0,M1,X38,S83,-1.000)
       deallocate(S83)
C
       call sumx2431(N2,N3,N0,N1,N2,N3,N0,N1,
     & N0,M1,N2,M2,N2,M2,N0,M1,X38,VBHPHP, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder621345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,M2,N3,M1,N2,M1,N2,t3C1,F2)
       allocate(Z38(M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K6*K6
       I3=K0*K5
       call DMATMAT(I1,I2,I3,X38,F2,Z38)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z38,-1.000)
       deallocate(Z38)
       deallocate(X38)
C
       allocate(D1(N0+1:N2,M1+1:N1,N2+1:M2,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,M1,N1,N2,M2,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S84(N2+1:M2,M1+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K7
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S84)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X39(M1+1:N1,N2+1:M2,N2+1:M2,N0+1:M1))
       X39=0.0d0
       call
     & sum3124(M1,N1,N2,M2,N2,M2,N0,M1,X39,S84,-1.000)
       deallocate(S84)
C
       call sumx2431(N2,N3,N0,N1,N2,N3,N0,N1,
     & M1,N1,N2,M2,N2,M2,N0,M1,X39,VBHPHP, 1.000)
C
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder621345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,M2,N3,M2,N3,M1,N2,M1,N2,t3C1,F2)
       allocate(Z39(M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K6*K6
       I3=K0*K7
       call DMATMAT(I1,I2,I3,X39,F2,Z39)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z39,-1.000)
       deallocate(Z39)
       deallocate(X39)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N0+1:M1))
       call reorder3142(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q19(N0+1:M1,N0+1:M1))
       I1=K5*K5
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q19)
       deallocate(D1)
       deallocate(B2)
C
       X17=X17+Q19
       deallocate(Q19)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N0+1:M1))
       call reorder3142(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q20(M1+1:N1,N0+1:M1))
       I1=K5*K7
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q20)
       deallocate(D1)
       deallocate(B2)
C
       X18=X18+Q20
       deallocate(Q20)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,M2+1:N3))
       call reorder1324(N2,N3,N1,N3,N0,N2,N1,N3,
     & N2,N3,N0,M1,M2,N3,M2,N3,VBPHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S85(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K5
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S85)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X40(N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N2))
       X40=0.0d0
       call
     & sum4123(N0,M1,M2,N3,M2,N3,M1,N2,X40,S85, 1.000)
       deallocate(S85)
C
       call sumx4213(N0,N2,N1,N3,N0,N2,N1,N3,
     & N0,M1,M2,N3,M2,N3,M1,N2,X40,VBPHPH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder431256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z40(M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K5*K8*K0*K6
       I3=K6*K5
       call DMATMAT(I1,I2,I3,X40,F2,Z40)
       deallocate(F2)
C
       call
     & sum124635(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z40, 1.000)
       call
     & sum125634(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z40,-1.000)
       deallocate(Z40)
       deallocate(X40)
C
       allocate(D1(N2+1:N3,M1+1:N2,M2+1:N3,M2+1:N3))
       call reorder1324(N2,N3,N1,N3,N0,N2,N1,N3,
     & N2,N3,M1,N2,M2,N3,M2,N3,VBPHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S86(M1+1:N2,M1+1:N2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K8
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S86)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X41(M1+1:N2,M2+1:N3,M2+1:N3,M1+1:N2))
       X41=0.0d0
       call
     & sum4123(M1,N2,M2,N3,M2,N3,M1,N2,X41,S86, 1.000)
       deallocate(S86)
C
       call sumx4213(N0,N2,N1,N3,N0,N2,N1,N3,
     & M1,N2,M2,N3,M2,N3,M1,N2,X41,VBPHPH, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder431256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z41(M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K5*K8*K0*K6
       I3=K6*K8
       call DMATMAT(I1,I2,I3,X41,F2,Z41)
       deallocate(F2)
C
       call
     & sum124635(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z41, 1.000)
       call
     & sum125634(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z41,-1.000)
       deallocate(Z41)
       deallocate(X41)
C
       allocate(D1(N2+1:N3,N0+1:M1,N1+1:M2,M2+1:N3))
       call reorder1324(N2,N3,N1,N3,N0,N2,N1,N3,
     & N2,N3,N0,M1,N1,M2,M2,N3,VBPHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S87(M1+1:N2,N0+1:M1,N1+1:M2,M2+1:N3))
       I1=K6*K9*K5
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S87)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X42(N0+1:M1,N1+1:M2,M2+1:N3,M1+1:N2))
       X42=0.0d0
       call
     & sum4123(N0,M1,N1,M2,M2,N3,M1,N2,X42,S87, 1.000)
       deallocate(S87)
C
       call sumx4213(N0,N2,N1,N3,N0,N2,N1,N3,
     & N0,M1,N1,M2,M2,N3,M1,N2,X42,VBPHPH, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder431256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N1,M2,M2,N3,N2,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z42(M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K5*K8*K0*K6
       I3=K9*K5
       call DMATMAT(I1,I2,I3,X42,F2,Z42)
       deallocate(F2)
C
       call
     & sum124635(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z42, 1.000)
       call
     & sum125634(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z42,-1.000)
       deallocate(Z42)
       deallocate(X42)
C
       allocate(D1(N2+1:N3,M1+1:N2,N1+1:M2,M2+1:N3))
       call reorder1324(N2,N3,N1,N3,N0,N2,N1,N3,
     & N2,N3,M1,N2,N1,M2,M2,N3,VBPHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S88(M1+1:N2,M1+1:N2,N1+1:M2,M2+1:N3))
       I1=K6*K9*K8
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S88)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X43(M1+1:N2,N1+1:M2,M2+1:N3,M1+1:N2))
       X43=0.0d0
       call
     & sum4123(M1,N2,N1,M2,M2,N3,M1,N2,X43,S88, 1.000)
       deallocate(S88)
C
       call sumx4213(N0,N2,N1,N3,N0,N2,N1,N3,
     & M1,N2,N1,M2,M2,N3,M1,N2,X43,VBPHPH, 1.000)
C
       allocate(F2(M1+1:N2,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder431256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N1,M2,M2,N3,N2,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z43(M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K5*K8*K0*K6
       I3=K9*K8
       call DMATMAT(I1,I2,I3,X43,F2,Z43)
       deallocate(F2)
C
       call
     & sum124635(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z43, 1.000)
       call
     & sum125634(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z43,-1.000)
       deallocate(Z43)
       deallocate(X43)
C
       allocate(D1(M2+1:N3,M2+1:N3,N0+1:N2,M2+1:N3))
       call reorder1234(N2,N3,N1,N3,N0,N2,N1,N3,
     & M2,N3,M2,N3,N0,N2,M2,N3,VBPHPP,D1)
       allocate(F2(M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N0+1:M1))
       call reorder132456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,N0,M1,t3C1,F2)
       allocate(U174(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:M1,N0+1:N2,M2+1:N3))
       I1=K6*K2
       I2=K5*K8*K8*K0
       I3=K6*K6
       call DMATMAT(I1,I2,I3,D1,F2,U174)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X77(N0+1:N2,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2,N0+1:M1))
       X77=0.0d0
       call
     & sum245613(N0,N2,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,X77,U174, 1.000)
       deallocate(U174)
C
       allocate(D1(N0+1:N2,N2+1:M2,M2+1:N3,M2+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,M2,M2,N3,M2,N3,VBPHPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S89(M2+1:N3,N2+1:M2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K0
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S89)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,M2,M2,N3,M2,N3,M2,N3,X44,S89,-1.000)
       deallocate(S89)
C
       allocate(D1(N0+1:N2,M2+1:N3,N1+1:M2,M2+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,M2,N3,N1,M2,M2,N3,VBPHPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S90(M2+1:N3,M2+1:N3,N1+1:M2,M2+1:N3))
       I1=K6*K9*K6
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S90)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,N1,M2,M2,N3,M2,N3,X45,S90,-1.000)
       deallocate(S90)
C
       allocate(D1(N0+1:N2,N2+1:M2,N1+1:M2,M2+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,M2,N1,M2,M2,N3,VBPHPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S91(M2+1:N3,N2+1:M2,N1+1:M2,M2+1:N3))
       I1=K6*K9*K0
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S91)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,M2,N1,M2,M2,N3,M2,N3,X46,S91,-1.000)
       deallocate(S91)
C
       allocate(D1(N0+1:N2,N2+1:M2,M2+1:N3,M2+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,M2,M2,N3,M2,N3,VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S92(N2+1:M2,N2+1:M2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K0
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S92)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,M2,M2,N3,N2,M2,M2,N3,X47,S92,-1.000)
       deallocate(S92)
C
       allocate(D1(N0+1:N2,M2+1:N3,N1+1:M2,M2+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,M2,N3,N1,M2,M2,N3,VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S93(N2+1:M2,M2+1:N3,N1+1:M2,M2+1:N3))
       I1=K6*K9*K6
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S93)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,N1,M2,N2,M2,M2,N3,X48,S93,-1.000)
       deallocate(S93)
C
       allocate(D1(N0+1:N2,N2+1:M2,N1+1:M2,M2+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,M2,N1,M2,M2,N3,VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S94(N2+1:M2,N2+1:M2,N1+1:M2,M2+1:N3))
       I1=K6*K9*K0
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S94)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,M2,N1,M2,N2,M2,M2,N3,X49,S94,-1.000)
       deallocate(S94)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,M2+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,N3,M2,N3,M2,N3,VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q21(M2+1:N3,M2+1:N3))
       I1=K6*K6
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q21)
       deallocate(D1)
       deallocate(B2)
C
       X19=X19+Q21
       deallocate(Q21)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,M2+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,N3,N1,M2,M2,N3,VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q22(N1+1:M2,M2+1:N3))
       I1=K6*K9
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q22)
       deallocate(D1)
       deallocate(B2)
C
       X20=X20+Q22
       deallocate(Q22)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder1342(N2,N3,N0,N2,N0,N2,N0,N2,
     & N2,N3,N0,M1,M1,N2,M1,N2,VCHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S95(M1+1:N2,N0+1:M1,M1+1:N2,M1+1:N2))
       I1=K8*K8*K5
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S95)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder2314(M1,N2,N0,M1,M1,N2,M1,N2,
     & N0,M1,M1,N2,M1,N2,M1,N2,S95,D1)
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder451236(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N2,M2,N3,N2,M2,M2,N3,N0,M1,t3C1,F2)
       allocate(Z184(M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1,M1+1:N2,M1+1:N2))
       I1=K8*K8
       I2=K5*K6*K0*K6
       I3=K8*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z184)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum123645(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z184, 1.000)
       call
     & sum123654(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z184,-1.000)
       deallocate(Z184)
       deallocate(S95)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder1342(N2,N3,N0,N2,N0,N2,N0,N2,
     & N2,N3,M1,N2,M1,N2,M1,N2,VCHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S96(M1+1:N2,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K8
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S96)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder2314(M1,N2,M1,N2,M1,N2,M1,N2,
     & M1,N2,M1,N2,M1,N2,M1,N2,S96,D1)
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder451236(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N2,M2,N3,N2,M2,M2,N3,N0,M1,t3C1,F2)
       allocate(Z185(M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1,M1+1:N2,M1+1:N2))
       I1=K8*K8
       I2=K5*K6*K0*K6
       I3=K8*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z185)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum123645(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z185, 0.500)
       call
     & sum123654(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z185,-0.500)
       deallocate(Z185)
       deallocate(S96)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,M1,M2,N3,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S97(M2+1:N3,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S97)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X52(N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N2))
       X52=0.0d0
       call
     & sum3124(N0,M1,M2,N3,M2,N3,M1,N2,X52,S97,-1.000)
       deallocate(S97)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,M1,N2,M2,N3,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S98(M2+1:N3,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S98)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X53(M1+1:N2,M2+1:N3,M2+1:N3,M1+1:N2))
       X53=0.0d0
       call
     & sum3124(M1,N2,M2,N3,M2,N3,M1,N2,X53,S98,-1.000)
       deallocate(S98)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,M1+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,M1,N2,M2,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S99(M2+1:N3,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0*K5
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S99)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X54(N0+1:M1,N2+1:M2,M2+1:N3,M1+1:N2))
       X54=0.0d0
       call
     & sum3124(N0,M1,N2,M2,M2,N3,M1,N2,X54,S99,-1.000)
       deallocate(S99)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,M1,N2,N2,M2,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S100(M2+1:N3,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K8
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S100)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X55(M1+1:N2,N2+1:M2,M2+1:N3,M1+1:N2))
       X55=0.0d0
       call
     & sum3124(M1,N2,N2,M2,M2,N3,M1,N2,X55,S100,-1.000)
       deallocate(S100)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,M1+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,M1,N2,M2,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S101(N2+1:M2,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0*K5
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S101)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X56(N0+1:M1,N2+1:M2,N2+1:M2,M1+1:N2))
       X56=0.0d0
       call
     & sum3124(N0,M1,N2,M2,N2,M2,M1,N2,X56,S101,-1.000)
       deallocate(S101)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,M1,N2,N2,M2,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S102(N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K8
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S102)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X57(M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       X57=0.0d0
       call
     & sum3124(M1,N2,N2,M2,N2,M2,M1,N2,X57,S102,-1.000)
       deallocate(S102)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M1+1:N2))
       call reorder3142(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q23(N0+1:M1,M1+1:N2))
       I1=K8*K5
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q23)
       deallocate(D1)
       deallocate(B2)
C
       X21=X21+Q23
       deallocate(Q23)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder3142(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q24(M1+1:N2,M1+1:N2))
       I1=K8*K8
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q24)
       deallocate(D1)
       deallocate(B2)
C
       X22=X22+Q24
       deallocate(Q24)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,M2+1:N3))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,M1,M2,N3,M2,N3,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S103(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K5
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S103)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,M2,N3,M2,N3,M1,N2,X52,S103,-1.000)
       deallocate(S103)
C
       allocate(D1(N2+1:N3,M1+1:N2,M2+1:N3,M2+1:N3))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,M1,N2,M2,N3,M2,N3,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S104(M1+1:N2,M1+1:N2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K8
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S104)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N2,M2,N3,M2,N3,M1,N2,X53,S104,-1.000)
       deallocate(S104)
C
       allocate(D1(N2+1:N3,N0+1:M1,N2+1:M2,M2+1:N3))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,M1,N2,M2,M2,N3,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S105(M1+1:N2,N0+1:M1,N2+1:M2,M2+1:N3))
       I1=K6*K0*K5
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S105)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,N2,M2,M2,N3,M1,N2,X54,S105,-1.000)
       deallocate(S105)
C
       allocate(D1(N2+1:N3,M1+1:N2,N2+1:M2,M2+1:N3))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,M1,N2,N2,M2,M2,N3,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S106(M1+1:N2,M1+1:N2,N2+1:M2,M2+1:N3))
       I1=K6*K0*K8
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S106)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N2,N2,M2,M2,N3,M1,N2,X55,S106,-1.000)
       deallocate(S106)
C
       allocate(D1(N2+1:N3,N0+1:M1,N2+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,M1,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S107(M1+1:N2,N0+1:M1,N2+1:M2,N2+1:M2))
       I1=K0*K0*K5
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S107)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,N2,M2,N2,M2,M1,N2,X56,S107,-1.000)
       deallocate(S107)
C
       allocate(D1(N2+1:N3,M1+1:N2,N2+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,M1,N2,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S108(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K8
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S108)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N2,N2,M2,N2,M2,M1,N2,X57,S108,-1.000)
       deallocate(S108)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,M2+1:N3))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,M2,N3,N2,M2,M2,N3,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S109(N2+1:M2,M2+1:N3,N2+1:M2,M2+1:N3))
       I1=K6*K0*K6
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S109)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X58(M2+1:N3,N2+1:M2,M2+1:N3,N2+1:M2))
       X58=0.0d0
       call
     & sum4123(M2,N3,N2,M2,M2,N3,N2,M2,X58,S109,-1.000)
       deallocate(S109)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,M2+1:N3))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,M2,N2,M2,M2,N3,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S110(N2+1:M2,N2+1:M2,N2+1:M2,M2+1:N3))
       I1=K6*K0*K0
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S110)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X59(N2+1:M2,N2+1:M2,M2+1:N3,N2+1:M2))
       X59=0.0d0
       call
     & sum4123(N2,M2,N2,M2,M2,N3,N2,M2,X59,S110,-1.000)
       deallocate(S110)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,M2+1:N3))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,M2,N3,M2,N3,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q25(M2+1:N3,M2+1:N3))
       I1=K6*K6
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q25)
       deallocate(D1)
       deallocate(B2)
C
       X23=X23-Q25
       deallocate(Q25)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M2+1:N3))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,M2,M2,N3,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q26(N2+1:M2,M2+1:N3))
       I1=K6*K0
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q26)
       deallocate(D1)
       deallocate(B2)
C
       X24=X24-Q26
       deallocate(Q26)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,M2,N3,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S111(M2+1:N3,M2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K6
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S111)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,N2,M2,M2,N3,N2,M2,X58,S111, 1.000)
       deallocate(S111)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,M2,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S112(M2+1:N3,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K0
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S112)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,M2,N2,M2,M2,N3,N2,M2,X59,S112, 1.000)
       deallocate(S112)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q27(N2+1:M2,N2+1:M2))
       I1=K0*K0
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q27)
       deallocate(D1)
       deallocate(B2)
C
       X25=X25-Q27
       deallocate(Q27)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder4231(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,M1,N2,VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2A,D2)
       allocate(S113(M2+1:N3,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K6
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S113)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N2,M2,N3,M1,N2,N0,M1,X8,S113, 1.000)
       deallocate(S113)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,M2+1:N3))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,N3,M2,N3,VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2A,D2)
       allocate(S114(M2+1:N3,N0+1:M1,N2+1:N3,M2+1:N3))
       I1=K6*K4
       I2=K5*K6
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S114)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,N3,M2,N3,M2,N3,N0,M1,X9,S114, 1.000)
       deallocate(S114)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,N3,N2,M2,VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2A,D2)
       allocate(S115(M2+1:N3,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K6
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S115)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,N3,N2,M2,M2,N3,N0,M1,X10,S115, 1.000)
       deallocate(S115)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2A,D2)
       allocate(S116(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K6
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S116)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X78(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:M1))
       X78=0.0d0
       call
     & sum3412(N0,M1,M2,N3,M2,N3,N0,M1,X78,S116, 1.000)
       deallocate(S116)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2A,D2)
       allocate(S117(M2+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K6
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S117)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X79(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:M1))
       X79=0.0d0
       call
     & sum3412(M1,N1,M2,N3,M2,N3,N0,M1,X79,S117, 1.000)
       deallocate(S117)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2A,D2)
       allocate(S118(M2+1:N3,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K6
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S118)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X80(N0+1:M1,N1+1:M2,M2+1:N3,N0+1:M1))
       X80=0.0d0
       call
     & sum3412(N0,M1,N1,M2,M2,N3,N0,M1,X80,S118, 1.000)
       deallocate(S118)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2A,D2)
       allocate(S119(M2+1:N3,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K5*K6
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S119)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X81(M1+1:N1,N1+1:M2,M2+1:N3,N0+1:M1))
       X81=0.0d0
       call
     & sum3412(M1,N1,N1,M2,M2,N3,N0,M1,X81,S119, 1.000)
       deallocate(S119)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,M1,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,M1,t2A,D2)
       allocate(Q28(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K3*K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,Q28)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N0,M1,N0,M1,X17,Q28, 0.500)
       deallocate(Q28)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,M1,N1,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,M1,t2A,D2)
       allocate(Q29(N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5
       I3=K3*K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,Q29)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M1,N1,N0,M1,X18,Q29, 0.500)
       deallocate(Q29)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,M2,N3,t2A,D2)
       allocate(Q30(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K3*K1*K1
       call DMATMAT(I1,I2,I3,D1,D2,Q30)
       deallocate(D1)
       deallocate(D2)
C
       allocate(B1(M2+1:N3,M2+1:N3))
       call reorder21(M2,N3,M2,N3,
     & M2,N3,M2,N3,Q30,B1)
       allocate(F2(M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N0+1:M1))
       call reorder312456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z216(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K8*K8*K0*K6
       I3=K6
       call DMATMAT(I1,I2,I3,B1,F2,Z216)
       deallocate(B1)
       deallocate(F2)
C
       call
     & sum124563(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z216, 0.500)
       deallocate(Z216)
       deallocate(Q30)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,M2,N3,t2A,D2)
       allocate(Q31(M2+1:N3,N1+1:M2))
       I1=K9
       I2=K6
       I3=K3*K1*K1
       call DMATMAT(I1,I2,I3,D1,D2,Q31)
       deallocate(D1)
       deallocate(D2)
C
       allocate(B1(N1+1:M2,M2+1:N3))
       call reorder21(M2,N3,N1,M2,
     & N1,M2,M2,N3,Q31,B1)
       allocate(F2(N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N0+1:M1))
       call reorder312456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N1,M2,M2,N3,N2,M2,M1,N2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z217(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K8*K8*K0*K6
       I3=K9
       call DMATMAT(I1,I2,I3,B1,F2,Z217)
       deallocate(B1)
       deallocate(F2)
C
       call
     & sum124563(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z217, 0.500)
       deallocate(Z217)
       deallocate(Q31)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2A,D2)
       allocate(S120(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K6
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S120)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X82(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:M1))
       X82=0.0d0
       call
     & sum3412(N0,M1,M2,N3,M2,N3,N0,M1,X82,S120, 1.000)
       deallocate(S120)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2A,D2)
       allocate(S121(M2+1:N3,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K6
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S121)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X83(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:M1))
       X83=0.0d0
       call
     & sum3412(M1,N2,M2,N3,M2,N3,N0,M1,X83,S121, 1.000)
       deallocate(S121)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2A,D2)
       allocate(S122(M2+1:N3,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K6
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S122)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X84(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:M1))
       X84=0.0d0
       call
     & sum3412(N0,M1,N2,M2,M2,N3,N0,M1,X84,S122, 1.000)
       deallocate(S122)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2A,D2)
       allocate(S123(M2+1:N3,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K6
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S123)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X85(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:M1))
       X85=0.0d0
       call
     & sum3412(M1,N2,N2,M2,M2,N3,N0,M1,X85,S123, 1.000)
       deallocate(S123)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,FAHP,B1)
       allocate(D2(N1+1:N3,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M2,N3,M1,N2,N0,M1,t2B,D2)
       allocate(S124(M2+1:N3,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K6
       I3=K3
       call DMATMAT(I1,I2,I3,B1,D2,S124)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,M2,N3,M1,N2,N0,M1,X1,S124, 1.000)
       deallocate(S124)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,FAHP,B1)
       allocate(D2(N1+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,M2,M1,N2,N0,M1,t2B,D2)
       allocate(S125(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K3
       call DMATMAT(I1,I2,I3,B1,D2,S125)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X2,S125, 1.000)
       deallocate(S125)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder3142(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N0,M1,VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S126(N2+1:M2,M1+1:N2,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K8*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S126)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X86(N0+1:N1,N2+1:M2,M1+1:N2,N0+1:M1))
       X86=0.0d0
       call
     & sum2314(N0,N1,N2,M2,M1,N2,N0,M1,X86,S126, 1.000)
       deallocate(S126)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder3142(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N0,M1,VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S127(M2+1:N3,M1+1:N2,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K8*K6
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S127)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,M2,N3,M1,N2,N0,M1,X1,S127, 1.000)
       deallocate(S127)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,M2+1:N3))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,M2,N3,VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S128(N2+1:M2,M1+1:N2,N1+1:N3,M2+1:N3))
       I1=K6*K3
       I2=K8*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S128)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X87(N1+1:N3,N2+1:M2,M2+1:N3,M1+1:N2))
       X87=0.0d0
       call
     & sum2413(N1,N3,N2,M2,M2,N3,M1,N2,X87,S128, 1.000)
       deallocate(S128)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,M2+1:N3))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,M2,N3,VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S129(M2+1:N3,M1+1:N2,N1+1:N3,M2+1:N3))
       I1=K6*K3
       I2=K8*K6
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S129)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X88(N1+1:N3,M2+1:N3,M2+1:N3,M1+1:N2))
       X88=0.0d0
       call
     & sum2413(N1,N3,M2,N3,M2,N3,M1,N2,X88,S129, 1.000)
       deallocate(S129)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,M1,N2,VBHHPH,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,M2,N3,t2B,D2)
       allocate(S130(N2+1:M2,M2+1:N3,N1+1:N3,M1+1:N2))
       I1=K8*K3
       I2=K6*K0
       I3=K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,S130)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N1,N3,N2,M2,M2,N3,M1,N2,X4,S130, 1.000)
       deallocate(S130)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,M1,N2,VBHHPH,D1)
       allocate(D2(N0+1:N2,N0+1:N1,M2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,M2,N3,M2,N3,t2B,D2)
       allocate(S131(M2+1:N3,M2+1:N3,N1+1:N3,M1+1:N2))
       I1=K8*K3
       I2=K6*K6
       I3=K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,S131)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N1,N3,M2,N3,M2,N3,M1,N2,X3,S131, 1.000)
       deallocate(S131)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder4231(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,M1,N2,VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S132(N2+1:M2,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S132)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,N2,M2,M1,N2,M1,N2,X76,S132, 1.000)
       deallocate(S132)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,M1+1:N2))
       call reorder3241(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N0,N1,M1,N2,VBHHPH,D1)
       allocate(D2(N0+1:N2,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S133(M2+1:N3,N0+1:M1,N0+1:N1,M1+1:N2))
       I1=K8*K1
       I2=K5*K6
       I3=K3*K2
       call DMATMAT(I1,I2,I3,D1,D2,S133)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N1,M2,N3,M1,N2,N0,M1,X1,S133,-1.000)
       deallocate(S133)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder4231(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,M1,N2,VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S134(M2+1:N3,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K6
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S134)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,M2,N3,M1,N2,M1,N2,X75,S134, 1.000)
       deallocate(S134)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,M1+1:N2))
       call reorder3241(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N0,N1,M1,N2,VBHHPH,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S135(N2+1:M2,N0+1:M1,N0+1:N1,M1+1:N2))
       I1=K8*K1
       I2=K5*K0
       I3=K3*K2
       call DMATMAT(I1,I2,I3,D1,D2,S135)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N1,N2,M2,M1,N2,N0,M1,X2,S135,-1.000)
       deallocate(S135)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,M2+1:N3))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N2,N3,N1,N3,M2,N3,VBHPPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S136(M2+1:N3,M1+1:N2,N1+1:N3,M2+1:N3))
       I1=K6*K3
       I2=K8*K6
       I3=K4*K1
       call DMATMAT(I1,I2,I3,D1,D2,S136)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N1,N3,M2,N3,M2,N3,M1,N2,X88,S136, 1.000)
       deallocate(S136)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,M2+1:N3))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,N3,M2,N3,VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S137(N2+1:M2,M1+1:N2,N2+1:N3,M2+1:N3))
       I1=K6*K4
       I2=K8*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S137)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X89(N2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       X89=0.0d0
       call
     & sum3412(N2,N3,M2,N3,N2,M2,M1,N2,X89,S137, 1.000)
       deallocate(S137)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N1,M2+1:N3))
       call reorder1243(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N1,N3,N0,N1,M2,N3,VBHPPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,M1+1:N2,N0+1:M1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,M1,N2,N0,M1,t2B,D2)
       allocate(S138(M1+1:N2,N0+1:M1,N0+1:N1,M2+1:N3))
       I1=K6*K1
       I2=K5*K8
       I3=K3*K4
       call DMATMAT(I1,I2,I3,D1,D2,S138)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,N1,M2,N3,M1,N2,N0,M1,X1,S138, 1.000)
       deallocate(S138)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:M2))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N2,N3,N1,N3,N2,M2,VBHPPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S139(M2+1:N3,M1+1:N2,N1+1:N3,N2+1:M2))
       I1=K0*K3
       I2=K8*K6
       I3=K4*K1
       call DMATMAT(I1,I2,I3,D1,D2,S139)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N1,N3,N2,M2,M2,N3,M1,N2,X87,S139, 1.000)
       deallocate(S139)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,N3,N2,M2,VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S140(M2+1:N3,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K6
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S140)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,M2,N3,N2,M2,M1,N2,X89,S140,-1.000)
       deallocate(S140)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N1,N2+1:M2))
       call reorder1243(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N1,N3,N0,N1,N2,M2,VBHPPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,M1+1:N2,N0+1:M1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,M1,N2,N0,M1,t2B,D2)
       allocate(S141(M1+1:N2,N0+1:M1,N0+1:N1,N2+1:M2))
       I1=K0*K1
       I2=K5*K8
       I3=K3*K4
       call DMATMAT(I1,I2,I3,D1,D2,S141)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,N1,N2,M2,M1,N2,N0,M1,X2,S141, 1.000)
       deallocate(S141)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,FBHP,B1)
       allocate(D2(N2+1:N3,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,M2,N3,M1,N2,N0,M1,t2B,D2)
       allocate(S142(M2+1:N3,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K6
       I3=K4
       call DMATMAT(I1,I2,I3,B1,D2,S142)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,N0,M1,X8,S142, 1.000)
       deallocate(S142)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,FBHP,B1)
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,M1,N2,M1,N2,t2C,D2)
       allocate(S143(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K4
       call DMATMAT(I1,I2,I3,B1,D2,S143)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X6,S143,-1.000)
       deallocate(S143)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,FBHP,B1)
       allocate(D2(N2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,M2,N3,M1,N2,M1,N2,t2C,D2)
       allocate(S144(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K4
       call DMATMAT(I1,I2,I3,B1,D2,S144)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X5,S144,-1.000)
       deallocate(S144)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder3142(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N0,M1,VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S145(N2+1:M2,M1+1:N2,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K8*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S145)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N2,M2,M1,N2,N0,M1,X86,S145, 1.000)
       deallocate(S145)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder3142(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N0,M1,VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S146(M2+1:N3,M1+1:N2,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K8*K6
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S146)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X90(N0+1:N1,M2+1:N3,M1+1:N2,N0+1:M1))
       X90=0.0d0
       call
     & sum2314(N0,N1,M2,N3,M1,N2,N0,M1,X90,S146, 1.000)
       deallocate(S146)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:M1))
       call reorder4132(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N2,N3,N0,N2,N0,M1,VBHHHP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S147(M2+1:N3,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K6
       I3=K4*K1
       call DMATMAT(I1,I2,I3,D1,D2,S147)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,M2,N3,M1,N2,N0,M1,X8,S147,-1.000)
       deallocate(S147)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N0,M1,VBHHHP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,M2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,M2,N3,M2,N3,t2B,D2)
       allocate(S148(M2+1:N3,M2+1:N3,N2+1:N3,N0+1:M1))
       I1=K5*K4
       I2=K6*K6
       I3=K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,S148)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N2,N3,M2,N3,M2,N3,N0,M1,X9,S148, 1.000)
       deallocate(S148)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N0,M1,VBHHHP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,M2,N3,t2B,D2)
       allocate(S149(N2+1:M2,M2+1:N3,N2+1:N3,N0+1:M1))
       I1=K5*K4
       I2=K6*K0
       I3=K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,S149)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N2,N3,N2,M2,M2,N3,N0,M1,X10,S149, 1.000)
       deallocate(S149)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,M2+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,N3,N1,N3,M2,N3,VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S150(N2+1:M2,M1+1:N2,N1+1:N3,M2+1:N3))
       I1=K6*K3
       I2=K8*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S150)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N1,N3,N2,M2,M2,N3,M1,N2,X87,S150,-1.000)
       deallocate(S150)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,M2+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,N3,N1,N3,M2,N3,VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S151(M2+1:N3,M1+1:N2,N1+1:N3,M2+1:N3))
       I1=K6*K3
       I2=K8*K6
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S151)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N1,N3,M2,N3,M2,N3,M1,N2,X88,S151,-1.000)
       deallocate(S151)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,M2+1:N3))
       call reorder1234(N2,N3,N1,N3,N0,N2,N1,N3,
     & N2,N3,N1,N3,N0,N2,M2,N3,VBPHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,M1+1:N2,N0+1:M1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,M1,N2,N0,M1,t2B,D2)
       allocate(S152(M1+1:N2,N0+1:M1,N0+1:N2,M2+1:N3))
       I1=K6*K2
       I2=K5*K8
       I3=K3*K4
       call DMATMAT(I1,I2,I3,D1,D2,S152)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,N2,M2,N3,M1,N2,N0,M1,X8,S152, 1.000)
       deallocate(S152)
C
       allocate(D1(N0+1:N2,N1+1:N3,N2+1:N3,M2+1:N3))
       call reorder3214(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N1,N3,N2,N3,M2,N3,VBPHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S153(M2+1:N3,N0+1:M1,N2+1:N3,M2+1:N3))
       I1=K6*K4
       I2=K5*K6
       I3=K3*K2
       call DMATMAT(I1,I2,I3,D1,D2,S153)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,M2,N3,M2,N3,N0,M1,X9,S153,-1.000)
       deallocate(S153)
C
       allocate(D1(N0+1:N2,N1+1:N3,N2+1:N3,M2+1:N3))
       call reorder3214(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N1,N3,N2,N3,M2,N3,VBPHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S154(N2+1:M2,N0+1:M1,N2+1:N3,M2+1:N3))
       I1=K6*K4
       I2=K5*K0
       I3=K3*K2
       call DMATMAT(I1,I2,I3,D1,D2,S154)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,N2,M2,M2,N3,N0,M1,X10,S154,-1.000)
       deallocate(S154)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,M1,N2,VCHHHP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,N2,M2,t2C,D2)
       allocate(S155(M2+1:N3,N2+1:M2,N2+1:N3,M1+1:N2))
       I1=K8*K4
       I2=K0*K6
       I3=K2*K2
       call DMATMAT(I1,I2,I3,D1,D2,S155)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N2,N3,M2,N3,N2,M2,M1,N2,X7,S155, 0.500)
       deallocate(S155)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder3142(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,M1,N2,VCHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S156(N2+1:M2,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S156)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,N2,M2,M1,N2,M1,N2,X76,S156, 1.000)
       deallocate(S156)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder3142(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,M1,N2,VCHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S157(M2+1:N3,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K6
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S157)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,M2,N3,M1,N2,M1,N2,X75,S157, 1.000)
       deallocate(S157)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder4132(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,M1,N2,VCHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S158(M2+1:N3,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K6
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S158)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N2,M2,N3,M1,N2,N0,M1,X8,S158,-1.000)
       deallocate(S158)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,M2+1:N3))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,M2,N3,VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S159(N2+1:M2,M1+1:N2,N2+1:N3,M2+1:N3))
       I1=K6*K4
       I2=K8*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S159)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,N3,M2,N3,N2,M2,M1,N2,X89,S159,-1.000)
       deallocate(S159)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,M2,VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S160(M2+1:N3,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K6
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S160)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,M2,N3,N2,M2,M1,N2,X89,S160, 1.000)
       deallocate(S160)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,M2+1:N3))
       call reorder1243(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N2,N3,N0,N2,M2,N3,VCHPPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,M1,N2,M1,N2,t2C,D2)
       allocate(S161(M1+1:N2,M1+1:N2,N0+1:N2,M2+1:N3))
       I1=K6*K2
       I2=K8*K8
       I3=K4*K4
       call DMATMAT(I1,I2,I3,D1,D2,S161)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,N2,M2,N3,M1,N2,M1,N2,X5,S161, 0.500)
       deallocate(S161)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,M2+1:N3))
       call reorder4213(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,M2,N3,VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S162(M2+1:N3,N0+1:M1,N2+1:N3,M2+1:N3))
       I1=K6*K4
       I2=K5*K6
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S162)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,N3,M2,N3,M2,N3,N0,M1,X9,S162, 1.000)
       deallocate(S162)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:M2))
       call reorder1243(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N2,N3,N0,N2,N2,M2,VCHPPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,M1,N2,M1,N2,t2C,D2)
       allocate(S163(M1+1:N2,M1+1:N2,N0+1:N2,N2+1:M2))
       I1=K0*K2
       I2=K8*K8
       I3=K4*K4
       call DMATMAT(I1,I2,I3,D1,D2,S163)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,N2,N2,M2,M1,N2,M1,N2,X6,S163, 0.500)
       deallocate(S163)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder4213(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,M2,VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S164(M2+1:N3,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K6
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S164)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,N3,N2,M2,M2,N3,N0,M1,X10,S164, 1.000)
       deallocate(S164)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,N0,M1,M2,N3,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder562134(N2,M2,M2,N3,M2,N3,M1,N2,N0,M1,N0,M1,
     & N0,M1,N0,M1,M2,N3,N2,M2,M2,N3,M1,N2,t3B2,F2)
       allocate(S165(N2+1:M2,M2+1:N3,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K6*K0
       I3=K6*K5*K5
       call DMATMAT(I1,I2,I3,D1,F2,S165)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,M2,N3,M1,N2,X87,S165,-0.500)
       deallocate(S165)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M1,N1,M2,N3,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder562134(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,N2,M2,M2,N3,M1,N2,t3B3,F2)
       allocate(S166(N2+1:M2,M2+1:N3,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K6*K0
       I3=K6*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,S166)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,M2,N3,M1,N2,X87,S166,-1.000)
       deallocate(S166)
C
       allocate(D1(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M1,N1,M2,N3,N1,N3,VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder562134(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N1,M1,N1,M2,N3,N2,M2,M2,N3,M1,N2,t3B3,F2)
       allocate(S167(N2+1:M2,M2+1:N3,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K6*K0
       I3=K6*K7*K7
       call DMATMAT(I1,I2,I3,D1,F2,S167)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,M2,N3,M1,N2,X87,S167,-0.500)
       deallocate(S167)
C
       allocate(D1(N0+1:M1,N0+1:M1,N1+1:M2,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,N0,M1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder563124(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & N0,M1,N0,M1,N1,M2,N2,M2,M2,N3,M1,N2,t3B4,F2)
       allocate(S168(N2+1:M2,M2+1:N3,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K6*K0
       I3=K9*K5*K5
       call DMATMAT(I1,I2,I3,D1,F2,S168)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,M2,N3,M1,N2,X87,S168, 0.500)
       deallocate(S168)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M1,N1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder563124(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,N1,M2,N2,M2,M2,N3,M1,N2,t3B1,F2)
       allocate(S169(N2+1:M2,M2+1:N3,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K6*K0
       I3=K9*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,S169)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,M2,N3,M1,N2,X87,S169, 1.000)
       deallocate(S169)
C
       allocate(D1(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M1,N1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder563124(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M1,N1,N1,M2,N2,M2,M2,N3,M1,N2,t3B1,F2)
       allocate(S170(N2+1:M2,M2+1:N3,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K6*K0
       I3=K9*K7*K7
       call DMATMAT(I1,I2,I3,D1,F2,S170)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,M2,N3,M1,N2,X87,S170, 0.500)
       deallocate(S170)
C
       allocate(D1(N0+1:M1,N0+1:M1,N1+1:M2,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,N0,M1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N2))
       call reorder563124(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & N0,M1,N0,M1,N1,M2,M2,N3,M2,N3,M1,N2,t3B4,F2)
       allocate(S171(M2+1:N3,M2+1:N3,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K6*K6
       I3=K9*K5*K5
       call DMATMAT(I1,I2,I3,D1,F2,S171)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,M2,N3,M1,N2,X88,S171, 0.500)
       deallocate(S171)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M1,N1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N2))
       call reorder563124(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,N1,M2,M2,N3,M2,N3,M1,N2,t3B1,F2)
       allocate(S172(M2+1:N3,M2+1:N3,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K6*K6
       I3=K9*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,S172)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,M2,N3,M1,N2,X88,S172, 1.000)
       deallocate(S172)
C
       allocate(D1(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M1,N1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N2))
       call reorder563124(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M1,N1,N1,M2,M2,N3,M2,N3,M1,N2,t3B1,F2)
       allocate(S173(M2+1:N3,M2+1:N3,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K6*K6
       I3=K9*K7*K7
       call DMATMAT(I1,I2,I3,D1,F2,S173)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,M2,N3,M1,N2,X88,S173, 0.500)
       deallocate(S173)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder523146(N2,M2,M2,N3,M2,N3,M1,N2,N0,M1,N0,M1,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,N0,M1,t3B2,F2)
       allocate(S174(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K6*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S174)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X86,S174, 0.500)
       deallocate(S174)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,M2,N3,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder623145(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,M1,N2,N0,M1,t3B3,F2)
       allocate(S175(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K6*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,S175)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X86,S175,-0.500)
       deallocate(S175)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder523146(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & N0,M1,M2,N3,N1,M2,N2,M2,M1,N2,N0,M1,t3B4,F2)
       allocate(S176(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K9*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S176)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X86,S176, 1.000)
       deallocate(S176)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder623145(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,N2,M2,M1,N2,N0,M1,t3B1,F2)
       allocate(S177(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K9*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,S177)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X86,S177,-1.000)
       deallocate(S177)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,N1,M2,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N1+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder523146(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & N0,M1,N1,M2,N1,M2,N2,M2,M1,N2,N0,M1,t3B4,F2)
       allocate(S178(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K9*K9*K5
       call DMATMAT(I1,I2,I3,D1,F2,S178)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X86,S178, 0.500)
       deallocate(S178)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,N1,M2,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder623145(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,N1,M2,N1,M2,N2,M2,M1,N2,N0,M1,t3B1,F2)
       allocate(S179(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K9*K9*K7
       call DMATMAT(I1,I2,I3,D1,F2,S179)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X86,S179,-0.500)
       deallocate(S179)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S180(M2+1:N3,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K6
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S180)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,M2,N3,M1,N2,X11,S180, 1.000)
       deallocate(S180)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S181(M2+1:N3,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K8*K6
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S181)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,M2,N3,M2,N3,M1,N2,X12,S181, 1.000)
       deallocate(S181)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S182(M2+1:N3,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K8*K6
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S182)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N1,M2,M2,N3,M1,N2,X13,S182, 1.000)
       deallocate(S182)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S183(M2+1:N3,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K8*K6
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S183)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,N1,M2,M2,N3,M1,N2,X14,S183, 1.000)
       deallocate(S183)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder523146(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & N0,M1,M2,N3,N1,M2,M2,N3,M1,N2,N0,M1,t3B4,F2)
       allocate(S184(M2+1:N3,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K6
       I3=K9*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S184)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,M1,N2,N0,M1,X90,S184, 1.000)
       deallocate(S184)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder623145(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,M2,N3,M1,N2,N0,M1,t3B1,F2)
       allocate(S185(M2+1:N3,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K6
       I3=K9*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,S185)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,M1,N2,N0,M1,X90,S185,-1.000)
       deallocate(S185)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,N1,M2,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N1+1:M2,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder523146(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & N0,M1,N1,M2,N1,M2,M2,N3,M1,N2,N0,M1,t3B4,F2)
       allocate(S186(M2+1:N3,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K6
       I3=K9*K9*K5
       call DMATMAT(I1,I2,I3,D1,F2,S186)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,M1,N2,N0,M1,X90,S186, 0.500)
       deallocate(S186)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,N1,M2,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder623145(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,N1,M2,N1,M2,M2,N3,M1,N2,N0,M1,t3B1,F2)
       allocate(S187(M2+1:N3,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K6
       I3=K9*K9*K7
       call DMATMAT(I1,I2,I3,D1,F2,S187)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,M1,N2,N0,M1,X90,S187,-0.500)
       deallocate(S187)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S188(N2+1:M2,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K8*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S188)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N1,M2,N2,M2,M1,N2,X15,S188, 1.000)
       deallocate(S188)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S189(N2+1:M2,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K8*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S189)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,N1,M2,N2,M2,M1,N2,X16,S189, 1.000)
       deallocate(S189)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N0,M1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N0,M1,M2,N3,N2,M2,M2,N3,M1,N2,t3C1,F2)
       allocate(S190(N2+1:M2,M2+1:N3,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K6*K0
       I3=K6*K5*K5
       call DMATMAT(I1,I2,I3,D1,F2,S190)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,M2,N3,M1,N2,X87,S190, 1.000)
       deallocate(S190)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N2,M2,M2,N3,M1,N2,t3C1,F2)
       allocate(S191(N2+1:M2,M2+1:N3,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K6*K0
       I3=K6*K5*K8
       call DMATMAT(I1,I2,I3,D1,F2,S191)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,M2,N3,M1,N2,X87,S191, 1.000)
       deallocate(S191)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N2,M2,M2,N3,M1,N2,t3C1,F2)
       allocate(S192(N2+1:M2,M2+1:N3,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K6*K0
       I3=K6*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,S192)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,M2,N3,M1,N2,X87,S192, 1.000)
       deallocate(S192)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N2,M2,M2,N3,M1,N2,t3C1,F2)
       allocate(S193(N2+1:M2,M2+1:N3,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K6*K0
       I3=K6*K7*K8
       call DMATMAT(I1,I2,I3,D1,F2,S193)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,M2,N3,M1,N2,X87,S193, 1.000)
       deallocate(S193)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N0,M1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N0,M1,N2,M2,N2,M2,M2,N3,M1,N2,t3C1,F2)
       allocate(S194(N2+1:M2,M2+1:N3,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K6*K0
       I3=K0*K5*K5
       call DMATMAT(I1,I2,I3,D1,F2,S194)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,M2,N3,M1,N2,X87,S194, 1.000)
       deallocate(S194)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,N2,M2,M2,N3,M1,N2,t3C1,F2)
       allocate(S195(N2+1:M2,M2+1:N3,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K6*K0
       I3=K0*K5*K8
       call DMATMAT(I1,I2,I3,D1,F2,S195)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,M2,N3,M1,N2,X87,S195, 1.000)
       deallocate(S195)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N1,N2,M2,N2,M2,M2,N3,M1,N2,t3C1,F2)
       allocate(S196(N2+1:M2,M2+1:N3,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K6*K0
       I3=K0*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,S196)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,M2,N3,M1,N2,X87,S196, 1.000)
       deallocate(S196)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,N2,M2,M2,N3,M1,N2,t3C1,F2)
       allocate(S197(N2+1:M2,M2+1:N3,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K6*K0
       I3=K0*K7*K8
       call DMATMAT(I1,I2,I3,D1,F2,S197)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,M2,N3,M1,N2,X87,S197, 1.000)
       deallocate(S197)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N0,M1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2))
       call reorder462135(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N0,M1,N2,M2,M2,N3,M2,N3,M1,N2,t3C1,F2)
       allocate(S198(M2+1:N3,M2+1:N3,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K6*K6
       I3=K0*K5*K5
       call DMATMAT(I1,I2,I3,D1,F2,S198)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,M2,N3,M1,N2,X88,S198,-1.000)
       deallocate(S198)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2))
       call reorder462135(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,M2,N3,M2,N3,M1,N2,t3C1,F2)
       allocate(S199(M2+1:N3,M2+1:N3,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K6*K6
       I3=K0*K5*K8
       call DMATMAT(I1,I2,I3,D1,F2,S199)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,M2,N3,M1,N2,X88,S199,-1.000)
       deallocate(S199)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2))
       call reorder462135(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N1,N2,M2,M2,N3,M2,N3,M1,N2,t3C1,F2)
       allocate(S200(M2+1:N3,M2+1:N3,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K6*K6
       I3=K0*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,S200)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,M2,N3,M1,N2,X88,S200,-1.000)
       deallocate(S200)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2))
       call reorder462135(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,M2,N3,M2,N3,M1,N2,t3C1,F2)
       allocate(S201(M2+1:N3,M2+1:N3,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K6*K6
       I3=K0*K7*K8
       call DMATMAT(I1,I2,I3,D1,F2,S201)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,M2,N3,M1,N2,X88,S201,-1.000)
       deallocate(S201)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N0,M1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,t3C1,F2)
       allocate(S202(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K5*K5
       call DMATMAT(I1,I2,I3,D1,F2,S202)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X89,S202,-1.000)
       deallocate(S202)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,t3C1,F2)
       allocate(S203(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K5*K8
       call DMATMAT(I1,I2,I3,D1,F2,S203)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X89,S203,-1.000)
       deallocate(S203)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,M2,N3,N2,M2,M1,N2,t3C1,F2)
       allocate(S204(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,S204)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X89,S204,-1.000)
       deallocate(S204)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,M2,N3,N2,M2,M1,N2,t3C1,F2)
       allocate(S205(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K7*K8
       call DMATMAT(I1,I2,I3,D1,F2,S205)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X89,S205,-1.000)
       deallocate(S205)
C
       allocate(D1(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N0,M1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N0,M1,N1,M2,M2,N3,N2,M2,M1,N2,t3C1,F2)
       allocate(S206(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K9*K5*K5
       call DMATMAT(I1,I2,I3,D1,F2,S206)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X89,S206,-1.000)
       deallocate(S206)
C
       allocate(D1(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,N1,M2,M2,N3,N2,M2,M1,N2,t3C1,F2)
       allocate(S207(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K9*K5*K8
       call DMATMAT(I1,I2,I3,D1,F2,S207)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X89,S207,-1.000)
       deallocate(S207)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N1,N1,M2,M2,N3,N2,M2,M1,N2,t3C1,F2)
       allocate(S208(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K9*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,S208)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X89,S208,-1.000)
       deallocate(S208)
C
       allocate(D1(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,N1,M2,M2,N3,N2,M2,M1,N2,t3C1,F2)
       allocate(S209(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K9*K7*K8
       call DMATMAT(I1,I2,I3,D1,F2,S209)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X89,S209,-1.000)
       deallocate(S209)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,M1,N0,M1,VBHHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,M1+1:N2,N0+1:M1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,M1,N2,N0,M1,t2B,D2)
       allocate(S210(M1+1:N2,N0+1:M1,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K5*K8
       I3=K3*K4
       call DMATMAT(I1,I2,I3,D1,D2,S210)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N0,M1,M1,N2,N0,M1,X30,S210, 1.000)
       deallocate(S210)
C
       allocate(D1(N2+1:N3,N1+1:N3,M1+1:N2,N0+1:M1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,M1,N2,N0,M1,VBHHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,M1+1:N2,N0+1:M1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,M1,N2,N0,M1,t2B,D2)
       allocate(S211(M1+1:N2,N0+1:M1,M1+1:N2,N0+1:M1))
       I1=K5*K8
       I2=K5*K8
       I3=K3*K4
       call DMATMAT(I1,I2,I3,D1,D2,S211)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,N0,M1,M1,N2,N0,M1,X31,S211, 1.000)
       deallocate(S211)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,M1,M1,N1,VBHHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,M1+1:N2,N0+1:M1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,M1,N2,N0,M1,t2B,D2)
       allocate(S212(M1+1:N2,N0+1:M1,N0+1:M1,M1+1:N1))
       I1=K7*K5
       I2=K5*K8
       I3=K3*K4
       call DMATMAT(I1,I2,I3,D1,D2,S212)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M1,N1,M1,N2,N0,M1,X32,S212, 1.000)
       deallocate(S212)
C
       allocate(D1(N2+1:N3,N1+1:N3,M1+1:N2,M1+1:N1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,M1,N2,M1,N1,VBHHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,M1+1:N2,N0+1:M1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,M1,N2,N0,M1,t2B,D2)
       allocate(S213(M1+1:N2,N0+1:M1,M1+1:N2,M1+1:N1))
       I1=K7*K8
       I2=K5*K8
       I3=K3*K4
       call DMATMAT(I1,I2,I3,D1,D2,S213)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,M1,N1,M1,N2,N0,M1,X33,S213, 1.000)
       deallocate(S213)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(S214(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K6*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S214)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X6,S214,-1.000)
       deallocate(S214)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(S215(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K6*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,S215)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X6,S215,-1.000)
       deallocate(S215)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(S216(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K6*K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,S216)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X6,S216,-1.000)
       deallocate(S216)
C
       allocate(D1(M1+1:N1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,N2,M2,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(S217(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K6*K0*K7
       call DMATMAT(I1,I2,I3,D1,F2,S217)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X6,S217,-1.000)
       deallocate(S217)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(S218(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K9*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S218)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X6,S218,-1.000)
       deallocate(S218)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,N1,M2,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(S219(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K9*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,S219)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X6,S219,-1.000)
       deallocate(S219)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(S220(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K9*K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,S220)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X6,S220,-1.000)
       deallocate(S220)
C
       allocate(D1(M1+1:N1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,N2,M2,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,N1,M2,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(S221(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K9*K0*K7
       call DMATMAT(I1,I2,I3,D1,F2,S221)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X6,S221,-1.000)
       deallocate(S221)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder3241(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S222(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K6
       I3=K3*K2
       call DMATMAT(I1,I2,I3,D1,D2,S222)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,M2,N3,N0,M1,X60,S222,-1.000)
       deallocate(S222)
C
       allocate(D1(N0+1:N2,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder3241(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,M1,N1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S223(M2+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K6
       I3=K3*K2
       call DMATMAT(I1,I2,I3,D1,D2,S223)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,M2,N3,M2,N3,N0,M1,X61,S223,-1.000)
       deallocate(S223)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder3241(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N0,M1,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S224(M2+1:N3,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K6
       I3=K3*K2
       call DMATMAT(I1,I2,I3,D1,D2,S224)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N2,M2,M2,N3,N0,M1,X62,S224,-1.000)
       deallocate(S224)
C
       allocate(D1(N0+1:N2,N1+1:N3,M1+1:N1,N2+1:M2))
       call reorder3241(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,M1,N1,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S225(M2+1:N3,N0+1:M1,M1+1:N1,N2+1:M2))
       I1=K0*K7
       I2=K5*K6
       I3=K3*K2
       call DMATMAT(I1,I2,I3,D1,D2,S225)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,N2,M2,M2,N3,N0,M1,X63,S225,-1.000)
       deallocate(S225)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder623145(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,M2,N3,M1,N2,M1,N2,t3C1,F2)
       allocate(S226(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K6*K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,S226)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X5,S226, 1.000)
       deallocate(S226)
C
       allocate(D1(M1+1:N1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,N2,M2,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder623145(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,M2,N3,M2,N3,M1,N2,M1,N2,t3C1,F2)
       allocate(S227(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K6*K0*K7
       call DMATMAT(I1,I2,I3,D1,F2,S227)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X5,S227, 1.000)
       deallocate(S227)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder613245(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,M2,N3,M1,N2,M1,N2,t3C3,F2)
       allocate(S228(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K9*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S228)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X5,S228,-1.000)
       deallocate(S228)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder613245(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,N1,M2,M2,N3,M1,N2,M1,N2,t3C3,F2)
       allocate(S229(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K9*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,S229)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X5,S229,-1.000)
       deallocate(S229)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder623145(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,M2,N3,M1,N2,M1,N2,t3C1,F2)
       allocate(S230(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K9*K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,S230)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X5,S230, 1.000)
       deallocate(S230)
C
       allocate(D1(M1+1:N1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,N2,M2,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder623145(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,N1,M2,M2,N3,M1,N2,M1,N2,t3C1,F2)
       allocate(S231(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K9*K0*K7
       call DMATMAT(I1,I2,I3,D1,F2,S231)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X5,S231, 1.000)
       deallocate(S231)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder3241(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N0,M1,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S232(N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K0
       I3=K3*K2
       call DMATMAT(I1,I2,I3,D1,D2,S232)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N2,M2,N2,M2,N0,M1,X64,S232,-1.000)
       deallocate(S232)
C
       allocate(D1(N0+1:N2,N1+1:N3,M1+1:N1,N2+1:M2))
       call reorder3241(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,M1,N1,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S233(N2+1:M2,N0+1:M1,M1+1:N1,N2+1:M2))
       I1=K0*K7
       I2=K5*K0
       I3=K3*K2
       call DMATMAT(I1,I2,I3,D1,D2,S233)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,N2,M2,N2,M2,N0,M1,X65,S233,-1.000)
       deallocate(S233)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S234(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K6
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S234)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X26(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:M1))
       X26=0.0d0
       call
     & sum3412(N0,M1,M2,N3,M2,N3,N0,M1,X26,S234,-1.000)
       deallocate(S234)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,M1,X26,VAHPHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z26(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K6
       I3=K6*K5
       call DMATMAT(I1,I2,I3,X26,F2,Z26)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z26,-1.000)
       deallocate(Z26)
       deallocate(X26)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S235(M2+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K6
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S235)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X27(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:M1))
       X27=0.0d0
       call
     & sum3412(M1,N1,M2,N3,M2,N3,N0,M1,X27,S235,-1.000)
       deallocate(S235)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,M2,N3,M2,N3,N0,M1,X27,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z27(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K6
       I3=K6*K7
       call DMATMAT(I1,I2,I3,X27,F2,Z27)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z27,-1.000)
       deallocate(Z27)
       deallocate(X27)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S236(M2+1:N3,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K6
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S236)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X28(N0+1:M1,N1+1:M2,M2+1:N3,N0+1:M1))
       X28=0.0d0
       call
     & sum3412(N0,M1,N1,M2,M2,N3,N0,M1,X28,S236,-1.000)
       deallocate(S236)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,M1,N1,M2,M2,N3,N0,M1,X28,VAHPHP, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N1,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z28(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K6
       I3=K9*K5
       call DMATMAT(I1,I2,I3,X28,F2,Z28)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z28,-1.000)
       deallocate(Z28)
       deallocate(X28)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S237(M2+1:N3,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K5*K6
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S237)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X29(M1+1:N1,N1+1:M2,M2+1:N3,N0+1:M1))
       X29=0.0d0
       call
     & sum3412(M1,N1,N1,M2,M2,N3,N0,M1,X29,S237,-1.000)
       deallocate(S237)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,N1,M2,M2,N3,N0,M1,X29,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N1,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z29(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K6
       I3=K9*K7
       call DMATMAT(I1,I2,I3,X29,F2,Z29)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z29,-1.000)
       deallocate(Z29)
       deallocate(X29)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,M1,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,M1,t2B,D2)
       allocate(Q32(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K3*K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,Q32)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N0,M1,N0,M1,X17,Q32, 1.000)
       deallocate(Q32)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,M1,N1,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,M1,t2B,D2)
       allocate(Q33(N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5
       I3=K3*K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,Q33)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M1,N1,N0,M1,X18,Q33, 1.000)
       deallocate(Q33)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(S238(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K6*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S238)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X86,S238, 1.000)
       deallocate(S238)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(S239(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K6*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,S239)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X86,S239, 1.000)
       deallocate(S239)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N2,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(S240(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K6*K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,S240)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X86,S240, 1.000)
       deallocate(S240)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,N2,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(S241(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K6*K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,S241)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X86,S241, 1.000)
       deallocate(S241)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N2,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(S242(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K9*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S242)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X86,S242, 1.000)
       deallocate(S242)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N1,M2,N2,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(S243(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K9*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,S243)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X86,S243, 1.000)
       deallocate(S243)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N2,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(S244(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K9*K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,S244)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X86,S244, 1.000)
       deallocate(S244)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,N1,M2,N2,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(S245(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K9*K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,S245)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X86,S245, 1.000)
       deallocate(S245)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S246(M2+1:N3,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K6
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S246)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3412(M2,N3,M1,N2,N0,M1,M2,N3,
     & N0,M1,M2,N3,M2,N3,M1,N2,S246,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N2,M2,M2,N3,M1,N2,N0,M1,t3C1,F2)
       allocate(Z346(N2+1:M2,M2+1:N3,M1+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K5*K8*K6*K0
       I3=K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z346)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z346,-1.000)
       call
     & sum235614(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z346, 1.000)
       deallocate(Z346)
       deallocate(S246)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S247(M2+1:N3,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K6
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S247)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3412(M2,N3,M1,N2,M1,N2,M2,N3,
     & M1,N2,M2,N3,M2,N3,M1,N2,S247,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N2,M2,M2,N3,M1,N2,N0,M1,t3C1,F2)
       allocate(Z347(N2+1:M2,M2+1:N3,M1+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K5*K8*K6*K0
       I3=K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z347)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z347,-1.000)
       call
     & sum235614(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z347, 1.000)
       deallocate(Z347)
       deallocate(S247)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S248(M2+1:N3,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K8*K6
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S248)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder3412(M2,N3,M1,N2,N0,M1,N2,M2,
     & N0,M1,N2,M2,M2,N3,M1,N2,S248,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N2,M2,M2,N3,M1,N2,N0,M1,t3C1,F2)
       allocate(Z348(N2+1:M2,M2+1:N3,M1+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K5*K8*K6*K0
       I3=K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z348)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z348,-1.000)
       call
     & sum235614(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z348, 1.000)
       deallocate(Z348)
       deallocate(S248)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S249(M2+1:N3,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K8*K6
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S249)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder3412(M2,N3,M1,N2,M1,N2,N2,M2,
     & M1,N2,N2,M2,M2,N3,M1,N2,S249,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,N2,M2,M2,N3,M1,N2,N0,M1,t3C1,F2)
       allocate(Z349(N2+1:M2,M2+1:N3,M1+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K5*K8*K6*K0
       I3=K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z349)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z349,-1.000)
       call
     & sum235614(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z349, 1.000)
       deallocate(Z349)
       deallocate(S249)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder423156(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,M2,N3,M1,N2,N0,M1,t3C1,F2)
       allocate(S250(M2+1:N3,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K6
       I3=K6*K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,S250)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,M1,N2,N0,M1,X90,S250,-1.000)
       deallocate(S250)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder423156(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,M2,N3,M1,N2,N0,M1,t3C1,F2)
       allocate(S251(M2+1:N3,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K6
       I3=K6*K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,S251)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,M1,N2,N0,M1,X90,S251,-1.000)
       deallocate(S251)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder413256(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,M2,N3,M1,N2,N0,M1,t3C3,F2)
       allocate(S252(M2+1:N3,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K6
       I3=K9*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S252)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,M1,N2,N0,M1,X90,S252, 1.000)
       deallocate(S252)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder413256(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N1,M2,M2,N3,M1,N2,N0,M1,t3C3,F2)
       allocate(S253(M2+1:N3,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K6
       I3=K9*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,S253)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,M1,N2,N0,M1,X90,S253, 1.000)
       deallocate(S253)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder423156(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,M2,N3,M1,N2,N0,M1,t3C1,F2)
       allocate(S254(M2+1:N3,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K6
       I3=K9*K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,S254)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,M1,N2,N0,M1,X90,S254,-1.000)
       deallocate(S254)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder423156(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,N1,M2,M2,N3,M1,N2,N0,M1,t3C1,F2)
       allocate(S255(M2+1:N3,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K6
       I3=K9*K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,S255)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,M1,N2,N0,M1,X90,S255,-1.000)
       deallocate(S255)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S256(N2+1:M2,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K8*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S256)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,M1,N2,M2,
     & N0,M1,N2,M2,N2,M2,M1,N2,S256,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder421356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,M2,N3,M1,N2,N0,M1,t3C1,F2)
       allocate(Z356(M2+1:N3,M2+1:N3,M1+1:N2,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K8*K6*K6
       I3=K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z356)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum134625(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z356,-1.000)
       call
     & sum135624(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z356, 1.000)
       deallocate(Z356)
       deallocate(S256)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S257(N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K8*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S257)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,M1,N2,N2,M2,
     & M1,N2,N2,M2,N2,M2,M1,N2,S257,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder421356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,M2,N3,M1,N2,N0,M1,t3C1,F2)
       allocate(Z357(M2+1:N3,M2+1:N3,M1+1:N2,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K8*K6*K6
       I3=K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z357)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum134625(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z357,-1.000)
       call
     & sum135624(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z357, 1.000)
       deallocate(Z357)
       deallocate(S257)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder4132(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S258(M2+1:N3,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K6
       I3=K4*K1
       call DMATMAT(I1,I2,I3,D1,D2,S258)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,M2,N3,M1,N2,X66,S258, 1.000)
       deallocate(S258)
C
       allocate(D1(N0+1:N1,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder4132(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,M1,N2,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S259(M2+1:N3,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K6
       I3=K4*K1
       call DMATMAT(I1,I2,I3,D1,D2,S259)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,M2,N3,M2,N3,M1,N2,X67,S259, 1.000)
       deallocate(S259)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder4132(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N0,M1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S260(M2+1:N3,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K8*K6
       I3=K4*K1
       call DMATMAT(I1,I2,I3,D1,D2,S260)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N1,M2,M2,N3,M1,N2,X68,S260, 1.000)
       deallocate(S260)
C
       allocate(D1(N0+1:N1,N2+1:N3,M1+1:N2,N1+1:M2))
       call reorder4132(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,M1,N2,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S261(M2+1:N3,M1+1:N2,M1+1:N2,N1+1:M2))
       I1=K9*K8
       I2=K8*K6
       I3=K4*K1
       call DMATMAT(I1,I2,I3,D1,D2,S261)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,N1,M2,M2,N3,M1,N2,X69,S261, 1.000)
       deallocate(S261)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:M1))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,M1,VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,M1,N2,t2B,D2)
       allocate(Q34(M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8
       I3=K3*K4*K1
       call DMATMAT(I1,I2,I3,D1,D2,Q34)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N0,M1,M1,N2,X70,Q34, 1.000)
       deallocate(Q34)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,M1,N2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,M1,N2,t2B,D2)
       allocate(Q35(M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8
       I3=K3*K4*K1
       call DMATMAT(I1,I2,I3,D1,D2,Q35)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M1,N2,M1,N2,X71,Q35, 1.000)
       deallocate(Q35)
C
       allocate(D1(M2+1:N3,M2+1:N3,N0+1:N2,N0+1:N1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & M2,N3,M2,N3,N0,N2,N0,N1,VBHHPP,D1)
       allocate(F2(M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N0+1:M1))
       call reorder132456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,N0,M1,t3C1,F2)
       allocate(U364(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:M1,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K5*K8*K8*K0
       I3=K6*K6
       call DMATMAT(I1,I2,I3,D1,F2,U364)
       deallocate(D1)
       deallocate(F2)
C
       allocate(F1(N0+1:N2,N0+1:N1,N2+1:M2,M1+1:N2,M1+1:N2,N0+1:M1))
       call reorder561234(N2,M2,M1,N2,M1,N2,N0,M1,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,M1,N2,M1,N2,N0,M1,U364,F1)
       allocate(D2(N0+1:N2,N0+1:N1,M2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,M2,N3,M2,N3,t2B,D2)
       allocate(Z365(M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N0+1:M1))
       I1=K5*K8*K8*K0
       I2=K6*K6
       I3=K1*K2
       call DMATMAT(I1,I2,I3,F1,D2,Z365)
       deallocate(F1)
       deallocate(D2)
C
       call
     & sum132456(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z365, 1.000)
       deallocate(Z365)
C
       allocate(F1(N0+1:N1,N0+1:N2,N2+1:M2,M1+1:N2,M1+1:N2,N0+1:M1))
       call reorder651234(N2,M2,M1,N2,M1,N2,N0,M1,N0,N2,N0,N1,
     & N0,N1,N0,N2,N2,M2,M1,N2,M1,N2,N0,M1,U364,F1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(U490(M2+1:N3,N0+1:N2,N2+1:M2,M1+1:N2,M1+1:N2,N0+1:M1))
       I1=K5*K8*K8*K0*K2
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,F1,B2,U490)
       deallocate(F1)
       deallocate(B2)
C
       call
     & sum312456(N0,N2,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,X77,U490,-1.000)
       deallocate(U490)
C
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(Z175(M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2,N0+1:M1))
       I1=K5*K8*K8*K6*K0
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,X77,B2,Z175)
       deallocate(B2)
C
       V3C=V3C-Z175
       deallocate(Z175)
       deallocate(X77)
C
       allocate(D1(N0+1:N2,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,M2,N3,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,M2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,M2,N3,M2,N3,t2B,D2)
       allocate(S263(M2+1:N3,M2+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K6*K6
       I3=K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,S263)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M2,N3,N1,M2,M2,N3,M2,N3,X45,S263, 1.000)
       deallocate(S263)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,M2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,M2,N3,M2,N3,t2B,D2)
       allocate(S264(M2+1:N3,M2+1:N3,N2+1:M2,N1+1:M2))
       I1=K9*K0
       I2=K6*K6
       I3=K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,S264)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,M2,N1,M2,M2,N3,M2,N3,X46,S264, 1.000)
       deallocate(S264)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,M2,N3,t2B,D2)
       allocate(Q36(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K3*K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,Q36)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M2,N3,M2,N3,X72,Q36,-1.000)
       deallocate(Q36)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,M2,N3,t2B,D2)
       allocate(Q37(M2+1:N3,N2+1:M2))
       I1=K0
       I2=K6
       I3=K3*K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,Q37)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N2,M2,M2,N3,X73,Q37,-1.000)
       deallocate(Q37)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:M2,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,M2,N3,t2B,D2)
       allocate(S265(N2+1:M2,M2+1:N3,N2+1:M2,M2+1:N3))
       I1=K6*K0
       I2=K6*K0
       I3=K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,S265)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,M2,M2,N3,N2,M2,M2,N3,X47,S265, 1.000)
       deallocate(S265)
C
       allocate(D1(N0+1:N2,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,M2,N3,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,M2,N3,t2B,D2)
       allocate(S266(N2+1:M2,M2+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K6*K0
       I3=K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,S266)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M2,N3,N1,M2,N2,M2,M2,N3,X48,S266, 1.000)
       deallocate(S266)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,M2,N3,t2B,D2)
       allocate(S267(N2+1:M2,M2+1:N3,N2+1:M2,N1+1:M2))
       I1=K9*K0
       I2=K6*K0
       I3=K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,S267)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,M2,N1,M2,N2,M2,M2,N3,X49,S267, 1.000)
       deallocate(S267)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,M2,t2B,D2)
       allocate(Q38(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K3*K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,Q38)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N2,M2,N2,M2,X74,Q38,-1.000)
       deallocate(Q38)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,M2,N3,t2B,D2)
       allocate(Q39(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K4*K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,Q39)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M2,N3,M2,N3,X19,Q39,-1.000)
       deallocate(Q39)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,M2,N3,t2B,D2)
       allocate(Q40(M2+1:N3,N1+1:M2))
       I1=K9
       I2=K6
       I3=K4*K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,Q40)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N1,M2,M2,N3,X20,Q40,-1.000)
       deallocate(Q40)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,N0,M1,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,t3D,F2)
       allocate(S268(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K5*K5
       call DMATMAT(I1,I2,I3,D1,F2,S268)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X89,S268,-0.500)
       deallocate(S268)
C
       allocate(D1(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M1,N2,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,t3D,F2)
       allocate(S269(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K8*K5
       call DMATMAT(I1,I2,I3,D1,F2,S269)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X89,S269,-1.000)
       deallocate(S269)
C
       allocate(D1(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M1,N2,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,t3D,F2)
       allocate(S270(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K8*K8
       call DMATMAT(I1,I2,I3,D1,F2,S270)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X89,S270,-0.500)
       deallocate(S270)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,N0,M1,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder452136(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N0,M1,N2,M2,M2,N3,N2,M2,M1,N2,t3D,F2)
       allocate(S271(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K0*K5*K5
       call DMATMAT(I1,I2,I3,D1,F2,S271)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X89,S271, 0.500)
       deallocate(S271)
C
       allocate(D1(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M1,N2,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder452136(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M1,N2,N2,M2,M2,N3,N2,M2,M1,N2,t3D,F2)
       allocate(S272(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K0*K8*K5
       call DMATMAT(I1,I2,I3,D1,F2,S272)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X89,S272, 1.000)
       deallocate(S272)
C
       allocate(D1(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M1,N2,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder452136(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M1,N2,N2,M2,M2,N3,N2,M2,M1,N2,t3D,F2)
       allocate(S273(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K0*K8*K8
       call DMATMAT(I1,I2,I3,D1,F2,S273)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X89,S273, 0.500)
       deallocate(S273)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M2,N3,M2,N3,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(S274(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K6*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S274)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X6,S274,-0.500)
       deallocate(S274)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M2,N3,M2,N3,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(S275(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K6*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,S275)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X6,S275,-0.500)
       deallocate(S275)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(S276(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K0*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S276)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X6,S276,-1.000)
       deallocate(S276)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(S277(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K0*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,S277)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X6,S277,-1.000)
       deallocate(S277)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,N2,M2,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(S278(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K0*K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,S278)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X6,S278,-0.500)
       deallocate(S278)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,N2,M2,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(S279(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K0*K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,S279)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X6,S279,-0.500)
       deallocate(S279)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder413256(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,t3D,F2)
       allocate(S280(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K0*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S280)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X5,S280, 1.000)
       deallocate(S280)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder413256(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,t3D,F2)
       allocate(S281(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K0*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,S281)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X5,S281, 1.000)
       deallocate(S281)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,N2,M2,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder423156(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,N2,M2,M2,N3,M1,N2,M1,N2,t3D,F2)
       allocate(S282(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K0*K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,S282)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X5,S282,-0.500)
       deallocate(S282)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,N2,M2,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder423156(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,N2,M2,M2,N3,M1,N2,M1,N2,t3D,F2)
       allocate(S283(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K0*K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,S283)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X5,S283,-0.500)
       deallocate(S283)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S284(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K6
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S284)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,M2,N3,N0,M1,X82,S284, 1.000)
       deallocate(S284)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S285(M2+1:N3,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K6
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S285)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,M2,N3,M2,N3,N0,M1,X83,S285, 1.000)
       deallocate(S285)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S286(M2+1:N3,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K6
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S286)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N2,M2,M2,N3,N0,M1,X84,S286, 1.000)
       deallocate(S286)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S287(M2+1:N3,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K6
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S287)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,N2,M2,M2,N3,N0,M1,X85,S287, 1.000)
       deallocate(S287)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder452136(N2,M2,M2,N3,M2,N3,M1,N2,N0,M1,N0,M1,
     & M1,N2,N0,M1,M2,N3,N2,M2,M2,N3,N0,M1,t3B2,F2)
       allocate(S288(N2+1:M2,M2+1:N3,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K6*K0
       I3=K6*K5*K8
       call DMATMAT(I1,I2,I3,D1,F2,S288)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,M2,N3,N0,M1,X10,S288,-1.000)
       deallocate(S288)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder462135(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,N2,M2,M2,N3,N0,M1,t3B3,F2)
       allocate(S289(N2+1:M2,M2+1:N3,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K6*K0
       I3=K6*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,S289)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,M2,N3,N0,M1,X10,S289, 1.000)
       deallocate(S289)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder462135(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,M2,N3,N2,M2,M2,N3,N0,M1,t3B3,F2)
       allocate(S290(N2+1:M2,M2+1:N3,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K6*K0
       I3=K6*K7*K8
       call DMATMAT(I1,I2,I3,D1,F2,S290)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,M2,N3,N0,M1,X10,S290, 1.000)
       deallocate(S290)
C
       allocate(D1(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder453126(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,N0,M1,N1,M2,N2,M2,M2,N3,N0,M1,t3B4,F2)
       allocate(S291(N2+1:M2,M2+1:N3,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K6*K0
       I3=K9*K5*K8
       call DMATMAT(I1,I2,I3,D1,F2,S291)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,M2,N3,N0,M1,X10,S291, 1.000)
       deallocate(S291)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder463125(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,N1,M2,N2,M2,M2,N3,N0,M1,t3B1,F2)
       allocate(S292(N2+1:M2,M2+1:N3,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K6*K0
       I3=K9*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,S292)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,M2,N3,N0,M1,X10,S292,-1.000)
       deallocate(S292)
C
       allocate(D1(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder463125(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,N1,M2,N2,M2,M2,N3,N0,M1,t3B1,F2)
       allocate(S293(N2+1:M2,M2+1:N3,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K6*K0
       I3=K9*K7*K8
       call DMATMAT(I1,I2,I3,D1,F2,S293)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,M2,N3,N0,M1,X10,S293,-1.000)
       deallocate(S293)
C
       allocate(D1(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N1+1:M2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder453126(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,N0,M1,N1,M2,M2,N3,M2,N3,N0,M1,t3B4,F2)
       allocate(S294(M2+1:N3,M2+1:N3,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K6*K6
       I3=K9*K5*K8
       call DMATMAT(I1,I2,I3,D1,F2,S294)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,M2,N3,N0,M1,X9,S294, 1.000)
       deallocate(S294)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder463125(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,N1,M2,M2,N3,M2,N3,N0,M1,t3B1,F2)
       allocate(S295(M2+1:N3,M2+1:N3,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K6*K6
       I3=K9*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,S295)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,M2,N3,N0,M1,X9,S295,-1.000)
       deallocate(S295)
C
       allocate(D1(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder463125(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,N1,M2,M2,N3,M2,N3,N0,M1,t3B1,F2)
       allocate(S296(M2+1:N3,M2+1:N3,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K6*K6
       I3=K9*K7*K8
       call DMATMAT(I1,I2,I3,D1,F2,S296)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,M2,N3,N0,M1,X9,S296,-1.000)
       deallocate(S296)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder512346(N2,M2,M2,N3,M2,N3,M1,N2,N0,M1,N0,M1,
     & N0,M1,N2,M2,M2,N3,M2,N3,M1,N2,N0,M1,t3B2,F2)
       allocate(S297(M2+1:N3,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K6
       I3=K6*K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,S297)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X91(N0+1:N2,M2+1:N3,M1+1:N2,N0+1:M1))
       X91=0.0d0
       call
     & sum2341(N0,N2,M2,N3,M1,N2,N0,M1,X91,S297, 1.000)
       deallocate(S297)
C
       allocate(D1(M1+1:N1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,N2,M2,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder612345(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N1,N2,M2,M2,N3,M2,N3,M1,N2,N0,M1,t3B3,F2)
       allocate(S298(M2+1:N3,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K6
       I3=K6*K0*K7
       call DMATMAT(I1,I2,I3,D1,F2,S298)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,N0,M1,X91,S298,-1.000)
       deallocate(S298)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder513246(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & N0,M1,M2,N3,N1,M2,M2,N3,M1,N2,N0,M1,t3B4,F2)
       allocate(S299(M2+1:N3,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K6
       I3=K9*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S299)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,N0,M1,X91,S299,-1.000)
       deallocate(S299)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder613245(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,M2,N3,M1,N2,N0,M1,t3B1,F2)
       allocate(S300(M2+1:N3,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K6
       I3=K9*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,S300)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,N0,M1,X91,S300, 1.000)
       deallocate(S300)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder513246(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & N0,M1,N2,M2,N1,M2,M2,N3,M1,N2,N0,M1,t3B4,F2)
       allocate(S301(M2+1:N3,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K6
       I3=K9*K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,S301)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,N0,M1,X91,S301,-1.000)
       deallocate(S301)
C
       allocate(D1(M1+1:N1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,N2,M2,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,N1+1:M2,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder613245(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,N2,M2,N1,M2,M2,N3,M1,N2,N0,M1,t3B1,F2)
       allocate(S302(M2+1:N3,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K6
       I3=K9*K0*K7
       call DMATMAT(I1,I2,I3,D1,F2,S302)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,N0,M1,X91,S302, 1.000)
       deallocate(S302)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S303(M2+1:N3,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K6
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S303)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,M2,N3,M1,N2,X11,S303, 1.000)
       deallocate(S303)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S304(M2+1:N3,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K8*K6
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S304)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,M2,N3,M2,N3,M1,N2,X12,S304, 1.000)
       deallocate(S304)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S305(M2+1:N3,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K8*K6
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S305)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N1,M2,M2,N3,M1,N2,X13,S305, 1.000)
       deallocate(S305)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S306(M2+1:N3,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K8*K6
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S306)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,N1,M2,M2,N3,M1,N2,X14,S306, 1.000)
       deallocate(S306)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S307(N2+1:M2,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K8*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S307)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N1,M2,N2,M2,M1,N2,X15,S307, 1.000)
       deallocate(S307)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S308(N2+1:M2,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K8*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S308)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,N1,M2,N2,M2,M1,N2,X16,S308, 1.000)
       deallocate(S308)
C
       allocate(D1(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M1,N2,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder451236(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N2,M2,N3,N2,M2,M2,N3,N0,M1,t3C1,F2)
       allocate(S309(N2+1:M2,M2+1:N3,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K6*K0
       I3=K6*K8*K5
       call DMATMAT(I1,I2,I3,D1,F2,S309)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,M2,N3,N0,M1,X10,S309, 1.000)
       deallocate(S309)
C
       allocate(D1(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M1,N2,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder451236(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N2,M2,N3,N2,M2,M2,N3,N0,M1,t3C1,F2)
       allocate(S310(N2+1:M2,M2+1:N3,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K6*K0
       I3=K6*K8*K8
       call DMATMAT(I1,I2,I3,D1,F2,S310)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,M2,N3,N0,M1,X10,S310, 0.500)
       deallocate(S310)
C
       allocate(D1(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M1,N2,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder451236(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N2,N2,M2,N2,M2,M2,N3,N0,M1,t3C1,F2)
       allocate(S311(N2+1:M2,M2+1:N3,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K6*K0
       I3=K0*K8*K5
       call DMATMAT(I1,I2,I3,D1,F2,S311)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,M2,N3,N0,M1,X10,S311, 1.000)
       deallocate(S311)
C
       allocate(D1(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M1,N2,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder451236(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N2,N2,M2,N2,M2,M2,N3,N0,M1,t3C1,F2)
       allocate(S312(N2+1:M2,M2+1:N3,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K6*K0
       I3=K0*K8*K8
       call DMATMAT(I1,I2,I3,D1,F2,S312)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,M2,N3,N0,M1,X10,S312, 0.500)
       deallocate(S312)
C
       allocate(D1(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M1,N2,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder452136(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N2,N2,M2,M2,N3,M2,N3,N0,M1,t3C1,F2)
       allocate(S313(M2+1:N3,M2+1:N3,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K6*K6
       I3=K0*K8*K5
       call DMATMAT(I1,I2,I3,D1,F2,S313)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,M2,N3,N0,M1,X9,S313,-1.000)
       deallocate(S313)
C
       allocate(D1(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M1,N2,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder452136(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N2,N2,M2,M2,N3,M2,N3,N0,M1,t3C1,F2)
       allocate(S314(M2+1:N3,M2+1:N3,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K6*K6
       I3=K0*K8*K8
       call DMATMAT(I1,I2,I3,D1,F2,S314)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,M2,N3,N0,M1,X9,S314,-0.500)
       deallocate(S314)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:M1,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,M1,M1,N2,VCHHPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,M1,N2,M1,N2,t2C,D2)
       allocate(S315(M1+1:N2,M1+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5
       I2=K8*K8
       I3=K4*K4
       call DMATMAT(I1,I2,I3,D1,D2,S315)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X50(N0+1:M1,M1+1:N2,M1+1:N2,M1+1:N2))
       X50=0.0d0
       call
     & sum3412(N0,M1,M1,N2,M1,N2,M1,N2,X50,S315, 0.500)
       deallocate(S315)
C
       allocate(D1(N2+1:N3,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,M1,N2,M1,N2,VCHHPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,M1,N2,M1,N2,t2C,D2)
       allocate(S316(M1+1:N2,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8
       I2=K8*K8
       I3=K4*K4
       call DMATMAT(I1,I2,I3,D1,D2,S316)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X51(M1+1:N2,M1+1:N2,M1+1:N2,M1+1:N2))
       X51=0.0d0
       call
     & sum3412(M1,N2,M1,N2,M1,N2,M1,N2,X51,S316, 0.500)
       deallocate(S316)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N2,M2,M2,N3,M1,N2,N0,M1,t3C1,F2)
       allocate(S317(M2+1:N3,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K6
       I3=K0*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S317)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,N0,M1,X91,S317,-1.000)
       deallocate(S317)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N2,M2,M2,N3,M1,N2,N0,M1,t3C1,F2)
       allocate(S318(M2+1:N3,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K6
       I3=K0*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,S318)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,N0,M1,X91,S318,-1.000)
       deallocate(S318)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,N2,M2,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N2,M2,M2,N3,M1,N2,N0,M1,t3C1,F2)
       allocate(S319(M2+1:N3,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K6
       I3=K0*K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,S319)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,N0,M1,X91,S319,-0.500)
       deallocate(S319)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,N2,M2,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,N2,M2,M2,N3,M1,N2,N0,M1,t3C1,F2)
       allocate(S320(M2+1:N3,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K6
       I3=K0*K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,S320)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,N0,M1,X91,S320,-0.500)
       deallocate(S320)
C
       allocate(D2(N0+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,M2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(Z406(M2+1:N3,N2+1:M2,M1+1:N2,M2+1:N3,M1+1:N2,N0+1:M1))
       I1=K5*K8*K6
       I2=K8*K0*K6
       I3=K2
       call DMATMAT(I1,I2,I3,X91,D2,Z406)
       deallocate(D2)
C
       call
     & sum125346(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z406,-1.000)
       call
     & sum124356(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z406, 1.000)
       deallocate(Z406)
       deallocate(X91)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S321(M2+1:N3,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K6
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S321)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,M2,N3,M1,N2,X52,S321,-1.000)
       deallocate(S321)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S322(M2+1:N3,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K6
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S322)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,M2,N3,M2,N3,M1,N2,X53,S322,-1.000)
       deallocate(S322)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S323(M2+1:N3,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K8*K6
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S323)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N2,M2,M2,N3,M1,N2,X54,S323,-1.000)
       deallocate(S323)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S324(M2+1:N3,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K8*K6
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S324)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,N2,M2,M2,N3,M1,N2,X55,S324,-1.000)
       deallocate(S324)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S325(N2+1:M2,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K8*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S325)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N2,M2,N2,M2,M1,N2,X56,S325,-1.000)
       deallocate(S325)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S326(N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K8*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S326)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,N2,M2,N2,M2,M1,N2,X57,S326,-1.000)
       deallocate(S326)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,M1,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,M1,N2,t2C,D2)
       allocate(Q41(M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8
       I3=K4*K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,Q41)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N0,M1,M1,N2,X21,Q41, 0.500)
       deallocate(Q41)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,M1,N2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,M1,N2,t2C,D2)
       allocate(Q42(M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8
       I3=K4*K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,Q42)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M1,N2,M1,N2,X22,Q42, 0.500)
       deallocate(Q42)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,N2,M2,t2C,D2)
       allocate(S327(M2+1:N3,N2+1:M2,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K0*K6
       I3=K2*K2
       call DMATMAT(I1,I2,I3,D1,D2,S327)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M2,N3,N2,M2,M2,N3,N2,M2,X58,S327, 0.500)
       deallocate(S327)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,M2,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,N2,M2,t2C,D2)
       allocate(S328(M2+1:N3,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0
       I2=K0*K6
       I3=K2*K2
       call DMATMAT(I1,I2,I3,D1,D2,S328)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,M2,N2,M2,M2,N3,N2,M2,X59,S328, 0.500)
       deallocate(S328)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,M2,N3,t2C,D2)
       allocate(Q43(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K4*K2*K2
       call DMATMAT(I1,I2,I3,D1,D2,Q43)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M2,N3,M2,N3,X23,Q43,-0.500)
       deallocate(Q43)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,M2,N3,t2C,D2)
       allocate(Q44(M2+1:N3,N2+1:M2))
       I1=K0
       I2=K6
       I3=K4*K2*K2
       call DMATMAT(I1,I2,I3,D1,D2,Q44)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N2,M2,M2,N3,X24,Q44,-0.500)
       deallocate(Q44)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,M2,t2C,D2)
       allocate(Q45(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K4*K2*K2
       call DMATMAT(I1,I2,I3,D1,D2,Q45)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N2,M2,N2,M2,X25,Q45,-0.500)
       deallocate(Q45)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,M1+1:N2))
       call reorder2341(N0,N2,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,N2,N0,N1,M1,N2,VBHHPH,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S329(N0+1:M1,N0+1:N2,N0+1:N1,M1+1:N2))
       I1=K8*K1*K2
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S329)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N0,N1,M1,N2,
     & N0,N2,N0,N1,M1,N2,N0,M1,S329,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S351(M2+1:N3,N0+1:N1,M1+1:N2,N0+1:M1))
       I1=K5*K8*K1
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S351)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,M2,N3,M1,N2,N0,M1,X1,S351,-1.000)
       deallocate(S351)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N0,N1,M1,N2,
     & N0,N2,N0,N1,M1,N2,N0,M1,S329,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S352(N2+1:M2,N0+1:N1,M1+1:N2,N0+1:M1))
       I1=K5*K8*K1
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S352)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N2,M2,M1,N2,N0,M1,X2,S352,-1.000)
       deallocate(S352)
C
       allocate(D1(N0+1:N1,N0+1:N2,M1+1:N2,N0+1:M1))
       call reorder3241(N0,M1,N0,N2,N0,N1,M1,N2,
     & N0,N1,N0,N2,M1,N2,N0,M1,S329,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S330(M2+1:N3,N0+1:N2,M1+1:N2,N0+1:M1))
       I1=K5*K8*K2
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S330)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,M2,N3,M1,N2,N0,M1,X8,S330,-1.000)
       deallocate(S330)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:N1,M2+1:N3))
       call reorder2341(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,M1,N0,N1,M2,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S335(N0+1:M1,N0+1:M1,N0+1:N1,M2+1:N3))
       I1=K6*K1*K5
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S335)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder3241(N0,M1,N0,M1,N0,N1,M2,N3,
     & N0,N1,N0,M1,M2,N3,N0,M1,S335,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S336(M2+1:N3,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S336)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,M2,N3,N0,M1,X78,S336,-1.000)
       deallocate(S336)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z210(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K6
       I3=K6*K5
       call DMATMAT(I1,I2,I3,X78,F2,Z210)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z210, 1.000)
       deallocate(Z210)
       deallocate(X78)
C
       allocate(D1(N1+1:N3,M1+1:N1,N0+1:N1,M2+1:N3))
       call reorder2341(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,M1,N1,N0,N1,M2,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S337(N0+1:M1,M1+1:N1,N0+1:N1,M2+1:N3))
       I1=K6*K1*K7
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S337)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder3241(N0,M1,M1,N1,N0,N1,M2,N3,
     & N0,N1,M1,N1,M2,N3,N0,M1,S337,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S338(M2+1:N3,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S338)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,M2,N3,M2,N3,N0,M1,X79,S338,-1.000)
       deallocate(S338)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z211(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K6
       I3=K6*K7
       call DMATMAT(I1,I2,I3,X79,F2,Z211)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z211, 1.000)
       deallocate(Z211)
       deallocate(X79)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:N1,N1+1:M2))
       call reorder2341(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,M1,N0,N1,N1,M2,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S339(N0+1:M1,N0+1:M1,N0+1:N1,N1+1:M2))
       I1=K9*K1*K5
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S339)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder3241(N0,M1,N0,M1,N0,N1,N1,M2,
     & N0,N1,N0,M1,N1,M2,N0,M1,S339,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S340(M2+1:N3,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S340)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N1,M2,M2,N3,N0,M1,X80,S340,-1.000)
       deallocate(S340)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N1,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z212(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K6
       I3=K9*K5
       call DMATMAT(I1,I2,I3,X80,F2,Z212)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z212, 1.000)
       deallocate(Z212)
       deallocate(X80)
C
       allocate(D1(N1+1:N3,M1+1:N1,N0+1:N1,N1+1:M2))
       call reorder2341(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,M1,N1,N0,N1,N1,M2,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S341(N0+1:M1,M1+1:N1,N0+1:N1,N1+1:M2))
       I1=K9*K1*K7
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S341)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder3241(N0,M1,M1,N1,N0,N1,N1,M2,
     & N0,N1,M1,N1,N1,M2,N0,M1,S341,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S342(M2+1:N3,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S342)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,N1,M2,M2,N3,N0,M1,X81,S342,-1.000)
       deallocate(S342)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N1,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z213(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K6
       I3=K9*K7
       call DMATMAT(I1,I2,I3,X81,F2,Z213)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z213, 1.000)
       deallocate(Z213)
       deallocate(X81)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,N1,N3,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q46(N0+1:M1,N1+1:N3))
       I1=K3*K5
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q46)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,N0+1:M1))
       call reorder21(N0,M1,N1,N3,
     & N1,N3,N0,M1,Q46,B1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(Q47(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,B1,B2,Q47)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(N0,M1,N0,M1,X17,Q47, 1.000)
       deallocate(Q47)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,N1,N3,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q48(M1+1:N1,N1+1:N3))
       I1=K3*K7
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q48)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,M1+1:N1))
       call reorder21(M1,N1,N1,N3,
     & N1,N3,M1,N1,Q48,B1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(Q49(N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,B1,B2,Q49)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(M1,N1,N0,M1,X18,Q49, 1.000)
       deallocate(Q49)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M2+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,M2,N3,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q50(N0+1:N1,M2+1:N3))
       I1=K6*K1
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q50)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(Q51(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,Q50,B2,Q51)
       deallocate(B2)
C
       call
     & sum21(M2,N3,M2,N3,X19,Q51,-1.000)
       deallocate(Q51)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:M2))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N1,M2,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q52(N0+1:N1,N1+1:M2))
       I1=K9*K1
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q52)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(Q53(M2+1:N3,N1+1:M2))
       I1=K9
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,Q52,B2,Q53)
       deallocate(B2)
C
       call
     & sum21(N1,M2,M2,N3,X20,Q53,-1.000)
       deallocate(Q53)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:N1,M2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,N0,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S343(N0+1:M1,N0+1:M1,N0+1:N1,M2+1:N3))
       I1=K6*K1*K5
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S343)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder3241(N0,M1,N0,M1,N0,N1,M2,N3,
     & N0,N1,N0,M1,M2,N3,N0,M1,S343,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S344(M2+1:N3,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S344)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,M2,N3,N0,M1,X82,S344,-1.000)
       deallocate(S344)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z218(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K6
       I3=K6*K5
       call DMATMAT(I1,I2,I3,X82,F2,Z218)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z218, 1.000)
       deallocate(Z218)
       deallocate(X82)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:N1,M2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,N0,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S345(N0+1:M1,M1+1:N2,N0+1:N1,M2+1:N3))
       I1=K6*K1*K8
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S345)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder3241(N0,M1,M1,N2,N0,N1,M2,N3,
     & N0,N1,M1,N2,M2,N3,N0,M1,S345,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S346(M2+1:N3,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S346)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M2,N3,M2,N3,N0,M1,X83,S346,-1.000)
       deallocate(S346)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z219(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K6
       I3=K6*K8
       call DMATMAT(I1,I2,I3,X83,F2,Z219)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z219, 1.000)
       deallocate(Z219)
       deallocate(X83)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:N1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,N0,N1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S347(N0+1:M1,N0+1:M1,N0+1:N1,N2+1:M2))
       I1=K0*K1*K5
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S347)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder3241(N0,M1,N0,M1,N0,N1,N2,M2,
     & N0,N1,N0,M1,N2,M2,N0,M1,S347,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S348(M2+1:N3,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S348)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N2,M2,M2,N3,N0,M1,X84,S348,-1.000)
       deallocate(S348)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z220(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K6
       I3=K0*K5
       call DMATMAT(I1,I2,I3,X84,F2,Z220)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z220,-1.000)
       deallocate(Z220)
       deallocate(X84)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:N1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,N0,N1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S349(N0+1:M1,M1+1:N2,N0+1:N1,N2+1:M2))
       I1=K0*K1*K8
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S349)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder3241(N0,M1,M1,N2,N0,N1,N2,M2,
     & N0,N1,M1,N2,N2,M2,N0,M1,S349,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S350(M2+1:N3,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S350)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,N2,M2,M2,N3,N0,M1,X85,S350,-1.000)
       deallocate(S350)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z221(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K6
       I3=K0*K8
       call DMATMAT(I1,I2,I3,X85,F2,Z221)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z221,-1.000)
       deallocate(Z221)
       deallocate(X85)
C
       allocate(D1(N1+1:N3,N0+1:N1,N2+1:N3,M2+1:N3))
       call reorder2413(N2,N3,N1,N3,N2,N3,N0,N1,
     & N1,N3,N0,N1,N2,N3,M2,N3,VBHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S331(N0+1:M1,N0+1:N1,N2+1:N3,M2+1:N3))
       I1=K6*K4*K1
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S331)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N0+1:N1,M2+1:N3,N0+1:M1))
       call reorder3241(N0,M1,N0,N1,N2,N3,M2,N3,
     & N2,N3,N0,N1,M2,N3,N0,M1,S331,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S353(M1+1:N2,N0+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K1
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S353)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N1,M2,N3,M1,N2,N0,M1,X1,S353, 1.000)
       deallocate(S353)
C
       allocate(D1(N0+1:N1,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N1,N2,N3,M2,N3,
     & N0,N1,N2,N3,M2,N3,N0,M1,S331,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S332(M2+1:N3,N2+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K4
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S332)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,N3,M2,N3,M2,N3,N0,M1,X9,S332,-1.000)
       deallocate(S332)
C
       allocate(D1(N1+1:N3,N0+1:N1,N2+1:N3,N2+1:M2))
       call reorder2413(N2,N3,N1,N3,N2,N3,N0,N1,
     & N1,N3,N0,N1,N2,N3,N2,M2,VBHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S333(N0+1:M1,N0+1:N1,N2+1:N3,N2+1:M2))
       I1=K0*K4*K1
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S333)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:M2,N0+1:M1))
       call reorder3241(N0,M1,N0,N1,N2,N3,N2,M2,
     & N2,N3,N0,N1,N2,M2,N0,M1,S333,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S354(M1+1:N2,N0+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K1
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S354)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N1,N2,M2,M1,N2,N0,M1,X2,S354, 1.000)
       deallocate(S354)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N1,N2,N3,N2,M2,
     & N0,N1,N2,N3,N2,M2,N0,M1,S333,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S334(M2+1:N3,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S334)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,N3,N2,M2,M2,N3,N0,M1,X10,S334,-1.000)
       deallocate(S334)
C
       allocate(D1(N0+1:N1,N0+1:N2,N1+1:N3,M1+1:N2))
       call reorder4321(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N0,N2,N1,N3,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S355(M2+1:N3,N0+1:N2,N1+1:N3,M1+1:N2))
       I1=K8*K3*K2
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S355)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder2314(M2,N3,N0,N2,N1,N3,M1,N2,
     & N0,N2,N1,N3,M2,N3,M1,N2,S355,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S357(N2+1:M2,N1+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K3
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S357)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,N2,M2,M2,N3,M1,N2,X4,S357, 1.000)
       deallocate(S357)
C
       allocate(D1(N0+1:N2,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder2314(M2,N3,N0,N2,N1,N3,M1,N2,
     & N0,N2,N1,N3,M2,N3,M1,N2,S355,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S356(M2+1:N3,N1+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K3
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S356)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,M2,N3,M1,N2,X3,S356, 1.000)
       deallocate(S356)
C
       allocate(D1(N2+1:N3,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,N1,N1,N3,M2,N3,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S358(M1+1:N2,N0+1:N1,N1+1:N3,M2+1:N3))
       I1=K6*K3*K1
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S358)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N1,N1,N3,M2,N3,
     & N0,N1,N1,N3,M2,N3,M1,N2,S358,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S359(M2+1:N3,N1+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K3
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S359)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,N3,M2,N3,M2,N3,M1,N2,X3,S359,-1.000)
       deallocate(S359)
C
       allocate(D1(N2+1:N3,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,N1,N1,N3,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S360(M1+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       I1=K0*K3*K1
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S360)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N1,N1,N3,N2,M2,
     & N0,N1,N1,N3,N2,M2,M1,N2,S360,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S361(M2+1:N3,N1+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K3
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S361)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,N3,N2,M2,M2,N3,M1,N2,X4,S361,-1.000)
       deallocate(S361)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:M1))
       call reorder1342(N2,N3,N0,N1,N0,N2,N0,N1,
     & N2,N3,N0,N2,N0,N1,N0,M1,VBHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S362(M1+1:N2,N0+1:N2,N0+1:N1,N0+1:M1))
       I1=K5*K1*K2
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S362)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,N0+1:M1))
       call reorder2314(M1,N2,N0,N2,N0,N1,N0,M1,
     & N0,N2,N0,N1,M1,N2,N0,M1,S362,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S434(M2+1:N3,N0+1:N1,M1+1:N2,N0+1:M1))
       I1=K5*K8*K1
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S434)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,M2,N3,M1,N2,N0,M1,X1,S434,-1.000)
       deallocate(S434)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,N0+1:M1))
       call reorder2314(M1,N2,N0,N2,N0,N1,N0,M1,
     & N0,N2,N0,N1,M1,N2,N0,M1,S362,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S435(N2+1:M2,N0+1:N1,M1+1:N2,N0+1:M1))
       I1=K5*K8*K1
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S435)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N2,M2,M1,N2,N0,M1,X2,S435,-1.000)
       deallocate(S435)
C
       allocate(D1(N0+1:N1,N0+1:N2,M1+1:N2,N0+1:M1))
       call reorder3214(M1,N2,N0,N2,N0,N1,N0,M1,
     & N0,N1,N0,N2,M1,N2,N0,M1,S362,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S363(M2+1:N3,N0+1:N2,M1+1:N2,N0+1:M1))
       I1=K5*K8*K2
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S363)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,M2,N3,M1,N2,N0,M1,X8,S363,-1.000)
       deallocate(S363)
C
       allocate(D1(N1+1:N3,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,N0,N2,N2,N3,M2,N3,VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S367(N0+1:M1,N0+1:N2,N2+1:N3,M2+1:N3))
       I1=K6*K4*K2
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S367)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N2,N3,M2,N3,
     & N0,N2,N2,N3,M2,N3,N0,M1,S367,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S369(M2+1:N3,N2+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K4
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S369)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,M2,N3,N0,M1,X9,S369,-1.000)
       deallocate(S369)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N2,N3,M2,N3,
     & N0,N2,N2,N3,M2,N3,N0,M1,S367,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S370(N2+1:M2,N2+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K4
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S370)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,M2,N3,N0,M1,X10,S370,-1.000)
       deallocate(S370)
C
       allocate(D1(N2+1:N3,N0+1:N2,M2+1:N3,N0+1:M1))
       call reorder3241(N0,M1,N0,N2,N2,N3,M2,N3,
     & N2,N3,N0,N2,M2,N3,N0,M1,S367,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S368(M1+1:N2,N0+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K2
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S368)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N2,M2,N3,M1,N2,N0,M1,X8,S368, 1.000)
       deallocate(S368)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:M1,N2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,N0,M1,N2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S371(N0+1:M1,N0+1:M1,N0+1:M1,N2+1:N3))
       I1=K4*K5*K5
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S371)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:M1,N0+1:M1))
       call reorder4231(N0,M1,N0,M1,N0,M1,N2,N3,
     & N2,N3,N0,M1,N0,M1,N0,M1,S371,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S372(M1+1:N2,N0+1:M1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K5
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S372)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N0,M1,M1,N2,N0,M1,X30,S372, 1.000)
       deallocate(S372)
C
       call sumx3412(N0,N2,N0,N1,N0,N2,N0,N1,
     & N0,M1,N0,M1,M1,N2,N0,M1,X30,VBHHHH, 1.000)
C
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N0,M1,M2,N3,N2,M2,M2,N3,M1,N2,t3C1,F2)
       allocate(Z30(M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2,N0+1:M1))
       I1=K5*K8
       I2=K8*K6*K0*K6
       I3=K5*K5
       call DMATMAT(I1,I2,I3,X30,F2,Z30)
       deallocate(F2)
C
       V3C=V3C-Z30
       call
     & sum123546(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z30, 1.000)
       deallocate(Z30)
       deallocate(X30)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:M1,N2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,N0,M1,N2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S373(N0+1:M1,M1+1:N2,N0+1:M1,N2+1:N3))
       I1=K4*K5*K8
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S373)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:M1,N0+1:M1))
       call reorder4231(N0,M1,M1,N2,N0,M1,N2,N3,
     & N2,N3,M1,N2,N0,M1,N0,M1,S373,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S374(M1+1:N2,M1+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K8
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S374)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,N0,M1,M1,N2,N0,M1,X31,S374, 1.000)
       deallocate(S374)
C
       call sumx3412(N0,N2,N0,N1,N0,N2,N0,N1,
     & M1,N2,N0,M1,M1,N2,N0,M1,X31,VBHHHH, 1.000)
C
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N2,M2,M2,N3,M1,N2,t3C1,F2)
       allocate(Z31(M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2,N0+1:M1))
       I1=K5*K8
       I2=K8*K6*K0*K6
       I3=K5*K8
       call DMATMAT(I1,I2,I3,X31,F2,Z31)
       deallocate(F2)
C
       V3C=V3C-Z31
       call
     & sum123546(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z31, 1.000)
       deallocate(Z31)
       deallocate(X31)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,N2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,M1,N1,N2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S375(N0+1:M1,N0+1:M1,M1+1:N1,N2+1:N3))
       I1=K4*K7*K5
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S375)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N1,N0+1:M1))
       call reorder4231(N0,M1,N0,M1,M1,N1,N2,N3,
     & N2,N3,N0,M1,M1,N1,N0,M1,S375,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S376(M1+1:N2,N0+1:M1,M1+1:N1,N0+1:M1))
       I1=K5*K7*K5
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S376)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M1,N1,M1,N2,N0,M1,X32,S376, 1.000)
       deallocate(S376)
C
       call sumx3412(N0,N2,N0,N1,N0,N2,N0,N1,
     & N0,M1,M1,N1,M1,N2,N0,M1,X32,VBHHHH, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N2,M2,M2,N3,M1,N2,t3C1,F2)
       allocate(Z32(M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2,N0+1:M1))
       I1=K5*K8
       I2=K8*K6*K0*K6
       I3=K7*K5
       call DMATMAT(I1,I2,I3,X32,F2,Z32)
       deallocate(F2)
C
       V3C=V3C-Z32
       call
     & sum123546(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z32, 1.000)
       deallocate(Z32)
       deallocate(X32)
C
       allocate(D1(N1+1:N3,M1+1:N2,M1+1:N1,N2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,M1,N1,N2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S377(N0+1:M1,M1+1:N2,M1+1:N1,N2+1:N3))
       I1=K4*K7*K8
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S377)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N1,N0+1:M1))
       call reorder4231(N0,M1,M1,N2,M1,N1,N2,N3,
     & N2,N3,M1,N2,M1,N1,N0,M1,S377,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S378(M1+1:N2,M1+1:N2,M1+1:N1,N0+1:M1))
       I1=K5*K7*K8
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S378)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M1,N1,M1,N2,N0,M1,X33,S378, 1.000)
       deallocate(S378)
C
       call sumx3412(N0,N2,N0,N1,N0,N2,N0,N1,
     & M1,N2,M1,N1,M1,N2,N0,M1,X33,VBHHHH, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N2,M2,M2,N3,M1,N2,t3C1,F2)
       allocate(Z33(M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2,N0+1:M1))
       I1=K5*K8
       I2=K8*K6*K0*K6
       I3=K7*K8
       call DMATMAT(I1,I2,I3,X33,F2,Z33)
       deallocate(F2)
C
       V3C=V3C-Z33
       call
     & sum123546(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z33, 1.000)
       deallocate(Z33)
       deallocate(X33)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:M1,M2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,N2,N0,M1,M2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S379(N0+1:M1,N0+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5*K2
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S379)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N0,M1,M2,N3,
     & N0,N2,N0,M1,M2,N3,N0,M1,S379,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S380(M2+1:N3,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S380)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,M2,N3,N0,M1,X60,S380,-1.000)
       deallocate(S380)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder612345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,t3C1,F2)
       allocate(Z93(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K6*K0
       I3=K6*K5
       call DMATMAT(I1,I2,I3,X60,F2,Z93)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z93,-1.000)
       deallocate(Z93)
       deallocate(X60)
C
       allocate(D1(N1+1:N3,N0+1:N2,M1+1:N1,M2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,N2,M1,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S381(N0+1:M1,N0+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7*K2
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S381)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,M1,N1,M2,N3,
     & N0,N2,M1,N1,M2,N3,N0,M1,S381,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S382(M2+1:N3,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S382)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,M2,N3,M2,N3,N0,M1,X61,S382,-1.000)
       deallocate(S382)
C
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder612345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,t3C1,F2)
       allocate(Z94(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K6*K0
       I3=K6*K7
       call DMATMAT(I1,I2,I3,X61,F2,Z94)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z94,-1.000)
       deallocate(Z94)
       deallocate(X61)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:M1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,N2,N0,M1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S383(N0+1:M1,N0+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5*K2
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S383)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N0,M1,N2,M2,
     & N0,N2,N0,M1,N2,M2,N0,M1,S383,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S387(N2+1:M2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S387)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N2,M2,N2,M2,N0,M1,X64,S387,-1.000)
       deallocate(S387)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder621345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,M2,N3,M1,N2,M1,N2,t3C1,F2)
       allocate(Z97(M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K6*K6
       I3=K0*K5
       call DMATMAT(I1,I2,I3,X64,F2,Z97)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z97,-1.000)
       deallocate(Z97)
       deallocate(X64)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N0,M1,N2,M2,
     & N0,N2,N0,M1,N2,M2,N0,M1,S383,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S384(M2+1:N3,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S384)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N2,M2,M2,N3,N0,M1,X62,S384,-1.000)
       deallocate(S384)
C
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder612345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N2,M2,M2,N3,M1,N2,M1,N2,t3C1,F2)
       allocate(Z95(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K6*K0
       I3=K0*K5
       call DMATMAT(I1,I2,I3,X62,F2,Z95)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z95,-1.000)
       deallocate(Z95)
       deallocate(X62)
C
       allocate(D1(N1+1:N3,N0+1:N2,M1+1:N1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,N2,M1,N1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S385(N0+1:M1,N0+1:N2,M1+1:N1,N2+1:M2))
       I1=K0*K7*K2
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S385)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N1,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,M1,N1,N2,M2,
     & N0,N2,M1,N1,N2,M2,N0,M1,S385,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S388(N2+1:M2,M1+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K7
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S388)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,N2,M2,N2,M2,N0,M1,X65,S388,-1.000)
       deallocate(S388)
C
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder621345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,M2,N3,M2,N3,M1,N2,M1,N2,t3C1,F2)
       allocate(Z98(M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K6*K6
       I3=K0*K7
       call DMATMAT(I1,I2,I3,X65,F2,Z98)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z98,-1.000)
       deallocate(Z98)
       deallocate(X65)
C
       allocate(D1(N0+1:N2,M1+1:N1,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,M1,N1,N2,M2,
     & N0,N2,M1,N1,N2,M2,N0,M1,S385,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S386(M2+1:N3,M1+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K7
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S386)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,N2,M2,M2,N3,N0,M1,X63,S386,-1.000)
       deallocate(S386)
C
       allocate(F2(M1+1:N1,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder612345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,N2,M2,M2,N3,M1,N2,M1,N2,t3C1,F2)
       allocate(Z96(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K6*K0
       I3=K0*K7
       call DMATMAT(I1,I2,I3,X63,F2,Z96)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z96,-1.000)
       deallocate(Z96)
       deallocate(X63)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,N1,N3,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q54(N0+1:M1,N1+1:N3))
       I1=K3*K5
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q54)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,N0+1:M1))
       call reorder21(N0,M1,N1,N3,
     & N1,N3,N0,M1,Q54,B1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(Q55(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,B1,B2,Q55)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(N0,M1,N0,M1,X17,Q55, 1.000)
       deallocate(Q55)
C
       call sumx21(N0,N1,N0,N1,
     & N0,M1,N0,M1,X17,FAHH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder612345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,t3C1,F2)
       allocate(Z17(M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8*K8*K6*K0*K6
       I3=K5
       call DMATMAT(I1,I2,I3,X17,F2,Z17)
       deallocate(F2)
C
       V3C=V3C-Z17
       deallocate(Z17)
       deallocate(X17)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,N1,N3,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q56(M1+1:N1,N1+1:N3))
       I1=K3*K7
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q56)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,M1+1:N1))
       call reorder21(M1,N1,N1,N3,
     & N1,N3,M1,N1,Q56,B1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(Q57(N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,B1,B2,Q57)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(M1,N1,N0,M1,X18,Q57, 1.000)
       deallocate(Q57)
C
       call sumx21(N0,N1,N0,N1,
     & M1,N1,N0,M1,X18,FAHH, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder612345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,t3C1,F2)
       allocate(Z18(M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8*K8*K6*K0*K6
       I3=K7
       call DMATMAT(I1,I2,I3,X18,F2,Z18)
       deallocate(F2)
C
       V3C=V3C-Z18
       deallocate(Z18)
       deallocate(X18)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:N1,M2+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,M1,N0,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S389(M1+1:N2,N0+1:M1,N0+1:N1,M2+1:N3))
       I1=K6*K1*K5
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S389)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder3241(M1,N2,N0,M1,N0,N1,M2,N3,
     & N0,N1,N0,M1,M2,N3,M1,N2,S389,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S390(M2+1:N3,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S390)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,M2,N3,M1,N2,X66,S390, 1.000)
       deallocate(S390)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder431256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z99(M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K5*K8*K0*K6
       I3=K6*K5
       call DMATMAT(I1,I2,I3,X66,F2,Z99)
       deallocate(F2)
C
       call
     & sum124635(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z99,-1.000)
       call
     & sum125634(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z99, 1.000)
       deallocate(Z99)
       deallocate(X66)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:N1,M2+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,M1,N2,N0,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S391(M1+1:N2,M1+1:N2,N0+1:N1,M2+1:N3))
       I1=K6*K1*K8
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S391)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder3241(M1,N2,M1,N2,N0,N1,M2,N3,
     & N0,N1,M1,N2,M2,N3,M1,N2,S391,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S392(M2+1:N3,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S392)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M2,N3,M2,N3,M1,N2,X67,S392, 1.000)
       deallocate(S392)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder431256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z100(M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K5*K8*K0*K6
       I3=K6*K8
       call DMATMAT(I1,I2,I3,X67,F2,Z100)
       deallocate(F2)
C
       call
     & sum124635(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z100,-1.000)
       call
     & sum125634(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z100, 1.000)
       deallocate(Z100)
       deallocate(X67)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:N1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,M1,N0,N1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S393(M1+1:N2,N0+1:M1,N0+1:N1,N1+1:M2))
       I1=K9*K1*K5
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S393)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,M1+1:N2))
       call reorder3241(M1,N2,N0,M1,N0,N1,N1,M2,
     & N0,N1,N0,M1,N1,M2,M1,N2,S393,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S394(M2+1:N3,N0+1:M1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K5
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S394)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N1,M2,M2,N3,M1,N2,X68,S394, 1.000)
       deallocate(S394)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder431256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N1,M2,M2,N3,N2,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z101(M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K5*K8*K0*K6
       I3=K9*K5
       call DMATMAT(I1,I2,I3,X68,F2,Z101)
       deallocate(F2)
C
       call
     & sum124635(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z101,-1.000)
       call
     & sum125634(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z101, 1.000)
       deallocate(Z101)
       deallocate(X68)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:N1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,M1,N2,N0,N1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S395(M1+1:N2,M1+1:N2,N0+1:N1,N1+1:M2))
       I1=K9*K1*K8
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S395)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N2,N1+1:M2,M1+1:N2))
       call reorder3241(M1,N2,M1,N2,N0,N1,N1,M2,
     & N0,N1,M1,N2,N1,M2,M1,N2,S395,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S396(M2+1:N3,M1+1:N2,N1+1:M2,M1+1:N2))
       I1=K8*K9*K8
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S396)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,N1,M2,M2,N3,M1,N2,X69,S396, 1.000)
       deallocate(S396)
C
       allocate(F2(M1+1:N2,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder431256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N1,M2,M2,N3,N2,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z102(M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K5*K8*K0*K6
       I3=K9*K8
       call DMATMAT(I1,I2,I3,X69,F2,Z102)
       deallocate(F2)
C
       call
     & sum124635(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z102,-1.000)
       call
     & sum125634(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z102, 1.000)
       deallocate(Z102)
       deallocate(X69)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,N2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q58(N0+1:M1,N2+1:N3))
       I1=K4*K5
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q58)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:M1))
       call reorder21(N0,M1,N2,N3,
     & N2,N3,N0,M1,Q58,B1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(Q59(M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,B1,B2,Q59)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(N0,M1,M1,N2,X70,Q59, 1.000)
       deallocate(Q59)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N2,M2,M2,N3,M1,N2,N0,M1,t3C1,F2)
       allocate(Z103(M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,N0+1:M1,M1+1:N2))
       I1=K8
       I2=K5*K8*K6*K0*K6
       I3=K5
       call DMATMAT(I1,I2,I3,X70,F2,Z103)
       deallocate(F2)
C
       call
     & sum123465(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z103, 1.000)
       call
     & sum123564(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z103,-1.000)
       deallocate(Z103)
       deallocate(X70)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,N2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q60(M1+1:N2,N2+1:N3))
       I1=K4*K8
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q60)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,M1+1:N2))
       call reorder21(M1,N2,N2,N3,
     & N2,N3,M1,N2,Q60,B1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(Q61(M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,B1,B2,Q61)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(M1,N2,M1,N2,X71,Q61, 1.000)
       deallocate(Q61)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N2,M2,M2,N3,M1,N2,N0,M1,t3C1,F2)
       allocate(Z104(M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,N0+1:M1,M1+1:N2))
       I1=K8
       I2=K5*K8*K6*K0*K6
       I3=K8
       call DMATMAT(I1,I2,I3,X71,F2,Z104)
       deallocate(F2)
C
       call
     & sum123465(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z104, 1.000)
       call
     & sum123564(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z104,-1.000)
       deallocate(Z104)
       deallocate(X71)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:M2,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,M2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,M2,N3,M2,N3,t2B,D2)
       allocate(S262(M2+1:N3,M2+1:N3,N2+1:M2,M2+1:N3))
       I1=K6*K0
       I2=K6*K6
       I3=K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,S262)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,M2,M2,N3,M2,N3,M2,N3,X44,S262, 1.000)
       deallocate(S262)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:M2,M2+1:N3))
       call reorder4312(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N0,N2,N2,M2,M2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S397(M2+1:N3,N0+1:N2,N2+1:M2,M2+1:N3))
       I1=K6*K0*K2
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S397)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:M2,M2+1:N3,M2+1:N3))
       call reorder2341(M2,N3,N0,N2,N2,M2,M2,N3,
     & N0,N2,N2,M2,M2,N3,M2,N3,S397,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S403(N2+1:M2,N2+1:M2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K0
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S403)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,M2,M2,N3,N2,M2,M2,N3,X47,S403, 1.000)
       deallocate(S403)
C
       call sumx1234(N2,N3,N1,N3,N2,M2,N1,N3,
     & N2,M2,M2,N3,N2,M2,M2,N3,X47,VBPAPP, 1.000)
C
       allocate(F2(N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2,N0+1:M1))
       call reorder231456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N2,M2,M2,N3,M2,N3,M1,N2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z47(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:M1,N2+1:M2,M2+1:N3))
       I1=K6*K0
       I2=K5*K8*K8*K6
       I3=K6*K0
       call DMATMAT(I1,I2,I3,X47,F2,Z47)
       deallocate(F2)
C
       call
     & sum145623(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z47, 1.000)
       deallocate(Z47)
       deallocate(X47)
C
       allocate(D1(N0+1:N2,N2+1:M2,M2+1:N3,M2+1:N3))
       call reorder2341(M2,N3,N0,N2,N2,M2,M2,N3,
     & N0,N2,N2,M2,M2,N3,M2,N3,S397,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S398(M2+1:N3,N2+1:M2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K0
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S398)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,M2,M2,N3,M2,N3,M2,N3,X44,S398, 1.000)
       deallocate(S398)
C
       call sumx3412(N2,N3,N1,N3,N2,M2,N1,N3,
     & N2,M2,M2,N3,M2,N3,M2,N3,X44,VBPAPP, 1.000)
C
       allocate(F2(N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N0+1:M1))
       call reorder132456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z44(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3))
       I1=K6*K6
       I2=K5*K8*K8*K0
       I3=K6*K0
       call DMATMAT(I1,I2,I3,X44,F2,Z44)
       deallocate(F2)
C
       call
     & sum245613(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z44, 1.000)
       deallocate(Z44)
       deallocate(X44)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:M2,N1+1:M2))
       call reorder4312(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N0,N2,N2,M2,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S401(M2+1:N3,N0+1:N2,N2+1:M2,N1+1:M2))
       I1=K9*K0*K2
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S401)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:M2,N1+1:M2,M2+1:N3))
       call reorder2341(M2,N3,N0,N2,N2,M2,N1,M2,
     & N0,N2,N2,M2,N1,M2,M2,N3,S401,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S405(N2+1:M2,N2+1:M2,N1+1:M2,M2+1:N3))
       I1=K6*K9*K0
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S405)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,M2,N1,M2,N2,M2,M2,N3,X49,S405, 1.000)
       deallocate(S405)
C
       call sumx1234(N2,N3,N1,N3,N2,M2,N1,N3,
     & N2,M2,N1,M2,N2,M2,M2,N3,X49,VBPAPP, 1.000)
C
       allocate(F2(N2+1:M2,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N2,N0+1:M1))
       call reorder231456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N2,M2,N1,M2,M2,N3,M1,N2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z49(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:M1,N2+1:M2,M2+1:N3))
       I1=K6*K0
       I2=K5*K8*K8*K6
       I3=K9*K0
       call DMATMAT(I1,I2,I3,X49,F2,Z49)
       deallocate(F2)
C
       call
     & sum145623(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z49, 1.000)
       deallocate(Z49)
       deallocate(X49)
C
       allocate(D1(N0+1:N2,N2+1:M2,N1+1:M2,M2+1:N3))
       call reorder2341(M2,N3,N0,N2,N2,M2,N1,M2,
     & N0,N2,N2,M2,N1,M2,M2,N3,S401,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S402(M2+1:N3,N2+1:M2,N1+1:M2,M2+1:N3))
       I1=K6*K9*K0
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S402)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,M2,N1,M2,M2,N3,M2,N3,X46,S402, 1.000)
       deallocate(S402)
C
       call sumx3412(N2,N3,N1,N3,N2,N3,N1,M2,
     & N2,M2,N1,M2,M2,N3,M2,N3,X46,VBAPPP, 1.000)
C
       allocate(F2(N2+1:M2,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N0+1:M1))
       call reorder132456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N2,M2,N1,M2,N2,M2,M1,N2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z46(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3))
       I1=K6*K6
       I2=K5*K8*K8*K0
       I3=K9*K0
       call DMATMAT(I1,I2,I3,X46,F2,Z46)
       deallocate(F2)
C
       call
     & sum245613(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z46, 1.000)
       deallocate(Z46)
       deallocate(X46)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N2,M2,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q64(N0+1:N2,N2+1:M2))
       I1=K0*K2
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q64)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(Q66(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,Q64,B2,Q66)
       deallocate(B2)
C
       call
     & sum21(N2,M2,N2,M2,X74,Q66,-1.000)
       deallocate(Q66)
C
       allocate(F2(N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2,N0+1:M1))
       call reorder213456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N2,M2,M2,N3,M2,N3,M1,N2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z115(M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0
       I2=K5*K8*K8*K6*K6
       I3=K0
       call DMATMAT(I1,I2,I3,X74,F2,Z115)
       deallocate(F2)
C
       call
     & sum134562(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z115, 1.000)
       deallocate(Z115)
       deallocate(X74)
C
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(Q65(M2+1:N3,N2+1:M2))
       I1=K0
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,Q64,B2,Q65)
       deallocate(B2)
C
       call
     & sum21(N2,M2,M2,N3,X73,Q65,-1.000)
       deallocate(Q65)
C
       allocate(F2(N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2,N0+1:M1))
       call reorder123456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N2,M2,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z111(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K8*K8*K6*K0
       I3=K0
       call DMATMAT(I1,I2,I3,X73,F2,Z111)
       deallocate(F2)
C
       call
     & sum234561(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z111, 1.000)
       deallocate(Z111)
       deallocate(X73)
C
       allocate(D1(N0+1:N1,N0+1:N2,M2+1:N3,N1+1:M2))
       call reorder4312(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N0,N2,M2,N3,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S399(M2+1:N3,N0+1:N2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K2
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S399)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M2+1:N3,N1+1:M2,M2+1:N3))
       call reorder2341(M2,N3,N0,N2,M2,N3,N1,M2,
     & N0,N2,M2,N3,N1,M2,M2,N3,S399,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S400(M2+1:N3,M2+1:N3,N1+1:M2,M2+1:N3))
       I1=K6*K9*K6
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S400)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,N1,M2,M2,N3,M2,N3,X45,S400, 1.000)
       deallocate(S400)
C
       call sumx3412(N2,N3,N1,N3,N2,N3,N1,M2,
     & M2,N3,N1,M2,M2,N3,M2,N3,X45,VBAPPP, 1.000)
C
       allocate(F2(M2+1:N3,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N0+1:M1))
       call reorder132456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M2,N3,N1,M2,N2,M2,M1,N2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z45(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3))
       I1=K6*K6
       I2=K5*K8*K8*K0
       I3=K9*K6
       call DMATMAT(I1,I2,I3,X45,F2,Z45)
       deallocate(F2)
C
       call
     & sum245613(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z45, 1.000)
       deallocate(Z45)
       deallocate(X45)
C
       allocate(D1(N0+1:N2,M2+1:N3,N1+1:M2,M2+1:N3))
       call reorder2341(M2,N3,N0,N2,M2,N3,N1,M2,
     & N0,N2,M2,N3,N1,M2,M2,N3,S399,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S404(N2+1:M2,M2+1:N3,N1+1:M2,M2+1:N3))
       I1=K6*K9*K6
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S404)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,N1,M2,N2,M2,M2,N3,X48,S404, 1.000)
       deallocate(S404)
C
       call sumx1234(N2,N3,N1,N3,N2,M2,N1,N3,
     & M2,N3,N1,M2,N2,M2,M2,N3,X48,VBPAPP, 1.000)
C
       allocate(F2(M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N2,N0+1:M1))
       call reorder132456(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & M2,N3,N1,M2,M2,N3,M1,N2,M1,N2,N0,M1,t3C3,F2)
       allocate(Z48(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:M1,N2+1:M2,M2+1:N3))
       I1=K6*K0
       I2=K5*K8*K8*K6
       I3=K9*K6
       call DMATMAT(I1,I2,I3,X48,F2,Z48)
       deallocate(F2)
C
       call
     & sum145623(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z48,-1.000)
       deallocate(Z48)
       deallocate(X48)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,M2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q62(N0+1:N2,M2+1:N3))
       I1=K6*K2
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q62)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(Q63(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,Q62,B2,Q63)
       deallocate(B2)
C
       call
     & sum21(M2,N3,M2,N3,X72,Q63,-1.000)
       deallocate(Q63)
C
       allocate(F2(M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2,N0+1:M1))
       call reorder123456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z110(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K8*K8*K6*K0
       I3=K6
       call DMATMAT(I1,I2,I3,X72,F2,Z110)
       deallocate(F2)
C
       call
     & sum234561(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z110, 1.000)
       deallocate(Z110)
       deallocate(X72)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q67(N0+1:N1,M2+1:N3))
       I1=K6*K1
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q67)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(Q68(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,Q67,B2,Q68)
       deallocate(B2)
C
       call
     & sum21(M2,N3,M2,N3,X19,Q68,-1.000)
       deallocate(Q68)
C
       call sumx12(N1,N3,N1,N3,
     & M2,N3,M2,N3,X19,FAPP, 1.000)
C
       allocate(F2(M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N0+1:M1))
       call reorder312456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z19(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K8*K8*K0*K6
       I3=K6
       call DMATMAT(I1,I2,I3,X19,F2,Z19)
       deallocate(F2)
C
       call
     & sum124563(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z19, 1.000)
       deallocate(Z19)
       deallocate(X19)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q69(N0+1:N1,N1+1:M2))
       I1=K9*K1
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q69)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(Q70(M2+1:N3,N1+1:M2))
       I1=K9
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,Q69,B2,Q70)
       deallocate(B2)
C
       call
     & sum21(N1,M2,M2,N3,X20,Q70,-1.000)
       deallocate(Q70)
C
       call sumx12(N1,N3,N1,N3,
     & N1,M2,M2,N3,X20,FAPP, 1.000)
C
       allocate(F2(N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N0+1:M1))
       call reorder312456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N1,M2,M2,N3,N2,M2,M1,N2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z20(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K8*K8*K0*K6
       I3=K9
       call DMATMAT(I1,I2,I3,X20,F2,Z20)
       deallocate(F2)
C
       call
     & sum124563(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z20, 1.000)
       deallocate(Z20)
       deallocate(X20)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       call reorder2341(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,N1,N0,N1,N1,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S406(N0+1:M1,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1*K1
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S406)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder2431(N0,M1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N1,N0,M1,S406,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S408(M2+1:N3,M1+1:N2,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K8*K6
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S408)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,M2,N3,M1,N2,N0,M1,X1,S408, 1.000)
       deallocate(S408)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder2431(N0,M1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N1,N0,M1,S406,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S407(N2+1:M2,M1+1:N2,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K8*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S407)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N2,M2,M1,N2,N0,M1,X86,S407, 1.000)
       deallocate(S407)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N1,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S409(N2+1:M2,M1+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K8*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S409)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N2,M2,M1,N2,S409,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S410(M2+1:N3,N1+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K3
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S410)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,N3,N2,M2,M2,N3,M1,N2,X87,S410, 1.000)
       deallocate(S410)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N1,N3,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q71(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q71)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q71,B1)
       allocate(D2(N1+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,M2,M1,N2,N0,M1,t2B,D2)
       allocate(S414(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K3
       call DMATMAT(I1,I2,I3,B1,D2,S414)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X2,S414, 1.000)
       deallocate(S414)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q71,B1)
       allocate(D2(N1+1:N3,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M2,N3,M1,N2,N0,M1,t2B,D2)
       allocate(S411(M2+1:N3,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K6
       I3=K3
       call DMATMAT(I1,I2,I3,B1,D2,S411)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,M2,N3,M1,N2,N0,M1,X1,S411, 1.000)
       deallocate(S411)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N1,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S412(M2+1:N3,M1+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K8*K6
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S412)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder3412(M2,N3,M1,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,M2,N3,M1,N2,S412,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S413(M2+1:N3,N1+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K3
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S413)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,N3,M2,N3,M2,N3,M1,N2,X88,S413, 1.000)
       deallocate(S413)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,N2,N0,N1,N2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S415(N0+1:M1,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4*K1*K2
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S415)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder2431(N0,M1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N2,N3,N0,N1,N0,M1,S415,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S417(M2+1:N3,M1+1:N2,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K8*K6
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S417)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,M2,N3,M1,N2,N0,M1,X90,S417, 1.000)
       deallocate(S417)
C
       allocate(D2(N0+1:N1,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,M2,M2,N3,M1,N2,t2B,D2)
       allocate(Z244(N2+1:M2,M2+1:N3,M1+1:N2,M2+1:N3,M1+1:N2,N0+1:M1))
       I1=K5*K8*K6
       I2=K8*K6*K0
       I3=K1
       call DMATMAT(I1,I2,I3,X90,D2,Z244)
       deallocate(D2)
C
       call
     & sum235146(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z244,-1.000)
       call
     & sum234156(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z244, 1.000)
       deallocate(Z244)
       deallocate(X90)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:M1))
       call reorder3421(N0,M1,N0,N2,N0,N1,N2,N3,
     & N0,N1,N2,N3,N0,N2,N0,M1,S415,D1)
       allocate(D2(N0+1:N1,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S418(M2+1:N3,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K6
       I3=K4*K1
       call DMATMAT(I1,I2,I3,D1,D2,S418)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,M2,N3,M1,N2,N0,M1,X8,S418,-1.000)
       deallocate(S418)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N0,N1,N2,N3,N0,M1,S415,D1)
       allocate(D2(N0+1:N2,N0+1:N1,M2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,M2,N3,M2,N3,t2B,D2)
       allocate(S419(M2+1:N3,M2+1:N3,N2+1:N3,N0+1:M1))
       I1=K5*K4
       I2=K6*K6
       I3=K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,S419)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N2,N3,M2,N3,M2,N3,N0,M1,X9,S419, 1.000)
       deallocate(S419)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N0,N1,N2,N3,N0,M1,S415,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,M2,N3,t2B,D2)
       allocate(S420(N2+1:M2,M2+1:N3,N2+1:N3,N0+1:M1))
       I1=K5*K4
       I2=K6*K0
       I3=K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,S420)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N2,N3,N2,M2,M2,N3,N0,M1,X10,S420, 1.000)
       deallocate(S420)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:M1))
       call reorder4231(N0,M1,N0,N2,N0,N1,N2,N3,
     & N2,N3,N0,N2,N0,N1,N0,M1,S415,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S518(M1+1:N2,N0+1:N2,N0+1:N1,N0+1:M1))
       I1=K5*K1*K2
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S518)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N2,M1+1:N2,N0+1:M1))
       call reorder3214(M1,N2,N0,N2,N0,N1,N0,M1,
     & N0,N1,N0,N2,M1,N2,N0,M1,S518,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S519(M2+1:N3,N0+1:N2,M1+1:N2,N0+1:M1))
       I1=K5*K8*K2
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S519)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,M2,N3,M1,N2,N0,M1,X8,S519,-1.000)
       deallocate(S519)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:N3,N0+1:M1))
       call reorder3241(N0,M1,N0,N2,N0,N1,N2,N3,
     & N0,N1,N0,N2,N2,N3,N0,M1,S415,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S520(M2+1:N3,N0+1:N2,N2+1:N3,N0+1:M1))
       I1=K5*K4*K2
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S520)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder2314(M2,N3,N0,N2,N2,N3,N0,M1,
     & N0,N2,N2,N3,M2,N3,N0,M1,S520,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S521(M2+1:N3,N2+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K4
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S521)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,M2,N3,N0,M1,X9,S521, 1.000)
       deallocate(S521)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder2431(N0,M1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N2,N3,N0,N1,N0,M1,S415,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S416(N2+1:M2,M1+1:N2,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K8*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S416)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N2,M2,M1,N2,N0,M1,X86,S416, 1.000)
       deallocate(S416)
C
       allocate(D2(N0+1:N1,M2+1:N3,M2+1:N3,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,M2,N3,M2,N3,M1,N2,t2B,D2)
       allocate(Z224(M2+1:N3,M2+1:N3,M1+1:N2,N2+1:M2,M1+1:N2,N0+1:M1))
       I1=K5*K8*K0
       I2=K8*K6*K6
       I3=K1
       call DMATMAT(I1,I2,I3,X86,D2,Z224)
       deallocate(D2)
C
       call
     & sum135246(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z224, 1.000)
       call
     & sum134256(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z224,-1.000)
       deallocate(Z224)
       deallocate(X86)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,N0+1:M1))
       call reorder2314(M1,N2,N0,N2,N0,N1,N0,M1,
     & N0,N2,N0,N1,M1,N2,N0,M1,S518,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S523(M2+1:N3,N0+1:N1,M1+1:N2,N0+1:M1))
       I1=K5*K8*K1
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S523)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,M2,N3,M1,N2,N0,M1,X1,S523,-1.000)
       deallocate(S523)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,N0+1:M1))
       call reorder2314(M1,N2,N0,N2,N0,N1,N0,M1,
     & N0,N2,N0,N1,M1,N2,N0,M1,S518,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S524(N2+1:M2,N0+1:N1,M1+1:N2,N0+1:M1))
       I1=K5*K8*K1
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S524)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N2,M2,M1,N2,N0,M1,X2,S524,-1.000)
       deallocate(S524)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder2314(M2,N3,N0,N2,N2,N3,N0,M1,
     & N0,N2,N2,N3,M2,N3,N0,M1,S520,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S522(N2+1:M2,N2+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K4
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S522)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,M2,N3,N0,M1,X10,S522, 1.000)
       deallocate(S522)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q72(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q72)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q72,B1)
       allocate(D2(N2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,M2,N3,M1,N2,M1,N2,t2C,D2)
       allocate(S433(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K4
       call DMATMAT(I1,I2,I3,B1,D2,S433)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X5,S433,-1.000)
       deallocate(S433)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q72,B1)
       allocate(D2(N2+1:N3,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,M2,N3,M1,N2,N0,M1,t2B,D2)
       allocate(S427(M2+1:N3,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K6
       I3=K4
       call DMATMAT(I1,I2,I3,B1,D2,S427)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,N0,M1,X8,S427, 1.000)
       deallocate(S427)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q72,B1)
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,M1,N2,M1,N2,t2C,D2)
       allocate(S430(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K4
       call DMATMAT(I1,I2,I3,B1,D2,S430)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X6,S430,-1.000)
       deallocate(S430)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:N3,N0+1:M1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N0,N2,N2,N3,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S364(M2+1:N3,N0+1:N2,N2+1:N3,N0+1:M1))
       I1=K5*K4*K2
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S364)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder2314(M2,N3,N0,N2,N2,N3,N0,M1,
     & N0,N2,N2,N3,M2,N3,N0,M1,S364,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S366(N2+1:M2,N2+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K4
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S366)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,M2,N3,N0,M1,X10,S366, 1.000)
       deallocate(S366)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder2314(M2,N3,N0,N2,N2,N3,N0,M1,
     & N0,N2,N2,N3,M2,N3,N0,M1,S364,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S365(M2+1:N3,N2+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K4
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S365)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,M2,N3,N0,M1,X9,S365, 1.000)
       deallocate(S365)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,M2+1:N3))
       call reorder1324(N2,N3,N1,N3,N0,N2,N1,N3,
     & N2,N3,N0,N2,N1,N3,M2,N3,VBPHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S436(M1+1:N2,N0+1:N2,N1+1:N3,M2+1:N3))
       I1=K6*K3*K2
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S436)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N1,N3,M2,N3,
     & N0,N2,N1,N3,M2,N3,M1,N2,S436,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S438(N2+1:M2,N1+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K3
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S438)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,N2,M2,M2,N3,M1,N2,X4,S438,-1.000)
       deallocate(S438)
C
       allocate(D1(N0+1:N2,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N1,N3,M2,N3,
     & N0,N2,N1,N3,M2,N3,M1,N2,S436,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S437(M2+1:N3,N1+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K3
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S437)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,M2,N3,M1,N2,X3,S437,-1.000)
       deallocate(S437)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,M1+1:N2))
       call reorder1342(N2,N3,N0,N2,N0,N2,N0,N2,
     & N2,N3,N0,N2,N0,N2,M1,N2,VCHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S439(M1+1:N2,N0+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2*K2
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S439)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder2314(M1,N2,N0,N2,N0,N2,M1,N2,
     & N0,N2,N0,N2,M1,N2,M1,N2,S439,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S441(N2+1:M2,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S441)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,N2,M2,M1,N2,M1,N2,X76,S441,-1.000)
       deallocate(S441)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder2314(M1,N2,N0,N2,N0,N2,M1,N2,
     & N0,N2,N0,N2,M1,N2,M1,N2,S439,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S440(M2+1:N3,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S440)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,M2,N3,M1,N2,M1,N2,X75,S440,-1.000)
       deallocate(S440)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder4312(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S442(N2+1:M2,N0+1:N2,N2+1:N3,M1+1:N2))
       I1=K8*K4*K2
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S442)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder2314(N2,M2,N0,N2,N2,N3,M1,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,S442,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S443(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S443)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,M1,N2,X7,S443, 1.000)
       deallocate(S443)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder2413(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,N2,N2,N3,M2,N3,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S444(M1+1:N2,N0+1:N2,N2+1:N3,M2+1:N3))
       I1=K6*K4*K2
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S444)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N2,N3,M2,N3,
     & N0,N2,N2,N3,M2,N3,M1,N2,S444,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S448(N2+1:M2,N2+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K4
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S448)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,N3,M2,N3,N2,M2,M1,N2,X7,S448,-1.000)
       deallocate(S448)
C
       allocate(D1(N2+1:N3,N0+1:N2,M2+1:N3,M1+1:N2))
       call reorder3241(M1,N2,N0,N2,N2,N3,M2,N3,
     & N2,N3,N0,N2,M2,N3,M1,N2,S444,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S445(M1+1:N2,N0+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K2
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S445)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N2,M2,N3,M1,N2,M1,N2,X5,S445, 1.000)
       deallocate(S445)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder2413(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,N2,N2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S446(M1+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4*K2
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S446)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N2,N3,N2,M2,
     & N0,N2,N2,N3,N2,M2,M1,N2,S446,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S449(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S449)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,M1,N2,X7,S449, 1.000)
       deallocate(S449)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:M2,M1+1:N2))
       call reorder3241(M1,N2,N0,N2,N2,N3,N2,M2,
     & N2,N3,N0,N2,N2,M2,M1,N2,S446,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S447(M1+1:N2,N0+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K2
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S447)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N2,N2,M2,M1,N2,M1,N2,X6,S447, 1.000)
       deallocate(S447)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,M2+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,N2,N0,M1,M2,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S450(M1+1:N2,N0+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5*K2
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S450)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,M1,M2,N3,
     & N0,N2,N0,M1,M2,N3,M1,N2,S450,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S451(M2+1:N3,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S451)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,M2,N3,M1,N2,X11,S451,-1.000)
       deallocate(S451)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,M2,N3,M2,N3,M1,N2,X11,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder521346(N2,M2,M2,N3,M2,N3,M1,N2,N0,M1,N0,M1,
     & N0,M1,M2,N3,N2,M2,M2,N3,M1,N2,N0,M1,t3B2,F2)
       allocate(Z11(N2+1:M2,M2+1:N3,M1+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K5*K8*K6*K0
       I3=K6*K5
       call DMATMAT(I1,I2,I3,X11,F2,Z11)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z11,-1.000)
       call
     & sum235614(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z11, 1.000)
       deallocate(Z11)
       deallocate(X11)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N1,M2+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,N2,M1,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S452(M1+1:N2,N0+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7*K2
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S452)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,M1,N1,M2,N3,
     & N0,N2,M1,N1,M2,N3,M1,N2,S452,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S453(M2+1:N3,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K7
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S453)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,M2,N3,M2,N3,M1,N2,X12,S453,-1.000)
       deallocate(S453)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,M2,N3,M2,N3,M1,N2,X12,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder621345(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,N2,M2,M2,N3,M1,N2,N0,M1,t3B3,F2)
       allocate(Z12(N2+1:M2,M2+1:N3,M1+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K5*K8*K6*K0
       I3=K6*K7
       call DMATMAT(I1,I2,I3,X12,F2,Z12)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z12, 1.000)
       call
     & sum235614(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z12,-1.000)
       deallocate(Z12)
       deallocate(X12)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,N2,N0,M1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S454(M1+1:N2,N0+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5*K2
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S454)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,M1,N1,M2,
     & N0,N2,N0,M1,N1,M2,M1,N2,S454,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S458(N2+1:M2,N0+1:M1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K5
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S458)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N1,M2,N2,M2,M1,N2,X15,S458,-1.000)
       deallocate(S458)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,N1,M2,N2,M2,M1,N2,X15,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder531246(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & N0,M1,N1,M2,M2,N3,M2,N3,M1,N2,N0,M1,t3B4,F2)
       allocate(Z15(M2+1:N3,M2+1:N3,M1+1:N2,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K8*K6*K6
       I3=K9*K5
       call DMATMAT(I1,I2,I3,X15,F2,Z15)
       deallocate(F2)
C
       call
     & sum134625(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z15,-1.000)
       call
     & sum135624(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z15, 1.000)
       deallocate(Z15)
       deallocate(X15)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,M1,N1,M2,
     & N0,N2,N0,M1,N1,M2,M1,N2,S454,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S455(M2+1:N3,N0+1:M1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K5
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S455)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N1,M2,M2,N3,M1,N2,X13,S455,-1.000)
       deallocate(S455)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,N1,M2,M2,N3,M1,N2,X13,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,N2+1:M2,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder531246(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & N0,M1,N1,M2,N2,M2,M2,N3,M1,N2,N0,M1,t3B4,F2)
       allocate(Z13(N2+1:M2,M2+1:N3,M1+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K5*K8*K6*K0
       I3=K9*K5
       call DMATMAT(I1,I2,I3,X13,F2,Z13)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z13, 1.000)
       call
     & sum235614(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z13,-1.000)
       deallocate(Z13)
       deallocate(X13)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,N2,M1,N1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S456(M1+1:N2,N0+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7*K2
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S456)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,M1,N1,N1,M2,
     & N0,N2,M1,N1,N1,M2,M1,N2,S456,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S459(N2+1:M2,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K7
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S459)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,N1,M2,N2,M2,M1,N2,X16,S459,-1.000)
       deallocate(S459)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,N1,M2,N2,M2,M1,N2,X16,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder631245(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,N1,M2,M2,N3,M2,N3,M1,N2,N0,M1,t3B1,F2)
       allocate(Z16(M2+1:N3,M2+1:N3,M1+1:N2,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K8*K6*K6
       I3=K9*K7
       call DMATMAT(I1,I2,I3,X16,F2,Z16)
       deallocate(F2)
C
       call
     & sum134625(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z16, 1.000)
       call
     & sum135624(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z16,-1.000)
       deallocate(Z16)
       deallocate(X16)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,M1,N1,N1,M2,
     & N0,N2,M1,N1,N1,M2,M1,N2,S456,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S457(M2+1:N3,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K7
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S457)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,N1,M2,M2,N3,M1,N2,X14,S457,-1.000)
       deallocate(S457)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,N1,M2,M2,N3,M1,N2,X14,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,N2+1:M2,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder631245(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,N1,M2,N2,M2,M2,N3,M1,N2,N0,M1,t3B1,F2)
       allocate(Z14(N2+1:M2,M2+1:N3,M1+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K5*K8*K6*K0
       I3=K9*K7
       call DMATMAT(I1,I2,I3,X14,F2,Z14)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z14,-1.000)
       call
     & sum235614(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z14, 1.000)
       deallocate(Z14)
       deallocate(X14)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,N2+1:N3))
       call reorder2341(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,M1,M1,N2,N2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S460(M1+1:N2,N0+1:M1,M1+1:N2,N2+1:N3))
       I1=K4*K8*K5
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S460)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder4231(M1,N2,N0,M1,M1,N2,N2,N3,
     & N2,N3,N0,M1,M1,N2,M1,N2,S460,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S461(M1+1:N2,N0+1:M1,M1+1:N2,M1+1:N2))
       I1=K8*K8*K5
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S461)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M1,N2,M1,N2,M1,N2,X50,S461, 1.000)
       deallocate(S461)
C
       call sumx3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,M1,M1,N2,M1,N2,M1,N2,X50,VCHHHH, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder451236(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N2,M2,N3,N2,M2,M2,N3,N0,M1,t3C1,F2)
       allocate(Z50(M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1,M1+1:N2,M1+1:N2))
       I1=K8*K8
       I2=K5*K6*K0*K6
       I3=K8*K5
       call DMATMAT(I1,I2,I3,X50,F2,Z50)
       deallocate(F2)
C
       call
     & sum123645(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z50, 1.000)
       deallocate(Z50)
       deallocate(X50)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,N2+1:N3))
       call reorder2341(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,M1,N2,M1,N2,N2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S462(M1+1:N2,M1+1:N2,M1+1:N2,N2+1:N3))
       I1=K4*K8*K8
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S462)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder4231(M1,N2,M1,N2,M1,N2,N2,N3,
     & N2,N3,M1,N2,M1,N2,M1,N2,S462,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S463(M1+1:N2,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K8
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S463)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M1,N2,M1,N2,M1,N2,X51,S463, 1.000)
       deallocate(S463)
C
       call sumx3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & M1,N2,M1,N2,M1,N2,M1,N2,X51,VCHHHH, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder451236(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N2,M2,N3,N2,M2,M2,N3,N0,M1,t3C1,F2)
       allocate(Z51(M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1,M1+1:N2,M1+1:N2))
       I1=K8*K8
       I2=K5*K6*K0*K6
       I3=K8*K8
       call DMATMAT(I1,I2,I3,X51,F2,Z51)
       deallocate(F2)
C
       call
     & sum123645(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z51, 0.500)
       deallocate(Z51)
       deallocate(X51)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,M2+1:N3))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,N2,N0,M1,M2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S464(M1+1:N2,N0+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5*K2
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S464)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,M1,M2,N3,
     & N0,N2,N0,M1,M2,N3,M1,N2,S464,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S465(M2+1:N3,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S465)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,M2,N3,M1,N2,X52,S465, 1.000)
       deallocate(S465)
C
       call sumx2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,M1,M2,N3,M2,N3,M1,N2,X52,VCHPHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N2,M2,M2,N3,M1,N2,N0,M1,t3C1,F2)
       allocate(Z52(N2+1:M2,M2+1:N3,M1+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K5*K8*K6*K0
       I3=K6*K5
       call DMATMAT(I1,I2,I3,X52,F2,Z52)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z52, 1.000)
       call
     & sum235614(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z52,-1.000)
       deallocate(Z52)
       deallocate(X52)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N2,M2+1:N3))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,N2,M1,N2,M2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S466(M1+1:N2,N0+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8*K2
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S466)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,M1,N2,M2,N3,
     & N0,N2,M1,N2,M2,N3,M1,N2,S466,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S467(M2+1:N3,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S467)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M2,N3,M2,N3,M1,N2,X53,S467, 1.000)
       deallocate(S467)
C
       call sumx2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,M2,N3,M2,N3,M1,N2,X53,VCHPHP, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N2,M2,M2,N3,M1,N2,N0,M1,t3C1,F2)
       allocate(Z53(N2+1:M2,M2+1:N3,M1+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K5*K8*K6*K0
       I3=K6*K8
       call DMATMAT(I1,I2,I3,X53,F2,Z53)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z53, 1.000)
       call
     & sum235614(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z53,-1.000)
       deallocate(Z53)
       deallocate(X53)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,N2+1:M2))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,N2,N0,M1,N2,M2,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S468(M1+1:N2,N0+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5*K2
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S468)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,M1,N2,M2,
     & N0,N2,N0,M1,N2,M2,M1,N2,S468,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S472(N2+1:M2,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0*K5
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S472)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N2,M2,N2,M2,M1,N2,X56,S472, 1.000)
       deallocate(S472)
C
       call sumx2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,M1,N2,M2,N2,M2,M1,N2,X56,VCHPHP, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder421356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,M2,N3,M1,N2,N0,M1,t3C1,F2)
       allocate(Z56(M2+1:N3,M2+1:N3,M1+1:N2,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K8*K6*K6
       I3=K0*K5
       call DMATMAT(I1,I2,I3,X56,F2,Z56)
       deallocate(F2)
C
       call
     & sum134625(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z56, 1.000)
       call
     & sum135624(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z56,-1.000)
       deallocate(Z56)
       deallocate(X56)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,M1,N2,M2,
     & N0,N2,N0,M1,N2,M2,M1,N2,S468,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S469(M2+1:N3,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0*K5
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S469)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N2,M2,M2,N3,M1,N2,X54,S469, 1.000)
       deallocate(S469)
C
       call sumx2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,M1,N2,M2,M2,N3,M1,N2,X54,VCHPHP, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N2,M2,M2,N3,M1,N2,N0,M1,t3C1,F2)
       allocate(Z54(N2+1:M2,M2+1:N3,M1+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K5*K8*K6*K0
       I3=K0*K5
       call DMATMAT(I1,I2,I3,X54,F2,Z54)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z54, 1.000)
       call
     & sum235614(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z54,-1.000)
       deallocate(Z54)
       deallocate(X54)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N2,N2+1:M2))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,N2,M1,N2,N2,M2,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S470(M1+1:N2,N0+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8*K2
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S470)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,M1,N2,N2,M2,
     & N0,N2,M1,N2,N2,M2,M1,N2,S470,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S473(N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K8
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S473)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,N2,M2,N2,M2,M1,N2,X57,S473, 1.000)
       deallocate(S473)
C
       call sumx2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,N2,M2,N2,M2,M1,N2,X57,VCHPHP, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder421356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,M2,N3,M1,N2,N0,M1,t3C1,F2)
       allocate(Z57(M2+1:N3,M2+1:N3,M1+1:N2,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K8*K6*K6
       I3=K0*K8
       call DMATMAT(I1,I2,I3,X57,F2,Z57)
       deallocate(F2)
C
       call
     & sum134625(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z57, 1.000)
       call
     & sum135624(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z57,-1.000)
       deallocate(Z57)
       deallocate(X57)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,M1,N2,N2,M2,
     & N0,N2,M1,N2,N2,M2,M1,N2,S470,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S471(M2+1:N3,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K8
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S471)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,N2,M2,M2,N3,M1,N2,X55,S471, 1.000)
       deallocate(S471)
C
       call sumx2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,N2,M2,M2,N3,M1,N2,X55,VCHPHP, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,N2,M2,M2,N3,M1,N2,N0,M1,t3C1,F2)
       allocate(Z55(N2+1:M2,M2+1:N3,M1+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K5*K8*K6*K0
       I3=K0*K8
       call DMATMAT(I1,I2,I3,X55,F2,Z55)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z55, 1.000)
       call
     & sum235614(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z55,-1.000)
       deallocate(Z55)
       deallocate(X55)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,N2,N3,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q73(N0+1:M1,N2+1:N3))
       I1=K4*K5
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q73)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:M1))
       call reorder21(N0,M1,N2,N3,
     & N2,N3,N0,M1,Q73,B1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(Q74(M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,B1,B2,Q74)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(N0,M1,M1,N2,X21,Q74, 1.000)
       deallocate(Q74)
C
       call sumx21(N0,N2,N0,N2,
     & N0,M1,M1,N2,X21,FBHH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N2,M2,M2,N3,M1,N2,N0,M1,t3C1,F2)
       allocate(Z21(M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,N0+1:M1,M1+1:N2))
       I1=K8
       I2=K5*K8*K6*K0*K6
       I3=K5
       call DMATMAT(I1,I2,I3,X21,F2,Z21)
       deallocate(F2)
C
       call
     & sum123465(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z21, 1.000)
       call
     & sum123564(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z21,-1.000)
       deallocate(Z21)
       deallocate(X21)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,N2,N3,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q75(M1+1:N2,N2+1:N3))
       I1=K4*K8
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q75)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,M1+1:N2))
       call reorder21(M1,N2,N2,N3,
     & N2,N3,M1,N2,Q75,B1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(Q76(M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,B1,B2,Q76)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(M1,N2,M1,N2,X22,Q76, 1.000)
       deallocate(Q76)
C
       call sumx21(N0,N2,N0,N2,
     & M1,N2,M1,N2,X22,FBHH, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N2,M2,M2,N3,M1,N2,N0,M1,t3C1,F2)
       allocate(Z22(M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,N0+1:M1,M1+1:N2))
       I1=K8
       I2=K5*K8*K6*K0*K6
       I3=K8
       call DMATMAT(I1,I2,I3,X22,F2,Z22)
       deallocate(F2)
C
       call
     & sum123465(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z22, 1.000)
       call
     & sum123564(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z22,-1.000)
       deallocate(Z22)
       deallocate(X22)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder4312(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,N2,M2,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S474(N2+1:M2,N0+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6*K2
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S474)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder2341(N2,M2,N0,N2,M2,N3,N2,M2,
     & N0,N2,M2,N3,N2,M2,N2,M2,S474,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S475(M2+1:N3,M2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K6
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S475)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,N2,M2,M2,N3,N2,M2,X58,S475, 1.000)
       deallocate(S475)
C
       call sumx1234(N2,N3,N2,N3,N2,N3,N2,M2,
     & M2,N3,N2,M2,M2,N3,N2,M2,X58,VCAPPP, 1.000)
C
       allocate(F2(M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2,N0+1:M1))
       call reorder123456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z58(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K5*K8*K8*K6
       I3=K0*K6
       call DMATMAT(I1,I2,I3,X58,F2,Z58)
       deallocate(F2)
C
       call
     & sum345612(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z58, 1.000)
       deallocate(Z58)
       deallocate(X58)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder4312(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,M2,N2,M2,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S476(N2+1:M2,N0+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K2
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S476)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder2341(N2,M2,N0,N2,N2,M2,N2,M2,
     & N0,N2,N2,M2,N2,M2,N2,M2,S476,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S477(M2+1:N3,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K0
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S477)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,M2,N2,M2,M2,N3,N2,M2,X59,S477, 1.000)
       deallocate(S477)
C
       call sumx1234(N2,N3,N2,N3,N2,N3,N2,M2,
     & N2,M2,N2,M2,M2,N3,N2,M2,X59,VCAPPP, 1.000)
C
       allocate(F2(N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2,N0+1:M1))
       call reorder123456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N2,M2,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z59(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K5*K8*K8*K6
       I3=K0*K0
       call DMATMAT(I1,I2,I3,X59,F2,Z59)
       deallocate(F2)
C
       call
     & sum345612(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z59, 0.500)
       deallocate(Z59)
       deallocate(X59)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,M2,N3,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q77(N0+1:N2,M2+1:N3))
       I1=K6*K2
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q77)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(Q78(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,Q77,B2,Q78)
       deallocate(B2)
C
       call
     & sum21(M2,N3,M2,N3,X23,Q78,-1.000)
       deallocate(Q78)
C
       call sumx12(N2,N3,N2,N3,
     & M2,N3,M2,N3,X23,FBPP, 1.000)
C
       allocate(F2(M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2,N0+1:M1))
       call reorder123456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z23(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K8*K8*K6*K0
       I3=K6
       call DMATMAT(I1,I2,I3,X23,F2,Z23)
       deallocate(F2)
C
       call
     & sum234561(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z23, 1.000)
       deallocate(Z23)
       deallocate(X23)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N2,M2,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q79(N0+1:N2,N2+1:M2))
       I1=K0*K2
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q79)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(Q81(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,Q79,B2,Q81)
       deallocate(B2)
C
       call
     & sum21(N2,M2,N2,M2,X25,Q81,-1.000)
       deallocate(Q81)
C
       call sumx12(N2,N3,N2,N3,
     & N2,M2,N2,M2,X25,FBPP, 1.000)
C
       allocate(F2(N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2,N0+1:M1))
       call reorder213456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N2,M2,M2,N3,M2,N3,M1,N2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z25(M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0
       I2=K5*K8*K8*K6*K6
       I3=K0
       call DMATMAT(I1,I2,I3,X25,F2,Z25)
       deallocate(F2)
C
       call
     & sum134562(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z25, 1.000)
       deallocate(Z25)
       deallocate(X25)
C
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(Q80(M2+1:N3,N2+1:M2))
       I1=K0
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,Q79,B2,Q80)
       deallocate(B2)
C
       call
     & sum21(N2,M2,M2,N3,X24,Q80,-1.000)
       deallocate(Q80)
C
       call sumx12(N2,N3,N2,N3,
     & N2,M2,M2,N3,X24,FBPP, 1.000)
C
       allocate(F2(N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2,N0+1:M1))
       call reorder123456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N2,M2,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z24(N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6
       I2=K5*K8*K8*K6*K0
       I3=K0
       call DMATMAT(I1,I2,I3,X24,F2,Z24)
       deallocate(F2)
C
       call
     & sum234561(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z24, 1.000)
       deallocate(Z24)
       deallocate(X24)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,N2,N0,N1,N1,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S478(M1+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1*K2
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S478)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,N1,N1,N3,
     & N0,N2,N0,N1,N1,N3,M1,N2,S478,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,M2,N3,t2B,D2)
       allocate(S483(N2+1:M2,M2+1:N3,N1+1:N3,M1+1:N2))
       I1=K8*K3
       I2=K6*K0
       I3=K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,S483)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N1,N3,N2,M2,M2,N3,M1,N2,X4,S483, 1.000)
       deallocate(S483)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,N1,N1,N3,
     & N0,N2,N0,N1,N1,N3,M1,N2,S478,D1)
       allocate(D2(N0+1:N2,N0+1:N1,M2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,M2,N3,M2,N3,t2B,D2)
       allocate(S484(M2+1:N3,M2+1:N3,N1+1:N3,M1+1:N2))
       I1=K8*K3
       I2=K6*K6
       I3=K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,S484)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N1,N3,M2,N3,M2,N3,M1,N2,X3,S484, 1.000)
       deallocate(S484)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder3421(M1,N2,N0,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N2,M1,N2,S478,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S485(N2+1:M2,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S485)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,N2,M2,M1,N2,M1,N2,X76,S485, 1.000)
       deallocate(S485)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder3421(M1,N2,N0,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N2,M1,N2,S478,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S487(M2+1:N3,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K6
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S487)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,M2,N3,M1,N2,M1,N2,X75,S487, 1.000)
       deallocate(S487)
C
       allocate(D1(N0+1:N1,N0+1:N2,N1+1:N3,M1+1:N2))
       call reorder3241(M1,N2,N0,N2,N0,N1,N1,N3,
     & N0,N1,N0,N2,N1,N3,M1,N2,S478,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S525(M2+1:N3,N0+1:N2,N1+1:N3,M1+1:N2))
       I1=K8*K3*K2
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S525)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder2314(M2,N3,N0,N2,N1,N3,M1,N2,
     & N0,N2,N1,N3,M2,N3,M1,N2,S525,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S526(M2+1:N3,N1+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K3
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S526)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,M2,N3,M1,N2,X3,S526, 1.000)
       deallocate(S526)
C
       call sumx2341(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,M2,N3,M2,N3,M1,N2,X3,VBPHPP, 1.000)
C
       allocate(D2(N1+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,M2,M1,N2,N0,M1,t2B,D2)
       allocate(Z3(N2+1:M2,M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K6
       I2=K5*K8*K0
       I3=K3
       call DMATMAT(I1,I2,I3,X3,D2,Z3)
       deallocate(D2)
C
       call
     & sum246135(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z3,-1.000)
       call
     & sum256134(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z3, 1.000)
       deallocate(Z3)
       deallocate(X3)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder3421(M1,N2,N0,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N2,M1,N2,S478,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2A,D2)
       allocate(S479(M2+1:N3,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K6
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S479)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N2,M2,N3,M1,N2,N0,M1,X8,S479, 1.000)
       deallocate(S479)
C
       allocate(D1(N0+1:N2,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder2314(M2,N3,N0,N2,N1,N3,M1,N2,
     & N0,N2,N1,N3,M2,N3,M1,N2,S525,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S527(N2+1:M2,N1+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K3
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S527)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,N2,M2,M2,N3,M1,N2,X4,S527, 1.000)
       deallocate(S527)
C
       call sumx2341(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,N2,M2,M2,N3,M1,N2,X4,VBPHPP, 1.000)
C
       allocate(D2(N1+1:N3,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M2,N3,M1,N2,N0,M1,t2B,D2)
       allocate(Z4(M2+1:N3,M1+1:N2,N0+1:M1,N2+1:M2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K0
       I2=K5*K8*K6
       I3=K3
       call DMATMAT(I1,I2,I3,X4,D2,Z4)
       deallocate(D2)
C
       call
     & sum146235(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z4, 1.000)
       call
     & sum156234(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z4,-1.000)
       deallocate(Z4)
       deallocate(X4)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2A,D2)
       allocate(S480(M2+1:N3,N0+1:M1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K5*K6
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S480)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3412(M2,N3,N0,M1,N0,N2,N2,N3,
     & N0,N2,N2,N3,M2,N3,N0,M1,S480,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S481(M2+1:N3,N2+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K4
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S481)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,M2,N3,N0,M1,X9,S481,-1.000)
       deallocate(S481)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3412(M2,N3,N0,M1,N0,N2,N2,N3,
     & N0,N2,N2,N3,M2,N3,N0,M1,S480,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S482(N2+1:M2,N2+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K4
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S482)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,M2,N3,N0,M1,X10,S482,-1.000)
       deallocate(S482)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N1+1:N3))
       call reorder4132(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N0,N2,N1,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S489(M2+1:N3,M1+1:N2,N0+1:N2,N1+1:N3))
       I1=K3*K2
       I2=K8*K6
       I3=K4*K1
       call DMATMAT(I1,I2,I3,D1,D2,S489)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder3412(M2,N3,M1,N2,N0,N2,N1,N3,
     & N0,N2,N1,N3,M2,N3,M1,N2,S489,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S494(N2+1:M2,N1+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K3
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S494)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,N2,M2,M2,N3,M1,N2,X87,S494,-1.000)
       deallocate(S494)
C
       allocate(D1(N0+1:N2,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder3412(M2,N3,M1,N2,N0,N2,N1,N3,
     & N0,N2,N1,N3,M2,N3,M1,N2,S489,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S490(M2+1:N3,N1+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K3
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S490)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,M2,N3,M1,N2,X88,S490,-1.000)
       deallocate(S490)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N2,N0,N1,VBHHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,M1+1:N2,N0+1:M1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,M1,N2,N0,M1,t2B,D2)
       allocate(S425(M1+1:N2,N0+1:M1,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K5*K8
       I3=K3*K4
       call DMATMAT(I1,I2,I3,D1,D2,S425)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,N0+1:M1))
       call reorder3412(M1,N2,N0,M1,N0,N2,N0,N1,
     & N0,N2,N0,N1,M1,N2,N0,M1,S425,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S498(N2+1:M2,N0+1:N1,M1+1:N2,N0+1:M1))
       I1=K5*K8*K1
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S498)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N2,M2,M1,N2,N0,M1,X2,S498,-1.000)
       deallocate(S498)
C
       allocate(D1(N0+1:N1,N0+1:N2,M1+1:N2,N0+1:M1))
       call reorder4312(M1,N2,N0,M1,N0,N2,N0,N1,
     & N0,N1,N0,N2,M1,N2,N0,M1,S425,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S426(M2+1:N3,N0+1:N2,M1+1:N2,N0+1:M1))
       I1=K5*K8*K2
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S426)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,M2,N3,M1,N2,N0,M1,X8,S426,-1.000)
       deallocate(S426)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,N0+1:M1))
       call reorder3412(M1,N2,N0,M1,N0,N2,N0,N1,
     & N0,N2,N0,N1,M1,N2,N0,M1,S425,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S493(M2+1:N3,N0+1:N1,M1+1:N2,N0+1:M1))
       I1=K5*K8*K1
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S493)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,M2,N3,M1,N2,N0,M1,X1,S493,-1.000)
       deallocate(S493)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S496(N2+1:M2,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4*K1
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S496)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder2431(N2,M2,N0,N1,N2,N3,N1,N3,
     & N0,N1,N1,N3,N2,N3,N2,M2,S496,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S497(M2+1:N3,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K6
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S497)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,M2,N3,N2,M2,M1,N2,X89,S497, 1.000)
       deallocate(S497)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S491(N2+1:M2,M1+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K8*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S491)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N2,M2,M1,N2,S491,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S492(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S492)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,M1,N2,X89,S492,-1.000)
       deallocate(S492)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N1,N3,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q82(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q82)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q82,B1)
       allocate(D2(N1+1:N3,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M2,N3,M1,N2,N0,M1,t2B,D2)
       allocate(S495(M2+1:N3,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K6
       I3=K3
       call DMATMAT(I1,I2,I3,B1,D2,S495)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,M2,N3,M1,N2,N0,M1,X1,S495, 1.000)
       deallocate(S495)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q82,B1)
       allocate(D2(N1+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,M2,M1,N2,N0,M1,t2B,D2)
       allocate(S499(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K3
       call DMATMAT(I1,I2,I3,B1,D2,S499)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X2,S499, 1.000)
       deallocate(S499)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,N2,N0,N2,N2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S500(M1+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2*K2
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S500)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder2431(M1,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N0,N2,M1,N2,S500,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S503(M2+1:N3,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K6
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S503)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,M2,N3,M1,N2,M1,N2,X75,S503,-1.000)
       deallocate(S503)
C
       allocate(D2(N0+1:N2,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,M2,M2,N3,N0,M1,t2B,D2)
       allocate(Z133(N2+1:M2,M2+1:N3,N0+1:M1,M2+1:N3,M1+1:N2,M1+1:N2))
       I1=K8*K8*K6
       I2=K5*K6*K0
       I3=K2
       call DMATMAT(I1,I2,I3,X75,D2,Z133)
       deallocate(D2)
C
       call
     & sum236145(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z133,-1.000)
       call
     & sum236154(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z133, 1.000)
       deallocate(Z133)
       deallocate(X75)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder3421(M1,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N0,N2,M1,N2,S500,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S504(M2+1:N3,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K6
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S504)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N2,M2,N3,M1,N2,N0,M1,X8,S504, 1.000)
       deallocate(S504)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,M1,N2,S500,D1)
       allocate(D2(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,N2,M2,t2C,D2)
       allocate(S501(M2+1:N3,N2+1:M2,N2+1:N3,M1+1:N2))
       I1=K8*K4
       I2=K0*K6
       I3=K2*K2
       call DMATMAT(I1,I2,I3,D1,D2,S501)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N2,N3,M2,N3,N2,M2,M1,N2,X7,S501,-0.500)
       deallocate(S501)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,M1+1:N2))
       call reorder4231(M1,N2,N0,N2,N0,N2,N2,N3,
     & N2,N3,N0,N2,N0,N2,M1,N2,S500,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S528(M1+1:N2,N0+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2*K2
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S528)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder2314(M1,N2,N0,N2,N0,N2,M1,N2,
     & N0,N2,N0,N2,M1,N2,M1,N2,S528,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S529(M2+1:N3,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S529)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,M2,N3,M1,N2,M1,N2,X5,S529, 1.000)
       deallocate(S529)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder3241(M1,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,M1,N2,S500,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S531(N2+1:M2,N0+1:N2,N2+1:N3,M1+1:N2))
       I1=K8*K4*K2
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S531)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder2314(N2,M2,N0,N2,N2,N3,M1,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,S531,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S532(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S532)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,M1,N2,X7,S532,-1.000)
       deallocate(S532)
C
       call sumx2314(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,M2,N3,N2,M2,M1,N2,X7,VCHPPP, 1.000)
C
       allocate(D2(N2+1:N3,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,M2,N3,M1,N2,N0,M1,t2B,D2)
       allocate(Z7(M2+1:N3,M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K6
       I2=K5*K8*K6
       I3=K4
       call DMATMAT(I1,I2,I3,X7,D2,Z7)
       deallocate(D2)
C
       call
     & sum346125(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z7, 1.000)
       call
     & sum356124(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z7,-1.000)
       deallocate(Z7)
       deallocate(X7)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder2431(M1,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N0,N2,M1,N2,S500,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S502(N2+1:M2,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S502)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,N2,M2,M1,N2,M1,N2,X76,S502,-1.000)
       deallocate(S502)
C
       allocate(D2(N0+1:N2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,M2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(Z134(M2+1:N3,M2+1:N3,N0+1:M1,N2+1:M2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K0
       I2=K5*K6*K6
       I3=K2
       call DMATMAT(I1,I2,I3,X76,D2,Z134)
       deallocate(D2)
C
       call
     & sum136245(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z134, 1.000)
       call
     & sum136254(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z134,-1.000)
       deallocate(Z134)
       deallocate(X76)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder2314(M1,N2,N0,N2,N0,N2,M1,N2,
     & N0,N2,N0,N2,M1,N2,M1,N2,S528,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S530(N2+1:M2,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S530)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,N2,M2,M1,N2,M1,N2,X6,S530, 1.000)
       deallocate(S530)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S512(M2+1:N3,N0+1:M1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K5*K6
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S512)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3412(M2,N3,N0,M1,N0,N2,N2,N3,
     & N0,N2,N2,N3,M2,N3,N0,M1,S512,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S513(M2+1:N3,N2+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K4
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S513)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,M2,N3,N0,M1,X9,S513,-1.000)
       deallocate(S513)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder4312(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S507(N2+1:M2,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4*K2
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S507)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder2431(N2,M2,N0,N2,N2,N3,N2,N3,
     & N0,N2,N2,N3,N2,N3,N2,M2,S507,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S516(M2+1:N3,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K6
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S516)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,N3,N2,M2,M2,N3,N0,M1,X10,S516, 1.000)
       deallocate(S516)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder2341(N2,M2,N0,N2,N2,N3,N2,N3,
     & N0,N2,N2,N3,N2,N3,N2,M2,S507,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S508(M2+1:N3,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K6
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S508)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,M2,N3,N2,M2,M1,N2,X89,S508, 1.000)
       deallocate(S508)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:M2))
       call reorder3421(N2,M2,N0,N2,N2,N3,N2,N3,
     & N2,N3,N2,N3,N0,N2,N2,M2,S507,D1)
       allocate(D2(N2+1:N3,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,M1,N2,M1,N2,t2C,D2)
       allocate(S514(M1+1:N2,M1+1:N2,N0+1:N2,N2+1:M2))
       I1=K0*K2
       I2=K8*K8
       I3=K4*K4
       call DMATMAT(I1,I2,I3,D1,D2,S514)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,N2,N2,M2,M1,N2,M1,N2,X6,S514, 0.500)
       deallocate(S514)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N2,N3,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q83(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q83)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q83,B1)
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,M1,N2,M1,N2,t2C,D2)
       allocate(S515(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K4
       call DMATMAT(I1,I2,I3,B1,D2,S515)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X6,S515,-1.000)
       deallocate(S515)
C
       call sumx2134(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,M2,M1,N2,M1,N2,X6,VCHHHP, 1.000)
C
       allocate(D2(N0+1:N2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,M2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(Z6(M2+1:N3,M2+1:N3,N0+1:M1,N2+1:M2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K0
       I2=K5*K6*K6
       I3=K2
       call DMATMAT(I1,I2,I3,X6,D2,Z6)
       deallocate(D2)
C
       call
     & sum136245(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z6, 1.000)
       deallocate(Z6)
       deallocate(X6)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q83,B1)
       allocate(D2(N2+1:N3,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,M2,N3,M1,N2,N0,M1,t2B,D2)
       allocate(S509(M2+1:N3,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K6
       I3=K4
       call DMATMAT(I1,I2,I3,B1,D2,S509)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,N0,M1,X8,S509, 1.000)
       deallocate(S509)
C
       call sumx1234(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,M2,N3,M1,N2,N0,M1,X8,VBHHPH, 1.000)
C
       allocate(D2(N0+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,M2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(Z8(M2+1:N3,N2+1:M2,M1+1:N2,M2+1:N3,M1+1:N2,N0+1:M1))
       I1=K5*K8*K6
       I2=K8*K0*K6
       I3=K2
       call DMATMAT(I1,I2,I3,X8,D2,Z8)
       deallocate(D2)
C
       call
     & sum124356(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z8, 1.000)
       call
     & sum125346(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z8,-1.000)
       deallocate(Z8)
       deallocate(X8)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q83,B1)
       allocate(D2(N2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,M2,N3,M1,N2,M1,N2,t2C,D2)
       allocate(S517(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K4
       call DMATMAT(I1,I2,I3,B1,D2,S517)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X5,S517,-1.000)
       deallocate(S517)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder4132(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S505(N2+1:M2,M1+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K8*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S505)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N2,M2,M1,N2,S505,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S506(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S506)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,M1,N2,X89,S506, 1.000)
       deallocate(S506)
C
       allocate(D2(N2+1:N3,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,M2,N3,M1,N2,N0,M1,t2B,D2)
       allocate(Z235(M2+1:N3,M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K6
       I2=K5*K8*K6
       I3=K4
       call DMATMAT(I1,I2,I3,X89,D2,Z235)
       deallocate(D2)
C
       call
     & sum356124(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z235,-1.000)
       call
     & sum346125(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z235, 1.000)
       deallocate(Z235)
       deallocate(X89)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,N2+1:N3))
       call reorder3241(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N0,N1,N2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S431(N2+1:M2,N0+1:M1,N0+1:N1,N2+1:N3))
       I1=K4*K1
       I2=K5*K0
       I3=K3*K2
       call DMATMAT(I1,I2,I3,D1,D2,S431)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:M2,N0+1:M1))
       call reorder4312(N2,M2,N0,M1,N0,N1,N2,N3,
     & N2,N3,N0,N1,N2,M2,N0,M1,S431,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S488(M1+1:N2,N0+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K1
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S488)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N1,N2,M2,M1,N2,N0,M1,X2,S488,-1.000)
       deallocate(S488)
C
       call sumx2134(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N2,M2,M1,N2,N0,M1,X2,VBHHHP, 1.000)
C
       allocate(D2(N0+1:N1,M2+1:N3,M2+1:N3,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,M2,N3,M2,N3,M1,N2,t2B,D2)
       allocate(Z2(M2+1:N3,M2+1:N3,M1+1:N2,N2+1:M2,M1+1:N2,N0+1:M1))
       I1=K5*K8*K0
       I2=K8*K6*K6
       I3=K1
       call DMATMAT(I1,I2,I3,X2,D2,Z2)
       deallocate(D2)
C
       call
     & sum134256(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z2,-1.000)
       call
     & sum135246(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z2, 1.000)
       deallocate(Z2)
       deallocate(X2)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3412(N2,M2,N0,M1,N0,N1,N2,N3,
     & N0,N1,N2,N3,N2,M2,N0,M1,S431,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S432(M2+1:N3,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S432)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,N3,N2,M2,M2,N3,N0,M1,X10,S432, 1.000)
       deallocate(S432)
C
       call sumx2314(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N2,M2,M2,N3,N0,M1,X10,VBHPPP, 1.000)
C
       allocate(D2(N2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,M2,N3,M1,N2,M1,N2,t2C,D2)
       allocate(Z10(M2+1:N3,M1+1:N2,M1+1:N2,N2+1:M2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K0
       I2=K8*K8*K6
       I3=K4
       call DMATMAT(I1,I2,I3,X10,D2,Z10)
       deallocate(D2)
C
       call
     & sum145236(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z10,-1.000)
       deallocate(Z10)
       deallocate(X10)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,N2+1:N3))
       call reorder3241(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N0,N1,N2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S428(M2+1:N3,N0+1:M1,N0+1:N1,N2+1:N3))
       I1=K4*K1
       I2=K5*K6
       I3=K3*K2
       call DMATMAT(I1,I2,I3,D1,D2,S428)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N0+1:N1,M2+1:N3,N0+1:M1))
       call reorder4312(M2,N3,N0,M1,N0,N1,N2,N3,
     & N2,N3,N0,N1,M2,N3,N0,M1,S428,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S486(M1+1:N2,N0+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K1
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S486)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N1,M2,N3,M1,N2,N0,M1,X1,S486,-1.000)
       deallocate(S486)
C
       call sumx2134(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,M2,N3,M1,N2,N0,M1,X1,VBHHHP, 1.000)
C
       allocate(D2(N0+1:N1,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,M2,M2,N3,M1,N2,t2B,D2)
       allocate(Z1(N2+1:M2,M2+1:N3,M1+1:N2,M2+1:N3,M1+1:N2,N0+1:M1))
       I1=K5*K8*K6
       I2=K8*K6*K0
       I3=K1
       call DMATMAT(I1,I2,I3,X1,D2,Z1)
       deallocate(D2)
C
       call
     & sum234156(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z1, 1.000)
       call
     & sum235146(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z1,-1.000)
       deallocate(Z1)
       deallocate(X1)
C
       allocate(D1(N0+1:N1,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3412(M2,N3,N0,M1,N0,N1,N2,N3,
     & N0,N1,N2,N3,M2,N3,N0,M1,S428,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S429(M2+1:N3,N2+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K4
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S429)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,N3,M2,N3,M2,N3,N0,M1,X9,S429, 1.000)
       deallocate(S429)
C
       call sumx2314(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,M2,N3,M2,N3,N0,M1,X9,VBHPPP, 1.000)
C
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,M1,N2,M1,N2,t2C,D2)
       allocate(Z9(N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K6
       I2=K8*K8*K0
       I3=K4
       call DMATMAT(I1,I2,I3,X9,D2,Z9)
       deallocate(D2)
C
       call
     & sum245136(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z9, 1.000)
       deallocate(Z9)
       deallocate(X9)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N1,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S421(N2+1:M2,M1+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K8*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S421)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N2,M2,M1,N2,S421,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S422(M2+1:N3,N1+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K3
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S422)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,N3,N2,M2,M2,N3,M1,N2,X87,S422, 1.000)
       deallocate(S422)
C
       allocate(D2(N1+1:N3,M2+1:N3,M1+1:N2,N0+1:M1))
       call reorder2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M2,N3,M1,N2,N0,M1,t2B,D2)
       allocate(Z226(M2+1:N3,M1+1:N2,N0+1:M1,N2+1:M2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K0
       I2=K5*K8*K6
       I3=K3
       call DMATMAT(I1,I2,I3,X87,D2,Z226)
       deallocate(D2)
C
       call
     & sum156234(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z226, 1.000)
       call
     & sum146235(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z226,-1.000)
       deallocate(Z226)
       deallocate(X87)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N1,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S423(M2+1:N3,M1+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K8*K6
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S423)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder3412(M2,N3,M1,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,M2,N3,M1,N2,S423,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S424(M2+1:N3,N1+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K3
       I2=K6
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S424)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,N3,M2,N3,M2,N3,M1,N2,X88,S424, 1.000)
       deallocate(S424)
C
       allocate(D2(N1+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,M2,M1,N2,N0,M1,t2B,D2)
       allocate(Z227(N2+1:M2,M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K6
       I2=K5*K8*K0
       I3=K3
       call DMATMAT(I1,I2,I3,X88,D2,Z227)
       deallocate(D2)
C
       call
     & sum256134(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z227,-1.000)
       call
     & sum246135(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z227, 1.000)
       deallocate(Z227)
       deallocate(X88)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,VCHHPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,M1,N2,M1,N2,t2C,D2)
       allocate(S510(M1+1:N2,M1+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K8*K8
       I3=K4*K4
       call DMATMAT(I1,I2,I3,D1,D2,S510)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder3412(M1,N2,M1,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,M1,N2,M1,N2,S510,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S511(M2+1:N3,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K6
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S511)
       deallocate(D1)
       deallocate(B2)
       deallocate(S510)
C
       call
     & sum2134(N0,N2,M2,N3,M1,N2,M1,N2,X5,S511,-0.500)
       deallocate(S511)
C
       call sumx2134(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,M2,N3,M1,N2,M1,N2,X5,VCHHHP, 1.000)
C
       allocate(D2(N0+1:N2,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,M2,M2,N3,N0,M1,t2B,D2)
       allocate(Z5(N2+1:M2,M2+1:N3,N0+1:M1,M2+1:N3,M1+1:N2,M1+1:N2))
       I1=K8*K8*K6
       I2=K5*K6*K0
       I3=K2
       call DMATMAT(I1,I2,I3,X5,D2,Z5)
       deallocate(D2)
C
       call
     & sum236145(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z5,-1.000)
       deallocate(Z5)
       deallocate(X5)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,M1,VBHPPH,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z60(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K6
       I3=K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z60)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z60, 1.000)
       deallocate(Z60)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N2,M2,N3,M2,N3,N0,M1,VBHPPH,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z61(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K6
       I3=K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z61)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z61, 1.000)
       deallocate(Z61)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,N2,M2,M2,N3,N0,M1,VBHPPH,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z62(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K6
       I3=K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z62)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z62,-1.000)
       deallocate(Z62)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N2,N2,M2,M2,N3,N0,M1,VBHPPH,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z63(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K6
       I3=K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z63)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,V3C,Z63,-1.000)
       deallocate(Z63)
C
       call sumx3(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,N0,M1,HT3C1,V3C,1.0)
       deallocate(V3C)
C
       end
