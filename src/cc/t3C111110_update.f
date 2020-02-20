       subroutine t3C111110_update(N0,N1,N2,N3,HT3C1,shift,
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
       real*8,allocatable::S10(:,:,:,:)
       real*8,allocatable::Q1(:,:)
       real*8,allocatable::Q2(:,:)
       real*8,allocatable::Q3(:,:)
       real*8,allocatable::Q4(:,:)
       real*8,allocatable::S11(:,:,:,:)
       real*8,allocatable::S12(:,:,:,:)
       real*8,allocatable::S13(:,:,:,:)
       real*8,allocatable::S14(:,:,:,:)
       real*8,allocatable::Q5(:,:)
       real*8,allocatable::Q6(:,:)
       real*8,allocatable::S15(:,:,:,:)
       real*8,allocatable::S16(:,:,:,:)
       real*8,allocatable::S17(:,:,:,:)
       real*8,allocatable::S18(:,:,:,:)
       real*8,allocatable::Q7(:,:)
       real*8,allocatable::Q8(:,:)
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
       real*8,allocatable::Q9(:,:)
       real*8,allocatable::Q10(:,:)
       real*8,allocatable::S35(:,:,:,:)
       real*8,allocatable::S36(:,:,:,:)
       real*8,allocatable::S37(:,:,:,:)
       real*8,allocatable::S38(:,:,:,:)
       real*8,allocatable::Q11(:,:)
       real*8,allocatable::Q12(:,:)
       real*8,allocatable::S39(:,:,:,:)
       real*8,allocatable::S40(:,:,:,:)
       real*8,allocatable::S41(:,:,:,:)
       real*8,allocatable::Q13(:,:)
       real*8,allocatable::Q14(:,:)
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
       real*8,allocatable::S75(:,:,:,:)
       real*8,allocatable::S76(:,:,:,:)
       real*8,allocatable::S77(:,:,:,:)
       real*8,allocatable::S78(:,:,:,:)
       real*8,allocatable::S79(:,:,:,:)
       real*8,allocatable::S80(:,:,:,:)
       real*8,allocatable::S81(:,:,:,:)
       real*8,allocatable::S82(:,:,:,:)
       real*8,allocatable::S83(:,:,:,:)
       real*8,allocatable::Q15(:,:)
       real*8,allocatable::Q16(:,:)
       real*8,allocatable::Q17(:,:)
       real*8,allocatable::Q18(:,:)
       real*8,allocatable::Q19(:,:)
       real*8,allocatable::Q20(:,:)
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
       real*8,allocatable::Q21(:,:)
       real*8,allocatable::Q22(:,:)
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
       real*8,allocatable::Q23(:,:)
       real*8,allocatable::Q24(:,:)
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
       real*8,allocatable::Q25(:,:)
       real*8,allocatable::Q26(:,:)
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
       real*8,allocatable::Q27(:,:)
       real*8,allocatable::Q28(:,:)
       real*8,allocatable::S129(:,:,:,:)
       real*8,allocatable::S130(:,:,:,:)
       real*8,allocatable::S131(:,:,:,:)
       real*8,allocatable::Q29(:,:)
       real*8,allocatable::Q30(:,:)
       real*8,allocatable::S132(:,:,:,:)
       real*8,allocatable::S133(:,:,:,:)
       real*8,allocatable::S134(:,:,:,:)
       real*8,allocatable::S135(:,:,:,:)
       real*8,allocatable::S136(:,:,:,:)
       real*8,allocatable::S137(:,:,:,:)
       real*8,allocatable::S138(:,:,:,:)
       real*8,allocatable::Q31(:,:)
       real*8,allocatable::Q32(:,:)
       real*8,allocatable::Q33(:,:)
       real*8,allocatable::Q34(:,:)
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
       real*8,allocatable::S262(:,:,:,:)
       real*8,allocatable::S263(:,:,:,:)
       real*8,allocatable::S264(:,:,:,:)
       real*8,allocatable::S265(:,:,:,:)
       real*8,allocatable::S266(:,:,:,:)
       real*8,allocatable::S267(:,:,:,:)
       real*8,allocatable::Q35(:,:)
       real*8,allocatable::Q36(:,:)
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
       real*8,allocatable::Q37(:,:)
       real*8,allocatable::Q38(:,:)
       real*8,allocatable::S294(:,:,:,:)
       real*8,allocatable::S295(:,:,:,:)
       real*8,allocatable::S296(:,:,:,:)
       real*8,allocatable::S297(:,:,:,:)
       real*8,allocatable::Q39(:,:)
       real*8,allocatable::Q40(:,:)
       real*8,allocatable::S298(:,:,:,:)
       real*8,allocatable::S299(:,:,:,:)
       real*8,allocatable::S300(:,:,:,:)
       real*8,allocatable::Q41(:,:)
       real*8,allocatable::Q42(:,:)
       real*8,allocatable::Q43(:,:)
       real*8,allocatable::Q44(:,:)
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
       real*8,allocatable::S327(:,:,:,:)
       real*8,allocatable::S328(:,:,:,:)
       real*8,allocatable::S329(:,:,:,:)
       real*8,allocatable::S330(:,:,:,:)
       real*8,allocatable::S331(:,:,:,:)
       real*8,allocatable::S332(:,:,:,:)
       real*8,allocatable::S333(:,:,:,:)
       real*8,allocatable::S334(:,:,:,:)
       real*8,allocatable::S335(:,:,:,:)
       real*8,allocatable::S336(:,:,:,:)
       real*8,allocatable::S337(:,:,:,:)
       real*8,allocatable::S338(:,:,:,:)
       real*8,allocatable::S339(:,:,:,:)
       real*8,allocatable::S340(:,:,:,:)
       real*8,allocatable::S341(:,:,:,:)
       real*8,allocatable::S342(:,:,:,:)
       real*8,allocatable::S343(:,:,:,:)
       real*8,allocatable::S344(:,:,:,:)
       real*8,allocatable::S345(:,:,:,:)
       real*8,allocatable::S346(:,:,:,:)
       real*8,allocatable::S347(:,:,:,:)
       real*8,allocatable::S348(:,:,:,:)
       real*8,allocatable::S349(:,:,:,:)
       real*8,allocatable::S350(:,:,:,:)
       real*8,allocatable::S351(:,:,:,:)
       real*8,allocatable::S352(:,:,:,:)
       real*8,allocatable::S353(:,:,:,:)
       real*8,allocatable::S354(:,:,:,:)
       real*8,allocatable::S355(:,:,:,:)
       real*8,allocatable::S356(:,:,:,:)
       real*8,allocatable::S357(:,:,:,:)
       real*8,allocatable::S358(:,:,:,:)
       real*8,allocatable::S359(:,:,:,:)
       real*8,allocatable::S360(:,:,:,:)
       real*8,allocatable::S361(:,:,:,:)
       real*8,allocatable::S362(:,:,:,:)
       real*8,allocatable::S363(:,:,:,:)
       real*8,allocatable::S364(:,:,:,:)
       real*8,allocatable::S365(:,:,:,:)
       real*8,allocatable::S366(:,:,:,:)
       real*8,allocatable::S367(:,:,:,:)
       real*8,allocatable::S368(:,:,:,:)
       real*8,allocatable::S369(:,:,:,:)
       real*8,allocatable::S370(:,:,:,:)
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
       real*8,allocatable::Q45(:,:)
       real*8,allocatable::Q46(:,:)
       real*8,allocatable::S382(:,:,:,:)
       real*8,allocatable::S383(:,:,:,:)
       real*8,allocatable::S384(:,:,:,:)
       real*8,allocatable::Q47(:,:)
       real*8,allocatable::Q48(:,:)
       real*8,allocatable::Q49(:,:)
       real*8,allocatable::Q50(:,:)
       real*8,allocatable::S385(:,:,:,:)
       real*8,allocatable::S407(:,:,:,:)
       real*8,allocatable::S408(:,:,:,:)
       real*8,allocatable::S386(:,:,:,:)
       real*8,allocatable::S391(:,:,:,:)
       real*8,allocatable::S392(:,:,:,:)
       real*8,allocatable::S393(:,:,:,:)
       real*8,allocatable::S394(:,:,:,:)
       real*8,allocatable::S395(:,:,:,:)
       real*8,allocatable::S396(:,:,:,:)
       real*8,allocatable::S397(:,:,:,:)
       real*8,allocatable::S398(:,:,:,:)
       real*8,allocatable::Q51(:,:)
       real*8,allocatable::Q52(:,:)
       real*8,allocatable::Q53(:,:)
       real*8,allocatable::Q54(:,:)
       real*8,allocatable::Q55(:,:)
       real*8,allocatable::Q56(:,:)
       real*8,allocatable::Q57(:,:)
       real*8,allocatable::Q58(:,:)
       real*8,allocatable::S399(:,:,:,:)
       real*8,allocatable::S400(:,:,:,:)
       real*8,allocatable::S401(:,:,:,:)
       real*8,allocatable::S402(:,:,:,:)
       real*8,allocatable::S403(:,:,:,:)
       real*8,allocatable::S404(:,:,:,:)
       real*8,allocatable::S405(:,:,:,:)
       real*8,allocatable::S406(:,:,:,:)
       real*8,allocatable::S387(:,:,:,:)
       real*8,allocatable::S409(:,:,:,:)
       real*8,allocatable::S388(:,:,:,:)
       real*8,allocatable::S389(:,:,:,:)
       real*8,allocatable::S410(:,:,:,:)
       real*8,allocatable::S390(:,:,:,:)
       real*8,allocatable::S411(:,:,:,:)
       real*8,allocatable::S413(:,:,:,:)
       real*8,allocatable::S412(:,:,:,:)
       real*8,allocatable::S414(:,:,:,:)
       real*8,allocatable::S415(:,:,:,:)
       real*8,allocatable::S416(:,:,:,:)
       real*8,allocatable::S417(:,:,:,:)
       real*8,allocatable::S418(:,:,:,:)
       real*8,allocatable::S491(:,:,:,:)
       real*8,allocatable::S492(:,:,:,:)
       real*8,allocatable::S419(:,:,:,:)
       real*8,allocatable::S423(:,:,:,:)
       real*8,allocatable::S425(:,:,:,:)
       real*8,allocatable::S426(:,:,:,:)
       real*8,allocatable::S424(:,:,:,:)
       real*8,allocatable::S427(:,:,:,:)
       real*8,allocatable::S428(:,:,:,:)
       real*8,allocatable::S429(:,:,:,:)
       real*8,allocatable::S430(:,:,:,:)
       real*8,allocatable::S431(:,:,:,:)
       real*8,allocatable::S432(:,:,:,:)
       real*8,allocatable::S433(:,:,:,:)
       real*8,allocatable::S434(:,:,:,:)
       real*8,allocatable::S435(:,:,:,:)
       real*8,allocatable::S443(:,:,:,:)
       real*8,allocatable::S436(:,:,:,:)
       real*8,allocatable::S439(:,:,:,:)
       real*8,allocatable::S445(:,:,:,:)
       real*8,allocatable::S440(:,:,:,:)
       real*8,allocatable::S437(:,:,:,:)
       real*8,allocatable::S444(:,:,:,:)
       real*8,allocatable::S438(:,:,:,:)
       real*8,allocatable::S441(:,:,:,:)
       real*8,allocatable::S446(:,:,:,:)
       real*8,allocatable::S442(:,:,:,:)
       real*8,allocatable::Q59(:,:)
       real*8,allocatable::Q60(:,:)
       real*8,allocatable::Q61(:,:)
       real*8,allocatable::Q62(:,:)
       real*8,allocatable::S447(:,:,:,:)
       real*8,allocatable::S448(:,:,:,:)
       real*8,allocatable::S449(:,:,:,:)
       real*8,allocatable::S450(:,:,:,:)
       real*8,allocatable::S451(:,:,:,:)
       real*8,allocatable::S452(:,:,:,:)
       real*8,allocatable::S453(:,:,:,:)
       real*8,allocatable::S454(:,:,:,:)
       real*8,allocatable::Q63(:,:)
       real*8,allocatable::Q64(:,:)
       real*8,allocatable::Q65(:,:)
       real*8,allocatable::Q66(:,:)
       real*8,allocatable::S455(:,:,:,:)
       real*8,allocatable::S456(:,:,:,:)
       real*8,allocatable::S457(:,:,:,:)
       real*8,allocatable::S463(:,:,:,:)
       real*8,allocatable::S458(:,:,:,:)
       real*8,allocatable::S461(:,:,:,:)
       real*8,allocatable::S465(:,:,:,:)
       real*8,allocatable::S462(:,:,:,:)
       real*8,allocatable::Q69(:,:)
       real*8,allocatable::Q72(:,:)
       real*8,allocatable::Q70(:,:)
       real*8,allocatable::S459(:,:,:,:)
       real*8,allocatable::S460(:,:,:,:)
       real*8,allocatable::S464(:,:,:,:)
       real*8,allocatable::Q67(:,:)
       real*8,allocatable::Q68(:,:)
       real*8,allocatable::Q71(:,:)
       real*8,allocatable::Q73(:,:)
       real*8,allocatable::Q74(:,:)
       real*8,allocatable::Q75(:,:)
       real*8,allocatable::Q76(:,:)
       real*8,allocatable::S466(:,:,:,:)
       real*8,allocatable::S468(:,:,:,:)
       real*8,allocatable::S467(:,:,:,:)
       real*8,allocatable::S469(:,:,:,:)
       real*8,allocatable::S470(:,:,:,:)
       real*8,allocatable::Q77(:,:)
       real*8,allocatable::S474(:,:,:,:)
       real*8,allocatable::S471(:,:,:,:)
       real*8,allocatable::S472(:,:,:,:)
       real*8,allocatable::S473(:,:,:,:)
       real*8,allocatable::S475(:,:,:,:)
       real*8,allocatable::S477(:,:,:,:)
       real*8,allocatable::S478(:,:,:,:)
       real*8,allocatable::S479(:,:,:,:)
       real*8,allocatable::S480(:,:,:,:)
       real*8,allocatable::S584(:,:,:,:)
       real*8,allocatable::S585(:,:,:,:)
       real*8,allocatable::S586(:,:,:,:)
       real*8,allocatable::S587(:,:,:,:)
       real*8,allocatable::S476(:,:,:,:)
       real*8,allocatable::S589(:,:,:,:)
       real*8,allocatable::S590(:,:,:,:)
       real*8,allocatable::S588(:,:,:,:)
       real*8,allocatable::Q78(:,:)
       real*8,allocatable::S490(:,:,:,:)
       real*8,allocatable::S486(:,:,:,:)
       real*8,allocatable::S488(:,:,:,:)
       real*8,allocatable::S420(:,:,:,:)
       real*8,allocatable::S422(:,:,:,:)
       real*8,allocatable::S421(:,:,:,:)
       real*8,allocatable::S493(:,:,:,:)
       real*8,allocatable::S495(:,:,:,:)
       real*8,allocatable::S494(:,:,:,:)
       real*8,allocatable::S496(:,:,:,:)
       real*8,allocatable::S498(:,:,:,:)
       real*8,allocatable::S497(:,:,:,:)
       real*8,allocatable::S499(:,:,:,:)
       real*8,allocatable::S500(:,:,:,:)
       real*8,allocatable::S501(:,:,:,:)
       real*8,allocatable::S505(:,:,:,:)
       real*8,allocatable::S502(:,:,:,:)
       real*8,allocatable::S503(:,:,:,:)
       real*8,allocatable::S506(:,:,:,:)
       real*8,allocatable::S504(:,:,:,:)
       real*8,allocatable::S507(:,:,:,:)
       real*8,allocatable::S515(:,:,:,:)
       real*8,allocatable::S508(:,:,:,:)
       real*8,allocatable::S511(:,:,:,:)
       real*8,allocatable::S517(:,:,:,:)
       real*8,allocatable::S512(:,:,:,:)
       real*8,allocatable::S509(:,:,:,:)
       real*8,allocatable::S516(:,:,:,:)
       real*8,allocatable::S510(:,:,:,:)
       real*8,allocatable::S513(:,:,:,:)
       real*8,allocatable::S518(:,:,:,:)
       real*8,allocatable::S514(:,:,:,:)
       real*8,allocatable::S519(:,:,:,:)
       real*8,allocatable::S520(:,:,:,:)
       real*8,allocatable::S521(:,:,:,:)
       real*8,allocatable::S522(:,:,:,:)
       real*8,allocatable::S523(:,:,:,:)
       real*8,allocatable::S524(:,:,:,:)
       real*8,allocatable::S525(:,:,:,:)
       real*8,allocatable::S533(:,:,:,:)
       real*8,allocatable::S526(:,:,:,:)
       real*8,allocatable::S529(:,:,:,:)
       real*8,allocatable::S535(:,:,:,:)
       real*8,allocatable::S530(:,:,:,:)
       real*8,allocatable::S527(:,:,:,:)
       real*8,allocatable::S534(:,:,:,:)
       real*8,allocatable::S528(:,:,:,:)
       real*8,allocatable::S531(:,:,:,:)
       real*8,allocatable::S536(:,:,:,:)
       real*8,allocatable::S532(:,:,:,:)
       real*8,allocatable::Q79(:,:)
       real*8,allocatable::Q80(:,:)
       real*8,allocatable::Q81(:,:)
       real*8,allocatable::Q82(:,:)
       real*8,allocatable::S537(:,:,:,:)
       real*8,allocatable::S538(:,:,:,:)
       real*8,allocatable::S539(:,:,:,:)
       real*8,allocatable::S540(:,:,:,:)
       real*8,allocatable::S541(:,:,:,:)
       real*8,allocatable::S542(:,:,:,:)
       real*8,allocatable::Q83(:,:)
       real*8,allocatable::Q87(:,:)
       real*8,allocatable::Q84(:,:)
       real*8,allocatable::Q85(:,:)
       real*8,allocatable::Q88(:,:)
       real*8,allocatable::Q86(:,:)
       real*8,allocatable::S543(:,:,:,:)
       real*8,allocatable::S548(:,:,:,:)
       real*8,allocatable::S549(:,:,:,:)
       real*8,allocatable::S550(:,:,:,:)
       real*8,allocatable::S551(:,:,:,:)
       real*8,allocatable::S552(:,:,:,:)
       real*8,allocatable::S553(:,:,:,:)
       real*8,allocatable::S544(:,:,:,:)
       real*8,allocatable::S545(:,:,:,:)
       real*8,allocatable::S547(:,:,:,:)
       real*8,allocatable::S546(:,:,:,:)
       real*8,allocatable::S554(:,:,:,:)
       real*8,allocatable::S560(:,:,:,:)
       real*8,allocatable::S555(:,:,:,:)
       real*8,allocatable::S558(:,:,:,:)
       real*8,allocatable::S564(:,:,:,:)
       real*8,allocatable::S559(:,:,:,:)
       real*8,allocatable::Q89(:,:)
       real*8,allocatable::S565(:,:,:,:)
       real*8,allocatable::S561(:,:,:,:)
       real*8,allocatable::S556(:,:,:,:)
       real*8,allocatable::S557(:,:,:,:)
       real*8,allocatable::S562(:,:,:,:)
       real*8,allocatable::S563(:,:,:,:)
       real*8,allocatable::S566(:,:,:,:)
       real*8,allocatable::S568(:,:,:,:)
       real*8,allocatable::S569(:,:,:,:)
       real*8,allocatable::S570(:,:,:,:)
       real*8,allocatable::S594(:,:,:,:)
       real*8,allocatable::S595(:,:,:,:)
       real*8,allocatable::S597(:,:,:,:)
       real*8,allocatable::S598(:,:,:,:)
       real*8,allocatable::S567(:,:,:,:)
       real*8,allocatable::S596(:,:,:,:)
       real*8,allocatable::S576(:,:,:,:)
       real*8,allocatable::S577(:,:,:,:)
       real*8,allocatable::S578(:,:,:,:)
       real*8,allocatable::S579(:,:,:,:)
       real*8,allocatable::S573(:,:,:,:)
       real*8,allocatable::S582(:,:,:,:)
       real*8,allocatable::S574(:,:,:,:)
       real*8,allocatable::S580(:,:,:,:)
       real*8,allocatable::Q90(:,:)
       real*8,allocatable::S581(:,:,:,:)
       real*8,allocatable::S575(:,:,:,:)
       real*8,allocatable::S583(:,:,:,:)
       real*8,allocatable::S483(:,:,:,:)
       real*8,allocatable::S489(:,:,:,:)
       real*8,allocatable::S487(:,:,:,:)
       real*8,allocatable::S591(:,:,:,:)
       real*8,allocatable::S592(:,:,:,:)
       real*8,allocatable::S484(:,:,:,:)
       real*8,allocatable::S485(:,:,:,:)
       real*8,allocatable::S593(:,:,:,:)
       real*8,allocatable::S481(:,:,:,:)
       real*8,allocatable::S482(:,:,:,:)
       real*8,allocatable::S571(:,:,:,:)
       real*8,allocatable::S572(:,:,:,:)
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
       real*8,allocatable::X17(:,:,:,:)
       real*8,allocatable::Z17(:,:,:,:,:,:)
       real*8,allocatable::X18(:,:,:,:)
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
       real*8,allocatable::X26(:,:)
       real*8,allocatable::Z26(:,:,:,:,:,:)
       real*8,allocatable::X27(:,:)
       real*8,allocatable::Z27(:,:,:,:,:,:)
       real*8,allocatable::X28(:,:)
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
       real*8,allocatable::X60(:,:,:,:)
       real*8,allocatable::Z60(:,:,:,:,:,:)
       real*8,allocatable::X61(:,:,:,:)
       real*8,allocatable::Z61(:,:,:,:,:,:)
       real*8,allocatable::X62(:,:,:,:)
       real*8,allocatable::Z62(:,:,:,:,:,:)
       real*8,allocatable::X63(:,:,:,:)
       real*8,allocatable::Z63(:,:,:,:,:,:)
       real*8,allocatable::X64(:,:,:,:)
       real*8,allocatable::Z64(:,:,:,:,:,:)
       real*8,allocatable::X65(:,:,:,:)
       real*8,allocatable::Z65(:,:,:,:,:,:)
       real*8,allocatable::X66(:,:,:,:)
       real*8,allocatable::Z66(:,:,:,:,:,:)
       real*8,allocatable::X67(:,:,:,:)
       real*8,allocatable::Z67(:,:,:,:,:,:)
       real*8,allocatable::X68(:,:,:,:)
       real*8,allocatable::Z68(:,:,:,:,:,:)
       real*8,allocatable::X69(:,:,:,:)
       real*8,allocatable::Z69(:,:,:,:,:,:)
       real*8,allocatable::Z70(:,:,:,:,:,:)
       real*8,allocatable::Z71(:,:,:,:,:,:)
       real*8,allocatable::Z72(:,:,:,:,:,:)
       real*8,allocatable::Z73(:,:,:,:,:,:)
       real*8,allocatable::Z88(:,:,:,:,:,:)
       real*8,allocatable::Z89(:,:,:,:,:,:)
       real*8,allocatable::Z90(:,:,:,:,:,:)
       real*8,allocatable::Z91(:,:,:,:,:,:)
       real*8,allocatable::Z94(:,:,:,:,:,:)
       real*8,allocatable::Z95(:,:,:,:,:,:)
       real*8,allocatable::Z96(:,:,:,:,:,:)
       real*8,allocatable::Z97(:,:,:,:,:,:)
       real*8,allocatable::X70(:,:,:,:)
       real*8,allocatable::Z104(:,:,:,:,:,:)
       real*8,allocatable::X71(:,:,:,:)
       real*8,allocatable::Z105(:,:,:,:,:,:)
       real*8,allocatable::X72(:,:,:,:)
       real*8,allocatable::Z106(:,:,:,:,:,:)
       real*8,allocatable::X73(:,:,:,:)
       real*8,allocatable::Z107(:,:,:,:,:,:)
       real*8,allocatable::X74(:,:,:,:)
       real*8,allocatable::Z108(:,:,:,:,:,:)
       real*8,allocatable::X75(:,:,:,:)
       real*8,allocatable::Z109(:,:,:,:,:,:)
       real*8,allocatable::X76(:,:,:,:)
       real*8,allocatable::Z110(:,:,:,:,:,:)
       real*8,allocatable::X77(:,:,:,:)
       real*8,allocatable::Z111(:,:,:,:,:,:)
       real*8,allocatable::X78(:,:,:,:)
       real*8,allocatable::Z112(:,:,:,:,:,:)
       real*8,allocatable::X79(:,:,:,:)
       real*8,allocatable::Z113(:,:,:,:,:,:)
       real*8,allocatable::X80(:,:,:,:)
       real*8,allocatable::Z114(:,:,:,:,:,:)
       real*8,allocatable::X81(:,:,:,:)
       real*8,allocatable::Z115(:,:,:,:,:,:)
       real*8,allocatable::X82(:,:)
       real*8,allocatable::Z116(:,:,:,:,:,:)
       real*8,allocatable::X83(:,:)
       real*8,allocatable::Z117(:,:,:,:,:,:)
       real*8,allocatable::X84(:,:)
       real*8,allocatable::Z122(:,:,:,:,:,:)
       real*8,allocatable::X85(:,:)
       real*8,allocatable::Z123(:,:,:,:,:,:)
       real*8,allocatable::X86(:,:)
       real*8,allocatable::Z127(:,:,:,:,:,:)
       real*8,allocatable::X87(:,:)
       real*8,allocatable::Z128(:,:,:,:,:,:)
       real*8,allocatable::Z129(:,:,:,:,:,:)
       real*8,allocatable::Z130(:,:,:,:,:,:)
       real*8,allocatable::Z131(:,:,:,:,:,:)
       real*8,allocatable::Z132(:,:,:,:,:,:)
       real*8,allocatable::Z133(:,:,:,:,:,:)
       real*8,allocatable::Z134(:,:,:,:,:,:)
       real*8,allocatable::Z135(:,:,:,:,:,:)
       real*8,allocatable::Z136(:,:,:,:,:,:)
       real*8,allocatable::X88(:,:,:,:)
       real*8,allocatable::Z147(:,:,:,:,:,:)
       real*8,allocatable::X89(:,:,:,:)
       real*8,allocatable::Z148(:,:,:,:,:,:)
       real*8,allocatable::Z204(:,:,:,:,:,:)
       real*8,allocatable::Z205(:,:,:,:,:,:)
       real*8,allocatable::Z206(:,:,:,:,:,:)
       real*8,allocatable::X90(:,:,:,:)
       real*8,allocatable::Z238(:,:,:,:,:,:)
       real*8,allocatable::X91(:,:,:,:)
       real*8,allocatable::Z239(:,:,:,:,:,:)
       real*8,allocatable::X92(:,:,:,:)
       real*8,allocatable::Z240(:,:,:,:,:,:)
       real*8,allocatable::X93(:,:,:,:)
       real*8,allocatable::Z241(:,:,:,:,:,:)
       real*8,allocatable::Z244(:,:,:,:,:,:)
       real*8,allocatable::Z245(:,:,:,:,:,:)
       real*8,allocatable::X94(:,:,:,:)
       real*8,allocatable::Z246(:,:,:,:,:,:)
       real*8,allocatable::X95(:,:,:,:)
       real*8,allocatable::Z247(:,:,:,:,:,:)
       real*8,allocatable::X96(:,:,:,:)
       real*8,allocatable::Z248(:,:,:,:,:,:)
       real*8,allocatable::X97(:,:,:,:)
       real*8,allocatable::Z249(:,:,:,:,:,:)
       real*8,allocatable::X98(:,:,:,:)
       real*8,allocatable::Z250(:,:,:,:,:,:)
       real*8,allocatable::X99(:,:,:,:)
       real*8,allocatable::Z252(:,:,:,:,:,:)
       real*8,allocatable::X100(:,:,:,:)
       real*8,allocatable::Z255(:,:,:,:,:,:)
       real*8,allocatable::X101(:,:,:,:)
       real*8,allocatable::Z263(:,:,:,:,:,:)
       real*8,allocatable::X102(:,:,:,:)
       real*8,allocatable::Z272(:,:,:,:,:,:)
       real*8,allocatable::Z385(:,:,:,:,:,:)
       real*8,allocatable::Z386(:,:,:,:,:,:)
       real*8,allocatable::Z387(:,:,:,:,:,:)
       real*8,allocatable::Z388(:,:,:,:,:,:)
       real*8,allocatable::Z395(:,:,:,:,:,:)
       real*8,allocatable::Z396(:,:,:,:,:,:)
       real*8,allocatable::Z397(:,:,:,:,:,:)
       real*8,allocatable::Z398(:,:,:,:,:,:)
       real*8,allocatable::X103(:,:,:,:)
       real*8,allocatable::Z454(:,:,:,:,:,:)
C
       allocate(V3C(M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N2,M1+1:N1))
       V3C=0.0d0
C
       allocate(D1(N1+1:N3,N0+1:N1,M2+1:N3,M1+1:N2))
       call reorder2431(N0,N2,N1,N3,N2,N3,N0,N1,
     & N1,N3,N0,N1,M2,N3,M1,N2,VBHPPH,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S1(M1+1:N1,N0+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S1)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X1(N0+1:N1,M2+1:N3,M1+1:N2,M1+1:N1))
       X1=0.0d0
       call
     & sum4123(N0,N1,M2,N3,M1,N2,M1,N1,X1,S1, 1.000)
       deallocate(S1)
C
       allocate(D1(N1+1:N3,N0+1:N1,N2+1:M2,M1+1:N2))
       call reorder2431(N0,N2,N1,N3,N2,N3,N0,N1,
     & N1,N3,N0,N1,N2,M2,M1,N2,VBHPPH,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S2(M1+1:N1,N0+1:N1,N2+1:M2,M1+1:N2))
       I1=K8*K0*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S2)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X2(N0+1:N1,N2+1:M2,M1+1:N2,M1+1:N1))
       X2=0.0d0
       call
     & sum4123(N0,N1,N2,M2,M1,N2,M1,N1,X2,S2, 1.000)
       deallocate(S2)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,VBHPPH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S3(N1+1:M2,N1+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S3)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X3(N1+1:N3,M2+1:N3,N1+1:M2,M1+1:N2))
       X3=0.0d0
       call
     & sum3124(N1,N3,M2,N3,N1,M2,M1,N2,X3,S3,-1.000)
       deallocate(S3)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,VBHPPH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S4(N1+1:M2,N1+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S4)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X4(N1+1:N3,N2+1:M2,N1+1:M2,M1+1:N2))
       X4=0.0d0
       call
     & sum3124(N1,N3,N2,M2,N1,M2,M1,N2,X4,S4,-1.000)
       deallocate(S4)
C
       allocate(D1(N0+1:N1,N0+1:N2,M1+1:N2,M1+1:N1))
       call reorder4312(N0,N2,N0,N1,N0,N2,N0,N1,
     & N0,N1,N0,N2,M1,N2,M1,N1,VBHHHH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S5(N1+1:M2,N0+1:N2,M1+1:N2,M1+1:N1))
       I1=K7*K8*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S5)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X8(N0+1:N2,N1+1:M2,M1+1:N2,M1+1:N1))
       X8=0.0d0
       call
     & sum2134(N0,N2,N1,M2,M1,N2,M1,N1,X8,S5,-1.000)
       deallocate(S5)
C
       allocate(D1(N0+1:N1,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder4132(N2,N3,N0,N1,N2,N3,N0,N1,
     & N0,N1,N2,N3,M2,N3,M1,N1,VBHPHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S6(N1+1:M2,N2+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K4
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S6)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X9(N2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       X9=0.0d0
       call
     & sum3124(N2,N3,M2,N3,N1,M2,M1,N1,X9,S6,-1.000)
       deallocate(S6)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:M2,M1+1:N1))
       call reorder4132(N2,N3,N0,N1,N2,N3,N0,N1,
     & N0,N1,N2,N3,N2,M2,M1,N1,VBHPHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S7(N1+1:M2,N2+1:N3,N2+1:M2,M1+1:N1))
       I1=K7*K0*K4
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S7)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X10(N2+1:N3,N2+1:M2,N1+1:M2,M1+1:N1))
       X10=0.0d0
       call
     & sum3124(N2,N3,N2,M2,N1,M2,M1,N1,X10,S7,-1.000)
       deallocate(S7)
C
       allocate(D1(N1+1:N3,N0+1:N2,N1+1:M2,M1+1:N2))
       call reorder2341(N0,N2,N1,N3,N0,N2,N1,N3,
     & N1,N3,N0,N2,N1,M2,M1,N2,VBPHPH,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S8(M1+1:N1,N0+1:N2,N1+1:M2,M1+1:N2))
       I1=K8*K9*K2
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S8)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,N2,N1,M2,M1,N2,M1,N1,X8,S8, 1.000)
       deallocate(S8)
C
       allocate(D1(N1+1:N3,N2+1:N3,M2+1:N3,N1+1:M2))
       call reorder2134(N2,N3,N1,N3,N2,N3,N1,M2,
     & N1,N3,N2,N3,M2,N3,N1,M2,VBAPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S9(M1+1:N1,N2+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6*K4
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S9)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N2,N3,M2,N3,N1,M2,M1,N1,X9,S9, 1.000)
       deallocate(S9)
C
       allocate(D1(N1+1:N3,N2+1:N3,N2+1:M2,N1+1:M2))
       call reorder2134(N2,N3,N1,N3,N2,N3,N1,M2,
     & N1,N3,N2,N3,N2,M2,N1,M2,VBAPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S10(M1+1:N1,N2+1:N3,N2+1:M2,N1+1:M2))
       I1=K9*K0*K4
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S10)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N2,N3,N2,M2,N1,M2,M1,N1,X10,S10, 1.000)
       deallocate(S10)
C
       allocate(B1(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,FAHP,B1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(Q1(M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q1)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X19(N0+1:M1,M1+1:N1))
       X19=0.0d0
       call
     & sum21(N0,M1,M1,N1,X19,Q1, 1.000)
       deallocate(Q1)
C
       allocate(B1(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,FAHP,B1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(Q2(M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q2)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X20(M1+1:N1,M1+1:N1))
       X20=0.0d0
       call
     & sum21(M1,N1,M1,N1,X20,Q2, 1.000)
       deallocate(Q2)
C
       allocate(B1(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,FAHP,B1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(Q3(N1+1:M2,M2+1:N3))
       I1=K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,B1,B2,Q3)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X21(M2+1:N3,N1+1:M2))
       X21=0.0d0
       call
     & sum21(M2,N3,N1,M2,X21,Q3,-1.000)
       deallocate(Q3)
C
       allocate(B1(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,FAHP,B1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(Q4(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,B1,B2,Q4)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X22(N1+1:M2,N1+1:M2))
       X22=0.0d0
       call
     & sum21(N1,M2,N1,M2,X22,Q4,-1.000)
       deallocate(Q4)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,M1+1:N1))
       call reorder4312(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,M1,M2,N3,M1,N1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S11(N1+1:M2,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S11)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder2314(N1,M2,N0,M1,M2,N3,M1,N1,
     & N0,M1,M2,N3,N1,M2,M1,N1,S11,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z88(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K8*K8*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z88)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z88,-1.000)
       deallocate(Z88)
       deallocate(S11)
C
       allocate(D1(N0+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder4312(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,M1,N1,M2,N3,M1,N1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S12(N1+1:M2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S12)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder2314(N1,M2,M1,N1,M2,N3,M1,N1,
     & M1,N1,M2,N3,N1,M2,M1,N1,S12,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z89(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K8*K8*K0*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z89)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z89,-1.000)
       deallocate(Z89)
       deallocate(S12)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,M1+1:N1))
       call reorder4312(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,M1,N1,M2,M1,N1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S13(N1+1:M2,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S13)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder2314(N1,M2,N0,M1,N1,M2,M1,N1,
     & N0,M1,N1,M2,N1,M2,M1,N1,S13,D1)
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N1,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z90(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K8*K8*K0*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,D1,F2,Z90)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z90,-1.000)
       deallocate(Z90)
       deallocate(S13)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       call reorder4312(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,M1,N1,N1,M2,M1,N1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S14(N1+1:M2,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S14)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder2314(N1,M2,M1,N1,N1,M2,M1,N1,
     & M1,N1,N1,M2,N1,M2,M1,N1,S14,D1)
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N1,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z91(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K8*K8*K0*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,D1,F2,Z91)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z91,-1.000)
       deallocate(Z91)
       deallocate(S14)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder3142(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,M1,N1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q5(N0+1:M1,M1+1:N1))
       I1=K7*K5
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q5)
       deallocate(D1)
       deallocate(B2)
C
       X19=X19+Q5
       deallocate(Q5)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M1+1:N1))
       call reorder3142(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,M1,N1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q6(M1+1:N1,M1+1:N1))
       I1=K7*K7
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q6)
       deallocate(D1)
       deallocate(B2)
C
       X20=X20+Q6
       deallocate(Q6)
C
       allocate(D1(N1+1:N3,N0+1:M1,M2+1:N3,N1+1:M2))
       call reorder2413(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N0,M1,M2,N3,N1,M2,VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S15(M1+1:N1,N0+1:M1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S15)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder2341(M1,N1,N0,M1,M2,N3,N1,M2,
     & N0,M1,M2,N3,N1,M2,M1,N1,S15,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z94(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K8*K8*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z94)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z94,-1.000)
       deallocate(Z94)
       deallocate(S15)
C
       allocate(D1(N1+1:N3,M1+1:N1,M2+1:N3,N1+1:M2))
       call reorder2413(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,M1,N1,M2,N3,N1,M2,VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S16(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S16)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder2341(M1,N1,M1,N1,M2,N3,N1,M2,
     & M1,N1,M2,N3,N1,M2,M1,N1,S16,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z95(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K8*K8*K0*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z95)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z95,-1.000)
       deallocate(Z95)
       deallocate(S16)
C
       allocate(D1(N1+1:N3,N0+1:M1,N1+1:M2,N1+1:M2))
       call reorder2413(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N0,M1,N1,M2,N1,M2,VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S17(M1+1:N1,N0+1:M1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S17)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder2341(M1,N1,N0,M1,N1,M2,N1,M2,
     & N0,M1,N1,M2,N1,M2,M1,N1,S17,D1)
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N1,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z96(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K8*K8*K0*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,D1,F2,Z96)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z96,-1.000)
       deallocate(Z96)
       deallocate(S17)
C
       allocate(D1(N1+1:N3,M1+1:N1,N1+1:M2,N1+1:M2))
       call reorder2413(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,M1,N1,N1,M2,N1,M2,VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S18(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S18)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder2341(M1,N1,M1,N1,N1,M2,N1,M2,
     & M1,N1,N1,M2,N1,M2,M1,N1,S18,D1)
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N1,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z97(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K8*K8*K0*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,D1,F2,Z97)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z97,-1.000)
       deallocate(Z97)
       deallocate(S18)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,M2,N3,N1,M2,VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q7(M2+1:N3,N1+1:M2))
       I1=K9*K6
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q7)
       deallocate(D1)
       deallocate(B2)
C
       X21=X21-Q7
       deallocate(Q7)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,M2,N1,M2,VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q8(N1+1:M2,N1+1:M2))
       I1=K9*K9
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q8)
       deallocate(D1)
       deallocate(B2)
C
       X22=X22-Q8
       deallocate(Q8)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:M1,M1+1:N2))
       call reorder2341(N0,N2,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,N0,M1,M1,N2,VBHHPH,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S19(M1+1:N1,N0+1:M1,N0+1:M1,M1+1:N2))
       I1=K8*K5*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S19)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X33(N0+1:M1,N0+1:M1,M1+1:N2,M1+1:N1))
       X33=0.0d0
       call
     & sum4123(N0,M1,N0,M1,M1,N2,M1,N1,X33,S19, 1.000)
       deallocate(S19)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:M1,M1+1:N2))
       call reorder2341(N0,N2,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,N0,M1,M1,N2,VBHHPH,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S20(M1+1:N1,M1+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S20)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X34(M1+1:N2,N0+1:M1,M1+1:N2,M1+1:N1))
       X34=0.0d0
       call
     & sum4123(M1,N2,N0,M1,M1,N2,M1,N1,X34,S20, 1.000)
       deallocate(S20)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,M1+1:N2))
       call reorder2341(N0,N2,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,M1,N1,M1,N2,VBHHPH,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S21(M1+1:N1,N0+1:M1,M1+1:N1,M1+1:N2))
       I1=K8*K7*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S21)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X35(N0+1:M1,M1+1:N1,M1+1:N2,M1+1:N1))
       X35=0.0d0
       call
     & sum4123(N0,M1,M1,N1,M1,N2,M1,N1,X35,S21, 1.000)
       deallocate(S21)
C
       allocate(D1(N1+1:N3,M1+1:N2,M1+1:N1,M1+1:N2))
       call reorder2341(N0,N2,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,M1,N1,M1,N2,VBHHPH,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S22(M1+1:N1,M1+1:N2,M1+1:N1,M1+1:N2))
       I1=K8*K7*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S22)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X36(M1+1:N2,M1+1:N1,M1+1:N2,M1+1:N1))
       X36=0.0d0
       call
     & sum4123(M1,N2,M1,N1,M1,N2,M1,N1,X36,S22, 1.000)
       deallocate(S22)
C
       allocate(D1(N1+1:N3,N0+1:M1,M2+1:N3,M2+1:N3))
       call reorder2413(N2,N3,N1,N3,N2,N3,N0,N1,
     & N1,N3,N0,M1,M2,N3,M2,N3,VBHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S23(M1+1:N1,N0+1:M1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S23)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X70(N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N1))
       X70=0.0d0
       call
     & sum4123(N0,M1,M2,N3,M2,N3,M1,N1,X70,S23, 1.000)
       deallocate(S23)
C
       allocate(D1(N1+1:N3,M1+1:N1,M2+1:N3,M2+1:N3))
       call reorder2413(N2,N3,N1,N3,N2,N3,N0,N1,
     & N1,N3,M1,N1,M2,N3,M2,N3,VBHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S24(M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S24)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X71(M1+1:N1,M2+1:N3,M2+1:N3,M1+1:N1))
       X71=0.0d0
       call
     & sum4123(M1,N1,M2,N3,M2,N3,M1,N1,X71,S24, 1.000)
       deallocate(S24)
C
       allocate(D1(N1+1:N3,N0+1:M1,N2+1:M2,M2+1:N3))
       call reorder2413(N2,N3,N1,N3,N2,N3,N0,N1,
     & N1,N3,N0,M1,N2,M2,M2,N3,VBHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S25(M1+1:N1,N0+1:M1,N2+1:M2,M2+1:N3))
       I1=K6*K0*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S25)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X72(N0+1:M1,N2+1:M2,M2+1:N3,M1+1:N1))
       X72=0.0d0
       call
     & sum4123(N0,M1,N2,M2,M2,N3,M1,N1,X72,S25, 1.000)
       deallocate(S25)
C
       allocate(D1(N1+1:N3,M1+1:N1,N2+1:M2,M2+1:N3))
       call reorder2413(N2,N3,N1,N3,N2,N3,N0,N1,
     & N1,N3,M1,N1,N2,M2,M2,N3,VBHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S26(M1+1:N1,M1+1:N1,N2+1:M2,M2+1:N3))
       I1=K6*K0*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S26)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X73(M1+1:N1,N2+1:M2,M2+1:N3,M1+1:N1))
       X73=0.0d0
       call
     & sum4123(M1,N1,N2,M2,M2,N3,M1,N1,X73,S26, 1.000)
       deallocate(S26)
C
       allocate(D1(N1+1:N3,N0+1:M1,M2+1:N3,N2+1:M2))
       call reorder2413(N2,N3,N1,N3,N2,N3,N0,N1,
     & N1,N3,N0,M1,M2,N3,N2,M2,VBHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S27(M1+1:N1,N0+1:M1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S27)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X74(N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N1))
       X74=0.0d0
       call
     & sum4123(N0,M1,M2,N3,N2,M2,M1,N1,X74,S27, 1.000)
       deallocate(S27)
C
       allocate(D1(N1+1:N3,M1+1:N1,M2+1:N3,N2+1:M2))
       call reorder2413(N2,N3,N1,N3,N2,N3,N0,N1,
     & N1,N3,M1,N1,M2,N3,N2,M2,VBHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S28(M1+1:N1,M1+1:N1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S28)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X75(M1+1:N1,M2+1:N3,N2+1:M2,M1+1:N1))
       X75=0.0d0
       call
     & sum4123(M1,N1,M2,N3,N2,M2,M1,N1,X75,S28, 1.000)
       deallocate(S28)
C
       allocate(D1(N1+1:N3,N0+1:M1,N2+1:M2,N2+1:M2))
       call reorder2413(N2,N3,N1,N3,N2,N3,N0,N1,
     & N1,N3,N0,M1,N2,M2,N2,M2,VBHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S29(M1+1:N1,N0+1:M1,N2+1:M2,N2+1:M2))
       I1=K0*K0*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S29)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X76(N0+1:M1,N2+1:M2,N2+1:M2,M1+1:N1))
       X76=0.0d0
       call
     & sum4123(N0,M1,N2,M2,N2,M2,M1,N1,X76,S29, 1.000)
       deallocate(S29)
C
       allocate(D1(N1+1:N3,M1+1:N1,N2+1:M2,N2+1:M2))
       call reorder2413(N2,N3,N1,N3,N2,N3,N0,N1,
     & N1,N3,M1,N1,N2,M2,N2,M2,VBHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S30(M1+1:N1,M1+1:N1,N2+1:M2,N2+1:M2))
       I1=K0*K0*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S30)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X77(M1+1:N1,N2+1:M2,N2+1:M2,M1+1:N1))
       X77=0.0d0
       call
     & sum4123(M1,N1,N2,M2,N2,M2,M1,N1,X77,S30, 1.000)
       deallocate(S30)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder4321(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N0,M1,M2,N3,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S31(N1+1:M2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S31)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X78(N0+1:M1,M2+1:N3,N1+1:M2,M1+1:N2))
       X78=0.0d0
       call
     & sum3124(N0,M1,M2,N3,N1,M2,M1,N2,X78,S31, 1.000)
       deallocate(S31)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder4321(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,M1,N2,M2,N3,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S32(N1+1:M2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S32)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X79(M1+1:N2,M2+1:N3,N1+1:M2,M1+1:N2))
       X79=0.0d0
       call
     & sum3124(M1,N2,M2,N3,N1,M2,M1,N2,X79,S32, 1.000)
       deallocate(S32)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,M1+1:N2))
       call reorder4321(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N0,M1,N1,M2,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S33(N1+1:M2,N0+1:M1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S33)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X80(N0+1:M1,N1+1:M2,N1+1:M2,M1+1:N2))
       X80=0.0d0
       call
     & sum3124(N0,M1,N1,M2,N1,M2,M1,N2,X80,S33, 1.000)
       deallocate(S33)
C
       allocate(D1(N0+1:N1,M1+1:N2,N1+1:M2,M1+1:N2))
       call reorder4321(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,M1,N2,N1,M2,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S34(N1+1:M2,M1+1:N2,N1+1:M2,M1+1:N2))
       I1=K8*K9*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S34)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X81(M1+1:N2,N1+1:M2,N1+1:M2,M1+1:N2))
       X81=0.0d0
       call
     & sum3124(M1,N2,N1,M2,N1,M2,M1,N2,X81,S34, 1.000)
       deallocate(S34)
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
       call EGEMM1(I1,I3,D1,B2,Q9)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X82(N0+1:M1,M1+1:N2))
       X82=0.0d0
       X82=X82+Q9
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
       call EGEMM1(I1,I3,D1,B2,Q10)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X83(M1+1:N2,M1+1:N2))
       X83=0.0d0
       X83=X83+Q10
       deallocate(Q10)
C
       allocate(D1(N0+1:N1,M2+1:N3,M2+1:N3,M2+1:N3))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,M2,N3,M2,N3,M2,N3,VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S35(N1+1:M2,M2+1:N3,M2+1:N3,M2+1:N3))
       I1=K6*K6*K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S35)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X49(M2+1:N3,M2+1:N3,M2+1:N3,N1+1:M2))
       X49=0.0d0
       call
     & sum4123(M2,N3,M2,N3,M2,N3,N1,M2,X49,S35,-1.000)
       deallocate(S35)
C
       allocate(D1(N0+1:N1,N2+1:M2,M2+1:N3,M2+1:N3))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N2,M2,M2,N3,M2,N3,VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S36(N1+1:M2,N2+1:M2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K0
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S36)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X50(N2+1:M2,M2+1:N3,M2+1:N3,N1+1:M2))
       X50=0.0d0
       call
     & sum4123(N2,M2,M2,N3,M2,N3,N1,M2,X50,S36,-1.000)
       deallocate(S36)
C
       allocate(D1(N0+1:N1,M2+1:N3,N1+1:M2,M2+1:N3))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,M2,N3,N1,M2,M2,N3,VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S37(N1+1:M2,M2+1:N3,N1+1:M2,M2+1:N3))
       I1=K6*K9*K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S37)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X51(M2+1:N3,N1+1:M2,M2+1:N3,N1+1:M2))
       X51=0.0d0
       call
     & sum4123(M2,N3,N1,M2,M2,N3,N1,M2,X51,S37,-1.000)
       deallocate(S37)
C
       allocate(D1(N0+1:N1,N2+1:M2,N1+1:M2,M2+1:N3))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N2,M2,N1,M2,M2,N3,VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S38(N1+1:M2,N2+1:M2,N1+1:M2,M2+1:N3))
       I1=K6*K9*K0
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S38)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X52(N2+1:M2,N1+1:M2,M2+1:N3,N1+1:M2))
       X52=0.0d0
       call
     & sum4123(N2,M2,N1,M2,M2,N3,N1,M2,X52,S38,-1.000)
       deallocate(S38)
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
       call EGEMM1(I1,I3,D1,B2,Q11)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X84(M2+1:N3,M2+1:N3))
       X84=0.0d0
       X84=X84+Q11
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
       call EGEMM1(I1,I3,D1,B2,Q12)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X85(N2+1:M2,M2+1:N3))
       X85=0.0d0
       X85=X85+Q12
       deallocate(Q12)
C
       allocate(D1(N0+1:N1,N2+1:M2,M2+1:N3,N2+1:M2))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N2,M2,M2,N3,N2,M2,VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S39(N1+1:M2,N2+1:M2,M2+1:N3,N2+1:M2))
       I1=K0*K6*K0
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S39)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X53(N2+1:M2,M2+1:N3,N2+1:M2,N1+1:M2))
       X53=0.0d0
       call
     & sum4123(N2,M2,M2,N3,N2,M2,N1,M2,X53,S39,-1.000)
       deallocate(S39)
C
       allocate(D1(N0+1:N1,M2+1:N3,N1+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,M2,N3,N1,M2,N2,M2,VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S40(N1+1:M2,M2+1:N3,N1+1:M2,N2+1:M2))
       I1=K0*K9*K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S40)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X54(M2+1:N3,N1+1:M2,N2+1:M2,N1+1:M2))
       X54=0.0d0
       call
     & sum4123(M2,N3,N1,M2,N2,M2,N1,M2,X54,S40,-1.000)
       deallocate(S40)
C
       allocate(D1(N0+1:N1,N2+1:M2,N1+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N2,M2,N1,M2,N2,M2,VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S41(N1+1:M2,N2+1:M2,N1+1:M2,N2+1:M2))
       I1=K0*K9*K0
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S41)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X55(N2+1:M2,N1+1:M2,N2+1:M2,N1+1:M2))
       X55=0.0d0
       call
     & sum4123(N2,M2,N1,M2,N2,M2,N1,M2,X55,S41,-1.000)
       deallocate(S41)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,N2+1:M2))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,M2,N3,N2,M2,VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q13(M2+1:N3,N2+1:M2))
       I1=K0*K6
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q13)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X86(M2+1:N3,N2+1:M2))
       X86=0.0d0
       X86=X86+Q13
       deallocate(Q13)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,N2+1:M2))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,M2,N2,M2,VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q14(N2+1:M2,N2+1:M2))
       I1=K0*K0
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q14)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X87(N2+1:M2,N2+1:M2))
       X87=0.0d0
       X87=X87+Q14
       deallocate(Q14)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,M1+1:N1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N0,M1,M2,N3,M1,N1,VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S42(N1+1:M2,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S42)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder2314(N1,M2,N0,M1,M2,N3,M1,N1,
     & N0,M1,M2,N3,N1,M2,M1,N1,S42,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z129(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K8*K8*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z129)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z129,-1.000)
       deallocate(Z129)
       deallocate(S42)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,M1+1:N1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,M1,N2,M2,N3,M1,N1,VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S43(N1+1:M2,M1+1:N2,M2+1:N3,M1+1:N1))
       I1=K7*K6*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S43)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder2314(N1,M2,M1,N2,M2,N3,M1,N1,
     & M1,N2,M2,N3,N1,M2,M1,N1,S43,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z130(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K8*K8*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z130)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z130,-1.000)
       deallocate(Z130)
       deallocate(S43)
C
       allocate(D1(N0+1:N1,N0+1:M1,N2+1:M2,M1+1:N1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N0,M1,N2,M2,M1,N1,VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S44(N1+1:M2,N0+1:M1,N2+1:M2,M1+1:N1))
       I1=K7*K0*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S44)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder2314(N1,M2,N0,M1,N2,M2,M1,N1,
     & N0,M1,N2,M2,N1,M2,M1,N1,S44,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z131(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K8*K8*K0*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z131)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z131, 1.000)
       deallocate(Z131)
       deallocate(S44)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,M1+1:N1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,M1,N2,N2,M2,M1,N1,VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S45(N1+1:M2,M1+1:N2,N2+1:M2,M1+1:N1))
       I1=K7*K0*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S45)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder2314(N1,M2,M1,N2,N2,M2,M1,N1,
     & M1,N2,N2,M2,N1,M2,M1,N1,S45,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z132(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K8*K8*K0*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z132)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z132, 1.000)
       deallocate(Z132)
       deallocate(S45)
C
       allocate(D1(N1+1:N3,N0+1:M1,M2+1:N3,N1+1:M2))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,N0,M1,M2,N3,N1,M2,VBPHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S46(M1+1:N1,N0+1:M1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S46)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder2341(M1,N1,N0,M1,M2,N3,N1,M2,
     & N0,M1,M2,N3,N1,M2,M1,N1,S46,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z133(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K8*K8*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z133)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z133, 1.000)
       deallocate(Z133)
       deallocate(S46)
C
       allocate(D1(N1+1:N3,M1+1:N2,M2+1:N3,N1+1:M2))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,M1,N2,M2,N3,N1,M2,VBPHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S47(M1+1:N1,M1+1:N2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S47)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder2341(M1,N1,M1,N2,M2,N3,N1,M2,
     & M1,N2,M2,N3,N1,M2,M1,N1,S47,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z134(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K8*K8*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z134)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z134, 1.000)
       deallocate(Z134)
       deallocate(S47)
C
       allocate(D1(N1+1:N3,N0+1:M1,N2+1:M2,N1+1:M2))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,N0,M1,N2,M2,N1,M2,VBPHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S48(M1+1:N1,N0+1:M1,N2+1:M2,N1+1:M2))
       I1=K9*K0*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S48)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder2341(M1,N1,N0,M1,N2,M2,N1,M2,
     & N0,M1,N2,M2,N1,M2,M1,N1,S48,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z135(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K8*K8*K0*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z135)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z135,-1.000)
       deallocate(Z135)
       deallocate(S48)
C
       allocate(D1(N1+1:N3,M1+1:N2,N2+1:M2,N1+1:M2))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,M1,N2,N2,M2,N1,M2,VBPHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S49(M1+1:N1,M1+1:N2,N2+1:M2,N1+1:M2))
       I1=K9*K0*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S49)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder2341(M1,N1,M1,N2,N2,M2,N1,M2,
     & M1,N2,N2,M2,N1,M2,M1,N1,S49,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z136(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K8*K8*K0*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z136)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z136,-1.000)
       deallocate(Z136)
       deallocate(S49)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,M1+1:N1))
       call reorder3412(N0,N2,N0,N1,N0,N2,N0,N1,
     & N0,N2,N0,N1,M1,N2,M1,N1,VBHHHH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S50(M2+1:N3,N0+1:N1,M1+1:N2,M1+1:N1))
       I1=K7*K8*K1
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S50)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,M2,N3,M1,N2,M1,N1,X1,S50,-1.000)
       deallocate(S50)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,M1+1:N1))
       call reorder3412(N0,N2,N0,N1,N0,N2,N0,N1,
     & N0,N2,N0,N1,M1,N2,M1,N1,VBHHHH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S51(N2+1:M2,N0+1:N1,M1+1:N2,M1+1:N1))
       I1=K7*K8*K1
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S51)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N2,M2,M1,N2,M1,N1,X2,S51,-1.000)
       deallocate(S51)
C
       allocate(D1(N2+1:N3,N0+1:N1,M2+1:N3,M1+1:N1))
       call reorder1432(N2,N3,N0,N1,N2,N3,N0,N1,
     & N2,N3,N0,N1,M2,N3,M1,N1,VBHPHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S52(M1+1:N2,N0+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K1
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S52)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N1,M2,N3,M1,N2,M1,N1,X1,S52, 1.000)
       deallocate(S52)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:M2,M1+1:N1))
       call reorder1432(N2,N3,N0,N1,N2,N3,N0,N1,
     & N2,N3,N0,N1,N2,M2,M1,N1,VBHPHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S53(M1+1:N2,N0+1:N1,N2+1:M2,M1+1:N1))
       I1=K7*K0*K1
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S53)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N1,N2,M2,M1,N2,M1,N1,X2,S53, 1.000)
       deallocate(S53)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,M1+1:N2))
       call reorder3241(N0,N2,N1,N3,N0,N2,N1,N3,
     & N0,N2,N1,N3,N1,M2,M1,N2,VBPHPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S54(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N2))
       I1=K8*K9*K3
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S54)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,N1,M2,M1,N2,X3,S54,-1.000)
       deallocate(S54)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,M1+1:N2))
       call reorder3241(N0,N2,N1,N3,N0,N2,N1,N3,
     & N0,N2,N1,N3,N1,M2,M1,N2,VBPHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S55(N2+1:M2,N1+1:N3,N1+1:M2,M1+1:N2))
       I1=K8*K9*K3
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S55)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,N2,M2,N1,M2,M1,N2,X4,S55,-1.000)
       deallocate(S55)
C
       allocate(D1(N2+1:N3,N1+1:N3,M2+1:N3,N1+1:M2))
       call reorder1234(N2,N3,N1,N3,N2,N3,N1,M2,
     & N2,N3,N1,N3,M2,N3,N1,M2,VBAPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S56(M1+1:N2,N1+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6*K3
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S56)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N1,N3,M2,N3,N1,M2,M1,N2,X3,S56, 1.000)
       deallocate(S56)
C
       allocate(D1(N2+1:N3,N1+1:N3,N2+1:M2,N1+1:M2))
       call reorder1234(N2,N3,N1,N3,N2,N3,N1,M2,
     & N2,N3,N1,N3,N2,M2,N1,M2,VBAPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S57(M1+1:N2,N1+1:N3,N2+1:M2,N1+1:M2))
       I1=K9*K0*K3
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S57)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N1,N3,N2,M2,N1,M2,M1,N2,X4,S57, 1.000)
       deallocate(S57)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,M1,N2,M1,N2,VCHHHH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S58(M2+1:N3,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S58)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X5(N0+1:N2,M2+1:N3,M1+1:N2,M1+1:N2))
       X5=0.0d0
       call
     & sum2134(N0,N2,M2,N3,M1,N2,M1,N2,X5,S58,-1.000)
       deallocate(S58)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,M1,N2,M1,N2,VCHHHH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S59(N2+1:M2,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S59)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X6(N0+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       X6=0.0d0
       call
     & sum2134(N0,N2,N2,M2,M1,N2,M1,N2,X6,S59,-1.000)
       deallocate(S59)
C
       allocate(D1(N2+1:N3,N0+1:N2,M2+1:N3,M1+1:N2))
       call reorder1432(N2,N3,N0,N2,N2,N3,N0,N2,
     & N2,N3,N0,N2,M2,N3,M1,N2,VCHPHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S60(M1+1:N2,N0+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S60)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X88(N0+1:N2,M2+1:N3,M1+1:N2,M1+1:N2))
       X88=0.0d0
       call
     & sum3124(N0,N2,M2,N3,M1,N2,M1,N2,X88,S60, 1.000)
       deallocate(S60)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:M2,M1+1:N2))
       call reorder1432(N2,N3,N0,N2,N2,N3,N0,N2,
     & N2,N3,N0,N2,N2,M2,M1,N2,VCHPHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S61(M1+1:N2,N0+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S61)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X89(N0+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       X89=0.0d0
       call
     & sum3124(N0,N2,N2,M2,M1,N2,M1,N2,X89,S61, 1.000)
       deallocate(S61)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder4132(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,VCHPHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S62(N2+1:M2,N2+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S62)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X7(N2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       X7=0.0d0
       call
     & sum3124(N2,N3,M2,N3,N2,M2,M1,N2,X7,S62,-1.000)
       deallocate(S62)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder4132(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,VCHPHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S63(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S63)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,M1,N2,X7,S63, 1.000)
       deallocate(S63)
C
       allocate(D1(N2+1:N3,N2+1:N3,M2+1:N3,N2+1:M2))
       call reorder1234(N2,N3,N2,N3,N2,N3,N2,M2,
     & N2,N3,N2,N3,M2,N3,N2,M2,VCAPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S64(M1+1:N2,N2+1:N3,M2+1:N3,N2+1:M2))
       I1=K0*K6*K4
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S64)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N2,N3,M2,N3,N2,M2,M1,N2,X7,S64,-1.000)
       deallocate(S64)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:M2,M1+1:N1))
       call reorder3124(N0,N2,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,N2,N1,M2,M1,N1,VBHPPH,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S65(M1+1:N2,N0+1:N2,N1+1:M2,M1+1:N1))
       I1=K7*K9*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S65)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N2,N1,M2,M1,N2,M1,N1,X8,S65, 1.000)
       deallocate(S65)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,VBHPPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S66(M2+1:N3,N2+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S66)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N1,M2,M1,N1,X9,S66,-1.000)
       deallocate(S66)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,VBHPPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S67(N2+1:M2,N2+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S67)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,N1,M2,M1,N1,X10,S67,-1.000)
       deallocate(S67)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,M1,M2,N3,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S68(M2+1:N3,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S68)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X11(N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N2))
       X11=0.0d0
       call
     & sum3124(N0,M1,M2,N3,M2,N3,M1,N2,X11,S68,-1.000)
       deallocate(S68)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,M2,N3,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S69(M2+1:N3,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S69)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X12(M1+1:N1,M2+1:N3,M2+1:N3,M1+1:N2))
       X12=0.0d0
       call
     & sum3124(M1,N1,M2,N3,M2,N3,M1,N2,X12,S69,-1.000)
       deallocate(S69)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,M1,N1,M2,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S70(M2+1:N3,N0+1:M1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S70)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X13(N0+1:M1,N1+1:M2,M2+1:N3,M1+1:N2))
       X13=0.0d0
       call
     & sum3124(N0,M1,N1,M2,M2,N3,M1,N2,X13,S70,-1.000)
       deallocate(S70)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,N1,M2,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S71(M2+1:N3,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S71)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X14(M1+1:N1,N1+1:M2,M2+1:N3,M1+1:N2))
       X14=0.0d0
       call
     & sum3124(M1,N1,N1,M2,M2,N3,M1,N2,X14,S71,-1.000)
       deallocate(S71)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,M1,M2,N3,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S72(N2+1:M2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S72)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X15(N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       X15=0.0d0
       call
     & sum3124(N0,M1,M2,N3,N2,M2,M1,N2,X15,S72,-1.000)
       deallocate(S72)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,M2,N3,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S73(N2+1:M2,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S73)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X16(M1+1:N1,M2+1:N3,N2+1:M2,M1+1:N2))
       X16=0.0d0
       call
     & sum3124(M1,N1,M2,N3,N2,M2,M1,N2,X16,S73,-1.000)
       deallocate(S73)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,M1,N1,M2,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S74(N2+1:M2,N0+1:M1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S74)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X17(N0+1:M1,N1+1:M2,N2+1:M2,M1+1:N2))
       X17=0.0d0
       call
     & sum3124(N0,M1,N1,M2,N2,M2,M1,N2,X17,S74,-1.000)
       deallocate(S74)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,N1,M2,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S75(N2+1:M2,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S75)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X18(M1+1:N1,N1+1:M2,N2+1:M2,M1+1:N2))
       X18=0.0d0
       call
     & sum3124(M1,N1,N1,M2,N2,M2,M1,N2,X18,S75,-1.000)
       deallocate(S75)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,M2+1:N3))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,M1,M2,N3,M2,N3,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S76(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S76)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,M2,N3,M2,N3,M1,N2,X11,S76, 1.000)
       deallocate(S76)
C
       allocate(D1(N2+1:N3,M1+1:N1,M2+1:N3,M2+1:N3))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,M1,N1,M2,N3,M2,N3,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S77(M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K7
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S77)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,M2,N3,M2,N3,M1,N2,X12,S77, 1.000)
       deallocate(S77)
C
       allocate(D1(N2+1:N3,N0+1:M1,N1+1:M2,M2+1:N3))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,M1,N1,M2,M2,N3,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S78(M1+1:N2,N0+1:M1,N1+1:M2,M2+1:N3))
       I1=K6*K9*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S78)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,N1,M2,M2,N3,M1,N2,X13,S78, 1.000)
       deallocate(S78)
C
       allocate(D1(N2+1:N3,M1+1:N1,N1+1:M2,M2+1:N3))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,M1,N1,N1,M2,M2,N3,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S79(M1+1:N2,M1+1:N1,N1+1:M2,M2+1:N3))
       I1=K6*K9*K7
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S79)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,N1,M2,M2,N3,M1,N2,X14,S79, 1.000)
       deallocate(S79)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,M1,M2,N3,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S80(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S80)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,M2,N3,N2,M2,M1,N2,X15,S80, 1.000)
       deallocate(S80)
C
       allocate(D1(N2+1:N3,M1+1:N1,M2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,M1,N1,M2,N3,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S81(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K7
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S81)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,M2,N3,N2,M2,M1,N2,X16,S81, 1.000)
       deallocate(S81)
C
       allocate(D1(N2+1:N3,N0+1:M1,N1+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,M1,N1,M2,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S82(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:M2))
       I1=K0*K9*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S82)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,N1,M2,N2,M2,M1,N2,X17,S82, 1.000)
       deallocate(S82)
C
       allocate(D1(N2+1:N3,M1+1:N1,N1+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,M1,N1,N1,M2,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S83(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:M2))
       I1=K0*K9*K7
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S83)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,N1,M2,N2,M2,M1,N2,X18,S83, 1.000)
       deallocate(S83)
C
       allocate(B1(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,FBHP,B1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(Q15(M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q15)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X23(N0+1:M1,M1+1:N2))
       X23=0.0d0
       call
     & sum21(N0,M1,M1,N2,X23,Q15, 1.000)
       deallocate(Q15)
C
       allocate(B1(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,FBHP,B1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(Q16(M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q16)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X24(M1+1:N2,M1+1:N2))
       X24=0.0d0
       call
     & sum21(M1,N2,M1,N2,X24,Q16, 1.000)
       deallocate(Q16)
C
       allocate(B1(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,FBHP,B1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(Q17(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,B1,B2,Q17)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X25(M2+1:N3,M2+1:N3))
       X25=0.0d0
       call
     & sum21(M2,N3,M2,N3,X25,Q17,-1.000)
       deallocate(Q17)
C
       allocate(B1(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,FBHP,B1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(Q18(M2+1:N3,N2+1:M2))
       I1=K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,B1,B2,Q18)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X26(N2+1:M2,M2+1:N3))
       X26=0.0d0
       call
     & sum21(N2,M2,M2,N3,X26,Q18,-1.000)
       deallocate(Q18)
C
       allocate(B1(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,FBHP,B1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(Q19(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,B1,B2,Q19)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X27(M2+1:N3,N2+1:M2))
       X27=0.0d0
       call
     & sum21(M2,N3,N2,M2,X27,Q19,-1.000)
       deallocate(Q19)
C
       allocate(B1(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,FBHP,B1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(Q20(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,B1,B2,Q20)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X28(N2+1:M2,N2+1:M2))
       X28=0.0d0
       call
     & sum21(N2,M2,N2,M2,X28,Q20,-1.000)
       deallocate(Q20)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder1342(N2,N3,N0,N1,N0,N2,N0,N1,
     & N2,N3,N0,M1,N0,M1,M1,N1,VBHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S84(M1+1:N2,N0+1:M1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S84)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N0,M1,M1,N2,M1,N1,X33,S84, 1.000)
       deallocate(S84)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:M1,M1+1:N1))
       call reorder1342(N2,N3,N0,N1,N0,N2,N0,N1,
     & N2,N3,M1,N2,N0,M1,M1,N1,VBHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S85(M1+1:N2,M1+1:N2,N0+1:M1,M1+1:N1))
       I1=K7*K5*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S85)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,N0,M1,M1,N2,M1,N1,X34,S85, 1.000)
       deallocate(S85)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N1,M1+1:N1))
       call reorder1342(N2,N3,N0,N1,N0,N2,N0,N1,
     & N2,N3,N0,M1,M1,N1,M1,N1,VBHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S86(M1+1:N2,N0+1:M1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S86)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M1,N1,M1,N2,M1,N1,X35,S86, 1.000)
       deallocate(S86)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N1,M1+1:N1))
       call reorder1342(N2,N3,N0,N1,N0,N2,N0,N1,
     & N2,N3,M1,N2,M1,N1,M1,N1,VBHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S87(M1+1:N2,M1+1:N2,M1+1:N1,M1+1:N1))
       I1=K7*K7*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S87)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M1,N1,M1,N2,M1,N1,X36,S87, 1.000)
       deallocate(S87)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N0,M1,M2,N3,M1,N1,VBHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S88(M2+1:N3,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S88)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X37(N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N1))
       X37=0.0d0
       call
     & sum3124(N0,M1,M2,N3,M2,N3,M1,N1,X37,S88,-1.000)
       deallocate(S88)
C
       call sumx2431(N2,N3,N0,N1,N2,N3,N0,N1,
     & N0,M1,M2,N3,M2,N3,M1,N1,X37,VBHPHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N2))
       call reorder612345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z37(N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K8*K8*K9*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,X37,F2,Z37)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z37,-1.000)
       deallocate(Z37)
       deallocate(X37)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,M1,N1,M2,N3,M1,N1,VBHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S89(M2+1:N3,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S89)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X38(M1+1:N1,M2+1:N3,M2+1:N3,M1+1:N1))
       X38=0.0d0
       call
     & sum3124(M1,N1,M2,N3,M2,N3,M1,N1,X38,S89,-1.000)
       deallocate(S89)
C
       call sumx2431(N2,N3,N0,N1,N2,N3,N0,N1,
     & M1,N1,M2,N3,M2,N3,M1,N1,X38,VBHPHP, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N2))
       call reorder612345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z38(N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K8*K8*K9*K0
       I3=K6*K7
       call EGEMM(I1,I2,I3,X38,F2,Z38)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z38,-1.000)
       deallocate(Z38)
       deallocate(X38)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,M1+1:N1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N0,M1,N2,M2,M1,N1,VBHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S90(M2+1:N3,N0+1:M1,N2+1:M2,M1+1:N1))
       I1=K7*K0*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S90)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X39(N0+1:M1,N2+1:M2,M2+1:N3,M1+1:N1))
       X39=0.0d0
       call
     & sum3124(N0,M1,N2,M2,M2,N3,M1,N1,X39,S90,-1.000)
       deallocate(S90)
C
       call sumx2431(N2,N3,N0,N1,N2,N3,N0,N1,
     & N0,M1,N2,M2,M2,N3,M1,N1,X39,VBHPHP, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N2))
       call reorder612345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N2,M2,N1,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z39(N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K8*K8*K9*K0
       I3=K0*K5
       call EGEMM(I1,I2,I3,X39,F2,Z39)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z39,-1.000)
       deallocate(Z39)
       deallocate(X39)
C
       allocate(D1(N0+1:N2,M1+1:N1,N2+1:M2,M1+1:N1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,M1,N1,N2,M2,M1,N1,VBHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S91(M2+1:N3,M1+1:N1,N2+1:M2,M1+1:N1))
       I1=K7*K0*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S91)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X40(M1+1:N1,N2+1:M2,M2+1:N3,M1+1:N1))
       X40=0.0d0
       call
     & sum3124(M1,N1,N2,M2,M2,N3,M1,N1,X40,S91,-1.000)
       deallocate(S91)
C
       call sumx2431(N2,N3,N0,N1,N2,N3,N0,N1,
     & M1,N1,N2,M2,M2,N3,M1,N1,X40,VBHPHP, 1.000)
C
       allocate(F2(M1+1:N1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N2))
       call reorder612345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,N2,M2,N1,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z40(N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K8*K8*K9*K0
       I3=K0*K7
       call EGEMM(I1,I2,I3,X40,F2,Z40)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z40,-1.000)
       deallocate(Z40)
       deallocate(X40)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N0,M1,M2,N3,M1,N1,VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S92(N2+1:M2,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S92)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X41(N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N1))
       X41=0.0d0
       call
     & sum3124(N0,M1,M2,N3,N2,M2,M1,N1,X41,S92,-1.000)
       deallocate(S92)
C
       call sumx2431(N2,N3,N0,N1,N2,N3,N0,N1,
     & N0,M1,M2,N3,N2,M2,M1,N1,X41,VBHPHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N2))
       call reorder612345(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,M1,N2,M1,N2,t3C3,F2)
       allocate(Z41(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N1))
       I1=K7*K0
       I2=K8*K8*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X41,F2,Z41)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z41, 1.000)
       deallocate(Z41)
       deallocate(X41)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,M1,N1,M2,N3,M1,N1,VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S93(N2+1:M2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S93)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X42(M1+1:N1,M2+1:N3,N2+1:M2,M1+1:N1))
       X42=0.0d0
       call
     & sum3124(M1,N1,M2,N3,N2,M2,M1,N1,X42,S93,-1.000)
       deallocate(S93)
C
       call sumx2431(N2,N3,N0,N1,N2,N3,N0,N1,
     & M1,N1,M2,N3,N2,M2,M1,N1,X42,VBHPHP, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N2))
       call reorder612345(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N1,M2,M1,N2,M1,N2,t3C3,F2)
       allocate(Z42(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N1))
       I1=K7*K0
       I2=K8*K8*K9*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X42,F2,Z42)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z42, 1.000)
       deallocate(Z42)
       deallocate(X42)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,M1+1:N1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N0,M1,N2,M2,M1,N1,VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S94(N2+1:M2,N0+1:M1,N2+1:M2,M1+1:N1))
       I1=K7*K0*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S94)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X43(N0+1:M1,N2+1:M2,N2+1:M2,M1+1:N1))
       X43=0.0d0
       call
     & sum3124(N0,M1,N2,M2,N2,M2,M1,N1,X43,S94,-1.000)
       deallocate(S94)
C
       call sumx2431(N2,N3,N0,N1,N2,N3,N0,N1,
     & N0,M1,N2,M2,N2,M2,M1,N1,X43,VBHPHP, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N2))
       call reorder621345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N1,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z43(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N1))
       I1=K7*K0
       I2=K8*K8*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,X43,F2,Z43)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z43,-1.000)
       deallocate(Z43)
       deallocate(X43)
C
       allocate(D1(N0+1:N2,M1+1:N1,N2+1:M2,M1+1:N1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,M1,N1,N2,M2,M1,N1,VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S95(N2+1:M2,M1+1:N1,N2+1:M2,M1+1:N1))
       I1=K7*K0*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S95)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X44(M1+1:N1,N2+1:M2,N2+1:M2,M1+1:N1))
       X44=0.0d0
       call
     & sum3124(M1,N1,N2,M2,N2,M2,M1,N1,X44,S95,-1.000)
       deallocate(S95)
C
       call sumx2431(N2,N3,N0,N1,N2,N3,N0,N1,
     & M1,N1,N2,M2,N2,M2,M1,N1,X44,VBHPHP, 1.000)
C
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N2))
       call reorder621345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,M2,N3,N1,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z44(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N1))
       I1=K7*K0
       I2=K8*K8*K9*K6
       I3=K0*K7
       call EGEMM(I1,I2,I3,X44,F2,Z44)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z44,-1.000)
       deallocate(Z44)
       deallocate(X44)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M1+1:N1))
       call reorder3142(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,M1,N1,VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q21(N0+1:M1,M1+1:N1))
       I1=K7*K5
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q21)
       deallocate(D1)
       deallocate(B2)
C
       X19=X19+Q21
       deallocate(Q21)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M1+1:N1))
       call reorder3142(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,M1,N1,VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q22(M1+1:N1,M1+1:N1))
       I1=K7*K7
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q22)
       deallocate(D1)
       deallocate(B2)
C
       X20=X20+Q22
       deallocate(Q22)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,N1+1:M2))
       call reorder1324(N2,N3,N1,N3,N0,N2,N1,N3,
     & N2,N3,N0,M1,M2,N3,N1,M2,VBPHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S96(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S96)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X45(N0+1:M1,M2+1:N3,N1+1:M2,M1+1:N2))
       X45=0.0d0
       call
     & sum4123(N0,M1,M2,N3,N1,M2,M1,N2,X45,S96, 1.000)
       deallocate(S96)
C
       call sumx4213(N0,N2,N1,N3,N0,N2,N1,N3,
     & N0,M1,M2,N3,N1,M2,M1,N2,X45,VBPHPH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder431256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z45(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9
       I2=K7*K8*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X45,F2,Z45)
       deallocate(F2)
C
       call
     & sum124635(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z45, 1.000)
       call
     & sum125634(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z45,-1.000)
       deallocate(Z45)
       deallocate(X45)
C
       allocate(D1(N2+1:N3,M1+1:N2,M2+1:N3,N1+1:M2))
       call reorder1324(N2,N3,N1,N3,N0,N2,N1,N3,
     & N2,N3,M1,N2,M2,N3,N1,M2,VBPHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S97(M1+1:N2,M1+1:N2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S97)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X46(M1+1:N2,M2+1:N3,N1+1:M2,M1+1:N2))
       X46=0.0d0
       call
     & sum4123(M1,N2,M2,N3,N1,M2,M1,N2,X46,S97, 1.000)
       deallocate(S97)
C
       call sumx4213(N0,N2,N1,N3,N0,N2,N1,N3,
     & M1,N2,M2,N3,N1,M2,M1,N2,X46,VBPHPH, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder431256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z46(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9
       I2=K7*K8*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X46,F2,Z46)
       deallocate(F2)
C
       call
     & sum124635(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z46, 1.000)
       call
     & sum125634(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z46,-1.000)
       deallocate(Z46)
       deallocate(X46)
C
       allocate(D1(N2+1:N3,N0+1:M1,N1+1:M2,N1+1:M2))
       call reorder1324(N2,N3,N1,N3,N0,N2,N1,N3,
     & N2,N3,N0,M1,N1,M2,N1,M2,VBPHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S98(M1+1:N2,N0+1:M1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S98)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X47(N0+1:M1,N1+1:M2,N1+1:M2,M1+1:N2))
       X47=0.0d0
       call
     & sum4123(N0,M1,N1,M2,N1,M2,M1,N2,X47,S98, 1.000)
       deallocate(S98)
C
       call sumx4213(N0,N2,N1,N3,N0,N2,N1,N3,
     & N0,M1,N1,M2,N1,M2,M1,N2,X47,VBPHPH, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder431256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N1,M2,M2,N3,N2,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z47(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9
       I2=K7*K8*K0*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X47,F2,Z47)
       deallocate(F2)
C
       call
     & sum124635(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z47, 1.000)
       call
     & sum125634(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z47,-1.000)
       deallocate(Z47)
       deallocate(X47)
C
       allocate(D1(N2+1:N3,M1+1:N2,N1+1:M2,N1+1:M2))
       call reorder1324(N2,N3,N1,N3,N0,N2,N1,N3,
     & N2,N3,M1,N2,N1,M2,N1,M2,VBPHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S99(M1+1:N2,M1+1:N2,N1+1:M2,N1+1:M2))
       I1=K9*K9*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S99)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X48(M1+1:N2,N1+1:M2,N1+1:M2,M1+1:N2))
       X48=0.0d0
       call
     & sum4123(M1,N2,N1,M2,N1,M2,M1,N2,X48,S99, 1.000)
       deallocate(S99)
C
       call sumx4213(N0,N2,N1,N3,N0,N2,N1,N3,
     & M1,N2,N1,M2,N1,M2,M1,N2,X48,VBPHPH, 1.000)
C
       allocate(F2(M1+1:N2,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder431256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N1,M2,M2,N3,N2,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z48(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9
       I2=K7*K8*K0*K6
       I3=K9*K8
       call EGEMM(I1,I2,I3,X48,F2,Z48)
       deallocate(F2)
C
       call
     & sum124635(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z48, 1.000)
       call
     & sum125634(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z48,-1.000)
       deallocate(Z48)
       deallocate(X48)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,M2,N3,M2,N3,N1,M2,VBPHPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S100(M2+1:N3,M2+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S100)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,M2,N3,M2,N3,N1,M2,X49,S100,-1.000)
       deallocate(S100)
C
       allocate(D1(N0+1:N2,N2+1:M2,M2+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,M2,M2,N3,N1,M2,VBPHPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S101(M2+1:N3,N2+1:M2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S101)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,M2,M2,N3,M2,N3,N1,M2,X50,S101,-1.000)
       deallocate(S101)
C
       allocate(D1(N0+1:N2,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,M2,N3,N1,M2,N1,M2,VBPHPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S102(M2+1:N3,M2+1:N3,N1+1:M2,N1+1:M2))
       I1=K9*K9*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S102)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,N1,M2,M2,N3,N1,M2,X51,S102,-1.000)
       deallocate(S102)
C
       allocate(D1(N0+1:N2,N2+1:M2,N1+1:M2,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,M2,N1,M2,N1,M2,VBPHPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S103(M2+1:N3,N2+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9*K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S103)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,M2,N1,M2,M2,N3,N1,M2,X52,S103,-1.000)
       deallocate(S103)
C
       allocate(D1(N0+1:N2,N2+1:M2,M2+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,M2,M2,N3,N1,M2,VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S104(N2+1:M2,N2+1:M2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S104)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,M2,M2,N3,N2,M2,N1,M2,X53,S104,-1.000)
       deallocate(S104)
C
       allocate(D1(N0+1:N2,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,M2,N3,N1,M2,N1,M2,VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S105(N2+1:M2,M2+1:N3,N1+1:M2,N1+1:M2))
       I1=K9*K9*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S105)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,N1,M2,N2,M2,N1,M2,X54,S105,-1.000)
       deallocate(S105)
C
       allocate(D1(N0+1:N2,N2+1:M2,N1+1:M2,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,M2,N1,M2,N1,M2,VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S106(N2+1:M2,N2+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9*K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S106)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,M2,N1,M2,N2,M2,N1,M2,X55,S106,-1.000)
       deallocate(S106)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,N3,M2,N3,N1,M2,VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q23(M2+1:N3,N1+1:M2))
       I1=K9*K6
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q23)
       deallocate(D1)
       deallocate(B2)
C
       X21=X21+Q23
       deallocate(Q23)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,N3,N1,M2,N1,M2,VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q24(N1+1:M2,N1+1:M2))
       I1=K9*K9
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q24)
       deallocate(D1)
       deallocate(B2)
C
       X22=X22+Q24
       deallocate(Q24)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:M1,M1+1:N2))
       call reorder1342(N2,N3,N0,N2,N0,N2,N0,N2,
     & N2,N3,N0,M1,N0,M1,M1,N2,VCHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S107(M1+1:N2,N0+1:M1,N0+1:M1,M1+1:N2))
       I1=K8*K5*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S107)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder2314(M1,N2,N0,M1,N0,M1,M1,N2,
     & N0,M1,N0,M1,M1,N2,M1,N2,S107,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & N0,M1,N0,M1,M2,N3,N2,M2,N1,M2,M1,N1,t3C4,F2)
       allocate(Z204(M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N1,M1+1:N2,M1+1:N2))
       I1=K8*K8
       I2=K7*K9*K0*K6
       I3=K5*K5
       call EGEMM(I1,I2,I3,D1,F2,Z204)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum123645(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z204, 0.500)
       call
     & sum123654(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z204,-0.500)
       deallocate(Z204)
       deallocate(S107)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder1342(N2,N3,N0,N2,N0,N2,N0,N2,
     & N2,N3,N0,M1,M1,N2,M1,N2,VCHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S108(M1+1:N2,N0+1:M1,M1+1:N2,M1+1:N2))
       I1=K8*K8*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S108)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder2314(M1,N2,N0,M1,M1,N2,M1,N2,
     & N0,M1,M1,N2,M1,N2,M1,N2,S108,D1)
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N2,M2,N3,N2,M2,N1,M2,M1,N1,t3C1,F2)
       allocate(Z205(M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N1,M1+1:N2,M1+1:N2))
       I1=K8*K8
       I2=K7*K9*K0*K6
       I3=K8*K5
       call EGEMM(I1,I2,I3,D1,F2,Z205)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum123645(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z205, 1.000)
       call
     & sum123654(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z205,-1.000)
       deallocate(Z205)
       deallocate(S108)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder1342(N2,N3,N0,N2,N0,N2,N0,N2,
     & N2,N3,M1,N2,M1,N2,M1,N2,VCHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S109(M1+1:N2,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S109)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder2314(M1,N2,M1,N2,M1,N2,M1,N2,
     & M1,N2,M1,N2,M1,N2,M1,N2,S109,D1)
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N2,M2,N3,N2,M2,N1,M2,M1,N1,t3C1,F2)
       allocate(Z206(M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N1,M1+1:N2,M1+1:N2))
       I1=K8*K8
       I2=K7*K9*K0*K6
       I3=K8*K8
       call EGEMM(I1,I2,I3,D1,F2,Z206)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum123645(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z206, 0.500)
       call
     & sum123654(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z206,-0.500)
       deallocate(Z206)
       deallocate(S109)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,M1,M2,N3,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S110(M2+1:N3,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S110)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X59(N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N2))
       X59=0.0d0
       call
     & sum3124(N0,M1,M2,N3,M2,N3,M1,N2,X59,S110,-1.000)
       deallocate(S110)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,M1,N2,M2,N3,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S111(M2+1:N3,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S111)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X60(M1+1:N2,M2+1:N3,M2+1:N3,M1+1:N2))
       X60=0.0d0
       call
     & sum3124(M1,N2,M2,N3,M2,N3,M1,N2,X60,S111,-1.000)
       deallocate(S111)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,M1+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,M1,N2,M2,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S112(M2+1:N3,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S112)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X61(N0+1:M1,N2+1:M2,M2+1:N3,M1+1:N2))
       X61=0.0d0
       call
     & sum3124(N0,M1,N2,M2,M2,N3,M1,N2,X61,S112,-1.000)
       deallocate(S112)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,M1,N2,N2,M2,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S113(M2+1:N3,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K8
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S113)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X62(M1+1:N2,N2+1:M2,M2+1:N3,M1+1:N2))
       X62=0.0d0
       call
     & sum3124(M1,N2,N2,M2,M2,N3,M1,N2,X62,S113,-1.000)
       deallocate(S113)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,M1,M2,N3,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S114(N2+1:M2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S114)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X63(N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       X63=0.0d0
       call
     & sum3124(N0,M1,M2,N3,N2,M2,M1,N2,X63,S114,-1.000)
       deallocate(S114)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,M1,N2,M2,N3,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S115(N2+1:M2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S115)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X64(M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       X64=0.0d0
       call
     & sum3124(M1,N2,M2,N3,N2,M2,M1,N2,X64,S115,-1.000)
       deallocate(S115)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,M1+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,M1,N2,M2,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S116(N2+1:M2,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S116)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X65(N0+1:M1,N2+1:M2,N2+1:M2,M1+1:N2))
       X65=0.0d0
       call
     & sum3124(N0,M1,N2,M2,N2,M2,M1,N2,X65,S116,-1.000)
       deallocate(S116)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,M1,N2,N2,M2,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S117(N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S117)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X66(M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       X66=0.0d0
       call
     & sum3124(M1,N2,N2,M2,N2,M2,M1,N2,X66,S117,-1.000)
       deallocate(S117)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M1+1:N2))
       call reorder3142(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q25(N0+1:M1,M1+1:N2))
       I1=K8*K5
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q25)
       deallocate(D1)
       deallocate(B2)
C
       X23=X23+Q25
       deallocate(Q25)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder3142(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q26(M1+1:N2,M1+1:N2))
       I1=K8*K8
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q26)
       deallocate(D1)
       deallocate(B2)
C
       X24=X24+Q26
       deallocate(Q26)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,M2+1:N3))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,M1,M2,N3,M2,N3,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S118(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S118)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,M2,N3,M2,N3,M1,N2,X59,S118,-1.000)
       deallocate(S118)
C
       allocate(D1(N2+1:N3,M1+1:N2,M2+1:N3,M2+1:N3))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,M1,N2,M2,N3,M2,N3,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S119(M1+1:N2,M1+1:N2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S119)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N2,M2,N3,M2,N3,M1,N2,X60,S119,-1.000)
       deallocate(S119)
C
       allocate(D1(N2+1:N3,N0+1:M1,N2+1:M2,M2+1:N3))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,M1,N2,M2,M2,N3,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S120(M1+1:N2,N0+1:M1,N2+1:M2,M2+1:N3))
       I1=K6*K0*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S120)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,N2,M2,M2,N3,M1,N2,X61,S120,-1.000)
       deallocate(S120)
C
       allocate(D1(N2+1:N3,M1+1:N2,N2+1:M2,M2+1:N3))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,M1,N2,N2,M2,M2,N3,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S121(M1+1:N2,M1+1:N2,N2+1:M2,M2+1:N3))
       I1=K6*K0*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S121)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N2,N2,M2,M2,N3,M1,N2,X62,S121,-1.000)
       deallocate(S121)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,M1,M2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S122(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S122)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,M2,N3,N2,M2,M1,N2,X63,S122,-1.000)
       deallocate(S122)
C
       allocate(D1(N2+1:N3,M1+1:N2,M2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,M1,N2,M2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S123(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S123)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N2,M2,N3,N2,M2,M1,N2,X64,S123,-1.000)
       deallocate(S123)
C
       allocate(D1(N2+1:N3,N0+1:M1,N2+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,M1,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S124(M1+1:N2,N0+1:M1,N2+1:M2,N2+1:M2))
       I1=K0*K0*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S124)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,N2,M2,N2,M2,M1,N2,X65,S124,-1.000)
       deallocate(S124)
C
       allocate(D1(N2+1:N3,M1+1:N2,N2+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,M1,N2,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S125(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S125)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N2,N2,M2,N2,M2,M1,N2,X66,S125,-1.000)
       deallocate(S125)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,M2+1:N3))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,M2,N3,M2,N3,M2,N3,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S126(N2+1:M2,M2+1:N3,M2+1:N3,M2+1:N3))
       I1=K6*K6*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S126)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X67(M2+1:N3,M2+1:N3,M2+1:N3,N2+1:M2))
       X67=0.0d0
       call
     & sum4123(M2,N3,M2,N3,M2,N3,N2,M2,X67,S126,-1.000)
       deallocate(S126)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,M2+1:N3))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,M2,N3,N2,M2,M2,N3,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S127(N2+1:M2,M2+1:N3,N2+1:M2,M2+1:N3))
       I1=K6*K0*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S127)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X68(M2+1:N3,N2+1:M2,M2+1:N3,N2+1:M2))
       X68=0.0d0
       call
     & sum4123(M2,N3,N2,M2,M2,N3,N2,M2,X68,S127,-1.000)
       deallocate(S127)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,M2+1:N3))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,M2,N2,M2,M2,N3,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S128(N2+1:M2,N2+1:M2,N2+1:M2,M2+1:N3))
       I1=K6*K0*K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S128)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X69(N2+1:M2,N2+1:M2,M2+1:N3,N2+1:M2))
       X69=0.0d0
       call
     & sum4123(N2,M2,N2,M2,M2,N3,N2,M2,X69,S128,-1.000)
       deallocate(S128)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,M2+1:N3))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,M2,N3,M2,N3,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q27(M2+1:N3,M2+1:N3))
       I1=K6*K6
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q27)
       deallocate(D1)
       deallocate(B2)
C
       X25=X25-Q27
       deallocate(Q27)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M2+1:N3))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,M2,M2,N3,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q28(N2+1:M2,M2+1:N3))
       I1=K6*K0
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q28)
       deallocate(D1)
       deallocate(B2)
C
       X26=X26-Q28
       deallocate(Q28)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,M2,N3,M2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S129(M2+1:N3,M2+1:N3,M2+1:N3,N2+1:M2))
       I1=K0*K6*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S129)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,M2,N3,M2,N3,N2,M2,X67,S129, 1.000)
       deallocate(S129)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,M2,N3,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S130(M2+1:N3,M2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S130)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,N2,M2,M2,N3,N2,M2,X68,S130, 1.000)
       deallocate(S130)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,M2,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S131(M2+1:N3,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S131)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,M2,N2,M2,M2,N3,N2,M2,X69,S131, 1.000)
       deallocate(S131)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,M2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q29(M2+1:N3,N2+1:M2))
       I1=K0*K6
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q29)
       deallocate(D1)
       deallocate(B2)
C
       X27=X27-Q29
       deallocate(Q29)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q30(N2+1:M2,N2+1:M2))
       I1=K0*K0
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q30)
       deallocate(D1)
       deallocate(B2)
C
       X28=X28-Q30
       deallocate(Q30)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder4231(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,M1,N2,VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S132(N1+1:M2,M1+1:N1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S132)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N2,N1,M2,M1,N2,M1,N1,X8,S132, 1.000)
       deallocate(S132)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,M2+1:N3))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,N3,M2,N3,VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S133(N1+1:M2,M1+1:N1,N2+1:N3,M2+1:N3))
       I1=K6*K4
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S133)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,N3,M2,N3,N1,M2,M1,N1,X9,S133, 1.000)
       deallocate(S133)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,N3,N2,M2,VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S134(N1+1:M2,M1+1:N1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S134)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,N3,N2,M2,N1,M2,M1,N1,X10,S134, 1.000)
       deallocate(S134)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S135(N1+1:M2,M1+1:N1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S135)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X90(N0+1:M1,M2+1:N3,N1+1:M2,M1+1:N1))
       X90=0.0d0
       call
     & sum3412(N0,M1,M2,N3,N1,M2,M1,N1,X90,S135, 1.000)
       deallocate(S135)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S136(N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S136)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X91(M1+1:N1,M2+1:N3,N1+1:M2,M1+1:N1))
       X91=0.0d0
       call
     & sum3412(M1,N1,M2,N3,N1,M2,M1,N1,X91,S136, 1.000)
       deallocate(S136)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S137(N1+1:M2,M1+1:N1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S137)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X92(N0+1:M1,N1+1:M2,N1+1:M2,M1+1:N1))
       X92=0.0d0
       call
     & sum3412(N0,M1,N1,M2,N1,M2,M1,N1,X92,S137, 1.000)
       deallocate(S137)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S138(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S138)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X93(M1+1:N1,N1+1:M2,N1+1:M2,M1+1:N1))
       X93=0.0d0
       call
     & sum3412(M1,N1,N1,M2,N1,M2,M1,N1,X93,S138, 1.000)
       deallocate(S138)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,M1,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,M1,N1,t2A,D2)
       allocate(Q31(M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7
       I3=K3*K3*K1
       call EGEMM(I1,I2,I3,D1,D2,Q31)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N0,M1,M1,N1,X19,Q31, 0.500)
       deallocate(Q31)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,M1,N1,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,M1,N1,t2A,D2)
       allocate(Q32(M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7
       I3=K3*K3*K1
       call EGEMM(I1,I2,I3,D1,D2,Q32)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M1,N1,M1,N1,X20,Q32, 0.500)
       deallocate(Q32)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,M2,t2A,D2)
       allocate(Q33(N1+1:M2,M2+1:N3))
       I1=K6
       I2=K9
       I3=K3*K1*K1
       call EGEMM(I1,I2,I3,D1,D2,Q33)
       deallocate(D1)
       deallocate(D2)
C
       allocate(B1(M2+1:N3,N1+1:M2))
       call reorder21(N1,M2,M2,N3,
     & M2,N3,N1,M2,Q33,B1)
       allocate(F2(M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N1))
       call reorder312456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z244(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K8*K8*K0*K6
       I3=K6
       call EGEMM(I1,I2,I3,B1,F2,Z244)
       deallocate(B1)
       deallocate(F2)
C
       call
     & sum124563(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z244, 0.500)
       deallocate(Z244)
       deallocate(Q33)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,M2,t2A,D2)
       allocate(Q34(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K3*K1*K1
       call EGEMM(I1,I2,I3,D1,D2,Q34)
       deallocate(D1)
       deallocate(D2)
C
       allocate(B1(N1+1:M2,N1+1:M2))
       call reorder21(N1,M2,N1,M2,
     & N1,M2,N1,M2,Q34,B1)
       allocate(F2(N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N1))
       call reorder312456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N1,M2,M2,N3,N2,M2,M1,N2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z245(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K8*K8*K0*K6
       I3=K9
       call EGEMM(I1,I2,I3,B1,F2,Z245)
       deallocate(B1)
       deallocate(F2)
C
       call
     & sum124563(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z245, 0.500)
       deallocate(Z245)
       deallocate(Q34)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S139(N1+1:M2,M1+1:N1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S139)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X94(N0+1:M1,M2+1:N3,N1+1:M2,M1+1:N1))
       X94=0.0d0
       call
     & sum3412(N0,M1,M2,N3,N1,M2,M1,N1,X94,S139, 1.000)
       deallocate(S139)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S140(N1+1:M2,M1+1:N1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S140)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X95(M1+1:N2,M2+1:N3,N1+1:M2,M1+1:N1))
       X95=0.0d0
       call
     & sum3412(M1,N2,M2,N3,N1,M2,M1,N1,X95,S140, 1.000)
       deallocate(S140)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S141(N1+1:M2,M1+1:N1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S141)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X96(N0+1:M1,N2+1:M2,N1+1:M2,M1+1:N1))
       X96=0.0d0
       call
     & sum3412(N0,M1,N2,M2,N1,M2,M1,N1,X96,S141, 1.000)
       deallocate(S141)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S142(N1+1:M2,M1+1:N1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S142)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X97(M1+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       X97=0.0d0
       call
     & sum3412(M1,N2,N2,M2,N1,M2,M1,N1,X97,S142, 1.000)
       deallocate(S142)
C
       allocate(B1(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,FAHP,B1)
       allocate(D2(N0+1:N1,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,M2,N1,M2,M1,N2,t2B,D2)
       allocate(S143(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K1
       call EGEMM(I1,I2,I3,B1,D2,S143)
       deallocate(B1)
       deallocate(D2)
C
       allocate(X98(N1+1:N3,N2+1:M2,N1+1:M2,M1+1:N2))
       X98=0.0d0
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X98,S143, 1.000)
       deallocate(S143)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,FAHP,B1)
       allocate(D2(N1+1:N3,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,M2,M1,N2,M1,N1,t2B,D2)
       allocate(S144(N2+1:M2,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K0
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S144)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,M1,N1,X2,S144, 1.000)
       deallocate(S144)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M1+1:N1))
       call reorder3142(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,M1,N1,VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S145(N2+1:M2,M1+1:N2,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S145)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X99(N0+1:N1,N2+1:M2,M1+1:N2,M1+1:N1))
       X99=0.0d0
       call
     & sum2314(N0,N1,N2,M2,M1,N2,M1,N1,X99,S145, 1.000)
       deallocate(S145)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M1+1:N1))
       call reorder3142(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,M1,N1,VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S146(M2+1:N3,M1+1:N2,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S146)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,M2,N3,M1,N2,M1,N1,X1,S146, 1.000)
       deallocate(S146)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,M2,VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S147(N2+1:M2,M1+1:N2,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S147)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N1,N3,N2,M2,N1,M2,M1,N2,X98,S147, 1.000)
       deallocate(S147)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,M2,VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S148(M2+1:N3,M1+1:N2,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S148)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X100(N1+1:N3,M2+1:N3,N1+1:M2,M1+1:N2))
       X100=0.0d0
       call
     & sum2413(N1,N3,M2,N3,N1,M2,M1,N2,X100,S148, 1.000)
       deallocate(S148)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,M1,N2,VBHHPH,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,N1,M2,t2B,D2)
       allocate(S149(N2+1:M2,N1+1:M2,N1+1:N3,M1+1:N2))
       I1=K8*K3
       I2=K9*K0
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S149)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N1,N3,N2,M2,N1,M2,M1,N2,X4,S149, 1.000)
       deallocate(S149)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,M1,N2,VBHHPH,D1)
       allocate(D2(N0+1:N2,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,M2,N3,N1,M2,t2B,D2)
       allocate(S150(M2+1:N3,N1+1:M2,N1+1:N3,M1+1:N2))
       I1=K8*K3
       I2=K9*K6
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S150)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N1,N3,M2,N3,N1,M2,M1,N2,X3,S150, 1.000)
       deallocate(S150)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder4231(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,M1,N2,VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S151(N2+1:M2,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S151)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,N2,M2,M1,N2,M1,N2,X89,S151, 1.000)
       deallocate(S151)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,M1+1:N2))
       call reorder3241(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N0,N1,M1,N2,VBHHPH,D1)
       allocate(D2(N0+1:N2,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,M2,N3,M1,N1,t2B,D2)
       allocate(S152(M2+1:N3,M1+1:N1,N0+1:N1,M1+1:N2))
       I1=K8*K1
       I2=K7*K6
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S152)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N1,M2,N3,M1,N2,M1,N1,X1,S152,-1.000)
       deallocate(S152)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder4231(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,M1,N2,VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S153(M2+1:N3,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S153)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,M2,N3,M1,N2,M1,N2,X88,S153, 1.000)
       deallocate(S153)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,M1+1:N2))
       call reorder3241(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N0,N1,M1,N2,VBHHPH,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,M1+1:N1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,M2,M1,N1,t2B,D2)
       allocate(S154(N2+1:M2,M1+1:N1,N0+1:N1,M1+1:N2))
       I1=K8*K1
       I2=K7*K0
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S154)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N1,N2,M2,M1,N2,M1,N1,X2,S154,-1.000)
       deallocate(S154)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,M2+1:N3))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N2,N3,N1,N3,M2,N3,VBHPPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,M2,M1,N2,t2B,D2)
       allocate(S155(N1+1:M2,M1+1:N2,N1+1:N3,M2+1:N3))
       I1=K6*K3
       I2=K8*K9
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S155)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N1,N3,M2,N3,N1,M2,M1,N2,X100,S155, 1.000)
       deallocate(S155)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,M2+1:N3))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,N3,M2,N3,VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S156(N2+1:M2,M1+1:N2,N2+1:N3,M2+1:N3))
       I1=K6*K4
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S156)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X101(N2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       X101=0.0d0
       call
     & sum3412(N2,N3,M2,N3,N2,M2,M1,N2,X101,S156, 1.000)
       deallocate(S156)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N1,M2+1:N3))
       call reorder1243(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N1,N3,N0,N1,M2,N3,VBHPPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,M1+1:N2,M1+1:N1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,M1,N2,M1,N1,t2B,D2)
       allocate(S157(M1+1:N2,M1+1:N1,N0+1:N1,M2+1:N3))
       I1=K6*K1
       I2=K7*K8
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,S157)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,N1,M2,N3,M1,N2,M1,N1,X1,S157, 1.000)
       deallocate(S157)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:M2))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N2,N3,N1,N3,N2,M2,VBHPPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,M2,M1,N2,t2B,D2)
       allocate(S158(N1+1:M2,M1+1:N2,N1+1:N3,N2+1:M2))
       I1=K0*K3
       I2=K8*K9
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S158)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N1,N3,N2,M2,N1,M2,M1,N2,X98,S158, 1.000)
       deallocate(S158)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,N3,N2,M2,VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S159(M2+1:N3,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S159)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,M2,N3,N2,M2,M1,N2,X101,S159,-1.000)
       deallocate(S159)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N1,N2+1:M2))
       call reorder1243(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N1,N3,N0,N1,N2,M2,VBHPPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,M1+1:N2,M1+1:N1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,M1,N2,M1,N1,t2B,D2)
       allocate(S160(M1+1:N2,M1+1:N1,N0+1:N1,N2+1:M2))
       I1=K0*K1
       I2=K7*K8
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,S160)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,N1,N2,M2,M1,N2,M1,N1,X2,S160, 1.000)
       deallocate(S160)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,FBHP,B1)
       allocate(D2(N2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,M2,M1,N2,M1,N1,t2B,D2)
       allocate(S161(N1+1:M2,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K9
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S161)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N2,N1,M2,M1,N2,M1,N1,X8,S161, 1.000)
       deallocate(S161)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,FBHP,B1)
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,M1,N2,M1,N2,t2C,D2)
       allocate(S162(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S162)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X6,S162,-1.000)
       deallocate(S162)
C
       allocate(B1(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,FBHP,B1)
       allocate(D2(N0+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,M2,N1,M2,M1,N1,t2B,D2)
       allocate(S163(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K2
       call EGEMM(I1,I2,I3,B1,D2,S163)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N2,N3,N2,M2,N1,M2,M1,N1,X10,S163,-1.000)
       deallocate(S163)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M1+1:N1))
       call reorder3142(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,M1,N1,VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S164(N2+1:M2,M1+1:N2,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S164)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N2,M2,M1,N2,M1,N1,X99,S164, 1.000)
       deallocate(S164)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M1+1:N1))
       call reorder3142(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,M1,N1,VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S165(M2+1:N3,M1+1:N2,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S165)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X102(N0+1:N1,M2+1:N3,M1+1:N2,M1+1:N1))
       X102=0.0d0
       call
     & sum2314(N0,N1,M2,N3,M1,N2,M1,N1,X102,S165, 1.000)
       deallocate(S165)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,M1+1:N1))
       call reorder4132(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N2,N3,N0,N2,M1,N1,VBHHHP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,M2,M1,N2,t2B,D2)
       allocate(S166(N1+1:M2,M1+1:N2,N0+1:N2,M1+1:N1))
       I1=K7*K2
       I2=K8*K9
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S166)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,N1,M2,M1,N2,M1,N1,X8,S166,-1.000)
       deallocate(S166)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,M1+1:N1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,M1,N1,VBHHHP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,M2,N3,N1,M2,t2B,D2)
       allocate(S167(M2+1:N3,N1+1:M2,N2+1:N3,M1+1:N1))
       I1=K7*K4
       I2=K9*K6
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S167)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N2,N3,M2,N3,N1,M2,M1,N1,X9,S167, 1.000)
       deallocate(S167)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,M1+1:N1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,M1,N1,VBHHHP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,N1,M2,t2B,D2)
       allocate(S168(N2+1:M2,N1+1:M2,N2+1:N3,M1+1:N1))
       I1=K7*K4
       I2=K9*K0
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S168)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N2,N3,N2,M2,N1,M2,M1,N1,X10,S168, 1.000)
       deallocate(S168)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,N3,N1,N3,N1,M2,VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S169(N2+1:M2,M1+1:N2,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S169)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N1,N3,N2,M2,N1,M2,M1,N2,X98,S169,-1.000)
       deallocate(S169)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,N3,N1,N3,N1,M2,VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S170(M2+1:N3,M1+1:N2,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S170)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N1,N3,M2,N3,N1,M2,M1,N2,X100,S170,-1.000)
       deallocate(S170)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:M2))
       call reorder1234(N2,N3,N1,N3,N0,N2,N1,N3,
     & N2,N3,N1,N3,N0,N2,N1,M2,VBPHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,M1+1:N2,M1+1:N1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,M1,N2,M1,N1,t2B,D2)
       allocate(S171(M1+1:N2,M1+1:N1,N0+1:N2,N1+1:M2))
       I1=K9*K2
       I2=K7*K8
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,S171)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,N2,N1,M2,M1,N2,M1,N1,X8,S171, 1.000)
       deallocate(S171)
C
       allocate(D1(N0+1:N2,N1+1:N3,N2+1:N3,N1+1:M2))
       call reorder3214(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N1,N3,N2,N3,N1,M2,VBPHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,M2,N3,M1,N1,t2B,D2)
       allocate(S172(M2+1:N3,M1+1:N1,N2+1:N3,N1+1:M2))
       I1=K9*K4
       I2=K7*K6
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S172)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,M2,N3,N1,M2,M1,N1,X9,S172,-1.000)
       deallocate(S172)
C
       allocate(D1(N0+1:N2,N1+1:N3,N2+1:N3,N1+1:M2))
       call reorder3214(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N1,N3,N2,N3,N1,M2,VBPHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,M1+1:N1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,M2,M1,N1,t2B,D2)
       allocate(S173(N2+1:M2,M1+1:N1,N2+1:N3,N1+1:M2))
       I1=K9*K4
       I2=K7*K0
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S173)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,N2,M2,N1,M2,M1,N1,X10,S173,-1.000)
       deallocate(S173)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,M1,N2,VCHHHP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,N2,M2,t2C,D2)
       allocate(S174(M2+1:N3,N2+1:M2,N2+1:N3,M1+1:N2))
       I1=K8*K4
       I2=K0*K6
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S174)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N2,N3,M2,N3,N2,M2,M1,N2,X7,S174, 0.500)
       deallocate(S174)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder3142(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,M1,N2,VCHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S175(N2+1:M2,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S175)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,N2,M2,M1,N2,M1,N2,X89,S175, 1.000)
       deallocate(S175)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder3142(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,M1,N2,VCHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S176(M2+1:N3,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S176)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,M2,N3,M1,N2,M1,N2,X88,S176, 1.000)
       deallocate(S176)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder4132(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,M1,N2,VCHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S177(N1+1:M2,M1+1:N1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S177)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N2,N1,M2,M1,N2,M1,N1,X8,S177,-1.000)
       deallocate(S177)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,M2+1:N3))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,M2,N3,VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S178(N2+1:M2,M1+1:N2,N2+1:N3,M2+1:N3))
       I1=K6*K4
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S178)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,N3,M2,N3,N2,M2,M1,N2,X101,S178,-1.000)
       deallocate(S178)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,M2,VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S179(M2+1:N3,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S179)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,M2,N3,N2,M2,M1,N2,X101,S179, 1.000)
       deallocate(S179)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,M2+1:N3))
       call reorder1243(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N2,N3,N0,N2,M2,N3,VCHPPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,M1,N2,M1,N2,t2C,D2)
       allocate(S180(M1+1:N2,M1+1:N2,N0+1:N2,M2+1:N3))
       I1=K6*K2
       I2=K8*K8
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S180)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,N2,M2,N3,M1,N2,M1,N2,X5,S180, 0.500)
       deallocate(S180)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,M2+1:N3))
       call reorder4213(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,M2,N3,VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S181(N1+1:M2,M1+1:N1,N2+1:N3,M2+1:N3))
       I1=K6*K4
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S181)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,N3,M2,N3,N1,M2,M1,N1,X9,S181, 1.000)
       deallocate(S181)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:M2))
       call reorder1243(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N2,N3,N0,N2,N2,M2,VCHPPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,M1,N2,M1,N2,t2C,D2)
       allocate(S182(M1+1:N2,M1+1:N2,N0+1:N2,N2+1:M2))
       I1=K0*K2
       I2=K8*K8
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S182)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,N2,N2,M2,M1,N2,M1,N2,X6,S182, 0.500)
       deallocate(S182)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder4213(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,M2,VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S183(N1+1:M2,M1+1:N1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S183)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,N3,N2,M2,N1,M2,M1,N1,X10,S183, 1.000)
       deallocate(S183)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,N0,M1,M2,N3,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder562134(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & N0,M1,N0,M1,M2,N3,N2,M2,N1,M2,M1,N2,t3B4,F2)
       allocate(S184(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S184)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X98,S184,-0.500)
       deallocate(S184)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M1,N1,M2,N3,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder562134(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,N2,M2,N1,M2,M1,N2,t3B1,F2)
       allocate(S185(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S185)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X98,S185,-1.000)
       deallocate(S185)
C
       allocate(D1(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M1,N1,M2,N3,N1,N3,VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder562134(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M1,N1,M2,N3,N2,M2,N1,M2,M1,N2,t3B1,F2)
       allocate(S186(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K6*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S186)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X98,S186,-0.500)
       deallocate(S186)
C
       allocate(D1(N0+1:M1,N0+1:M1,N1+1:M2,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,N0,M1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder562134(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & N0,M1,N0,M1,N1,M2,N2,M2,N1,M2,M1,N2,t3B4,F2)
       allocate(S187(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K9*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S187)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X98,S187,-0.500)
       deallocate(S187)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M1,N1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder562134(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,N1,M2,N2,M2,N1,M2,M1,N2,t3B1,F2)
       allocate(S188(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S188)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X98,S188,-1.000)
       deallocate(S188)
C
       allocate(D1(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M1,N1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder562134(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M1,N1,N1,M2,N2,M2,N1,M2,M1,N2,t3B1,F2)
       allocate(S189(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K9*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S189)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X98,S189,-0.500)
       deallocate(S189)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,N0,M1,M2,N3,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder562134(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & N0,M1,N0,M1,M2,N3,M2,N3,N1,M2,M1,N2,t3B4,F2)
       allocate(S190(M2+1:N3,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K6
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S190)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N2,X100,S190,-0.500)
       deallocate(S190)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M1,N1,M2,N3,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder562134(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,M2,N3,N1,M2,M1,N2,t3B1,F2)
       allocate(S191(M2+1:N3,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S191)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N2,X100,S191,-1.000)
       deallocate(S191)
C
       allocate(D1(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M1,N1,M2,N3,N1,N3,VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder562134(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M1,N1,M2,N3,M2,N3,N1,M2,M1,N2,t3B1,F2)
       allocate(S192(M2+1:N3,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K6
       I3=K6*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S192)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N2,X100,S192,-0.500)
       deallocate(S192)
C
       allocate(D1(N0+1:M1,N0+1:M1,N1+1:M2,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,N0,M1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder562134(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & N0,M1,N0,M1,N1,M2,M2,N3,N1,M2,M1,N2,t3B4,F2)
       allocate(S193(M2+1:N3,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K6
       I3=K9*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S193)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N2,X100,S193,-0.500)
       deallocate(S193)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M1,N1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder562134(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,N1,M2,M2,N3,N1,M2,M1,N2,t3B1,F2)
       allocate(S194(M2+1:N3,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K6
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S194)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N2,X100,S194,-1.000)
       deallocate(S194)
C
       allocate(D1(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M1,N1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder562134(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M1,N1,N1,M2,M2,N3,N1,M2,M1,N2,t3B1,F2)
       allocate(S195(M2+1:N3,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K6
       I3=K9*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S195)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N2,X100,S195,-0.500)
       deallocate(S195)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder523146(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N1,t3B3,F2)
       allocate(S196(N2+1:M2,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K0
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S196)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,M1,N1,X99,S196, 0.500)
       deallocate(S196)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,M2,N3,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder523146(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N1,t3B3,F2)
       allocate(S197(N2+1:M2,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K0
       I3=K6*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S197)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,M1,N1,X99,S197, 0.500)
       deallocate(S197)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder523146(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N2,M2,M1,N2,M1,N1,t3B1,F2)
       allocate(S198(N2+1:M2,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K0
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S198)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,M1,N1,X99,S198, 1.000)
       deallocate(S198)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder523146(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,N2,M2,M1,N2,M1,N1,t3B1,F2)
       allocate(S199(N2+1:M2,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K0
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S199)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,M1,N1,X99,S199, 1.000)
       deallocate(S199)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,N1,M2,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder523146(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N1,M2,N1,M2,N2,M2,M1,N2,M1,N1,t3B1,F2)
       allocate(S200(N2+1:M2,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K0
       I3=K9*K9*K5
       call EGEMM(I1,I2,I3,D1,F2,S200)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,M1,N1,X99,S200, 0.500)
       deallocate(S200)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,N1,M2,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder523146(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,N1,M2,N1,M2,N2,M2,M1,N2,M1,N1,t3B1,F2)
       allocate(S201(N2+1:M2,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K0
       I3=K9*K9*K7
       call EGEMM(I1,I2,I3,D1,F2,S201)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,M1,N1,X99,S201, 0.500)
       deallocate(S201)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S202(M2+1:N3,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S202)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,M2,N3,M1,N2,X11,S202, 1.000)
       deallocate(S202)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S203(M2+1:N3,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S203)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,M2,N3,M2,N3,M1,N2,X12,S203, 1.000)
       deallocate(S203)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S204(M2+1:N3,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S204)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N1,M2,M2,N3,M1,N2,X13,S204, 1.000)
       deallocate(S204)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S205(M2+1:N3,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S205)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,N1,M2,M2,N3,M1,N2,X14,S205, 1.000)
       deallocate(S205)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder523146(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,M2,N3,M1,N2,M1,N1,t3B1,F2)
       allocate(S206(M2+1:N3,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K6
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S206)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,M1,N2,M1,N1,X102,S206, 1.000)
       deallocate(S206)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder523146(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,M2,N3,M1,N2,M1,N1,t3B1,F2)
       allocate(S207(M2+1:N3,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K6
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S207)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,M1,N2,M1,N1,X102,S207, 1.000)
       deallocate(S207)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,N1,M2,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder523146(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N1,M2,N1,M2,M2,N3,M1,N2,M1,N1,t3B1,F2)
       allocate(S208(M2+1:N3,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K6
       I3=K9*K9*K5
       call EGEMM(I1,I2,I3,D1,F2,S208)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,M1,N2,M1,N1,X102,S208, 0.500)
       deallocate(S208)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,N1,M2,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder523146(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,N1,M2,N1,M2,M2,N3,M1,N2,M1,N1,t3B1,F2)
       allocate(S209(M2+1:N3,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K6
       I3=K9*K9*K7
       call EGEMM(I1,I2,I3,D1,F2,S209)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,M1,N2,M1,N1,X102,S209, 0.500)
       deallocate(S209)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S210(N2+1:M2,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S210)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,N2,M2,M1,N2,X15,S210, 1.000)
       deallocate(S210)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S211(N2+1:M2,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S211)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,M2,N3,N2,M2,M1,N2,X16,S211, 1.000)
       deallocate(S211)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S212(N2+1:M2,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S212)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N1,M2,N2,M2,M1,N2,X17,S212, 1.000)
       deallocate(S212)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S213(N2+1:M2,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S213)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,N1,M2,N2,M2,M1,N2,X18,S213, 1.000)
       deallocate(S213)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N0,M1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N0,M1,M2,N3,N2,M2,N1,M2,M1,N2,t3C1,F2)
       allocate(S214(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S214)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X98,S214, 1.000)
       deallocate(S214)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N2,M2,N1,M2,M1,N2,t3C1,F2)
       allocate(S215(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S215)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X98,S215, 1.000)
       deallocate(S215)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N2,M2,N1,M2,M1,N2,t3C1,F2)
       allocate(S216(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S216)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X98,S216, 1.000)
       deallocate(S216)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N2,M2,N1,M2,M1,N2,t3C1,F2)
       allocate(S217(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S217)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X98,S217, 1.000)
       deallocate(S217)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N0,M1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N0,M1,N2,M2,N2,M2,N1,M2,M1,N2,t3C1,F2)
       allocate(S218(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S218)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X98,S218, 1.000)
       deallocate(S218)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,N2,M2,N1,M2,M1,N2,t3C1,F2)
       allocate(S219(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K0*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S219)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X98,S219, 1.000)
       deallocate(S219)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N1,N2,M2,N2,M2,N1,M2,M1,N2,t3C1,F2)
       allocate(S220(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K0*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S220)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X98,S220, 1.000)
       deallocate(S220)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,N2,M2,N1,M2,M1,N2,t3C1,F2)
       allocate(S221(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K0*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S221)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X98,S221, 1.000)
       deallocate(S221)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N0,M1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder461235(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & N0,M1,N0,M1,M2,N3,M2,N3,N1,M2,M1,N2,t3C3,F2)
       allocate(S222(M2+1:N3,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K6
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S222)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N2,X100,S222, 1.000)
       deallocate(S222)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder461235(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,M2,N3,N1,M2,M1,N2,t3C3,F2)
       allocate(S223(M2+1:N3,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K6
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S223)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N2,X100,S223, 1.000)
       deallocate(S223)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder461235(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,M2,N3,N1,M2,M1,N2,t3C3,F2)
       allocate(S224(M2+1:N3,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S224)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N2,X100,S224, 1.000)
       deallocate(S224)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder461235(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,M2,N3,N1,M2,M1,N2,t3C3,F2)
       allocate(S225(M2+1:N3,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K6
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S225)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N2,X100,S225, 1.000)
       deallocate(S225)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N0,M1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder462135(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N0,M1,N2,M2,M2,N3,N1,M2,M1,N2,t3C1,F2)
       allocate(S226(M2+1:N3,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K6
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S226)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N2,X100,S226,-1.000)
       deallocate(S226)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder462135(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,M2,N3,N1,M2,M1,N2,t3C1,F2)
       allocate(S227(M2+1:N3,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K6
       I3=K0*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S227)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N2,X100,S227,-1.000)
       deallocate(S227)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder462135(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N1,N2,M2,M2,N3,N1,M2,M1,N2,t3C1,F2)
       allocate(S228(M2+1:N3,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K6
       I3=K0*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S228)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N2,X100,S228,-1.000)
       deallocate(S228)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder462135(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,M2,N3,N1,M2,M1,N2,t3C1,F2)
       allocate(S229(M2+1:N3,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K6
       I3=K0*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S229)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N2,X100,S229,-1.000)
       deallocate(S229)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N0,M1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,t3C1,F2)
       allocate(S230(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S230)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X101,S230,-1.000)
       deallocate(S230)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,t3C1,F2)
       allocate(S231(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S231)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X101,S231,-1.000)
       deallocate(S231)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,M2,N3,N2,M2,M1,N2,t3C1,F2)
       allocate(S232(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S232)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X101,S232,-1.000)
       deallocate(S232)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,M2,N3,N2,M2,M1,N2,t3C1,F2)
       allocate(S233(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S233)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X101,S233,-1.000)
       deallocate(S233)
C
       allocate(D1(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N0,M1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N0,M1,N1,M2,M2,N3,N2,M2,M1,N2,t3C1,F2)
       allocate(S234(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K9*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S234)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X101,S234,-1.000)
       deallocate(S234)
C
       allocate(D1(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,N1,M2,M2,N3,N2,M2,M1,N2,t3C1,F2)
       allocate(S235(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K9*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S235)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X101,S235,-1.000)
       deallocate(S235)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N1,N1,M2,M2,N3,N2,M2,M1,N2,t3C1,F2)
       allocate(S236(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S236)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X101,S236,-1.000)
       deallocate(S236)
C
       allocate(D1(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,N1,M2,M2,N3,N2,M2,M1,N2,t3C1,F2)
       allocate(S237(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K9*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S237)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X101,S237,-1.000)
       deallocate(S237)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,M1,N0,M1,VBHHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,M1+1:N2,M1+1:N1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,M1,N2,M1,N1,t2B,D2)
       allocate(S238(M1+1:N2,M1+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K7*K8
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,S238)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N0,M1,M1,N2,M1,N1,X33,S238, 1.000)
       deallocate(S238)
C
       allocate(D1(N2+1:N3,N1+1:N3,M1+1:N2,N0+1:M1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,M1,N2,N0,M1,VBHHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,M1+1:N2,M1+1:N1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,M1,N2,M1,N1,t2B,D2)
       allocate(S239(M1+1:N2,M1+1:N1,M1+1:N2,N0+1:M1))
       I1=K5*K8
       I2=K7*K8
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,S239)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,N0,M1,M1,N2,M1,N1,X34,S239, 1.000)
       deallocate(S239)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,M1,M1,N1,VBHHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,M1+1:N2,M1+1:N1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,M1,N2,M1,N1,t2B,D2)
       allocate(S240(M1+1:N2,M1+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5
       I2=K7*K8
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,S240)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M1,N1,M1,N2,M1,N1,X35,S240, 1.000)
       deallocate(S240)
C
       allocate(D1(N2+1:N3,N1+1:N3,M1+1:N2,M1+1:N1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,M1,N2,M1,N1,VBHHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,M1+1:N2,M1+1:N1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,M1,N2,M1,N1,t2B,D2)
       allocate(S241(M1+1:N2,M1+1:N1,M1+1:N2,M1+1:N1))
       I1=K7*K8
       I2=K7*K8
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,S241)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,M1,N1,M1,N2,M1,N1,X36,S241, 1.000)
       deallocate(S241)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(S242(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S242)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X6,S242,-1.000)
       deallocate(S242)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(S243(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K6*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S243)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X6,S243,-1.000)
       deallocate(S243)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(S244(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S244)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X6,S244,-1.000)
       deallocate(S244)
C
       allocate(D1(M1+1:N1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,N2,M2,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(S245(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K6*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S245)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X6,S245,-1.000)
       deallocate(S245)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(S246(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S246)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X6,S246,-1.000)
       deallocate(S246)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,N1,M2,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(S247(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S247)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X6,S247,-1.000)
       deallocate(S247)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(S248(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S248)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X6,S248,-1.000)
       deallocate(S248)
C
       allocate(D1(M1+1:N1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,N2,M2,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,N1,M2,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(S249(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K9*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S249)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X6,S249,-1.000)
       deallocate(S249)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder3241(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,M2,N3,M1,N1,t2B,D2)
       allocate(S250(M2+1:N3,M1+1:N1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K7*K6
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S250)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,M2,N3,M1,N1,X70,S250,-1.000)
       deallocate(S250)
C
       allocate(D1(N0+1:N2,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder3241(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,M1,N1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,M2,N3,M1,N1,t2B,D2)
       allocate(S251(M2+1:N3,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K7*K6
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S251)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,M2,N3,M2,N3,M1,N1,X71,S251,-1.000)
       deallocate(S251)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder3241(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N0,M1,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,M2,N3,M1,N1,t2B,D2)
       allocate(S252(M2+1:N3,M1+1:N1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K7*K6
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S252)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N2,M2,M2,N3,M1,N1,X72,S252,-1.000)
       deallocate(S252)
C
       allocate(D1(N0+1:N2,N1+1:N3,M1+1:N1,N2+1:M2))
       call reorder3241(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,M1,N1,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,M2,N3,M1,N1,t2B,D2)
       allocate(S253(M2+1:N3,M1+1:N1,M1+1:N1,N2+1:M2))
       I1=K0*K7
       I2=K7*K6
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S253)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,N2,M2,M2,N3,M1,N1,X73,S253,-1.000)
       deallocate(S253)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder623145(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,M2,N3,M1,N2,M1,N2,t3C1,F2)
       allocate(S254(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S254)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X5,S254, 1.000)
       deallocate(S254)
C
       allocate(D1(M1+1:N1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,N2,M2,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder623145(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,M2,N3,M2,N3,M1,N2,M1,N2,t3C1,F2)
       allocate(S255(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K6*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S255)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X5,S255, 1.000)
       deallocate(S255)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder613245(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,M2,N3,M1,N2,M1,N2,t3C3,F2)
       allocate(S256(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S256)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X5,S256,-1.000)
       deallocate(S256)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder613245(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,N1,M2,M2,N3,M1,N2,M1,N2,t3C3,F2)
       allocate(S257(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S257)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X5,S257,-1.000)
       deallocate(S257)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder623145(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,M2,N3,M1,N2,M1,N2,t3C1,F2)
       allocate(S258(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S258)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X5,S258, 1.000)
       deallocate(S258)
C
       allocate(D1(M1+1:N1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,N2,M2,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder623145(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,N1,M2,M2,N3,M1,N2,M1,N2,t3C1,F2)
       allocate(S259(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K9*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S259)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X5,S259, 1.000)
       deallocate(S259)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder3241(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,M1+1:N1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,M2,M1,N1,t2B,D2)
       allocate(S260(N2+1:M2,M1+1:N1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K7*K0
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S260)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,N2,M2,M1,N1,X74,S260,-1.000)
       deallocate(S260)
C
       allocate(D1(N0+1:N2,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder3241(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,M1,N1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,M1+1:N1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,M2,M1,N1,t2B,D2)
       allocate(S261(N2+1:M2,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K7*K0
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S261)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,M2,N3,N2,M2,M1,N1,X75,S261,-1.000)
       deallocate(S261)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder3241(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N0,M1,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,M1+1:N1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,M2,M1,N1,t2B,D2)
       allocate(S262(N2+1:M2,M1+1:N1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K7*K0
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S262)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N2,M2,N2,M2,M1,N1,X76,S262,-1.000)
       deallocate(S262)
C
       allocate(D1(N0+1:N2,N1+1:N3,M1+1:N1,N2+1:M2))
       call reorder3241(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,M1,N1,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,M1+1:N1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,M2,M1,N1,t2B,D2)
       allocate(S263(N2+1:M2,M1+1:N1,M1+1:N1,N2+1:M2))
       I1=K0*K7
       I2=K7*K0
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S263)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,N2,M2,N2,M2,M1,N1,X77,S263,-1.000)
       deallocate(S263)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S264(N1+1:M2,M1+1:N1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S264)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X29(N0+1:M1,M2+1:N3,N1+1:M2,M1+1:N1))
       X29=0.0d0
       call
     & sum3412(N0,M1,M2,N3,N1,M2,M1,N1,X29,S264,-1.000)
       deallocate(S264)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,M1,M2,N3,N1,M2,M1,N1,X29,VAHPHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z29(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K8*K8*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X29,F2,Z29)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z29,-1.000)
       deallocate(Z29)
       deallocate(X29)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S265(N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S265)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X30(M1+1:N1,M2+1:N3,N1+1:M2,M1+1:N1))
       X30=0.0d0
       call
     & sum3412(M1,N1,M2,N3,N1,M2,M1,N1,X30,S265,-1.000)
       deallocate(S265)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,M2,N3,N1,M2,M1,N1,X30,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z30(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K8*K8*K0*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X30,F2,Z30)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z30,-1.000)
       deallocate(Z30)
       deallocate(X30)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S266(N1+1:M2,M1+1:N1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S266)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X31(N0+1:M1,N1+1:M2,N1+1:M2,M1+1:N1))
       X31=0.0d0
       call
     & sum3412(N0,M1,N1,M2,N1,M2,M1,N1,X31,S266,-1.000)
       deallocate(S266)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,M1,N1,M2,N1,M2,M1,N1,X31,VAHPHP, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N1,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z31(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K8*K8*K0*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X31,F2,Z31)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z31,-1.000)
       deallocate(Z31)
       deallocate(X31)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S267(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S267)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X32(M1+1:N1,N1+1:M2,N1+1:M2,M1+1:N1))
       X32=0.0d0
       call
     & sum3412(M1,N1,N1,M2,N1,M2,M1,N1,X32,S267,-1.000)
       deallocate(S267)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,N1,M2,N1,M2,M1,N1,X32,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N1,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z32(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K8*K8*K0*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X32,F2,Z32)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z32,-1.000)
       deallocate(Z32)
       deallocate(X32)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,M1,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,M1,N1,t2B,D2)
       allocate(Q35(M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7
       I3=K3*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q35)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N0,M1,M1,N1,X19,Q35, 1.000)
       deallocate(Q35)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,M1,N1,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,M1,N1,t2B,D2)
       allocate(Q36(M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7
       I3=K3*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q36)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M1,N1,M1,N1,X20,Q36, 1.000)
       deallocate(Q36)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(S268(N2+1:M2,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K0
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S268)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,M1,N1,X99,S268, 1.000)
       deallocate(S268)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(S269(N2+1:M2,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K0
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S269)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,M1,N1,X99,S269, 1.000)
       deallocate(S269)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N2,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(S270(N2+1:M2,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K0
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S270)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,M1,N1,X99,S270, 1.000)
       deallocate(S270)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,N2,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(S271(N2+1:M2,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K0
       I3=K6*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S271)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,M1,N1,X99,S271, 1.000)
       deallocate(S271)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N2,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(S272(N2+1:M2,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K0
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S272)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,M1,N1,X99,S272, 1.000)
       deallocate(S272)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N1,M2,N2,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(S273(N2+1:M2,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K0
       I3=K9*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S273)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,M1,N1,X99,S273, 1.000)
       deallocate(S273)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N2,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(S274(N2+1:M2,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K0
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S274)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,M1,N1,X99,S274, 1.000)
       deallocate(S274)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,N1,M2,N2,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(S275(N2+1:M2,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K0
       I3=K9*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S275)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,M1,N1,X99,S275, 1.000)
       deallocate(S275)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S276(M2+1:N3,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S276)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3412(M2,N3,M1,N2,N0,M1,M2,N3,
     & N0,M1,M2,N3,M2,N3,M1,N2,S276,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N2,M2,N1,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z385(N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K7*K8*K9*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z385)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z385,-1.000)
       call
     & sum235614(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z385, 1.000)
       deallocate(Z385)
       deallocate(S276)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S277(M2+1:N3,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S277)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3412(M2,N3,M1,N2,M1,N2,M2,N3,
     & M1,N2,M2,N3,M2,N3,M1,N2,S277,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N2,M2,N1,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z386(N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K7*K8*K9*K0
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z386)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z386,-1.000)
       call
     & sum235614(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z386, 1.000)
       deallocate(Z386)
       deallocate(S277)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S278(M2+1:N3,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S278)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder3412(M2,N3,M1,N2,N0,M1,N2,M2,
     & N0,M1,N2,M2,M2,N3,M1,N2,S278,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N2,M2,N1,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z387(N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K7*K8*K9*K0
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z387)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z387,-1.000)
       call
     & sum235614(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z387, 1.000)
       deallocate(Z387)
       deallocate(S278)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S279(M2+1:N3,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S279)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder3412(M2,N3,M1,N2,M1,N2,N2,M2,
     & M1,N2,N2,M2,M2,N3,M1,N2,S279,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,N2,M2,N1,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z388(N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K7*K8*K9*K0
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z388)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z388,-1.000)
       call
     & sum235614(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z388, 1.000)
       deallocate(Z388)
       deallocate(S279)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder423156(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,M2,N3,M1,N2,M1,N1,t3C1,F2)
       allocate(S280(M2+1:N3,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K6
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S280)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,M1,N2,M1,N1,X102,S280,-1.000)
       deallocate(S280)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder423156(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,M2,N3,M1,N2,M1,N1,t3C1,F2)
       allocate(S281(M2+1:N3,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K6
       I3=K6*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S281)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,M1,N2,M1,N1,X102,S281,-1.000)
       deallocate(S281)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder413256(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,M2,N3,M1,N2,M1,N1,t3C3,F2)
       allocate(S282(M2+1:N3,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K6
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S282)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,M1,N2,M1,N1,X102,S282, 1.000)
       deallocate(S282)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder413256(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N1,M2,M2,N3,M1,N2,M1,N1,t3C3,F2)
       allocate(S283(M2+1:N3,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K6
       I3=K9*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S283)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,M1,N2,M1,N1,X102,S283, 1.000)
       deallocate(S283)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder423156(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,M2,N3,M1,N2,M1,N1,t3C1,F2)
       allocate(S284(M2+1:N3,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K6
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S284)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,M1,N2,M1,N1,X102,S284,-1.000)
       deallocate(S284)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder423156(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,N1,M2,M2,N3,M1,N2,M1,N1,t3C1,F2)
       allocate(S285(M2+1:N3,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K6
       I3=K9*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S285)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,M1,N2,M1,N1,X102,S285,-1.000)
       deallocate(S285)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S286(N2+1:M2,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S286)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,M1,M2,N3,
     & N0,M1,M2,N3,N2,M2,M1,N2,S286,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder412356(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,M1,N2,M1,N1,t3C3,F2)
       allocate(Z395(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K7*K8*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z395)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum134625(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z395, 1.000)
       call
     & sum135624(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z395,-1.000)
       deallocate(Z395)
       deallocate(S286)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S287(N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S287)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,M1,N2,M2,N3,
     & M1,N2,M2,N3,N2,M2,M1,N2,S287,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder412356(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,M2,N3,N1,M2,M1,N2,M1,N1,t3C3,F2)
       allocate(Z396(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K7*K8*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z396)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum134625(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z396, 1.000)
       call
     & sum135624(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z396,-1.000)
       deallocate(Z396)
       deallocate(S287)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S288(N2+1:M2,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S288)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,M1,N2,M2,
     & N0,M1,N2,M2,N2,M2,M1,N2,S288,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder421356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N1,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z397(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K7*K8*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z397)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum134625(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z397,-1.000)
       call
     & sum135624(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z397, 1.000)
       deallocate(Z397)
       deallocate(S288)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S289(N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S289)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,M1,N2,N2,M2,
     & M1,N2,N2,M2,N2,M2,M1,N2,S289,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder421356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,N1,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z398(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K7*K8*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z398)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum134625(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z398,-1.000)
       call
     & sum135624(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z398, 1.000)
       deallocate(Z398)
       deallocate(S289)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder4132(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,M2,M1,N2,t2B,D2)
       allocate(S290(N1+1:M2,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K9
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S290)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,N1,M2,M1,N2,X78,S290, 1.000)
       deallocate(S290)
C
       allocate(D1(N0+1:N1,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder4132(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,M1,N2,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,M2,M1,N2,t2B,D2)
       allocate(S291(N1+1:M2,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K9
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S291)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,M2,N3,N1,M2,M1,N2,X79,S291, 1.000)
       deallocate(S291)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder4132(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N0,M1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,M2,M1,N2,t2B,D2)
       allocate(S292(N1+1:M2,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K8*K9
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S292)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N1,M2,N1,M2,M1,N2,X80,S292, 1.000)
       deallocate(S292)
C
       allocate(D1(N0+1:N1,N2+1:N3,M1+1:N2,N1+1:M2))
       call reorder4132(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,M1,N2,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,M2,M1,N2,t2B,D2)
       allocate(S293(N1+1:M2,M1+1:N2,M1+1:N2,N1+1:M2))
       I1=K9*K8
       I2=K8*K9
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S293)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,N1,M2,N1,M2,M1,N2,X81,S293, 1.000)
       deallocate(S293)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:M1))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,M1,VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,M1,N2,t2B,D2)
       allocate(Q37(M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8
       I3=K3*K4*K1
       call EGEMM(I1,I2,I3,D1,D2,Q37)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N0,M1,M1,N2,X82,Q37, 1.000)
       deallocate(Q37)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,M1,N2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,M1,N2,t2B,D2)
       allocate(Q38(M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8
       I3=K3*K4*K1
       call EGEMM(I1,I2,I3,D1,D2,Q38)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M1,N2,M1,N2,X83,Q38, 1.000)
       deallocate(Q38)
C
       allocate(D1(N0+1:N2,N0+1:N1,M2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,M2,N3,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,M2,N3,N1,M2,t2B,D2)
       allocate(S294(M2+1:N3,N1+1:M2,M2+1:N3,M2+1:N3))
       I1=K6*K6
       I2=K9*K6
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S294)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M2,N3,M2,N3,M2,N3,N1,M2,X49,S294, 1.000)
       deallocate(S294)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:M2,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,M2,N3,N1,M2,t2B,D2)
       allocate(S295(M2+1:N3,N1+1:M2,N2+1:M2,M2+1:N3))
       I1=K6*K0
       I2=K9*K6
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S295)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,M2,M2,N3,M2,N3,N1,M2,X50,S295, 1.000)
       deallocate(S295)
C
       allocate(D1(N0+1:N2,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,M2,N3,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,M2,N3,N1,M2,t2B,D2)
       allocate(S296(M2+1:N3,N1+1:M2,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K9*K6
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S296)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M2,N3,N1,M2,M2,N3,N1,M2,X51,S296, 1.000)
       deallocate(S296)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,M2,N3,N1,M2,t2B,D2)
       allocate(S297(M2+1:N3,N1+1:M2,N2+1:M2,N1+1:M2))
       I1=K9*K0
       I2=K9*K6
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S297)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,M2,N1,M2,M2,N3,N1,M2,X52,S297, 1.000)
       deallocate(S297)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,M2,N3,t2B,D2)
       allocate(Q39(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q39)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M2,N3,M2,N3,X84,Q39,-1.000)
       deallocate(Q39)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,M2,N3,t2B,D2)
       allocate(Q40(M2+1:N3,N2+1:M2))
       I1=K0
       I2=K6
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q40)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N2,M2,M2,N3,X85,Q40,-1.000)
       deallocate(Q40)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:M2,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,N1,M2,t2B,D2)
       allocate(S298(N2+1:M2,N1+1:M2,N2+1:M2,M2+1:N3))
       I1=K6*K0
       I2=K9*K0
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S298)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,M2,M2,N3,N2,M2,N1,M2,X53,S298, 1.000)
       deallocate(S298)
C
       allocate(D1(N0+1:N2,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,M2,N3,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,N1,M2,t2B,D2)
       allocate(S299(N2+1:M2,N1+1:M2,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K9*K0
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S299)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M2,N3,N1,M2,N2,M2,N1,M2,X54,S299, 1.000)
       deallocate(S299)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,N1,M2,t2B,D2)
       allocate(S300(N2+1:M2,N1+1:M2,N2+1:M2,N1+1:M2))
       I1=K9*K0
       I2=K9*K0
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S300)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,M2,N1,M2,N2,M2,N1,M2,X55,S300, 1.000)
       deallocate(S300)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,M2,t2B,D2)
       allocate(Q41(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q41)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M2,N3,N2,M2,X86,Q41,-1.000)
       deallocate(Q41)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,M2,t2B,D2)
       allocate(Q42(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q42)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N2,M2,N2,M2,X87,Q42,-1.000)
       deallocate(Q42)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,M2,t2B,D2)
       allocate(Q43(N1+1:M2,M2+1:N3))
       I1=K6
       I2=K9
       I3=K4*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q43)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M2,N3,N1,M2,X21,Q43,-1.000)
       deallocate(Q43)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,M2,t2B,D2)
       allocate(Q44(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K4*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q44)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N1,M2,N1,M2,X22,Q44,-1.000)
       deallocate(Q44)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,N0,M1,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,t3D,F2)
       allocate(S301(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S301)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X101,S301,-0.500)
       deallocate(S301)
C
       allocate(D1(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M1,N2,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,t3D,F2)
       allocate(S302(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S302)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X101,S302,-1.000)
       deallocate(S302)
C
       allocate(D1(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M1,N2,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,t3D,F2)
       allocate(S303(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S303)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X101,S303,-0.500)
       deallocate(S303)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,N0,M1,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder452136(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N0,M1,N2,M2,M2,N3,N2,M2,M1,N2,t3D,F2)
       allocate(S304(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S304)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X101,S304, 0.500)
       deallocate(S304)
C
       allocate(D1(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M1,N2,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder452136(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M1,N2,N2,M2,M2,N3,N2,M2,M1,N2,t3D,F2)
       allocate(S305(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K0*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S305)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X101,S305, 1.000)
       deallocate(S305)
C
       allocate(D1(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M1,N2,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder452136(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M1,N2,N2,M2,M2,N3,N2,M2,M1,N2,t3D,F2)
       allocate(S306(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K0*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S306)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X101,S306, 0.500)
       deallocate(S306)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M2,N3,M2,N3,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(S307(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S307)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X6,S307,-0.500)
       deallocate(S307)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M2,N3,M2,N3,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(S308(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S308)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X6,S308,-0.500)
       deallocate(S308)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(S309(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K0*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S309)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X6,S309,-1.000)
       deallocate(S309)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(S310(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K0*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S310)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X6,S310,-1.000)
       deallocate(S310)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,N2,M2,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(S311(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K0*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S311)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X6,S311,-0.500)
       deallocate(S311)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,N2,M2,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(S312(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K0*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S312)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X6,S312,-0.500)
       deallocate(S312)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder413256(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,t3D,F2)
       allocate(S313(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K0*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S313)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X5,S313, 1.000)
       deallocate(S313)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder413256(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,t3D,F2)
       allocate(S314(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K0*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S314)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X5,S314, 1.000)
       deallocate(S314)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,N2,M2,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder423156(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,N2,M2,M2,N3,M1,N2,M1,N2,t3D,F2)
       allocate(S315(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K0*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S315)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X5,S315,-0.500)
       deallocate(S315)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,N2,M2,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder423156(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,N2,M2,M2,N3,M1,N2,M1,N2,t3D,F2)
       allocate(S316(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K0*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S316)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X5,S316,-0.500)
       deallocate(S316)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S317(N1+1:M2,M1+1:N1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S317)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,N1,M2,M1,N1,X94,S317, 1.000)
       deallocate(S317)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S318(N1+1:M2,M1+1:N1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S318)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,M2,N3,N1,M2,M1,N1,X95,S318, 1.000)
       deallocate(S318)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S319(N1+1:M2,M1+1:N1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S319)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N2,M2,N1,M2,M1,N1,X96,S319, 1.000)
       deallocate(S319)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S320(N1+1:M2,M1+1:N1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S320)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,N2,M2,N1,M2,M1,N1,X97,S320, 1.000)
       deallocate(S320)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N0,M1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder452136(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N0,M1,M2,N3,N2,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(S321(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S321)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N1,M2,M1,N1,X10,S321,-1.000)
       deallocate(S321)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder452136(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N0,M1,M2,N3,N2,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(S322(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S322)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N1,M2,M1,N1,X10,S322,-1.000)
       deallocate(S322)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder452136(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,N2,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(S323(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S323)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N1,M2,M1,N1,X10,S323,-1.000)
       deallocate(S323)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder452136(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,M2,N3,N2,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(S324(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S324)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N1,M2,M1,N1,X10,S324,-1.000)
       deallocate(S324)
C
       allocate(D1(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N0,M1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder452136(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N0,M1,N1,M2,N2,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(S325(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K9*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S325)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N1,M2,M1,N1,X10,S325,-1.000)
       deallocate(S325)
C
       allocate(D1(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder452136(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N0,M1,N1,M2,N2,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(S326(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K9*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S326)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N1,M2,M1,N1,X10,S326,-1.000)
       deallocate(S326)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder452136(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,N1,M2,N2,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(S327(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S327)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N1,M2,M1,N1,X10,S327,-1.000)
       deallocate(S327)
C
       allocate(D1(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder452136(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,N1,M2,N2,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(S328(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K9*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S328)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N1,M2,M1,N1,X10,S328,-1.000)
       deallocate(S328)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N0,M1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder452136(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N0,M1,M2,N3,M2,N3,N1,M2,M1,N1,t3B1,F2)
       allocate(S329(M2+1:N3,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K6
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S329)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N1,M2,M1,N1,X9,S329,-1.000)
       deallocate(S329)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder452136(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N0,M1,M2,N3,M2,N3,N1,M2,M1,N1,t3B1,F2)
       allocate(S330(M2+1:N3,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K6
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S330)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N1,M2,M1,N1,X9,S330,-1.000)
       deallocate(S330)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder452136(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,M2,N3,N1,M2,M1,N1,t3B1,F2)
       allocate(S331(M2+1:N3,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S331)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N1,M2,M1,N1,X9,S331,-1.000)
       deallocate(S331)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder452136(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,M2,N3,M2,N3,N1,M2,M1,N1,t3B1,F2)
       allocate(S332(M2+1:N3,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K6
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S332)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N1,M2,M1,N1,X9,S332,-1.000)
       deallocate(S332)
C
       allocate(D1(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N0,M1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder452136(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N0,M1,N1,M2,M2,N3,N1,M2,M1,N1,t3B1,F2)
       allocate(S333(M2+1:N3,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K6
       I3=K9*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S333)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N1,M2,M1,N1,X9,S333,-1.000)
       deallocate(S333)
C
       allocate(D1(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder452136(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N0,M1,N1,M2,M2,N3,N1,M2,M1,N1,t3B1,F2)
       allocate(S334(M2+1:N3,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K6
       I3=K9*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S334)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N1,M2,M1,N1,X9,S334,-1.000)
       deallocate(S334)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder452136(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,N1,M2,M2,N3,N1,M2,M1,N1,t3B1,F2)
       allocate(S335(M2+1:N3,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K6
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S335)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N1,M2,M1,N1,X9,S335,-1.000)
       deallocate(S335)
C
       allocate(D1(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder452136(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,N1,M2,M2,N3,N1,M2,M1,N1,t3B1,F2)
       allocate(S336(M2+1:N3,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K6
       I3=K9*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S336)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N1,M2,M1,N1,X9,S336,-1.000)
       deallocate(S336)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder512346(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,M1,N2,M1,N1,t3B1,F2)
       allocate(S337(N1+1:M2,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K9
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S337)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X103(N0+1:N2,N1+1:M2,M1+1:N2,M1+1:N1))
       X103=0.0d0
       call
     & sum2341(N0,N2,N1,M2,M1,N2,M1,N1,X103,S337, 1.000)
       deallocate(S337)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder512346(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N1,M2,M1,N2,M1,N1,t3B1,F2)
       allocate(S338(N1+1:M2,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K9
       I3=K6*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S338)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N1,M2,M1,N2,M1,N1,X103,S338, 1.000)
       deallocate(S338)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder512346(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,N1,M2,M1,N2,M1,N1,t3B1,F2)
       allocate(S339(N1+1:M2,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K9
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S339)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N1,M2,M1,N2,M1,N1,X103,S339, 1.000)
       deallocate(S339)
C
       allocate(D1(M1+1:N1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,N2,M2,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder512346(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,N2,M2,M2,N3,N1,M2,M1,N2,M1,N1,t3B1,F2)
       allocate(S340(N1+1:M2,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K9
       I3=K6*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S340)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N1,M2,M1,N2,M1,N1,X103,S340, 1.000)
       deallocate(S340)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder512346(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,M1,N2,M1,N1,t3B1,F2)
       allocate(S341(N1+1:M2,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K9
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S341)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N1,M2,M1,N2,M1,N1,X103,S341, 1.000)
       deallocate(S341)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder512346(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,N1,M2,M1,N2,M1,N1,t3B1,F2)
       allocate(S342(N1+1:M2,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K9
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S342)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N1,M2,M1,N2,M1,N1,X103,S342, 1.000)
       deallocate(S342)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder512346(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,N1,M2,N1,M2,M1,N2,M1,N1,t3B1,F2)
       allocate(S343(N1+1:M2,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K9
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S343)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N1,M2,M1,N2,M1,N1,X103,S343, 1.000)
       deallocate(S343)
C
       allocate(D1(M1+1:N1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,N2,M2,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder512346(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,N2,M2,N1,M2,N1,M2,M1,N2,M1,N1,t3B1,F2)
       allocate(S344(N1+1:M2,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K9
       I3=K9*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S344)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N1,M2,M1,N2,M1,N1,X103,S344, 1.000)
       deallocate(S344)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S345(M2+1:N3,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S345)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,M2,N3,M1,N2,X11,S345, 1.000)
       deallocate(S345)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S346(M2+1:N3,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S346)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,M2,N3,M2,N3,M1,N2,X12,S346, 1.000)
       deallocate(S346)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S347(M2+1:N3,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S347)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N1,M2,M2,N3,M1,N2,X13,S347, 1.000)
       deallocate(S347)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S348(M2+1:N3,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S348)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,N1,M2,M2,N3,M1,N2,X14,S348, 1.000)
       deallocate(S348)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S349(N2+1:M2,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S349)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,N2,M2,M1,N2,X15,S349, 1.000)
       deallocate(S349)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S350(N2+1:M2,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S350)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,M2,N3,N2,M2,M1,N2,X16,S350, 1.000)
       deallocate(S350)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S351(N2+1:M2,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S351)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N1,M2,N2,M2,M1,N2,X17,S351, 1.000)
       deallocate(S351)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S352(N2+1:M2,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S352)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,N1,M2,N2,M2,M1,N2,X18,S352, 1.000)
       deallocate(S352)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,N0,M1,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & N0,M1,N0,M1,M2,N3,N2,M2,N1,M2,M1,N1,t3C4,F2)
       allocate(S353(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S353)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N1,M2,M1,N1,X10,S353, 0.500)
       deallocate(S353)
C
       allocate(D1(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M1,N2,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N2,M2,N3,N2,M2,N1,M2,M1,N1,t3C1,F2)
       allocate(S354(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K6*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S354)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N1,M2,M1,N1,X10,S354, 1.000)
       deallocate(S354)
C
       allocate(D1(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M1,N2,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N2,M2,N3,N2,M2,N1,M2,M1,N1,t3C1,F2)
       allocate(S355(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K6*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S355)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N1,M2,M1,N1,X10,S355, 0.500)
       deallocate(S355)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,N0,M1,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & N0,M1,N0,M1,N2,M2,N2,M2,N1,M2,M1,N1,t3C4,F2)
       allocate(S356(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S356)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N1,M2,M1,N1,X10,S356, 0.500)
       deallocate(S356)
C
       allocate(D1(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M1,N2,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N2,N2,M2,N2,M2,N1,M2,M1,N1,t3C1,F2)
       allocate(S357(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K0*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S357)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N1,M2,M1,N1,X10,S357, 1.000)
       deallocate(S357)
C
       allocate(D1(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M1,N2,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N2,N2,M2,N2,M2,N1,M2,M1,N1,t3C1,F2)
       allocate(S358(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K0*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S358)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N1,M2,M1,N1,X10,S358, 0.500)
       deallocate(S358)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,N0,M1,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder451236(M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,M1,N1,
     & N0,M1,N0,M1,M2,N3,M2,N3,N1,M2,M1,N1,t3C2,F2)
       allocate(S359(M2+1:N3,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K6
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S359)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N1,M2,M1,N1,X9,S359, 0.500)
       deallocate(S359)
C
       allocate(D1(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M1,N2,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder451236(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N2,M2,N3,M2,N3,N1,M2,M1,N1,t3C3,F2)
       allocate(S360(M2+1:N3,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K6
       I3=K6*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S360)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N1,M2,M1,N1,X9,S360, 1.000)
       deallocate(S360)
C
       allocate(D1(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M1,N2,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder451236(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N2,M2,N3,M2,N3,N1,M2,M1,N1,t3C3,F2)
       allocate(S361(M2+1:N3,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K6
       I3=K6*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S361)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N1,M2,M1,N1,X9,S361, 0.500)
       deallocate(S361)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,N0,M1,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder452136(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & N0,M1,N0,M1,N2,M2,M2,N3,N1,M2,M1,N1,t3C4,F2)
       allocate(S362(M2+1:N3,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K6
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S362)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N1,M2,M1,N1,X9,S362,-0.500)
       deallocate(S362)
C
       allocate(D1(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M1,N2,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder452136(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N2,N2,M2,M2,N3,N1,M2,M1,N1,t3C1,F2)
       allocate(S363(M2+1:N3,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K6
       I3=K0*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S363)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N1,M2,M1,N1,X9,S363,-1.000)
       deallocate(S363)
C
       allocate(D1(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M1,N2,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder452136(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N2,N2,M2,M2,N3,N1,M2,M1,N1,t3C1,F2)
       allocate(S364(M2+1:N3,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K6
       I3=K0*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S364)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N1,M2,M1,N1,X9,S364,-0.500)
       deallocate(S364)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:M1,N0+1:M1))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,M1,N0,M1,VCHHPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,M1,N2,M1,N2,t2C,D2)
       allocate(S365(M1+1:N2,M1+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K8*K8
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S365)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X56(N0+1:M1,N0+1:M1,M1+1:N2,M1+1:N2))
       X56=0.0d0
       call
     & sum3412(N0,M1,N0,M1,M1,N2,M1,N2,X56,S365, 0.500)
       deallocate(S365)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:M1,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,M1,M1,N2,VCHHPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,M1,N2,M1,N2,t2C,D2)
       allocate(S366(M1+1:N2,M1+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5
       I2=K8*K8
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S366)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X57(N0+1:M1,M1+1:N2,M1+1:N2,M1+1:N2))
       X57=0.0d0
       call
     & sum3412(N0,M1,M1,N2,M1,N2,M1,N2,X57,S366, 0.500)
       deallocate(S366)
C
       allocate(D1(N2+1:N3,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,M1,N2,M1,N2,VCHHPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,M1,N2,M1,N2,t2C,D2)
       allocate(S367(M1+1:N2,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8
       I2=K8*K8
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S367)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X58(M1+1:N2,M1+1:N2,M1+1:N2,M1+1:N2))
       X58=0.0d0
       call
     & sum3412(M1,N2,M1,N2,M1,N2,M1,N2,X58,S367, 0.500)
       deallocate(S367)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M2,N3,M2,N3,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder412356(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,M1,N2,M1,N1,t3C3,F2)
       allocate(S368(N1+1:M2,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K9
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S368)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N1,M2,M1,N2,M1,N1,X103,S368,-0.500)
       deallocate(S368)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M2,N3,M2,N3,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder412356(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,M2,N3,N1,M2,M1,N2,M1,N1,t3C3,F2)
       allocate(S369(N1+1:M2,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K9
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S369)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N1,M2,M1,N2,M1,N1,X103,S369,-0.500)
       deallocate(S369)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N2,M2,N1,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(S370(N1+1:M2,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K9
       I3=K0*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S370)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N1,M2,M1,N2,M1,N1,X103,S370,-1.000)
       deallocate(S370)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N2,M2,N1,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(S371(N1+1:M2,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K9
       I3=K0*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S371)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N1,M2,M1,N2,M1,N1,X103,S371,-1.000)
       deallocate(S371)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,N2,M2,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N2,M2,N1,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(S372(N1+1:M2,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K9
       I3=K0*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S372)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N1,M2,M1,N2,M1,N1,X103,S372,-0.500)
       deallocate(S372)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,N2,M2,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,N2,M2,N1,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(S373(N1+1:M2,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K9
       I3=K0*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S373)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N1,M2,M1,N2,M1,N1,X103,S373,-0.500)
       deallocate(S373)
C
       allocate(D2(N0+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,M2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(Z454(M2+1:N3,N2+1:M2,M1+1:N2,N1+1:M2,M1+1:N2,M1+1:N1))
       I1=K7*K8*K9
       I2=K8*K0*K6
       I3=K2
       call EGEMM(I1,I2,I3,X103,D2,Z454)
       deallocate(D2)
C
       call
     & sum125346(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z454,-1.000)
       call
     & sum124356(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z454, 1.000)
       deallocate(Z454)
       deallocate(X103)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S374(M2+1:N3,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S374)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,M2,N3,M1,N2,X59,S374,-1.000)
       deallocate(S374)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S375(M2+1:N3,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S375)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,M2,N3,M2,N3,M1,N2,X60,S375,-1.000)
       deallocate(S375)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S376(M2+1:N3,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S376)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N2,M2,M2,N3,M1,N2,X61,S376,-1.000)
       deallocate(S376)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S377(M2+1:N3,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S377)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,N2,M2,M2,N3,M1,N2,X62,S377,-1.000)
       deallocate(S377)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S378(N2+1:M2,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S378)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,N2,M2,M1,N2,X63,S378,-1.000)
       deallocate(S378)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S379(N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S379)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,M2,N3,N2,M2,M1,N2,X64,S379,-1.000)
       deallocate(S379)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S380(N2+1:M2,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S380)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N2,M2,N2,M2,M1,N2,X65,S380,-1.000)
       deallocate(S380)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S381(N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S381)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,N2,M2,N2,M2,M1,N2,X66,S381,-1.000)
       deallocate(S381)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,M1,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,M1,N2,t2C,D2)
       allocate(Q45(M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8
       I3=K4*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q45)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N0,M1,M1,N2,X23,Q45, 0.500)
       deallocate(Q45)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,M1,N2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,M1,N2,t2C,D2)
       allocate(Q46(M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8
       I3=K4*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q46)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M1,N2,M1,N2,X24,Q46, 0.500)
       deallocate(Q46)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,N2,M2,t2C,D2)
       allocate(S382(M2+1:N3,N2+1:M2,M2+1:N3,M2+1:N3))
       I1=K6*K6
       I2=K0*K6
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S382)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M2,N3,M2,N3,M2,N3,N2,M2,X67,S382, 0.500)
       deallocate(S382)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,N2,M2,t2C,D2)
       allocate(S383(M2+1:N3,N2+1:M2,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K0*K6
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S383)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M2,N3,N2,M2,M2,N3,N2,M2,X68,S383, 0.500)
       deallocate(S383)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,M2,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,N2,M2,t2C,D2)
       allocate(S384(M2+1:N3,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0
       I2=K0*K6
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S384)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,M2,N2,M2,M2,N3,N2,M2,X69,S384, 0.500)
       deallocate(S384)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,M2,N3,t2C,D2)
       allocate(Q47(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,Q47)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M2,N3,M2,N3,X25,Q47,-0.500)
       deallocate(Q47)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,M2,N3,t2C,D2)
       allocate(Q48(M2+1:N3,N2+1:M2))
       I1=K0
       I2=K6
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,Q48)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N2,M2,M2,N3,X26,Q48,-0.500)
       deallocate(Q48)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,M2,t2C,D2)
       allocate(Q49(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,Q49)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M2,N3,N2,M2,X27,Q49,-0.500)
       deallocate(Q49)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,M2,t2C,D2)
       allocate(Q50(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,Q50)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N2,M2,N2,M2,X28,Q50,-0.500)
       deallocate(Q50)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,M1+1:N2))
       call reorder2341(N0,N2,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,N2,N0,N1,M1,N2,VBHHPH,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S385(M1+1:N1,N0+1:N2,N0+1:N1,M1+1:N2))
       I1=K8*K1*K2
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S385)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,M1+1:N1))
       call reorder2341(M1,N1,N0,N2,N0,N1,M1,N2,
     & N0,N2,N0,N1,M1,N2,M1,N1,S385,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S407(M2+1:N3,N0+1:N1,M1+1:N2,M1+1:N1))
       I1=K7*K8*K1
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S407)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,M2,N3,M1,N2,M1,N1,X1,S407,-1.000)
       deallocate(S407)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,M1+1:N1))
       call reorder2341(M1,N1,N0,N2,N0,N1,M1,N2,
     & N0,N2,N0,N1,M1,N2,M1,N1,S385,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S408(N2+1:M2,N0+1:N1,M1+1:N2,M1+1:N1))
       I1=K7*K8*K1
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S408)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N2,M2,M1,N2,M1,N1,X2,S408,-1.000)
       deallocate(S408)
C
       allocate(D1(N0+1:N1,N0+1:N2,M1+1:N2,M1+1:N1))
       call reorder3241(M1,N1,N0,N2,N0,N1,M1,N2,
     & N0,N1,N0,N2,M1,N2,M1,N1,S385,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S386(N1+1:M2,N0+1:N2,M1+1:N2,M1+1:N1))
       I1=K7*K8*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S386)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,N1,M2,M1,N2,M1,N1,X8,S386,-1.000)
       deallocate(S386)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:N1,M2+1:N3))
       call reorder2341(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,M1,N0,N1,M2,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S391(M1+1:N1,N0+1:M1,N0+1:N1,M2+1:N3))
       I1=K6*K1*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S391)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,M1+1:N1))
       call reorder3241(M1,N1,N0,M1,N0,N1,M2,N3,
     & N0,N1,N0,M1,M2,N3,M1,N1,S391,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S392(N1+1:M2,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S392)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,N1,M2,M1,N1,X90,S392,-1.000)
       deallocate(S392)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z238(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K8*K8*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X90,F2,Z238)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z238, 1.000)
       deallocate(Z238)
       deallocate(X90)
C
       allocate(D1(N1+1:N3,M1+1:N1,N0+1:N1,M2+1:N3))
       call reorder2341(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,M1,N1,N0,N1,M2,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S393(M1+1:N1,M1+1:N1,N0+1:N1,M2+1:N3))
       I1=K6*K1*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S393)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder3241(M1,N1,M1,N1,N0,N1,M2,N3,
     & N0,N1,M1,N1,M2,N3,M1,N1,S393,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S394(N1+1:M2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S394)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,M2,N3,N1,M2,M1,N1,X91,S394,-1.000)
       deallocate(S394)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z239(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K8*K8*K0*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X91,F2,Z239)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z239, 1.000)
       deallocate(Z239)
       deallocate(X91)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:N1,N1+1:M2))
       call reorder2341(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,M1,N0,N1,N1,M2,VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S395(M1+1:N1,N0+1:M1,N0+1:N1,N1+1:M2))
       I1=K9*K1*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S395)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,M1+1:N1))
       call reorder3241(M1,N1,N0,M1,N0,N1,N1,M2,
     & N0,N1,N0,M1,N1,M2,M1,N1,S395,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S396(N1+1:M2,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S396)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N1,M2,N1,M2,M1,N1,X92,S396,-1.000)
       deallocate(S396)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N1,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z240(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K8*K8*K0*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X92,F2,Z240)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z240, 1.000)
       deallocate(Z240)
       deallocate(X92)
C
       allocate(D1(N1+1:N3,M1+1:N1,N0+1:N1,N1+1:M2))
       call reorder2341(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,M1,N1,N0,N1,N1,M2,VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S397(M1+1:N1,M1+1:N1,N0+1:N1,N1+1:M2))
       I1=K9*K1*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S397)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       call reorder3241(M1,N1,M1,N1,N0,N1,N1,M2,
     & N0,N1,M1,N1,N1,M2,M1,N1,S397,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S398(N1+1:M2,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S398)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,N1,M2,N1,M2,M1,N1,X93,S398,-1.000)
       deallocate(S398)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N1,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z241(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K8*K8*K0*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X93,F2,Z241)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z241, 1.000)
       deallocate(Z241)
       deallocate(X93)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,N1,N3,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q51(N0+1:M1,N1+1:N3))
       I1=K3*K5
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q51)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,N0+1:M1))
       call reorder21(N0,M1,N1,N3,
     & N1,N3,N0,M1,Q51,B1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(Q52(M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q52)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(N0,M1,M1,N1,X19,Q52, 1.000)
       deallocate(Q52)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,N1,N3,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q53(M1+1:N1,N1+1:N3))
       I1=K3*K7
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q53)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,M1+1:N1))
       call reorder21(M1,N1,N1,N3,
     & N1,N3,M1,N1,Q53,B1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(Q54(M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q54)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(M1,N1,M1,N1,X20,Q54, 1.000)
       deallocate(Q54)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M2+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,M2,N3,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q55(N0+1:N1,M2+1:N3))
       I1=K6*K1
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q55)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(Q56(N1+1:M2,M2+1:N3))
       I1=K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,Q55,B2,Q56)
       deallocate(B2)
C
       call
     & sum21(M2,N3,N1,M2,X21,Q56,-1.000)
       deallocate(Q56)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:M2))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N1,M2,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q57(N0+1:N1,N1+1:M2))
       I1=K9*K1
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q57)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(Q58(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,Q57,B2,Q58)
       deallocate(B2)
C
       call
     & sum21(N1,M2,N1,M2,X22,Q58,-1.000)
       deallocate(Q58)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:N1,M2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,N0,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S399(M1+1:N1,N0+1:M1,N0+1:N1,M2+1:N3))
       I1=K6*K1*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S399)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,M1+1:N1))
       call reorder3241(M1,N1,N0,M1,N0,N1,M2,N3,
     & N0,N1,N0,M1,M2,N3,M1,N1,S399,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S400(N1+1:M2,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S400)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,N1,M2,M1,N1,X94,S400,-1.000)
       deallocate(S400)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z246(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K8*K8*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X94,F2,Z246)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z246, 1.000)
       deallocate(Z246)
       deallocate(X94)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:N1,M2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,N0,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S401(M1+1:N1,M1+1:N2,N0+1:N1,M2+1:N3))
       I1=K6*K1*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S401)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,M1+1:N1))
       call reorder3241(M1,N1,M1,N2,N0,N1,M2,N3,
     & N0,N1,M1,N2,M2,N3,M1,N1,S401,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S402(N1+1:M2,M1+1:N2,M2+1:N3,M1+1:N1))
       I1=K7*K6*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S402)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M2,N3,N1,M2,M1,N1,X95,S402,-1.000)
       deallocate(S402)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z247(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K8*K8*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X95,F2,Z247)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z247, 1.000)
       deallocate(Z247)
       deallocate(X95)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:N1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,N0,N1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S403(M1+1:N1,N0+1:M1,N0+1:N1,N2+1:M2))
       I1=K0*K1*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S403)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,N2+1:M2,M1+1:N1))
       call reorder3241(M1,N1,N0,M1,N0,N1,N2,M2,
     & N0,N1,N0,M1,N2,M2,M1,N1,S403,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S404(N1+1:M2,N0+1:M1,N2+1:M2,M1+1:N1))
       I1=K7*K0*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S404)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N2,M2,N1,M2,M1,N1,X96,S404,-1.000)
       deallocate(S404)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z248(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K8*K8*K0*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,X96,F2,Z248)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z248,-1.000)
       deallocate(Z248)
       deallocate(X96)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:N1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,N0,N1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S405(M1+1:N1,M1+1:N2,N0+1:N1,N2+1:M2))
       I1=K0*K1*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S405)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,M1+1:N1))
       call reorder3241(M1,N1,M1,N2,N0,N1,N2,M2,
     & N0,N1,M1,N2,N2,M2,M1,N1,S405,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S406(N1+1:M2,M1+1:N2,N2+1:M2,M1+1:N1))
       I1=K7*K0*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S406)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,N2,M2,N1,M2,M1,N1,X97,S406,-1.000)
       deallocate(S406)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z249(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K8*K8*K0*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X97,F2,Z249)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z249,-1.000)
       deallocate(Z249)
       deallocate(X97)
C
       allocate(D1(N1+1:N3,N0+1:N1,N2+1:N3,M2+1:N3))
       call reorder2413(N2,N3,N1,N3,N2,N3,N0,N1,
     & N1,N3,N0,N1,N2,N3,M2,N3,VBHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S387(M1+1:N1,N0+1:N1,N2+1:N3,M2+1:N3))
       I1=K6*K4*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S387)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N0+1:N1,M2+1:N3,M1+1:N1))
       call reorder3241(M1,N1,N0,N1,N2,N3,M2,N3,
     & N2,N3,N0,N1,M2,N3,M1,N1,S387,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S409(M1+1:N2,N0+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K1
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S409)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N1,M2,N3,M1,N2,M1,N1,X1,S409, 1.000)
       deallocate(S409)
C
       allocate(D1(N0+1:N1,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder2341(M1,N1,N0,N1,N2,N3,M2,N3,
     & N0,N1,N2,N3,M2,N3,M1,N1,S387,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S388(N1+1:M2,N2+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K4
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S388)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,N3,M2,N3,N1,M2,M1,N1,X9,S388,-1.000)
       deallocate(S388)
C
       allocate(D1(N1+1:N3,N0+1:N1,N2+1:N3,N2+1:M2))
       call reorder2413(N2,N3,N1,N3,N2,N3,N0,N1,
     & N1,N3,N0,N1,N2,N3,N2,M2,VBHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S389(M1+1:N1,N0+1:N1,N2+1:N3,N2+1:M2))
       I1=K0*K4*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S389)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:M2,M1+1:N1))
       call reorder3241(M1,N1,N0,N1,N2,N3,N2,M2,
     & N2,N3,N0,N1,N2,M2,M1,N1,S389,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S410(M1+1:N2,N0+1:N1,N2+1:M2,M1+1:N1))
       I1=K7*K0*K1
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S410)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N1,N2,M2,M1,N2,M1,N1,X2,S410, 1.000)
       deallocate(S410)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:M2,M1+1:N1))
       call reorder2341(M1,N1,N0,N1,N2,N3,N2,M2,
     & N0,N1,N2,N3,N2,M2,M1,N1,S389,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S390(N1+1:M2,N2+1:N3,N2+1:M2,M1+1:N1))
       I1=K7*K0*K4
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S390)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,N3,N2,M2,N1,M2,M1,N1,X10,S390,-1.000)
       deallocate(S390)
C
       allocate(D1(N0+1:N1,N0+1:N2,N1+1:N3,M1+1:N2))
       call reorder4321(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N0,N2,N1,N3,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S411(N1+1:M2,N0+1:N2,N1+1:N3,M1+1:N2))
       I1=K8*K3*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S411)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,M1+1:N2))
       call reorder2314(N1,M2,N0,N2,N1,N3,M1,N2,
     & N0,N2,N1,N3,N1,M2,M1,N2,S411,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S413(N2+1:M2,N1+1:N3,N1+1:M2,M1+1:N2))
       I1=K8*K9*K3
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S413)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,N2,M2,N1,M2,M1,N2,X4,S413, 1.000)
       deallocate(S413)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,M1+1:N2))
       call reorder2314(N1,M2,N0,N2,N1,N3,M1,N2,
     & N0,N2,N1,N3,N1,M2,M1,N2,S411,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S412(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N2))
       I1=K8*K9*K3
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S412)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,N1,M2,M1,N2,X3,S412, 1.000)
       deallocate(S412)
C
       allocate(D1(N2+1:N3,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,N1,N1,N3,M2,N3,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S414(M1+1:N2,N0+1:N1,N1+1:N3,M2+1:N3))
       I1=K6*K3*K1
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S414)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N1,N1,N3,M2,N3,
     & N0,N1,N1,N3,M2,N3,M1,N2,S414,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S415(N1+1:M2,N1+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S415)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,N3,M2,N3,N1,M2,M1,N2,X3,S415,-1.000)
       deallocate(S415)
C
       allocate(D1(N2+1:N3,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,N1,N1,N3,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S416(M1+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       I1=K0*K3*K1
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S416)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N1,N1,N3,N2,M2,
     & N0,N1,N1,N3,N2,M2,M1,N2,S416,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S417(N1+1:M2,N1+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S417)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,N3,N2,M2,N1,M2,M1,N2,X4,S417,-1.000)
       deallocate(S417)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,M1+1:N1))
       call reorder1342(N2,N3,N0,N1,N0,N2,N0,N1,
     & N2,N3,N0,N2,N0,N1,M1,N1,VBHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S418(M1+1:N2,N0+1:N2,N0+1:N1,M1+1:N1))
       I1=K7*K1*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S418)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,M1+1:N1))
       call reorder2314(M1,N2,N0,N2,N0,N1,M1,N1,
     & N0,N2,N0,N1,M1,N2,M1,N1,S418,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S491(M2+1:N3,N0+1:N1,M1+1:N2,M1+1:N1))
       I1=K7*K8*K1
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S491)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,M2,N3,M1,N2,M1,N1,X1,S491,-1.000)
       deallocate(S491)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,M1+1:N1))
       call reorder2314(M1,N2,N0,N2,N0,N1,M1,N1,
     & N0,N2,N0,N1,M1,N2,M1,N1,S418,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S492(N2+1:M2,N0+1:N1,M1+1:N2,M1+1:N1))
       I1=K7*K8*K1
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S492)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N2,M2,M1,N2,M1,N1,X2,S492,-1.000)
       deallocate(S492)
C
       allocate(D1(N0+1:N1,N0+1:N2,M1+1:N2,M1+1:N1))
       call reorder3214(M1,N2,N0,N2,N0,N1,M1,N1,
     & N0,N1,N0,N2,M1,N2,M1,N1,S418,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S419(N1+1:M2,N0+1:N2,M1+1:N2,M1+1:N1))
       I1=K7*K8*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S419)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,N1,M2,M1,N2,M1,N1,X8,S419,-1.000)
       deallocate(S419)
C
       allocate(D1(N1+1:N3,N0+1:N2,N2+1:N3,N1+1:M2))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,N0,N2,N2,N3,N1,M2,VBPHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S423(M1+1:N1,N0+1:N2,N2+1:N3,N1+1:M2))
       I1=K9*K4*K2
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S423)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder2341(M1,N1,N0,N2,N2,N3,N1,M2,
     & N0,N2,N2,N3,N1,M2,M1,N1,S423,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S425(M2+1:N3,N2+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S425)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N1,M2,M1,N1,X9,S425,-1.000)
       deallocate(S425)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder2341(M1,N1,N0,N2,N2,N3,N1,M2,
     & N0,N2,N2,N3,N1,M2,M1,N1,S423,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S426(N2+1:M2,N2+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S426)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,N1,M2,M1,N1,X10,S426,-1.000)
       deallocate(S426)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:M2,M1+1:N1))
       call reorder3241(M1,N1,N0,N2,N2,N3,N1,M2,
     & N2,N3,N0,N2,N1,M2,M1,N1,S423,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S424(M1+1:N2,N0+1:N2,N1+1:M2,M1+1:N1))
       I1=K7*K9*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S424)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N2,N1,M2,M1,N2,M1,N1,X8,S424, 1.000)
       deallocate(S424)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:M1,N2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,N0,M1,N2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S427(M1+1:N1,N0+1:M1,N0+1:M1,N2+1:N3))
       I1=K4*K5*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S427)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder4231(M1,N1,N0,M1,N0,M1,N2,N3,
     & N2,N3,N0,M1,N0,M1,M1,N1,S427,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S428(M1+1:N2,N0+1:M1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S428)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N0,M1,M1,N2,M1,N1,X33,S428, 1.000)
       deallocate(S428)
C
       call sumx3412(N0,N2,N0,N1,N0,N2,N0,N1,
     & N0,M1,N0,M1,M1,N2,M1,N1,X33,VBHHHH, 1.000)
C
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N0,M1,M2,N3,N2,M2,N1,M2,M1,N2,t3C1,F2)
       allocate(Z33(M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N2,M1+1:N1))
       I1=K7*K8
       I2=K8*K9*K0*K6
       I3=K5*K5
       call EGEMM(I1,I2,I3,X33,F2,Z33)
       deallocate(F2)
C
       V3C=V3C-Z33
       call
     & sum123546(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z33, 1.000)
       deallocate(Z33)
       deallocate(X33)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:M1,N2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,N0,M1,N2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S429(M1+1:N1,M1+1:N2,N0+1:M1,N2+1:N3))
       I1=K4*K5*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S429)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:M1,M1+1:N1))
       call reorder4231(M1,N1,M1,N2,N0,M1,N2,N3,
     & N2,N3,M1,N2,N0,M1,M1,N1,S429,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S430(M1+1:N2,M1+1:N2,N0+1:M1,M1+1:N1))
       I1=K7*K5*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S430)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,N0,M1,M1,N2,M1,N1,X34,S430, 1.000)
       deallocate(S430)
C
       call sumx3412(N0,N2,N0,N1,N0,N2,N0,N1,
     & M1,N2,N0,M1,M1,N2,M1,N1,X34,VBHHHH, 1.000)
C
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N2,M2,N1,M2,M1,N2,t3C1,F2)
       allocate(Z34(M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N2,M1+1:N1))
       I1=K7*K8
       I2=K8*K9*K0*K6
       I3=K5*K8
       call EGEMM(I1,I2,I3,X34,F2,Z34)
       deallocate(F2)
C
       V3C=V3C-Z34
       call
     & sum123546(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z34, 1.000)
       deallocate(Z34)
       deallocate(X34)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,N2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,M1,N1,N2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S431(M1+1:N1,N0+1:M1,M1+1:N1,N2+1:N3))
       I1=K4*K7*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S431)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N1,M1+1:N1))
       call reorder4231(M1,N1,N0,M1,M1,N1,N2,N3,
     & N2,N3,N0,M1,M1,N1,M1,N1,S431,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S432(M1+1:N2,N0+1:M1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S432)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M1,N1,M1,N2,M1,N1,X35,S432, 1.000)
       deallocate(S432)
C
       call sumx3412(N0,N2,N0,N1,N0,N2,N0,N1,
     & N0,M1,M1,N1,M1,N2,M1,N1,X35,VBHHHH, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N2,M2,N1,M2,M1,N2,t3C1,F2)
       allocate(Z35(M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N2,M1+1:N1))
       I1=K7*K8
       I2=K8*K9*K0*K6
       I3=K7*K5
       call EGEMM(I1,I2,I3,X35,F2,Z35)
       deallocate(F2)
C
       V3C=V3C-Z35
       call
     & sum123546(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z35, 1.000)
       deallocate(Z35)
       deallocate(X35)
C
       allocate(D1(N1+1:N3,M1+1:N2,M1+1:N1,N2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,M1,N1,N2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S433(M1+1:N1,M1+1:N2,M1+1:N1,N2+1:N3))
       I1=K4*K7*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S433)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N1,M1+1:N1))
       call reorder4231(M1,N1,M1,N2,M1,N1,N2,N3,
     & N2,N3,M1,N2,M1,N1,M1,N1,S433,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S434(M1+1:N2,M1+1:N2,M1+1:N1,M1+1:N1))
       I1=K7*K7*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S434)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M1,N1,M1,N2,M1,N1,X36,S434, 1.000)
       deallocate(S434)
C
       call sumx3412(N0,N2,N0,N1,N0,N2,N0,N1,
     & M1,N2,M1,N1,M1,N2,M1,N1,X36,VBHHHH, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N2,M2,N1,M2,M1,N2,t3C1,F2)
       allocate(Z36(M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N2,M1+1:N1))
       I1=K7*K8
       I2=K8*K9*K0*K6
       I3=K7*K8
       call EGEMM(I1,I2,I3,X36,F2,Z36)
       deallocate(F2)
C
       V3C=V3C-Z36
       call
     & sum123546(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z36, 1.000)
       deallocate(Z36)
       deallocate(X36)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:M1,M2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,N2,N0,M1,M2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S435(M1+1:N1,N0+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5*K2
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S435)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N1))
       call reorder2341(M1,N1,N0,N2,N0,M1,M2,N3,
     & N0,N2,N0,M1,M2,N3,M1,N1,S435,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S443(N2+1:M2,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S443)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,N2,M2,M1,N1,X74,S443,-1.000)
       deallocate(S443)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N2))
       call reorder612345(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,M1,N2,M1,N2,t3C3,F2)
       allocate(Z108(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N1))
       I1=K7*K0
       I2=K8*K8*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X74,F2,Z108)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z108, 1.000)
       deallocate(Z108)
       deallocate(X74)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N1))
       call reorder2341(M1,N1,N0,N2,N0,M1,M2,N3,
     & N0,N2,N0,M1,M2,N3,M1,N1,S435,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S436(M2+1:N3,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S436)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,M2,N3,M1,N1,X70,S436,-1.000)
       deallocate(S436)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N2))
       call reorder612345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z104(N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K8*K8*K9*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,X70,F2,Z104)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z104,-1.000)
       deallocate(Z104)
       deallocate(X70)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:M1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,N2,N0,M1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S439(M1+1:N1,N0+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5*K2
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S439)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,M1+1:N1))
       call reorder2341(M1,N1,N0,N2,N0,M1,N2,M2,
     & N0,N2,N0,M1,N2,M2,M1,N1,S439,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S445(N2+1:M2,N0+1:M1,N2+1:M2,M1+1:N1))
       I1=K7*K0*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S445)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N2,M2,N2,M2,M1,N1,X76,S445,-1.000)
       deallocate(S445)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N2))
       call reorder621345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N1,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z110(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N1))
       I1=K7*K0
       I2=K8*K8*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,X76,F2,Z110)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z110,-1.000)
       deallocate(Z110)
       deallocate(X76)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,M1+1:N1))
       call reorder2341(M1,N1,N0,N2,N0,M1,N2,M2,
     & N0,N2,N0,M1,N2,M2,M1,N1,S439,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S440(M2+1:N3,N0+1:M1,N2+1:M2,M1+1:N1))
       I1=K7*K0*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S440)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N2,M2,M2,N3,M1,N1,X72,S440,-1.000)
       deallocate(S440)
C
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N2))
       call reorder612345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N2,M2,N1,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z106(N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K8*K8*K9*K0
       I3=K0*K5
       call EGEMM(I1,I2,I3,X72,F2,Z106)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z106,-1.000)
       deallocate(Z106)
       deallocate(X72)
C
       allocate(D1(N1+1:N3,N0+1:N2,M1+1:N1,M2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,N2,M1,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S437(M1+1:N1,N0+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7*K2
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S437)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder2341(M1,N1,N0,N2,M1,N1,M2,N3,
     & N0,N2,M1,N1,M2,N3,M1,N1,S437,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S444(N2+1:M2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S444)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,M2,N3,N2,M2,M1,N1,X75,S444,-1.000)
       deallocate(S444)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N2))
       call reorder612345(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N1,M2,M1,N2,M1,N2,t3C3,F2)
       allocate(Z109(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N1))
       I1=K7*K0
       I2=K8*K8*K9*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X75,F2,Z109)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z109, 1.000)
       deallocate(Z109)
       deallocate(X75)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder2341(M1,N1,N0,N2,M1,N1,M2,N3,
     & N0,N2,M1,N1,M2,N3,M1,N1,S437,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S438(M2+1:N3,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S438)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,M2,N3,M2,N3,M1,N1,X71,S438,-1.000)
       deallocate(S438)
C
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N2))
       call reorder612345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z105(N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K8*K8*K9*K0
       I3=K6*K7
       call EGEMM(I1,I2,I3,X71,F2,Z105)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z105,-1.000)
       deallocate(Z105)
       deallocate(X71)
C
       allocate(D1(N1+1:N3,N0+1:N2,M1+1:N1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,N2,M1,N1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S441(M1+1:N1,N0+1:N2,M1+1:N1,N2+1:M2))
       I1=K0*K7*K2
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S441)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N1,N2+1:M2,M1+1:N1))
       call reorder2341(M1,N1,N0,N2,M1,N1,N2,M2,
     & N0,N2,M1,N1,N2,M2,M1,N1,S441,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S446(N2+1:M2,M1+1:N1,N2+1:M2,M1+1:N1))
       I1=K7*K0*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S446)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,N2,M2,N2,M2,M1,N1,X77,S446,-1.000)
       deallocate(S446)
C
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N2))
       call reorder621345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,M2,N3,N1,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z111(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,M1+1:N1))
       I1=K7*K0
       I2=K8*K8*K9*K6
       I3=K0*K7
       call EGEMM(I1,I2,I3,X77,F2,Z111)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z111,-1.000)
       deallocate(Z111)
       deallocate(X77)
C
       allocate(D1(N0+1:N2,M1+1:N1,N2+1:M2,M1+1:N1))
       call reorder2341(M1,N1,N0,N2,M1,N1,N2,M2,
     & N0,N2,M1,N1,N2,M2,M1,N1,S441,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S442(M2+1:N3,M1+1:N1,N2+1:M2,M1+1:N1))
       I1=K7*K0*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S442)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,N2,M2,M2,N3,M1,N1,X73,S442,-1.000)
       deallocate(S442)
C
       allocate(F2(M1+1:N1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N2))
       call reorder612345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,N2,M2,N1,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z107(N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K8*K8*K9*K0
       I3=K0*K7
       call EGEMM(I1,I2,I3,X73,F2,Z107)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z107,-1.000)
       deallocate(Z107)
       deallocate(X73)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,N1,N3,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q59(N0+1:M1,N1+1:N3))
       I1=K3*K5
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q59)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,N0+1:M1))
       call reorder21(N0,M1,N1,N3,
     & N1,N3,N0,M1,Q59,B1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(Q60(M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q60)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(N0,M1,M1,N1,X19,Q60, 1.000)
       deallocate(Q60)
C
       call sumx21(N0,N1,N0,N1,
     & N0,M1,M1,N1,X19,FAHH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N2))
       call reorder612345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z19(M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N2,M1+1:N1))
       I1=K7
       I2=K8*K8*K9*K0*K6
       I3=K5
       call EGEMM(I1,I2,I3,X19,F2,Z19)
       deallocate(F2)
C
       V3C=V3C-Z19
       deallocate(Z19)
       deallocate(X19)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,N1,N3,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q61(M1+1:N1,N1+1:N3))
       I1=K3*K7
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q61)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,M1+1:N1))
       call reorder21(M1,N1,N1,N3,
     & N1,N3,M1,N1,Q61,B1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(Q62(M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q62)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(M1,N1,M1,N1,X20,Q62, 1.000)
       deallocate(Q62)
C
       call sumx21(N0,N1,N0,N1,
     & M1,N1,M1,N1,X20,FAHH, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N2))
       call reorder612345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z20(M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N2,M1+1:N1))
       I1=K7
       I2=K8*K8*K9*K0*K6
       I3=K7
       call EGEMM(I1,I2,I3,X20,F2,Z20)
       deallocate(F2)
C
       V3C=V3C-Z20
       deallocate(Z20)
       deallocate(X20)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:N1,M2+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,M1,N0,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S447(M1+1:N2,N0+1:M1,N0+1:N1,M2+1:N3))
       I1=K6*K1*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S447)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder3241(M1,N2,N0,M1,N0,N1,M2,N3,
     & N0,N1,N0,M1,M2,N3,M1,N2,S447,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S448(N1+1:M2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S448)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,N1,M2,M1,N2,X78,S448, 1.000)
       deallocate(S448)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder431256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z112(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9
       I2=K7*K8*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X78,F2,Z112)
       deallocate(F2)
C
       call
     & sum124635(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z112,-1.000)
       call
     & sum125634(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z112, 1.000)
       deallocate(Z112)
       deallocate(X78)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:N1,M2+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,M1,N2,N0,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S449(M1+1:N2,M1+1:N2,N0+1:N1,M2+1:N3))
       I1=K6*K1*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S449)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder3241(M1,N2,M1,N2,N0,N1,M2,N3,
     & N0,N1,M1,N2,M2,N3,M1,N2,S449,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S450(N1+1:M2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S450)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M2,N3,N1,M2,M1,N2,X79,S450, 1.000)
       deallocate(S450)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder431256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z113(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9
       I2=K7*K8*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X79,F2,Z113)
       deallocate(F2)
C
       call
     & sum124635(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z113,-1.000)
       call
     & sum125634(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z113, 1.000)
       deallocate(Z113)
       deallocate(X79)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:N1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,M1,N0,N1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S451(M1+1:N2,N0+1:M1,N0+1:N1,N1+1:M2))
       I1=K9*K1*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S451)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,M1+1:N2))
       call reorder3241(M1,N2,N0,M1,N0,N1,N1,M2,
     & N0,N1,N0,M1,N1,M2,M1,N2,S451,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S452(N1+1:M2,N0+1:M1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S452)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N1,M2,N1,M2,M1,N2,X80,S452, 1.000)
       deallocate(S452)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder431256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N1,M2,M2,N3,N2,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z114(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9
       I2=K7*K8*K0*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X80,F2,Z114)
       deallocate(F2)
C
       call
     & sum124635(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z114,-1.000)
       call
     & sum125634(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z114, 1.000)
       deallocate(Z114)
       deallocate(X80)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:N1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,M1,N2,N0,N1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S453(M1+1:N2,M1+1:N2,N0+1:N1,N1+1:M2))
       I1=K9*K1*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S453)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N2,N1+1:M2,M1+1:N2))
       call reorder3241(M1,N2,M1,N2,N0,N1,N1,M2,
     & N0,N1,M1,N2,N1,M2,M1,N2,S453,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S454(N1+1:M2,M1+1:N2,N1+1:M2,M1+1:N2))
       I1=K8*K9*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S454)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,N1,M2,N1,M2,M1,N2,X81,S454, 1.000)
       deallocate(S454)
C
       allocate(F2(M1+1:N2,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder431256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N1,M2,M2,N3,N2,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z115(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9
       I2=K7*K8*K0*K6
       I3=K9*K8
       call EGEMM(I1,I2,I3,X81,F2,Z115)
       deallocate(F2)
C
       call
     & sum124635(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z115,-1.000)
       call
     & sum125634(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z115, 1.000)
       deallocate(Z115)
       deallocate(X81)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,N2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q63(N0+1:M1,N2+1:N3))
       I1=K4*K5
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q63)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:M1))
       call reorder21(N0,M1,N2,N3,
     & N2,N3,N0,M1,Q63,B1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(Q64(M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q64)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(N0,M1,M1,N2,X82,Q64, 1.000)
       deallocate(Q64)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N2,M2,N1,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z116(M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N2))
       I1=K8
       I2=K7*K8*K9*K0*K6
       I3=K5
       call EGEMM(I1,I2,I3,X82,F2,Z116)
       deallocate(F2)
C
       call
     & sum123465(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z116, 1.000)
       call
     & sum123564(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z116,-1.000)
       deallocate(Z116)
       deallocate(X82)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,N2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q65(M1+1:N2,N2+1:N3))
       I1=K4*K8
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q65)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,M1+1:N2))
       call reorder21(M1,N2,N2,N3,
     & N2,N3,M1,N2,Q65,B1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(Q66(M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q66)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(M1,N2,M1,N2,X83,Q66, 1.000)
       deallocate(Q66)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N2,M2,N1,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z117(M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N2))
       I1=K8
       I2=K7*K8*K9*K0*K6
       I3=K8
       call EGEMM(I1,I2,I3,X83,F2,Z117)
       deallocate(F2)
C
       call
     & sum123465(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z117, 1.000)
       call
     & sum123564(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z117,-1.000)
       deallocate(Z117)
       deallocate(X83)
C
       allocate(D1(N0+1:N1,N0+1:N2,M2+1:N3,M2+1:N3))
       call reorder4312(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N0,N2,M2,N3,M2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S455(N1+1:M2,N0+1:N2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S455)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,N1+1:M2))
       call reorder2341(N1,M2,N0,N2,M2,N3,M2,N3,
     & N0,N2,M2,N3,M2,N3,N1,M2,S455,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S456(M2+1:N3,M2+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S456)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,M2,N3,M2,N3,N1,M2,X49,S456, 1.000)
       deallocate(S456)
C
       call sumx1234(N2,N3,N1,N3,N2,N3,N1,M2,
     & M2,N3,M2,N3,M2,N3,N1,M2,X49,VBAPPP, 1.000)
C
       allocate(F2(M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N1))
       call reorder132456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z49(N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K7*K8*K8*K0
       I3=K6*K6
       call EGEMM(I1,I2,I3,X49,F2,Z49)
       deallocate(F2)
C
       call
     & sum245613(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z49, 1.000)
       deallocate(Z49)
       deallocate(X49)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:M2,M2+1:N3))
       call reorder4312(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N0,N2,N2,M2,M2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S457(N1+1:M2,N0+1:N2,N2+1:M2,M2+1:N3))
       I1=K6*K0*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S457)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:M2,M2+1:N3,N1+1:M2))
       call reorder2341(N1,M2,N0,N2,N2,M2,M2,N3,
     & N0,N2,N2,M2,M2,N3,N1,M2,S457,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S463(N2+1:M2,N2+1:M2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S463)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,M2,M2,N3,N2,M2,N1,M2,X53,S463, 1.000)
       deallocate(S463)
C
       call sumx1234(N2,N3,N1,N3,N2,N3,N1,M2,
     & N2,M2,M2,N3,N2,M2,N1,M2,X53,VBAPPP, 1.000)
C
       allocate(F2(N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2,M1+1:N1))
       call reorder231456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N2,M2,M2,N3,M2,N3,M1,N2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z53(M2+1:N3,M1+1:N2,M1+1:N2,M1+1:N1,N2+1:M2,N1+1:M2))
       I1=K9*K0
       I2=K7*K8*K8*K6
       I3=K6*K0
       call EGEMM(I1,I2,I3,X53,F2,Z53)
       deallocate(F2)
C
       call
     & sum145623(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z53, 1.000)
       deallocate(Z53)
       deallocate(X53)
C
       allocate(D1(N0+1:N2,N2+1:M2,M2+1:N3,N1+1:M2))
       call reorder2341(N1,M2,N0,N2,N2,M2,M2,N3,
     & N0,N2,N2,M2,M2,N3,N1,M2,S457,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S458(M2+1:N3,N2+1:M2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S458)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,M2,M2,N3,M2,N3,N1,M2,X50,S458, 1.000)
       deallocate(S458)
C
       call sumx1234(N2,N3,N1,N3,N2,N3,N1,M2,
     & N2,M2,M2,N3,M2,N3,N1,M2,X50,VBAPPP, 1.000)
C
       allocate(F2(N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N1))
       call reorder132456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z50(N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K7*K8*K8*K0
       I3=K6*K0
       call EGEMM(I1,I2,I3,X50,F2,Z50)
       deallocate(F2)
C
       call
     & sum245613(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z50, 1.000)
       deallocate(Z50)
       deallocate(X50)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:M2,N1+1:M2))
       call reorder4312(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N0,N2,N2,M2,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S461(N1+1:M2,N0+1:N2,N2+1:M2,N1+1:M2))
       I1=K9*K0*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S461)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:M2,N1+1:M2,N1+1:M2))
       call reorder2341(N1,M2,N0,N2,N2,M2,N1,M2,
     & N0,N2,N2,M2,N1,M2,N1,M2,S461,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S465(N2+1:M2,N2+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9*K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S465)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,M2,N1,M2,N2,M2,N1,M2,X55,S465, 1.000)
       deallocate(S465)
C
       call sumx1234(N2,N3,N1,N3,N2,N3,N1,M2,
     & N2,M2,N1,M2,N2,M2,N1,M2,X55,VBAPPP, 1.000)
C
       allocate(F2(N2+1:M2,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N2,M1+1:N1))
       call reorder231456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N2,M2,N1,M2,M2,N3,M1,N2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z55(M2+1:N3,M1+1:N2,M1+1:N2,M1+1:N1,N2+1:M2,N1+1:M2))
       I1=K9*K0
       I2=K7*K8*K8*K6
       I3=K9*K0
       call EGEMM(I1,I2,I3,X55,F2,Z55)
       deallocate(F2)
C
       call
     & sum145623(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z55, 1.000)
       deallocate(Z55)
       deallocate(X55)
C
       allocate(D1(N0+1:N2,N2+1:M2,N1+1:M2,N1+1:M2))
       call reorder2341(N1,M2,N0,N2,N2,M2,N1,M2,
     & N0,N2,N2,M2,N1,M2,N1,M2,S461,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S462(M2+1:N3,N2+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9*K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S462)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,M2,N1,M2,M2,N3,N1,M2,X52,S462, 1.000)
       deallocate(S462)
C
       call sumx1234(N2,N3,N1,N3,N2,N3,N1,M2,
     & N2,M2,N1,M2,M2,N3,N1,M2,X52,VBAPPP, 1.000)
C
       allocate(F2(N2+1:M2,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N1))
       call reorder132456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N2,M2,N1,M2,N2,M2,M1,N2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z52(N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K7*K8*K8*K0
       I3=K9*K0
       call EGEMM(I1,I2,I3,X52,F2,Z52)
       deallocate(F2)
C
       call
     & sum245613(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z52, 1.000)
       deallocate(Z52)
       deallocate(X52)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N2,M2,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q69(N0+1:N2,N2+1:M2))
       I1=K0*K2
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q69)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(Q72(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q69,B2,Q72)
       deallocate(B2)
C
       call
     & sum21(N2,M2,N2,M2,X87,Q72,-1.000)
       deallocate(Q72)
C
       allocate(F2(N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N2,M1+1:N1))
       call reorder213456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N2,M2,M2,N3,N1,M2,M1,N2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z128(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N2,M1+1:N1,N2+1:M2))
       I1=K0
       I2=K7*K8*K8*K9*K6
       I3=K0
       call EGEMM(I1,I2,I3,X87,F2,Z128)
       deallocate(F2)
C
       call
     & sum134562(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z128, 1.000)
       deallocate(Z128)
       deallocate(X87)
C
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(Q70(M2+1:N3,N2+1:M2))
       I1=K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,Q69,B2,Q70)
       deallocate(B2)
C
       call
     & sum21(N2,M2,M2,N3,X85,Q70,-1.000)
       deallocate(Q70)
C
       allocate(F2(N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N2,M1+1:N1))
       call reorder123456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N2,M2,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z123(N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K8*K8*K9*K0
       I3=K0
       call EGEMM(I1,I2,I3,X85,F2,Z123)
       deallocate(F2)
C
       call
     & sum234561(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z123, 1.000)
       deallocate(Z123)
       deallocate(X85)
C
       allocate(D1(N0+1:N1,N0+1:N2,M2+1:N3,N1+1:M2))
       call reorder4312(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N0,N2,M2,N3,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S459(N1+1:M2,N0+1:N2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S459)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder2341(N1,M2,N0,N2,M2,N3,N1,M2,
     & N0,N2,M2,N3,N1,M2,N1,M2,S459,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S460(M2+1:N3,M2+1:N3,N1+1:M2,N1+1:M2))
       I1=K9*K9*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S460)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,N1,M2,M2,N3,N1,M2,X51,S460, 1.000)
       deallocate(S460)
C
       call sumx1234(N2,N3,N1,N3,N2,N3,N1,M2,
     & M2,N3,N1,M2,M2,N3,N1,M2,X51,VBAPPP, 1.000)
C
       allocate(F2(M2+1:N3,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N1))
       call reorder132456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M2,N3,N1,M2,N2,M2,M1,N2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z51(N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K7*K8*K8*K0
       I3=K9*K6
       call EGEMM(I1,I2,I3,X51,F2,Z51)
       deallocate(F2)
C
       call
     & sum245613(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z51, 1.000)
       deallocate(Z51)
       deallocate(X51)
C
       allocate(D1(N0+1:N2,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder2341(N1,M2,N0,N2,M2,N3,N1,M2,
     & N0,N2,M2,N3,N1,M2,N1,M2,S459,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S464(N2+1:M2,M2+1:N3,N1+1:M2,N1+1:M2))
       I1=K9*K9*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S464)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,N1,M2,N2,M2,N1,M2,X54,S464, 1.000)
       deallocate(S464)
C
       call sumx1234(N2,N3,N1,N3,N2,N3,N1,M2,
     & M2,N3,N1,M2,N2,M2,N1,M2,X54,VBAPPP, 1.000)
C
       allocate(F2(M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N2,M1+1:N1))
       call reorder132456(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & M2,N3,N1,M2,M2,N3,M1,N2,M1,N2,M1,N1,t3C3,F2)
       allocate(Z54(M2+1:N3,M1+1:N2,M1+1:N2,M1+1:N1,N2+1:M2,N1+1:M2))
       I1=K9*K0
       I2=K7*K8*K8*K6
       I3=K9*K6
       call EGEMM(I1,I2,I3,X54,F2,Z54)
       deallocate(F2)
C
       call
     & sum145623(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z54,-1.000)
       deallocate(Z54)
       deallocate(X54)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,M2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q67(N0+1:N2,M2+1:N3))
       I1=K6*K2
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q67)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(Q68(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,Q67,B2,Q68)
       deallocate(B2)
C
       call
     & sum21(M2,N3,M2,N3,X84,Q68,-1.000)
       deallocate(Q68)
C
       allocate(F2(M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N2,M1+1:N1))
       call reorder123456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z122(N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K8*K8*K9*K0
       I3=K6
       call EGEMM(I1,I2,I3,X84,F2,Z122)
       deallocate(F2)
C
       call
     & sum234561(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z122, 1.000)
       deallocate(Z122)
       deallocate(X84)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(Q71(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q67,B2,Q71)
       deallocate(B2)
C
       call
     & sum21(M2,N3,N2,M2,X86,Q71,-1.000)
       deallocate(Q71)
C
       allocate(F2(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N2,M1+1:N1))
       call reorder123456(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & M2,N3,M2,N3,N1,M2,M1,N2,M1,N2,M1,N1,t3C3,F2)
       allocate(Z127(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N2,M1+1:N1,N2+1:M2))
       I1=K0
       I2=K7*K8*K8*K9*K6
       I3=K6
       call EGEMM(I1,I2,I3,X86,F2,Z127)
       deallocate(F2)
C
       call
     & sum134562(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z127,-1.000)
       deallocate(Z127)
       deallocate(X86)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q73(N0+1:N1,M2+1:N3))
       I1=K6*K1
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q73)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(Q74(N1+1:M2,M2+1:N3))
       I1=K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,Q73,B2,Q74)
       deallocate(B2)
C
       call
     & sum21(M2,N3,N1,M2,X21,Q74,-1.000)
       deallocate(Q74)
C
       call sumx12(N1,N3,N1,N3,
     & M2,N3,N1,M2,X21,FAPP, 1.000)
C
       allocate(F2(M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N1))
       call reorder312456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z21(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K8*K8*K0*K6
       I3=K6
       call EGEMM(I1,I2,I3,X21,F2,Z21)
       deallocate(F2)
C
       call
     & sum124563(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z21, 1.000)
       deallocate(Z21)
       deallocate(X21)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q75(N0+1:N1,N1+1:M2))
       I1=K9*K1
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q75)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(Q76(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,Q75,B2,Q76)
       deallocate(B2)
C
       call
     & sum21(N1,M2,N1,M2,X22,Q76,-1.000)
       deallocate(Q76)
C
       call sumx12(N1,N3,N1,N3,
     & N1,M2,N1,M2,X22,FAPP, 1.000)
C
       allocate(F2(N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N1))
       call reorder312456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N1,M2,M2,N3,N2,M2,M1,N2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z22(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K8*K8*K0*K6
       I3=K9
       call EGEMM(I1,I2,I3,X22,F2,Z22)
       deallocate(F2)
C
       call
     & sum124563(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z22, 1.000)
       deallocate(Z22)
       deallocate(X22)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       call reorder2341(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,N1,N0,N1,N1,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S466(M1+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S466)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M1+1:N1))
       call reorder2431(M1,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N1,M1,N1,S466,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S468(M2+1:N3,M1+1:N2,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S468)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,M2,N3,M1,N2,M1,N1,X1,S468, 1.000)
       deallocate(S468)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M1+1:N1))
       call reorder2431(M1,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N1,M1,N1,S466,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S467(N2+1:M2,M1+1:N2,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S467)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N2,M2,M1,N2,M1,N1,X99,S467, 1.000)
       deallocate(S467)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N1,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S469(N2+1:M2,M1+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S469)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N2,M2,M1,N2,S469,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S470(N1+1:M2,N1+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S470)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,N3,N2,M2,N1,M2,M1,N2,X98,S470, 1.000)
       deallocate(S470)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N1,N3,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q77(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q77)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q77,B1)
       allocate(D2(N1+1:N3,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,M2,M1,N2,M1,N1,t2B,D2)
       allocate(S474(N2+1:M2,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K0
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S474)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,M1,N1,X2,S474, 1.000)
       deallocate(S474)
C
       allocate(D2(N0+1:N1,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,M2,N1,M2,M1,N2,t2B,D2)
       allocate(S471(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K1
       call EGEMM(I1,I2,I3,Q77,D2,S471)
       deallocate(D2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X98,S471, 1.000)
       deallocate(S471)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder4312(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S472(N1+1:M2,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S472)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder2341(N1,M2,N0,N1,N1,N3,N1,N3,
     & N0,N1,N1,N3,N1,N3,N1,M2,S472,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S473(M2+1:N3,M1+1:N2,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S473)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N1,N3,M2,N3,N1,M2,M1,N2,X100,S473, 1.000)
       deallocate(S473)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,N2,N0,N1,N2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S475(M1+1:N1,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4*K1*K2
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S475)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M1+1:N1))
       call reorder2431(M1,N1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N2,N3,N0,N1,M1,N1,S475,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S477(M2+1:N3,M1+1:N2,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S477)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,M2,N3,M1,N2,M1,N1,X102,S477, 1.000)
       deallocate(S477)
C
       allocate(D2(N0+1:N1,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,M2,N1,M2,M1,N2,t2B,D2)
       allocate(Z272(N2+1:M2,N1+1:M2,M1+1:N2,M2+1:N3,M1+1:N2,M1+1:N1))
       I1=K7*K8*K6
       I2=K8*K9*K0
       I3=K1
       call EGEMM(I1,I2,I3,X102,D2,Z272)
       deallocate(D2)
C
       call
     & sum235146(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z272,-1.000)
       call
     & sum234156(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z272, 1.000)
       deallocate(Z272)
       deallocate(X102)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,M1+1:N1))
       call reorder3421(M1,N1,N0,N2,N0,N1,N2,N3,
     & N0,N1,N2,N3,N0,N2,M1,N1,S475,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,M2,M1,N2,t2B,D2)
       allocate(S478(N1+1:M2,M1+1:N2,N0+1:N2,M1+1:N1))
       I1=K7*K2
       I2=K8*K9
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S478)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,N1,M2,M1,N2,M1,N1,X8,S478,-1.000)
       deallocate(S478)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,M1+1:N1))
       call reorder2341(M1,N1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N0,N1,N2,N3,M1,N1,S475,D1)
       allocate(D2(N0+1:N2,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,M2,N3,N1,M2,t2B,D2)
       allocate(S479(M2+1:N3,N1+1:M2,N2+1:N3,M1+1:N1))
       I1=K7*K4
       I2=K9*K6
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S479)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N2,N3,M2,N3,N1,M2,M1,N1,X9,S479, 1.000)
       deallocate(S479)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,M1+1:N1))
       call reorder2341(M1,N1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N0,N1,N2,N3,M1,N1,S475,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,N1,M2,t2B,D2)
       allocate(S480(N2+1:M2,N1+1:M2,N2+1:N3,M1+1:N1))
       I1=K7*K4
       I2=K9*K0
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S480)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N2,N3,N2,M2,N1,M2,M1,N1,X10,S480, 1.000)
       deallocate(S480)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,M1+1:N1))
       call reorder4231(M1,N1,N0,N2,N0,N1,N2,N3,
     & N2,N3,N0,N2,N0,N1,M1,N1,S475,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S584(M1+1:N2,N0+1:N2,N0+1:N1,M1+1:N1))
       I1=K7*K1*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S584)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N2,M1+1:N2,M1+1:N1))
       call reorder3214(M1,N2,N0,N2,N0,N1,M1,N1,
     & N0,N1,N0,N2,M1,N2,M1,N1,S584,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S585(N1+1:M2,N0+1:N2,M1+1:N2,M1+1:N1))
       I1=K7*K8*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S585)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,N1,M2,M1,N2,M1,N1,X8,S585,-1.000)
       deallocate(S585)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:N3,M1+1:N1))
       call reorder3241(M1,N1,N0,N2,N0,N1,N2,N3,
     & N0,N1,N0,N2,N2,N3,M1,N1,S475,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S586(N1+1:M2,N0+1:N2,N2+1:N3,M1+1:N1))
       I1=K7*K4*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S586)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder2314(N1,M2,N0,N2,N2,N3,M1,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,S586,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S587(M2+1:N3,N2+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S587)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N1,M2,M1,N1,X9,S587, 1.000)
       deallocate(S587)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M1+1:N1))
       call reorder2431(M1,N1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N2,N3,N0,N1,M1,N1,S475,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S476(N2+1:M2,M1+1:N2,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S476)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N2,M2,M1,N2,M1,N1,X99,S476, 1.000)
       deallocate(S476)
C
       allocate(D2(N0+1:N1,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,M2,N3,N1,M2,M1,N2,t2B,D2)
       allocate(Z252(M2+1:N3,N1+1:M2,M1+1:N2,N2+1:M2,M1+1:N2,M1+1:N1))
       I1=K7*K8*K0
       I2=K8*K9*K6
       I3=K1
       call EGEMM(I1,I2,I3,X99,D2,Z252)
       deallocate(D2)
C
       call
     & sum135246(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z252, 1.000)
       call
     & sum134256(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z252,-1.000)
       deallocate(Z252)
       deallocate(X99)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,M1+1:N1))
       call reorder2314(M1,N2,N0,N2,N0,N1,M1,N1,
     & N0,N2,N0,N1,M1,N2,M1,N1,S584,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S589(M2+1:N3,N0+1:N1,M1+1:N2,M1+1:N1))
       I1=K7*K8*K1
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S589)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,M2,N3,M1,N2,M1,N1,X1,S589,-1.000)
       deallocate(S589)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,M1+1:N1))
       call reorder2314(M1,N2,N0,N2,N0,N1,M1,N1,
     & N0,N2,N0,N1,M1,N2,M1,N1,S584,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S590(N2+1:M2,N0+1:N1,M1+1:N2,M1+1:N1))
       I1=K7*K8*K1
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S590)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N2,M2,M1,N2,M1,N1,X2,S590,-1.000)
       deallocate(S590)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder2314(N1,M2,N0,N2,N2,N3,M1,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,S586,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S588(N2+1:M2,N2+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S588)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,N1,M2,M1,N1,X10,S588, 1.000)
       deallocate(S588)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q78(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q78)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D2(N0+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,M2,N1,M2,M1,N1,t2B,D2)
       allocate(S490(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K2
       call EGEMM(I1,I2,I3,Q78,D2,S490)
       deallocate(D2)
C
       call
     & sum2341(N2,N3,N2,M2,N1,M2,M1,N1,X10,S490,-1.000)
       deallocate(S490)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q78,B1)
       allocate(D2(N2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,M2,M1,N2,M1,N1,t2B,D2)
       allocate(S486(N1+1:M2,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K9
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S486)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N2,N1,M2,M1,N2,M1,N1,X8,S486, 1.000)
       deallocate(S486)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q78,B1)
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,M1,N2,M1,N2,t2C,D2)
       allocate(S488(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S488)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X6,S488,-1.000)
       deallocate(S488)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:N3,M1+1:N1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N0,N2,N2,N3,M1,N1,VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S420(N1+1:M2,N0+1:N2,N2+1:N3,M1+1:N1))
       I1=K7*K4*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S420)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder2314(N1,M2,N0,N2,N2,N3,M1,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,S420,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S422(N2+1:M2,N2+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S422)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,N1,M2,M1,N1,X10,S422, 1.000)
       deallocate(S422)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder2314(N1,M2,N0,N2,N2,N3,M1,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,S420,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S421(M2+1:N3,N2+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S421)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N1,M2,M1,N1,X9,S421, 1.000)
       deallocate(S421)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N1+1:M2))
       call reorder1324(N2,N3,N1,N3,N0,N2,N1,N3,
     & N2,N3,N0,N2,N1,N3,N1,M2,VBPHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S493(M1+1:N2,N0+1:N2,N1+1:N3,N1+1:M2))
       I1=K9*K3*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S493)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N1,N3,N1,M2,
     & N0,N2,N1,N3,N1,M2,M1,N2,S493,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S495(N2+1:M2,N1+1:N3,N1+1:M2,M1+1:N2))
       I1=K8*K9*K3
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S495)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,N2,M2,N1,M2,M1,N2,X4,S495,-1.000)
       deallocate(S495)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N1,N3,N1,M2,
     & N0,N2,N1,N3,N1,M2,M1,N2,S493,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S494(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N2))
       I1=K8*K9*K3
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S494)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,N1,M2,M1,N2,X3,S494,-1.000)
       deallocate(S494)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,M1+1:N2))
       call reorder1342(N2,N3,N0,N2,N0,N2,N0,N2,
     & N2,N3,N0,N2,N0,N2,M1,N2,VCHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S496(M1+1:N2,N0+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S496)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder2314(M1,N2,N0,N2,N0,N2,M1,N2,
     & N0,N2,N0,N2,M1,N2,M1,N2,S496,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S498(N2+1:M2,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S498)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,N2,M2,M1,N2,M1,N2,X89,S498,-1.000)
       deallocate(S498)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder2314(M1,N2,N0,N2,N0,N2,M1,N2,
     & N0,N2,N0,N2,M1,N2,M1,N2,S496,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S497(M2+1:N3,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S497)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,M2,N3,M1,N2,M1,N2,X88,S497,-1.000)
       deallocate(S497)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder4312(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S499(N2+1:M2,N0+1:N2,N2+1:N3,M1+1:N2))
       I1=K8*K4*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S499)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder2314(N2,M2,N0,N2,N2,N3,M1,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,S499,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S500(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S500)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,M1,N2,X7,S500, 1.000)
       deallocate(S500)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder2413(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,N2,N2,N3,M2,N3,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S501(M1+1:N2,N0+1:N2,N2+1:N3,M2+1:N3))
       I1=K6*K4*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S501)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N2,N3,M2,N3,
     & N0,N2,N2,N3,M2,N3,M1,N2,S501,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S505(N2+1:M2,N2+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S505)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,N3,M2,N3,N2,M2,M1,N2,X7,S505,-1.000)
       deallocate(S505)
C
       allocate(D1(N2+1:N3,N0+1:N2,M2+1:N3,M1+1:N2))
       call reorder3241(M1,N2,N0,N2,N2,N3,M2,N3,
     & N2,N3,N0,N2,M2,N3,M1,N2,S501,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S502(M1+1:N2,N0+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S502)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N2,M2,N3,M1,N2,M1,N2,X5,S502, 1.000)
       deallocate(S502)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder2413(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,N2,N2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S503(M1+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S503)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N2,N3,N2,M2,
     & N0,N2,N2,N3,N2,M2,M1,N2,S503,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S506(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S506)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,M1,N2,X7,S506, 1.000)
       deallocate(S506)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:M2,M1+1:N2))
       call reorder3241(M1,N2,N0,N2,N2,N3,N2,M2,
     & N2,N3,N0,N2,N2,M2,M1,N2,S503,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S504(M1+1:N2,N0+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S504)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N2,N2,M2,M1,N2,M1,N2,X6,S504, 1.000)
       deallocate(S504)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,M2+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,N2,N0,M1,M2,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S507(M1+1:N2,N0+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S507)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,M1,M2,N3,
     & N0,N2,N0,M1,M2,N3,M1,N2,S507,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S515(N2+1:M2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S515)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,N2,M2,M1,N2,X15,S515,-1.000)
       deallocate(S515)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,M2,N3,N2,M2,M1,N2,X15,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder521346(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,M1,N2,M1,N1,t3B1,F2)
       allocate(Z15(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K7*K8*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X15,F2,Z15)
       deallocate(F2)
C
       call
     & sum134625(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z15, 1.000)
       call
     & sum135624(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z15,-1.000)
       deallocate(Z15)
       deallocate(X15)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,M1,M2,N3,
     & N0,N2,N0,M1,M2,N3,M1,N2,S507,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S508(M2+1:N3,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S508)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,M2,N3,M1,N2,X11,S508,-1.000)
       deallocate(S508)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,M2,N3,M2,N3,M1,N2,X11,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder521346(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,N2,M2,N1,M2,M1,N2,M1,N1,t3B1,F2)
       allocate(Z11(N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K7*K8*K9*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,X11,F2,Z11)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z11,-1.000)
       call
     & sum235614(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z11, 1.000)
       deallocate(Z11)
       deallocate(X11)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,N2,N0,M1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S511(M1+1:N2,N0+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S511)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,M1,N1,M2,
     & N0,N2,N0,M1,N1,M2,M1,N2,S511,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S517(N2+1:M2,N0+1:M1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S517)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N1,M2,N2,M2,M1,N2,X17,S517,-1.000)
       deallocate(S517)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,N1,M2,N2,M2,M1,N2,X17,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder521346(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N1,M2,M2,N3,N1,M2,M1,N2,M1,N1,t3B1,F2)
       allocate(Z17(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K7*K8*K9*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X17,F2,Z17)
       deallocate(F2)
C
       call
     & sum134625(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z17, 1.000)
       call
     & sum135624(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z17,-1.000)
       deallocate(Z17)
       deallocate(X17)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,M1,N1,M2,
     & N0,N2,N0,M1,N1,M2,M1,N2,S511,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S512(M2+1:N3,N0+1:M1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S512)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N1,M2,M2,N3,M1,N2,X13,S512,-1.000)
       deallocate(S512)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,N1,M2,M2,N3,M1,N2,X13,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder521346(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N1,M2,N2,M2,N1,M2,M1,N2,M1,N1,t3B1,F2)
       allocate(Z13(N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K7*K8*K9*K0
       I3=K9*K5
       call EGEMM(I1,I2,I3,X13,F2,Z13)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z13,-1.000)
       call
     & sum235614(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z13, 1.000)
       deallocate(Z13)
       deallocate(X13)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N1,M2+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,N2,M1,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S509(M1+1:N2,N0+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S509)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,M1,N1,M2,N3,
     & N0,N2,M1,N1,M2,N3,M1,N2,S509,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S516(N2+1:M2,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S516)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,M2,N3,N2,M2,M1,N2,X16,S516,-1.000)
       deallocate(S516)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,M2,N3,N2,M2,M1,N2,X16,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder521346(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N1,M2,M1,N2,M1,N1,t3B1,F2)
       allocate(Z16(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K7*K8*K9*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X16,F2,Z16)
       deallocate(F2)
C
       call
     & sum134625(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z16, 1.000)
       call
     & sum135624(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z16,-1.000)
       deallocate(Z16)
       deallocate(X16)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,M1,N1,M2,N3,
     & N0,N2,M1,N1,M2,N3,M1,N2,S509,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S510(M2+1:N3,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S510)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,M2,N3,M2,N3,M1,N2,X12,S510,-1.000)
       deallocate(S510)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,M2,N3,M2,N3,M1,N2,X12,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder521346(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,N2,M2,N1,M2,M1,N2,M1,N1,t3B1,F2)
       allocate(Z12(N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K7*K8*K9*K0
       I3=K6*K7
       call EGEMM(I1,I2,I3,X12,F2,Z12)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z12,-1.000)
       call
     & sum235614(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z12, 1.000)
       deallocate(Z12)
       deallocate(X12)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,N2,M1,N1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S513(M1+1:N2,N0+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S513)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,M1,N1,N1,M2,
     & N0,N2,M1,N1,N1,M2,M1,N2,S513,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S518(N2+1:M2,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S518)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,N1,M2,N2,M2,M1,N2,X18,S518,-1.000)
       deallocate(S518)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,N1,M2,N2,M2,M1,N2,X18,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder521346(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,N1,M2,M2,N3,N1,M2,M1,N2,M1,N1,t3B1,F2)
       allocate(Z18(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K7*K8*K9*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X18,F2,Z18)
       deallocate(F2)
C
       call
     & sum134625(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z18, 1.000)
       call
     & sum135624(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z18,-1.000)
       deallocate(Z18)
       deallocate(X18)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,M1,N1,N1,M2,
     & N0,N2,M1,N1,N1,M2,M1,N2,S513,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S514(M2+1:N3,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S514)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,N1,M2,M2,N3,M1,N2,X14,S514,-1.000)
       deallocate(S514)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,N1,M2,M2,N3,M1,N2,X14,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder521346(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,N1,M2,N2,M2,N1,M2,M1,N2,M1,N1,t3B1,F2)
       allocate(Z14(N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K7*K8*K9*K0
       I3=K9*K7
       call EGEMM(I1,I2,I3,X14,F2,Z14)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z14,-1.000)
       call
     & sum235614(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z14, 1.000)
       deallocate(Z14)
       deallocate(X14)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:M1,N2+1:N3))
       call reorder2341(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,M1,N0,M1,N2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S519(M1+1:N2,N0+1:M1,N0+1:M1,N2+1:N3))
       I1=K4*K5*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S519)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:M1,M1+1:N2))
       call reorder4231(M1,N2,N0,M1,N0,M1,N2,N3,
     & N2,N3,N0,M1,N0,M1,M1,N2,S519,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S520(M1+1:N2,N0+1:M1,N0+1:M1,M1+1:N2))
       I1=K8*K5*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S520)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N0,M1,M1,N2,M1,N2,X56,S520, 1.000)
       deallocate(S520)
C
       call sumx3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,M1,N0,M1,M1,N2,M1,N2,X56,VCHHHH, 1.000)
C
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & N0,M1,N0,M1,M2,N3,N2,M2,N1,M2,M1,N1,t3C4,F2)
       allocate(Z56(M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N1,M1+1:N2,M1+1:N2))
       I1=K8*K8
       I2=K7*K9*K0*K6
       I3=K5*K5
       call EGEMM(I1,I2,I3,X56,F2,Z56)
       deallocate(F2)
C
       call
     & sum123645(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z56, 0.500)
       deallocate(Z56)
       deallocate(X56)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,N2+1:N3))
       call reorder2341(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,M1,M1,N2,N2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S521(M1+1:N2,N0+1:M1,M1+1:N2,N2+1:N3))
       I1=K4*K8*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S521)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder4231(M1,N2,N0,M1,M1,N2,N2,N3,
     & N2,N3,N0,M1,M1,N2,M1,N2,S521,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S522(M1+1:N2,N0+1:M1,M1+1:N2,M1+1:N2))
       I1=K8*K8*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S522)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M1,N2,M1,N2,M1,N2,X57,S522, 1.000)
       deallocate(S522)
C
       call sumx3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,M1,M1,N2,M1,N2,M1,N2,X57,VCHHHH, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N2,M2,N3,N2,M2,N1,M2,M1,N1,t3C1,F2)
       allocate(Z57(M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N1,M1+1:N2,M1+1:N2))
       I1=K8*K8
       I2=K7*K9*K0*K6
       I3=K8*K5
       call EGEMM(I1,I2,I3,X57,F2,Z57)
       deallocate(F2)
C
       call
     & sum123645(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z57, 1.000)
       deallocate(Z57)
       deallocate(X57)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,N2+1:N3))
       call reorder2341(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,M1,N2,M1,N2,N2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S523(M1+1:N2,M1+1:N2,M1+1:N2,N2+1:N3))
       I1=K4*K8*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S523)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder4231(M1,N2,M1,N2,M1,N2,N2,N3,
     & N2,N3,M1,N2,M1,N2,M1,N2,S523,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S524(M1+1:N2,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S524)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M1,N2,M1,N2,M1,N2,X58,S524, 1.000)
       deallocate(S524)
C
       call sumx3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & M1,N2,M1,N2,M1,N2,M1,N2,X58,VCHHHH, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N2,M2,N3,N2,M2,N1,M2,M1,N1,t3C1,F2)
       allocate(Z58(M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N1,M1+1:N2,M1+1:N2))
       I1=K8*K8
       I2=K7*K9*K0*K6
       I3=K8*K8
       call EGEMM(I1,I2,I3,X58,F2,Z58)
       deallocate(F2)
C
       call
     & sum123645(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z58, 0.500)
       deallocate(Z58)
       deallocate(X58)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,M2+1:N3))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,N2,N0,M1,M2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S525(M1+1:N2,N0+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S525)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,M1,M2,N3,
     & N0,N2,N0,M1,M2,N3,M1,N2,S525,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S533(N2+1:M2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S533)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,N2,M2,M1,N2,X63,S533, 1.000)
       deallocate(S533)
C
       call sumx2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,M1,M2,N3,N2,M2,M1,N2,X63,VCHPHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder412356(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,M1,N2,M1,N1,t3C3,F2)
       allocate(Z63(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K7*K8*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X63,F2,Z63)
       deallocate(F2)
C
       call
     & sum134625(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z63,-1.000)
       call
     & sum135624(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z63, 1.000)
       deallocate(Z63)
       deallocate(X63)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,M1,M2,N3,
     & N0,N2,N0,M1,M2,N3,M1,N2,S525,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S526(M2+1:N3,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S526)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,M2,N3,M1,N2,X59,S526, 1.000)
       deallocate(S526)
C
       call sumx2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,M1,M2,N3,M2,N3,M1,N2,X59,VCHPHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N2,M2,N1,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z59(N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K7*K8*K9*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,X59,F2,Z59)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z59, 1.000)
       call
     & sum235614(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z59,-1.000)
       deallocate(Z59)
       deallocate(X59)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,N2+1:M2))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,N2,N0,M1,N2,M2,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S529(M1+1:N2,N0+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S529)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,M1,N2,M2,
     & N0,N2,N0,M1,N2,M2,M1,N2,S529,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S535(N2+1:M2,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S535)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N2,M2,N2,M2,M1,N2,X65,S535, 1.000)
       deallocate(S535)
C
       call sumx2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,M1,N2,M2,N2,M2,M1,N2,X65,VCHPHP, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder421356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N1,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z65(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K7*K8*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,X65,F2,Z65)
       deallocate(F2)
C
       call
     & sum134625(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z65, 1.000)
       call
     & sum135624(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z65,-1.000)
       deallocate(Z65)
       deallocate(X65)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,M1,N2,M2,
     & N0,N2,N0,M1,N2,M2,M1,N2,S529,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S530(M2+1:N3,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S530)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N2,M2,M2,N3,M1,N2,X61,S530, 1.000)
       deallocate(S530)
C
       call sumx2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,M1,N2,M2,M2,N3,M1,N2,X61,VCHPHP, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N2,M2,N1,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z61(N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K7*K8*K9*K0
       I3=K0*K5
       call EGEMM(I1,I2,I3,X61,F2,Z61)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z61, 1.000)
       call
     & sum235614(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z61,-1.000)
       deallocate(Z61)
       deallocate(X61)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N2,M2+1:N3))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,N2,M1,N2,M2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S527(M1+1:N2,N0+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S527)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,M1,N2,M2,N3,
     & N0,N2,M1,N2,M2,N3,M1,N2,S527,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S534(N2+1:M2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S534)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M2,N3,N2,M2,M1,N2,X64,S534, 1.000)
       deallocate(S534)
C
       call sumx2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,M2,N3,N2,M2,M1,N2,X64,VCHPHP, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder412356(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,M2,N3,N1,M2,M1,N2,M1,N1,t3C3,F2)
       allocate(Z64(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K7*K8*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X64,F2,Z64)
       deallocate(F2)
C
       call
     & sum134625(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z64,-1.000)
       call
     & sum135624(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z64, 1.000)
       deallocate(Z64)
       deallocate(X64)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,M1,N2,M2,N3,
     & N0,N2,M1,N2,M2,N3,M1,N2,S527,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S528(M2+1:N3,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S528)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M2,N3,M2,N3,M1,N2,X60,S528, 1.000)
       deallocate(S528)
C
       call sumx2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,M2,N3,M2,N3,M1,N2,X60,VCHPHP, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N2,M2,N1,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z60(N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K7*K8*K9*K0
       I3=K6*K8
       call EGEMM(I1,I2,I3,X60,F2,Z60)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z60, 1.000)
       call
     & sum235614(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z60,-1.000)
       deallocate(Z60)
       deallocate(X60)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N2,N2+1:M2))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,N2,M1,N2,N2,M2,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S531(M1+1:N2,N0+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S531)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,M1,N2,N2,M2,
     & N0,N2,M1,N2,N2,M2,M1,N2,S531,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S536(N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S536)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,N2,M2,N2,M2,M1,N2,X66,S536, 1.000)
       deallocate(S536)
C
       call sumx2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,N2,M2,N2,M2,M1,N2,X66,VCHPHP, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder421356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,N1,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z66(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K7*K8*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X66,F2,Z66)
       deallocate(F2)
C
       call
     & sum134625(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z66, 1.000)
       call
     & sum135624(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z66,-1.000)
       deallocate(Z66)
       deallocate(X66)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,M1,N2,N2,M2,
     & N0,N2,M1,N2,N2,M2,M1,N2,S531,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S532(M2+1:N3,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K8
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S532)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,N2,M2,M2,N3,M1,N2,X62,S532, 1.000)
       deallocate(S532)
C
       call sumx2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,N2,M2,M2,N3,M1,N2,X62,VCHPHP, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,N2,M2,N1,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z62(N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K7*K8*K9*K0
       I3=K0*K8
       call EGEMM(I1,I2,I3,X62,F2,Z62)
       deallocate(F2)
C
       call
     & sum234615(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z62, 1.000)
       call
     & sum235614(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z62,-1.000)
       deallocate(Z62)
       deallocate(X62)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,N2,N3,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q79(N0+1:M1,N2+1:N3))
       I1=K4*K5
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q79)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:M1))
       call reorder21(N0,M1,N2,N3,
     & N2,N3,N0,M1,Q79,B1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(Q80(M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q80)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(N0,M1,M1,N2,X23,Q80, 1.000)
       deallocate(Q80)
C
       call sumx21(N0,N2,N0,N2,
     & N0,M1,M1,N2,X23,FBHH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N2,M2,N1,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z23(M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N2))
       I1=K8
       I2=K7*K8*K9*K0*K6
       I3=K5
       call EGEMM(I1,I2,I3,X23,F2,Z23)
       deallocate(F2)
C
       call
     & sum123465(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z23, 1.000)
       call
     & sum123564(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z23,-1.000)
       deallocate(Z23)
       deallocate(X23)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,N2,N3,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q81(M1+1:N2,N2+1:N3))
       I1=K4*K8
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q81)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,M1+1:N2))
       call reorder21(M1,N2,N2,N3,
     & N2,N3,M1,N2,Q81,B1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(Q82(M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q82)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(M1,N2,M1,N2,X24,Q82, 1.000)
       deallocate(Q82)
C
       call sumx21(N0,N2,N0,N2,
     & M1,N2,M1,N2,X24,FBHH, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N2,M2,N1,M2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z24(M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N1,M1+1:N2))
       I1=K8
       I2=K7*K8*K9*K0*K6
       I3=K8
       call EGEMM(I1,I2,I3,X24,F2,Z24)
       deallocate(F2)
C
       call
     & sum123465(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z24, 1.000)
       call
     & sum123564(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z24,-1.000)
       deallocate(Z24)
       deallocate(X24)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,M2+1:N3))
       call reorder4312(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,M2,N3,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S537(N2+1:M2,N0+1:N2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S537)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,N2+1:M2))
       call reorder2341(N2,M2,N0,N2,M2,N3,M2,N3,
     & N0,N2,M2,N3,M2,N3,N2,M2,S537,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S538(M2+1:N3,M2+1:N3,M2+1:N3,N2+1:M2))
       I1=K0*K6*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S538)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,M2,N3,M2,N3,N2,M2,X67,S538, 1.000)
       deallocate(S538)
C
       call sumx1234(N2,N3,N2,N3,N2,N3,N2,M2,
     & M2,N3,M2,N3,M2,N3,N2,M2,X67,VCAPPP, 1.000)
C
       allocate(F2(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N2,M1+1:N1))
       call reorder123456(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & M2,N3,M2,N3,N1,M2,M1,N2,M1,N2,M1,N1,t3C3,F2)
       allocate(Z67(N1+1:M2,M1+1:N2,M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K7*K8*K8*K9
       I3=K6*K6
       call EGEMM(I1,I2,I3,X67,F2,Z67)
       deallocate(F2)
C
       call
     & sum345612(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z67, 0.500)
       deallocate(Z67)
       deallocate(X67)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder4312(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,N2,M2,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S539(N2+1:M2,N0+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S539)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder2341(N2,M2,N0,N2,M2,N3,N2,M2,
     & N0,N2,M2,N3,N2,M2,N2,M2,S539,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S540(M2+1:N3,M2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S540)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,N2,M2,M2,N3,N2,M2,X68,S540, 1.000)
       deallocate(S540)
C
       call sumx1234(N2,N3,N2,N3,N2,N3,N2,M2,
     & M2,N3,N2,M2,M2,N3,N2,M2,X68,VCAPPP, 1.000)
C
       allocate(F2(M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N2,M1+1:N1))
       call reorder123456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z68(N1+1:M2,M1+1:N2,M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K7*K8*K8*K9
       I3=K0*K6
       call EGEMM(I1,I2,I3,X68,F2,Z68)
       deallocate(F2)
C
       call
     & sum345612(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z68, 1.000)
       deallocate(Z68)
       deallocate(X68)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder4312(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,M2,N2,M2,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S541(N2+1:M2,N0+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S541)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder2341(N2,M2,N0,N2,N2,M2,N2,M2,
     & N0,N2,N2,M2,N2,M2,N2,M2,S541,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S542(M2+1:N3,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S542)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,M2,N2,M2,M2,N3,N2,M2,X69,S542, 1.000)
       deallocate(S542)
C
       call sumx1234(N2,N3,N2,N3,N2,N3,N2,M2,
     & N2,M2,N2,M2,M2,N3,N2,M2,X69,VCAPPP, 1.000)
C
       allocate(F2(N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N2,M1+1:N1))
       call reorder123456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N2,M2,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z69(N1+1:M2,M1+1:N2,M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K7*K8*K8*K9
       I3=K0*K0
       call EGEMM(I1,I2,I3,X69,F2,Z69)
       deallocate(F2)
C
       call
     & sum345612(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z69, 0.500)
       deallocate(Z69)
       deallocate(X69)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,M2,N3,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q83(N0+1:N2,M2+1:N3))
       I1=K6*K2
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q83)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(Q87(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q83,B2,Q87)
       deallocate(B2)
C
       call
     & sum21(M2,N3,N2,M2,X27,Q87,-1.000)
       deallocate(Q87)
C
       call sumx12(N2,N3,N2,N3,
     & M2,N3,N2,M2,X27,FBPP, 1.000)
C
       allocate(F2(M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N2,M1+1:N1))
       call reorder123456(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & M2,N3,M2,N3,N1,M2,M1,N2,M1,N2,M1,N1,t3C3,F2)
       allocate(Z27(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N2,M1+1:N1,N2+1:M2))
       I1=K0
       I2=K7*K8*K8*K9*K6
       I3=K6
       call EGEMM(I1,I2,I3,X27,F2,Z27)
       deallocate(F2)
C
       call
     & sum134562(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z27,-1.000)
       deallocate(Z27)
       deallocate(X27)
C
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(Q84(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,Q83,B2,Q84)
       deallocate(B2)
C
       call
     & sum21(M2,N3,M2,N3,X25,Q84,-1.000)
       deallocate(Q84)
C
       call sumx12(N2,N3,N2,N3,
     & M2,N3,M2,N3,X25,FBPP, 1.000)
C
       allocate(F2(M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N2,M1+1:N1))
       call reorder123456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z25(N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K8*K8*K9*K0
       I3=K6
       call EGEMM(I1,I2,I3,X25,F2,Z25)
       deallocate(F2)
C
       call
     & sum234561(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z25, 1.000)
       deallocate(Z25)
       deallocate(X25)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N2,M2,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q85(N0+1:N2,N2+1:M2))
       I1=K0*K2
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q85)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(Q88(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q85,B2,Q88)
       deallocate(B2)
C
       call
     & sum21(N2,M2,N2,M2,X28,Q88,-1.000)
       deallocate(Q88)
C
       call sumx12(N2,N3,N2,N3,
     & N2,M2,N2,M2,X28,FBPP, 1.000)
C
       allocate(F2(N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N2,M1+1:N1))
       call reorder213456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N2,M2,M2,N3,N1,M2,M1,N2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z28(M2+1:N3,N1+1:M2,M1+1:N2,M1+1:N2,M1+1:N1,N2+1:M2))
       I1=K0
       I2=K7*K8*K8*K9*K6
       I3=K0
       call EGEMM(I1,I2,I3,X28,F2,Z28)
       deallocate(F2)
C
       call
     & sum134562(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z28, 1.000)
       deallocate(Z28)
       deallocate(X28)
C
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(Q86(M2+1:N3,N2+1:M2))
       I1=K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,Q85,B2,Q86)
       deallocate(B2)
C
       call
     & sum21(N2,M2,M2,N3,X26,Q86,-1.000)
       deallocate(Q86)
C
       call sumx12(N2,N3,N2,N3,
     & N2,M2,M2,N3,X26,FBPP, 1.000)
C
       allocate(F2(N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N2,M1+1:N1))
       call reorder123456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N2,M2,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,t3C1,F2)
       allocate(Z26(N2+1:M2,N1+1:M2,M1+1:N2,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K8*K8*K9*K0
       I3=K0
       call EGEMM(I1,I2,I3,X26,F2,Z26)
       deallocate(F2)
C
       call
     & sum234561(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z26, 1.000)
       deallocate(Z26)
       deallocate(X26)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,N2,N0,N1,N1,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S543(M1+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S543)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,N1,N1,N3,
     & N0,N2,N0,N1,N1,N3,M1,N2,S543,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,N1,M2,t2B,D2)
       allocate(S548(N2+1:M2,N1+1:M2,N1+1:N3,M1+1:N2))
       I1=K8*K3
       I2=K9*K0
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S548)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N1,N3,N2,M2,N1,M2,M1,N2,X4,S548, 1.000)
       deallocate(S548)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,N1,N1,N3,
     & N0,N2,N0,N1,N1,N3,M1,N2,S543,D1)
       allocate(D2(N0+1:N2,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,M2,N3,N1,M2,t2B,D2)
       allocate(S549(M2+1:N3,N1+1:M2,N1+1:N3,M1+1:N2))
       I1=K8*K3
       I2=K9*K6
       I3=K1*K2
       call EGEMM(I1,I2,I3,D1,D2,S549)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N1,N3,M2,N3,N1,M2,M1,N2,X3,S549, 1.000)
       deallocate(S549)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder3421(M1,N2,N0,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N2,M1,N2,S543,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S550(N2+1:M2,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S550)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,N2,M2,M1,N2,M1,N2,X89,S550, 1.000)
       deallocate(S550)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,M1+1:N2))
       call reorder2431(M1,N2,N0,N2,N0,N1,N1,N3,
     & N0,N2,N1,N3,N0,N1,M1,N2,S543,D1)
       allocate(D2(N0+1:N2,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,M2,N3,M1,N1,t2B,D2)
       allocate(S551(M2+1:N3,M1+1:N1,N0+1:N1,M1+1:N2))
       I1=K8*K1
       I2=K7*K6
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S551)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N1,M2,N3,M1,N2,M1,N1,X1,S551,-1.000)
       deallocate(S551)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder3421(M1,N2,N0,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N2,M1,N2,S543,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S552(M2+1:N3,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S552)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,M2,N3,M1,N2,M1,N2,X88,S552, 1.000)
       deallocate(S552)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,M1+1:N2))
       call reorder2431(M1,N2,N0,N2,N0,N1,N1,N3,
     & N0,N2,N1,N3,N0,N1,M1,N2,S543,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,M1+1:N1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,M2,M1,N1,t2B,D2)
       allocate(S553(N2+1:M2,M1+1:N1,N0+1:N1,M1+1:N2))
       I1=K8*K1
       I2=K7*K0
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S553)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N1,N2,M2,M1,N2,M1,N1,X2,S553,-1.000)
       deallocate(S553)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder3421(M1,N2,N0,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N2,M1,N2,S543,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S544(N1+1:M2,M1+1:N1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S544)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N2,N1,M2,M1,N2,M1,N1,X8,S544, 1.000)
       deallocate(S544)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S545(N1+1:M2,M1+1:N1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S545)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3412(N1,M2,M1,N1,N0,N2,N2,N3,
     & N0,N2,N2,N3,N1,M2,M1,N1,S545,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S547(N2+1:M2,N2+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S547)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,N1,M2,M1,N1,X10,S547,-1.000)
       deallocate(S547)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3412(N1,M2,M1,N1,N0,N2,N2,N3,
     & N0,N2,N2,N3,N1,M2,M1,N1,S545,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S546(M2+1:N3,N2+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S546)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N1,M2,M1,N1,X9,S546,-1.000)
       deallocate(S546)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N1+1:N3))
       call reorder4132(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N0,N2,N1,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,M2,M1,N2,t2B,D2)
       allocate(S554(N1+1:M2,M1+1:N2,N0+1:N2,N1+1:N3))
       I1=K3*K2
       I2=K8*K9
       I3=K4*K1
       call EGEMM(I1,I2,I3,D1,D2,S554)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,M1+1:N2))
       call reorder3412(N1,M2,M1,N2,N0,N2,N1,N3,
     & N0,N2,N1,N3,N1,M2,M1,N2,S554,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S560(N2+1:M2,N1+1:N3,N1+1:M2,M1+1:N2))
       I1=K8*K9*K3
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S560)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,N2,M2,N1,M2,M1,N2,X98,S560,-1.000)
       deallocate(S560)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,M1+1:N2))
       call reorder3412(N1,M2,M1,N2,N0,N2,N1,N3,
     & N0,N2,N1,N3,N1,M2,M1,N2,S554,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S555(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N2))
       I1=K8*K9*K3
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S555)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,N1,M2,M1,N2,X100,S555,-1.000)
       deallocate(S555)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N2,N0,N1,VBHHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,M1+1:N2,M1+1:N1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,M1,N2,M1,N1,t2B,D2)
       allocate(S558(M1+1:N2,M1+1:N1,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K7*K8
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,S558)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,M1+1:N1))
       call reorder3412(M1,N2,M1,N1,N0,N2,N0,N1,
     & N0,N2,N0,N1,M1,N2,M1,N1,S558,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S564(N2+1:M2,N0+1:N1,M1+1:N2,M1+1:N1))
       I1=K7*K8*K1
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S564)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N2,M2,M1,N2,M1,N1,X2,S564,-1.000)
       deallocate(S564)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,M1+1:N1))
       call reorder3412(M1,N2,M1,N1,N0,N2,N0,N1,
     & N0,N2,N0,N1,M1,N2,M1,N1,S558,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S559(M2+1:N3,N0+1:N1,M1+1:N2,M1+1:N1))
       I1=K7*K8*K1
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S559)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,M2,N3,M1,N2,M1,N1,X1,S559,-1.000)
       deallocate(S559)
C
       call sumx2134(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,M2,N3,M1,N2,M1,N1,X1,VBHHHP, 1.000)
C
       allocate(D2(N0+1:N1,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,M2,N1,M2,M1,N2,t2B,D2)
       allocate(Z1(N2+1:M2,N1+1:M2,M1+1:N2,M2+1:N3,M1+1:N2,M1+1:N1))
       I1=K7*K8*K6
       I2=K8*K9*K0
       I3=K1
       call EGEMM(I1,I2,I3,X1,D2,Z1)
       deallocate(D2)
C
       call
     & sum234156(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z1, 1.000)
       call
     & sum235146(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z1,-1.000)
       deallocate(Z1)
       deallocate(X1)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N1,N3,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q89(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q89)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q89,B1)
       allocate(D2(N1+1:N3,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,M2,M1,N2,M1,N1,t2B,D2)
       allocate(S565(N2+1:M2,M1+1:N2,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K8*K0
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S565)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,M1,N1,X2,S565, 1.000)
       deallocate(S565)
C
       call sumx2134(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N2,M2,M1,N2,M1,N1,X2,VBHHHP, 1.000)
C
       allocate(D2(N0+1:N1,M2+1:N3,N1+1:M2,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,M2,N3,N1,M2,M1,N2,t2B,D2)
       allocate(Z2(M2+1:N3,N1+1:M2,M1+1:N2,N2+1:M2,M1+1:N2,M1+1:N1))
       I1=K7*K8*K0
       I2=K8*K9*K6
       I3=K1
       call EGEMM(I1,I2,I3,X2,D2,Z2)
       deallocate(D2)
C
       call
     & sum134256(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z2,-1.000)
       call
     & sum135246(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z2, 1.000)
       deallocate(Z2)
       deallocate(X2)
C
       allocate(D2(N0+1:N1,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,M2,N1,M2,M1,N2,t2B,D2)
       allocate(S561(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K1
       call EGEMM(I1,I2,I3,Q89,D2,S561)
       deallocate(D2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X98,S561, 1.000)
       deallocate(S561)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S556(N2+1:M2,M1+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S556)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N2,M2,M1,N2,S556,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S557(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S557)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,M1,N2,X101,S557,-1.000)
       deallocate(S557)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S562(N2+1:M2,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4*K1
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S562)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder2431(N2,M2,N0,N1,N2,N3,N1,N3,
     & N0,N1,N1,N3,N2,N3,N2,M2,S562,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S563(M2+1:N3,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S563)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,M2,N3,N2,M2,M1,N2,X101,S563, 1.000)
       deallocate(S563)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,N2,N0,N2,N2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S566(M1+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S566)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder2431(M1,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N0,N2,M1,N2,S566,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S568(N2+1:M2,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S568)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,N2,M2,M1,N2,M1,N2,X89,S568,-1.000)
       deallocate(S568)
C
       allocate(D2(N0+1:N2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,M2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(Z148(M2+1:N3,N1+1:M2,M1+1:N1,N2+1:M2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K0
       I2=K7*K9*K6
       I3=K2
       call EGEMM(I1,I2,I3,X89,D2,Z148)
       deallocate(D2)
C
       call
     & sum136245(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z148, 1.000)
       call
     & sum136254(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z148,-1.000)
       deallocate(Z148)
       deallocate(X89)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder2431(M1,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N0,N2,M1,N2,S566,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S569(M2+1:N3,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S569)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,M2,N3,M1,N2,M1,N2,X88,S569,-1.000)
       deallocate(S569)
C
       allocate(D2(N0+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,M2,N1,M2,M1,N1,t2B,D2)
       allocate(Z147(N2+1:M2,N1+1:M2,M1+1:N1,M2+1:N3,M1+1:N2,M1+1:N2))
       I1=K8*K8*K6
       I2=K7*K9*K0
       I3=K2
       call EGEMM(I1,I2,I3,X88,D2,Z147)
       deallocate(D2)
C
       call
     & sum236145(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z147,-1.000)
       call
     & sum236154(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z147, 1.000)
       deallocate(Z147)
       deallocate(X88)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder3421(M1,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N0,N2,M1,N2,S566,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S570(N1+1:M2,M1+1:N1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S570)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N2,N1,M2,M1,N2,M1,N1,X8,S570, 1.000)
       deallocate(S570)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,M1+1:N2))
       call reorder4231(M1,N2,N0,N2,N0,N2,N2,N3,
     & N2,N3,N0,N2,N0,N2,M1,N2,S566,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S594(M1+1:N2,N0+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S594)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder2314(M1,N2,N0,N2,N0,N2,M1,N2,
     & N0,N2,N0,N2,M1,N2,M1,N2,S594,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S595(M2+1:N3,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S595)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,M2,N3,M1,N2,M1,N2,X5,S595, 1.000)
       deallocate(S595)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder3241(M1,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,M1,N2,S566,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S597(N2+1:M2,N0+1:N2,N2+1:N3,M1+1:N2))
       I1=K8*K4*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S597)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder2314(N2,M2,N0,N2,N2,N3,M1,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,S597,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S598(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S598)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,M1,N2,X7,S598,-1.000)
       deallocate(S598)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,M1,N2,S566,D1)
       allocate(D2(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,N2,M2,t2C,D2)
       allocate(S567(M2+1:N3,N2+1:M2,N2+1:N3,M1+1:N2))
       I1=K8*K4
       I2=K0*K6
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S567)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N2,N3,M2,N3,N2,M2,M1,N2,X7,S567,-0.500)
       deallocate(S567)
C
       call sumx2314(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,M2,N3,N2,M2,M1,N2,X7,VCHPPP, 1.000)
C
       allocate(D2(N2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,M2,M1,N2,M1,N1,t2B,D2)
       allocate(Z7(N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K6
       I2=K7*K8*K9
       I3=K4
       call EGEMM(I1,I2,I3,X7,D2,Z7)
       deallocate(D2)
C
       call
     & sum346125(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z7, 1.000)
       call
     & sum356124(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z7,-1.000)
       deallocate(Z7)
       deallocate(X7)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder2314(M1,N2,N0,N2,N0,N2,M1,N2,
     & N0,N2,N0,N2,M1,N2,M1,N2,S594,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S596(N2+1:M2,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S596)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,N2,M2,M1,N2,M1,N2,X6,S596, 1.000)
       deallocate(S596)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,VCHHPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,M1,N2,M1,N2,t2C,D2)
       allocate(S576(M1+1:N2,M1+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K8*K8
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S576)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder3412(M1,N2,M1,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,M1,N2,M1,N2,S576,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S577(M2+1:N3,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S577)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,M2,N3,M1,N2,M1,N2,X5,S577,-0.500)
       deallocate(S577)
C
       call sumx2134(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,M2,N3,M1,N2,M1,N2,X5,VCHHHP, 1.000)
C
       allocate(D2(N0+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,M2,N1,M2,M1,N1,t2B,D2)
       allocate(Z5(N2+1:M2,N1+1:M2,M1+1:N1,M2+1:N3,M1+1:N2,M1+1:N2))
       I1=K8*K8*K6
       I2=K7*K9*K0
       I3=K2
       call EGEMM(I1,I2,I3,X5,D2,Z5)
       deallocate(D2)
C
       call
     & sum236145(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z5,-1.000)
       deallocate(Z5)
       deallocate(X5)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S578(N1+1:M2,M1+1:N1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S578)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3412(N1,M2,M1,N1,N0,N2,N2,N3,
     & N0,N2,N2,N3,N1,M2,M1,N1,S578,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S579(M2+1:N3,N2+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S579)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N1,M2,M1,N1,X9,S579,-1.000)
       deallocate(S579)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder4312(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S573(N2+1:M2,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S573)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder2431(N2,M2,N0,N2,N2,N3,N2,N3,
     & N0,N2,N2,N3,N2,N3,N2,M2,S573,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S582(N1+1:M2,M1+1:N1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S582)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,N3,N2,M2,N1,M2,M1,N1,X10,S582, 1.000)
       deallocate(S582)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder2341(N2,M2,N0,N2,N2,N3,N2,N3,
     & N0,N2,N2,N3,N2,N3,N2,M2,S573,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S574(M2+1:N3,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S574)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,M2,N3,N2,M2,M1,N2,X101,S574, 1.000)
       deallocate(S574)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:M2))
       call reorder3421(N2,M2,N0,N2,N2,N3,N2,N3,
     & N2,N3,N2,N3,N0,N2,N2,M2,S573,D1)
       allocate(D2(N2+1:N3,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,M1,N2,M1,N2,t2C,D2)
       allocate(S580(M1+1:N2,M1+1:N2,N0+1:N2,N2+1:M2))
       I1=K0*K2
       I2=K8*K8
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S580)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,N2,N2,M2,M1,N2,M1,N2,X6,S580, 0.500)
       deallocate(S580)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N2,N3,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q90(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q90)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q90,B1)
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,M1,N2,M1,N2,t2C,D2)
       allocate(S581(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S581)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X6,S581,-1.000)
       deallocate(S581)
C
       call sumx2134(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,M2,M1,N2,M1,N2,X6,VCHHHP, 1.000)
C
       allocate(D2(N0+1:N2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,M2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(Z6(M2+1:N3,N1+1:M2,M1+1:N1,N2+1:M2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K0
       I2=K7*K9*K6
       I3=K2
       call EGEMM(I1,I2,I3,X6,D2,Z6)
       deallocate(D2)
C
       call
     & sum136245(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z6, 1.000)
       deallocate(Z6)
       deallocate(X6)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q90,B1)
       allocate(D2(N2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,M2,M1,N2,M1,N1,t2B,D2)
       allocate(S575(N1+1:M2,M1+1:N2,M1+1:N1,N0+1:N2))
       I1=K2
       I2=K7*K8*K9
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S575)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N2,N1,M2,M1,N2,M1,N1,X8,S575, 1.000)
       deallocate(S575)
C
       allocate(D2(N0+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,M2,N1,M2,M1,N1,t2B,D2)
       allocate(S583(N2+1:M2,N1+1:M2,M1+1:N1,N2+1:N3))
       I1=K4
       I2=K7*K9*K0
       I3=K2
       call EGEMM(I1,I2,I3,Q90,D2,S583)
       deallocate(D2)
C
       call
     & sum2341(N2,N3,N2,M2,N1,M2,M1,N1,X10,S583,-1.000)
       deallocate(S583)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:N3,N1+1:N3))
       call reorder4312(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N0,N2,N2,N3,N1,N3,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S483(N1+1:M2,N0+1:N2,N2+1:N3,N1+1:N3))
       I1=K3*K4*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S483)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N2+1:N3,N1+1:M2))
       call reorder2431(N1,M2,N0,N2,N2,N3,N1,N3,
     & N0,N2,N1,N3,N2,N3,N1,M2,S483,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,M1+1:N1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,M2,M1,N1,t2B,D2)
       allocate(S489(N2+1:M2,M1+1:N1,N2+1:N3,N1+1:M2))
       I1=K9*K4
       I2=K7*K0
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S489)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,N2,M2,N1,M2,M1,N1,X10,S489, 1.000)
       deallocate(S489)
C
       call sumx2314(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N2,M2,N1,M2,M1,N1,X10,VBHPPP, 1.000)
C
       allocate(D2(N2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,M2,N3,M1,N2,M1,N2,t2C,D2)
       allocate(Z10(M2+1:N3,M1+1:N2,M1+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       I1=K7*K9*K0
       I2=K8*K8*K6
       I3=K4
       call EGEMM(I1,I2,I3,X10,D2,Z10)
       deallocate(D2)
C
       call
     & sum145236(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z10,-1.000)
       deallocate(Z10)
       deallocate(X10)
C
       allocate(D1(N0+1:N2,N1+1:N3,N2+1:N3,N1+1:M2))
       call reorder2431(N1,M2,N0,N2,N2,N3,N1,N3,
     & N0,N2,N1,N3,N2,N3,N1,M2,S483,D1)
       allocate(D2(N0+1:N2,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,M2,N3,M1,N1,t2B,D2)
       allocate(S487(M2+1:N3,M1+1:N1,N2+1:N3,N1+1:M2))
       I1=K9*K4
       I2=K7*K6
       I3=K3*K2
       call EGEMM(I1,I2,I3,D1,D2,S487)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,M2,N3,N1,M2,M1,N1,X9,S487, 1.000)
       deallocate(S487)
C
       call sumx2314(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,M2,N3,N1,M2,M1,N1,X9,VBHPPP, 1.000)
C
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,M1,N2,M1,N2,t2C,D2)
       allocate(Z9(N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K6
       I2=K8*K8*K0
       I3=K4
       call EGEMM(I1,I2,I3,X9,D2,Z9)
       deallocate(D2)
C
       call
     & sum245136(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z9, 1.000)
       deallocate(Z9)
       deallocate(X9)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N1+1:M2))
       call reorder3241(N1,M2,N0,N2,N2,N3,N1,N3,
     & N2,N3,N0,N2,N1,N3,N1,M2,S483,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S591(M1+1:N2,N0+1:N2,N1+1:N3,N1+1:M2))
       I1=K9*K3*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S591)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N1,N3,N1,M2,
     & N0,N2,N1,N3,N1,M2,M1,N2,S591,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S592(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N2))
       I1=K8*K9*K3
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S592)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,N1,M2,M1,N2,X3,S592, 1.000)
       deallocate(S592)
C
       call sumx2341(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,M2,N3,N1,M2,M1,N2,X3,VBPHPP, 1.000)
C
       allocate(D2(N1+1:N3,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,M2,M1,N2,M1,N1,t2B,D2)
       allocate(Z3(N2+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,N1+1:M2,M1+1:N2))
       I1=K8*K9*K6
       I2=K7*K8*K0
       I3=K3
       call EGEMM(I1,I2,I3,X3,D2,Z3)
       deallocate(D2)
C
       call
     & sum246135(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z3,-1.000)
       call
     & sum256134(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z3, 1.000)
       deallocate(Z3)
       deallocate(X3)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder2341(N1,M2,N0,N2,N2,N3,N1,N3,
     & N0,N2,N2,N3,N1,N3,N1,M2,S483,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S484(M2+1:N3,M1+1:N2,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S484)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N1,N3,M2,N3,N1,M2,M1,N2,X100,S484, 1.000)
       deallocate(S484)
C
       allocate(D2(N1+1:N3,N2+1:M2,M1+1:N2,M1+1:N1))
       call reorder2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,M2,M1,N2,M1,N1,t2B,D2)
       allocate(Z255(N2+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,N1+1:M2,M1+1:N2))
       I1=K8*K9*K6
       I2=K7*K8*K0
       I3=K3
       call EGEMM(I1,I2,I3,X100,D2,Z255)
       deallocate(D2)
C
       call
     & sum256134(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z255,-1.000)
       call
     & sum246135(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z255, 1.000)
       deallocate(Z255)
       deallocate(X100)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:M2))
       call reorder3421(N1,M2,N0,N2,N2,N3,N1,N3,
     & N2,N3,N1,N3,N0,N2,N1,M2,S483,D1)
       allocate(D2(N2+1:N3,N1+1:N3,M1+1:N2,M1+1:N1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,M1,N2,M1,N1,t2B,D2)
       allocate(S485(M1+1:N2,M1+1:N1,N0+1:N2,N1+1:M2))
       I1=K9*K2
       I2=K7*K8
       I3=K3*K4
       call EGEMM(I1,I2,I3,D1,D2,S485)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,N2,N1,M2,M1,N2,M1,N1,X8,S485,-1.000)
       deallocate(S485)
C
       call sumx1234(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,M2,M1,N2,M1,N1,X8,VBHHPH, 1.000)
C
       allocate(D2(N0+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,M2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(Z8(M2+1:N3,N2+1:M2,M1+1:N2,N1+1:M2,M1+1:N2,M1+1:N1))
       I1=K7*K8*K9
       I2=K8*K0*K6
       I3=K2
       call EGEMM(I1,I2,I3,X8,D2,Z8)
       deallocate(D2)
C
       call
     & sum124356(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z8, 1.000)
       call
     & sum125346(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z8,-1.000)
       deallocate(Z8)
       deallocate(X8)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N1,N3,N1,M2,
     & N0,N2,N1,N3,N1,M2,M1,N2,S591,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S593(N2+1:M2,N1+1:N3,N1+1:M2,M1+1:N2))
       I1=K8*K9*K3
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S593)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,N2,M2,N1,M2,M1,N2,X4,S593, 1.000)
       deallocate(S593)
C
       call sumx2341(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,N2,M2,N1,M2,M1,N2,X4,VBPHPP, 1.000)
C
       allocate(D2(N1+1:N3,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M2,N3,M1,N2,M1,N1,t2B,D2)
       allocate(Z4(M2+1:N3,M1+1:N2,M1+1:N1,N2+1:M2,N1+1:M2,M1+1:N2))
       I1=K8*K9*K0
       I2=K7*K8*K6
       I3=K3
       call EGEMM(I1,I2,I3,X4,D2,Z4)
       deallocate(D2)
C
       call
     & sum146235(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z4, 1.000)
       call
     & sum156234(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z4,-1.000)
       deallocate(Z4)
       deallocate(X4)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N1,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S481(N2+1:M2,M1+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S481)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N2,M2,M1,N2,S481,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S482(N1+1:M2,N1+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S482)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,N3,N2,M2,N1,M2,M1,N2,X98,S482, 1.000)
       deallocate(S482)
C
       allocate(D2(N1+1:N3,M2+1:N3,M1+1:N2,M1+1:N1))
       call reorder2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M2,N3,M1,N2,M1,N1,t2B,D2)
       allocate(Z250(M2+1:N3,M1+1:N2,M1+1:N1,N2+1:M2,N1+1:M2,M1+1:N2))
       I1=K8*K9*K0
       I2=K7*K8*K6
       I3=K3
       call EGEMM(I1,I2,I3,X98,D2,Z250)
       deallocate(D2)
C
       call
     & sum156234(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z250, 1.000)
       call
     & sum146235(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z250,-1.000)
       deallocate(Z250)
       deallocate(X98)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder4132(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S571(N2+1:M2,M1+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S571)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N2,M2,M1,N2,S571,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S572(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S572)
       deallocate(D1)
       deallocate(B2)
       deallocate(S571)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,M1,N2,X101,S572, 1.000)
       deallocate(S572)
C
       allocate(D2(N2+1:N3,N1+1:M2,M1+1:N2,M1+1:N1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,M2,M1,N2,M1,N1,t2B,D2)
       allocate(Z263(N1+1:M2,M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K6
       I2=K7*K8*K9
       I3=K4
       call EGEMM(I1,I2,I3,X101,D2,Z263)
       deallocate(D2)
C
       call
     & sum356124(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z263,-1.000)
       call
     & sum346125(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z263, 1.000)
       deallocate(Z263)
       deallocate(X101)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,M2,N3,N1,M2,M1,N1,VBHPPH,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z70(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K8*K8*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z70)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z70, 1.000)
       deallocate(Z70)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N2,M2,N3,N1,M2,M1,N1,VBHPPH,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z71(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K8*K8*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z71)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z71, 1.000)
       deallocate(Z71)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,N2,M2,N1,M2,M1,N1,VBHPPH,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z72(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K8*K8*K0*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z72)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z72,-1.000)
       deallocate(Z72)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N2,N2,M2,N1,M2,M1,N1,VBHPPH,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z73(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K8*K8*K0*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z73)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,V3C,Z73,-1.000)
       deallocate(Z73)
C
       call sumx3(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M2,N3,N2,M2,N1,M2,M1,N2,M1,N2,M1,N1,HT3C1,V3C,1.0)
       deallocate(V3C)
C
       end
