       subroutine t3D110110_update(N0,N1,N2,N3,HT3D,shift,
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
       real*8,allocatable::Q9(:,:)
       real*8,allocatable::Q10(:,:)
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
       real*8,allocatable::S75(:,:,:,:)
       real*8,allocatable::S76(:,:,:,:)
       real*8,allocatable::S77(:,:,:,:)
       real*8,allocatable::S78(:,:,:,:)
       real*8,allocatable::S79(:,:,:,:)
       real*8,allocatable::S80(:,:,:,:)
       real*8,allocatable::S81(:,:,:,:)
       real*8,allocatable::Q11(:,:)
       real*8,allocatable::Q12(:,:)
       real*8,allocatable::Q13(:,:)
       real*8,allocatable::Q14(:,:)
       real*8,allocatable::Q15(:,:)
       real*8,allocatable::Q16(:,:)
       real*8,allocatable::Q17(:,:)
       real*8,allocatable::Q18(:,:)
       real*8,allocatable::Q19(:,:)
       real*8,allocatable::Q20(:,:)
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
       real*8,allocatable::Q21(:,:)
       real*8,allocatable::Q22(:,:)
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
       real*8,allocatable::S120(:,:,:,:)
       real*8,allocatable::S121(:,:,:,:)
       real*8,allocatable::S122(:,:,:,:)
       real*8,allocatable::S123(:,:,:,:)
       real*8,allocatable::S124(:,:,:,:)
       real*8,allocatable::S125(:,:,:,:)
       real*8,allocatable::Q23(:,:)
       real*8,allocatable::Q24(:,:)
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
       real*8,allocatable::Q25(:,:)
       real*8,allocatable::Q26(:,:)
       real*8,allocatable::S144(:,:,:,:)
       real*8,allocatable::S145(:,:,:,:)
       real*8,allocatable::S146(:,:,:,:)
       real*8,allocatable::S147(:,:,:,:)
       real*8,allocatable::S148(:,:,:,:)
       real*8,allocatable::S149(:,:,:,:)
       real*8,allocatable::S150(:,:,:,:)
       real*8,allocatable::S151(:,:,:,:)
       real*8,allocatable::Q27(:,:)
       real*8,allocatable::Q28(:,:)
       real*8,allocatable::S152(:,:,:,:)
       real*8,allocatable::S153(:,:,:,:)
       real*8,allocatable::Q29(:,:)
       real*8,allocatable::Q30(:,:)
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
       real*8,allocatable::Q31(:,:)
       real*8,allocatable::Q32(:,:)
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
       real*8,allocatable::Q33(:,:)
       real*8,allocatable::Q34(:,:)
       real*8,allocatable::Q35(:,:)
       real*8,allocatable::Q36(:,:)
       real*8,allocatable::Q37(:,:)
       real*8,allocatable::Q38(:,:)
       real*8,allocatable::Q39(:,:)
       real*8,allocatable::Q40(:,:)
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
       real*8,allocatable::S382(:,:,:,:)
       real*8,allocatable::S383(:,:,:,:)
       real*8,allocatable::Q41(:,:)
       real*8,allocatable::Q42(:,:)
       real*8,allocatable::S384(:,:,:,:)
       real*8,allocatable::S385(:,:,:,:)
       real*8,allocatable::S386(:,:,:,:)
       real*8,allocatable::S387(:,:,:,:)
       real*8,allocatable::S388(:,:,:,:)
       real*8,allocatable::S389(:,:,:,:)
       real*8,allocatable::S390(:,:,:,:)
       real*8,allocatable::S391(:,:,:,:)
       real*8,allocatable::S392(:,:,:,:)
       real*8,allocatable::S393(:,:,:,:)
       real*8,allocatable::S394(:,:,:,:)
       real*8,allocatable::S395(:,:,:,:)
       real*8,allocatable::S396(:,:,:,:)
       real*8,allocatable::S397(:,:,:,:)
       real*8,allocatable::S398(:,:,:,:)
       real*8,allocatable::S399(:,:,:,:)
       real*8,allocatable::S400(:,:,:,:)
       real*8,allocatable::S401(:,:,:,:)
       real*8,allocatable::S402(:,:,:,:)
       real*8,allocatable::S403(:,:,:,:)
       real*8,allocatable::S404(:,:,:,:)
       real*8,allocatable::S405(:,:,:,:)
       real*8,allocatable::S406(:,:,:,:)
       real*8,allocatable::S407(:,:,:,:)
       real*8,allocatable::S408(:,:,:,:)
       real*8,allocatable::S409(:,:,:,:)
       real*8,allocatable::S410(:,:,:,:)
       real*8,allocatable::S411(:,:,:,:)
       real*8,allocatable::Q43(:,:)
       real*8,allocatable::Q44(:,:)
       real*8,allocatable::S412(:,:,:,:)
       real*8,allocatable::S413(:,:,:,:)
       real*8,allocatable::S414(:,:,:,:)
       real*8,allocatable::Q45(:,:)
       real*8,allocatable::Q46(:,:)
       real*8,allocatable::S415(:,:,:,:)
       real*8,allocatable::S416(:,:,:,:)
       real*8,allocatable::Q47(:,:)
       real*8,allocatable::Q48(:,:)
       real*8,allocatable::Q49(:,:)
       real*8,allocatable::Q50(:,:)
       real*8,allocatable::Q51(:,:)
       real*8,allocatable::Q55(:,:)
       real*8,allocatable::Q52(:,:)
       real*8,allocatable::Q53(:,:)
       real*8,allocatable::Q56(:,:)
       real*8,allocatable::Q54(:,:)
       real*8,allocatable::Q57(:,:)
       real*8,allocatable::Q61(:,:)
       real*8,allocatable::Q63(:,:)
       real*8,allocatable::Q58(:,:)
       real*8,allocatable::Q59(:,:)
       real*8,allocatable::Q60(:,:)
       real*8,allocatable::Q64(:,:)
       real*8,allocatable::Q62(:,:)
       real*8,allocatable::Q65(:,:)
       real*8,allocatable::S418(:,:,:,:)
       real*8,allocatable::S419(:,:,:,:)
       real*8,allocatable::S420(:,:,:,:)
       real*8,allocatable::S417(:,:,:,:)
       real*8,allocatable::S421(:,:,:,:)
       real*8,allocatable::S423(:,:,:,:)
       real*8,allocatable::S424(:,:,:,:)
       real*8,allocatable::S422(:,:,:,:)
       real*8,allocatable::S425(:,:,:,:)
       real*8,allocatable::S427(:,:,:,:)
       real*8,allocatable::S428(:,:,:,:)
       real*8,allocatable::S426(:,:,:,:)
       real*8,allocatable::S429(:,:,:,:)
       real*8,allocatable::S433(:,:,:,:)
       real*8,allocatable::S430(:,:,:,:)
       real*8,allocatable::S431(:,:,:,:)
       real*8,allocatable::S432(:,:,:,:)
       real*8,allocatable::S434(:,:,:,:)
       real*8,allocatable::S444(:,:,:,:)
       real*8,allocatable::S447(:,:,:,:)
       real*8,allocatable::S448(:,:,:,:)
       real*8,allocatable::S435(:,:,:,:)
       real*8,allocatable::S440(:,:,:,:)
       real*8,allocatable::S443(:,:,:,:)
       real*8,allocatable::S441(:,:,:,:)
       real*8,allocatable::S442(:,:,:,:)
       real*8,allocatable::S436(:,:,:,:)
       real*8,allocatable::S437(:,:,:,:)
       real*8,allocatable::S449(:,:,:,:)
       real*8,allocatable::S451(:,:,:,:)
       real*8,allocatable::S445(:,:,:,:)
       real*8,allocatable::S438(:,:,:,:)
       real*8,allocatable::S450(:,:,:,:)
       real*8,allocatable::S446(:,:,:,:)
       real*8,allocatable::S452(:,:,:,:)
       real*8,allocatable::S439(:,:,:,:)
       real*8,allocatable::S453(:,:,:,:)
       real*8,allocatable::S457(:,:,:,:)
       real*8,allocatable::S454(:,:,:,:)
       real*8,allocatable::S455(:,:,:,:)
       real*8,allocatable::S456(:,:,:,:)
       real*8,allocatable::S458(:,:,:,:)
       real*8,allocatable::S460(:,:,:,:)
       real*8,allocatable::S459(:,:,:,:)
       real*8,allocatable::S461(:,:,:,:)
       real*8,allocatable::S465(:,:,:,:)
       real*8,allocatable::S462(:,:,:,:)
       real*8,allocatable::S463(:,:,:,:)
       real*8,allocatable::S466(:,:,:,:)
       real*8,allocatable::S464(:,:,:,:)
       real*8,allocatable::S467(:,:,:,:)
       real*8,allocatable::S475(:,:,:,:)
       real*8,allocatable::S479(:,:,:,:)
       real*8,allocatable::S468(:,:,:,:)
       real*8,allocatable::S473(:,:,:,:)
       real*8,allocatable::S478(:,:,:,:)
       real*8,allocatable::S482(:,:,:,:)
       real*8,allocatable::S474(:,:,:,:)
       real*8,allocatable::S471(:,:,:,:)
       real*8,allocatable::S472(:,:,:,:)
       real*8,allocatable::S481(:,:,:,:)
       real*8,allocatable::S477(:,:,:,:)
       real*8,allocatable::S469(:,:,:,:)
       real*8,allocatable::S470(:,:,:,:)
       real*8,allocatable::S476(:,:,:,:)
       real*8,allocatable::S480(:,:,:,:)
       real*8,allocatable::S483(:,:,:,:)
       real*8,allocatable::S491(:,:,:,:)
       real*8,allocatable::S495(:,:,:,:)
       real*8,allocatable::S484(:,:,:,:)
       real*8,allocatable::S489(:,:,:,:)
       real*8,allocatable::S494(:,:,:,:)
       real*8,allocatable::S498(:,:,:,:)
       real*8,allocatable::S490(:,:,:,:)
       real*8,allocatable::S487(:,:,:,:)
       real*8,allocatable::S488(:,:,:,:)
       real*8,allocatable::S497(:,:,:,:)
       real*8,allocatable::S493(:,:,:,:)
       real*8,allocatable::S485(:,:,:,:)
       real*8,allocatable::S486(:,:,:,:)
       real*8,allocatable::S492(:,:,:,:)
       real*8,allocatable::S496(:,:,:,:)
       real*8,allocatable::S499(:,:,:,:)
       real*8,allocatable::S505(:,:,:,:)
       real*8,allocatable::S500(:,:,:,:)
       real*8,allocatable::S503(:,:,:,:)
       real*8,allocatable::S504(:,:,:,:)
       real*8,allocatable::S501(:,:,:,:)
       real*8,allocatable::S506(:,:,:,:)
       real*8,allocatable::S502(:,:,:,:)
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
       real*8,allocatable::S526(:,:,:,:)
       real*8,allocatable::Q66(:,:)
       real*8,allocatable::Q70(:,:)
       real*8,allocatable::Q67(:,:)
       real*8,allocatable::S527(:,:,:,:)
       real*8,allocatable::S535(:,:,:,:)
       real*8,allocatable::S528(:,:,:,:)
       real*8,allocatable::S531(:,:,:,:)
       real*8,allocatable::S537(:,:,:,:)
       real*8,allocatable::S532(:,:,:,:)
       real*8,allocatable::S529(:,:,:,:)
       real*8,allocatable::S536(:,:,:,:)
       real*8,allocatable::S530(:,:,:,:)
       real*8,allocatable::S533(:,:,:,:)
       real*8,allocatable::S538(:,:,:,:)
       real*8,allocatable::S534(:,:,:,:)
       real*8,allocatable::S539(:,:,:,:)
       real*8,allocatable::S540(:,:,:,:)
       real*8,allocatable::S541(:,:,:,:)
       real*8,allocatable::S542(:,:,:,:)
       real*8,allocatable::S543(:,:,:,:)
       real*8,allocatable::S544(:,:,:,:)
       real*8,allocatable::S545(:,:,:,:)
       real*8,allocatable::S546(:,:,:,:)
       real*8,allocatable::Q68(:,:)
       real*8,allocatable::Q71(:,:)
       real*8,allocatable::Q69(:,:)
       real*8,allocatable::S547(:,:,:,:)
       real*8,allocatable::S548(:,:,:,:)
       real*8,allocatable::S549(:,:,:,:)
       real*8,allocatable::S559(:,:,:,:)
       real*8,allocatable::S550(:,:,:,:)
       real*8,allocatable::S553(:,:,:,:)
       real*8,allocatable::S554(:,:,:,:)
       real*8,allocatable::S555(:,:,:,:)
       real*8,allocatable::S556(:,:,:,:)
       real*8,allocatable::S557(:,:,:,:)
       real*8,allocatable::S558(:,:,:,:)
       real*8,allocatable::Q72(:,:)
       real*8,allocatable::Q76(:,:)
       real*8,allocatable::Q78(:,:)
       real*8,allocatable::Q73(:,:)
       real*8,allocatable::S551(:,:,:,:)
       real*8,allocatable::S552(:,:,:,:)
       real*8,allocatable::S560(:,:,:,:)
       real*8,allocatable::Q74(:,:)
       real*8,allocatable::Q75(:,:)
       real*8,allocatable::Q79(:,:)
       real*8,allocatable::Q77(:,:)
       real*8,allocatable::S561(:,:,:,:)
       real*8,allocatable::S563(:,:,:,:)
       real*8,allocatable::S564(:,:,:,:)
       real*8,allocatable::S565(:,:,:,:)
       real*8,allocatable::S566(:,:,:,:)
       real*8,allocatable::S567(:,:,:,:)
       real*8,allocatable::S562(:,:,:,:)
       real*8,allocatable::S568(:,:,:,:)
       real*8,allocatable::S570(:,:,:,:)
       real*8,allocatable::S571(:,:,:,:)
       real*8,allocatable::S569(:,:,:,:)
       real*8,allocatable::S572(:,:,:,:)
       real*8,allocatable::S581(:,:,:,:)
       real*8,allocatable::S573(:,:,:,:)
       real*8,allocatable::S576(:,:,:,:)
       real*8,allocatable::S586(:,:,:,:)
       real*8,allocatable::S588(:,:,:,:)
       real*8,allocatable::S577(:,:,:,:)
       real*8,allocatable::S574(:,:,:,:)
       real*8,allocatable::S580(:,:,:,:)
       real*8,allocatable::S575(:,:,:,:)
       real*8,allocatable::S584(:,:,:,:)
       real*8,allocatable::S585(:,:,:,:)
       real*8,allocatable::S578(:,:,:,:)
       real*8,allocatable::S587(:,:,:,:)
       real*8,allocatable::S589(:,:,:,:)
       real*8,allocatable::S579(:,:,:,:)
       real*8,allocatable::S582(:,:,:,:)
       real*8,allocatable::S583(:,:,:,:)
       real*8,allocatable::S590(:,:,:,:)
       real*8,allocatable::S592(:,:,:,:)
       real*8,allocatable::S593(:,:,:,:)
       real*8,allocatable::S594(:,:,:,:)
       real*8,allocatable::S595(:,:,:,:)
       real*8,allocatable::S596(:,:,:,:)
       real*8,allocatable::S597(:,:,:,:)
       real*8,allocatable::S598(:,:,:,:)
       real*8,allocatable::S633(:,:,:,:)
       real*8,allocatable::S634(:,:,:,:)
       real*8,allocatable::S637(:,:,:,:)
       real*8,allocatable::S638(:,:,:,:)
       real*8,allocatable::S641(:,:,:,:)
       real*8,allocatable::S642(:,:,:,:)
       real*8,allocatable::S643(:,:,:,:)
       real*8,allocatable::S644(:,:,:,:)
       real*8,allocatable::S591(:,:,:,:)
       real*8,allocatable::S635(:,:,:,:)
       real*8,allocatable::S636(:,:,:,:)
       real*8,allocatable::S639(:,:,:,:)
       real*8,allocatable::S640(:,:,:,:)
       real*8,allocatable::S645(:,:,:,:)
       real*8,allocatable::S605(:,:,:,:)
       real*8,allocatable::S616(:,:,:,:)
       real*8,allocatable::S606(:,:,:,:)
       real*8,allocatable::S612(:,:,:,:)
       real*8,allocatable::S613(:,:,:,:)
       real*8,allocatable::S626(:,:,:,:)
       real*8,allocatable::S630(:,:,:,:)
       real*8,allocatable::S632(:,:,:,:)
       real*8,allocatable::S614(:,:,:,:)
       real*8,allocatable::S620(:,:,:,:)
       real*8,allocatable::S622(:,:,:,:)
       real*8,allocatable::S621(:,:,:,:)
       real*8,allocatable::S623(:,:,:,:)
       real*8,allocatable::S624(:,:,:,:)
       real*8,allocatable::S610(:,:,:,:)
       real*8,allocatable::S628(:,:,:,:)
       real*8,allocatable::S629(:,:,:,:)
       real*8,allocatable::S611(:,:,:,:)
       real*8,allocatable::S625(:,:,:,:)
       real*8,allocatable::Q80(:,:)
       real*8,allocatable::S631(:,:,:,:)
       real*8,allocatable::S615(:,:,:,:)
       real*8,allocatable::S609(:,:,:,:)
       real*8,allocatable::S627(:,:,:,:)
       real*8,allocatable::S599(:,:,:,:)
       real*8,allocatable::S601(:,:,:,:)
       real*8,allocatable::S618(:,:,:,:)
       real*8,allocatable::S619(:,:,:,:)
       real*8,allocatable::S617(:,:,:,:)
       real*8,allocatable::S646(:,:,:,:)
       real*8,allocatable::S647(:,:,:,:)
       real*8,allocatable::S648(:,:,:,:)
       real*8,allocatable::S649(:,:,:,:)
       real*8,allocatable::S600(:,:,:,:)
       real*8,allocatable::S650(:,:,:,:)
       real*8,allocatable::S602(:,:,:,:)
       real*8,allocatable::S607(:,:,:,:)
       real*8,allocatable::S608(:,:,:,:)
       real*8,allocatable::S604(:,:,:,:)
       real*8,allocatable::S603(:,:,:,:)
       real*8,allocatable::X1(:,:,:,:)
       real*8,allocatable::Z1(:,:,:,:,:,:)
       real*8,allocatable::X2(:,:,:,:)
       real*8,allocatable::Z2(:,:,:,:,:,:)
       real*8,allocatable::X3(:,:,:,:)
       real*8,allocatable::Z3(:,:,:,:,:,:)
       real*8,allocatable::X4(:,:,:,:)
       real*8,allocatable::Z4(:,:,:,:,:,:)
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
       real*8,allocatable::X12(:,:,:,:)
       real*8,allocatable::Z13(:,:,:,:,:,:)
       real*8,allocatable::X13(:,:,:,:)
       real*8,allocatable::Z14(:,:,:,:,:,:)
       real*8,allocatable::X14(:,:,:,:)
       real*8,allocatable::Z15(:,:,:,:,:,:)
       real*8,allocatable::X15(:,:,:,:)
       real*8,allocatable::Z16(:,:,:,:,:,:)
       real*8,allocatable::X16(:,:,:,:)
       real*8,allocatable::Z17(:,:,:,:,:,:)
       real*8,allocatable::X17(:,:,:,:)
       real*8,allocatable::Z18(:,:,:,:,:,:)
       real*8,allocatable::X18(:,:,:,:)
       real*8,allocatable::Z19(:,:,:,:,:,:)
       real*8,allocatable::X19(:,:,:,:)
       real*8,allocatable::Z20(:,:,:,:,:,:)
       real*8,allocatable::X20(:,:,:,:)
       real*8,allocatable::Z21(:,:,:,:,:,:)
       real*8,allocatable::X21(:,:,:,:)
       real*8,allocatable::Z22(:,:,:,:,:,:)
       real*8,allocatable::X22(:,:,:,:)
       real*8,allocatable::Z23(:,:,:,:,:,:)
       real*8,allocatable::X23(:,:,:,:)
       real*8,allocatable::Z24(:,:,:,:,:,:)
       real*8,allocatable::X24(:,:)
       real*8,allocatable::Z25(:,:,:,:,:,:)
       real*8,allocatable::X25(:,:)
       real*8,allocatable::Z26(:,:,:,:,:,:)
       real*8,allocatable::X26(:,:)
       real*8,allocatable::Z27(:,:,:,:,:,:)
       real*8,allocatable::X27(:,:)
       real*8,allocatable::Z28(:,:,:,:,:,:)
       real*8,allocatable::X28(:,:)
       real*8,allocatable::Z29(:,:,:,:,:,:)
       real*8,allocatable::X29(:,:)
       real*8,allocatable::Z30(:,:,:,:,:,:)
       real*8,allocatable::X30(:,:)
       real*8,allocatable::Z31(:,:,:,:,:,:)
       real*8,allocatable::X31(:,:)
       real*8,allocatable::Z32(:,:,:,:,:,:)
       real*8,allocatable::X32(:,:)
       real*8,allocatable::Z33(:,:,:,:,:,:)
       real*8,allocatable::X33(:,:)
       real*8,allocatable::Z34(:,:,:,:,:,:)
       real*8,allocatable::X34(:,:,:,:)
       real*8,allocatable::Z35(:,:,:,:,:,:)
       real*8,allocatable::X35(:,:,:,:)
       real*8,allocatable::Z36(:,:,:,:,:,:)
       real*8,allocatable::X36(:,:,:,:)
       real*8,allocatable::Z37(:,:,:,:,:,:)
       real*8,allocatable::X37(:,:,:,:)
       real*8,allocatable::Z38(:,:,:,:,:,:)
       real*8,allocatable::X38(:,:,:,:)
       real*8,allocatable::Z39(:,:,:,:,:,:)
       real*8,allocatable::X39(:,:,:,:)
       real*8,allocatable::Z40(:,:,:,:,:,:)
       real*8,allocatable::X40(:,:,:,:)
       real*8,allocatable::Z41(:,:,:,:,:,:)
       real*8,allocatable::X41(:,:,:,:)
       real*8,allocatable::Z42(:,:,:,:,:,:)
       real*8,allocatable::X42(:,:,:,:)
       real*8,allocatable::Z43(:,:,:,:,:,:)
       real*8,allocatable::Z44(:,:,:,:,:,:)
       real*8,allocatable::Z45(:,:,:,:,:,:)
       real*8,allocatable::Z46(:,:,:,:,:,:)
       real*8,allocatable::Z47(:,:,:,:,:,:)
       real*8,allocatable::X43(:,:,:,:)
       real*8,allocatable::Z48(:,:,:,:,:,:)
       real*8,allocatable::X44(:,:,:,:)
       real*8,allocatable::Z49(:,:,:,:,:,:)
       real*8,allocatable::X45(:,:,:,:)
       real*8,allocatable::Z50(:,:,:,:,:,:)
       real*8,allocatable::X46(:,:,:,:)
       real*8,allocatable::Z51(:,:,:,:,:,:)
       real*8,allocatable::Z52(:,:,:,:,:,:)
       real*8,allocatable::Z53(:,:,:,:,:,:)
       real*8,allocatable::Z54(:,:,:,:,:,:)
       real*8,allocatable::Z55(:,:,:,:,:,:)
       real*8,allocatable::X47(:,:,:,:)
       real*8,allocatable::Z56(:,:,:,:,:,:)
       real*8,allocatable::X48(:,:,:,:)
       real*8,allocatable::Z57(:,:,:,:,:,:)
       real*8,allocatable::X49(:,:,:,:)
       real*8,allocatable::Z58(:,:,:,:,:,:)
       real*8,allocatable::X50(:,:,:,:)
       real*8,allocatable::Z59(:,:,:,:,:,:)
       real*8,allocatable::X51(:,:,:,:)
       real*8,allocatable::Z60(:,:,:,:,:,:)
       real*8,allocatable::X52(:,:)
       real*8,allocatable::Z61(:,:,:,:,:,:)
       real*8,allocatable::X53(:,:)
       real*8,allocatable::Z62(:,:,:,:,:,:)
       real*8,allocatable::X54(:,:)
       real*8,allocatable::Z63(:,:,:,:,:,:)
       real*8,allocatable::X55(:,:)
       real*8,allocatable::Z64(:,:,:,:,:,:)
       real*8,allocatable::X56(:,:)
       real*8,allocatable::Z65(:,:,:,:,:,:)
       real*8,allocatable::X57(:,:)
       real*8,allocatable::Z66(:,:,:,:,:,:)
       real*8,allocatable::X58(:,:)
       real*8,allocatable::Z67(:,:,:,:,:,:)
       real*8,allocatable::X59(:,:)
       real*8,allocatable::Z68(:,:,:,:,:,:)
       real*8,allocatable::X60(:,:)
       real*8,allocatable::Z69(:,:,:,:,:,:)
       real*8,allocatable::X61(:,:)
       real*8,allocatable::Z70(:,:,:,:,:,:)
       real*8,allocatable::X62(:,:,:,:)
       real*8,allocatable::Z72(:,:,:,:,:,:)
       real*8,allocatable::X63(:,:,:,:)
       real*8,allocatable::Z73(:,:,:,:,:,:)
       real*8,allocatable::X64(:,:,:,:)
       real*8,allocatable::Z74(:,:,:,:,:,:)
       real*8,allocatable::X65(:,:,:,:)
       real*8,allocatable::Z75(:,:,:,:,:,:)
       real*8,allocatable::X66(:,:,:,:)
       real*8,allocatable::Z76(:,:,:,:,:,:)
       real*8,allocatable::X67(:,:,:,:)
       real*8,allocatable::Z78(:,:,:,:,:,:)
       real*8,allocatable::X68(:,:,:,:)
       real*8,allocatable::Z79(:,:,:,:,:,:)
       real*8,allocatable::X69(:,:,:,:)
       real*8,allocatable::Z83(:,:,:,:,:,:)
       real*8,allocatable::X70(:,:,:,:)
       real*8,allocatable::Z84(:,:,:,:,:,:)
       real*8,allocatable::X71(:,:,:,:)
       real*8,allocatable::Z95(:,:,:,:,:,:)
       real*8,allocatable::X72(:,:,:,:)
       real*8,allocatable::Z96(:,:,:,:,:,:)
       real*8,allocatable::X73(:,:,:,:)
       real*8,allocatable::Z108(:,:,:,:,:,:)
       real*8,allocatable::X74(:,:,:,:)
       real*8,allocatable::Z109(:,:,:,:,:,:)
       real*8,allocatable::X75(:,:,:,:)
       real*8,allocatable::Z110(:,:,:,:,:,:)
       real*8,allocatable::X76(:,:,:,:)
       real*8,allocatable::Z111(:,:,:,:,:,:)
       real*8,allocatable::X77(:,:,:,:)
       real*8,allocatable::Z112(:,:,:,:,:,:)
       real*8,allocatable::X78(:,:,:,:)
       real*8,allocatable::Z113(:,:,:,:,:,:)
       real*8,allocatable::X79(:,:,:,:)
       real*8,allocatable::Z114(:,:,:,:,:,:)
       real*8,allocatable::X80(:,:,:,:)
       real*8,allocatable::Z115(:,:,:,:,:,:)
       real*8,allocatable::X81(:,:,:,:)
       real*8,allocatable::Z132(:,:,:,:,:,:)
       real*8,allocatable::X82(:,:,:,:)
       real*8,allocatable::Z133(:,:,:,:,:,:)
       real*8,allocatable::X83(:,:,:,:)
       real*8,allocatable::Z134(:,:,:,:,:,:)
       real*8,allocatable::X84(:,:,:,:)
       real*8,allocatable::Z135(:,:,:,:,:,:)
       real*8,allocatable::X85(:,:,:,:)
       real*8,allocatable::Z136(:,:,:,:,:,:)
       real*8,allocatable::X86(:,:,:,:)
       real*8,allocatable::Z137(:,:,:,:,:,:)
       real*8,allocatable::X87(:,:,:,:)
       real*8,allocatable::Z138(:,:,:,:,:,:)
       real*8,allocatable::X88(:,:,:,:)
       real*8,allocatable::Z139(:,:,:,:,:,:)
       real*8,allocatable::Z162(:,:,:,:,:,:)
       real*8,allocatable::Z163(:,:,:,:,:,:)
       real*8,allocatable::X89(:,:,:,:)
       real*8,allocatable::Z171(:,:,:,:,:,:)
       real*8,allocatable::X90(:,:,:,:)
       real*8,allocatable::Z172(:,:,:,:,:,:)
       real*8,allocatable::X91(:,:,:,:)
       real*8,allocatable::Z173(:,:,:,:,:,:)
       real*8,allocatable::X92(:,:,:,:)
       real*8,allocatable::Z174(:,:,:,:,:,:)
       real*8,allocatable::X93(:,:,:,:)
       real*8,allocatable::Z175(:,:,:,:,:,:)
       real*8,allocatable::X94(:,:,:,:)
       real*8,allocatable::Z176(:,:,:,:,:,:)
       real*8,allocatable::X95(:,:,:,:)
       real*8,allocatable::Z177(:,:,:,:,:,:)
       real*8,allocatable::X96(:,:,:,:)
       real*8,allocatable::Z178(:,:,:,:,:,:)
       real*8,allocatable::Z192(:,:,:,:,:,:)
       real*8,allocatable::Z193(:,:,:,:,:,:)
       real*8,allocatable::Z194(:,:,:,:,:,:)
       real*8,allocatable::Z195(:,:,:,:,:,:)
       real*8,allocatable::X97(:,:,:,:)
       real*8,allocatable::Z200(:,:,:,:,:,:)
       real*8,allocatable::X98(:,:,:,:)
       real*8,allocatable::Z201(:,:,:,:,:,:)
       real*8,allocatable::X99(:,:,:,:)
       real*8,allocatable::Z202(:,:,:,:,:,:)
       real*8,allocatable::X100(:,:,:,:)
       real*8,allocatable::Z203(:,:,:,:,:,:)
       real*8,allocatable::X101(:,:,:,:)
       real*8,allocatable::Z204(:,:,:,:,:,:)
       real*8,allocatable::X102(:,:,:,:)
       real*8,allocatable::Z205(:,:,:,:,:,:)
       real*8,allocatable::X103(:,:,:,:)
       real*8,allocatable::Z206(:,:,:,:,:,:)
       real*8,allocatable::X104(:,:,:,:)
       real*8,allocatable::Z207(:,:,:,:,:,:)
       real*8,allocatable::Z218(:,:,:,:,:,:)
       real*8,allocatable::Z219(:,:,:,:,:,:)
       real*8,allocatable::Z220(:,:,:,:,:,:)
       real*8,allocatable::Z221(:,:,:,:,:,:)
       real*8,allocatable::X105(:,:,:,:)
       real*8,allocatable::Z222(:,:,:,:,:,:)
       real*8,allocatable::X106(:,:,:,:)
       real*8,allocatable::Z223(:,:,:,:,:,:)
       real*8,allocatable::X107(:,:,:,:)
       real*8,allocatable::Z224(:,:,:,:,:,:)
       real*8,allocatable::X108(:,:,:,:)
       real*8,allocatable::Z225(:,:,:,:,:,:)
       real*8,allocatable::X109(:,:,:,:)
       real*8,allocatable::Z226(:,:,:,:,:,:)
       real*8,allocatable::X110(:,:,:,:)
       real*8,allocatable::Z227(:,:,:,:,:,:)
       real*8,allocatable::Z265(:,:,:,:,:,:)
       real*8,allocatable::Z266(:,:,:,:,:,:)
       real*8,allocatable::Z267(:,:,:,:,:,:)
       real*8,allocatable::Z268(:,:,:,:,:,:)
       real*8,allocatable::Z269(:,:,:,:,:,:)
       real*8,allocatable::Z270(:,:,:,:,:,:)
       real*8,allocatable::Z271(:,:,:,:,:,:)
       real*8,allocatable::Z272(:,:,:,:,:,:)
       real*8,allocatable::Z273(:,:,:,:,:,:)
       real*8,allocatable::Z274(:,:,:,:,:,:)
       real*8,allocatable::Z275(:,:,:,:,:,:)
       real*8,allocatable::Z276(:,:,:,:,:,:)
       real*8,allocatable::Z277(:,:,:,:,:,:)
       real*8,allocatable::Z278(:,:,:,:,:,:)
       real*8,allocatable::Z279(:,:,:,:,:,:)
       real*8,allocatable::Z280(:,:,:,:,:,:)
       real*8,allocatable::Z281(:,:,:,:,:,:)
       real*8,allocatable::Z282(:,:,:,:,:,:)
       real*8,allocatable::Z283(:,:,:,:,:,:)
       real*8,allocatable::Z284(:,:,:,:,:,:)
       real*8,allocatable::Z285(:,:,:,:,:,:)
       real*8,allocatable::Z286(:,:,:,:,:,:)
       real*8,allocatable::Z287(:,:,:,:,:,:)
       real*8,allocatable::Z288(:,:,:,:,:,:)
       real*8,allocatable::Z289(:,:,:,:,:,:)
       real*8,allocatable::Z290(:,:,:,:,:,:)
       real*8,allocatable::Z291(:,:,:,:,:,:)
       real*8,allocatable::Z292(:,:,:,:,:,:)
       real*8,allocatable::Z293(:,:,:,:,:,:)
       real*8,allocatable::Z294(:,:,:,:,:,:)
       real*8,allocatable::Z295(:,:,:,:,:,:)
       real*8,allocatable::Z296(:,:,:,:,:,:)
       real*8,allocatable::X111(:,:,:,:)
       real*8,allocatable::Z297(:,:,:,:,:,:)
       real*8,allocatable::X112(:,:,:,:)
       real*8,allocatable::Z298(:,:,:,:,:,:)
       real*8,allocatable::X113(:,:,:,:)
       real*8,allocatable::Z299(:,:,:,:,:,:)
       real*8,allocatable::X114(:,:,:,:)
       real*8,allocatable::Z300(:,:,:,:,:,:)
       real*8,allocatable::Z303(:,:,:,:,:,:)
       real*8,allocatable::Z304(:,:,:,:,:,:)
       real*8,allocatable::Z305(:,:,:,:,:,:)
       real*8,allocatable::Z306(:,:,:,:,:,:)
       real*8,allocatable::Z307(:,:,:,:,:,:)
       real*8,allocatable::Z308(:,:,:,:,:,:)
       real*8,allocatable::Z309(:,:,:,:,:,:)
       real*8,allocatable::Z310(:,:,:,:,:,:)
       real*8,allocatable::X115(:,:,:,:)
       real*8,allocatable::Z311(:,:,:,:,:,:)
       real*8,allocatable::X116(:,:,:,:)
       real*8,allocatable::Z312(:,:,:,:,:,:)
       real*8,allocatable::X117(:,:,:,:)
       real*8,allocatable::Z313(:,:,:,:,:,:)
       real*8,allocatable::X118(:,:,:,:)
       real*8,allocatable::Z314(:,:,:,:,:,:)
       real*8,allocatable::X119(:,:,:,:)
       real*8,allocatable::Z326(:,:,:,:,:,:)
       real*8,allocatable::X120(:,:,:,:)
       real*8,allocatable::Z330(:,:,:,:,:,:)
       real*8,allocatable::Z331(:,:,:,:,:,:)
       real*8,allocatable::Z334(:,:,:,:,:,:)
       real*8,allocatable::X121(:,:,:,:)
       real*8,allocatable::Z336(:,:,:,:,:,:)
       real*8,allocatable::Z339(:,:,:,:,:,:)
       real*8,allocatable::X122(:,:,:,:)
       real*8,allocatable::Z342(:,:,:,:,:,:)
       real*8,allocatable::X123(:,:,:,:)
       real*8,allocatable::Z344(:,:,:,:,:,:)
       real*8,allocatable::Z346(:,:,:,:,:,:)
       real*8,allocatable::Z347(:,:,:,:,:,:)
       real*8,allocatable::X124(:,:,:,:)
       real*8,allocatable::Z389(:,:,:,:,:,:)
       real*8,allocatable::X125(:,:,:,:)
       real*8,allocatable::Z399(:,:,:,:,:,:)
       real*8,allocatable::X126(:,:,:,:)
       real*8,allocatable::Z441(:,:,:,:,:,:)
       real*8,allocatable::X127(:,:,:,:)
       real*8,allocatable::Z447(:,:,:,:,:,:)
       real*8,allocatable::X128(:,:,:,:)
       real*8,allocatable::Z456(:,:,:,:,:,:)
       real*8,allocatable::X129(:,:,:,:)
       real*8,allocatable::Z462(:,:,:,:,:,:)
C
       allocate(V3D(M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2))
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
       allocate(X52(N0+1:M1,M1+1:N2))
       X52=0.0d0
       X52=X52+Q1
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
       allocate(X53(M1+1:N2,M1+1:N2))
       X53=0.0d0
       X53=X53+Q2
       deallocate(Q2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder4231(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,N0,M1,VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q3(N0+1:M1,N0+1:M1))
       I1=K5*K5
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q3)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X54(N0+1:M1,N0+1:M1))
       X54=0.0d0
       X54=X54+Q3
       deallocate(Q3)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N0+1:M1))
       call reorder4231(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,N0,M1,VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q4(M1+1:N2,N0+1:M1))
       I1=K5*K8
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q4)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X55(M1+1:N2,N0+1:M1))
       X55=0.0d0
       X55=X55+Q4
       deallocate(Q4)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M2+1:N3))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,M2,N3,M2,N3,VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q5(M2+1:N3,M2+1:N3))
       I1=K6*K6
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q5)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X56(M2+1:N3,M2+1:N3))
       X56=0.0d0
       X56=X56+Q5
       deallocate(Q5)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,M2+1:N3))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,M2,M2,N3,VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q6(N2+1:M2,M2+1:N3))
       I1=K6*K0
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q6)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X57(N2+1:M2,M2+1:N3))
       X57=0.0d0
       X57=X57+Q6
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
       allocate(X58(M2+1:N3,N2+1:M2))
       X58=0.0d0
       X58=X58+Q7
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
       allocate(X59(N2+1:M2,N2+1:M2))
       X59=0.0d0
       X59=X59+Q8
       deallocate(Q8)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,N2+1:M2))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,M2,N3,N2,M2,VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q9(M2+1:N3,N2+1:M2))
       I1=K0*K6
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q9)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X60(M2+1:N3,N2+1:M2))
       X60=0.0d0
       X60=X60+Q9
       deallocate(Q9)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,N2+1:M2))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,M2,N2,M2,VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q10(N2+1:M2,N2+1:M2))
       I1=K0*K0
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q10)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X61(N2+1:M2,N2+1:M2))
       X61=0.0d0
       X61=X61+Q10
       deallocate(Q10)
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
       allocate(X62(N0+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       X62=0.0d0
       call
     & sum2134(N0,N2,N2,M2,M1,N2,M1,N2,X62,S2, 1.000)
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
       allocate(X63(N0+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       X63=0.0d0
       call
     & sum2134(N0,N2,N2,M2,M1,N2,M1,N2,X63,S3, 1.000)
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
       allocate(X64(N0+1:N2,M2+1:N3,M1+1:N2,M1+1:N2))
       X64=0.0d0
       call
     & sum3124(N0,N2,M2,N3,M1,N2,M1,N2,X64,S4, 1.000)
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
       allocate(X65(N0+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       X65=0.0d0
       call
     & sum3124(N0,N2,N2,M2,M1,N2,M1,N2,X65,S5, 1.000)
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
       allocate(X66(N0+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       X66=0.0d0
       call
     & sum3124(N0,N2,N2,M2,M1,N2,M1,N2,X66,S6, 1.000)
       deallocate(S6)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,M1,M1,N2,VCHHHH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S7(M2+1:N3,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S7)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X3(N0+1:N2,M2+1:N3,N0+1:M1,M1+1:N2))
       X3=0.0d0
       call
     & sum2134(N0,N2,M2,N3,N0,M1,M1,N2,X3,S7,-1.000)
       deallocate(S7)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,M1,M1,N2,VCHHHH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S8(N2+1:M2,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S8)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X67(N0+1:N2,N2+1:M2,N0+1:M1,M1+1:N2))
       X67=0.0d0
       call
     & sum2134(N0,N2,N2,M2,N0,M1,M1,N2,X67,S8, 1.000)
       deallocate(S8)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder4312(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,M1,M1,N2,VCHHHH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S9(N2+1:M2,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S9)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X68(N0+1:N2,N2+1:M2,N0+1:M1,M1+1:N2))
       X68=0.0d0
       call
     & sum2134(N0,N2,N2,M2,N0,M1,M1,N2,X68,S9, 1.000)
       deallocate(S9)
C
       allocate(D1(N2+1:N3,N0+1:N2,M2+1:N3,M1+1:N2))
       call reorder1432(N2,N3,N0,N2,N2,N3,N0,N2,
     & N2,N3,N0,N2,M2,N3,M1,N2,VCHPHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S10(N0+1:M1,N0+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S10)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N2,M2,N3,N0,M1,M1,N2,X3,S10, 1.000)
       deallocate(S10)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:M2,M1+1:N2))
       call reorder1432(N2,N3,N0,N2,N2,N3,N0,N2,
     & N2,N3,N0,N2,N2,M2,M1,N2,VCHPHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S11(N0+1:M1,N0+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S11)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N2,N2,M2,N0,M1,M1,N2,X67,S11,-1.000)
       deallocate(S11)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:M2,M1+1:N2))
       call reorder1432(N2,N3,N0,N2,N2,N3,N0,N2,
     & N2,N3,N0,N2,N2,M2,M1,N2,VCHPHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S12(N0+1:M1,N0+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S12)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N2,N2,M2,N0,M1,M1,N2,X68,S12, 1.000)
       deallocate(S12)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder4132(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,VCHPHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S13(N2+1:M2,N2+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S13)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X69(N2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       X69=0.0d0
       call
     & sum3124(N2,N3,M2,N3,N2,M2,M1,N2,X69,S13, 1.000)
       deallocate(S13)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder4132(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,VCHPHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S14(N2+1:M2,N2+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S14)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X70(N2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       X70=0.0d0
       call
     & sum3124(N2,N3,M2,N3,N2,M2,M1,N2,X70,S14, 1.000)
       deallocate(S14)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder4132(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,VCHPHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S15(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S15)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,M1,N2,X69,S15,-1.000)
       deallocate(S15)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder4132(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,VCHPHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S16(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S16)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,M1,N2,X70,S16,-1.000)
       deallocate(S16)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder4132(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,VCHPHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S17(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S17)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X5(N2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       X5=0.0d0
       call
     & sum3124(N2,N3,N2,M2,N2,M2,M1,N2,X5,S17,-1.000)
       deallocate(S17)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder4132(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,VCHPHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S18(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S18)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,N2,M2,M1,N2,X5,S18, 1.000)
       deallocate(S18)
C
       allocate(D1(N2+1:N3,N0+1:N2,M2+1:N3,N0+1:M1))
       call reorder1432(N2,N3,N0,N2,N2,N3,N0,N2,
     & N2,N3,N0,N2,M2,N3,N0,M1,VCHPHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S19(M1+1:N2,N0+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S19)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,N2,M2,N3,N0,M1,M1,N2,X3,S19,-1.000)
       deallocate(S19)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:M2,N0+1:M1))
       call reorder1432(N2,N3,N0,N2,N2,N3,N0,N2,
     & N2,N3,N0,N2,N2,M2,N0,M1,VCHPHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S20(M1+1:N2,N0+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S20)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,N2,N2,M2,N0,M1,M1,N2,X67,S20, 1.000)
       deallocate(S20)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:M2,N0+1:M1))
       call reorder1432(N2,N3,N0,N2,N2,N3,N0,N2,
     & N2,N3,N0,N2,N2,M2,N0,M1,VCHPHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S21(M1+1:N2,N0+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S21)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,N2,N2,M2,N0,M1,M1,N2,X68,S21,-1.000)
       deallocate(S21)
C
       allocate(D1(N2+1:N3,N2+1:N3,M2+1:N3,N2+1:M2))
       call reorder1234(N2,N3,N2,N3,N2,N3,N2,M2,
     & N2,N3,N2,N3,M2,N3,N2,M2,VCAPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S22(M1+1:N2,N2+1:N3,M2+1:N3,N2+1:M2))
       I1=K0*K6*K4
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S22)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N2,N3,M2,N3,N2,M2,M1,N2,X69,S22, 1.000)
       deallocate(S22)
C
       allocate(D1(N2+1:N3,N2+1:N3,M2+1:N3,N2+1:M2))
       call reorder1234(N2,N3,N2,N3,N2,N3,N2,M2,
     & N2,N3,N2,N3,M2,N3,N2,M2,VCAPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S23(M1+1:N2,N2+1:N3,M2+1:N3,N2+1:M2))
       I1=K0*K6*K4
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S23)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N2,N3,M2,N3,N2,M2,M1,N2,X70,S23, 1.000)
       deallocate(S23)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:M2,N2+1:M2))
       call reorder1234(N2,N3,N2,N3,N2,N3,N2,M2,
     & N2,N3,N2,N3,N2,M2,N2,M2,VCAPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S24(M1+1:N2,N2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K4
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S24)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N2,N3,N2,M2,N2,M2,M1,N2,X5,S24,-1.000)
       deallocate(S24)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder4132(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,M2,N3,N0,M1,VCHPHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S25(N2+1:M2,N2+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S25)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X71(N2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1))
       X71=0.0d0
       call
     & sum3124(N2,N3,M2,N3,N2,M2,N0,M1,X71,S25, 1.000)
       deallocate(S25)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder4132(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,M2,N3,N0,M1,VCHPHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S26(N2+1:M2,N2+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S26)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X72(N2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1))
       X72=0.0d0
       call
     & sum3124(N2,N3,M2,N3,N2,M2,N0,M1,X72,S26, 1.000)
       deallocate(S26)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder4132(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,VCHPHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S27(M2+1:N3,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S27)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,N0,M1,X71,S27,-1.000)
       deallocate(S27)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder4132(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,VCHPHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S28(M2+1:N3,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S28)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,N0,M1,X72,S28,-1.000)
       deallocate(S28)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder4132(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,VCHPHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S29(N2+1:M2,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S29)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X7(N2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1))
       X7=0.0d0
       call
     & sum3124(N2,N3,N2,M2,N2,M2,N0,M1,X7,S29,-1.000)
       deallocate(S29)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder4132(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,VCHPHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S30(N2+1:M2,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S30)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,N2,M2,N0,M1,X7,S30, 1.000)
       deallocate(S30)
C
       allocate(D1(N2+1:N3,N2+1:N3,M2+1:N3,N2+1:M2))
       call reorder1234(N2,N3,N2,N3,N2,N3,N2,M2,
     & N2,N3,N2,N3,M2,N3,N2,M2,VCAPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S31(N0+1:M1,N2+1:N3,M2+1:N3,N2+1:M2))
       I1=K0*K6*K4
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S31)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N2,N3,M2,N3,N2,M2,N0,M1,X71,S31, 1.000)
       deallocate(S31)
C
       allocate(D1(N2+1:N3,N2+1:N3,M2+1:N3,N2+1:M2))
       call reorder1234(N2,N3,N2,N3,N2,N3,N2,M2,
     & N2,N3,N2,N3,M2,N3,N2,M2,VCAPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S32(N0+1:M1,N2+1:N3,M2+1:N3,N2+1:M2))
       I1=K0*K6*K4
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S32)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N2,N3,M2,N3,N2,M2,N0,M1,X72,S32, 1.000)
       deallocate(S32)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:M2,N2+1:M2))
       call reorder1234(N2,N3,N2,N3,N2,N3,N2,M2,
     & N2,N3,N2,N3,N2,M2,N2,M2,VCAPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S33(N0+1:M1,N2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K4
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S33)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N2,N3,N2,M2,N2,M2,N0,M1,X7,S33,-1.000)
       deallocate(S33)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,M1,M2,N3,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S34(M2+1:N3,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S34)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X8(N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N2))
       X8=0.0d0
       call
     & sum3124(N0,M1,M2,N3,M2,N3,M1,N2,X8,S34,-1.000)
       deallocate(S34)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,M2,N3,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S35(M2+1:N3,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S35)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X9(M1+1:N1,M2+1:N3,M2+1:N3,M1+1:N2))
       X9=0.0d0
       call
     & sum3124(M1,N1,M2,N3,M2,N3,M1,N2,X9,S35,-1.000)
       deallocate(S35)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,M1,N1,M2,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S36(M2+1:N3,N0+1:M1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S36)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X10(N0+1:M1,N1+1:M2,M2+1:N3,M1+1:N2))
       X10=0.0d0
       call
     & sum3124(N0,M1,N1,M2,M2,N3,M1,N2,X10,S36,-1.000)
       deallocate(S36)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,N1,M2,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S37(M2+1:N3,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S37)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X11(M1+1:N1,N1+1:M2,M2+1:N3,M1+1:N2))
       X11=0.0d0
       call
     & sum3124(M1,N1,N1,M2,M2,N3,M1,N2,X11,S37,-1.000)
       deallocate(S37)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,M1,M2,N3,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S38(N2+1:M2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S38)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X73(N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       X73=0.0d0
       call
     & sum3124(N0,M1,M2,N3,N2,M2,M1,N2,X73,S38, 1.000)
       deallocate(S38)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,M2,N3,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S39(N2+1:M2,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S39)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X74(M1+1:N1,M2+1:N3,N2+1:M2,M1+1:N2))
       X74=0.0d0
       call
     & sum3124(M1,N1,M2,N3,N2,M2,M1,N2,X74,S39, 1.000)
       deallocate(S39)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,M1,N1,M2,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S40(N2+1:M2,N0+1:M1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S40)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X75(N0+1:M1,N1+1:M2,N2+1:M2,M1+1:N2))
       X75=0.0d0
       call
     & sum3124(N0,M1,N1,M2,N2,M2,M1,N2,X75,S40, 1.000)
       deallocate(S40)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,N1,M2,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S41(N2+1:M2,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S41)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X76(M1+1:N1,N1+1:M2,N2+1:M2,M1+1:N2))
       X76=0.0d0
       call
     & sum3124(M1,N1,N1,M2,N2,M2,M1,N2,X76,S41, 1.000)
       deallocate(S41)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,M1,M2,N3,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S42(N2+1:M2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S42)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X77(N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       X77=0.0d0
       call
     & sum3124(N0,M1,M2,N3,N2,M2,M1,N2,X77,S42, 1.000)
       deallocate(S42)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,M2,N3,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S43(N2+1:M2,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S43)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X78(M1+1:N1,M2+1:N3,N2+1:M2,M1+1:N2))
       X78=0.0d0
       call
     & sum3124(M1,N1,M2,N3,N2,M2,M1,N2,X78,S43, 1.000)
       deallocate(S43)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,M1,N1,M2,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S44(N2+1:M2,N0+1:M1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S44)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X79(N0+1:M1,N1+1:M2,N2+1:M2,M1+1:N2))
       X79=0.0d0
       call
     & sum3124(N0,M1,N1,M2,N2,M2,M1,N2,X79,S44, 1.000)
       deallocate(S44)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,N1,M2,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S45(N2+1:M2,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S45)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X80(M1+1:N1,N1+1:M2,N2+1:M2,M1+1:N2))
       X80=0.0d0
       call
     & sum3124(M1,N1,N1,M2,N2,M2,M1,N2,X80,S45, 1.000)
       deallocate(S45)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,M2+1:N3))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,M1,M2,N3,M2,N3,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S46(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S46)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,M2,N3,M2,N3,M1,N2,X8,S46, 1.000)
       deallocate(S46)
C
       allocate(D1(N2+1:N3,M1+1:N1,M2+1:N3,M2+1:N3))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,M1,N1,M2,N3,M2,N3,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S47(M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K7
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S47)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,M2,N3,M2,N3,M1,N2,X9,S47, 1.000)
       deallocate(S47)
C
       allocate(D1(N2+1:N3,N0+1:M1,N1+1:M2,M2+1:N3))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,M1,N1,M2,M2,N3,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S48(M1+1:N2,N0+1:M1,N1+1:M2,M2+1:N3))
       I1=K6*K9*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S48)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,N1,M2,M2,N3,M1,N2,X10,S48, 1.000)
       deallocate(S48)
C
       allocate(D1(N2+1:N3,M1+1:N1,N1+1:M2,M2+1:N3))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,M1,N1,N1,M2,M2,N3,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S49(M1+1:N2,M1+1:N1,N1+1:M2,M2+1:N3))
       I1=K6*K9*K7
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S49)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,N1,M2,M2,N3,M1,N2,X11,S49, 1.000)
       deallocate(S49)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,M1,M2,N3,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S50(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S50)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,M2,N3,N2,M2,M1,N2,X73,S50,-1.000)
       deallocate(S50)
C
       allocate(D1(N2+1:N3,M1+1:N1,M2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,M1,N1,M2,N3,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S51(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K7
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S51)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,M2,N3,N2,M2,M1,N2,X74,S51,-1.000)
       deallocate(S51)
C
       allocate(D1(N2+1:N3,N0+1:M1,N1+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,M1,N1,M2,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S52(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:M2))
       I1=K0*K9*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S52)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,N1,M2,N2,M2,M1,N2,X75,S52,-1.000)
       deallocate(S52)
C
       allocate(D1(N2+1:N3,M1+1:N1,N1+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,M1,N1,N1,M2,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S53(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:M2))
       I1=K0*K9*K7
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S53)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,N1,M2,N2,M2,M1,N2,X76,S53,-1.000)
       deallocate(S53)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,M1,M2,N3,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S54(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S54)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,M2,N3,N2,M2,M1,N2,X77,S54,-1.000)
       deallocate(S54)
C
       allocate(D1(N2+1:N3,M1+1:N1,M2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,M1,N1,M2,N3,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S55(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K7
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S55)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,M2,N3,N2,M2,M1,N2,X78,S55,-1.000)
       deallocate(S55)
C
       allocate(D1(N2+1:N3,N0+1:M1,N1+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,M1,N1,M2,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S56(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:M2))
       I1=K0*K9*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S56)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,N1,M2,N2,M2,M1,N2,X79,S56,-1.000)
       deallocate(S56)
C
       allocate(D1(N2+1:N3,M1+1:N1,N1+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,M1,N1,N1,M2,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S57(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:M2))
       I1=K0*K9*K7
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S57)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,N1,M2,N2,M2,M1,N2,X80,S57,-1.000)
       deallocate(S57)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,M1,M2,N3,N0,M1,VBHHPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S58(M2+1:N3,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S58)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X16(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:M1))
       X16=0.0d0
       call
     & sum3124(N0,M1,M2,N3,M2,N3,N0,M1,X16,S58,-1.000)
       deallocate(S58)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,M2,N3,N0,M1,VBHHPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S59(M2+1:N3,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S59)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X17(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:M1))
       X17=0.0d0
       call
     & sum3124(M1,N1,M2,N3,M2,N3,N0,M1,X17,S59,-1.000)
       deallocate(S59)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,M1,N1,M2,N0,M1,VBHHPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S60(M2+1:N3,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S60)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X18(N0+1:M1,N1+1:M2,M2+1:N3,N0+1:M1))
       X18=0.0d0
       call
     & sum3124(N0,M1,N1,M2,M2,N3,N0,M1,X18,S60,-1.000)
       deallocate(S60)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,N1,M2,N0,M1,VBHHPH,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S61(M2+1:N3,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S61)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X19(M1+1:N1,N1+1:M2,M2+1:N3,N0+1:M1))
       X19=0.0d0
       call
     & sum3124(M1,N1,N1,M2,M2,N3,N0,M1,X19,S61,-1.000)
       deallocate(S61)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,M1,M2,N3,N0,M1,VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S62(N2+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S62)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X81(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       X81=0.0d0
       call
     & sum3124(N0,M1,M2,N3,N2,M2,N0,M1,X81,S62, 1.000)
       deallocate(S62)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,M2,N3,N0,M1,VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S63(N2+1:M2,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S63)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X82(M1+1:N1,M2+1:N3,N2+1:M2,N0+1:M1))
       X82=0.0d0
       call
     & sum3124(M1,N1,M2,N3,N2,M2,N0,M1,X82,S63, 1.000)
       deallocate(S63)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,M1,N1,M2,N0,M1,VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S64(N2+1:M2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S64)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X83(N0+1:M1,N1+1:M2,N2+1:M2,N0+1:M1))
       X83=0.0d0
       call
     & sum3124(N0,M1,N1,M2,N2,M2,N0,M1,X83,S64, 1.000)
       deallocate(S64)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,N1,M2,N0,M1,VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S65(N2+1:M2,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S65)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X84(M1+1:N1,N1+1:M2,N2+1:M2,N0+1:M1))
       X84=0.0d0
       call
     & sum3124(M1,N1,N1,M2,N2,M2,N0,M1,X84,S65, 1.000)
       deallocate(S65)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,M1,M2,N3,N0,M1,VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S66(N2+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S66)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X85(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       X85=0.0d0
       call
     & sum3124(N0,M1,M2,N3,N2,M2,N0,M1,X85,S66, 1.000)
       deallocate(S66)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,M2,N3,N0,M1,VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S67(N2+1:M2,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S67)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X86(M1+1:N1,M2+1:N3,N2+1:M2,N0+1:M1))
       X86=0.0d0
       call
     & sum3124(M1,N1,M2,N3,N2,M2,N0,M1,X86,S67, 1.000)
       deallocate(S67)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,M1,N1,M2,N0,M1,VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S68(N2+1:M2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S68)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X87(N0+1:M1,N1+1:M2,N2+1:M2,N0+1:M1))
       X87=0.0d0
       call
     & sum3124(N0,M1,N1,M2,N2,M2,N0,M1,X87,S68, 1.000)
       deallocate(S68)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,N1,M2,N0,M1,VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S69(N2+1:M2,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S69)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X88(M1+1:N1,N1+1:M2,N2+1:M2,N0+1:M1))
       X88=0.0d0
       call
     & sum3124(M1,N1,N1,M2,N2,M2,N0,M1,X88,S69, 1.000)
       deallocate(S69)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,M2+1:N3))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,M1,M2,N3,M2,N3,VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S70(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S70)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,M2,N3,M2,N3,N0,M1,X16,S70, 1.000)
       deallocate(S70)
C
       allocate(D1(N2+1:N3,M1+1:N1,M2+1:N3,M2+1:N3))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,M1,N1,M2,N3,M2,N3,VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S71(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K7
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S71)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,M2,N3,M2,N3,N0,M1,X17,S71, 1.000)
       deallocate(S71)
C
       allocate(D1(N2+1:N3,N0+1:M1,N1+1:M2,M2+1:N3))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,M1,N1,M2,M2,N3,VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S72(N0+1:M1,N0+1:M1,N1+1:M2,M2+1:N3))
       I1=K6*K9*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S72)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,N1,M2,M2,N3,N0,M1,X18,S72, 1.000)
       deallocate(S72)
C
       allocate(D1(N2+1:N3,M1+1:N1,N1+1:M2,M2+1:N3))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,M1,N1,N1,M2,M2,N3,VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S73(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3))
       I1=K6*K9*K7
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S73)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,N1,M2,M2,N3,N0,M1,X19,S73, 1.000)
       deallocate(S73)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,M1,M2,N3,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S74(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S74)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,M2,N3,N2,M2,N0,M1,X81,S74,-1.000)
       deallocate(S74)
C
       allocate(D1(N2+1:N3,M1+1:N1,M2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,M1,N1,M2,N3,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S75(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K7
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S75)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,M2,N3,N2,M2,N0,M1,X82,S75,-1.000)
       deallocate(S75)
C
       allocate(D1(N2+1:N3,N0+1:M1,N1+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,M1,N1,M2,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S76(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:M2))
       I1=K0*K9*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S76)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,N1,M2,N2,M2,N0,M1,X83,S76,-1.000)
       deallocate(S76)
C
       allocate(D1(N2+1:N3,M1+1:N1,N1+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,M1,N1,N1,M2,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S77(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:M2))
       I1=K0*K9*K7
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S77)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,N1,M2,N2,M2,N0,M1,X84,S77,-1.000)
       deallocate(S77)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,M1,M2,N3,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S78(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S78)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,M2,N3,N2,M2,N0,M1,X85,S78,-1.000)
       deallocate(S78)
C
       allocate(D1(N2+1:N3,M1+1:N1,M2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,M1,N1,M2,N3,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S79(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K7
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S79)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,M2,N3,N2,M2,N0,M1,X86,S79,-1.000)
       deallocate(S79)
C
       allocate(D1(N2+1:N3,N0+1:M1,N1+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,M1,N1,M2,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S80(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:M2))
       I1=K0*K9*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S80)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,N1,M2,N2,M2,N0,M1,X87,S80,-1.000)
       deallocate(S80)
C
       allocate(D1(N2+1:N3,M1+1:N1,N1+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,M1,N1,N1,M2,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S81(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:M2))
       I1=K0*K9*K7
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S81)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,N1,M2,N2,M2,N0,M1,X88,S81,-1.000)
       deallocate(S81)
C
       allocate(B1(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,FBHP,B1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(Q11(M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q11)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X24(N0+1:M1,M1+1:N2))
       X24=0.0d0
       call
     & sum21(N0,M1,M1,N2,X24,Q11, 1.000)
       deallocate(Q11)
C
       allocate(B1(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,FBHP,B1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(Q12(M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q12)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X25(M1+1:N2,M1+1:N2))
       X25=0.0d0
       call
     & sum21(M1,N2,M1,N2,X25,Q12, 1.000)
       deallocate(Q12)
C
       allocate(B1(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,FBHP,B1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(Q13(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q13)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X26(N0+1:M1,N0+1:M1))
       X26=0.0d0
       call
     & sum21(N0,M1,N0,M1,X26,Q13, 1.000)
       deallocate(Q13)
C
       allocate(B1(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,FBHP,B1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(Q14(N0+1:M1,M1+1:N2))
       I1=K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q14)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X27(M1+1:N2,N0+1:M1))
       X27=0.0d0
       call
     & sum21(M1,N2,N0,M1,X27,Q14, 1.000)
       deallocate(Q14)
C
       allocate(B1(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,FBHP,B1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(Q15(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,B1,B2,Q15)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X28(M2+1:N3,M2+1:N3))
       X28=0.0d0
       call
     & sum21(M2,N3,M2,N3,X28,Q15,-1.000)
       deallocate(Q15)
C
       allocate(B1(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,FBHP,B1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(Q16(M2+1:N3,N2+1:M2))
       I1=K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,B1,B2,Q16)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X29(N2+1:M2,M2+1:N3))
       X29=0.0d0
       call
     & sum21(N2,M2,M2,N3,X29,Q16,-1.000)
       deallocate(Q16)
C
       allocate(B1(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,FBHP,B1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(Q17(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,B1,B2,Q17)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X30(M2+1:N3,N2+1:M2))
       X30=0.0d0
       call
     & sum21(M2,N3,N2,M2,X30,Q17,-1.000)
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
       call EGEMM(I1,I2,I3,B1,B2,Q18)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X31(N2+1:M2,N2+1:M2))
       X31=0.0d0
       call
     & sum21(N2,M2,N2,M2,X31,Q18,-1.000)
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
       allocate(X32(M2+1:N3,N2+1:M2))
       X32=0.0d0
       call
     & sum21(M2,N3,N2,M2,X32,Q19,-1.000)
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
       allocate(X33(N2+1:M2,N2+1:M2))
       X33=0.0d0
       call
     & sum21(N2,M2,N2,M2,X33,Q20,-1.000)
       deallocate(Q20)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder1342(N2,N3,N0,N2,N0,N2,N0,N2,
     & N2,N3,N0,M1,M1,N2,M1,N2,VCHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S82(M1+1:N2,N0+1:M1,M1+1:N2,M1+1:N2))
       I1=K8*K8*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S82)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder2314(M1,N2,N0,M1,M1,N2,M1,N2,
     & N0,M1,M1,N2,M1,N2,M1,N2,S82,D1)
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M1,N2,M2,N3,N2,M2,N2,M2,N0,M1,t3D,F2)
       allocate(Z162(M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2))
       I1=K8*K8
       I2=K5*K0*K0*K6
       I3=K8*K5
       call EGEMM(I1,I2,I3,D1,F2,Z162)
       deallocate(D1)
       deallocate(F2)
C
       V3D=V3D-Z162
       call
     & sum123465(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z162, 1.000)
       deallocate(Z162)
       deallocate(S82)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder1342(N2,N3,N0,N2,N0,N2,N0,N2,
     & N2,N3,M1,N2,M1,N2,M1,N2,VCHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S83(M1+1:N2,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S83)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder2314(M1,N2,M1,N2,M1,N2,M1,N2,
     & M1,N2,M1,N2,M1,N2,M1,N2,S83,D1)
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder561234(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M1,N2,M2,N3,N2,M2,N2,M2,N0,M1,t3D,F2)
       allocate(Z163(M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2))
       I1=K8*K8
       I2=K5*K0*K0*K6
       I3=K8*K8
       call EGEMM(I1,I2,I3,D1,F2,Z163)
       deallocate(D1)
       deallocate(F2)
C
       V3D=V3D+0.500*Z163
       call
     & sum123465(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z163,-0.500)
       deallocate(Z163)
       deallocate(S83)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:M1,M1+1:N2))
       call reorder1342(N2,N3,N0,N2,N0,N2,N0,N2,
     & N2,N3,N0,M1,N0,M1,M1,N2,VCHHHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S84(N0+1:M1,N0+1:M1,N0+1:M1,M1+1:N2))
       I1=K8*K5*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S84)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X36(N0+1:M1,N0+1:M1,N0+1:M1,M1+1:N2))
       X36=0.0d0
       call
     & sum3124(N0,M1,N0,M1,N0,M1,M1,N2,X36,S84, 1.000)
       deallocate(S84)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder1342(N2,N3,N0,N2,N0,N2,N0,N2,
     & N2,N3,N0,M1,M1,N2,M1,N2,VCHHHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S85(N0+1:M1,N0+1:M1,M1+1:N2,M1+1:N2))
       I1=K8*K8*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S85)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X37(N0+1:M1,M1+1:N2,N0+1:M1,M1+1:N2))
       X37=0.0d0
       call
     & sum3124(N0,M1,M1,N2,N0,M1,M1,N2,X37,S85, 1.000)
       deallocate(S85)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder1342(N2,N3,N0,N2,N0,N2,N0,N2,
     & N2,N3,M1,N2,M1,N2,M1,N2,VCHHHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S86(N0+1:M1,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S86)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X38(M1+1:N2,M1+1:N2,N0+1:M1,M1+1:N2))
       X38=0.0d0
       call
     & sum3124(M1,N2,M1,N2,N0,M1,M1,N2,X38,S86, 1.000)
       deallocate(S86)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,M1,M2,N3,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S87(M2+1:N3,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S87)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X39(N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N2))
       X39=0.0d0
       call
     & sum3124(N0,M1,M2,N3,M2,N3,M1,N2,X39,S87,-1.000)
       deallocate(S87)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,M1,N2,M2,N3,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S88(M2+1:N3,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S88)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X40(M1+1:N2,M2+1:N3,M2+1:N3,M1+1:N2))
       X40=0.0d0
       call
     & sum3124(M1,N2,M2,N3,M2,N3,M1,N2,X40,S88,-1.000)
       deallocate(S88)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,M1+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,M1,N2,M2,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S89(M2+1:N3,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S89)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X41(N0+1:M1,N2+1:M2,M2+1:N3,M1+1:N2))
       X41=0.0d0
       call
     & sum3124(N0,M1,N2,M2,M2,N3,M1,N2,X41,S89,-1.000)
       deallocate(S89)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,M1,N2,N2,M2,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S90(M2+1:N3,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K8
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S90)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X42(M1+1:N2,N2+1:M2,M2+1:N3,M1+1:N2))
       X42=0.0d0
       call
     & sum3124(M1,N2,N2,M2,M2,N3,M1,N2,X42,S90,-1.000)
       deallocate(S90)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,M1,M2,N3,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S91(N2+1:M2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S91)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X89(N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       X89=0.0d0
       call
     & sum3124(N0,M1,M2,N3,N2,M2,M1,N2,X89,S91, 1.000)
       deallocate(S91)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,M1,N2,M2,N3,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S92(N2+1:M2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S92)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X90(M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       X90=0.0d0
       call
     & sum3124(M1,N2,M2,N3,N2,M2,M1,N2,X90,S92, 1.000)
       deallocate(S92)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,M1+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,M1,N2,M2,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S93(N2+1:M2,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S93)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X91(N0+1:M1,N2+1:M2,N2+1:M2,M1+1:N2))
       X91=0.0d0
       call
     & sum3124(N0,M1,N2,M2,N2,M2,M1,N2,X91,S93, 1.000)
       deallocate(S93)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,M1,N2,N2,M2,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S94(N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S94)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X92(M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       X92=0.0d0
       call
     & sum3124(M1,N2,N2,M2,N2,M2,M1,N2,X92,S94, 1.000)
       deallocate(S94)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder4312(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,M1,M2,N3,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S95(N2+1:M2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S95)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X93(N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       X93=0.0d0
       call
     & sum3124(N0,M1,M2,N3,N2,M2,M1,N2,X93,S95, 1.000)
       deallocate(S95)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder4312(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,M1,N2,M2,N3,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S96(N2+1:M2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S96)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X94(M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       X94=0.0d0
       call
     & sum3124(M1,N2,M2,N3,N2,M2,M1,N2,X94,S96, 1.000)
       deallocate(S96)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,M1+1:N2))
       call reorder4312(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,M1,N2,M2,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S97(N2+1:M2,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S97)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X95(N0+1:M1,N2+1:M2,N2+1:M2,M1+1:N2))
       X95=0.0d0
       call
     & sum3124(N0,M1,N2,M2,N2,M2,M1,N2,X95,S97, 1.000)
       deallocate(S97)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       call reorder4312(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,M1,N2,N2,M2,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S98(N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S98)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X96(M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       X96=0.0d0
       call
     & sum3124(M1,N2,N2,M2,N2,M2,M1,N2,X96,S98, 1.000)
       deallocate(S98)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M1+1:N2))
       call reorder3142(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q21(N0+1:M1,M1+1:N2))
       I1=K8*K5
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q21)
       deallocate(D1)
       deallocate(B2)
C
       X24=X24+Q21
       deallocate(Q21)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder3142(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q22(M1+1:N2,M1+1:N2))
       I1=K8*K8
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q22)
       deallocate(D1)
       deallocate(B2)
C
       X25=X25+Q22
       deallocate(Q22)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:M1,N0+1:M1))
       call reorder1342(N2,N3,N0,N2,N0,N2,N0,N2,
     & N2,N3,N0,M1,N0,M1,N0,M1,VCHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S99(M1+1:N2,N0+1:M1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S99)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,N0,M1,N0,M1,M1,N2,X36,S99,-1.000)
       deallocate(S99)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder1342(N2,N3,N0,N2,N0,N2,N0,N2,
     & N2,N3,N0,M1,M1,N2,N0,M1,VCHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S100(M1+1:N2,N0+1:M1,M1+1:N2,N0+1:M1))
       I1=K5*K8*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S100)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,M1,N2,N0,M1,M1,N2,X37,S100,-1.000)
       deallocate(S100)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,N0+1:M1))
       call reorder1342(N2,N3,N0,N2,N0,N2,N0,N2,
     & N2,N3,M1,N2,M1,N2,N0,M1,VCHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S101(M1+1:N2,M1+1:N2,M1+1:N2,N0+1:M1))
       I1=K5*K8*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S101)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N2,M1,N2,N0,M1,M1,N2,X38,S101,-1.000)
       deallocate(S101)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,M2+1:N3))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,M1,M2,N3,M2,N3,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S102(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S102)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,M2,N3,M2,N3,M1,N2,X39,S102,-1.000)
       deallocate(S102)
C
       allocate(D1(N2+1:N3,M1+1:N2,M2+1:N3,M2+1:N3))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,M1,N2,M2,N3,M2,N3,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S103(M1+1:N2,M1+1:N2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S103)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N2,M2,N3,M2,N3,M1,N2,X40,S103,-1.000)
       deallocate(S103)
C
       allocate(D1(N2+1:N3,N0+1:M1,N2+1:M2,M2+1:N3))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,M1,N2,M2,M2,N3,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S104(M1+1:N2,N0+1:M1,N2+1:M2,M2+1:N3))
       I1=K6*K0*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S104)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,N2,M2,M2,N3,M1,N2,X41,S104,-1.000)
       deallocate(S104)
C
       allocate(D1(N2+1:N3,M1+1:N2,N2+1:M2,M2+1:N3))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,M1,N2,N2,M2,M2,N3,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S105(M1+1:N2,M1+1:N2,N2+1:M2,M2+1:N3))
       I1=K6*K0*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S105)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N2,N2,M2,M2,N3,M1,N2,X42,S105,-1.000)
       deallocate(S105)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,M1,M2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S106(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S106)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,M2,N3,N2,M2,M1,N2,X89,S106, 1.000)
       deallocate(S106)
C
       allocate(D1(N2+1:N3,M1+1:N2,M2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,M1,N2,M2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S107(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S107)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N2,M2,N3,N2,M2,M1,N2,X90,S107, 1.000)
       deallocate(S107)
C
       allocate(D1(N2+1:N3,N0+1:M1,N2+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,M1,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S108(M1+1:N2,N0+1:M1,N2+1:M2,N2+1:M2))
       I1=K0*K0*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S108)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,N2,M2,N2,M2,M1,N2,X91,S108, 1.000)
       deallocate(S108)
C
       allocate(D1(N2+1:N3,M1+1:N2,N2+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,M1,N2,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S109(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S109)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N2,N2,M2,N2,M2,M1,N2,X92,S109, 1.000)
       deallocate(S109)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,M1,M2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S110(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S110)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,M1,M2,N3,N2,M2,
     & N0,M1,M2,N3,N2,M2,M1,N2,S110,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z192(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z192)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z192, 1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z192,-1.000)
       deallocate(Z192)
       deallocate(S110)
C
       allocate(D1(N2+1:N3,M1+1:N2,M2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,M1,N2,M2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S111(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S111)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder2341(M1,N2,M1,N2,M2,N3,N2,M2,
     & M1,N2,M2,N3,N2,M2,M1,N2,S111,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder512346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z193(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z193)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z193,-1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z193, 1.000)
       deallocate(Z193)
       deallocate(S111)
C
       allocate(D1(N2+1:N3,N0+1:M1,N2+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,M1,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S112(M1+1:N2,N0+1:M1,N2+1:M2,N2+1:M2))
       I1=K0*K0*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S112)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,M1,N2,M2,N2,M2,
     & N0,M1,N2,M2,N2,M2,M1,N2,S112,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,M2,N3,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z194(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z194)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z194,-1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z194, 1.000)
       deallocate(Z194)
       deallocate(S112)
C
       allocate(D1(N2+1:N3,M1+1:N2,N2+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,M1,N2,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S113(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S113)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder2341(M1,N2,M1,N2,N2,M2,N2,M2,
     & M1,N2,N2,M2,N2,M2,M1,N2,S113,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder521346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,M2,N3,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z195(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z195)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z195, 1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z195,-1.000)
       deallocate(Z195)
       deallocate(S113)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,M1,M2,N3,N0,M1,VCHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S114(M2+1:N3,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S114)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X43(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:M1))
       X43=0.0d0
       call
     & sum3124(N0,M1,M2,N3,M2,N3,N0,M1,X43,S114,-1.000)
       deallocate(S114)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,M1,N2,M2,N3,N0,M1,VCHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S115(M2+1:N3,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S115)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X44(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:M1))
       X44=0.0d0
       call
     & sum3124(M1,N2,M2,N3,M2,N3,N0,M1,X44,S115,-1.000)
       deallocate(S115)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,M1,N2,M2,N0,M1,VCHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S116(M2+1:N3,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S116)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X45(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:M1))
       X45=0.0d0
       call
     & sum3124(N0,M1,N2,M2,M2,N3,N0,M1,X45,S116,-1.000)
       deallocate(S116)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,M1,N2,N2,M2,N0,M1,VCHHHP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S117(M2+1:N3,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S117)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X46(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:M1))
       X46=0.0d0
       call
     & sum3124(M1,N2,N2,M2,M2,N3,N0,M1,X46,S117,-1.000)
       deallocate(S117)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,M1,M2,N3,N0,M1,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S118(N2+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S118)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X97(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       X97=0.0d0
       call
     & sum3124(N0,M1,M2,N3,N2,M2,N0,M1,X97,S118, 1.000)
       deallocate(S118)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,M1,N2,M2,N3,N0,M1,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S119(N2+1:M2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S119)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X98(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:M1))
       X98=0.0d0
       call
     & sum3124(M1,N2,M2,N3,N2,M2,N0,M1,X98,S119, 1.000)
       deallocate(S119)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,M1,N2,M2,N0,M1,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S120(N2+1:M2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S120)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X99(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:M1))
       X99=0.0d0
       call
     & sum3124(N0,M1,N2,M2,N2,M2,N0,M1,X99,S120, 1.000)
       deallocate(S120)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,M1,N2,N2,M2,N0,M1,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S121(N2+1:M2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S121)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X100(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:M1))
       X100=0.0d0
       call
     & sum3124(M1,N2,N2,M2,N2,M2,N0,M1,X100,S121, 1.000)
       deallocate(S121)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder4312(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,M1,M2,N3,N0,M1,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S122(N2+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S122)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X101(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       X101=0.0d0
       call
     & sum3124(N0,M1,M2,N3,N2,M2,N0,M1,X101,S122, 1.000)
       deallocate(S122)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder4312(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,M1,N2,M2,N3,N0,M1,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S123(N2+1:M2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S123)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X102(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:M1))
       X102=0.0d0
       call
     & sum3124(M1,N2,M2,N3,N2,M2,N0,M1,X102,S123, 1.000)
       deallocate(S123)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder4312(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,M1,N2,M2,N0,M1,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S124(N2+1:M2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S124)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X103(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:M1))
       X103=0.0d0
       call
     & sum3124(N0,M1,N2,M2,N2,M2,N0,M1,X103,S124, 1.000)
       deallocate(S124)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder4312(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,M1,N2,N2,M2,N0,M1,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S125(N2+1:M2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S125)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X104(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:M1))
       X104=0.0d0
       call
     & sum3124(M1,N2,N2,M2,N2,M2,N0,M1,X104,S125, 1.000)
       deallocate(S125)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N0+1:M1))
       call reorder3142(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,N0,M1,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q23(N0+1:M1,N0+1:M1))
       I1=K5*K5
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q23)
       deallocate(D1)
       deallocate(B2)
C
       X26=X26+Q23
       deallocate(Q23)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N0+1:M1))
       call reorder3142(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,N0,M1,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q24(M1+1:N2,N0+1:M1))
       I1=K5*K8
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q24)
       deallocate(D1)
       deallocate(B2)
C
       X27=X27+Q24
       deallocate(Q24)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,M2+1:N3))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,M1,M2,N3,M2,N3,VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S126(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S126)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,M2,N3,M2,N3,N0,M1,X43,S126,-1.000)
       deallocate(S126)
C
       allocate(D1(N2+1:N3,M1+1:N2,M2+1:N3,M2+1:N3))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,M1,N2,M2,N3,M2,N3,VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S127(N0+1:M1,M1+1:N2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S127)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N2,M2,N3,M2,N3,N0,M1,X44,S127,-1.000)
       deallocate(S127)
C
       allocate(D1(N2+1:N3,N0+1:M1,N2+1:M2,M2+1:N3))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,M1,N2,M2,M2,N3,VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S128(N0+1:M1,N0+1:M1,N2+1:M2,M2+1:N3))
       I1=K6*K0*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S128)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,N2,M2,M2,N3,N0,M1,X45,S128,-1.000)
       deallocate(S128)
C
       allocate(D1(N2+1:N3,M1+1:N2,N2+1:M2,M2+1:N3))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,M1,N2,N2,M2,M2,N3,VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S129(N0+1:M1,M1+1:N2,N2+1:M2,M2+1:N3))
       I1=K6*K0*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S129)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N2,N2,M2,M2,N3,N0,M1,X46,S129,-1.000)
       deallocate(S129)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,M1,M2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S130(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S130)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,M2,N3,N2,M2,N0,M1,X97,S130, 1.000)
       deallocate(S130)
C
       allocate(D1(N2+1:N3,M1+1:N2,M2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,M1,N2,M2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S131(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S131)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N2,M2,N3,N2,M2,N0,M1,X98,S131, 1.000)
       deallocate(S131)
C
       allocate(D1(N2+1:N3,N0+1:M1,N2+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,M1,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S132(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:M2))
       I1=K0*K0*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S132)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,N2,M2,N2,M2,N0,M1,X99,S132, 1.000)
       deallocate(S132)
C
       allocate(D1(N2+1:N3,M1+1:N2,N2+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,M1,N2,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S133(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S133)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N2,N2,M2,N2,M2,N0,M1,X100,S133, 1.000)
       deallocate(S133)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,M1,M2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S134(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S134)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,M1,M2,N3,N2,M2,
     & N0,M1,M2,N3,N2,M2,N0,M1,S134,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z218(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z218)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z218, 1.000)
       deallocate(Z218)
       deallocate(S134)
C
       allocate(D1(N2+1:N3,M1+1:N2,M2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,M1,N2,M2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S135(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S135)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,M1,N2,M2,N3,N2,M2,
     & M1,N2,M2,N3,N2,M2,N0,M1,S135,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z219(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z219)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z219, 1.000)
       deallocate(Z219)
       deallocate(S135)
C
       allocate(D1(N2+1:N3,N0+1:M1,N2+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,M1,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S136(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:M2))
       I1=K0*K0*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S136)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,M1,N2,M2,N2,M2,
     & N0,M1,N2,M2,N2,M2,N0,M1,S136,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z220(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z220)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z220,-1.000)
       deallocate(Z220)
       deallocate(S136)
C
       allocate(D1(N2+1:N3,M1+1:N2,N2+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,M1,N2,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S137(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S137)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,M1,N2,N2,M2,N2,M2,
     & M1,N2,N2,M2,N2,M2,N0,M1,S137,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z221(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z221)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z221,-1.000)
       deallocate(Z221)
       deallocate(S137)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,M2+1:N3))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,M2,N3,M2,N3,M2,N3,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S138(N2+1:M2,M2+1:N3,M2+1:N3,M2+1:N3))
       I1=K6*K6*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S138)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X105(M2+1:N3,M2+1:N3,M2+1:N3,N2+1:M2))
       X105=0.0d0
       call
     & sum4123(M2,N3,M2,N3,M2,N3,N2,M2,X105,S138, 1.000)
       deallocate(S138)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,M2+1:N3))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,M2,N3,N2,M2,M2,N3,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S139(N2+1:M2,M2+1:N3,N2+1:M2,M2+1:N3))
       I1=K6*K0*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S139)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X106(M2+1:N3,N2+1:M2,M2+1:N3,N2+1:M2))
       X106=0.0d0
       call
     & sum4123(M2,N3,N2,M2,M2,N3,N2,M2,X106,S139, 1.000)
       deallocate(S139)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,M2+1:N3))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,M2,N2,M2,M2,N3,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S140(N2+1:M2,N2+1:M2,N2+1:M2,M2+1:N3))
       I1=K6*K0*K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S140)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X107(N2+1:M2,N2+1:M2,M2+1:N3,N2+1:M2))
       X107=0.0d0
       call
     & sum4123(N2,M2,N2,M2,M2,N3,N2,M2,X107,S140, 1.000)
       deallocate(S140)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,M2+1:N3))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,M2,N3,M2,N3,M2,N3,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S141(N2+1:M2,M2+1:N3,M2+1:N3,M2+1:N3))
       I1=K6*K6*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S141)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X108(M2+1:N3,M2+1:N3,M2+1:N3,N2+1:M2))
       X108=0.0d0
       call
     & sum4123(M2,N3,M2,N3,M2,N3,N2,M2,X108,S141, 1.000)
       deallocate(S141)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,M2+1:N3))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,M2,N3,N2,M2,M2,N3,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S142(N2+1:M2,M2+1:N3,N2+1:M2,M2+1:N3))
       I1=K6*K0*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S142)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X109(M2+1:N3,N2+1:M2,M2+1:N3,N2+1:M2))
       X109=0.0d0
       call
     & sum4123(M2,N3,N2,M2,M2,N3,N2,M2,X109,S142, 1.000)
       deallocate(S142)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,M2+1:N3))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,M2,N2,M2,M2,N3,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S143(N2+1:M2,N2+1:M2,N2+1:M2,M2+1:N3))
       I1=K6*K0*K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S143)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X110(N2+1:M2,N2+1:M2,M2+1:N3,N2+1:M2))
       X110=0.0d0
       call
     & sum4123(N2,M2,N2,M2,M2,N3,N2,M2,X110,S143, 1.000)
       deallocate(S143)
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
       call EGEMM1(I1,I3,D1,B2,Q25)
       deallocate(D1)
       deallocate(B2)
C
       X28=X28-Q25
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
       call EGEMM1(I1,I3,D1,B2,Q26)
       deallocate(D1)
       deallocate(B2)
C
       X29=X29-Q26
       deallocate(Q26)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,M2,N3,M2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S144(M2+1:N3,M2+1:N3,M2+1:N3,N2+1:M2))
       I1=K0*K6*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S144)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,M2,N3,M2,N3,N2,M2,X105,S144,-1.000)
       deallocate(S144)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,M2,N3,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S145(M2+1:N3,M2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S145)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,N2,M2,M2,N3,N2,M2,X106,S145,-1.000)
       deallocate(S145)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,M2,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S146(M2+1:N3,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S146)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,M2,N2,M2,M2,N3,N2,M2,X107,S146,-1.000)
       deallocate(S146)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,M2,N3,M2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S147(M2+1:N3,M2+1:N3,M2+1:N3,N2+1:M2))
       I1=K0*K6*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S147)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,M2,N3,M2,N3,N2,M2,X108,S147,-1.000)
       deallocate(S147)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,M2,N3,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S148(M2+1:N3,M2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S148)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,N2,M2,M2,N3,N2,M2,X109,S148,-1.000)
       deallocate(S148)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,M2,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S149(M2+1:N3,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S149)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,M2,N2,M2,M2,N3,N2,M2,X110,S149,-1.000)
       deallocate(S149)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,M2,N3,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S150(N2+1:M2,M2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S150)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X50(M2+1:N3,N2+1:M2,N2+1:M2,N2+1:M2))
       X50=0.0d0
       call
     & sum4123(M2,N3,N2,M2,N2,M2,N2,M2,X50,S150,-1.000)
       deallocate(S150)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,M2,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S151(N2+1:M2,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S151)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X51(N2+1:M2,N2+1:M2,N2+1:M2,N2+1:M2))
       X51=0.0d0
       call
     & sum4123(N2,M2,N2,M2,N2,M2,N2,M2,X51,S151,-1.000)
       deallocate(S151)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,M2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q27(M2+1:N3,N2+1:M2))
       I1=K0*K6
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q27)
       deallocate(D1)
       deallocate(B2)
C
       X30=X30-Q27
       deallocate(Q27)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q28(N2+1:M2,N2+1:M2))
       I1=K0*K0
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q28)
       deallocate(D1)
       deallocate(B2)
C
       X31=X31-Q28
       deallocate(Q28)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,M2,N3,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S152(N2+1:M2,M2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S152)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,N2,M2,N2,M2,N2,M2,X50,S152, 1.000)
       deallocate(S152)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,M2,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S153(N2+1:M2,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S153)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,M2,N2,M2,N2,M2,N2,M2,X51,S153, 1.000)
       deallocate(S153)
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
       X32=X32-Q29
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
       X33=X33-Q30
       deallocate(Q30)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder4231(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,M1,N2,VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S154(M2+1:N3,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S154)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,M2,N3,M1,N2,M1,N2,X64,S154, 1.000)
       deallocate(S154)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder4231(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,M1,N2,VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S155(N2+1:M2,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S155)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,N2,M2,M1,N2,M1,N2,X65,S155, 1.000)
       deallocate(S155)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder4231(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,M1,N2,VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S156(N2+1:M2,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S156)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,N2,M2,M1,N2,M1,N2,X66,S156, 1.000)
       deallocate(S156)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder4231(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,M1,N2,VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S157(M2+1:N3,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S157)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,M2,N3,N0,M1,M1,N2,X3,S157, 1.000)
       deallocate(S157)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder4231(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,M1,N2,VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S158(N2+1:M2,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S158)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,N2,M2,N0,M1,M1,N2,X67,S158,-1.000)
       deallocate(S158)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder4231(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,M1,N2,VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S159(N2+1:M2,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S159)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,N2,M2,N0,M1,M1,N2,X68,S159, 1.000)
       deallocate(S159)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:M1))
       call reorder4231(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N0,M1,VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S160(M2+1:N3,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S160)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N2,M2,N3,N0,M1,M1,N2,X3,S160,-1.000)
       deallocate(S160)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:M1))
       call reorder4231(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N0,M1,VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S161(N2+1:M2,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S161)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N2,N2,M2,N0,M1,M1,N2,X67,S161, 1.000)
       deallocate(S161)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:M1))
       call reorder4231(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N0,M1,VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S162(N2+1:M2,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S162)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N2,N2,M2,N0,M1,M1,N2,X68,S162,-1.000)
       deallocate(S162)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,M2+1:N3))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,N3,M2,N3,VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S163(N2+1:M2,M1+1:N2,N2+1:N3,M2+1:N3))
       I1=K6*K4
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S163)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,N3,M2,N3,N2,M2,M1,N2,X69,S163,-1.000)
       deallocate(S163)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,M2+1:N3))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,N3,M2,N3,VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S164(N2+1:M2,M1+1:N2,N2+1:N3,M2+1:N3))
       I1=K6*K4
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S164)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,N3,M2,N3,N2,M2,M1,N2,X70,S164,-1.000)
       deallocate(S164)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,N3,N2,M2,VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S165(M2+1:N3,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S165)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,M2,N3,N2,M2,M1,N2,X69,S165, 1.000)
       deallocate(S165)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,N3,N2,M2,VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S166(M2+1:N3,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S166)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,M2,N3,N2,M2,M1,N2,X70,S166, 1.000)
       deallocate(S166)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,N3,N2,M2,VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S167(N2+1:M2,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S167)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,N3,N2,M2,N2,M2,M1,N2,X5,S167, 1.000)
       deallocate(S167)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,N3,N2,M2,VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S168(N2+1:M2,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S168)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,N2,M2,N2,M2,M1,N2,X5,S168,-1.000)
       deallocate(S168)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,M2+1:N3))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,N3,M2,N3,VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S169(N2+1:M2,N0+1:M1,N2+1:N3,M2+1:N3))
       I1=K6*K4
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S169)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,N3,M2,N3,N2,M2,N0,M1,X71,S169,-1.000)
       deallocate(S169)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,M2+1:N3))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,N3,M2,N3,VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S170(N2+1:M2,N0+1:M1,N2+1:N3,M2+1:N3))
       I1=K6*K4
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S170)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,N3,M2,N3,N2,M2,N0,M1,X72,S170,-1.000)
       deallocate(S170)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,N3,N2,M2,VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S171(M2+1:N3,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S171)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,M2,N3,N2,M2,N0,M1,X71,S171, 1.000)
       deallocate(S171)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,N3,N2,M2,VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S172(M2+1:N3,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S172)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,M2,N3,N2,M2,N0,M1,X72,S172, 1.000)
       deallocate(S172)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,N3,N2,M2,VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S173(N2+1:M2,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S173)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,N3,N2,M2,N2,M2,N0,M1,X7,S173, 1.000)
       deallocate(S173)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,N3,N2,M2,VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S174(N2+1:M2,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S174)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,N2,M2,N2,M2,N0,M1,X7,S174,-1.000)
       deallocate(S174)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S175(M2+1:N3,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S175)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3412(M2,N3,M1,N2,N0,M1,M2,N3,
     & N0,M1,M2,N3,M2,N3,M1,N2,S175,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z265(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K8*K5*K0*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z265)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z265, 1.000)
       call
     & sum234615(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z265,-1.000)
       deallocate(Z265)
       deallocate(S175)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S176(M2+1:N3,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S176)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3412(M2,N3,M1,N2,M1,N1,M2,N3,
     & M1,N1,M2,N3,M2,N3,M1,N2,S176,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z266(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K8*K5*K0*K0
       I3=K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z266)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z266, 1.000)
       call
     & sum234615(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z266,-1.000)
       deallocate(Z266)
       deallocate(S176)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S177(M2+1:N3,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S177)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,N1+1:M2,M2+1:N3,M1+1:N2))
       call reorder3412(M2,N3,M1,N2,N0,M1,N1,M2,
     & N0,M1,N1,M2,M2,N3,M1,N2,S177,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N1,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z267(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K8*K5*K0*K0
       I3=K9*K5
       call EGEMM(I1,I2,I3,D1,F2,Z267)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z267, 1.000)
       call
     & sum234615(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z267,-1.000)
       deallocate(Z267)
       deallocate(S177)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S178(M2+1:N3,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S178)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,N1+1:M2,M2+1:N3,M1+1:N2))
       call reorder3412(M2,N3,M1,N2,M1,N1,N1,M2,
     & M1,N1,N1,M2,M2,N3,M1,N2,S178,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N1,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z268(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K8*K5*K0*K0
       I3=K9*K7
       call EGEMM(I1,I2,I3,D1,F2,Z268)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z268, 1.000)
       call
     & sum234615(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z268,-1.000)
       deallocate(Z268)
       deallocate(S178)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S179(N2+1:M2,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S179)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,M1,M2,N3,
     & N0,M1,M2,N3,N2,M2,M1,N2,S179,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z269(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z269)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z269,-1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z269, 1.000)
       deallocate(Z269)
       deallocate(S179)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S180(N2+1:M2,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S180)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,M1,N1,M2,N3,
     & M1,N1,M2,N3,N2,M2,M1,N2,S180,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z270(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z270)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z270,-1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z270, 1.000)
       deallocate(Z270)
       deallocate(S180)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S181(N2+1:M2,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S181)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,N1+1:M2,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,M1,N1,M2,
     & N0,M1,N1,M2,N2,M2,M1,N2,S181,D1)
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N1,M2,M2,N3,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z271(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,D1,F2,Z271)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z271,-1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z271, 1.000)
       deallocate(Z271)
       deallocate(S181)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S182(N2+1:M2,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S182)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,N1+1:M2,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,M1,N1,N1,M2,
     & M1,N1,N1,M2,N2,M2,M1,N2,S182,D1)
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N1,M2,M2,N3,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z272(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,D1,F2,Z272)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z272,-1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z272, 1.000)
       deallocate(Z272)
       deallocate(S182)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S183(N2+1:M2,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S183)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,M1,M2,N3,
     & N0,M1,M2,N3,N2,M2,M1,N2,S183,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z273(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z273)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z273, 1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z273,-1.000)
       deallocate(Z273)
       deallocate(S183)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S184(N2+1:M2,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S184)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,M1,N1,M2,N3,
     & M1,N1,M2,N3,N2,M2,M1,N2,S184,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z274(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z274)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z274, 1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z274,-1.000)
       deallocate(Z274)
       deallocate(S184)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S185(N2+1:M2,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S185)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,N1+1:M2,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,M1,N1,M2,
     & N0,M1,N1,M2,N2,M2,M1,N2,S185,D1)
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N1,M2,M2,N3,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z275(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,D1,F2,Z275)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z275, 1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z275,-1.000)
       deallocate(Z275)
       deallocate(S185)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S186(N2+1:M2,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S186)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,N1+1:M2,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,M1,N1,N1,M2,
     & M1,N1,N1,M2,N2,M2,M1,N2,S186,D1)
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N1,M2,M2,N3,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z276(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,D1,F2,Z276)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z276, 1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z276,-1.000)
       deallocate(Z276)
       deallocate(S186)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S187(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S187)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3412(M2,N3,N0,M1,N0,M1,M2,N3,
     & N0,M1,M2,N3,M2,N3,N0,M1,S187,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z277(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z277)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum235614(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z277, 1.000)
       deallocate(Z277)
       deallocate(S187)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S188(M2+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S188)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3412(M2,N3,N0,M1,M1,N1,M2,N3,
     & M1,N1,M2,N3,M2,N3,N0,M1,S188,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z278(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K0
       I3=K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z278)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum235614(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z278, 1.000)
       deallocate(Z278)
       deallocate(S188)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S189(M2+1:N3,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S189)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,N1+1:M2,M2+1:N3,N0+1:M1))
       call reorder3412(M2,N3,N0,M1,N0,M1,N1,M2,
     & N0,M1,N1,M2,M2,N3,N0,M1,S189,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N1,M2,N2,M2,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z279(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K0
       I3=K9*K5
       call EGEMM(I1,I2,I3,D1,F2,Z279)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum235614(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z279, 1.000)
       deallocate(Z279)
       deallocate(S189)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S190(M2+1:N3,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S190)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,N1+1:M2,M2+1:N3,N0+1:M1))
       call reorder3412(M2,N3,N0,M1,M1,N1,N1,M2,
     & M1,N1,N1,M2,M2,N3,N0,M1,S190,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N1,M2,N2,M2,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z280(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K0
       I3=K9*K7
       call EGEMM(I1,I2,I3,D1,F2,Z280)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum235614(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z280, 1.000)
       deallocate(Z280)
       deallocate(S190)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S191(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S191)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3412(N2,M2,N0,M1,N0,M1,M2,N3,
     & N0,M1,M2,N3,N2,M2,N0,M1,S191,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z281(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z281)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z281,-1.000)
       deallocate(Z281)
       deallocate(S191)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S192(N2+1:M2,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S192)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3412(N2,M2,N0,M1,M1,N1,M2,N3,
     & M1,N1,M2,N3,N2,M2,N0,M1,S192,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z282(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z282)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z282,-1.000)
       deallocate(Z282)
       deallocate(S192)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S193(N2+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S193)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,N1+1:M2,N2+1:M2,N0+1:M1))
       call reorder3412(N2,M2,N0,M1,N0,M1,N1,M2,
     & N0,M1,N1,M2,N2,M2,N0,M1,S193,D1)
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N1,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z283(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,D1,F2,Z283)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z283,-1.000)
       deallocate(Z283)
       deallocate(S193)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S194(N2+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S194)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,N1+1:M2,N2+1:M2,N0+1:M1))
       call reorder3412(N2,M2,N0,M1,M1,N1,N1,M2,
     & M1,N1,N1,M2,N2,M2,N0,M1,S194,D1)
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N1,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z284(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,D1,F2,Z284)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z284,-1.000)
       deallocate(Z284)
       deallocate(S194)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S195(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S195)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3412(N2,M2,N0,M1,N0,M1,M2,N3,
     & N0,M1,M2,N3,N2,M2,N0,M1,S195,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z285(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z285)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z285, 1.000)
       deallocate(Z285)
       deallocate(S195)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S196(N2+1:M2,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S196)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3412(N2,M2,N0,M1,M1,N1,M2,N3,
     & M1,N1,M2,N3,N2,M2,N0,M1,S196,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z286(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z286)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z286, 1.000)
       deallocate(Z286)
       deallocate(S196)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S197(N2+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S197)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,N1+1:M2,N2+1:M2,N0+1:M1))
       call reorder3412(N2,M2,N0,M1,N0,M1,N1,M2,
     & N0,M1,N1,M2,N2,M2,N0,M1,S197,D1)
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N1,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z287(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,D1,F2,Z287)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z287, 1.000)
       deallocate(Z287)
       deallocate(S197)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S198(N2+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S198)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,N1+1:M2,N2+1:M2,N0+1:M1))
       call reorder3412(N2,M2,N0,M1,M1,N1,N1,M2,
     & M1,N1,N1,M2,N2,M2,N0,M1,S198,D1)
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N1,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z288(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,D1,F2,Z288)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z288, 1.000)
       deallocate(Z288)
       deallocate(S198)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S199(M2+1:N3,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S199)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3412(M2,N3,M1,N2,N0,M1,M2,N3,
     & N0,M1,M2,N3,M2,N3,M1,N2,S199,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z289(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K8*K5*K0*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z289)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z289, 1.000)
       call
     & sum234615(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z289,-1.000)
       deallocate(Z289)
       deallocate(S199)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S200(M2+1:N3,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S200)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3412(M2,N3,M1,N2,M1,N2,M2,N3,
     & M1,N2,M2,N3,M2,N3,M1,N2,S200,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder512346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z290(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K8*K5*K0*K0
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z290)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z290,-1.000)
       call
     & sum234615(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z290, 1.000)
       deallocate(Z290)
       deallocate(S200)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S201(M2+1:N3,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S201)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder3412(M2,N3,M1,N2,N0,M1,N2,M2,
     & N0,M1,N2,M2,M2,N3,M1,N2,S201,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z291(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K8*K5*K0*K0
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z291)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z291, 1.000)
       call
     & sum234615(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z291,-1.000)
       deallocate(Z291)
       deallocate(S201)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S202(M2+1:N3,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S202)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,M1+1:N2))
       call reorder3412(M2,N3,M1,N2,M1,N2,N2,M2,
     & M1,N2,N2,M2,M2,N3,M1,N2,S202,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder512346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z292(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K8*K5*K0*K0
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z292)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z292,-1.000)
       call
     & sum234615(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z292, 1.000)
       deallocate(Z292)
       deallocate(S202)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S203(N2+1:M2,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S203)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,M1,M2,N3,
     & N0,M1,M2,N3,N2,M2,M1,N2,S203,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z293(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z293)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z293,-1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z293, 1.000)
       deallocate(Z293)
       deallocate(S203)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S204(N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S204)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,M1,N2,M2,N3,
     & M1,N2,M2,N3,N2,M2,M1,N2,S204,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder512346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z294(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z294)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z294, 1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z294,-1.000)
       deallocate(Z294)
       deallocate(S204)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S205(N2+1:M2,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S205)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,M1,N2,M2,
     & N0,M1,N2,M2,N2,M2,M1,N2,S205,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,M2,N3,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z295(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z295)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z295, 1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z295,-1.000)
       deallocate(Z295)
       deallocate(S205)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S206(N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S206)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,M1,N2,N2,M2,
     & M1,N2,N2,M2,N2,M2,M1,N2,S206,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder521346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,M2,N3,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z296(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z296)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z296,-1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z296, 1.000)
       deallocate(Z296)
       deallocate(S206)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S207(N2+1:M2,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S207)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X111(N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       X111=0.0d0
       call
     & sum3412(N0,M1,M2,N3,N2,M2,M1,N2,X111,S207, 1.000)
       deallocate(S207)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S208(N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S208)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X112(M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       X112=0.0d0
       call
     & sum3412(M1,N2,M2,N3,N2,M2,M1,N2,X112,S208, 1.000)
       deallocate(S208)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S209(N2+1:M2,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S209)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X113(N0+1:M1,N2+1:M2,N2+1:M2,M1+1:N2))
       X113=0.0d0
       call
     & sum3412(N0,M1,N2,M2,N2,M2,M1,N2,X113,S209, 1.000)
       deallocate(S209)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S210(N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S210)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X114(M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       X114=0.0d0
       call
     & sum3412(M1,N2,N2,M2,N2,M2,M1,N2,X114,S210, 1.000)
       deallocate(S210)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:M1))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,M1,VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,M1,N2,t2B,D2)
       allocate(Q31(M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8
       I3=K3*K4*K1
       call EGEMM(I1,I2,I3,D1,D2,Q31)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N0,M1,M1,N2,X52,Q31, 1.000)
       deallocate(Q31)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,M1,N2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,M1,N2,t2B,D2)
       allocate(Q32(M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8
       I3=K3*K4*K1
       call EGEMM(I1,I2,I3,D1,D2,Q32)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M1,N2,M1,N2,X53,Q32, 1.000)
       deallocate(Q32)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S211(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S211)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3412(M2,N3,N0,M1,N0,M1,M2,N3,
     & N0,M1,M2,N3,M2,N3,N0,M1,S211,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z303(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z303)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum235614(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z303, 1.000)
       deallocate(Z303)
       deallocate(S211)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S212(M2+1:N3,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S212)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3412(M2,N3,N0,M1,M1,N2,M2,N3,
     & M1,N2,M2,N3,M2,N3,N0,M1,S212,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z304(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K0
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z304)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum235614(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z304, 1.000)
       deallocate(Z304)
       deallocate(S212)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S213(M2+1:N3,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S213)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder3412(M2,N3,N0,M1,N0,M1,N2,M2,
     & N0,M1,N2,M2,M2,N3,N0,M1,S213,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z305(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K0
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z305)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum235614(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z305, 1.000)
       deallocate(Z305)
       deallocate(S213)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S214(M2+1:N3,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S214)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder3412(M2,N3,N0,M1,M1,N2,N2,M2,
     & M1,N2,N2,M2,M2,N3,N0,M1,S214,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z306(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K0
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z306)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum235614(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z306, 1.000)
       deallocate(Z306)
       deallocate(S214)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S215(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S215)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3412(N2,M2,N0,M1,N0,M1,M2,N3,
     & N0,M1,M2,N3,N2,M2,N0,M1,S215,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z307(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z307)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z307,-1.000)
       deallocate(Z307)
       deallocate(S215)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S216(N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S216)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3412(N2,M2,N0,M1,M1,N2,M2,N3,
     & M1,N2,M2,N3,N2,M2,N0,M1,S216,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z308(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z308)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z308,-1.000)
       deallocate(Z308)
       deallocate(S216)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S217(N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S217)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder3412(N2,M2,N0,M1,N0,M1,N2,M2,
     & N0,M1,N2,M2,N2,M2,N0,M1,S217,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z309(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z309)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z309, 1.000)
       deallocate(Z309)
       deallocate(S217)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S218(N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S218)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder3412(N2,M2,N0,M1,M1,N2,N2,M2,
     & M1,N2,N2,M2,N2,M2,N0,M1,S218,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z310(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z310)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z310, 1.000)
       deallocate(Z310)
       deallocate(S218)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S219(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S219)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X115(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       X115=0.0d0
       call
     & sum3412(N0,M1,M2,N3,N2,M2,N0,M1,X115,S219, 1.000)
       deallocate(S219)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S220(N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S220)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X116(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:M1))
       X116=0.0d0
       call
     & sum3412(M1,N2,M2,N3,N2,M2,N0,M1,X116,S220, 1.000)
       deallocate(S220)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S221(N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S221)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X117(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:M1))
       X117=0.0d0
       call
     & sum3412(N0,M1,N2,M2,N2,M2,N0,M1,X117,S221, 1.000)
       deallocate(S221)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S222(N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S222)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X118(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:M1))
       X118=0.0d0
       call
     & sum3412(M1,N2,N2,M2,N2,M2,N0,M1,X118,S222, 1.000)
       deallocate(S222)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:M1))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,M1,VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:M1))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,M1,t2B,D2)
       allocate(Q33(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K3*K4*K1
       call EGEMM(I1,I2,I3,D1,D2,Q33)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N0,M1,N0,M1,X54,Q33, 1.000)
       deallocate(Q33)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,M1,N2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:M1))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,M1,t2B,D2)
       allocate(Q34(N0+1:M1,M1+1:N2))
       I1=K8
       I2=K5
       I3=K3*K4*K1
       call EGEMM(I1,I2,I3,D1,D2,Q34)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M1,N2,N0,M1,X55,Q34, 1.000)
       deallocate(Q34)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,M2,N3,t2B,D2)
       allocate(Q35(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q35)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M2,N3,M2,N3,X56,Q35,-1.000)
       deallocate(Q35)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,M2,N3,t2B,D2)
       allocate(Q36(M2+1:N3,N2+1:M2))
       I1=K0
       I2=K6
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q36)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N2,M2,M2,N3,X57,Q36,-1.000)
       deallocate(Q36)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,M2,t2B,D2)
       allocate(Q37(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q37)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M2,N3,N2,M2,X58,Q37,-1.000)
       deallocate(Q37)
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
       call EGEMM(I1,I2,I3,D1,D2,Q38)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N2,M2,N2,M2,X59,Q38,-1.000)
       deallocate(Q38)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,M2,t2B,D2)
       allocate(Q39(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q39)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M2,N3,N2,M2,X60,Q39,-1.000)
       deallocate(Q39)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,M2,t2B,D2)
       allocate(Q40(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K3*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q40)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N2,M2,N2,M2,X61,Q40,-1.000)
       deallocate(Q40)
C
       allocate(B1(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,FBHP,B1)
       allocate(D2(N0+1:N2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,M2,N2,M2,N0,M1,t2C,D2)
       allocate(S223(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K2
       call EGEMM(I1,I2,I3,B1,D2,S223)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,N0,M1,X7,S223,-1.000)
       deallocate(S223)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,FBHP,B1)
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,M1,N2,M1,N2,t2C,D2)
       allocate(S224(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S224)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X62,S224, 1.000)
       deallocate(S224)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,FBHP,B1)
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,M1,N2,M1,N2,t2C,D2)
       allocate(S225(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S225)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X63,S225,-1.000)
       deallocate(S225)
C
       allocate(B1(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,FBHP,B1)
       allocate(D2(N0+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,M2,N2,M2,M1,N2,t2C,D2)
       allocate(S226(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K2
       call EGEMM(I1,I2,I3,B1,D2,S226)
       deallocate(B1)
       deallocate(D2)
C
       allocate(X119(N2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       X119=0.0d0
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X119,S226, 1.000)
       deallocate(S226)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,FBHP,B1)
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,N0,M1,M1,N2,t2C,D2)
       allocate(S227(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S227)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X67,S227, 1.000)
       deallocate(S227)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,FBHP,B1)
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,N0,M1,M1,N2,t2C,D2)
       allocate(S228(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S228)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X68,S228,-1.000)
       deallocate(S228)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,M1,N2,VCHHHP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,M2,N2,M2,t2C,D2)
       allocate(S229(N2+1:M2,N2+1:M2,N2+1:N3,M1+1:N2))
       I1=K8*K4
       I2=K0*K0
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S229)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N2,N3,N2,M2,N2,M2,M1,N2,X5,S229, 0.500)
       deallocate(S229)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,M1,N2,VCHHHP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,N2,M2,t2C,D2)
       allocate(S230(M2+1:N3,N2+1:M2,N2+1:N3,M1+1:N2))
       I1=K8*K4
       I2=K0*K6
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S230)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X120(N2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       X120=0.0d0
       call
     & sum2314(N2,N3,M2,N3,N2,M2,M1,N2,X120,S230, 1.000)
       deallocate(S230)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder4132(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,M1,N2,VCHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S231(N2+1:M2,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S231)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder3124(N2,M2,N0,M1,N0,N2,M1,N2,
     & N0,N2,N2,M2,N0,M1,M1,N2,S231,D1)
       allocate(D2(N0+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,M2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(Z331(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:M2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K0
       I2=K8*K0*K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,D2,Z331)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum125346(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z331,-1.000)
       call
     & sum135246(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z331, 1.000)
       call
     & sum126345(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z331, 1.000)
       call
     & sum136245(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z331,-1.000)
       deallocate(Z331)
       deallocate(S231)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder3142(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,M1,N2,VCHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S232(M2+1:N3,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S232)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,M2,N3,M1,N2,M1,N2,X64,S232, 1.000)
       deallocate(S232)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder3142(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,M1,N2,VCHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2C,D2)
       allocate(S233(M2+1:N3,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S233)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,M2,N3,N0,M1,M1,N2,X3,S233, 1.000)
       deallocate(S233)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder3142(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,M1,N2,VCHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S234(N2+1:M2,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S234)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder3124(N2,M2,M1,N2,N0,N2,M1,N2,
     & N0,N2,N2,M2,M1,N2,M1,N2,S234,D1)
       allocate(D2(N0+1:N2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,M2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(Z334(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:M2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K0
       I2=K5*K0*K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,D2,Z334)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum134256(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z334, 1.000)
       call
     & sum124356(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z334,-1.000)
       call
     & sum134265(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z334,-1.000)
       call
     & sum124365(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z334, 1.000)
       deallocate(Z334)
       deallocate(S234)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:M1))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N0,M1,VCHHHP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,M2,N2,M2,t2C,D2)
       allocate(S235(N2+1:M2,N2+1:M2,N2+1:N3,N0+1:M1))
       I1=K5*K4
       I2=K0*K0
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S235)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N2,N3,N2,M2,N2,M2,N0,M1,X7,S235, 0.500)
       deallocate(S235)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:M1))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N0,M1,VCHHHP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,N2,M2,t2C,D2)
       allocate(S236(M2+1:N3,N2+1:M2,N2+1:N3,N0+1:M1))
       I1=K5*K4
       I2=K0*K6
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S236)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X121(N2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1))
       X121=0.0d0
       call
     & sum2314(N2,N3,M2,N3,N2,M2,N0,M1,X121,S236, 1.000)
       deallocate(S236)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,M2+1:N3))
       call reorder4213(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,M2,N3,VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S237(N2+1:M2,N0+1:M1,N2+1:N3,M2+1:N3))
       I1=K6*K4
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S237)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,N3,M2,N3,N2,M2,N0,M1,X121,S237, 2.000)
       deallocate(S237)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,M2+1:N3))
       call reorder1243(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N2,N3,N0,N2,M2,N3,VCHPPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,M1,N2,M1,N2,t2C,D2)
       allocate(S238(M1+1:N2,M1+1:N2,N0+1:N2,M2+1:N3))
       I1=K6*K2
       I2=K8*K8
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S238)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,N2,M2,N3,M1,N2,M1,N2,X1,S238, 0.500)
       deallocate(S238)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder4213(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,M2,VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S239(N2+1:M2,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S239)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder3412(N2,M2,N0,M1,N2,N3,N2,M2,
     & N2,N3,N2,M2,N2,M2,N0,M1,S239,D1)
       allocate(D2(N2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,M2,N3,M1,N2,M1,N2,t2C,D2)
       allocate(Z339(M2+1:N3,M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K0
       I2=K8*K8*K6
       I3=K4
       call EGEMM(I1,I2,I3,D1,D2,Z339)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum156234(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z339,-1.000)
       call
     & sum156324(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z339, 1.000)
       deallocate(Z339)
       deallocate(S239)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,M2,VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2C,D2)
       allocate(S240(M2+1:N3,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S240)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X6(N2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1))
       X6=0.0d0
       call
     & sum2413(N2,N3,M2,N3,N2,M2,N0,M1,X6,S240, 1.000)
       deallocate(S240)
C
       call sumx2314(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,M2,N3,N2,M2,N0,M1,X6,VCHPPP, 1.000)
C
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,M1,N2,M1,N2,t2C,D2)
       allocate(Z7(N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K6
       I2=K8*K8*K0
       I3=K4
       call EGEMM(I1,I2,I3,X6,D2,Z7)
       deallocate(D2)
C
       call
     & sum356124(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z7,-1.000)
       call
     & sum256134(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z7, 1.000)
       deallocate(Z7)
       deallocate(X6)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:M2))
       call reorder1243(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N2,N3,N0,N2,N2,M2,VCHPPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,M1,N2,M1,N2,t2C,D2)
       allocate(S241(M1+1:N2,M1+1:N2,N0+1:N2,N2+1:M2))
       I1=K0*K2
       I2=K8*K8
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S241)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X2(N0+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       X2=0.0d0
       call
     & sum3412(N0,N2,N2,M2,M1,N2,M1,N2,X2,S241, 0.500)
       deallocate(S241)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:M1))
       call reorder4132(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N0,M1,VCHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S242(N2+1:M2,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S242)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X122(N0+1:N2,N2+1:M2,N0+1:M1,M1+1:N2))
       X122=0.0d0
       call
     & sum2413(N0,N2,N2,M2,N0,M1,M1,N2,X122,S242, 1.000)
       deallocate(S242)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:M1))
       call reorder3142(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N0,M1,VCHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S243(M2+1:N3,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S243)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N2,M2,N3,N0,M1,M1,N2,X3,S243,-1.000)
       deallocate(S243)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,M2+1:N3))
       call reorder4213(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,M2,N3,VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S244(N2+1:M2,M1+1:N2,N2+1:N3,M2+1:N3))
       I1=K6*K4
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S244)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X123(N2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       X123=0.0d0
       call
     & sum3412(N2,N3,M2,N3,N2,M2,M1,N2,X123,S244, 1.000)
       deallocate(S244)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,M2+1:N3))
       call reorder1243(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N2,N3,N0,N2,M2,N3,VCHPPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:M1,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,M1,M1,N2,t2C,D2)
       allocate(S245(N0+1:M1,M1+1:N2,N0+1:N2,M2+1:N3))
       I1=K6*K2
       I2=K8*K5
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S245)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,N2,M2,N3,N0,M1,M1,N2,X3,S245, 0.500)
       deallocate(S245)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder4213(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,M2,VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S246(N2+1:M2,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S246)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N2,N3,N2,M2,
     & N2,N3,N2,M2,N2,M2,M1,N2,S246,D1)
       allocate(D2(N2+1:N3,M2+1:N3,N0+1:M1,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,M2,N3,N0,M1,M1,N2,t2C,D2)
       allocate(Z346(M2+1:N3,N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K0
       I2=K8*K5*K6
       I3=K4
       call EGEMM(I1,I2,I3,D1,D2,Z346)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum146235(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z346, 1.000)
       call
     & sum146325(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z346,-1.000)
       call
     & sum145236(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z346,-1.000)
       call
     & sum145326(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z346, 1.000)
       deallocate(Z346)
       deallocate(S246)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,M2,VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S247(M2+1:N3,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S247)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3142(M2,N3,M1,N2,N2,N3,N2,M2,
     & N2,N3,M2,N3,N2,M2,M1,N2,S247,D1)
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,N0,M1,M1,N2,t2C,D2)
       allocate(Z347(N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K6
       I2=K8*K5*K0
       I3=K4
       call EGEMM(I1,I2,I3,D1,D2,Z347)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum346125(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z347, 1.000)
       call
     & sum246135(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z347,-1.000)
       call
     & sum345126(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z347,-1.000)
       call
     & sum245136(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z347, 1.000)
       deallocate(Z347)
       deallocate(S247)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:M2))
       call reorder1243(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N2,N3,N0,N2,N2,M2,VCHPPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:M1,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,M1,M1,N2,t2C,D2)
       allocate(S248(N0+1:M1,M1+1:N2,N0+1:N2,N2+1:M2))
       I1=K0*K2
       I2=K8*K5
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S248)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X4(N0+1:N2,N2+1:M2,N0+1:M1,M1+1:N2))
       X4=0.0d0
       call
     & sum3412(N0,N2,N2,M2,N0,M1,M1,N2,X4,S248, 0.500)
       deallocate(S248)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder563124(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N2,M2,N2,M2,N0,M1,t3C1,F2)
       allocate(S249(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S249)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,N0,M1,X7,S249, 1.000)
       deallocate(S249)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & N0,M1,M1,N1,M2,N3,N2,M2,N2,M2,N0,M1,t3C4,F2)
       allocate(S250(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S250)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,N0,M1,X7,S250,-1.000)
       deallocate(S250)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder563124(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N2,M2,N2,M2,N0,M1,t3C1,F2)
       allocate(S251(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S251)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,N0,M1,X7,S251, 1.000)
       deallocate(S251)
C
       allocate(D1(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder563124(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,N1,M2,N2,M2,N2,M2,N0,M1,t3C1,F2)
       allocate(S252(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K9*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S252)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,N0,M1,X7,S252, 1.000)
       deallocate(S252)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & N0,M1,M1,N1,N1,M2,N2,M2,N2,M2,N0,M1,t3C4,F2)
       allocate(S253(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S253)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,N0,M1,X7,S253,-1.000)
       deallocate(S253)
C
       allocate(D1(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder563124(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,N1,M2,N2,M2,N2,M2,N0,M1,t3C1,F2)
       allocate(S254(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K9*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S254)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,N0,M1,X7,S254, 1.000)
       deallocate(S254)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder563124(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,M2,N3,N2,M2,N0,M1,t3C1,F2)
       allocate(S255(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K6
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S255)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,N0,M1,X121,S255, 2.000)
       deallocate(S255)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & N0,M1,M1,N1,M2,N3,M2,N3,N2,M2,N0,M1,t3C4,F2)
       allocate(S256(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S256)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,N0,M1,X121,S256,-2.000)
       deallocate(S256)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder563124(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,M2,N3,N2,M2,N0,M1,t3C1,F2)
       allocate(S257(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K6
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S257)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,N0,M1,X121,S257, 2.000)
       deallocate(S257)
C
       allocate(D1(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder563124(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,N1,M2,M2,N3,N2,M2,N0,M1,t3C1,F2)
       allocate(S258(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K6
       I3=K9*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S258)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,N0,M1,X121,S258, 2.000)
       deallocate(S258)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & N0,M1,M1,N1,N1,M2,M2,N3,N2,M2,N0,M1,t3C4,F2)
       allocate(S259(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K6
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S259)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,N0,M1,X121,S259,-2.000)
       deallocate(S259)
C
       allocate(D1(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder563124(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,N1,M2,M2,N3,N2,M2,N0,M1,t3C1,F2)
       allocate(S260(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K6
       I3=K9*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S260)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,N0,M1,X121,S260, 2.000)
       deallocate(S260)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N0,M1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N0,M1,M2,N3,N2,M2,N2,M2,M1,N2,t3C1,F2)
       allocate(S261(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S261)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X119,S261, 1.000)
       deallocate(S261)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N2,M2,N2,M2,M1,N2,t3C1,F2)
       allocate(S262(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S262)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X119,S262, 1.000)
       deallocate(S262)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N2,M2,N2,M2,M1,N2,t3C1,F2)
       allocate(S263(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S263)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X119,S263, 1.000)
       deallocate(S263)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N2,M2,N2,M2,M1,N2,t3C1,F2)
       allocate(S264(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S264)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X119,S264, 1.000)
       deallocate(S264)
C
       allocate(D1(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N0,M1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N0,M1,N1,M2,N2,M2,N2,M2,M1,N2,t3C1,F2)
       allocate(S265(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K9*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S265)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X119,S265, 1.000)
       deallocate(S265)
C
       allocate(D1(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,N1,M2,N2,M2,N2,M2,M1,N2,t3C1,F2)
       allocate(S266(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K9*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S266)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X119,S266, 1.000)
       deallocate(S266)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N1,N1,M2,N2,M2,N2,M2,M1,N2,t3C1,F2)
       allocate(S267(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S267)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X119,S267, 1.000)
       deallocate(S267)
C
       allocate(D1(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,N1,M2,N2,M2,N2,M2,M1,N2,t3C1,F2)
       allocate(S268(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K9*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S268)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X119,S268, 1.000)
       deallocate(S268)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N0,M1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,t3C1,F2)
       allocate(S269(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S269)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X123,S269,-1.000)
       deallocate(S269)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,t3C1,F2)
       allocate(S270(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S270)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X123,S270,-1.000)
       deallocate(S270)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,M2,N3,N2,M2,M1,N2,t3C1,F2)
       allocate(S271(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S271)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X123,S271,-1.000)
       deallocate(S271)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,M2,N3,N2,M2,M1,N2,t3C1,F2)
       allocate(S272(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S272)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X123,S272,-1.000)
       deallocate(S272)
C
       allocate(D1(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N0,M1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N0,M1,N1,M2,M2,N3,N2,M2,M1,N2,t3C1,F2)
       allocate(S273(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K9*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S273)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X123,S273,-1.000)
       deallocate(S273)
C
       allocate(D1(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,N1,M2,M2,N3,N2,M2,M1,N2,t3C1,F2)
       allocate(S274(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K9*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S274)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X123,S274,-1.000)
       deallocate(S274)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N1,N1,M2,M2,N3,N2,M2,M1,N2,t3C1,F2)
       allocate(S275(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S275)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X123,S275,-1.000)
       deallocate(S275)
C
       allocate(D1(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,N1,M2,M2,N3,N2,M2,M1,N2,t3C1,F2)
       allocate(S276(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K9*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S276)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X123,S276,-1.000)
       deallocate(S276)
C
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,N0,M1,M1,N2,t2C,D2)
       allocate(Z344(N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K6
       I2=K8*K5*K0
       I3=K4
       call EGEMM(I1,I2,I3,X123,D2,Z344)
       deallocate(D2)
C
       call
     & sum246135(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z344,-1.000)
       call
     & sum346125(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z344, 1.000)
       call
     & sum345126(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z344,-1.000)
       call
     & sum245136(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z344, 1.000)
       deallocate(Z344)
       deallocate(X123)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(S277(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S277)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X122,S277,-1.000)
       deallocate(S277)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(S278(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K6*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S278)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X122,S278,-1.000)
       deallocate(S278)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(S279(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S279)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X122,S279,-1.000)
       deallocate(S279)
C
       allocate(D1(M1+1:N1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,N2,M2,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,M2,N3,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(S280(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K6*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S280)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X122,S280,-1.000)
       deallocate(S280)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(S281(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S281)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X122,S281,-1.000)
       deallocate(S281)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,N1,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(S282(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S282)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X122,S282,-1.000)
       deallocate(S282)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(S283(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S283)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X122,S283,-1.000)
       deallocate(S283)
C
       allocate(D1(M1+1:N1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,N2,M2,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,N1,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(S284(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K9*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S284)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X122,S284,-1.000)
       deallocate(S284)
C
       allocate(D2(N0+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,M2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(Z342(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:M2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K0
       I2=K8*K0*K6
       I3=K2
       call EGEMM(I1,I2,I3,X122,D2,Z342)
       deallocate(D2)
C
       call
     & sum126345(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z342,-1.000)
       call
     & sum136245(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z342, 1.000)
       call
     & sum135246(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z342,-1.000)
       call
     & sum125346(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z342, 1.000)
       deallocate(Z342)
       deallocate(X122)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S285(M2+1:N3,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S285)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,M2,N3,M1,N2,X8,S285, 1.000)
       deallocate(S285)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S286(M2+1:N3,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S286)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,M2,N3,M2,N3,M1,N2,X9,S286, 1.000)
       deallocate(S286)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S287(M2+1:N3,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S287)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N1,M2,M2,N3,M1,N2,X10,S287, 1.000)
       deallocate(S287)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S288(M2+1:N3,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S288)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,N1,M2,M2,N3,M1,N2,X11,S288, 1.000)
       deallocate(S288)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N2))
       call reorder623145(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,M2,N3,N0,M1,M1,N2,t3C1,F2)
       allocate(S289(M2+1:N3,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K6
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S289)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X124(N0+1:N2,M2+1:N3,N0+1:M1,M1+1:N2))
       X124=0.0d0
       call
     & sum2341(N0,N2,M2,N3,N0,M1,M1,N2,X124,S289, 1.000)
       deallocate(S289)
C
       allocate(D1(M1+1:N1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,N2,M2,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N2))
       call reorder623145(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,M2,N3,M2,N3,N0,M1,M1,N2,t3C1,F2)
       allocate(S290(M2+1:N3,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K6
       I3=K6*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S290)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,N0,M1,M1,N2,X124,S290, 1.000)
       deallocate(S290)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N2))
       call reorder613245(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,M2,N3,N0,M1,M1,N2,t3C3,F2)
       allocate(S291(M2+1:N3,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K6
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S291)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,N0,M1,M1,N2,X124,S291,-1.000)
       deallocate(S291)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N2))
       call reorder613245(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,N1,M2,M2,N3,N0,M1,M1,N2,t3C3,F2)
       allocate(S292(M2+1:N3,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K6
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S292)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,N0,M1,M1,N2,X124,S292,-1.000)
       deallocate(S292)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N2))
       call reorder623145(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,M2,N3,N0,M1,M1,N2,t3C1,F2)
       allocate(S293(M2+1:N3,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K6
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S293)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,N0,M1,M1,N2,X124,S293, 1.000)
       deallocate(S293)
C
       allocate(D1(M1+1:N1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,N2,M2,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N2))
       call reorder623145(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,N1,M2,M2,N3,N0,M1,M1,N2,t3C1,F2)
       allocate(S294(M2+1:N3,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K6
       I3=K9*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S294)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,N0,M1,M1,N2,X124,S294, 1.000)
       deallocate(S294)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S295(N2+1:M2,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S295)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X12(N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       X12=0.0d0
       call
     & sum3412(N0,M1,M2,N3,N2,M2,M1,N2,X12,S295, 1.000)
       deallocate(S295)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,M2,N3,N2,M2,M1,N2,X12,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z13(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X12,F2,Z13)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z13,-1.000)
       call
     & sum124536(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z13, 1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z13, 1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z13,-1.000)
       deallocate(Z13)
       deallocate(X12)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S296(N2+1:M2,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S296)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X13(M1+1:N1,M2+1:N3,N2+1:M2,M1+1:N2))
       X13=0.0d0
       call
     & sum3412(M1,N1,M2,N3,N2,M2,M1,N2,X13,S296, 1.000)
       deallocate(S296)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,M2,N3,N2,M2,M1,N2,X13,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z14(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X13,F2,Z14)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z14,-1.000)
       call
     & sum124536(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z14, 1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z14, 1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z14,-1.000)
       deallocate(Z14)
       deallocate(X13)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S297(N2+1:M2,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S297)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X14(N0+1:M1,N1+1:M2,N2+1:M2,M1+1:N2))
       X14=0.0d0
       call
     & sum3412(N0,M1,N1,M2,N2,M2,M1,N2,X14,S297, 1.000)
       deallocate(S297)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,N1,M2,N2,M2,M1,N2,X14,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N1,M2,M2,N3,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z15(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X14,F2,Z15)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z15,-1.000)
       call
     & sum124536(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z15, 1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z15, 1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z15,-1.000)
       deallocate(Z15)
       deallocate(X14)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S298(N2+1:M2,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S298)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X15(M1+1:N1,N1+1:M2,N2+1:M2,M1+1:N2))
       X15=0.0d0
       call
     & sum3412(M1,N1,N1,M2,N2,M2,M1,N2,X15,S298, 1.000)
       deallocate(S298)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,N1,M2,N2,M2,M1,N2,X15,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N1,M2,M2,N3,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z16(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X15,F2,Z16)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z16,-1.000)
       call
     & sum124536(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z16, 1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z16, 1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z16,-1.000)
       deallocate(Z16)
       deallocate(X15)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(S299(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S299)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X125(N0+1:N2,N2+1:M2,M1+1:N2,M1+1:N2))
       X125=0.0d0
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X125,S299, 1.000)
       deallocate(S299)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(S300(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K6*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S300)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X125,S300, 1.000)
       deallocate(S300)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(S301(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S301)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X125,S301, 1.000)
       deallocate(S301)
C
       allocate(D1(M1+1:N1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,N2,M2,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(S302(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K6*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S302)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X125,S302, 1.000)
       deallocate(S302)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(S303(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S303)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X125,S303, 1.000)
       deallocate(S303)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,N1,M2,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(S304(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S304)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X125,S304, 1.000)
       deallocate(S304)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(S305(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S305)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X125,S305, 1.000)
       deallocate(S305)
C
       allocate(D1(M1+1:N1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,N2,M2,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,N1+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,N1,M2,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(S306(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K9*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S306)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X125,S306, 1.000)
       deallocate(S306)
C
       allocate(D2(N0+1:N2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,M2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(Z399(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:M2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K0
       I2=K5*K0*K6
       I3=K2
       call EGEMM(I1,I2,I3,X125,D2,Z399)
       deallocate(D2)
C
       call
     & sum124356(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z399, 1.000)
       call
     & sum134256(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z399,-1.000)
       deallocate(Z399)
       deallocate(X125)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2C,D2)
       allocate(S307(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S307)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,M2,N3,N0,M1,X16,S307, 1.000)
       deallocate(S307)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2C,D2)
       allocate(S308(M2+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S308)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,M2,N3,M2,N3,N0,M1,X17,S308, 1.000)
       deallocate(S308)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2C,D2)
       allocate(S309(M2+1:N3,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S309)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N1,M2,M2,N3,N0,M1,X18,S309, 1.000)
       deallocate(S309)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2C,D2)
       allocate(S310(M2+1:N3,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S310)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,N1,M2,M2,N3,N0,M1,X19,S310, 1.000)
       deallocate(S310)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder623145(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,M2,N3,M1,N2,M1,N2,t3C1,F2)
       allocate(S311(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S311)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X1,S311, 1.000)
       deallocate(S311)
C
       allocate(D1(M1+1:N1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,N2,M2,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder623145(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,M2,N3,M2,N3,M1,N2,M1,N2,t3C1,F2)
       allocate(S312(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K6*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S312)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X1,S312, 1.000)
       deallocate(S312)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder613245(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,M2,N3,M1,N2,M1,N2,t3C3,F2)
       allocate(S313(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S313)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X1,S313,-1.000)
       deallocate(S313)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder613245(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,N1,M2,M2,N3,M1,N2,M1,N2,t3C3,F2)
       allocate(S314(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S314)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X1,S314,-1.000)
       deallocate(S314)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder623145(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,M2,N3,M1,N2,M1,N2,t3C1,F2)
       allocate(S315(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S315)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X1,S315, 1.000)
       deallocate(S315)
C
       allocate(D1(M1+1:N1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,N2,M2,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,N1+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder623145(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,N1,M2,M2,N3,M1,N2,M1,N2,t3C1,F2)
       allocate(S316(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K9*K0*K7
       call EGEMM(I1,I2,I3,D1,F2,S316)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X1,S316, 1.000)
       deallocate(S316)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S317(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S317)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X20(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       X20=0.0d0
       call
     & sum3412(N0,M1,M2,N3,N2,M2,N0,M1,X20,S317, 1.000)
       deallocate(S317)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,M2,N3,N2,M2,N0,M1,X20,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z21(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X20,F2,Z21)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z21,-1.000)
       call
     & sum125634(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z21, 1.000)
       deallocate(Z21)
       deallocate(X20)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S318(N2+1:M2,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S318)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X21(M1+1:N1,M2+1:N3,N2+1:M2,N0+1:M1))
       X21=0.0d0
       call
     & sum3412(M1,N1,M2,N3,N2,M2,N0,M1,X21,S318, 1.000)
       deallocate(S318)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,M2,N3,N2,M2,N0,M1,X21,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z22(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X21,F2,Z22)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z22,-1.000)
       call
     & sum125634(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z22, 1.000)
       deallocate(Z22)
       deallocate(X21)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S319(N2+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S319)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X22(N0+1:M1,N1+1:M2,N2+1:M2,N0+1:M1))
       X22=0.0d0
       call
     & sum3412(N0,M1,N1,M2,N2,M2,N0,M1,X22,S319, 1.000)
       deallocate(S319)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,N1,M2,N2,M2,N0,M1,X22,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N1,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z23(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X22,F2,Z23)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z23,-1.000)
       call
     & sum125634(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z23, 1.000)
       deallocate(Z23)
       deallocate(X22)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S320(N2+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S320)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X23(M1+1:N1,N1+1:M2,N2+1:M2,N0+1:M1))
       X23=0.0d0
       call
     & sum3412(M1,N1,N1,M2,N2,M2,N0,M1,X23,S320, 1.000)
       deallocate(S320)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,N1,M2,N2,M2,N0,M1,X23,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N1,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z24(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X23,F2,Z24)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z24,-1.000)
       call
     & sum125634(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z24, 1.000)
       deallocate(Z24)
       deallocate(X23)
C
       allocate(D1(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M1,N2,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M1,N2,M2,N3,N2,M2,N2,M2,N0,M1,t3D,F2)
       allocate(S321(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K6*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S321)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,N0,M1,X7,S321,-1.000)
       deallocate(S321)
C
       allocate(D1(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M1,N2,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder561234(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M1,N2,M2,N3,N2,M2,N2,M2,N0,M1,t3D,F2)
       allocate(S322(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K6*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S322)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,N0,M1,X7,S322, 0.500)
       deallocate(S322)
C
       allocate(D1(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M1,N2,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M1,N2,N2,M2,N2,M2,N2,M2,N0,M1,t3D,F2)
       allocate(S323(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K0*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S323)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,N0,M1,X7,S323,-1.000)
       deallocate(S323)
C
       allocate(D1(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M1,N2,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder561234(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M1,N2,N2,M2,N2,M2,N2,M2,N0,M1,t3D,F2)
       allocate(S324(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K0*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S324)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,N0,M1,X7,S324, 0.500)
       deallocate(S324)
C
       allocate(D1(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M1,N2,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M1,N2,M2,N3,M2,N3,N2,M2,N0,M1,t3D,F2)
       allocate(S325(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K6
       I3=K6*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S325)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,N0,M1,X72,S325, 1.000)
       deallocate(S325)
C
       allocate(D1(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M1,N2,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder561234(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M1,N2,M2,N3,M2,N3,N2,M2,N0,M1,t3D,F2)
       allocate(S326(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K6
       I3=K6*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S326)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,N0,M1,X72,S326,-0.500)
       deallocate(S326)
C
       allocate(D1(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M1,N2,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder462135(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M1,N2,N2,M2,M2,N3,N2,M2,N0,M1,t3D,F2)
       allocate(S327(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K6
       I3=K0*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S327)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,N0,M1,X72,S327,-1.000)
       deallocate(S327)
C
       allocate(D1(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M1,N2,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder562134(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M1,N2,N2,M2,M2,N3,N2,M2,N0,M1,t3D,F2)
       allocate(S328(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K6
       I3=K0*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S328)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,N0,M1,X72,S328, 0.500)
       deallocate(S328)
C
       allocate(D1(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M1,N2,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M1,N2,M2,N3,M2,N3,N2,M2,N0,M1,t3D,F2)
       allocate(S329(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K6
       I3=K6*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S329)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,N0,M1,X71,S329,-1.000)
       deallocate(S329)
C
       allocate(D1(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M1,N2,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder561234(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M1,N2,M2,N3,M2,N3,N2,M2,N0,M1,t3D,F2)
       allocate(S330(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K6
       I3=K6*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S330)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,N0,M1,X71,S330, 0.500)
       deallocate(S330)
C
       allocate(D1(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M1,N2,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder462135(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M1,N2,N2,M2,M2,N3,N2,M2,N0,M1,t3D,F2)
       allocate(S331(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K6
       I3=K0*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S331)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,N0,M1,X71,S331, 1.000)
       deallocate(S331)
C
       allocate(D1(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M1,N2,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder562134(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M1,N2,N2,M2,M2,N3,N2,M2,N0,M1,t3D,F2)
       allocate(S332(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K6
       I3=K0*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S332)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,N0,M1,X71,S332,-0.500)
       deallocate(S332)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:M1,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,M1,M1,N2,VCHHPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,M1,N2,M1,N2,t2C,D2)
       allocate(S333(M1+1:N2,M1+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5
       I2=K8*K8
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S333)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X34(N0+1:M1,M1+1:N2,M1+1:N2,M1+1:N2))
       X34=0.0d0
       call
     & sum3412(N0,M1,M1,N2,M1,N2,M1,N2,X34,S333, 0.500)
       deallocate(S333)
C
       allocate(D1(N2+1:N3,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,M1,N2,M1,N2,VCHHPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,M1,N2,M1,N2,t2C,D2)
       allocate(S334(M1+1:N2,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8
       I2=K8*K8
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S334)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X35(M1+1:N2,M1+1:N2,M1+1:N2,M1+1:N2))
       X35=0.0d0
       call
     & sum3412(M1,N2,M1,N2,M1,N2,M1,N2,X35,S334, 0.500)
       deallocate(S334)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,N0,M1,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N0,M1,M2,N3,N2,M2,N2,M2,M1,N2,t3D,F2)
       allocate(S335(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S335)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X119,S335,-0.500)
       deallocate(S335)
C
       allocate(D1(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M1,N2,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M1,N2,M2,N3,N2,M2,N2,M2,M1,N2,t3D,F2)
       allocate(S336(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K6*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S336)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X119,S336,-1.000)
       deallocate(S336)
C
       allocate(D1(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M1,N2,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M1,N2,M2,N3,N2,M2,N2,M2,M1,N2,t3D,F2)
       allocate(S337(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K6*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S337)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X119,S337,-0.500)
       deallocate(S337)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,N0,M1,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N0,M1,N2,M2,N2,M2,N2,M2,M1,N2,t3D,F2)
       allocate(S338(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S338)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X119,S338,-0.500)
       deallocate(S338)
C
       allocate(D1(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M1,N2,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M1,N2,N2,M2,N2,M2,N2,M2,M1,N2,t3D,F2)
       allocate(S339(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K0*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S339)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X119,S339,-1.000)
       deallocate(S339)
C
       allocate(D1(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M1,N2,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M1,N2,N2,M2,N2,M2,N2,M2,M1,N2,t3D,F2)
       allocate(S340(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K0*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S340)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X119,S340,-0.500)
       deallocate(S340)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,N0,M1,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,t3D,F2)
       allocate(S341(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S341)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X126(N2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       X126=0.0d0
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X126,S341, 1.000)
       deallocate(S341)
C
       allocate(D1(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M1,N2,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,t3D,F2)
       allocate(S342(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S342)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X126,S342, 2.000)
       deallocate(S342)
C
       allocate(D1(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M1,N2,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,t3D,F2)
       allocate(S343(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S343)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X126,S343, 1.000)
       deallocate(S343)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,N0,M1,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder452136(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N0,M1,N2,M2,M2,N3,N2,M2,M1,N2,t3D,F2)
       allocate(S344(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S344)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X126,S344,-1.000)
       deallocate(S344)
C
       allocate(D1(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M1,N2,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder452136(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M1,N2,N2,M2,M2,N3,N2,M2,M1,N2,t3D,F2)
       allocate(S345(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K0*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S345)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X126,S345,-2.000)
       deallocate(S345)
C
       allocate(D1(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M1,N2,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder452136(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M1,N2,N2,M2,M2,N3,N2,M2,M1,N2,t3D,F2)
       allocate(S346(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K0*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S346)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X126,S346,-1.000)
       deallocate(S346)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,N0,M1,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,t3D,F2)
       allocate(S347(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S347)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X127(N2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       X127=0.0d0
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X127,S347, 1.000)
       deallocate(S347)
C
       allocate(D1(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M1,N2,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,t3D,F2)
       allocate(S348(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S348)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X127,S348, 2.000)
       deallocate(S348)
C
       allocate(D1(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M1,N2,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,t3D,F2)
       allocate(S349(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K6*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S349)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X127,S349, 1.000)
       deallocate(S349)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,N0,M1,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder452136(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N0,M1,N2,M2,M2,N3,N2,M2,M1,N2,t3D,F2)
       allocate(S350(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S350)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X127,S350,-1.000)
       deallocate(S350)
C
       allocate(D1(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M1,N2,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder452136(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M1,N2,N2,M2,M2,N3,N2,M2,M1,N2,t3D,F2)
       allocate(S351(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K0*K8*K5
       call EGEMM(I1,I2,I3,D1,F2,S351)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X127,S351,-2.000)
       deallocate(S351)
C
       allocate(D1(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M1,N2,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder452136(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M1,N2,N2,M2,M2,N3,N2,M2,M1,N2,t3D,F2)
       allocate(S352(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K6
       I3=K0*K8*K8
       call EGEMM(I1,I2,I3,D1,F2,S352)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,M2,N3,N2,M2,M1,N2,X127,S352,-1.000)
       deallocate(S352)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:M1,N0+1:M1))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,M1,N0,M1,VCHHPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:M1,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,M1,M1,N2,t2C,D2)
       allocate(S353(N0+1:M1,M1+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K8*K5
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S353)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N0,M1,N0,M1,M1,N2,X36,S353, 0.500)
       deallocate(S353)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:M1,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,M1,M1,N2,VCHHPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:M1,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,M1,M1,N2,t2C,D2)
       allocate(S354(N0+1:M1,M1+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5
       I2=K8*K5
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S354)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M1,N2,N0,M1,M1,N2,X37,S354, 0.500)
       deallocate(S354)
C
       allocate(D1(N2+1:N3,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,M1,N2,M1,N2,VCHHPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:M1,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,M1,M1,N2,t2C,D2)
       allocate(S355(N0+1:M1,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8
       I2=K8*K5
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S355)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,M1,N2,N0,M1,M1,N2,X38,S355, 0.500)
       deallocate(S355)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M2,N3,M2,N3,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(S356(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S356)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X128(N0+1:N2,N2+1:M2,N0+1:M1,M1+1:N2))
       X128=0.0d0
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X128,S356, 1.000)
       deallocate(S356)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M2,N3,M2,N3,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder512346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(S357(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S357)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X128,S357,-1.000)
       deallocate(S357)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(S358(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K0*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S358)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X128,S358, 2.000)
       deallocate(S358)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder512346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(S359(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K0*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S359)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X128,S359,-2.000)
       deallocate(S359)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,N2,M2,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(S360(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K0*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S360)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X128,S360, 1.000)
       deallocate(S360)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,N2,M2,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder512346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(S361(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K0*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S361)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X128,S361,-1.000)
       deallocate(S361)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M2,N3,M2,N3,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(S362(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S362)
       deallocate(D1)
       deallocate(F2)
C
       allocate(X129(N0+1:N2,N2+1:M2,N0+1:M1,M1+1:N2))
       X129=0.0d0
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X129,S362, 1.000)
       deallocate(S362)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M2,N3,M2,N3,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder512346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(S363(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S363)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X129,S363,-1.000)
       deallocate(S363)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(S364(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K0*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S364)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X129,S364, 2.000)
       deallocate(S364)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder512346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(S365(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K0*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S365)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X129,S365,-2.000)
       deallocate(S365)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,N2,M2,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(S366(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K0*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S366)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X129,S366, 1.000)
       deallocate(S366)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,N2,M2,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder512346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(S367(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K0*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S367)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X129,S367,-1.000)
       deallocate(S367)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S368(M2+1:N3,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S368)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,M2,N3,M1,N2,X39,S368,-1.000)
       deallocate(S368)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S369(M2+1:N3,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S369)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,M2,N3,M2,N3,M1,N2,X40,S369,-1.000)
       deallocate(S369)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S370(M2+1:N3,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S370)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N2,M2,M2,N3,M1,N2,X41,S370,-1.000)
       deallocate(S370)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S371(M2+1:N3,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S371)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,N2,M2,M2,N3,M1,N2,X42,S371,-1.000)
       deallocate(S371)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1,M1+1:N2))
       call reorder413256(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,N2,M2,M2,N3,N0,M1,M1,N2,t3D,F2)
       allocate(S372(M2+1:N3,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K6
       I3=K0*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S372)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,N0,M1,M1,N2,X124,S372, 1.000)
       deallocate(S372)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1,M1+1:N2))
       call reorder513246(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,M2,N3,N0,M1,M1,N2,t3D,F2)
       allocate(S373(M2+1:N3,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K6
       I3=K0*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S373)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,N0,M1,M1,N2,X124,S373,-1.000)
       deallocate(S373)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,N2,M2,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,M2+1:N3,N0+1:M1,M1+1:N2))
       call reorder423156(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,N2,M2,M2,N3,N0,M1,M1,N2,t3D,F2)
       allocate(S374(M2+1:N3,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K6
       I3=K0*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S374)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,N0,M1,M1,N2,X124,S374,-0.500)
       deallocate(S374)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,N2,M2,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,M2+1:N3,N0+1:M1,M1+1:N2))
       call reorder523146(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,N2,M2,M2,N3,N0,M1,M1,N2,t3D,F2)
       allocate(S375(M2+1:N3,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K6
       I3=K0*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S375)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,N0,M1,M1,N2,X124,S375, 0.500)
       deallocate(S375)
C
       allocate(D2(N0+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,M2,N2,M2,M1,N2,t2C,D2)
       allocate(Z389(N2+1:M2,N2+1:M2,M1+1:N2,M2+1:N3,N0+1:M1,M1+1:N2))
       I1=K8*K5*K6
       I2=K8*K0*K0
       I3=K2
       call EGEMM(I1,I2,I3,X124,D2,Z389)
       deallocate(D2)
C
       call
     & sum236145(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z389,-1.000)
       call
     & sum235146(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z389, 1.000)
       deallocate(Z389)
       deallocate(X124)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S376(N2+1:M2,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S376)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,N2,M2,M1,N2,X89,S376, 1.000)
       deallocate(S376)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S377(N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S377)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,M2,N3,N2,M2,M1,N2,X90,S377, 1.000)
       deallocate(S377)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S378(N2+1:M2,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S378)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N2,M2,N2,M2,M1,N2,X91,S378, 1.000)
       deallocate(S378)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S379(N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S379)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,N2,M2,N2,M2,M1,N2,X92,S379, 1.000)
       deallocate(S379)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S380(N2+1:M2,M1+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S380)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,N2,M2,M1,N2,X111,S380, 1.000)
       deallocate(S380)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z297(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X111,F2,Z297)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z297, 1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z297,-1.000)
       deallocate(Z297)
       deallocate(X111)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S381(N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S381)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,M2,N3,N2,M2,M1,N2,X112,S381, 1.000)
       deallocate(S381)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder512346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z298(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X112,F2,Z298)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z298,-1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z298, 1.000)
       deallocate(Z298)
       deallocate(X112)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S382(N2+1:M2,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S382)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N2,M2,N2,M2,M1,N2,X113,S382, 1.000)
       deallocate(S382)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,M2,N3,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z299(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,X113,F2,Z299)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z299,-1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z299, 1.000)
       deallocate(Z299)
       deallocate(X113)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S383(N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S383)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,N2,M2,N2,M2,M1,N2,X114,S383, 1.000)
       deallocate(S383)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder521346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,M2,N3,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z300(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X114,F2,Z300)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z300, 1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z300,-1.000)
       deallocate(Z300)
       deallocate(X114)
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
       call EGEMM(I1,I2,I3,D1,D2,Q41)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N0,M1,M1,N2,X24,Q41, 0.500)
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
       call EGEMM(I1,I2,I3,D1,D2,Q42)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M1,N2,M1,N2,X25,Q42, 0.500)
       deallocate(Q42)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M2,N3,M2,N3,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(S384(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S384)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X63,S384, 0.500)
       deallocate(S384)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M2,N3,M2,N3,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(S385(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S385)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X63,S385, 0.500)
       deallocate(S385)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(S386(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K0*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S386)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X63,S386, 1.000)
       deallocate(S386)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(S387(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K0*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S387)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X63,S387, 1.000)
       deallocate(S387)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,N2,M2,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(S388(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K0*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S388)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X63,S388, 0.500)
       deallocate(S388)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,N2,M2,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(S389(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K0*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S389)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X63,S389, 0.500)
       deallocate(S389)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M2,N3,M2,N3,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(S390(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S390)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X62,S390, 0.500)
       deallocate(S390)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M2,N3,M2,N3,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(S391(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S391)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X62,S391, 0.500)
       deallocate(S391)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(S392(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K0*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S392)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X62,S392, 1.000)
       deallocate(S392)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(S393(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K0*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S393)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X62,S393, 1.000)
       deallocate(S393)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,N2,M2,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(S394(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K0*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S394)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X62,S394, 0.500)
       deallocate(S394)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,N2,M2,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(S395(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K0*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S395)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X62,S395, 0.500)
       deallocate(S395)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2C,D2)
       allocate(S396(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S396)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,M2,N3,N0,M1,X43,S396,-1.000)
       deallocate(S396)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2C,D2)
       allocate(S397(M2+1:N3,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S397)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,M2,N3,M2,N3,N0,M1,X44,S397,-1.000)
       deallocate(S397)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2C,D2)
       allocate(S398(M2+1:N3,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S398)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N2,M2,M2,N3,N0,M1,X45,S398,-1.000)
       deallocate(S398)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2C,D2)
       allocate(S399(M2+1:N3,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S399)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,N2,M2,M2,N3,N0,M1,X46,S399,-1.000)
       deallocate(S399)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder413256(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,t3D,F2)
       allocate(S400(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K0*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S400)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X1,S400, 1.000)
       deallocate(S400)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder413256(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,M2,N3,M1,N2,M1,N2,t3D,F2)
       allocate(S401(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K0*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S401)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X1,S401, 1.000)
       deallocate(S401)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,N2,M2,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder423156(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,N2,M2,M2,N3,M1,N2,M1,N2,t3D,F2)
       allocate(S402(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K0*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S402)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X1,S402,-0.500)
       deallocate(S402)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,N2,M2,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder423156(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,N2,M2,M2,N3,M1,N2,M1,N2,t3D,F2)
       allocate(S403(M2+1:N3,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K6
       I3=K0*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S403)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,M2,N3,M1,N2,M1,N2,X1,S403,-0.500)
       deallocate(S403)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S404(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S404)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,N2,M2,N0,M1,X97,S404, 1.000)
       deallocate(S404)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S405(N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S405)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,M2,N3,N2,M2,N0,M1,X98,S405, 1.000)
       deallocate(S405)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S406(N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S406)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N2,M2,N2,M2,N0,M1,X99,S406, 1.000)
       deallocate(S406)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S407(N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S407)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,N2,M2,N2,M2,N0,M1,X100,S407, 1.000)
       deallocate(S407)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S408(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S408)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,N2,M2,N0,M1,X115,S408, 1.000)
       deallocate(S408)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z311(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X115,F2,Z311)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z311, 1.000)
       deallocate(Z311)
       deallocate(X115)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S409(N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S409)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,M2,N3,N2,M2,N0,M1,X116,S409, 1.000)
       deallocate(S409)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z312(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X116,F2,Z312)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z312, 1.000)
       deallocate(Z312)
       deallocate(X116)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S410(N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S410)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N2,M2,N2,M2,N0,M1,X117,S410, 1.000)
       deallocate(S410)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z313(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,X117,F2,Z313)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z313,-1.000)
       deallocate(Z313)
       deallocate(X117)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S411(N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S411)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,N2,M2,N2,M2,N0,M1,X118,S411, 1.000)
       deallocate(S411)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z314(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X118,F2,Z314)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z314,-1.000)
       deallocate(Z314)
       deallocate(X118)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,M1,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,M1,t2C,D2)
       allocate(Q43(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K4*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q43)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N0,M1,N0,M1,X26,Q43, 0.500)
       deallocate(Q43)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,M1,N2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,M1,t2C,D2)
       allocate(Q44(N0+1:M1,M1+1:N2))
       I1=K8
       I2=K5
       I3=K4*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q44)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M1,N2,N0,M1,X27,Q44, 0.500)
       deallocate(Q44)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,N2,M2,t2C,D2)
       allocate(S412(M2+1:N3,N2+1:M2,M2+1:N3,M2+1:N3))
       I1=K6*K6
       I2=K0*K6
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S412)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X47(M2+1:N3,M2+1:N3,M2+1:N3,N2+1:M2))
       X47=0.0d0
       call
     & sum3412(M2,N3,M2,N3,M2,N3,N2,M2,X47,S412, 0.500)
       deallocate(S412)
C
       call sumx1234(N2,N3,N2,N3,N2,N3,N2,M2,
     & M2,N3,M2,N3,M2,N3,N2,M2,X47,VCAPPP, 1.000)
C
       allocate(F2(M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder123456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,M1,N2,t3D,F2)
       allocate(Z56(N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K8*K8*K5*K0
       I3=K6*K6
       call EGEMM(I1,I2,I3,X47,F2,Z56)
       deallocate(F2)
C
       call
     & sum345612(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z56, 0.500)
       call
     & sum245613(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z56,-0.500)
       deallocate(Z56)
       deallocate(X47)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,N2,M2,t2C,D2)
       allocate(S413(M2+1:N3,N2+1:M2,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K0*K6
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S413)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X48(M2+1:N3,N2+1:M2,M2+1:N3,N2+1:M2))
       X48=0.0d0
       call
     & sum3412(M2,N3,N2,M2,M2,N3,N2,M2,X48,S413, 0.500)
       deallocate(S413)
C
       call sumx1234(N2,N3,N2,N3,N2,N3,N2,M2,
     & M2,N3,N2,M2,M2,N3,N2,M2,X48,VCAPPP, 1.000)
C
       allocate(F2(M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder123456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,t3D,F2)
       allocate(Z57(N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K8*K8*K5*K0
       I3=K0*K6
       call EGEMM(I1,I2,I3,X48,F2,Z57)
       deallocate(F2)
C
       call
     & sum345612(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z57, 1.000)
       call
     & sum245613(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z57,-1.000)
       deallocate(Z57)
       deallocate(X48)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,M2,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,N2,M2,t2C,D2)
       allocate(S414(M2+1:N3,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0
       I2=K0*K6
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S414)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X49(N2+1:M2,N2+1:M2,M2+1:N3,N2+1:M2))
       X49=0.0d0
       call
     & sum3412(N2,M2,N2,M2,M2,N3,N2,M2,X49,S414, 0.500)
       deallocate(S414)
C
       call sumx1234(N2,N3,N2,N3,N2,N3,N2,M2,
     & N2,M2,N2,M2,M2,N3,N2,M2,X49,VCAPPP, 1.000)
C
       allocate(F2(N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder123456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N2,M2,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,t3D,F2)
       allocate(Z58(N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K8*K8*K5*K0
       I3=K0*K0
       call EGEMM(I1,I2,I3,X49,F2,Z58)
       deallocate(F2)
C
       call
     & sum345612(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z58, 0.500)
       call
     & sum245613(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z58,-0.500)
       deallocate(Z58)
       deallocate(X49)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,M2,N3,t2C,D2)
       allocate(Q45(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,Q45)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M2,N3,M2,N3,X28,Q45,-0.500)
       deallocate(Q45)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,M2,N3,t2C,D2)
       allocate(Q46(M2+1:N3,N2+1:M2))
       I1=K0
       I2=K6
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,Q46)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N2,M2,M2,N3,X29,Q46,-0.500)
       deallocate(Q46)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,M2,N2,M2,t2C,D2)
       allocate(S415(N2+1:M2,N2+1:M2,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K0*K0
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S415)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M2,N3,N2,M2,N2,M2,N2,M2,X50,S415, 0.500)
       deallocate(S415)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,M2,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,M2,N2,M2,t2C,D2)
       allocate(S416(N2+1:M2,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0
       I2=K0*K0
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S416)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,M2,N2,M2,N2,M2,N2,M2,X51,S416, 0.500)
       deallocate(S416)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,M2,t2C,D2)
       allocate(Q47(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,Q47)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M2,N3,N2,M2,X30,Q47,-0.500)
       deallocate(Q47)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,M2,t2C,D2)
       allocate(Q48(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K4*K2*K2
       call EGEMM(I1,I2,I3,D1,D2,Q48)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N2,M2,N2,M2,X31,Q48,-0.500)
       deallocate(Q48)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
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
     & sum21(M2,N3,N2,M2,X60,Q49, 0.500)
       deallocate(Q49)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
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
     & sum21(N2,M2,N2,M2,X61,Q50, 0.500)
       deallocate(Q50)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,N2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q51(N0+1:M1,N2+1:N3))
       I1=K4*K5
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q51)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:M1))
       call reorder21(N0,M1,N2,N3,
     & N2,N3,N0,M1,Q51,B1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(Q55(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q55)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(N0,M1,N0,M1,X54,Q55, 1.000)
       deallocate(Q55)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z63(M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8*K8*K0*K0*K6
       I3=K5
       call EGEMM(I1,I2,I3,X54,F2,Z63)
       deallocate(F2)
C
       call
     & sum123564(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z63,-1.000)
       deallocate(Z63)
       deallocate(X54)
C
       allocate(B1(N2+1:N3,N0+1:M1))
       call reorder21(N0,M1,N2,N3,
     & N2,N3,N0,M1,Q51,B1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(Q52(M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q52)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(N0,M1,M1,N2,X52,Q52, 1.000)
       deallocate(Q52)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z61(M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8*K5*K0*K0*K6
       I3=K5
       call EGEMM(I1,I2,I3,X52,F2,Z61)
       deallocate(F2)
C
       V3D=V3D-Z61
       call
     & sum123465(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z61, 1.000)
       deallocate(Z61)
       deallocate(X52)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,N2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q53(M1+1:N2,N2+1:N3))
       I1=K4*K8
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q53)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,M1+1:N2))
       call reorder21(M1,N2,N2,N3,
     & N2,N3,M1,N2,Q53,B1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(Q56(N0+1:M1,M1+1:N2))
       I1=K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q56)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(M1,N2,N0,M1,X55,Q56, 1.000)
       deallocate(Q56)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z64(M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8*K8*K0*K0*K6
       I3=K8
       call EGEMM(I1,I2,I3,X55,F2,Z64)
       deallocate(F2)
C
       call
     & sum123564(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z64,-1.000)
       deallocate(Z64)
       deallocate(X55)
C
       allocate(B1(N2+1:N3,M1+1:N2))
       call reorder21(M1,N2,N2,N3,
     & N2,N3,M1,N2,Q53,B1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(Q54(M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q54)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(M1,N2,M1,N2,X53,Q54, 1.000)
       deallocate(Q54)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder512346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z62(M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8*K5*K0*K0*K6
       I3=K8
       call EGEMM(I1,I2,I3,X53,F2,Z62)
       deallocate(F2)
C
       V3D=V3D+Z62
       call
     & sum123465(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z62,-1.000)
       deallocate(Z62)
       deallocate(X53)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,M2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q57(N0+1:N2,M2+1:N3))
       I1=K6*K2
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q57)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(Q61(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q57,B2,Q61)
       deallocate(B2)
C
       call
     & sum21(M2,N3,N2,M2,X58,Q61,-1.000)
       deallocate(Q61)
C
       allocate(F2(M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder123456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,M1,N2,t3D,F2)
       allocate(Z67(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K8*K5*K0*K6
       I3=K6
       call EGEMM(I1,I2,I3,X58,F2,Z67)
       deallocate(F2)
C
       call
     & sum134562(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z67,-1.000)
       deallocate(Z67)
       deallocate(X58)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(Q63(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q57,B2,Q63)
       deallocate(B2)
C
       call
     & sum21(M2,N3,N2,M2,X60,Q63,-1.000)
       deallocate(Q63)
C
       allocate(F2(M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder123456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,M1,N2,t3D,F2)
       allocate(Z69(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K8*K5*K0*K6
       I3=K6
       call EGEMM(I1,I2,I3,X60,F2,Z69)
       deallocate(F2)
C
       call
     & sum124563(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z69, 1.000)
       deallocate(Z69)
       deallocate(X60)
C
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(Q58(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,Q57,B2,Q58)
       deallocate(B2)
C
       call
     & sum21(M2,N3,M2,N3,X56,Q58,-1.000)
       deallocate(Q58)
C
       allocate(F2(M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder123456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,t3D,F2)
       allocate(Z65(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6
       I2=K8*K8*K5*K0*K0
       I3=K6
       call EGEMM(I1,I2,I3,X56,F2,Z65)
       deallocate(F2)
C
       call
     & sum234561(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z65, 1.000)
       deallocate(Z65)
       deallocate(X56)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N2,M2,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q59(N0+1:N2,N2+1:M2))
       I1=K0*K2
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q59)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(Q60(M2+1:N3,N2+1:M2))
       I1=K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,Q59,B2,Q60)
       deallocate(B2)
C
       call
     & sum21(N2,M2,M2,N3,X57,Q60,-1.000)
       deallocate(Q60)
C
       allocate(F2(N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder123456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N2,M2,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,t3D,F2)
       allocate(Z66(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6
       I2=K8*K8*K5*K0*K0
       I3=K0
       call EGEMM(I1,I2,I3,X57,F2,Z66)
       deallocate(F2)
C
       call
     & sum234561(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z66, 1.000)
       deallocate(Z66)
       deallocate(X57)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(Q64(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q59,B2,Q64)
       deallocate(B2)
C
       call
     & sum21(N2,M2,N2,M2,X61,Q64,-1.000)
       deallocate(Q64)
C
       allocate(F2(N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder213456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N2,M2,M2,N3,N2,M2,N0,M1,M1,N2,M1,N2,t3D,F2)
       allocate(Z70(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K8*K5*K0*K6
       I3=K0
       call EGEMM(I1,I2,I3,X61,F2,Z70)
       deallocate(F2)
C
       call
     & sum124563(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z70,-1.000)
       deallocate(Z70)
       deallocate(X61)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(Q62(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q59,B2,Q62)
       deallocate(B2)
C
       call
     & sum21(N2,M2,N2,M2,X59,Q62,-1.000)
       deallocate(Q62)
C
       allocate(F2(N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder213456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N2,M2,M2,N3,N2,M2,N0,M1,M1,N2,M1,N2,t3D,F2)
       allocate(Z68(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K8*K5*K0*K6
       I3=K0
       call EGEMM(I1,I2,I3,X59,F2,Z68)
       deallocate(F2)
C
       call
     & sum134562(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z68, 1.000)
       deallocate(Z68)
       deallocate(X59)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q65(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q65)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q65,B1)
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,M1,N2,M1,N2,t2C,D2)
       allocate(S418(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S418)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X2,S418,-1.000)
       deallocate(S418)
C
       allocate(D2(N0+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,M2,N2,M2,M1,N2,t2C,D2)
       allocate(S419(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K2
       call EGEMM(I1,I2,I3,Q65,D2,S419)
       deallocate(D2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X119,S419, 1.000)
       deallocate(S419)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q65,B1)
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,N0,M1,M1,N2,t2C,D2)
       allocate(S420(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S420)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X4,S420,-1.000)
       deallocate(S420)
C
       allocate(D2(N0+1:N2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,M2,N2,M2,N0,M1,t2C,D2)
       allocate(S417(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K2
       call EGEMM(I1,I2,I3,Q65,D2,S417)
       deallocate(D2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,N0,M1,X7,S417,-1.000)
       deallocate(S417)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,M1+1:N2))
       call reorder1342(N2,N3,N0,N2,N0,N2,N0,N2,
     & N2,N3,N0,N2,N0,N2,M1,N2,VCHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S421(M1+1:N2,N0+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S421)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder2314(M1,N2,N0,N2,N0,N2,M1,N2,
     & N0,N2,N0,N2,M1,N2,M1,N2,S421,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S423(N2+1:M2,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S423)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,N2,M2,M1,N2,M1,N2,X65,S423,-1.000)
       deallocate(S423)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder3214(M1,N2,N0,N2,N0,N2,M1,N2,
     & N0,N2,N0,N2,M1,N2,M1,N2,S421,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S424(N2+1:M2,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S424)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,N2,M2,M1,N2,M1,N2,X66,S424, 1.000)
       deallocate(S424)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder2314(M1,N2,N0,N2,N0,N2,M1,N2,
     & N0,N2,N0,N2,M1,N2,M1,N2,S421,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S422(M2+1:N3,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S422)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,M2,N3,M1,N2,M1,N2,X64,S422,-1.000)
       deallocate(S422)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,M1+1:N2))
       call reorder1342(N2,N3,N0,N2,N0,N2,N0,N2,
     & N2,N3,N0,N2,N0,N2,M1,N2,VCHHHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S425(N0+1:M1,N0+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S425)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder2314(N0,M1,N0,N2,N0,N2,M1,N2,
     & N0,N2,N0,N2,N0,M1,M1,N2,S425,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S427(N2+1:M2,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S427)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,N2,M2,N0,M1,M1,N2,X67,S427, 1.000)
       deallocate(S427)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder3214(N0,M1,N0,N2,N0,N2,M1,N2,
     & N0,N2,N0,N2,N0,M1,M1,N2,S425,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S428(N2+1:M2,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S428)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,N2,M2,N0,M1,M1,N2,X68,S428, 1.000)
       deallocate(S428)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder2314(N0,M1,N0,N2,N0,N2,M1,N2,
     & N0,N2,N0,N2,N0,M1,M1,N2,S425,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S426(M2+1:N3,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S426)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,M2,N3,N0,M1,M1,N2,X3,S426,-1.000)
       deallocate(S426)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder4312(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S429(N2+1:M2,N0+1:N2,N2+1:N3,M1+1:N2))
       I1=K8*K4*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S429)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder2314(N2,M2,N0,N2,N2,N3,M1,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,S429,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S433(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S433)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,N3,N2,M2,N2,M2,M1,N2,X5,S433,-1.000)
       deallocate(S433)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder2314(N2,M2,N0,N2,N2,N3,M1,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,S429,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S430(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S430)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,M1,N2,X69,S430,-1.000)
       deallocate(S430)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder4312(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S431(N2+1:M2,N0+1:N2,N2+1:N3,M1+1:N2))
       I1=K8*K4*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S431)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder2314(N2,M2,N0,N2,N2,N3,M1,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,S431,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S432(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S432)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,M1,N2,X70,S432,-1.000)
       deallocate(S432)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder2413(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,N2,N2,N3,M2,N3,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S434(M1+1:N2,N0+1:N2,N2+1:N3,M2+1:N3))
       I1=K6*K4*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S434)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N0+1:N2,M2+1:N3,M1+1:N2))
       call reorder3241(M1,N2,N0,N2,N2,N3,M2,N3,
     & N2,N3,N0,N2,M2,N3,M1,N2,S434,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S444(N0+1:M1,N0+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S444)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N2,M2,N3,N0,M1,M1,N2,X3,S444, 1.000)
       deallocate(S444)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N2,N3,M2,N3,
     & N0,N2,N2,N3,M2,N3,M1,N2,S434,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S447(N2+1:M2,N2+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S447)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,N3,M2,N3,N2,M2,M1,N2,X69,S447, 1.000)
       deallocate(S447)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N2,N3,M2,N3,
     & N0,N2,N2,N3,M2,N3,M1,N2,S434,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S448(N2+1:M2,N2+1:N3,M2+1:N3,M1+1:N2))
       I1=K8*K6*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S448)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,N3,M2,N3,N2,M2,M1,N2,X70,S448, 1.000)
       deallocate(S448)
C
       allocate(D1(N2+1:N3,N0+1:N2,M2+1:N3,M1+1:N2))
       call reorder3241(M1,N2,N0,N2,N2,N3,M2,N3,
     & N2,N3,N0,N2,M2,N3,M1,N2,S434,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S435(M1+1:N2,N0+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S435)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N2,M2,N3,M1,N2,M1,N2,X1,S435, 1.000)
       deallocate(S435)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:M1))
       call reorder1342(N2,N3,N0,N2,N0,N2,N0,N2,
     & N2,N3,N0,N2,N0,N2,N0,M1,VCHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S440(M1+1:N2,N0+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S440)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder3241(M1,N2,N0,N2,N0,N2,N0,M1,
     & N0,N2,N0,N2,N0,M1,M1,N2,S440,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S443(N2+1:M2,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S443)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,N2,M2,N0,M1,M1,N2,X68,S443,-1.000)
       deallocate(S443)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,N2,N0,M1,
     & N0,N2,N0,N2,N0,M1,M1,N2,S440,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S441(M2+1:N3,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S441)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,M2,N3,N0,M1,M1,N2,X3,S441, 1.000)
       deallocate(S441)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,N2,N0,M1,
     & N0,N2,N0,N2,N0,M1,M1,N2,S440,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S442(N2+1:M2,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S442)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,N2,M2,N0,M1,M1,N2,X67,S442,-1.000)
       deallocate(S442)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder2413(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,N2,N2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S436(M1+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S436)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:M2,M1+1:N2))
       call reorder3241(M1,N2,N0,N2,N2,N3,N2,M2,
     & N2,N3,N0,N2,N2,M2,M1,N2,S436,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S437(M1+1:N2,N0+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S437)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N2,N2,M2,M1,N2,M1,N2,X62,S437,-1.000)
       deallocate(S437)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N2,N3,N2,M2,
     & N0,N2,N2,N3,N2,M2,M1,N2,S436,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S449(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S449)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,M1,N2,X69,S449,-1.000)
       deallocate(S449)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N2,N3,N2,M2,
     & N0,N2,N2,N3,N2,M2,M1,N2,S436,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S451(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S451)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,N3,N2,M2,N2,M2,M1,N2,X5,S451,-1.000)
       deallocate(S451)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:M2,M1+1:N2))
       call reorder3241(M1,N2,N0,N2,N2,N3,N2,M2,
     & N2,N3,N0,N2,N2,M2,M1,N2,S436,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S445(N0+1:M1,N0+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S445)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N2,N2,M2,N0,M1,M1,N2,X67,S445,-1.000)
       deallocate(S445)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder2413(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,N2,N2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S438(M1+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S438)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N2,N3,N2,M2,
     & N0,N2,N2,N3,N2,M2,M1,N2,S438,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S450(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S450)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,M1,N2,X70,S450,-1.000)
       deallocate(S450)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:M2,M1+1:N2))
       call reorder3241(M1,N2,N0,N2,N2,N3,N2,M2,
     & N2,N3,N0,N2,N2,M2,M1,N2,S438,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S446(N0+1:M1,N0+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S446)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N2,N2,M2,N0,M1,M1,N2,X68,S446, 1.000)
       deallocate(S446)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N2,N3,N2,M2,
     & N0,N2,N2,N3,N2,M2,M1,N2,S438,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S452(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S452)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,N2,M2,M1,N2,X5,S452, 1.000)
       deallocate(S452)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:M2,M1+1:N2))
       call reorder3241(M1,N2,N0,N2,N2,N3,N2,M2,
     & N2,N3,N0,N2,N2,M2,M1,N2,S438,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S439(M1+1:N2,N0+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S439)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N2,N2,M2,M1,N2,M1,N2,X63,S439, 1.000)
       deallocate(S439)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:M1))
       call reorder4312(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N0,M1,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S453(N2+1:M2,N0+1:N2,N2+1:N3,N0+1:M1))
       I1=K5*K4*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S453)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder2314(N2,M2,N0,N2,N2,N3,N0,M1,
     & N0,N2,N2,N3,N2,M2,N0,M1,S453,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S457(N2+1:M2,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S457)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,N3,N2,M2,N2,M2,N0,M1,X7,S457,-1.000)
       deallocate(S457)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder2314(N2,M2,N0,N2,N2,N3,N0,M1,
     & N0,N2,N2,N3,N2,M2,N0,M1,S453,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S454(M2+1:N3,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S454)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,N0,M1,X71,S454,-1.000)
       deallocate(S454)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:M1))
       call reorder4312(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N0,M1,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S455(N2+1:M2,N0+1:N2,N2+1:N3,N0+1:M1))
       I1=K5*K4*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S455)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder2314(N2,M2,N0,N2,N2,N3,N0,M1,
     & N0,N2,N2,N3,N2,M2,N0,M1,S455,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S456(M2+1:N3,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S456)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,N0,M1,X72,S456,-1.000)
       deallocate(S456)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,N2,N2,N3,M2,N3,VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S458(N0+1:M1,N0+1:N2,N2+1:N3,M2+1:N3))
       I1=K6*K4*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S458)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N2,N3,M2,N3,
     & N0,N2,N2,N3,M2,N3,N0,M1,S458,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S460(N2+1:M2,N2+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S460)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,N3,M2,N3,N2,M2,N0,M1,X72,S460,-1.000)
       deallocate(S460)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N2,N3,M2,N3,
     & N0,N2,N2,N3,M2,N3,N0,M1,S458,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S459(N2+1:M2,N2+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S459)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,N3,M2,N3,N2,M2,N0,M1,X71,S459,-1.000)
       deallocate(S459)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,N2,N2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S461(N0+1:M1,N0+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S461)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N2,N3,N2,M2,
     & N0,N2,N2,N3,N2,M2,N0,M1,S461,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S465(N2+1:M2,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S465)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,N3,N2,M2,N2,M2,N0,M1,X7,S465, 1.000)
       deallocate(S465)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N2,N3,N2,M2,
     & N0,N2,N2,N3,N2,M2,N0,M1,S461,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S462(M2+1:N3,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S462)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,N0,M1,X71,S462, 1.000)
       deallocate(S462)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,N2,N2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S463(N0+1:M1,N0+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S463)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N2,N3,N2,M2,
     & N0,N2,N2,N3,N2,M2,N0,M1,S463,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S466(N2+1:M2,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S466)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,N2,M2,N0,M1,X7,S466,-1.000)
       deallocate(S466)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N2,N3,N2,M2,
     & N0,N2,N2,N3,N2,M2,N0,M1,S463,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S464(M2+1:N3,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S464)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,N0,M1,X72,S464, 1.000)
       deallocate(S464)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,M2+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,N2,N0,M1,M2,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S467(M1+1:N2,N0+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S467)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,M1,M2,N3,
     & N0,N2,N0,M1,M2,N3,M1,N2,S467,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S475(N2+1:M2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S475)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,N2,M2,M1,N2,X73,S475, 1.000)
       deallocate(S475)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z108(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X73,F2,Z108)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z108, 1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z108,-1.000)
       deallocate(Z108)
       deallocate(X73)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,M1,M2,N3,
     & N0,N2,N0,M1,M2,N3,M1,N2,S467,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S479(N2+1:M2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S479)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,N2,M2,M1,N2,X77,S479, 1.000)
       deallocate(S479)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z112(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X77,F2,Z112)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z112,-1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z112, 1.000)
       deallocate(Z112)
       deallocate(X77)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,M1,M2,N3,
     & N0,N2,N0,M1,M2,N3,M1,N2,S467,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S468(M2+1:N3,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S468)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,M2,N3,M1,N2,X8,S468,-1.000)
       deallocate(S468)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,M2,N3,M2,N3,M1,N2,X8,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z9(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K8*K5*K0*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,X8,F2,Z9)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z9, 1.000)
       call
     & sum234615(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z9,-1.000)
       deallocate(Z9)
       deallocate(X8)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,N2,M1,N1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S473(M1+1:N2,N0+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S473)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,M1,N1,N1,M2,
     & N0,N2,M1,N1,N1,M2,M1,N2,S473,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S478(N2+1:M2,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S478)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,N1,M2,N2,M2,M1,N2,X76,S478, 1.000)
       deallocate(S478)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N1,M2,M2,N3,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z111(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X76,F2,Z111)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z111, 1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z111,-1.000)
       deallocate(Z111)
       deallocate(X76)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,M1,N1,N1,M2,
     & N0,N2,M1,N1,N1,M2,M1,N2,S473,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S482(N2+1:M2,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S482)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,N1,M2,N2,M2,M1,N2,X80,S482, 1.000)
       deallocate(S482)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N1,M2,M2,N3,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z115(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X80,F2,Z115)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z115,-1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z115, 1.000)
       deallocate(Z115)
       deallocate(X80)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,M1,N1,N1,M2,
     & N0,N2,M1,N1,N1,M2,M1,N2,S473,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S474(M2+1:N3,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S474)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,N1,M2,M2,N3,M1,N2,X11,S474,-1.000)
       deallocate(S474)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,N1,M2,M2,N3,M1,N2,X11,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N1,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z12(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K8*K5*K0*K0
       I3=K9*K7
       call EGEMM(I1,I2,I3,X11,F2,Z12)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z12, 1.000)
       call
     & sum234615(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z12,-1.000)
       deallocate(Z12)
       deallocate(X11)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,N2,N0,M1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S471(M1+1:N2,N0+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S471)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,M1,N1,M2,
     & N0,N2,N0,M1,N1,M2,M1,N2,S471,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S472(M2+1:N3,N0+1:M1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S472)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N1,M2,M2,N3,M1,N2,X10,S472,-1.000)
       deallocate(S472)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,N1,M2,M2,N3,M1,N2,X10,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N1,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z11(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K8*K5*K0*K0
       I3=K9*K5
       call EGEMM(I1,I2,I3,X10,F2,Z11)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z11, 1.000)
       call
     & sum234615(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z11,-1.000)
       deallocate(Z11)
       deallocate(X10)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,M1,N1,M2,
     & N0,N2,N0,M1,N1,M2,M1,N2,S471,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S481(N2+1:M2,N0+1:M1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S481)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N1,M2,N2,M2,M1,N2,X79,S481, 1.000)
       deallocate(S481)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N1,M2,M2,N3,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z114(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X79,F2,Z114)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z114,-1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z114, 1.000)
       deallocate(Z114)
       deallocate(X79)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,M1,N1,M2,
     & N0,N2,N0,M1,N1,M2,M1,N2,S471,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S477(N2+1:M2,N0+1:M1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S477)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N1,M2,N2,M2,M1,N2,X75,S477, 1.000)
       deallocate(S477)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N1,M2,M2,N3,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z110(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X75,F2,Z110)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z110, 1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z110,-1.000)
       deallocate(Z110)
       deallocate(X75)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N1,M2+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,N2,M1,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S469(M1+1:N2,N0+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S469)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,M1,N1,M2,N3,
     & N0,N2,M1,N1,M2,N3,M1,N2,S469,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S470(M2+1:N3,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S470)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,M2,N3,M2,N3,M1,N2,X9,S470,-1.000)
       deallocate(S470)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,M2,N3,M2,N3,M1,N2,X9,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z10(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K8*K5*K0*K0
       I3=K6*K7
       call EGEMM(I1,I2,I3,X9,F2,Z10)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z10, 1.000)
       call
     & sum234615(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z10,-1.000)
       deallocate(Z10)
       deallocate(X9)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,M1,N1,M2,N3,
     & N0,N2,M1,N1,M2,N3,M1,N2,S469,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S476(N2+1:M2,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S476)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,M2,N3,N2,M2,M1,N2,X74,S476, 1.000)
       deallocate(S476)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z109(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X74,F2,Z109)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z109, 1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z109,-1.000)
       deallocate(Z109)
       deallocate(X74)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,M1,N1,M2,N3,
     & N0,N2,M1,N1,M2,N3,M1,N2,S469,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S480(N2+1:M2,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S480)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,M2,N3,N2,M2,M1,N2,X78,S480, 1.000)
       deallocate(S480)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z113(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X78,F2,Z113)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z113,-1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z113, 1.000)
       deallocate(Z113)
       deallocate(X78)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,M2+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,N2,N0,M1,M2,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S483(N0+1:M1,N0+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S483)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N0,M1,M2,N3,
     & N0,N2,N0,M1,M2,N3,N0,M1,S483,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S491(N2+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S491)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,N2,M2,N0,M1,X81,S491, 1.000)
       deallocate(S491)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z132(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X81,F2,Z132)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z132, 1.000)
       deallocate(Z132)
       deallocate(X81)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N0,M1,M2,N3,
     & N0,N2,N0,M1,M2,N3,N0,M1,S483,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S495(N2+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S495)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,N2,M2,N0,M1,X85,S495, 1.000)
       deallocate(S495)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z136(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X85,F2,Z136)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z136,-1.000)
       deallocate(Z136)
       deallocate(X85)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N0,M1,M2,N3,
     & N0,N2,N0,M1,M2,N3,N0,M1,S483,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S484(M2+1:N3,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S484)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,M2,N3,N0,M1,X16,S484,-1.000)
       deallocate(S484)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,M1,X16,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z17(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,X16,F2,Z17)
       deallocate(F2)
C
       call
     & sum235614(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z17, 1.000)
       deallocate(Z17)
       deallocate(X16)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,N2,M1,N1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S489(N0+1:M1,N0+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S489)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,M1,N1,N1,M2,
     & N0,N2,M1,N1,N1,M2,N0,M1,S489,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S494(N2+1:M2,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S494)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,N1,M2,N2,M2,N0,M1,X84,S494, 1.000)
       deallocate(S494)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N1,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z135(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X84,F2,Z135)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z135, 1.000)
       deallocate(Z135)
       deallocate(X84)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,M1,N1,N1,M2,
     & N0,N2,M1,N1,N1,M2,N0,M1,S489,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S498(N2+1:M2,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S498)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,N1,M2,N2,M2,N0,M1,X88,S498, 1.000)
       deallocate(S498)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N1,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z139(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X88,F2,Z139)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z139,-1.000)
       deallocate(Z139)
       deallocate(X88)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,M1,N1,N1,M2,
     & N0,N2,M1,N1,N1,M2,N0,M1,S489,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S490(M2+1:N3,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S490)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,N1,M2,M2,N3,N0,M1,X19,S490,-1.000)
       deallocate(S490)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,N1,M2,M2,N3,N0,M1,X19,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N1,M2,N2,M2,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z20(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K0
       I3=K9*K7
       call EGEMM(I1,I2,I3,X19,F2,Z20)
       deallocate(F2)
C
       call
     & sum235614(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z20, 1.000)
       deallocate(Z20)
       deallocate(X19)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,N2,N0,M1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S487(N0+1:M1,N0+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S487)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N0,M1,N1,M2,
     & N0,N2,N0,M1,N1,M2,N0,M1,S487,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S488(M2+1:N3,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S488)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N1,M2,M2,N3,N0,M1,X18,S488,-1.000)
       deallocate(S488)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,N1,M2,M2,N3,N0,M1,X18,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N1,M2,N2,M2,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z19(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K0
       I3=K9*K5
       call EGEMM(I1,I2,I3,X18,F2,Z19)
       deallocate(F2)
C
       call
     & sum235614(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z19, 1.000)
       deallocate(Z19)
       deallocate(X18)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N0,M1,N1,M2,
     & N0,N2,N0,M1,N1,M2,N0,M1,S487,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S497(N2+1:M2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S497)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N1,M2,N2,M2,N0,M1,X87,S497, 1.000)
       deallocate(S497)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N1,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z138(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X87,F2,Z138)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z138,-1.000)
       deallocate(Z138)
       deallocate(X87)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N0,M1,N1,M2,
     & N0,N2,N0,M1,N1,M2,N0,M1,S487,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S493(N2+1:M2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S493)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N1,M2,N2,M2,N0,M1,X83,S493, 1.000)
       deallocate(S493)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N1,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z134(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X83,F2,Z134)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z134, 1.000)
       deallocate(Z134)
       deallocate(X83)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N1,M2+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,N2,M1,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S485(N0+1:M1,N0+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S485)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,M1,N1,M2,N3,
     & N0,N2,M1,N1,M2,N3,N0,M1,S485,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S486(M2+1:N3,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S486)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,M2,N3,M2,N3,N0,M1,X17,S486,-1.000)
       deallocate(S486)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,M2,N3,M2,N3,N0,M1,X17,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z18(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K0
       I3=K6*K7
       call EGEMM(I1,I2,I3,X17,F2,Z18)
       deallocate(F2)
C
       call
     & sum235614(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z18, 1.000)
       deallocate(Z18)
       deallocate(X17)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,M1,N1,M2,N3,
     & N0,N2,M1,N1,M2,N3,N0,M1,S485,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S492(N2+1:M2,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S492)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,M2,N3,N2,M2,N0,M1,X82,S492, 1.000)
       deallocate(S492)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z133(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X82,F2,Z133)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z133, 1.000)
       deallocate(Z133)
       deallocate(X82)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,M1,N1,M2,N3,
     & N0,N2,M1,N1,M2,N3,N0,M1,S485,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S496(N2+1:M2,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S496)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,M2,N3,N2,M2,N0,M1,X86,S496, 1.000)
       deallocate(S496)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3C1,F2)
       allocate(Z137(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X86,F2,Z137)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z137,-1.000)
       deallocate(Z137)
       deallocate(X86)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,N2+1:N3))
       call reorder2341(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,M1,M1,N2,N2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S499(M1+1:N2,N0+1:M1,M1+1:N2,N2+1:N3))
       I1=K4*K8*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S499)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder4231(M1,N2,N0,M1,M1,N2,N2,N3,
     & N2,N3,N0,M1,M1,N2,M1,N2,S499,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S505(N0+1:M1,N0+1:M1,M1+1:N2,M1+1:N2))
       I1=K8*K8*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S505)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M1,N2,N0,M1,M1,N2,X37,S505, 1.000)
       deallocate(S505)
C
       call sumx3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,M1,M1,N2,N0,M1,M1,N2,X37,VCHHHH, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M1,N2,M2,N3,N2,M2,N2,M2,M1,N2,t3D,F2)
       allocate(Z38(M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5
       I2=K8*K0*K0*K6
       I3=K8*K5
       call EGEMM(I1,I2,I3,X37,F2,Z38)
       deallocate(F2)
C
       call
     & sum123546(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z38,-1.000)
       call
     & sum123645(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z38, 1.000)
       deallocate(Z38)
       deallocate(X37)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder4231(M1,N2,N0,M1,M1,N2,N2,N3,
     & N2,N3,N0,M1,M1,N2,M1,N2,S499,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S500(M1+1:N2,N0+1:M1,M1+1:N2,M1+1:N2))
       I1=K8*K8*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S500)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M1,N2,M1,N2,M1,N2,X34,S500, 1.000)
       deallocate(S500)
C
       call sumx3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,M1,M1,N2,M1,N2,M1,N2,X34,VCHHHH, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M1,N2,M2,N3,N2,M2,N2,M2,N0,M1,t3D,F2)
       allocate(Z35(M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2))
       I1=K8*K8
       I2=K5*K0*K0*K6
       I3=K8*K5
       call EGEMM(I1,I2,I3,X34,F2,Z35)
       deallocate(F2)
C
       V3D=V3D-Z35
       deallocate(Z35)
       deallocate(X34)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:M1,N2+1:N3))
       call reorder2341(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,M1,N0,M1,N2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S503(M1+1:N2,N0+1:M1,N0+1:M1,N2+1:N3))
       I1=K4*K5*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S503)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:M1,M1+1:N2))
       call reorder4231(M1,N2,N0,M1,N0,M1,N2,N3,
     & N2,N3,N0,M1,N0,M1,M1,N2,S503,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S504(N0+1:M1,N0+1:M1,N0+1:M1,M1+1:N2))
       I1=K8*K5*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S504)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N0,M1,N0,M1,M1,N2,X36,S504, 1.000)
       deallocate(S504)
C
       call sumx3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,M1,N0,M1,N0,M1,M1,N2,X36,VCHHHH, 1.000)
C
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N0,M1,M2,N3,N2,M2,N2,M2,M1,N2,t3D,F2)
       allocate(Z37(M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5
       I2=K8*K0*K0*K6
       I3=K5*K5
       call EGEMM(I1,I2,I3,X36,F2,Z37)
       deallocate(F2)
C
       call
     & sum123546(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z37,-0.500)
       call
     & sum123645(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z37, 0.500)
       deallocate(Z37)
       deallocate(X36)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,N2+1:N3))
       call reorder2341(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,M1,N2,M1,N2,N2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S501(M1+1:N2,M1+1:N2,M1+1:N2,N2+1:N3))
       I1=K4*K8*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S501)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder4231(M1,N2,M1,N2,M1,N2,N2,N3,
     & N2,N3,M1,N2,M1,N2,M1,N2,S501,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S506(N0+1:M1,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S506)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M1,N2,N0,M1,M1,N2,X38,S506, 1.000)
       deallocate(S506)
C
       call sumx3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & M1,N2,M1,N2,N0,M1,M1,N2,X38,VCHHHH, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M1,N2,M2,N3,N2,M2,N2,M2,M1,N2,t3D,F2)
       allocate(Z39(M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5
       I2=K8*K0*K0*K6
       I3=K8*K8
       call EGEMM(I1,I2,I3,X38,F2,Z39)
       deallocate(F2)
C
       call
     & sum123546(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z39,-0.500)
       call
     & sum123645(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z39, 0.500)
       deallocate(Z39)
       deallocate(X38)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder4231(M1,N2,M1,N2,M1,N2,N2,N3,
     & N2,N3,M1,N2,M1,N2,M1,N2,S501,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S502(M1+1:N2,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S502)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M1,N2,M1,N2,M1,N2,X35,S502, 1.000)
       deallocate(S502)
C
       call sumx3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & M1,N2,M1,N2,M1,N2,M1,N2,X35,VCHHHH, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder561234(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M1,N2,M2,N3,N2,M2,N2,M2,N0,M1,t3D,F2)
       allocate(Z36(M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2))
       I1=K8*K8
       I2=K5*K0*K0*K6
       I3=K8*K8
       call EGEMM(I1,I2,I3,X35,F2,Z36)
       deallocate(F2)
C
       V3D=V3D+0.500*Z36
       deallocate(Z36)
       deallocate(X35)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,M2+1:N3))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,N2,N0,M1,M2,N3,VCHHPP,D1)
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
     & sum3124(N0,M1,M2,N3,N2,M2,M1,N2,X89,S515,-1.000)
       deallocate(S515)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z171(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X89,F2,Z171)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z171,-1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z171, 1.000)
       deallocate(Z171)
       deallocate(X89)
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
     & sum3124(N0,M1,M2,N3,M2,N3,M1,N2,X39,S508, 1.000)
       deallocate(S508)
C
       call sumx2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,M1,M2,N3,M2,N3,M1,N2,X39,VCHPHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z40(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K8*K5*K0*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,X39,F2,Z40)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z40,-1.000)
       call
     & sum234615(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z40, 1.000)
       deallocate(Z40)
       deallocate(X39)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,N2+1:M2))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,N2,N0,M1,N2,M2,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S511(M1+1:N2,N0+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S511)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,M1,N2,M2,
     & N0,N2,N0,M1,N2,M2,M1,N2,S511,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S517(N2+1:M2,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S517)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N2,M2,N2,M2,M1,N2,X91,S517,-1.000)
       deallocate(S517)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,M2,N3,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z173(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,X91,F2,Z173)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z173, 1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z173,-1.000)
       deallocate(Z173)
       deallocate(X91)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,M1,N2,M2,
     & N0,N2,N0,M1,N2,M2,M1,N2,S511,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S512(M2+1:N3,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S512)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N2,M2,M2,N3,M1,N2,X41,S512, 1.000)
       deallocate(S512)
C
       call sumx2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,M1,N2,M2,M2,N3,M1,N2,X41,VCHPHP, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z42(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K8*K5*K0*K0
       I3=K0*K5
       call EGEMM(I1,I2,I3,X41,F2,Z42)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z42,-1.000)
       call
     & sum234615(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z42, 1.000)
       deallocate(Z42)
       deallocate(X41)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N2,M2+1:N3))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,N2,M1,N2,M2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S509(M1+1:N2,N0+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S509)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,M1,N2,M2,N3,
     & N0,N2,M1,N2,M2,N3,M1,N2,S509,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S516(N2+1:M2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S516)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M2,N3,N2,M2,M1,N2,X90,S516,-1.000)
       deallocate(S516)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder512346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z172(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X90,F2,Z172)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z172, 1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z172,-1.000)
       deallocate(Z172)
       deallocate(X90)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,M1,N2,M2,N3,
     & N0,N2,M1,N2,M2,N3,M1,N2,S509,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S510(M2+1:N3,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S510)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M2,N3,M2,N3,M1,N2,X40,S510, 1.000)
       deallocate(S510)
C
       call sumx2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,M2,N3,M2,N3,M1,N2,X40,VCHPHP, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder512346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z41(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K8*K5*K0*K0
       I3=K6*K8
       call EGEMM(I1,I2,I3,X40,F2,Z41)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z41, 1.000)
       call
     & sum234615(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z41,-1.000)
       deallocate(Z41)
       deallocate(X40)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N2,N2+1:M2))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,N2,M1,N2,N2,M2,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S513(M1+1:N2,N0+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S513)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,M1,N2,N2,M2,
     & N0,N2,M1,N2,N2,M2,M1,N2,S513,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S518(N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S518)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,N2,M2,N2,M2,M1,N2,X92,S518,-1.000)
       deallocate(S518)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder521346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,M2,N3,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z174(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X92,F2,Z174)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z174,-1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z174, 1.000)
       deallocate(Z174)
       deallocate(X92)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,M1,N2,N2,M2,
     & N0,N2,M1,N2,N2,M2,M1,N2,S513,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S514(M2+1:N3,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K8
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S514)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,N2,M2,M2,N3,M1,N2,X42,S514, 1.000)
       deallocate(S514)
C
       call sumx2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,N2,M2,M2,N3,M1,N2,X42,VCHPHP, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder512346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z43(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6
       I2=K8*K5*K0*K0
       I3=K0*K8
       call EGEMM(I1,I2,I3,X42,F2,Z43)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z43, 1.000)
       call
     & sum234615(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z43,-1.000)
       deallocate(Z43)
       deallocate(X42)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:N2,M2+1:N3))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,M1,N0,N2,M2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S519(M1+1:N2,N0+1:M1,N0+1:N2,M2+1:N3))
       I1=K6*K2*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S519)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,M1+1:N2))
       call reorder3241(M1,N2,N0,M1,N0,N2,M2,N3,
     & N0,N2,N0,M1,M2,N3,M1,N2,S519,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S520(N2+1:M2,N0+1:M1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S520)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,N2,M2,M1,N2,X93,S520,-1.000)
       deallocate(S520)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z175(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X93,F2,Z175)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z175,-1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z175, 1.000)
       deallocate(Z175)
       deallocate(X93)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:N2,M2+1:N3))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,M1,N2,N0,N2,M2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S521(M1+1:N2,M1+1:N2,N0+1:N2,M2+1:N3))
       I1=K6*K2*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S521)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder3241(M1,N2,M1,N2,N0,N2,M2,N3,
     & N0,N2,M1,N2,M2,N3,M1,N2,S521,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S522(N2+1:M2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S522)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M2,N3,N2,M2,M1,N2,X94,S522,-1.000)
       deallocate(S522)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder512346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z176(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X94,F2,Z176)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z176, 1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z176,-1.000)
       deallocate(Z176)
       deallocate(X94)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:N2,N2+1:M2))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,M1,N0,N2,N2,M2,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S523(M1+1:N2,N0+1:M1,N0+1:N2,N2+1:M2))
       I1=K0*K2*K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S523)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,M1+1:N2))
       call reorder3241(M1,N2,N0,M1,N0,N2,N2,M2,
     & N0,N2,N0,M1,N2,M2,M1,N2,S523,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S524(N2+1:M2,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S524)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N2,M2,N2,M2,M1,N2,X95,S524,-1.000)
       deallocate(S524)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,M2,N3,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z177(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,X95,F2,Z177)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z177, 1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z177,-1.000)
       deallocate(Z177)
       deallocate(X95)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:N2,N2+1:M2))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,M1,N2,N0,N2,N2,M2,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S525(M1+1:N2,M1+1:N2,N0+1:N2,N2+1:M2))
       I1=K0*K2*K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S525)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       call reorder3241(M1,N2,M1,N2,N0,N2,N2,M2,
     & N0,N2,M1,N2,N2,M2,M1,N2,S525,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S526(N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S526)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,N2,M2,N2,M2,M1,N2,X96,S526,-1.000)
       deallocate(S526)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder521346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,M2,N3,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z178(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X96,F2,Z178)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z178,-1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z178, 1.000)
       deallocate(Z178)
       deallocate(X96)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,N2,N3,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q66(N0+1:M1,N2+1:N3))
       I1=K4*K5
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q66)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:M1))
       call reorder21(N0,M1,N2,N3,
     & N2,N3,N0,M1,Q66,B1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(Q70(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q70)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(N0,M1,N0,M1,X26,Q70, 1.000)
       deallocate(Q70)
C
       call sumx21(N0,N2,N0,N2,
     & N0,M1,N0,M1,X26,FBHH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z27(M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8*K8*K0*K0*K6
       I3=K5
       call EGEMM(I1,I2,I3,X26,F2,Z27)
       deallocate(F2)
C
       call
     & sum123564(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z27,-1.000)
       deallocate(Z27)
       deallocate(X26)
C
       allocate(B1(N2+1:N3,N0+1:M1))
       call reorder21(N0,M1,N2,N3,
     & N2,N3,N0,M1,Q66,B1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(Q67(M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q67)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(N0,M1,M1,N2,X24,Q67, 1.000)
       deallocate(Q67)
C
       call sumx21(N0,N2,N0,N2,
     & N0,M1,M1,N2,X24,FBHH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z25(M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8*K5*K0*K0*K6
       I3=K5
       call EGEMM(I1,I2,I3,X24,F2,Z25)
       deallocate(F2)
C
       V3D=V3D-Z25
       call
     & sum123465(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z25, 1.000)
       deallocate(Z25)
       deallocate(X24)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,M2+1:N3))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,N2,N0,M1,M2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S527(N0+1:M1,N0+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S527)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N0,M1,M2,N3,
     & N0,N2,N0,M1,M2,N3,N0,M1,S527,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S535(N2+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S535)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,N2,M2,N0,M1,X97,S535,-1.000)
       deallocate(S535)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z200(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X97,F2,Z200)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z200,-1.000)
       deallocate(Z200)
       deallocate(X97)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N0,M1,M2,N3,
     & N0,N2,N0,M1,M2,N3,N0,M1,S527,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S528(M2+1:N3,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S528)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,M2,N3,N0,M1,X43,S528, 1.000)
       deallocate(S528)
C
       call sumx2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,M1,M2,N3,M2,N3,N0,M1,X43,VCHPHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z48(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K0
       I3=K6*K5
       call EGEMM(I1,I2,I3,X43,F2,Z48)
       deallocate(F2)
C
       call
     & sum235614(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z48,-1.000)
       deallocate(Z48)
       deallocate(X43)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,N2+1:M2))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,N2,N0,M1,N2,M2,VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S531(N0+1:M1,N0+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S531)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N0,M1,N2,M2,
     & N0,N2,N0,M1,N2,M2,N0,M1,S531,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S537(N2+1:M2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S537)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N2,M2,N2,M2,N0,M1,X99,S537,-1.000)
       deallocate(S537)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z202(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,X99,F2,Z202)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z202, 1.000)
       deallocate(Z202)
       deallocate(X99)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N0,M1,N2,M2,
     & N0,N2,N0,M1,N2,M2,N0,M1,S531,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S532(M2+1:N3,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S532)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N2,M2,M2,N3,N0,M1,X45,S532, 1.000)
       deallocate(S532)
C
       call sumx2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,M1,N2,M2,M2,N3,N0,M1,X45,VCHPHP, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z50(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K0
       I3=K0*K5
       call EGEMM(I1,I2,I3,X45,F2,Z50)
       deallocate(F2)
C
       call
     & sum235614(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z50,-1.000)
       deallocate(Z50)
       deallocate(X45)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N2,M2+1:N3))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,N2,M1,N2,M2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S529(N0+1:M1,N0+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S529)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,M1,N2,M2,N3,
     & N0,N2,M1,N2,M2,N3,N0,M1,S529,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S536(N2+1:M2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S536)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M2,N3,N2,M2,N0,M1,X98,S536,-1.000)
       deallocate(S536)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z201(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X98,F2,Z201)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z201,-1.000)
       deallocate(Z201)
       deallocate(X98)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,M1,N2,M2,N3,
     & N0,N2,M1,N2,M2,N3,N0,M1,S529,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S530(M2+1:N3,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S530)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M2,N3,M2,N3,N0,M1,X44,S530, 1.000)
       deallocate(S530)
C
       call sumx2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,M2,N3,M2,N3,N0,M1,X44,VCHPHP, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z49(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K0
       I3=K6*K8
       call EGEMM(I1,I2,I3,X44,F2,Z49)
       deallocate(F2)
C
       call
     & sum235614(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z49,-1.000)
       deallocate(Z49)
       deallocate(X44)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N2,N2+1:M2))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,N2,M1,N2,N2,M2,VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S533(N0+1:M1,N0+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S533)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,M1,N2,N2,M2,
     & N0,N2,M1,N2,N2,M2,N0,M1,S533,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S538(N2+1:M2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S538)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,N2,M2,N2,M2,N0,M1,X100,S538,-1.000)
       deallocate(S538)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z203(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X100,F2,Z203)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z203, 1.000)
       deallocate(Z203)
       deallocate(X100)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,M1,N2,N2,M2,
     & N0,N2,M1,N2,N2,M2,N0,M1,S533,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S534(M2+1:N3,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S534)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,N2,M2,M2,N3,N0,M1,X46,S534, 1.000)
       deallocate(S534)
C
       call sumx2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,N2,M2,M2,N3,N0,M1,X46,VCHPHP, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z51(N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K8*K8*K0*K0
       I3=K0*K8
       call EGEMM(I1,I2,I3,X46,F2,Z51)
       deallocate(F2)
C
       call
     & sum235614(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z51,-1.000)
       deallocate(Z51)
       deallocate(X46)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:N2,M2+1:N3))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,M1,N0,N2,M2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S539(N0+1:M1,N0+1:M1,N0+1:N2,M2+1:N3))
       I1=K6*K2*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S539)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder3241(N0,M1,N0,M1,N0,N2,M2,N3,
     & N0,N2,N0,M1,M2,N3,N0,M1,S539,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S540(N2+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S540)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,N2,M2,N0,M1,X101,S540,-1.000)
       deallocate(S540)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z204(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X101,F2,Z204)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z204,-1.000)
       deallocate(Z204)
       deallocate(X101)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:N2,M2+1:N3))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,M1,N2,N0,N2,M2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S541(N0+1:M1,M1+1:N2,N0+1:N2,M2+1:N3))
       I1=K6*K2*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S541)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder3241(N0,M1,M1,N2,N0,N2,M2,N3,
     & N0,N2,M1,N2,M2,N3,N0,M1,S541,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S542(N2+1:M2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S542)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M2,N3,N2,M2,N0,M1,X102,S542,-1.000)
       deallocate(S542)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z205(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X102,F2,Z205)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z205,-1.000)
       deallocate(Z205)
       deallocate(X102)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:N2,N2+1:M2))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,M1,N0,N2,N2,M2,VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S543(N0+1:M1,N0+1:M1,N0+1:N2,N2+1:M2))
       I1=K0*K2*K5
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S543)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder3241(N0,M1,N0,M1,N0,N2,N2,M2,
     & N0,N2,N0,M1,N2,M2,N0,M1,S543,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S544(N2+1:M2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S544)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N2,M2,N2,M2,N0,M1,X103,S544,-1.000)
       deallocate(S544)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z206(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,X103,F2,Z206)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z206, 1.000)
       deallocate(Z206)
       deallocate(X103)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:N2,N2+1:M2))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,M1,N2,N0,N2,N2,M2,VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S545(N0+1:M1,M1+1:N2,N0+1:N2,N2+1:M2))
       I1=K0*K2*K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S545)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder3241(N0,M1,M1,N2,N0,N2,N2,M2,
     & N0,N2,M1,N2,N2,M2,N0,M1,S545,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S546(N2+1:M2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S546)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,N2,M2,N2,M2,N0,M1,X104,S546,-1.000)
       deallocate(S546)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z207(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X104,F2,Z207)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z207, 1.000)
       deallocate(Z207)
       deallocate(X104)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,N2,N3,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q68(M1+1:N2,N2+1:N3))
       I1=K4*K8
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q68)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,M1+1:N2))
       call reorder21(M1,N2,N2,N3,
     & N2,N3,M1,N2,Q68,B1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(Q71(N0+1:M1,M1+1:N2))
       I1=K8
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q71)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(M1,N2,N0,M1,X27,Q71, 1.000)
       deallocate(Q71)
C
       call sumx21(N0,N2,N0,N2,
     & M1,N2,N0,M1,X27,FBHH, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z28(M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8*K8*K0*K0*K6
       I3=K8
       call EGEMM(I1,I2,I3,X27,F2,Z28)
       deallocate(F2)
C
       call
     & sum123564(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z28,-1.000)
       deallocate(Z28)
       deallocate(X27)
C
       allocate(B1(N2+1:N3,M1+1:N2))
       call reorder21(M1,N2,N2,N3,
     & N2,N3,M1,N2,Q68,B1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(Q69(M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,B1,B2,Q69)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(M1,N2,M1,N2,X25,Q69, 1.000)
       deallocate(Q69)
C
       call sumx21(N0,N2,N0,N2,
     & M1,N2,M1,N2,X25,FBHH, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder512346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z26(M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8*K5*K0*K0*K6
       I3=K8
       call EGEMM(I1,I2,I3,X25,F2,Z26)
       deallocate(F2)
C
       V3D=V3D+Z26
       call
     & sum123465(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z26,-1.000)
       deallocate(Z26)
       deallocate(X25)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,M2+1:N3))
       call reorder4312(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,M2,N3,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S547(N2+1:M2,N0+1:N2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S547)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,N2+1:M2))
       call reorder2341(N2,M2,N0,N2,M2,N3,M2,N3,
     & N0,N2,M2,N3,M2,N3,N2,M2,S547,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S548(M2+1:N3,M2+1:N3,M2+1:N3,N2+1:M2))
       I1=K0*K6*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S548)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,M2,N3,M2,N3,N2,M2,X105,S548,-1.000)
       deallocate(S548)
C
       allocate(F2(M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder123456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,M1,N2,t3D,F2)
       allocate(Z222(N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K8*K8*K5*K0
       I3=K6*K6
       call EGEMM(I1,I2,I3,X105,F2,Z222)
       deallocate(F2)
C
       call
     & sum345612(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z222,-0.500)
       deallocate(Z222)
       deallocate(X105)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder4312(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,N2,M2,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S549(N2+1:M2,N0+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S549)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder2341(N2,M2,N0,N2,M2,N3,N2,M2,
     & N0,N2,M2,N3,N2,M2,N2,M2,S549,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S559(N2+1:M2,M2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S559)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M2,N3,N2,M2,N2,M2,N2,M2,X50,S559,-1.000)
       deallocate(S559)
C
       call sumx1234(N2,N3,N2,N3,N2,N3,N2,M2,
     & M2,N3,N2,M2,N2,M2,N2,M2,X50,VCAPPP, 1.000)
C
       allocate(F2(M2+1:N3,N2+1:M2,M2+1:N3,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder132456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M2,N3,N2,M2,M2,N3,N0,M1,M1,N2,M1,N2,t3D,F2)
       allocate(Z59(M2+1:N3,N0+1:M1,M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0
       I2=K8*K8*K5*K6
       I3=K0*K6
       call EGEMM(I1,I2,I3,X50,F2,Z59)
       deallocate(F2)
C
       call
     & sum145623(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z59,-1.000)
       deallocate(Z59)
       deallocate(X50)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder2341(N2,M2,N0,N2,M2,N3,N2,M2,
     & N0,N2,M2,N3,N2,M2,N2,M2,S549,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S550(M2+1:N3,M2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S550)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,N2,M2,M2,N3,N2,M2,X106,S550,-1.000)
       deallocate(S550)
C
       allocate(F2(M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder123456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,t3D,F2)
       allocate(Z223(N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K8*K8*K5*K0
       I3=K0*K6
       call EGEMM(I1,I2,I3,X106,F2,Z223)
       deallocate(F2)
C
       call
     & sum345612(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z223,-1.000)
       deallocate(Z223)
       deallocate(X106)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,M2+1:N3))
       call reorder4312(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,M2,N3,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S553(N2+1:M2,N0+1:N2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S553)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,N2+1:M2))
       call reorder2341(N2,M2,N0,N2,M2,N3,M2,N3,
     & N0,N2,M2,N3,M2,N3,N2,M2,S553,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S554(M2+1:N3,M2+1:N3,M2+1:N3,N2+1:M2))
       I1=K0*K6*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S554)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,M2,N3,M2,N3,N2,M2,X108,S554,-1.000)
       deallocate(S554)
C
       allocate(F2(M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder123456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,M1,N2,t3D,F2)
       allocate(Z225(N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K8*K8*K5*K0
       I3=K6*K6
       call EGEMM(I1,I2,I3,X108,F2,Z225)
       deallocate(F2)
C
       call
     & sum245613(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z225, 0.500)
       deallocate(Z225)
       deallocate(X108)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder4312(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,N2,M2,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S555(N2+1:M2,N0+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S555)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder2341(N2,M2,N0,N2,M2,N3,N2,M2,
     & N0,N2,M2,N3,N2,M2,N2,M2,S555,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S556(M2+1:N3,M2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S556)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,N2,M2,M2,N3,N2,M2,X109,S556,-1.000)
       deallocate(S556)
C
       allocate(F2(M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder123456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,t3D,F2)
       allocate(Z226(N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K8*K8*K5*K0
       I3=K0*K6
       call EGEMM(I1,I2,I3,X109,F2,Z226)
       deallocate(F2)
C
       call
     & sum245613(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z226, 1.000)
       deallocate(Z226)
       deallocate(X109)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder4312(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,M2,N2,M2,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S557(N2+1:M2,N0+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S557)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder2341(N2,M2,N0,N2,N2,M2,N2,M2,
     & N0,N2,N2,M2,N2,M2,N2,M2,S557,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S558(M2+1:N3,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S558)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,M2,N2,M2,M2,N3,N2,M2,X110,S558,-1.000)
       deallocate(S558)
C
       allocate(F2(N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder123456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N2,M2,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,t3D,F2)
       allocate(Z227(N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K8*K8*K5*K0
       I3=K0*K0
       call EGEMM(I1,I2,I3,X110,F2,Z227)
       deallocate(F2)
C
       call
     & sum245613(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z227, 0.500)
       deallocate(Z227)
       deallocate(X110)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,M2,N3,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q72(N0+1:N2,M2+1:N3))
       I1=K6*K2
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q72)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(Q76(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q72,B2,Q76)
       deallocate(B2)
C
       call
     & sum21(M2,N3,N2,M2,X30,Q76,-1.000)
       deallocate(Q76)
C
       call sumx12(N2,N3,N2,N3,
     & M2,N3,N2,M2,X30,FBPP, 1.000)
C
       allocate(F2(M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder123456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,M1,N2,t3D,F2)
       allocate(Z31(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K8*K5*K0*K6
       I3=K6
       call EGEMM(I1,I2,I3,X30,F2,Z31)
       deallocate(F2)
C
       call
     & sum134562(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z31,-1.000)
       deallocate(Z31)
       deallocate(X30)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(Q78(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q72,B2,Q78)
       deallocate(B2)
C
       call
     & sum21(M2,N3,N2,M2,X32,Q78,-1.000)
       deallocate(Q78)
C
       call sumx12(N2,N3,N2,N3,
     & M2,N3,N2,M2,X32,FBPP, 1.000)
C
       allocate(F2(M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder123456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,M1,N2,t3D,F2)
       allocate(Z33(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K8*K5*K0*K6
       I3=K6
       call EGEMM(I1,I2,I3,X32,F2,Z33)
       deallocate(F2)
C
       call
     & sum124563(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z33, 1.000)
       deallocate(Z33)
       deallocate(X32)
C
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(Q73(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,Q72,B2,Q73)
       deallocate(B2)
C
       call
     & sum21(M2,N3,M2,N3,X28,Q73,-1.000)
       deallocate(Q73)
C
       call sumx12(N2,N3,N2,N3,
     & M2,N3,M2,N3,X28,FBPP, 1.000)
C
       allocate(F2(M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder123456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,t3D,F2)
       allocate(Z29(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6
       I2=K8*K8*K5*K0*K0
       I3=K6
       call EGEMM(I1,I2,I3,X28,F2,Z29)
       deallocate(F2)
C
       call
     & sum234561(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z29, 1.000)
       deallocate(Z29)
       deallocate(X28)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder4312(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,M2,N2,M2,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S551(N2+1:M2,N0+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S551)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder2341(N2,M2,N0,N2,N2,M2,N2,M2,
     & N0,N2,N2,M2,N2,M2,N2,M2,S551,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S552(M2+1:N3,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S552)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,M2,N2,M2,M2,N3,N2,M2,X107,S552,-1.000)
       deallocate(S552)
C
       allocate(F2(N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder123456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N2,M2,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,t3D,F2)
       allocate(Z224(N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K8*K8*K5*K0
       I3=K0*K0
       call EGEMM(I1,I2,I3,X107,F2,Z224)
       deallocate(F2)
C
       call
     & sum345612(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z224,-0.500)
       deallocate(Z224)
       deallocate(X107)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder2341(N2,M2,N0,N2,N2,M2,N2,M2,
     & N0,N2,N2,M2,N2,M2,N2,M2,S551,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S560(N2+1:M2,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S560)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N2,M2,N2,M2,N2,M2,N2,M2,X51,S560,-1.000)
       deallocate(S560)
C
       call sumx1234(N2,N3,N2,N3,N2,N3,N2,M2,
     & N2,M2,N2,M2,N2,M2,N2,M2,X51,VCAPPP, 1.000)
C
       allocate(F2(N2+1:M2,N2+1:M2,M2+1:N3,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder231456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N2,M2,N2,M2,M2,N3,N0,M1,M1,N2,M1,N2,t3D,F2)
       allocate(Z60(M2+1:N3,N0+1:M1,M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0
       I2=K8*K8*K5*K6
       I3=K0*K0
       call EGEMM(I1,I2,I3,X51,F2,Z60)
       deallocate(F2)
C
       call
     & sum145623(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z60, 0.500)
       deallocate(Z60)
       deallocate(X51)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N2,M2,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q74(N0+1:N2,N2+1:M2))
       I1=K0*K2
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q74)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(Q75(M2+1:N3,N2+1:M2))
       I1=K0
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,Q74,B2,Q75)
       deallocate(B2)
C
       call
     & sum21(N2,M2,M2,N3,X29,Q75,-1.000)
       deallocate(Q75)
C
       call sumx12(N2,N3,N2,N3,
     & N2,M2,M2,N3,X29,FBPP, 1.000)
C
       allocate(F2(N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder123456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N2,M2,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,t3D,F2)
       allocate(Z30(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6
       I2=K8*K8*K5*K0*K0
       I3=K0
       call EGEMM(I1,I2,I3,X29,F2,Z30)
       deallocate(F2)
C
       call
     & sum234561(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z30, 1.000)
       deallocate(Z30)
       deallocate(X29)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(Q79(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q74,B2,Q79)
       deallocate(B2)
C
       call
     & sum21(N2,M2,N2,M2,X33,Q79,-1.000)
       deallocate(Q79)
C
       call sumx12(N2,N3,N2,N3,
     & N2,M2,N2,M2,X33,FBPP, 1.000)
C
       allocate(F2(N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder213456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N2,M2,M2,N3,N2,M2,N0,M1,M1,N2,M1,N2,t3D,F2)
       allocate(Z34(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K8*K5*K0*K6
       I3=K0
       call EGEMM(I1,I2,I3,X33,F2,Z34)
       deallocate(F2)
C
       call
     & sum124563(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z34,-1.000)
       deallocate(Z34)
       deallocate(X33)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(Q77(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,Q74,B2,Q77)
       deallocate(B2)
C
       call
     & sum21(N2,M2,N2,M2,X31,Q77,-1.000)
       deallocate(Q77)
C
       call sumx12(N2,N3,N2,N3,
     & N2,M2,N2,M2,X31,FBPP, 1.000)
C
       allocate(F2(N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder213456(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N2,M2,M2,N3,N2,M2,N0,M1,M1,N2,M1,N2,t3D,F2)
       allocate(Z32(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0
       I2=K8*K8*K5*K0*K6
       I3=K0
       call EGEMM(I1,I2,I3,X31,F2,Z32)
       deallocate(F2)
C
       call
     & sum134562(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z32, 1.000)
       deallocate(Z32)
       deallocate(X31)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,N2,N0,N1,N1,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S561(M1+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S561)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder3421(M1,N2,N0,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N2,M1,N2,S561,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S563(N2+1:M2,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S563)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,N2,M2,M1,N2,M1,N2,X65,S563, 1.000)
       deallocate(S563)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder3421(M1,N2,N0,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N2,M1,N2,S561,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S564(N2+1:M2,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S564)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,N2,M2,M1,N2,M1,N2,X66,S564, 1.000)
       deallocate(S564)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder3421(M1,N2,N0,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N2,M1,N2,S561,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S565(M2+1:N3,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S565)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,M2,N3,N0,M1,M1,N2,X3,S565, 1.000)
       deallocate(S565)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder3421(M1,N2,N0,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N2,M1,N2,S561,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S566(N2+1:M2,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S566)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,N2,M2,N0,M1,M1,N2,X67,S566,-1.000)
       deallocate(S566)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder3421(M1,N2,N0,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N2,M1,N2,S561,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S567(N2+1:M2,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S567)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,N2,M2,N0,M1,M1,N2,X68,S567, 1.000)
       deallocate(S567)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder3421(M1,N2,N0,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N2,M1,N2,S561,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S562(M2+1:N3,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S562)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,M2,N3,M1,N2,M1,N2,X64,S562, 1.000)
       deallocate(S562)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,N2,N0,N1,N1,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S568(N0+1:M1,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S568)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:M1))
       call reorder3421(N0,M1,N0,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N2,N0,M1,S568,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S570(N2+1:M2,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S570)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N2,N2,M2,N0,M1,M1,N2,X67,S570, 1.000)
       deallocate(S570)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:M1))
       call reorder3421(N0,M1,N0,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N2,N0,M1,S568,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S571(N2+1:M2,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S571)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N2,N2,M2,N0,M1,M1,N2,X68,S571,-1.000)
       deallocate(S571)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:M1))
       call reorder3421(N0,M1,N0,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N2,N0,M1,S568,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S569(M2+1:N3,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S569)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N2,M2,N3,N0,M1,M1,N2,X3,S569,-1.000)
       deallocate(S569)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S572(N2+1:M2,M1+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S572)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N2,M2,M1,N2,S572,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S581(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S581)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,N3,N2,M2,N2,M2,M1,N2,X5,S581, 1.000)
       deallocate(S581)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N2,M2,M1,N2,S572,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S573(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S573)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,M1,N2,X69,S573, 1.000)
       deallocate(S573)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S576(N2+1:M2,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4*K1
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S576)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder2431(N2,M2,N0,N1,N2,N3,N1,N3,
     & N0,N1,N1,N3,N2,N3,N2,M2,S576,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S586(M2+1:N3,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S586)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,M2,N3,N2,M2,N0,M1,X71,S586,-1.000)
       deallocate(S586)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder2431(N2,M2,N0,N1,N2,N3,N1,N3,
     & N0,N1,N1,N3,N2,N3,N2,M2,S576,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S588(N2+1:M2,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S588)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,N3,N2,M2,N2,M2,N0,M1,X7,S588,-1.000)
       deallocate(S588)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder2431(N2,M2,N0,N1,N2,N3,N1,N3,
     & N0,N1,N1,N3,N2,N3,N2,M2,S576,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S577(M2+1:N3,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S577)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,M2,N3,N2,M2,M1,N2,X69,S577,-1.000)
       deallocate(S577)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S574(N2+1:M2,M1+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K8*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S574)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N2,M2,M1,N2,S574,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S580(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S580)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,N2,M2,M1,N2,X5,S580,-1.000)
       deallocate(S580)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N2,M2,M1,N2,S574,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S575(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S575)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,M1,N2,X70,S575, 1.000)
       deallocate(S575)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S584(N2+1:M2,N0+1:M1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S584)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3412(N2,M2,N0,M1,N0,N2,N2,N3,
     & N0,N2,N2,N3,N2,M2,N0,M1,S584,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S585(M2+1:N3,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S585)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,N0,M1,X72,S585, 1.000)
       deallocate(S585)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,N3,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S578(N2+1:M2,N0+1:N1,N2+1:N3,N1+1:N3))
       I1=K3*K4*K1
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S578)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder2431(N2,M2,N0,N1,N2,N3,N1,N3,
     & N0,N1,N1,N3,N2,N3,N2,M2,S578,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S587(M2+1:N3,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S587)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,M2,N3,N2,M2,N0,M1,X72,S587,-1.000)
       deallocate(S587)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder2431(N2,M2,N0,N1,N2,N3,N1,N3,
     & N0,N1,N1,N3,N2,N3,N2,M2,S578,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S589(N2+1:M2,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S589)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,N2,M2,N2,M2,N0,M1,X7,S589, 1.000)
       deallocate(S589)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder2431(N2,M2,N0,N1,N2,N3,N1,N3,
     & N0,N1,N1,N3,N2,N3,N2,M2,S578,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N2,t2B,D2)
       allocate(S579(M2+1:N3,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S579)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,M2,N3,N2,M2,M1,N2,X70,S579,-1.000)
       deallocate(S579)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S582(N2+1:M2,N0+1:M1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K5*K0
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S582)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3412(N2,M2,N0,M1,N0,N2,N2,N3,
     & N0,N2,N2,N3,N2,M2,N0,M1,S582,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S583(M2+1:N3,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S583)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,N0,M1,X71,S583, 1.000)
       deallocate(S583)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,N2,N0,N2,N2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S590(M1+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S590)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,M1,N2,S590,D1)
       allocate(D2(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,N2,M2,t2C,D2)
       allocate(S592(M2+1:N3,N2+1:M2,N2+1:N3,M1+1:N2))
       I1=K8*K4
       I2=K0*K6
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S592)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N2,N3,M2,N3,N2,M2,M1,N2,X120,S592,-1.000)
       deallocate(S592)
C
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,N0,M1,M1,N2,t2C,D2)
       allocate(Z330(N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K6
       I2=K8*K5*K0
       I3=K4
       call EGEMM(I1,I2,I3,X120,D2,Z330)
       deallocate(D2)
C
       call
     & sum245136(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z330, 0.500)
       call
     & sum345126(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z330,-0.500)
       call
     & sum246135(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z330,-0.500)
       call
     & sum346125(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z330, 0.500)
       deallocate(Z330)
       deallocate(X120)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder3421(M1,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N0,N2,M1,N2,S590,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S593(N2+1:M2,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S593)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,N2,M2,N0,M1,M1,N2,X68,S593, 1.000)
       deallocate(S593)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder2431(M1,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N0,N2,M1,N2,S590,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S594(N2+1:M2,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S594)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,N2,M2,N0,M1,M1,N2,X67,S594, 1.000)
       deallocate(S594)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder2431(M1,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N0,N2,M1,N2,S590,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S595(M2+1:N3,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S595)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,M2,N3,M1,N2,M1,N2,X64,S595,-1.000)
       deallocate(S595)
C
       allocate(D2(N0+1:N2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,M2,N2,M2,N0,M1,t2C,D2)
       allocate(Z74(N2+1:M2,N2+1:M2,N0+1:M1,M2+1:N3,M1+1:N2,M1+1:N2))
       I1=K8*K8*K6
       I2=K5*K0*K0
       I3=K2
       call EGEMM(I1,I2,I3,X64,D2,Z74)
       deallocate(D2)
C
       call
     & sum234156(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z74,-1.000)
       call
     & sum234165(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z74, 1.000)
       deallocate(Z74)
       deallocate(X64)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder2431(M1,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N0,N2,M1,N2,S590,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2C,D2)
       allocate(S596(M2+1:N3,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S596)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,M2,N3,N0,M1,M1,N2,X3,S596,-1.000)
       deallocate(S596)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder2431(M1,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N0,N2,M1,N2,S590,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S597(N2+1:M2,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S597)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,N2,M2,M1,N2,M1,N2,X65,S597,-1.000)
       deallocate(S597)
C
       allocate(D2(N0+1:N2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,M2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(Z75(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:M2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K0
       I2=K5*K0*K6
       I3=K2
       call EGEMM(I1,I2,I3,X65,D2,Z75)
       deallocate(D2)
C
       call
     & sum134256(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z75, 1.000)
       call
     & sum134265(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z75,-1.000)
       deallocate(Z75)
       deallocate(X65)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder3421(M1,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N0,N2,M1,N2,S590,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S598(N2+1:M2,M1+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S598)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,N2,M2,M1,N2,M1,N2,X66,S598, 1.000)
       deallocate(S598)
C
       allocate(D2(N0+1:N2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,M2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(Z76(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:M2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K0
       I2=K5*K0*K6
       I3=K2
       call EGEMM(I1,I2,I3,X66,D2,Z76)
       deallocate(D2)
C
       call
     & sum124356(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z76,-1.000)
       call
     & sum124365(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z76, 1.000)
       deallocate(Z76)
       deallocate(X66)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,M1+1:N2))
       call reorder4231(M1,N2,N0,N2,N0,N2,N2,N3,
     & N2,N3,N0,N2,N0,N2,M1,N2,S590,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S633(M1+1:N2,N0+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2*K2
       I2=K8
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S633)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder2314(M1,N2,N0,N2,N0,N2,M1,N2,
     & N0,N2,N0,N2,M1,N2,M1,N2,S633,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S634(M2+1:N3,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S634)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,M2,N3,M1,N2,M1,N2,X1,S634, 1.000)
       deallocate(S634)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,M1+1:N2))
       call reorder4231(M1,N2,N0,N2,N0,N2,N2,N3,
     & N2,N3,N0,N2,N0,N2,M1,N2,S590,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S637(N0+1:M1,N0+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S637)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder2314(N0,M1,N0,N2,N0,N2,M1,N2,
     & N0,N2,N0,N2,N0,M1,M1,N2,S637,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S638(M2+1:N3,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S638)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,M2,N3,N0,M1,M1,N2,X3,S638, 1.000)
       deallocate(S638)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder3241(M1,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,M1,N2,S590,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S641(N2+1:M2,N0+1:N2,N2+1:N3,M1+1:N2))
       I1=K8*K4*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S641)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder2314(N2,M2,N0,N2,N2,N3,M1,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,S641,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S642(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S642)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,M1,N2,X69,S642, 1.000)
       deallocate(S642)
C
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,N0,M1,M1,N2,t2C,D2)
       allocate(Z83(N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K6
       I2=K8*K5*K0
       I3=K4
       call EGEMM(I1,I2,I3,X69,D2,Z83)
       deallocate(D2)
C
       call
     & sum345126(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z83, 1.000)
       call
     & sum346125(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z83,-1.000)
       deallocate(Z83)
       deallocate(X69)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder3241(M1,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,M1,N2,S590,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S643(N2+1:M2,N0+1:N2,N2+1:N3,M1+1:N2))
       I1=K8*K4*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S643)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder2314(N2,M2,N0,N2,N2,N3,M1,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,S643,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S644(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S644)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,M1,N2,X70,S644, 1.000)
       deallocate(S644)
C
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,N0,M1,M1,N2,t2C,D2)
       allocate(Z84(N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K6
       I2=K8*K5*K0
       I3=K4
       call EGEMM(I1,I2,I3,X70,D2,Z84)
       deallocate(D2)
C
       call
     & sum245136(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z84,-1.000)
       call
     & sum246135(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z84, 1.000)
       deallocate(Z84)
       deallocate(X70)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,M1,N2,S590,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,M2,N2,M2,t2C,D2)
       allocate(S591(N2+1:M2,N2+1:M2,N2+1:N3,M1+1:N2))
       I1=K8*K4
       I2=K0*K0
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S591)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N2,N3,N2,M2,N2,M2,M1,N2,X5,S591,-0.500)
       deallocate(S591)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder2314(M1,N2,N0,N2,N0,N2,M1,N2,
     & N0,N2,N0,N2,M1,N2,M1,N2,S633,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S635(N2+1:M2,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S635)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,N2,M2,M1,N2,M1,N2,X62,S635,-1.000)
       deallocate(S635)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder3214(M1,N2,N0,N2,N0,N2,M1,N2,
     & N0,N2,N0,N2,M1,N2,M1,N2,S633,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S636(N2+1:M2,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S636)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,N2,M2,M1,N2,M1,N2,X63,S636,-1.000)
       deallocate(S636)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder2314(N0,M1,N0,N2,N0,N2,M1,N2,
     & N0,N2,N0,N2,N0,M1,M1,N2,S637,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S639(N2+1:M2,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S639)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,N2,M2,N0,M1,M1,N2,X67,S639,-1.000)
       deallocate(S639)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder3214(N0,M1,N0,N2,N0,N2,M1,N2,
     & N0,N2,N0,N2,N0,M1,M1,N2,S637,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S640(N2+1:M2,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S640)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,N2,M2,N0,M1,M1,N2,X68,S640,-1.000)
       deallocate(S640)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder2314(N2,M2,N0,N2,N2,N3,M1,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,S643,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S645(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S645)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,N2,M2,M1,N2,X5,S645,-1.000)
       deallocate(S645)
C
       call sumx2314(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N2,M2,N2,M2,M1,N2,X5,VCHPPP, 1.000)
C
       allocate(D2(N2+1:N3,M2+1:N3,N0+1:M1,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,M2,N3,N0,M1,M1,N2,t2C,D2)
       allocate(Z6(M2+1:N3,N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K0
       I2=K8*K5*K6
       I3=K4
       call EGEMM(I1,I2,I3,X5,D2,Z6)
       deallocate(D2)
C
       call
     & sum145236(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z6,-1.000)
       call
     & sum146235(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z6, 1.000)
       deallocate(Z6)
       deallocate(X5)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,VCHHPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,M1,N2,M1,N2,t2C,D2)
       allocate(S605(M1+1:N2,M1+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K8*K8
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S605)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder4312(M1,N2,M1,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,M1,N2,M1,N2,S605,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S616(N2+1:M2,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S616)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,N2,M2,M1,N2,M1,N2,X63,S616, 0.500)
       deallocate(S616)
C
       allocate(D2(N0+1:N2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,M2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(Z73(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:M2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K0
       I2=K5*K0*K6
       I3=K2
       call EGEMM(I1,I2,I3,X63,D2,Z73)
       deallocate(D2)
C
       call
     & sum124356(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z73,-1.000)
       deallocate(Z73)
       deallocate(X63)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder3412(M1,N2,M1,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,M1,N2,M1,N2,S605,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S606(M2+1:N3,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S606)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,M2,N3,M1,N2,M1,N2,X1,S606,-0.500)
       deallocate(S606)
C
       call sumx2134(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,M2,N3,M1,N2,M1,N2,X1,VCHHHP, 1.000)
C
       allocate(D2(N0+1:N2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,M2,N2,M2,N0,M1,t2C,D2)
       allocate(Z1(N2+1:M2,N2+1:M2,N0+1:M1,M2+1:N3,M1+1:N2,M1+1:N2))
       I1=K8*K8*K6
       I2=K5*K0*K0
       I3=K2
       call EGEMM(I1,I2,I3,X1,D2,Z1)
       deallocate(D2)
C
       call
     & sum234156(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z1,-1.000)
       deallocate(Z1)
       deallocate(X1)
C
       allocate(D1(N0+1:N2,N0+1:N2,M1+1:N2,M1+1:N2))
       call reorder3412(M1,N2,M1,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,M1,N2,M1,N2,S605,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S612(N2+1:M2,N0+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S612)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,N2,M2,M1,N2,M1,N2,X62,S612, 0.500)
       deallocate(S612)
C
       allocate(D2(N0+1:N2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,M2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(Z72(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:M2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K0
       I2=K5*K0*K6
       I3=K2
       call EGEMM(I1,I2,I3,X62,D2,Z72)
       deallocate(D2)
C
       call
     & sum134256(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z72,-1.000)
       deallocate(Z72)
       deallocate(X62)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder4312(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S613(N2+1:M2,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S613)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder2431(N2,M2,N0,N2,N2,N3,N2,N3,
     & N0,N2,N2,N3,N2,N3,N2,M2,S613,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S626(N2+1:M2,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S626)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,N2,M2,N2,M2,M1,N2,X119,S626, 1.000)
       deallocate(S626)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder2341(N2,M2,N0,N2,N2,N3,N2,N3,
     & N0,N2,N2,N3,N2,N3,N2,M2,S613,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S630(M2+1:N3,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S630)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,M2,N3,N2,M2,M1,N2,X126,S630, 2.000)
       deallocate(S630)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:M2))
       call reorder3421(N2,M2,N0,N2,N2,N3,N2,N3,
     & N2,N3,N2,N3,N0,N2,N2,M2,S613,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:M1,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,M1,M1,N2,t2C,D2)
       allocate(S632(N0+1:M1,M1+1:N2,N0+1:N2,N2+1:M2))
       I1=K0*K2
       I2=K8*K5
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S632)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,N2,N2,M2,N0,M1,M1,N2,X68,S632, 0.500)
       deallocate(S632)
C
       allocate(D2(N0+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,M2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(Z79(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:M2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K0
       I2=K8*K0*K6
       I3=K2
       call EGEMM(I1,I2,I3,X68,D2,Z79)
       deallocate(D2)
C
       call
     & sum125346(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z79, 1.000)
       call
     & sum126345(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z79,-1.000)
       deallocate(Z79)
       deallocate(X68)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder2341(N2,M2,N0,N2,N2,N3,N2,N3,
     & N0,N2,N2,N3,N2,N3,N2,M2,S613,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2C,D2)
       allocate(S614(M2+1:N3,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S614)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,M2,N3,N2,M2,N0,M1,X72,S614,-1.000)
       deallocate(S614)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S620(N2+1:M2,M1+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S620)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N2,M2,M1,N2,S620,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S622(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S622)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,M1,N2,X127,S622, 2.000)
       deallocate(S622)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N2,M2,M1,N2,S620,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S621(M2+1:N3,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S621)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,M1,N2,X126,S621,-2.000)
       deallocate(S621)
C
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,N0,M1,M1,N2,t2C,D2)
       allocate(Z441(N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K6
       I2=K8*K5*K0
       I3=K4
       call EGEMM(I1,I2,I3,X126,D2,Z441)
       deallocate(D2)
C
       call
     & sum246135(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z441,-0.500)
       call
     & sum245136(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z441, 0.500)
       deallocate(Z441)
       deallocate(X126)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,VCHHPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:M1,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,M1,M1,N2,t2C,D2)
       allocate(S623(N0+1:M1,M1+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K8*K5
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S623)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder3412(N0,M1,M1,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,M1,M1,N2,S623,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S624(M2+1:N3,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S624)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,M2,N3,N0,M1,M1,N2,X3,S624,-0.500)
       deallocate(S624)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:N3))
       call reorder4312(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,N3,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S610(N2+1:M2,N0+1:N2,N2+1:N3,N2+1:N3))
       I1=K4*K4*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S610)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder2341(N2,M2,N0,N2,N2,N3,N2,N3,
     & N0,N2,N2,N3,N2,N3,N2,M2,S610,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S628(M2+1:N3,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S628)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,M2,N3,N2,M2,M1,N2,X127,S628,-2.000)
       deallocate(S628)
C
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,N0,M1,M1,N2,t2C,D2)
       allocate(Z447(N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K6
       I2=K8*K5*K0
       I3=K4
       call EGEMM(I1,I2,I3,X127,D2,Z447)
       deallocate(D2)
C
       call
     & sum346125(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z447,-0.500)
       call
     & sum345126(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z447, 0.500)
       deallocate(Z447)
       deallocate(X127)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:M2))
       call reorder3421(N2,M2,N0,N2,N2,N3,N2,N3,
     & N2,N3,N2,N3,N0,N2,N2,M2,S610,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:M1,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,M1,M1,N2,t2C,D2)
       allocate(S629(N0+1:M1,M1+1:N2,N0+1:N2,N2+1:M2))
       I1=K0*K2
       I2=K8*K5
       I3=K4*K4
       call EGEMM(I1,I2,I3,D1,D2,S629)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,N2,N2,M2,N0,M1,M1,N2,X67,S629,-0.500)
       deallocate(S629)
C
       allocate(D2(N0+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,M2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(Z78(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:M2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K0
       I2=K8*K0*K6
       I3=K2
       call EGEMM(I1,I2,I3,X67,D2,Z78)
       deallocate(D2)
C
       call
     & sum135246(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z78, 1.000)
       call
     & sum136245(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z78,-1.000)
       deallocate(Z78)
       deallocate(X67)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder2341(N2,M2,N0,N2,N2,N3,N2,N3,
     & N0,N2,N2,N3,N2,N3,N2,M2,S610,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2C,D2)
       allocate(S611(M2+1:N3,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S611)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,M2,N3,N2,M2,N0,M1,X71,S611,-1.000)
       deallocate(S611)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder2431(N2,M2,N0,N2,N2,N3,N2,N3,
     & N0,N2,N2,N3,N2,N3,N2,M2,S610,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S625(N2+1:M2,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S625)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,N3,N2,M2,N2,M2,M1,N2,X119,S625,-1.000)
       deallocate(S625)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N2,N3,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q80(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q80)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q80,B1)
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,N0,M1,M1,N2,t2C,D2)
       allocate(S631(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S631)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X4,S631,-1.000)
       deallocate(S631)
C
       call sumx2134(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,M2,N0,M1,M1,N2,X4,VCHHHP, 1.000)
C
       allocate(D2(N0+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,M2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(Z4(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:M2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K0
       I2=K8*K0*K6
       I3=K2
       call EGEMM(I1,I2,I3,X4,D2,Z4)
       deallocate(D2)
C
       call
     & sum135246(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z4,-1.000)
       call
     & sum125346(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z4, 1.000)
       call
     & sum136245(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z4, 1.000)
       call
     & sum126345(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z4,-1.000)
       deallocate(Z4)
       deallocate(X4)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q80,B1)
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,M1,N2,M1,N2,t2C,D2)
       allocate(S615(N2+1:M2,M1+1:N2,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K8*K0
       I3=K4
       call EGEMM(I1,I2,I3,B1,D2,S615)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N2,N2,M2,M1,N2,M1,N2,X2,S615,-1.000)
       deallocate(S615)
C
       call sumx2134(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,M2,M1,N2,M1,N2,X2,VCHHHP, 1.000)
C
       allocate(D2(N0+1:N2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,M2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(Z2(M2+1:N3,N2+1:M2,N0+1:M1,N2+1:M2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K0
       I2=K5*K0*K6
       I3=K2
       call EGEMM(I1,I2,I3,X2,D2,Z2)
       deallocate(D2)
C
       call
     & sum134256(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z2, 1.000)
       call
     & sum124356(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z2,-1.000)
       deallocate(Z2)
       deallocate(X2)
C
       allocate(D2(N0+1:N2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,M2,N2,M2,N0,M1,t2C,D2)
       allocate(S609(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K2
       call EGEMM(I1,I2,I3,Q80,D2,S609)
       deallocate(D2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,N0,M1,X7,S609,-1.000)
       deallocate(S609)
C
       allocate(D2(N0+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,M2,N2,M2,M1,N2,t2C,D2)
       allocate(S627(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K2
       call EGEMM(I1,I2,I3,Q80,D2,S627)
       deallocate(D2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X119,S627, 1.000)
       deallocate(S627)
C
       allocate(D2(N2+1:N3,M2+1:N3,N0+1:M1,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,M2,N3,N0,M1,M1,N2,t2C,D2)
       allocate(Z326(M2+1:N3,N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K0
       I2=K8*K5*K6
       I3=K4
       call EGEMM(I1,I2,I3,X119,D2,Z326)
       deallocate(D2)
C
       call
     & sum146235(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z326,-1.000)
       call
     & sum145236(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z326, 1.000)
       deallocate(Z326)
       deallocate(X119)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,N2,N0,N2,N2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S599(N0+1:M1,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2*K2
       I2=K5
       I3=K4
       call EGEMM(I1,I2,I3,D1,B2,S599)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,N0,M1,S599,D1)
       allocate(D2(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,N2,M2,t2C,D2)
       allocate(S601(M2+1:N3,N2+1:M2,N2+1:N3,N0+1:M1))
       I1=K5*K4
       I2=K0*K6
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S601)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N2,N3,M2,N3,N2,M2,N0,M1,X121,S601,-1.000)
       deallocate(S601)
C
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,M1,N2,M1,N2,t2C,D2)
       allocate(Z336(N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K6
       I2=K8*K8*K0
       I3=K4
       call EGEMM(I1,I2,I3,X121,D2,Z336)
       deallocate(D2)
C
       call
     & sum256134(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z336, 0.500)
       call
     & sum356124(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z336,-0.500)
       deallocate(Z336)
       deallocate(X121)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:M1))
       call reorder2431(N0,M1,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N0,N2,N0,M1,S599,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S618(N2+1:M2,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S618)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N2,N2,M2,N0,M1,M1,N2,X129,S618,-2.000)
       deallocate(S618)
C
       allocate(D2(N0+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,M2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(Z462(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:M2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K0
       I2=K8*K0*K6
       I3=K2
       call EGEMM(I1,I2,I3,X129,D2,Z462)
       deallocate(D2)
C
       call
     & sum136245(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z462,-0.500)
       call
     & sum135246(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z462, 0.500)
       deallocate(Z462)
       deallocate(X129)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:M1))
       call reorder2431(N0,M1,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N0,N2,N0,M1,S599,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M2,N3,M1,N2,t2C,D2)
       allocate(S619(M2+1:N3,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S619)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N2,M2,N3,N0,M1,M1,N2,X3,S619, 1.000)
       deallocate(S619)
C
       call sumx2134(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,M2,N3,N0,M1,M1,N2,X3,VCHHHP, 1.000)
C
       allocate(D2(N0+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,M2,N2,M2,M1,N2,t2C,D2)
       allocate(Z3(N2+1:M2,N2+1:M2,M1+1:N2,M2+1:N3,N0+1:M1,M1+1:N2))
       I1=K8*K5*K6
       I2=K8*K0*K0
       I3=K2
       call EGEMM(I1,I2,I3,X3,D2,Z3)
       deallocate(D2)
C
       call
     & sum235146(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z3, 1.000)
       call
     & sum236145(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z3,-1.000)
       deallocate(Z3)
       deallocate(X3)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:M1))
       call reorder3421(N0,M1,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N0,N2,N0,M1,S599,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S617(N2+1:M2,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S617)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N2,N2,M2,N0,M1,M1,N2,X128,S617,-2.000)
       deallocate(S617)
C
       allocate(D2(N0+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,M2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(Z456(M2+1:N3,N2+1:M2,M1+1:N2,N2+1:M2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K0
       I2=K8*K0*K6
       I3=K2
       call EGEMM(I1,I2,I3,X128,D2,Z456)
       deallocate(D2)
C
       call
     & sum126345(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z456,-0.500)
       call
     & sum125346(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z456, 0.500)
       deallocate(Z456)
       deallocate(X128)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:M1))
       call reorder3241(N0,M1,N0,N2,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,N0,M1,S599,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S646(N2+1:M2,N0+1:N2,N2+1:N3,N0+1:M1))
       I1=K5*K4*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S646)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder2314(N2,M2,N0,N2,N2,N3,N0,M1,
     & N0,N2,N2,N3,N2,M2,N0,M1,S646,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S647(M2+1:N3,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S647)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,N0,M1,X71,S647, 1.000)
       deallocate(S647)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:M1))
       call reorder3241(N0,M1,N0,N2,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,N0,M1,S599,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S648(N2+1:M2,N0+1:N2,N2+1:N3,N0+1:M1))
       I1=K5*K4*K2
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S648)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder2314(N2,M2,N0,N2,N2,N3,N0,M1,
     & N0,N2,N2,N3,N2,M2,N0,M1,S648,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S649(M2+1:N3,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S649)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,N0,M1,X72,S649, 1.000)
       deallocate(S649)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,N0,M1,S599,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,M2,N2,M2,t2C,D2)
       allocate(S600(N2+1:M2,N2+1:M2,N2+1:N3,N0+1:M1))
       I1=K5*K4
       I2=K0*K0
       I3=K2*K2
       call EGEMM(I1,I2,I3,D1,D2,S600)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N2,N3,N2,M2,N2,M2,N0,M1,X7,S600,-0.500)
       deallocate(S600)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder2314(N2,M2,N0,N2,N2,N3,N0,M1,
     & N0,N2,N2,N3,N2,M2,N0,M1,S648,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S650(N2+1:M2,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S650)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,N2,M2,N0,M1,X7,S650,-1.000)
       deallocate(S650)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S602(N2+1:M2,N0+1:M1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K5*K0
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S602)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3412(N2,M2,N0,M1,N0,N2,N2,N3,
     & N0,N2,N2,N3,N2,M2,N0,M1,S602,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S607(N2+1:M2,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S607)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,N2,M2,N0,M1,X7,S607,-1.000)
       deallocate(S607)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3412(N2,M2,N0,M1,N0,N2,N2,N3,
     & N0,N2,N2,N3,N2,M2,N0,M1,S602,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S608(N2+1:M2,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K0
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S608)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,N3,N2,M2,N2,M2,N0,M1,X7,S608, 1.000)
       deallocate(S608)
C
       call sumx2314(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N2,M2,N2,M2,N0,M1,X7,VCHPPP, 1.000)
C
       allocate(D2(N2+1:N3,M2+1:N3,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,M2,N3,M1,N2,M1,N2,t2C,D2)
       allocate(Z8(M2+1:N3,M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K0
       I2=K8*K8*K6
       I3=K4
       call EGEMM(I1,I2,I3,X7,D2,Z8)
       deallocate(D2)
C
       call
     & sum156234(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z8,-1.000)
       deallocate(Z8)
       deallocate(X7)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3412(N2,M2,N0,M1,N0,N2,N2,N3,
     & N0,N2,N2,N3,N2,M2,N0,M1,S602,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S604(M2+1:N3,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S604)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,N0,M1,X71,S604, 1.000)
       deallocate(S604)
C
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,M1,N2,M1,N2,t2C,D2)
       allocate(Z95(N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K6
       I2=K8*K8*K0
       I3=K4
       call EGEMM(I1,I2,I3,X71,D2,Z95)
       deallocate(D2)
C
       call
     & sum356124(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z95, 1.000)
       deallocate(Z95)
       deallocate(X71)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3412(N2,M2,N0,M1,N0,N2,N2,N3,
     & N0,N2,N2,N3,N2,M2,N0,M1,S602,D1)
       allocate(B2(N0+1:N2,M2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,M2,N3,t1B,B2)
       allocate(S603(M2+1:N3,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K6
       I3=K2
       call EGEMM(I1,I2,I3,D1,B2,S603)
       deallocate(D1)
       deallocate(B2)
       deallocate(S602)
C
       call
     & sum2134(N2,N3,M2,N3,N2,M2,N0,M1,X72,S603, 1.000)
       deallocate(S603)
C
       allocate(D2(N2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,M1,N2,M1,N2,t2C,D2)
       allocate(Z96(N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K6
       I2=K8*K8*K0
       I3=K4
       call EGEMM(I1,I2,I3,X72,D2,Z96)
       deallocate(D2)
C
       call
     & sum256134(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z96,-1.000)
       deallocate(Z96)
       deallocate(X72)
C
       allocate(D1(N2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,M2,N3,N2,M2,M1,N2,VCHPPP,D1)
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,N0,M1,M1,N2,t2C,D2)
       allocate(Z5(N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K6
       I2=K8*K5*K0
       I3=K4
       call EGEMM(I1,I2,I3,D1,D2,Z5)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum345126(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z5,-1.000)
       call
     & sum245136(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z5, 1.000)
       call
     & sum346125(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z5, 1.000)
       call
     & sum246135(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z5,-1.000)
       deallocate(Z5)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder4132(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,M1,M2,N3,N2,M2,M1,N2,VCHPHP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z44(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z44)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z44, 1.000)
       call
     & sum124536(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z44,-1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z44,-1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z44, 1.000)
       deallocate(Z44)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder4132(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,M2,N3,N2,M2,M1,N2,VCHPHP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder512346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z45(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z45)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z45,-1.000)
       call
     & sum124536(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z45, 1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z45, 1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z45,-1.000)
       deallocate(Z45)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder4132(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,M1,N2,M2,N2,M2,M1,N2,VCHPHP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,M2,N3,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z46(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z46)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z46,-1.000)
       call
     & sum124536(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z46, 1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z46, 1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z46,-1.000)
       deallocate(Z46)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder4132(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,N2,M2,N2,M2,M1,N2,VCHPHP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder521346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,M2,N3,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z47(M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K8*K5*K0*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z47)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z47, 1.000)
       call
     & sum124536(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z47,-1.000)
       call
     & sum134625(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z47,-1.000)
       call
     & sum124635(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z47, 1.000)
       deallocate(Z47)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder4132(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,M1,M2,N3,N2,M2,N0,M1,VCHPHP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z52(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z52)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z52, 1.000)
       call
     & sum125634(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z52,-1.000)
       deallocate(Z52)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder4132(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,M2,N3,N2,M2,N0,M1,VCHPHP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z53(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z53)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z53, 1.000)
       call
     & sum125634(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z53,-1.000)
       deallocate(Z53)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder4132(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,M1,N2,M2,N2,M2,N0,M1,VCHPHP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z54(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z54)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z54,-1.000)
       call
     & sum125634(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z54, 1.000)
       deallocate(Z54)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder4132(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,N2,M2,N2,M2,N0,M1,VCHPHP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2))
       call reorder421356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,M2,N3,N2,M2,M1,N2,M1,N2,t3D,F2)
       allocate(Z55(M2+1:N3,N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K8*K0*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z55)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z55,-1.000)
       call
     & sum125634(M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,V3D,Z55, 1.000)
       deallocate(Z55)
C
       call sumx3(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,M1,N2,HT3D,V3D,1.0)
       deallocate(V3D)
C
       end
