       subroutine t3A110110_update(N0,N1,N2,N3,HT3A,shift,
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
       real*8,allocatable::Q11(:,:)
       real*8,allocatable::Q12(:,:)
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
       real*8,allocatable::Q13(:,:)
       real*8,allocatable::Q14(:,:)
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
       real*8,allocatable::Q15(:,:)
       real*8,allocatable::Q16(:,:)
       real*8,allocatable::S96(:,:,:,:)
       real*8,allocatable::S97(:,:,:,:)
       real*8,allocatable::S98(:,:,:,:)
       real*8,allocatable::S99(:,:,:,:)
       real*8,allocatable::S100(:,:,:,:)
       real*8,allocatable::S101(:,:,:,:)
       real*8,allocatable::S102(:,:,:,:)
       real*8,allocatable::S103(:,:,:,:)
       real*8,allocatable::Q17(:,:)
       real*8,allocatable::Q18(:,:)
       real*8,allocatable::S104(:,:,:,:)
       real*8,allocatable::S105(:,:,:,:)
       real*8,allocatable::Q19(:,:)
       real*8,allocatable::Q20(:,:)
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
       real*8,allocatable::Q21(:,:)
       real*8,allocatable::Q22(:,:)
       real*8,allocatable::Q23(:,:)
       real*8,allocatable::Q24(:,:)
       real*8,allocatable::Q25(:,:)
       real*8,allocatable::Q26(:,:)
       real*8,allocatable::Q27(:,:)
       real*8,allocatable::Q28(:,:)
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
       real*8,allocatable::S268(:,:,:,:)
       real*8,allocatable::S269(:,:,:,:)
       real*8,allocatable::S270(:,:,:,:)
       real*8,allocatable::S271(:,:,:,:)
       real*8,allocatable::S272(:,:,:,:)
       real*8,allocatable::S273(:,:,:,:)
       real*8,allocatable::S274(:,:,:,:)
       real*8,allocatable::S275(:,:,:,:)
       real*8,allocatable::S276(:,:,:,:)
       real*8,allocatable::Q31(:,:)
       real*8,allocatable::Q32(:,:)
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
       real*8,allocatable::Q33(:,:)
       real*8,allocatable::Q34(:,:)
       real*8,allocatable::S305(:,:,:,:)
       real*8,allocatable::S306(:,:,:,:)
       real*8,allocatable::S307(:,:,:,:)
       real*8,allocatable::S308(:,:,:,:)
       real*8,allocatable::S309(:,:,:,:)
       real*8,allocatable::S310(:,:,:,:)
       real*8,allocatable::Q35(:,:)
       real*8,allocatable::Q36(:,:)
       real*8,allocatable::S311(:,:,:,:)
       real*8,allocatable::S312(:,:,:,:)
       real*8,allocatable::Q37(:,:)
       real*8,allocatable::Q38(:,:)
       real*8,allocatable::Q39(:,:)
       real*8,allocatable::Q40(:,:)
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
       real*8,allocatable::S412(:,:,:,:)
       real*8,allocatable::S413(:,:,:,:)
       real*8,allocatable::S414(:,:,:,:)
       real*8,allocatable::S415(:,:,:,:)
       real*8,allocatable::S416(:,:,:,:)
       real*8,allocatable::S417(:,:,:,:)
       real*8,allocatable::S418(:,:,:,:)
       real*8,allocatable::S419(:,:,:,:)
       real*8,allocatable::S420(:,:,:,:)
       real*8,allocatable::S421(:,:,:,:)
       real*8,allocatable::S422(:,:,:,:)
       real*8,allocatable::S423(:,:,:,:)
       real*8,allocatable::S424(:,:,:,:)
       real*8,allocatable::S425(:,:,:,:)
       real*8,allocatable::S426(:,:,:,:)
       real*8,allocatable::S427(:,:,:,:)
       real*8,allocatable::S428(:,:,:,:)
       real*8,allocatable::S429(:,:,:,:)
       real*8,allocatable::S430(:,:,:,:)
       real*8,allocatable::S431(:,:,:,:)
       real*8,allocatable::S432(:,:,:,:)
       real*8,allocatable::S433(:,:,:,:)
       real*8,allocatable::S434(:,:,:,:)
       real*8,allocatable::Q41(:,:)
       real*8,allocatable::Q42(:,:)
       real*8,allocatable::S435(:,:,:,:)
       real*8,allocatable::S436(:,:,:,:)
       real*8,allocatable::S437(:,:,:,:)
       real*8,allocatable::S438(:,:,:,:)
       real*8,allocatable::S439(:,:,:,:)
       real*8,allocatable::S440(:,:,:,:)
       real*8,allocatable::S441(:,:,:,:)
       real*8,allocatable::S442(:,:,:,:)
       real*8,allocatable::S443(:,:,:,:)
       real*8,allocatable::S444(:,:,:,:)
       real*8,allocatable::S445(:,:,:,:)
       real*8,allocatable::S446(:,:,:,:)
       real*8,allocatable::Q43(:,:)
       real*8,allocatable::Q44(:,:)
       real*8,allocatable::Q45(:,:)
       real*8,allocatable::Q46(:,:)
       real*8,allocatable::Q47(:,:)
       real*8,allocatable::Q48(:,:)
       real*8,allocatable::Q49(:,:)
       real*8,allocatable::Q50(:,:)
       real*8,allocatable::S447(:,:,:,:)
       real*8,allocatable::S448(:,:,:,:)
       real*8,allocatable::S449(:,:,:,:)
       real*8,allocatable::S450(:,:,:,:)
       real*8,allocatable::S451(:,:,:,:)
       real*8,allocatable::S452(:,:,:,:)
       real*8,allocatable::S453(:,:,:,:)
       real*8,allocatable::S454(:,:,:,:)
       real*8,allocatable::S455(:,:,:,:)
       real*8,allocatable::S456(:,:,:,:)
       real*8,allocatable::S457(:,:,:,:)
       real*8,allocatable::S458(:,:,:,:)
       real*8,allocatable::S459(:,:,:,:)
       real*8,allocatable::S460(:,:,:,:)
       real*8,allocatable::S461(:,:,:,:)
       real*8,allocatable::S462(:,:,:,:)
       real*8,allocatable::S463(:,:,:,:)
       real*8,allocatable::S464(:,:,:,:)
       real*8,allocatable::S465(:,:,:,:)
       real*8,allocatable::S466(:,:,:,:)
       real*8,allocatable::S467(:,:,:,:)
       real*8,allocatable::S468(:,:,:,:)
       real*8,allocatable::S469(:,:,:,:)
       real*8,allocatable::S470(:,:,:,:)
       real*8,allocatable::S471(:,:,:,:)
       real*8,allocatable::S473(:,:,:,:)
       real*8,allocatable::S474(:,:,:,:)
       real*8,allocatable::S472(:,:,:,:)
       real*8,allocatable::S475(:,:,:,:)
       real*8,allocatable::S477(:,:,:,:)
       real*8,allocatable::S478(:,:,:,:)
       real*8,allocatable::S476(:,:,:,:)
       real*8,allocatable::S479(:,:,:,:)
       real*8,allocatable::S483(:,:,:,:)
       real*8,allocatable::S480(:,:,:,:)
       real*8,allocatable::S481(:,:,:,:)
       real*8,allocatable::S482(:,:,:,:)
       real*8,allocatable::S484(:,:,:,:)
       real*8,allocatable::S494(:,:,:,:)
       real*8,allocatable::S497(:,:,:,:)
       real*8,allocatable::S498(:,:,:,:)
       real*8,allocatable::S485(:,:,:,:)
       real*8,allocatable::S490(:,:,:,:)
       real*8,allocatable::S493(:,:,:,:)
       real*8,allocatable::S491(:,:,:,:)
       real*8,allocatable::S492(:,:,:,:)
       real*8,allocatable::S486(:,:,:,:)
       real*8,allocatable::S487(:,:,:,:)
       real*8,allocatable::S499(:,:,:,:)
       real*8,allocatable::S501(:,:,:,:)
       real*8,allocatable::S495(:,:,:,:)
       real*8,allocatable::S488(:,:,:,:)
       real*8,allocatable::S500(:,:,:,:)
       real*8,allocatable::S496(:,:,:,:)
       real*8,allocatable::S502(:,:,:,:)
       real*8,allocatable::S489(:,:,:,:)
       real*8,allocatable::S503(:,:,:,:)
       real*8,allocatable::S507(:,:,:,:)
       real*8,allocatable::S504(:,:,:,:)
       real*8,allocatable::S505(:,:,:,:)
       real*8,allocatable::S506(:,:,:,:)
       real*8,allocatable::S508(:,:,:,:)
       real*8,allocatable::S510(:,:,:,:)
       real*8,allocatable::S509(:,:,:,:)
       real*8,allocatable::S511(:,:,:,:)
       real*8,allocatable::S515(:,:,:,:)
       real*8,allocatable::S512(:,:,:,:)
       real*8,allocatable::S513(:,:,:,:)
       real*8,allocatable::S516(:,:,:,:)
       real*8,allocatable::S514(:,:,:,:)
       real*8,allocatable::S517(:,:,:,:)
       real*8,allocatable::S523(:,:,:,:)
       real*8,allocatable::S518(:,:,:,:)
       real*8,allocatable::S521(:,:,:,:)
       real*8,allocatable::S522(:,:,:,:)
       real*8,allocatable::S519(:,:,:,:)
       real*8,allocatable::S524(:,:,:,:)
       real*8,allocatable::S520(:,:,:,:)
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
       real*8,allocatable::S537(:,:,:,:)
       real*8,allocatable::S538(:,:,:,:)
       real*8,allocatable::S539(:,:,:,:)
       real*8,allocatable::S540(:,:,:,:)
       real*8,allocatable::S541(:,:,:,:)
       real*8,allocatable::S542(:,:,:,:)
       real*8,allocatable::S543(:,:,:,:)
       real*8,allocatable::S544(:,:,:,:)
       real*8,allocatable::Q51(:,:)
       real*8,allocatable::Q55(:,:)
       real*8,allocatable::Q52(:,:)
       real*8,allocatable::S545(:,:,:,:)
       real*8,allocatable::S553(:,:,:,:)
       real*8,allocatable::S546(:,:,:,:)
       real*8,allocatable::S549(:,:,:,:)
       real*8,allocatable::S555(:,:,:,:)
       real*8,allocatable::S550(:,:,:,:)
       real*8,allocatable::S547(:,:,:,:)
       real*8,allocatable::S554(:,:,:,:)
       real*8,allocatable::S548(:,:,:,:)
       real*8,allocatable::S551(:,:,:,:)
       real*8,allocatable::S556(:,:,:,:)
       real*8,allocatable::S552(:,:,:,:)
       real*8,allocatable::S557(:,:,:,:)
       real*8,allocatable::S558(:,:,:,:)
       real*8,allocatable::S559(:,:,:,:)
       real*8,allocatable::S560(:,:,:,:)
       real*8,allocatable::S561(:,:,:,:)
       real*8,allocatable::S562(:,:,:,:)
       real*8,allocatable::S563(:,:,:,:)
       real*8,allocatable::S564(:,:,:,:)
       real*8,allocatable::Q53(:,:)
       real*8,allocatable::Q56(:,:)
       real*8,allocatable::Q54(:,:)
       real*8,allocatable::S565(:,:,:,:)
       real*8,allocatable::S566(:,:,:,:)
       real*8,allocatable::S567(:,:,:,:)
       real*8,allocatable::S577(:,:,:,:)
       real*8,allocatable::S568(:,:,:,:)
       real*8,allocatable::S571(:,:,:,:)
       real*8,allocatable::S572(:,:,:,:)
       real*8,allocatable::S573(:,:,:,:)
       real*8,allocatable::S574(:,:,:,:)
       real*8,allocatable::S575(:,:,:,:)
       real*8,allocatable::S576(:,:,:,:)
       real*8,allocatable::Q57(:,:)
       real*8,allocatable::Q61(:,:)
       real*8,allocatable::Q63(:,:)
       real*8,allocatable::Q58(:,:)
       real*8,allocatable::S569(:,:,:,:)
       real*8,allocatable::S570(:,:,:,:)
       real*8,allocatable::S578(:,:,:,:)
       real*8,allocatable::Q59(:,:)
       real*8,allocatable::Q60(:,:)
       real*8,allocatable::Q64(:,:)
       real*8,allocatable::Q62(:,:)
       real*8,allocatable::S579(:,:,:,:)
       real*8,allocatable::S587(:,:,:,:)
       real*8,allocatable::S591(:,:,:,:)
       real*8,allocatable::S580(:,:,:,:)
       real*8,allocatable::S585(:,:,:,:)
       real*8,allocatable::S590(:,:,:,:)
       real*8,allocatable::S594(:,:,:,:)
       real*8,allocatable::S586(:,:,:,:)
       real*8,allocatable::S583(:,:,:,:)
       real*8,allocatable::S584(:,:,:,:)
       real*8,allocatable::S593(:,:,:,:)
       real*8,allocatable::S589(:,:,:,:)
       real*8,allocatable::S581(:,:,:,:)
       real*8,allocatable::S582(:,:,:,:)
       real*8,allocatable::S588(:,:,:,:)
       real*8,allocatable::S592(:,:,:,:)
       real*8,allocatable::S595(:,:,:,:)
       real*8,allocatable::S603(:,:,:,:)
       real*8,allocatable::S607(:,:,:,:)
       real*8,allocatable::S596(:,:,:,:)
       real*8,allocatable::S601(:,:,:,:)
       real*8,allocatable::S606(:,:,:,:)
       real*8,allocatable::S610(:,:,:,:)
       real*8,allocatable::S602(:,:,:,:)
       real*8,allocatable::S599(:,:,:,:)
       real*8,allocatable::S600(:,:,:,:)
       real*8,allocatable::S609(:,:,:,:)
       real*8,allocatable::S605(:,:,:,:)
       real*8,allocatable::S597(:,:,:,:)
       real*8,allocatable::S598(:,:,:,:)
       real*8,allocatable::S604(:,:,:,:)
       real*8,allocatable::S608(:,:,:,:)
       real*8,allocatable::Q65(:,:)
       real*8,allocatable::Q69(:,:)
       real*8,allocatable::Q66(:,:)
       real*8,allocatable::Q67(:,:)
       real*8,allocatable::Q70(:,:)
       real*8,allocatable::Q68(:,:)
       real*8,allocatable::Q71(:,:)
       real*8,allocatable::Q75(:,:)
       real*8,allocatable::Q77(:,:)
       real*8,allocatable::Q72(:,:)
       real*8,allocatable::Q73(:,:)
       real*8,allocatable::Q74(:,:)
       real*8,allocatable::Q78(:,:)
       real*8,allocatable::Q76(:,:)
       real*8,allocatable::S611(:,:,:,:)
       real*8,allocatable::S613(:,:,:,:)
       real*8,allocatable::S614(:,:,:,:)
       real*8,allocatable::S615(:,:,:,:)
       real*8,allocatable::S616(:,:,:,:)
       real*8,allocatable::S617(:,:,:,:)
       real*8,allocatable::S618(:,:,:,:)
       real*8,allocatable::S619(:,:,:,:)
       real*8,allocatable::S620(:,:,:,:)
       real*8,allocatable::S695(:,:,:,:)
       real*8,allocatable::S696(:,:,:,:)
       real*8,allocatable::S699(:,:,:,:)
       real*8,allocatable::S700(:,:,:,:)
       real*8,allocatable::S703(:,:,:,:)
       real*8,allocatable::S704(:,:,:,:)
       real*8,allocatable::S705(:,:,:,:)
       real*8,allocatable::S706(:,:,:,:)
       real*8,allocatable::S612(:,:,:,:)
       real*8,allocatable::S697(:,:,:,:)
       real*8,allocatable::S698(:,:,:,:)
       real*8,allocatable::S701(:,:,:,:)
       real*8,allocatable::S702(:,:,:,:)
       real*8,allocatable::S707(:,:,:,:)
       real*8,allocatable::S634(:,:,:,:)
       real*8,allocatable::S651(:,:,:,:)
       real*8,allocatable::S654(:,:,:,:)
       real*8,allocatable::S655(:,:,:,:)
       real*8,allocatable::S635(:,:,:,:)
       real*8,allocatable::S629(:,:,:,:)
       real*8,allocatable::S636(:,:,:,:)
       real*8,allocatable::S630(:,:,:,:)
       real*8,allocatable::S640(:,:,:,:)
       real*8,allocatable::S621(:,:,:,:)
       real*8,allocatable::S644(:,:,:,:)
       real*8,allocatable::S622(:,:,:,:)
       real*8,allocatable::S623(:,:,:,:)
       real*8,allocatable::S624(:,:,:,:)
       real*8,allocatable::S642(:,:,:,:)
       real*8,allocatable::S708(:,:,:,:)
       real*8,allocatable::S709(:,:,:,:)
       real*8,allocatable::S710(:,:,:,:)
       real*8,allocatable::S711(:,:,:,:)
       real*8,allocatable::S643(:,:,:,:)
       real*8,allocatable::S712(:,:,:,:)
       real*8,allocatable::Q79(:,:)
       real*8,allocatable::S657(:,:,:,:)
       real*8,allocatable::S659(:,:,:,:)
       real*8,allocatable::S641(:,:,:,:)
       real*8,allocatable::S633(:,:,:,:)
       real*8,allocatable::S653(:,:,:,:)
       real*8,allocatable::S639(:,:,:,:)
       real*8,allocatable::S660(:,:,:,:)
       real*8,allocatable::S663(:,:,:,:)
       real*8,allocatable::S664(:,:,:,:)
       real*8,allocatable::S665(:,:,:,:)
       real*8,allocatable::S666(:,:,:,:)
       real*8,allocatable::S661(:,:,:,:)
       real*8,allocatable::S662(:,:,:,:)
       real*8,allocatable::S669(:,:,:,:)
       real*8,allocatable::S672(:,:,:,:)
       real*8,allocatable::S670(:,:,:,:)
       real*8,allocatable::S667(:,:,:,:)
       real*8,allocatable::S668(:,:,:,:)
       real*8,allocatable::S671(:,:,:,:)
       real*8,allocatable::S675(:,:,:,:)
       real*8,allocatable::S686(:,:,:,:)
       real*8,allocatable::S688(:,:,:,:)
       real*8,allocatable::S676(:,:,:,:)
       real*8,allocatable::S677(:,:,:,:)
       real*8,allocatable::S678(:,:,:,:)
       real*8,allocatable::S679(:,:,:,:)
       real*8,allocatable::S680(:,:,:,:)
       real*8,allocatable::S673(:,:,:,:)
       real*8,allocatable::S687(:,:,:,:)
       real*8,allocatable::S674(:,:,:,:)
       real*8,allocatable::S685(:,:,:,:)
       real*8,allocatable::S683(:,:,:,:)
       real*8,allocatable::S684(:,:,:,:)
       real*8,allocatable::Q80(:,:)
       real*8,allocatable::S690(:,:,:,:)
       real*8,allocatable::S691(:,:,:,:)
       real*8,allocatable::S692(:,:,:,:)
       real*8,allocatable::S693(:,:,:,:)
       real*8,allocatable::S694(:,:,:,:)
       real*8,allocatable::S689(:,:,:,:)
       real*8,allocatable::S645(:,:,:,:)
       real*8,allocatable::S646(:,:,:,:)
       real*8,allocatable::S647(:,:,:,:)
       real*8,allocatable::S648(:,:,:,:)
       real*8,allocatable::S649(:,:,:,:)
       real*8,allocatable::S650(:,:,:,:)
       real*8,allocatable::S625(:,:,:,:)
       real*8,allocatable::S631(:,:,:,:)
       real*8,allocatable::S626(:,:,:,:)
       real*8,allocatable::S637(:,:,:,:)
       real*8,allocatable::S656(:,:,:,:)
       real*8,allocatable::S652(:,:,:,:)
       real*8,allocatable::S638(:,:,:,:)
       real*8,allocatable::S658(:,:,:,:)
       real*8,allocatable::S627(:,:,:,:)
       real*8,allocatable::S628(:,:,:,:)
       real*8,allocatable::S632(:,:,:,:)
       real*8,allocatable::S681(:,:,:,:)
       real*8,allocatable::S682(:,:,:,:)
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
       real*8,allocatable::X13(:,:)
       real*8,allocatable::Z13(:,:,:,:,:,:)
       real*8,allocatable::X14(:,:)
       real*8,allocatable::Z14(:,:,:,:,:,:)
       real*8,allocatable::X15(:,:)
       real*8,allocatable::Z15(:,:,:,:,:,:)
       real*8,allocatable::X16(:,:)
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
       real*8,allocatable::Z68(:,:,:,:,:,:)
       real*8,allocatable::Z69(:,:,:,:,:,:)
       real*8,allocatable::Z70(:,:,:,:,:,:)
       real*8,allocatable::Z71(:,:,:,:,:,:)
       real*8,allocatable::X68(:,:,:,:)
       real*8,allocatable::Z72(:,:,:,:,:,:)
       real*8,allocatable::X69(:,:,:,:)
       real*8,allocatable::Z73(:,:,:,:,:,:)
       real*8,allocatable::X70(:,:,:,:)
       real*8,allocatable::Z74(:,:,:,:,:,:)
       real*8,allocatable::X71(:,:,:,:)
       real*8,allocatable::Z75(:,:,:,:,:,:)
       real*8,allocatable::X72(:,:,:,:)
       real*8,allocatable::Z76(:,:,:,:,:,:)
       real*8,allocatable::X73(:,:,:,:)
       real*8,allocatable::Z77(:,:,:,:,:,:)
       real*8,allocatable::X74(:,:,:,:)
       real*8,allocatable::Z78(:,:,:,:,:,:)
       real*8,allocatable::X75(:,:,:,:)
       real*8,allocatable::Z79(:,:,:,:,:,:)
       real*8,allocatable::Z80(:,:,:,:,:,:)
       real*8,allocatable::Z81(:,:,:,:,:,:)
       real*8,allocatable::Z82(:,:,:,:,:,:)
       real*8,allocatable::Z83(:,:,:,:,:,:)
       real*8,allocatable::X76(:,:,:,:)
       real*8,allocatable::Z87(:,:,:,:,:,:)
       real*8,allocatable::X77(:,:,:,:)
       real*8,allocatable::Z88(:,:,:,:,:,:)
       real*8,allocatable::X78(:,:,:,:)
       real*8,allocatable::Z89(:,:,:,:,:,:)
       real*8,allocatable::Z127(:,:,:,:,:,:)
       real*8,allocatable::Z128(:,:,:,:,:,:)
       real*8,allocatable::X79(:,:,:,:)
       real*8,allocatable::Z140(:,:,:,:,:,:)
       real*8,allocatable::X80(:,:,:,:)
       real*8,allocatable::Z141(:,:,:,:,:,:)
       real*8,allocatable::X81(:,:,:,:)
       real*8,allocatable::Z142(:,:,:,:,:,:)
       real*8,allocatable::X82(:,:,:,:)
       real*8,allocatable::Z143(:,:,:,:,:,:)
       real*8,allocatable::X83(:,:,:,:)
       real*8,allocatable::Z169(:,:,:,:,:,:)
       real*8,allocatable::X84(:,:,:,:)
       real*8,allocatable::Z170(:,:,:,:,:,:)
       real*8,allocatable::X85(:,:,:,:)
       real*8,allocatable::Z171(:,:,:,:,:,:)
       real*8,allocatable::X86(:,:,:,:)
       real*8,allocatable::Z172(:,:,:,:,:,:)
       real*8,allocatable::Z209(:,:,:,:,:,:)
       real*8,allocatable::Z210(:,:,:,:,:,:)
       real*8,allocatable::Z211(:,:,:,:,:,:)
       real*8,allocatable::Z212(:,:,:,:,:,:)
       real*8,allocatable::Z213(:,:,:,:,:,:)
       real*8,allocatable::Z214(:,:,:,:,:,:)
       real*8,allocatable::Z215(:,:,:,:,:,:)
       real*8,allocatable::Z216(:,:,:,:,:,:)
       real*8,allocatable::Z217(:,:,:,:,:,:)
       real*8,allocatable::Z218(:,:,:,:,:,:)
       real*8,allocatable::Z219(:,:,:,:,:,:)
       real*8,allocatable::Z220(:,:,:,:,:,:)
       real*8,allocatable::Z221(:,:,:,:,:,:)
       real*8,allocatable::Z222(:,:,:,:,:,:)
       real*8,allocatable::Z223(:,:,:,:,:,:)
       real*8,allocatable::Z224(:,:,:,:,:,:)
       real*8,allocatable::Z225(:,:,:,:,:,:)
       real*8,allocatable::Z226(:,:,:,:,:,:)
       real*8,allocatable::Z227(:,:,:,:,:,:)
       real*8,allocatable::Z228(:,:,:,:,:,:)
       real*8,allocatable::Z229(:,:,:,:,:,:)
       real*8,allocatable::Z230(:,:,:,:,:,:)
       real*8,allocatable::Z231(:,:,:,:,:,:)
       real*8,allocatable::Z232(:,:,:,:,:,:)
       real*8,allocatable::Z233(:,:,:,:,:,:)
       real*8,allocatable::Z234(:,:,:,:,:,:)
       real*8,allocatable::Z235(:,:,:,:,:,:)
       real*8,allocatable::Z236(:,:,:,:,:,:)
       real*8,allocatable::Z237(:,:,:,:,:,:)
       real*8,allocatable::Z238(:,:,:,:,:,:)
       real*8,allocatable::Z239(:,:,:,:,:,:)
       real*8,allocatable::Z240(:,:,:,:,:,:)
       real*8,allocatable::Z241(:,:,:,:,:,:)
       real*8,allocatable::Z242(:,:,:,:,:,:)
       real*8,allocatable::Z243(:,:,:,:,:,:)
       real*8,allocatable::Z244(:,:,:,:,:,:)
       real*8,allocatable::Z245(:,:,:,:,:,:)
       real*8,allocatable::Z246(:,:,:,:,:,:)
       real*8,allocatable::Z247(:,:,:,:,:,:)
       real*8,allocatable::Z248(:,:,:,:,:,:)
       real*8,allocatable::Z249(:,:,:,:,:,:)
       real*8,allocatable::Z250(:,:,:,:,:,:)
       real*8,allocatable::Z251(:,:,:,:,:,:)
       real*8,allocatable::Z252(:,:,:,:,:,:)
       real*8,allocatable::Z253(:,:,:,:,:,:)
       real*8,allocatable::Z254(:,:,:,:,:,:)
       real*8,allocatable::Z255(:,:,:,:,:,:)
       real*8,allocatable::Z256(:,:,:,:,:,:)
       real*8,allocatable::X87(:,:,:,:)
       real*8,allocatable::Z270(:,:,:,:,:,:)
       real*8,allocatable::X88(:,:,:,:)
       real*8,allocatable::Z294(:,:,:,:,:,:)
       real*8,allocatable::X89(:,:,:,:)
       real*8,allocatable::Z295(:,:,:,:,:,:)
       real*8,allocatable::X90(:,:,:,:)
       real*8,allocatable::Z297(:,:,:,:,:,:)
       real*8,allocatable::X91(:,:,:,:)
       real*8,allocatable::Z298(:,:,:,:,:,:)
       real*8,allocatable::X92(:,:,:,:)
       real*8,allocatable::Z320(:,:,:,:,:,:)
       real*8,allocatable::Z386(:,:,:,:,:,:)
       real*8,allocatable::Z387(:,:,:,:,:,:)
       real*8,allocatable::Z388(:,:,:,:,:,:)
       real*8,allocatable::Z389(:,:,:,:,:,:)
       real*8,allocatable::Z416(:,:,:,:,:,:)
       real*8,allocatable::Z417(:,:,:,:,:,:)
       real*8,allocatable::Z418(:,:,:,:,:,:)
       real*8,allocatable::Z419(:,:,:,:,:,:)
       real*8,allocatable::Z434(:,:,:,:,:,:)
       real*8,allocatable::Z435(:,:,:,:,:,:)
       real*8,allocatable::X93(:,:,:,:)
       real*8,allocatable::Z494(:,:,:,:,:,:)
       real*8,allocatable::X94(:,:,:,:)
       real*8,allocatable::Z495(:,:,:,:,:,:)
       real*8,allocatable::X95(:,:,:,:)
       real*8,allocatable::Z496(:,:,:,:,:,:)
       real*8,allocatable::X96(:,:,:,:)
       real*8,allocatable::Z497(:,:,:,:,:,:)
       real*8,allocatable::X97(:,:,:,:)
       real*8,allocatable::Z504(:,:,:,:,:,:)
       real*8,allocatable::X98(:,:,:,:)
       real*8,allocatable::Z505(:,:,:,:,:,:)
       real*8,allocatable::X99(:,:,:,:)
       real*8,allocatable::Z506(:,:,:,:,:,:)
       real*8,allocatable::X100(:,:,:,:)
       real*8,allocatable::Z507(:,:,:,:,:,:)
       real*8,allocatable::X101(:,:,:,:)
       real*8,allocatable::Z508(:,:,:,:,:,:)
       real*8,allocatable::X102(:,:,:,:)
       real*8,allocatable::Z509(:,:,:,:,:,:)
       real*8,allocatable::X103(:,:,:,:)
       real*8,allocatable::Z510(:,:,:,:,:,:)
       real*8,allocatable::X104(:,:,:,:)
       real*8,allocatable::Z511(:,:,:,:,:,:)
       real*8,allocatable::X105(:,:,:,:)
       real*8,allocatable::Z528(:,:,:,:,:,:)
       real*8,allocatable::X106(:,:,:,:)
       real*8,allocatable::Z529(:,:,:,:,:,:)
       real*8,allocatable::X107(:,:,:,:)
       real*8,allocatable::Z530(:,:,:,:,:,:)
       real*8,allocatable::X108(:,:,:,:)
       real*8,allocatable::Z531(:,:,:,:,:,:)
       real*8,allocatable::X109(:,:,:,:)
       real*8,allocatable::Z538(:,:,:,:,:,:)
       real*8,allocatable::X110(:,:,:,:)
       real*8,allocatable::Z539(:,:,:,:,:,:)
       real*8,allocatable::X111(:,:,:,:)
       real*8,allocatable::Z540(:,:,:,:,:,:)
       real*8,allocatable::X112(:,:,:,:)
       real*8,allocatable::Z541(:,:,:,:,:,:)
       real*8,allocatable::X113(:,:,:,:)
       real*8,allocatable::Z542(:,:,:,:,:,:)
       real*8,allocatable::X114(:,:,:,:)
       real*8,allocatable::Z543(:,:,:,:,:,:)
       real*8,allocatable::X115(:,:,:,:)
       real*8,allocatable::Z544(:,:,:,:,:,:)
       real*8,allocatable::X116(:,:,:,:)
       real*8,allocatable::Z545(:,:,:,:,:,:)
C
       allocate(V3A(M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M1+1:N1))
       V3A=0.0d0
C
       allocate(D1(N0+1:N1,N0+1:N1,M1+1:N1,M1+1:N1))
       call reorder3412(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,M1,N1,M1,N1,VAHHHH,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S1(M2+1:N3,N0+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S1)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X1(N0+1:N1,M2+1:N3,M1+1:N1,M1+1:N1))
       X1=0.0d0
       call
     & sum2134(N0,N1,M2,N3,M1,N1,M1,N1,X1,S1,-1.000)
       deallocate(S1)
C
       allocate(D1(N0+1:N1,N0+1:N1,M1+1:N1,M1+1:N1))
       call reorder3412(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,M1,N1,M1,N1,VAHHHH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S2(N1+1:M2,N0+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S2)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X2(N0+1:N1,N1+1:M2,M1+1:N1,M1+1:N1))
       X2=0.0d0
       call
     & sum2134(N0,N1,N1,M2,M1,N1,M1,N1,X2,S2,-1.000)
       deallocate(S2)
C
       allocate(D1(N0+1:N1,N0+1:N1,M1+1:N1,M1+1:N1))
       call reorder4312(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,M1,N1,M1,N1,VAHHHH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S3(N1+1:M2,N0+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S3)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X3(N0+1:N1,N1+1:M2,M1+1:N1,M1+1:N1))
       X3=0.0d0
       call
     & sum2134(N0,N1,N1,M2,M1,N1,M1,N1,X3,S3, 1.000)
       deallocate(S3)
C
       allocate(D1(N1+1:N3,N0+1:N1,M2+1:N3,M1+1:N1))
       call reorder1432(N1,N3,N0,N1,N1,N3,N0,N1,
     & N1,N3,N0,N1,M2,N3,M1,N1,VAHPHP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S4(M1+1:N1,N0+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S4)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X76(N0+1:N1,M2+1:N3,M1+1:N1,M1+1:N1))
       X76=0.0d0
       call
     & sum3124(N0,N1,M2,N3,M1,N1,M1,N1,X76,S4, 1.000)
       deallocate(S4)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:M2,M1+1:N1))
       call reorder1432(N1,N3,N0,N1,N1,N3,N0,N1,
     & N1,N3,N0,N1,N1,M2,M1,N1,VAHPHP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S5(M1+1:N1,N0+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S5)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X77(N0+1:N1,N1+1:M2,M1+1:N1,M1+1:N1))
       X77=0.0d0
       call
     & sum3124(N0,N1,N1,M2,M1,N1,M1,N1,X77,S5, 1.000)
       deallocate(S5)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:M2,M1+1:N1))
       call reorder1432(N1,N3,N0,N1,N1,N3,N0,N1,
     & N1,N3,N0,N1,N1,M2,M1,N1,VAHPHP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S6(M1+1:N1,N0+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S6)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X78(N0+1:N1,N1+1:M2,M1+1:N1,M1+1:N1))
       X78=0.0d0
       call
     & sum3124(N0,N1,N1,M2,M1,N1,M1,N1,X78,S6, 1.000)
       deallocate(S6)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder3412(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,M1,M1,N1,VAHHHH,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S7(M2+1:N3,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S7)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X4(N0+1:N1,M2+1:N3,N0+1:M1,M1+1:N1))
       X4=0.0d0
       call
     & sum2134(N0,N1,M2,N3,N0,M1,M1,N1,X4,S7,-1.000)
       deallocate(S7)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder3412(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,M1,M1,N1,VAHHHH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S8(N1+1:M2,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S8)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X5(N0+1:N1,N1+1:M2,N0+1:M1,M1+1:N1))
       X5=0.0d0
       call
     & sum2134(N0,N1,N1,M2,N0,M1,M1,N1,X5,S8,-1.000)
       deallocate(S8)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder4312(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,M1,M1,N1,VAHHHH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S9(N1+1:M2,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S9)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X6(N0+1:N1,N1+1:M2,N0+1:M1,M1+1:N1))
       X6=0.0d0
       call
     & sum2134(N0,N1,N1,M2,N0,M1,M1,N1,X6,S9, 1.000)
       deallocate(S9)
C
       allocate(D1(N1+1:N3,N0+1:N1,M2+1:N3,M1+1:N1))
       call reorder1432(N1,N3,N0,N1,N1,N3,N0,N1,
     & N1,N3,N0,N1,M2,N3,M1,N1,VAHPHP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S10(N0+1:M1,N0+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S10)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N1,M2,N3,N0,M1,M1,N1,X4,S10, 1.000)
       deallocate(S10)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:M2,M1+1:N1))
       call reorder1432(N1,N3,N0,N1,N1,N3,N0,N1,
     & N1,N3,N0,N1,N1,M2,M1,N1,VAHPHP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S11(N0+1:M1,N0+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S11)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N1,N1,M2,N0,M1,M1,N1,X5,S11, 1.000)
       deallocate(S11)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:M2,M1+1:N1))
       call reorder1432(N1,N3,N0,N1,N1,N3,N0,N1,
     & N1,N3,N0,N1,N1,M2,M1,N1,VAHPHP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S12(N0+1:M1,N0+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S12)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N1,N1,M2,N0,M1,M1,N1,X6,S12, 1.000)
       deallocate(S12)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder4132(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N1,VAHPHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S13(N1+1:M2,N1+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S13)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X7(N1+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       X7=0.0d0
       call
     & sum3124(N1,N3,M2,N3,N1,M2,M1,N1,X7,S13,-1.000)
       deallocate(S13)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder4132(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N1,VAHPHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S14(N1+1:M2,N1+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S14)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X8(N1+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       X8=0.0d0
       call
     & sum3124(N1,N3,M2,N3,N1,M2,M1,N1,X8,S14,-1.000)
       deallocate(S14)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder4132(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,VAHPHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S15(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S15)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,N1,M2,M1,N1,X7,S15, 1.000)
       deallocate(S15)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder4132(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,VAHPHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S16(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S16)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,N1,M2,M1,N1,X8,S16, 1.000)
       deallocate(S16)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder4132(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,VAHPHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S17(N1+1:M2,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S17)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X9(N1+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       X9=0.0d0
       call
     & sum3124(N1,N3,N1,M2,N1,M2,M1,N1,X9,S17,-1.000)
       deallocate(S17)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder4132(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,VAHPHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S18(N1+1:M2,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S18)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,N1,M2,N1,M2,M1,N1,X9,S18, 1.000)
       deallocate(S18)
C
       allocate(D1(N1+1:N3,N0+1:N1,M2+1:N3,N0+1:M1))
       call reorder1432(N1,N3,N0,N1,N1,N3,N0,N1,
     & N1,N3,N0,N1,M2,N3,N0,M1,VAHPHP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S19(M1+1:N1,N0+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S19)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,N1,M2,N3,N0,M1,M1,N1,X4,S19,-1.000)
       deallocate(S19)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:M2,N0+1:M1))
       call reorder1432(N1,N3,N0,N1,N1,N3,N0,N1,
     & N1,N3,N0,N1,N1,M2,N0,M1,VAHPHP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S20(M1+1:N1,N0+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S20)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,N1,N1,M2,N0,M1,M1,N1,X5,S20,-1.000)
       deallocate(S20)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:M2,N0+1:M1))
       call reorder1432(N1,N3,N0,N1,N1,N3,N0,N1,
     & N1,N3,N0,N1,N1,M2,N0,M1,VAHPHP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S21(M1+1:N1,N0+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S21)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,N1,N1,M2,N0,M1,M1,N1,X6,S21,-1.000)
       deallocate(S21)
C
       allocate(D1(N1+1:N3,N1+1:N3,M2+1:N3,N1+1:M2))
       call reorder1234(N1,N3,N1,N3,N1,N3,N1,M2,
     & N1,N3,N1,N3,M2,N3,N1,M2,VAAPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S22(M1+1:N1,N1+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6*K3
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S22)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N1,N3,M2,N3,N1,M2,M1,N1,X7,S22,-1.000)
       deallocate(S22)
C
       allocate(D1(N1+1:N3,N1+1:N3,M2+1:N3,N1+1:M2))
       call reorder1234(N1,N3,N1,N3,N1,N3,N1,M2,
     & N1,N3,N1,N3,M2,N3,N1,M2,VAAPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S23(M1+1:N1,N1+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6*K3
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S23)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N1,N3,M2,N3,N1,M2,M1,N1,X8,S23,-1.000)
       deallocate(S23)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:M2,N1+1:M2))
       call reorder1234(N1,N3,N1,N3,N1,N3,N1,M2,
     & N1,N3,N1,N3,N1,M2,N1,M2,VAAPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S24(M1+1:N1,N1+1:N3,N1+1:M2,N1+1:M2))
       I1=K9*K9*K3
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S24)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N1,N3,N1,M2,N1,M2,M1,N1,X9,S24,-1.000)
       deallocate(S24)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder4132(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,VAHPHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S25(N1+1:M2,N1+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S25)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X10(N1+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       X10=0.0d0
       call
     & sum3124(N1,N3,M2,N3,N1,M2,N0,M1,X10,S25,-1.000)
       deallocate(S25)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder4132(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,VAHPHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S26(N1+1:M2,N1+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S26)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X11(N1+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       X11=0.0d0
       call
     & sum3124(N1,N3,M2,N3,N1,M2,N0,M1,X11,S26,-1.000)
       deallocate(S26)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder4132(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,VAHPHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S27(M2+1:N3,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S27)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,N1,M2,N0,M1,X10,S27, 1.000)
       deallocate(S27)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder4132(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,VAHPHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S28(M2+1:N3,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S28)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,N1,M2,N0,M1,X11,S28, 1.000)
       deallocate(S28)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder4132(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,VAHPHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S29(N1+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S29)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X12(N1+1:N3,N1+1:M2,N1+1:M2,N0+1:M1))
       X12=0.0d0
       call
     & sum3124(N1,N3,N1,M2,N1,M2,N0,M1,X12,S29,-1.000)
       deallocate(S29)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder4132(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,VAHPHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S30(N1+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S30)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,N1,M2,N1,M2,N0,M1,X12,S30, 1.000)
       deallocate(S30)
C
       allocate(D1(N1+1:N3,N1+1:N3,M2+1:N3,N1+1:M2))
       call reorder1234(N1,N3,N1,N3,N1,N3,N1,M2,
     & N1,N3,N1,N3,M2,N3,N1,M2,VAAPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S31(N0+1:M1,N1+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6*K3
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S31)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N1,N3,M2,N3,N1,M2,N0,M1,X10,S31,-1.000)
       deallocate(S31)
C
       allocate(D1(N1+1:N3,N1+1:N3,M2+1:N3,N1+1:M2))
       call reorder1234(N1,N3,N1,N3,N1,N3,N1,M2,
     & N1,N3,N1,N3,M2,N3,N1,M2,VAAPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S32(N0+1:M1,N1+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6*K3
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S32)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N1,N3,M2,N3,N1,M2,N0,M1,X11,S32,-1.000)
       deallocate(S32)
C
       allocate(D1(N1+1:N3,N1+1:N3,N1+1:M2,N1+1:M2))
       call reorder1234(N1,N3,N1,N3,N1,N3,N1,M2,
     & N1,N3,N1,N3,N1,M2,N1,M2,VAAPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S33(N0+1:M1,N1+1:N3,N1+1:M2,N1+1:M2))
       I1=K9*K9*K3
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S33)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N1,N3,N1,M2,N1,M2,N0,M1,X12,S33,-1.000)
       deallocate(S33)
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
       allocate(X13(N0+1:M1,M1+1:N1))
       X13=0.0d0
       call
     & sum21(N0,M1,M1,N1,X13,Q1, 1.000)
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
       allocate(X14(M1+1:N1,M1+1:N1))
       X14=0.0d0
       call
     & sum21(M1,N1,M1,N1,X14,Q2, 1.000)
       deallocate(Q2)
C
       allocate(B1(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,FAHP,B1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(Q3(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q3)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X15(N0+1:M1,N0+1:M1))
       X15=0.0d0
       call
     & sum21(N0,M1,N0,M1,X15,Q3, 1.000)
       deallocate(Q3)
C
       allocate(B1(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,FAHP,B1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(Q4(N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q4)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X16(M1+1:N1,N0+1:M1))
       X16=0.0d0
       call
     & sum21(M1,N1,N0,M1,X16,Q4, 1.000)
       deallocate(Q4)
C
       allocate(B1(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,FAHP,B1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(Q5(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,B1,B2,Q5)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X17(M2+1:N3,M2+1:N3))
       X17=0.0d0
       call
     & sum21(M2,N3,M2,N3,X17,Q5,-1.000)
       deallocate(Q5)
C
       allocate(B1(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,FAHP,B1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(Q6(M2+1:N3,N1+1:M2))
       I1=K9
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,B1,B2,Q6)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X18(N1+1:M2,M2+1:N3))
       X18=0.0d0
       call
     & sum21(N1,M2,M2,N3,X18,Q6,-1.000)
       deallocate(Q6)
C
       allocate(B1(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,FAHP,B1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(Q7(N1+1:M2,M2+1:N3))
       I1=K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,B1,B2,Q7)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X19(M2+1:N3,N1+1:M2))
       X19=0.0d0
       call
     & sum21(M2,N3,N1,M2,X19,Q7,-1.000)
       deallocate(Q7)
C
       allocate(B1(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,FAHP,B1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(Q8(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,B1,B2,Q8)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X20(N1+1:M2,N1+1:M2))
       X20=0.0d0
       call
     & sum21(N1,M2,N1,M2,X20,Q8,-1.000)
       deallocate(Q8)
C
       allocate(B1(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,FAHP,B1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(Q9(N1+1:M2,M2+1:N3))
       I1=K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,B1,B2,Q9)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X21(M2+1:N3,N1+1:M2))
       X21=0.0d0
       call
     & sum21(M2,N3,N1,M2,X21,Q9,-1.000)
       deallocate(Q9)
C
       allocate(B1(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,FAHP,B1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(Q10(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,B1,B2,Q10)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X22(N1+1:M2,N1+1:M2))
       X22=0.0d0
       call
     & sum21(N1,M2,N1,M2,X22,Q10,-1.000)
       deallocate(Q10)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,M1+1:N1))
       call reorder1342(N1,N3,N0,N1,N0,N1,N0,N1,
     & N1,N3,N0,M1,M1,N1,M1,N1,VAHHHP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S34(M1+1:N1,N0+1:M1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S34)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M1+1:N1,M1+1:N1,M1+1:N1))
       call reorder2314(M1,N1,N0,M1,M1,N1,M1,N1,
     & N0,M1,M1,N1,M1,N1,M1,N1,S34,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder461235(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,N1,M2,N1,M2,N0,M1,t3A,F2)
       allocate(Z127(M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M1+1:N1))
       I1=K7*K7
       I2=K5*K9*K9*K6
       I3=K7*K5
       call EGEMM(I1,I2,I3,D1,F2,Z127)
       deallocate(D1)
       deallocate(F2)
C
       V3A=V3A-Z127
       call
     & sum123465(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z127, 1.000)
       deallocate(Z127)
       deallocate(S34)
C
       allocate(D1(N1+1:N3,M1+1:N1,M1+1:N1,M1+1:N1))
       call reorder1342(N1,N3,N0,N1,N0,N1,N0,N1,
     & N1,N3,M1,N1,M1,N1,M1,N1,VAHHHP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S35(M1+1:N1,M1+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S35)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N1,M1+1:N1,M1+1:N1,M1+1:N1))
       call reorder2314(M1,N1,M1,N1,M1,N1,M1,N1,
     & M1,N1,M1,N1,M1,N1,M1,N1,S35,D1)
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder561234(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M1,N1,M2,N3,N1,M2,N1,M2,N0,M1,t3A,F2)
       allocate(Z128(M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M1+1:N1))
       I1=K7*K7
       I2=K5*K9*K9*K6
       I3=K7*K7
       call EGEMM(I1,I2,I3,D1,F2,Z128)
       deallocate(D1)
       deallocate(F2)
C
       V3A=V3A+0.500*Z128
       call
     & sum123465(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z128,-0.500)
       deallocate(Z128)
       deallocate(S35)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder1342(N1,N3,N0,N1,N0,N1,N0,N1,
     & N1,N3,N0,M1,N0,M1,M1,N1,VAHHHP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S36(N0+1:M1,N0+1:M1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S36)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X25(N0+1:M1,N0+1:M1,N0+1:M1,M1+1:N1))
       X25=0.0d0
       call
     & sum3124(N0,M1,N0,M1,N0,M1,M1,N1,X25,S36, 1.000)
       deallocate(S36)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,M1+1:N1))
       call reorder1342(N1,N3,N0,N1,N0,N1,N0,N1,
     & N1,N3,N0,M1,M1,N1,M1,N1,VAHHHP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S37(N0+1:M1,N0+1:M1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S37)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X26(N0+1:M1,M1+1:N1,N0+1:M1,M1+1:N1))
       X26=0.0d0
       call
     & sum3124(N0,M1,M1,N1,N0,M1,M1,N1,X26,S37, 1.000)
       deallocate(S37)
C
       allocate(D1(N1+1:N3,M1+1:N1,M1+1:N1,M1+1:N1))
       call reorder1342(N1,N3,N0,N1,N0,N1,N0,N1,
     & N1,N3,M1,N1,M1,N1,M1,N1,VAHHHP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S38(N0+1:M1,M1+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S38)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X27(M1+1:N1,M1+1:N1,N0+1:M1,M1+1:N1))
       X27=0.0d0
       call
     & sum3124(M1,N1,M1,N1,N0,M1,M1,N1,X27,S38, 1.000)
       deallocate(S38)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,M1+1:N1))
       call reorder3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,M1,M2,N3,M1,N1,VAHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S39(M2+1:N3,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S39)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X28(N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N1))
       X28=0.0d0
       call
     & sum3124(N0,M1,M2,N3,M2,N3,M1,N1,X28,S39,-1.000)
       deallocate(S39)
C
       allocate(D1(N0+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,M1,N1,M2,N3,M1,N1,VAHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S40(M2+1:N3,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K7
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S40)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X29(M1+1:N1,M2+1:N3,M2+1:N3,M1+1:N1))
       X29=0.0d0
       call
     & sum3124(M1,N1,M2,N3,M2,N3,M1,N1,X29,S40,-1.000)
       deallocate(S40)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,M1+1:N1))
       call reorder3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,M1,N1,M2,M1,N1,VAHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S41(M2+1:N3,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S41)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X30(N0+1:M1,N1+1:M2,M2+1:N3,M1+1:N1))
       X30=0.0d0
       call
     & sum3124(N0,M1,N1,M2,M2,N3,M1,N1,X30,S41,-1.000)
       deallocate(S41)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       call reorder3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,M1,N1,N1,M2,M1,N1,VAHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S42(M2+1:N3,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K7
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S42)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X31(M1+1:N1,N1+1:M2,M2+1:N3,M1+1:N1))
       X31=0.0d0
       call
     & sum3124(M1,N1,N1,M2,M2,N3,M1,N1,X31,S42,-1.000)
       deallocate(S42)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,M1+1:N1))
       call reorder3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,M1,M2,N3,M1,N1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S43(N1+1:M2,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S43)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X32(N0+1:M1,M2+1:N3,N1+1:M2,M1+1:N1))
       X32=0.0d0
       call
     & sum3124(N0,M1,M2,N3,N1,M2,M1,N1,X32,S43,-1.000)
       deallocate(S43)
C
       allocate(D1(N0+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,M1,N1,M2,N3,M1,N1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S44(N1+1:M2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S44)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X33(M1+1:N1,M2+1:N3,N1+1:M2,M1+1:N1))
       X33=0.0d0
       call
     & sum3124(M1,N1,M2,N3,N1,M2,M1,N1,X33,S44,-1.000)
       deallocate(S44)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,M1+1:N1))
       call reorder3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,M1,N1,M2,M1,N1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S45(N1+1:M2,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S45)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X34(N0+1:M1,N1+1:M2,N1+1:M2,M1+1:N1))
       X34=0.0d0
       call
     & sum3124(N0,M1,N1,M2,N1,M2,M1,N1,X34,S45,-1.000)
       deallocate(S45)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       call reorder3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,M1,N1,N1,M2,M1,N1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S46(N1+1:M2,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S46)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X35(M1+1:N1,N1+1:M2,N1+1:M2,M1+1:N1))
       X35=0.0d0
       call
     & sum3124(M1,N1,N1,M2,N1,M2,M1,N1,X35,S46,-1.000)
       deallocate(S46)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,M1+1:N1))
       call reorder4312(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,M1,M2,N3,M1,N1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S47(N1+1:M2,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S47)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X79(N0+1:M1,M2+1:N3,N1+1:M2,M1+1:N1))
       X79=0.0d0
       call
     & sum3124(N0,M1,M2,N3,N1,M2,M1,N1,X79,S47, 1.000)
       deallocate(S47)
C
       allocate(D1(N0+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder4312(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,M1,N1,M2,N3,M1,N1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S48(N1+1:M2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S48)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X80(M1+1:N1,M2+1:N3,N1+1:M2,M1+1:N1))
       X80=0.0d0
       call
     & sum3124(M1,N1,M2,N3,N1,M2,M1,N1,X80,S48, 1.000)
       deallocate(S48)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,M1+1:N1))
       call reorder4312(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,M1,N1,M2,M1,N1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S49(N1+1:M2,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S49)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X81(N0+1:M1,N1+1:M2,N1+1:M2,M1+1:N1))
       X81=0.0d0
       call
     & sum3124(N0,M1,N1,M2,N1,M2,M1,N1,X81,S49, 1.000)
       deallocate(S49)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       call reorder4312(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,M1,N1,N1,M2,M1,N1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S50(N1+1:M2,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S50)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X82(M1+1:N1,N1+1:M2,N1+1:M2,M1+1:N1))
       X82=0.0d0
       call
     & sum3124(M1,N1,N1,M2,N1,M2,M1,N1,X82,S50, 1.000)
       deallocate(S50)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder3142(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,M1,N1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q11(N0+1:M1,M1+1:N1))
       I1=K7*K5
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q11)
       deallocate(D1)
       deallocate(B2)
C
       X13=X13+Q11
       deallocate(Q11)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M1+1:N1))
       call reorder3142(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,M1,N1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q12(M1+1:N1,M1+1:N1))
       I1=K7*K7
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q12)
       deallocate(D1)
       deallocate(B2)
C
       X14=X14+Q12
       deallocate(Q12)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:M1,N0+1:M1))
       call reorder1342(N1,N3,N0,N1,N0,N1,N0,N1,
     & N1,N3,N0,M1,N0,M1,N0,M1,VAHHHP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S51(M1+1:N1,N0+1:M1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S51)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,N0,M1,N0,M1,M1,N1,X25,S51,-1.000)
       deallocate(S51)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,N0+1:M1))
       call reorder1342(N1,N3,N0,N1,N0,N1,N0,N1,
     & N1,N3,N0,M1,M1,N1,N0,M1,VAHHHP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S52(M1+1:N1,N0+1:M1,M1+1:N1,N0+1:M1))
       I1=K5*K7*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S52)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,M1,N1,N0,M1,M1,N1,X26,S52,-1.000)
       deallocate(S52)
C
       allocate(D1(N1+1:N3,M1+1:N1,M1+1:N1,N0+1:M1))
       call reorder1342(N1,N3,N0,N1,N0,N1,N0,N1,
     & N1,N3,M1,N1,M1,N1,N0,M1,VAHHHP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S53(M1+1:N1,M1+1:N1,M1+1:N1,N0+1:M1))
       I1=K5*K7*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S53)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,M1,N1,N0,M1,M1,N1,X27,S53,-1.000)
       deallocate(S53)
C
       allocate(D1(N1+1:N3,N0+1:M1,M2+1:N3,M2+1:N3))
       call reorder1423(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N0,M1,M2,N3,M2,N3,VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S54(M1+1:N1,N0+1:M1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S54)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,M2,N3,M2,N3,M1,N1,X28,S54,-1.000)
       deallocate(S54)
C
       allocate(D1(N1+1:N3,M1+1:N1,M2+1:N3,M2+1:N3))
       call reorder1423(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,M1,N1,M2,N3,M2,N3,VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S55(M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S55)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,M2,N3,M2,N3,M1,N1,X29,S55,-1.000)
       deallocate(S55)
C
       allocate(D1(N1+1:N3,N0+1:M1,N1+1:M2,M2+1:N3))
       call reorder1423(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N0,M1,N1,M2,M2,N3,VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S56(M1+1:N1,N0+1:M1,N1+1:M2,M2+1:N3))
       I1=K6*K9*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S56)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,N1,M2,M2,N3,M1,N1,X30,S56,-1.000)
       deallocate(S56)
C
       allocate(D1(N1+1:N3,M1+1:N1,N1+1:M2,M2+1:N3))
       call reorder1423(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,M1,N1,N1,M2,M2,N3,VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S57(M1+1:N1,M1+1:N1,N1+1:M2,M2+1:N3))
       I1=K6*K9*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S57)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,N1,M2,M2,N3,M1,N1,X31,S57,-1.000)
       deallocate(S57)
C
       allocate(D1(N1+1:N3,N0+1:M1,M2+1:N3,N1+1:M2))
       call reorder1423(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N0,M1,M2,N3,N1,M2,VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S58(M1+1:N1,N0+1:M1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S58)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,M2,N3,N1,M2,M1,N1,X32,S58,-1.000)
       deallocate(S58)
C
       allocate(D1(N1+1:N3,M1+1:N1,M2+1:N3,N1+1:M2))
       call reorder1423(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,M1,N1,M2,N3,N1,M2,VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S59(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S59)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,M2,N3,N1,M2,M1,N1,X33,S59,-1.000)
       deallocate(S59)
C
       allocate(D1(N1+1:N3,N0+1:M1,N1+1:M2,N1+1:M2))
       call reorder1423(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N0,M1,N1,M2,N1,M2,VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S60(M1+1:N1,N0+1:M1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S60)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,N1,M2,N1,M2,M1,N1,X34,S60,-1.000)
       deallocate(S60)
C
       allocate(D1(N1+1:N3,M1+1:N1,N1+1:M2,N1+1:M2))
       call reorder1423(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,M1,N1,N1,M2,N1,M2,VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S61(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S61)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,N1,M2,N1,M2,M1,N1,X35,S61,-1.000)
       deallocate(S61)
C
       allocate(D1(N1+1:N3,N0+1:M1,M2+1:N3,N1+1:M2))
       call reorder1423(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N0,M1,M2,N3,N1,M2,VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S62(M1+1:N1,N0+1:M1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S62)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X36(N0+1:M1,M2+1:N3,N1+1:M2,M1+1:N1))
       X36=0.0d0
       call
     & sum4123(N0,M1,M2,N3,N1,M2,M1,N1,X36,S62,-1.000)
       deallocate(S62)
C
       allocate(D1(N1+1:N3,M1+1:N1,M2+1:N3,N1+1:M2))
       call reorder1423(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,M1,N1,M2,N3,N1,M2,VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S63(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S63)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X37(M1+1:N1,M2+1:N3,N1+1:M2,M1+1:N1))
       X37=0.0d0
       call
     & sum4123(M1,N1,M2,N3,N1,M2,M1,N1,X37,S63,-1.000)
       deallocate(S63)
C
       allocate(D1(N1+1:N3,N0+1:M1,N1+1:M2,N1+1:M2))
       call reorder1423(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N0,M1,N1,M2,N1,M2,VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S64(M1+1:N1,N0+1:M1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S64)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X38(N0+1:M1,N1+1:M2,N1+1:M2,M1+1:N1))
       X38=0.0d0
       call
     & sum4123(N0,M1,N1,M2,N1,M2,M1,N1,X38,S64,-1.000)
       deallocate(S64)
C
       allocate(D1(N1+1:N3,M1+1:N1,N1+1:M2,N1+1:M2))
       call reorder1423(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,M1,N1,N1,M2,N1,M2,VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S65(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S65)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X39(M1+1:N1,N1+1:M2,N1+1:M2,M1+1:N1))
       X39=0.0d0
       call
     & sum4123(M1,N1,N1,M2,N1,M2,M1,N1,X39,S65,-1.000)
       deallocate(S65)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,M1,M2,N3,N0,M1,VAHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S66(M2+1:N3,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S66)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X40(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:M1))
       X40=0.0d0
       call
     & sum3124(N0,M1,M2,N3,M2,N3,N0,M1,X40,S66,-1.000)
       deallocate(S66)
C
       allocate(D1(N0+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,M1,N1,M2,N3,N0,M1,VAHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S67(M2+1:N3,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S67)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X41(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:M1))
       X41=0.0d0
       call
     & sum3124(M1,N1,M2,N3,M2,N3,N0,M1,X41,S67,-1.000)
       deallocate(S67)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,M1,N1,M2,N0,M1,VAHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S68(M2+1:N3,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S68)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X42(N0+1:M1,N1+1:M2,M2+1:N3,N0+1:M1))
       X42=0.0d0
       call
     & sum3124(N0,M1,N1,M2,M2,N3,N0,M1,X42,S68,-1.000)
       deallocate(S68)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,M1,N1,N1,M2,N0,M1,VAHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S69(M2+1:N3,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S69)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X43(M1+1:N1,N1+1:M2,M2+1:N3,N0+1:M1))
       X43=0.0d0
       call
     & sum3124(M1,N1,N1,M2,M2,N3,N0,M1,X43,S69,-1.000)
       deallocate(S69)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,M1,M2,N3,N0,M1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S70(N1+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S70)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X44(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       X44=0.0d0
       call
     & sum3124(N0,M1,M2,N3,N1,M2,N0,M1,X44,S70,-1.000)
       deallocate(S70)
C
       allocate(D1(N0+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,M1,N1,M2,N3,N0,M1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S71(N1+1:M2,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S71)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X45(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:M1))
       X45=0.0d0
       call
     & sum3124(M1,N1,M2,N3,N1,M2,N0,M1,X45,S71,-1.000)
       deallocate(S71)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,M1,N1,M2,N0,M1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S72(N1+1:M2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S72)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X46(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:M1))
       X46=0.0d0
       call
     & sum3124(N0,M1,N1,M2,N1,M2,N0,M1,X46,S72,-1.000)
       deallocate(S72)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,M1,N1,N1,M2,N0,M1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S73(N1+1:M2,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S73)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X47(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:M1))
       X47=0.0d0
       call
     & sum3124(M1,N1,N1,M2,N1,M2,N0,M1,X47,S73,-1.000)
       deallocate(S73)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder4312(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,M1,M2,N3,N0,M1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S74(N1+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S74)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X83(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       X83=0.0d0
       call
     & sum3124(N0,M1,M2,N3,N1,M2,N0,M1,X83,S74, 1.000)
       deallocate(S74)
C
       allocate(D1(N0+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder4312(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,M1,N1,M2,N3,N0,M1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S75(N1+1:M2,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S75)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X84(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:M1))
       X84=0.0d0
       call
     & sum3124(M1,N1,M2,N3,N1,M2,N0,M1,X84,S75, 1.000)
       deallocate(S75)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder4312(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,M1,N1,M2,N0,M1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S76(N1+1:M2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S76)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X85(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:M1))
       X85=0.0d0
       call
     & sum3124(N0,M1,N1,M2,N1,M2,N0,M1,X85,S76, 1.000)
       deallocate(S76)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder4312(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,M1,N1,N1,M2,N0,M1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S77(N1+1:M2,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S77)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X86(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:M1))
       X86=0.0d0
       call
     & sum3124(M1,N1,N1,M2,N1,M2,N0,M1,X86,S77, 1.000)
       deallocate(S77)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder3142(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,N0,M1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q13(N0+1:M1,N0+1:M1))
       I1=K5*K5
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q13)
       deallocate(D1)
       deallocate(B2)
C
       X15=X15+Q13
       deallocate(Q13)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N0+1:M1))
       call reorder3142(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,N0,M1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q14(M1+1:N1,N0+1:M1))
       I1=K5*K7
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q14)
       deallocate(D1)
       deallocate(B2)
C
       X16=X16+Q14
       deallocate(Q14)
C
       allocate(D1(N1+1:N3,N0+1:M1,M2+1:N3,M2+1:N3))
       call reorder1423(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N0,M1,M2,N3,M2,N3,VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S78(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S78)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,M2,N3,M2,N3,N0,M1,X40,S78,-1.000)
       deallocate(S78)
C
       allocate(D1(N1+1:N3,M1+1:N1,M2+1:N3,M2+1:N3))
       call reorder1423(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,M1,N1,M2,N3,M2,N3,VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S79(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S79)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,M2,N3,M2,N3,N0,M1,X41,S79,-1.000)
       deallocate(S79)
C
       allocate(D1(N1+1:N3,N0+1:M1,N1+1:M2,M2+1:N3))
       call reorder1423(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N0,M1,N1,M2,M2,N3,VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S80(N0+1:M1,N0+1:M1,N1+1:M2,M2+1:N3))
       I1=K6*K9*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S80)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,N1,M2,M2,N3,N0,M1,X42,S80,-1.000)
       deallocate(S80)
C
       allocate(D1(N1+1:N3,M1+1:N1,N1+1:M2,M2+1:N3))
       call reorder1423(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,M1,N1,N1,M2,M2,N3,VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S81(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3))
       I1=K6*K9*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S81)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,N1,M2,M2,N3,N0,M1,X43,S81,-1.000)
       deallocate(S81)
C
       allocate(D1(N1+1:N3,N0+1:M1,M2+1:N3,N1+1:M2))
       call reorder1423(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N0,M1,M2,N3,N1,M2,VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S82(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S82)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,M2,N3,N1,M2,N0,M1,X44,S82,-1.000)
       deallocate(S82)
C
       allocate(D1(N1+1:N3,M1+1:N1,M2+1:N3,N1+1:M2))
       call reorder1423(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,M1,N1,M2,N3,N1,M2,VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S83(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S83)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,M2,N3,N1,M2,N0,M1,X45,S83,-1.000)
       deallocate(S83)
C
       allocate(D1(N1+1:N3,N0+1:M1,N1+1:M2,N1+1:M2))
       call reorder1423(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N0,M1,N1,M2,N1,M2,VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S84(N0+1:M1,N0+1:M1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S84)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,N1,M2,N1,M2,N0,M1,X46,S84,-1.000)
       deallocate(S84)
C
       allocate(D1(N1+1:N3,M1+1:N1,N1+1:M2,N1+1:M2))
       call reorder1423(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,M1,N1,N1,M2,N1,M2,VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S85(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S85)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,N1,M2,N1,M2,N0,M1,X47,S85,-1.000)
       deallocate(S85)
C
       allocate(D1(N1+1:N3,N0+1:M1,M2+1:N3,N1+1:M2))
       call reorder1423(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N0,M1,M2,N3,N1,M2,VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S86(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S86)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X48(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       X48=0.0d0
       call
     & sum4123(N0,M1,M2,N3,N1,M2,N0,M1,X48,S86,-1.000)
       deallocate(S86)
C
       allocate(D1(N1+1:N3,M1+1:N1,M2+1:N3,N1+1:M2))
       call reorder1423(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,M1,N1,M2,N3,N1,M2,VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S87(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S87)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X49(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:M1))
       X49=0.0d0
       call
     & sum4123(M1,N1,M2,N3,N1,M2,N0,M1,X49,S87,-1.000)
       deallocate(S87)
C
       allocate(D1(N1+1:N3,N0+1:M1,N1+1:M2,N1+1:M2))
       call reorder1423(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N0,M1,N1,M2,N1,M2,VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S88(N0+1:M1,N0+1:M1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S88)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X50(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:M1))
       X50=0.0d0
       call
     & sum4123(N0,M1,N1,M2,N1,M2,N0,M1,X50,S88,-1.000)
       deallocate(S88)
C
       allocate(D1(N1+1:N3,M1+1:N1,N1+1:M2,N1+1:M2))
       call reorder1423(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,M1,N1,N1,M2,N1,M2,VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S89(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S89)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X51(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:M1))
       X51=0.0d0
       call
     & sum4123(M1,N1,N1,M2,N1,M2,N0,M1,X51,S89,-1.000)
       deallocate(S89)
C
       allocate(D1(N0+1:N1,M2+1:N3,M2+1:N3,M2+1:N3))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,M2,N3,M2,N3,M2,N3,VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S90(N1+1:M2,M2+1:N3,M2+1:N3,M2+1:N3))
       I1=K6*K6*K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S90)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X52(M2+1:N3,M2+1:N3,M2+1:N3,N1+1:M2))
       X52=0.0d0
       call
     & sum4123(M2,N3,M2,N3,M2,N3,N1,M2,X52,S90,-1.000)
       deallocate(S90)
C
       allocate(D1(N0+1:N1,M2+1:N3,N1+1:M2,M2+1:N3))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,M2,N3,N1,M2,M2,N3,VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S91(N1+1:M2,M2+1:N3,N1+1:M2,M2+1:N3))
       I1=K6*K9*K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S91)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X53(M2+1:N3,N1+1:M2,M2+1:N3,N1+1:M2))
       X53=0.0d0
       call
     & sum4123(M2,N3,N1,M2,M2,N3,N1,M2,X53,S91,-1.000)
       deallocate(S91)
C
       allocate(D1(N0+1:N1,N1+1:M2,N1+1:M2,M2+1:N3))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,M2,N1,M2,M2,N3,VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S92(N1+1:M2,N1+1:M2,N1+1:M2,M2+1:N3))
       I1=K6*K9*K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S92)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X54(N1+1:M2,N1+1:M2,M2+1:N3,N1+1:M2))
       X54=0.0d0
       call
     & sum4123(N1,M2,N1,M2,M2,N3,N1,M2,X54,S92,-1.000)
       deallocate(S92)
C
       allocate(D1(N0+1:N1,M2+1:N3,M2+1:N3,M2+1:N3))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,M2,N3,M2,N3,M2,N3,VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S93(N1+1:M2,M2+1:N3,M2+1:N3,M2+1:N3))
       I1=K6*K6*K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S93)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X55(M2+1:N3,M2+1:N3,M2+1:N3,N1+1:M2))
       X55=0.0d0
       call
     & sum4123(M2,N3,M2,N3,M2,N3,N1,M2,X55,S93,-1.000)
       deallocate(S93)
C
       allocate(D1(N0+1:N1,M2+1:N3,N1+1:M2,M2+1:N3))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,M2,N3,N1,M2,M2,N3,VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S94(N1+1:M2,M2+1:N3,N1+1:M2,M2+1:N3))
       I1=K6*K9*K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S94)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X56(M2+1:N3,N1+1:M2,M2+1:N3,N1+1:M2))
       X56=0.0d0
       call
     & sum4123(M2,N3,N1,M2,M2,N3,N1,M2,X56,S94,-1.000)
       deallocate(S94)
C
       allocate(D1(N0+1:N1,N1+1:M2,N1+1:M2,M2+1:N3))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,M2,N1,M2,M2,N3,VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S95(N1+1:M2,N1+1:M2,N1+1:M2,M2+1:N3))
       I1=K6*K9*K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S95)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X57(N1+1:M2,N1+1:M2,M2+1:N3,N1+1:M2))
       X57=0.0d0
       call
     & sum4123(N1,M2,N1,M2,M2,N3,N1,M2,X57,S95,-1.000)
       deallocate(S95)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M2+1:N3))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,M2,N3,M2,N3,VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q15(M2+1:N3,M2+1:N3))
       I1=K6*K6
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q15)
       deallocate(D1)
       deallocate(B2)
C
       X17=X17-Q15
       deallocate(Q15)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M2+1:N3))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,M2,M2,N3,VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q16(N1+1:M2,M2+1:N3))
       I1=K6*K9
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q16)
       deallocate(D1)
       deallocate(B2)
C
       X18=X18-Q16
       deallocate(Q16)
C
       allocate(D1(N0+1:N1,M2+1:N3,M2+1:N3,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,M2,N3,M2,N3,N1,M2,VAHPPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S96(M2+1:N3,M2+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6*K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S96)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,M2,N3,M2,N3,N1,M2,X52,S96, 1.000)
       deallocate(S96)
C
       allocate(D1(N0+1:N1,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,M2,N3,N1,M2,N1,M2,VAHPPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S97(M2+1:N3,M2+1:N3,N1+1:M2,N1+1:M2))
       I1=K9*K9*K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S97)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,N1,M2,M2,N3,N1,M2,X53,S97, 1.000)
       deallocate(S97)
C
       allocate(D1(N0+1:N1,N1+1:M2,N1+1:M2,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,M2,N1,M2,N1,M2,VAHPPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S98(M2+1:N3,N1+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9*K9
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S98)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,M2,N1,M2,M2,N3,N1,M2,X54,S98, 1.000)
       deallocate(S98)
C
       allocate(D1(N0+1:N1,M2+1:N3,M2+1:N3,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,M2,N3,M2,N3,N1,M2,VAHPPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S99(M2+1:N3,M2+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6*K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S99)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,M2,N3,M2,N3,N1,M2,X55,S99, 1.000)
       deallocate(S99)
C
       allocate(D1(N0+1:N1,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,M2,N3,N1,M2,N1,M2,VAHPPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S100(M2+1:N3,M2+1:N3,N1+1:M2,N1+1:M2))
       I1=K9*K9*K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S100)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,N1,M2,M2,N3,N1,M2,X56,S100, 1.000)
       deallocate(S100)
C
       allocate(D1(N0+1:N1,N1+1:M2,N1+1:M2,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,M2,N1,M2,N1,M2,VAHPPP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S101(M2+1:N3,N1+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9*K9
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S101)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,M2,N1,M2,M2,N3,N1,M2,X57,S101, 1.000)
       deallocate(S101)
C
       allocate(D1(N0+1:N1,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,M2,N3,N1,M2,N1,M2,VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S102(N1+1:M2,M2+1:N3,N1+1:M2,N1+1:M2))
       I1=K9*K9*K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S102)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X58(M2+1:N3,N1+1:M2,N1+1:M2,N1+1:M2))
       X58=0.0d0
       call
     & sum4123(M2,N3,N1,M2,N1,M2,N1,M2,X58,S102,-1.000)
       deallocate(S102)
C
       allocate(D1(N0+1:N1,N1+1:M2,N1+1:M2,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,M2,N1,M2,N1,M2,VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S103(N1+1:M2,N1+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9*K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S103)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X59(N1+1:M2,N1+1:M2,N1+1:M2,N1+1:M2))
       X59=0.0d0
       call
     & sum4123(N1,M2,N1,M2,N1,M2,N1,M2,X59,S103,-1.000)
       deallocate(S103)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,M2,N3,N1,M2,VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q17(M2+1:N3,N1+1:M2))
       I1=K9*K6
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q17)
       deallocate(D1)
       deallocate(B2)
C
       X19=X19-Q17
       deallocate(Q17)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,M2,N1,M2,VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q18(N1+1:M2,N1+1:M2))
       I1=K9*K9
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q18)
       deallocate(D1)
       deallocate(B2)
C
       X20=X20-Q18
       deallocate(Q18)
C
       allocate(D1(N0+1:N1,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,M2,N3,N1,M2,N1,M2,VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S104(N1+1:M2,M2+1:N3,N1+1:M2,N1+1:M2))
       I1=K9*K9*K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S104)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,N1,M2,N1,M2,N1,M2,X58,S104, 1.000)
       deallocate(S104)
C
       allocate(D1(N0+1:N1,N1+1:M2,N1+1:M2,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,M2,N1,M2,N1,M2,VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S105(N1+1:M2,N1+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9*K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S105)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,M2,N1,M2,N1,M2,N1,M2,X59,S105, 1.000)
       deallocate(S105)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,M2,N3,N1,M2,VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q19(M2+1:N3,N1+1:M2))
       I1=K9*K6
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q19)
       deallocate(D1)
       deallocate(B2)
C
       X21=X21-Q19
       deallocate(Q19)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,M2,N1,M2,VAHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q20(N1+1:M2,N1+1:M2))
       I1=K9*K9
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q20)
       deallocate(D1)
       deallocate(B2)
C
       X22=X22-Q20
       deallocate(Q20)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,M1+1:N1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N0,M1,M2,N3,M1,N1,VBHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S106(M2+1:N3,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S106)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder2314(M2,N3,N0,M1,M2,N3,M1,N1,
     & N0,M1,M2,N3,M2,N3,M1,N1,S106,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z209(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K5*K9*K9
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z209)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z209,-1.000)
       call
     & sum234615(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z209, 1.000)
       deallocate(Z209)
       deallocate(S106)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,M1+1:N1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,M1,N2,M2,N3,M1,N1,VBHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S107(M2+1:N3,M1+1:N2,M2+1:N3,M1+1:N1))
       I1=K7*K6*K8
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S107)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder2314(M2,N3,M1,N2,M2,N3,M1,N1,
     & M1,N2,M2,N3,M2,N3,M1,N1,S107,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z210(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K5*K9*K9
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z210)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z210,-1.000)
       call
     & sum234615(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z210, 1.000)
       deallocate(Z210)
       deallocate(S107)
C
       allocate(D1(N0+1:N1,N0+1:M1,N2+1:M2,M1+1:N1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N0,M1,N2,M2,M1,N1,VBHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S108(M2+1:N3,N0+1:M1,N2+1:M2,M1+1:N1))
       I1=K7*K0*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S108)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,M1+1:N1))
       call reorder2314(M2,N3,N0,M1,N2,M2,M1,N1,
     & N0,M1,N2,M2,M2,N3,M1,N1,S108,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z211(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K5*K9*K9
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z211)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z211,-1.000)
       call
     & sum234615(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z211, 1.000)
       deallocate(Z211)
       deallocate(S108)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,M1+1:N1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,M1,N2,N2,M2,M1,N1,VBHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S109(M2+1:N3,M1+1:N2,N2+1:M2,M1+1:N1))
       I1=K7*K0*K8
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S109)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,M1+1:N1))
       call reorder2314(M2,N3,M1,N2,N2,M2,M1,N1,
     & M1,N2,N2,M2,M2,N3,M1,N1,S109,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z212(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K5*K9*K9
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z212)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z212,-1.000)
       call
     & sum234615(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z212, 1.000)
       deallocate(Z212)
       deallocate(S109)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,M1+1:N1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N0,M1,M2,N3,M1,N1,VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S110(N1+1:M2,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S110)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder2314(N1,M2,N0,M1,M2,N3,M1,N1,
     & N0,M1,M2,N3,N1,M2,M1,N1,S110,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z213(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z213)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z213, 1.000)
       call
     & sum134625(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z213,-1.000)
       deallocate(Z213)
       deallocate(S110)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,M1+1:N1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,M1,N2,M2,N3,M1,N1,VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S111(N1+1:M2,M1+1:N2,M2+1:N3,M1+1:N1))
       I1=K7*K6*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S111)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder2314(N1,M2,M1,N2,M2,N3,M1,N1,
     & M1,N2,M2,N3,N1,M2,M1,N1,S111,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z214(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z214)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z214, 1.000)
       call
     & sum134625(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z214,-1.000)
       deallocate(Z214)
       deallocate(S111)
C
       allocate(D1(N0+1:N1,N0+1:M1,N2+1:M2,M1+1:N1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N0,M1,N2,M2,M1,N1,VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S112(N1+1:M2,N0+1:M1,N2+1:M2,M1+1:N1))
       I1=K7*K0*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S112)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder2314(N1,M2,N0,M1,N2,M2,M1,N1,
     & N0,M1,N2,M2,N1,M2,M1,N1,S112,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z215(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z215)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z215, 1.000)
       call
     & sum134625(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z215,-1.000)
       deallocate(Z215)
       deallocate(S112)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,M1+1:N1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,M1,N2,N2,M2,M1,N1,VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S113(N1+1:M2,M1+1:N2,N2+1:M2,M1+1:N1))
       I1=K7*K0*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S113)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder2314(N1,M2,M1,N2,N2,M2,M1,N1,
     & M1,N2,N2,M2,N1,M2,M1,N1,S113,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z216(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z216)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z216, 1.000)
       call
     & sum134625(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z216,-1.000)
       deallocate(Z216)
       deallocate(S113)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,M1+1:N1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N0,M1,M2,N3,M1,N1,VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S114(N1+1:M2,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S114)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder2314(N1,M2,N0,M1,M2,N3,M1,N1,
     & N0,M1,M2,N3,N1,M2,M1,N1,S114,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z217(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z217)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z217,-1.000)
       call
     & sum124635(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z217, 1.000)
       deallocate(Z217)
       deallocate(S114)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,M1+1:N1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,M1,N2,M2,N3,M1,N1,VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S115(N1+1:M2,M1+1:N2,M2+1:N3,M1+1:N1))
       I1=K7*K6*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S115)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder2314(N1,M2,M1,N2,M2,N3,M1,N1,
     & M1,N2,M2,N3,N1,M2,M1,N1,S115,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z218(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z218)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z218,-1.000)
       call
     & sum124635(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z218, 1.000)
       deallocate(Z218)
       deallocate(S115)
C
       allocate(D1(N0+1:N1,N0+1:M1,N2+1:M2,M1+1:N1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N0,M1,N2,M2,M1,N1,VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S116(N1+1:M2,N0+1:M1,N2+1:M2,M1+1:N1))
       I1=K7*K0*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S116)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder2314(N1,M2,N0,M1,N2,M2,M1,N1,
     & N0,M1,N2,M2,N1,M2,M1,N1,S116,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z219(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z219)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z219,-1.000)
       call
     & sum124635(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z219, 1.000)
       deallocate(Z219)
       deallocate(S116)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,M1+1:N1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,M1,N2,N2,M2,M1,N1,VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S117(N1+1:M2,M1+1:N2,N2+1:M2,M1+1:N1))
       I1=K7*K0*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S117)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder2314(N1,M2,M1,N2,N2,M2,M1,N1,
     & M1,N2,N2,M2,N1,M2,M1,N1,S117,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z220(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z220)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z220,-1.000)
       call
     & sum124635(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z220, 1.000)
       deallocate(Z220)
       deallocate(S117)
C
       allocate(D1(N1+1:N3,N0+1:M1,M2+1:N3,M2+1:N3))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,N0,M1,M2,N3,M2,N3,VBPHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S118(M1+1:N1,N0+1:M1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S118)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder2341(M1,N1,N0,M1,M2,N3,M2,N3,
     & N0,M1,M2,N3,M2,N3,M1,N1,S118,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z221(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K5*K9*K9
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z221)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z221, 1.000)
       call
     & sum234615(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z221,-1.000)
       deallocate(Z221)
       deallocate(S118)
C
       allocate(D1(N1+1:N3,M1+1:N2,M2+1:N3,M2+1:N3))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,M1,N2,M2,N3,M2,N3,VBPHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S119(M1+1:N1,M1+1:N2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S119)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,M1+1:N1))
       call reorder2341(M1,N1,M1,N2,M2,N3,M2,N3,
     & M1,N2,M2,N3,M2,N3,M1,N1,S119,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z222(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K5*K9*K9
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z222)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z222, 1.000)
       call
     & sum234615(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z222,-1.000)
       deallocate(Z222)
       deallocate(S119)
C
       allocate(D1(N1+1:N3,N0+1:M1,N2+1:M2,M2+1:N3))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,N0,M1,N2,M2,M2,N3,VBPHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S120(M1+1:N1,N0+1:M1,N2+1:M2,M2+1:N3))
       I1=K6*K0*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S120)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,M1+1:N1))
       call reorder2341(M1,N1,N0,M1,N2,M2,M2,N3,
     & N0,M1,N2,M2,M2,N3,M1,N1,S120,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z223(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K5*K9*K9
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z223)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z223, 1.000)
       call
     & sum234615(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z223,-1.000)
       deallocate(Z223)
       deallocate(S120)
C
       allocate(D1(N1+1:N3,M1+1:N2,N2+1:M2,M2+1:N3))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,M1,N2,N2,M2,M2,N3,VBPHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S121(M1+1:N1,M1+1:N2,N2+1:M2,M2+1:N3))
       I1=K6*K0*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S121)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,M1+1:N1))
       call reorder2341(M1,N1,M1,N2,N2,M2,M2,N3,
     & M1,N2,N2,M2,M2,N3,M1,N1,S121,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z224(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K5*K9*K9
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z224)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z224, 1.000)
       call
     & sum234615(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z224,-1.000)
       deallocate(Z224)
       deallocate(S121)
C
       allocate(D1(N1+1:N3,N0+1:M1,M2+1:N3,N1+1:M2))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,N0,M1,M2,N3,N1,M2,VBPHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S122(M1+1:N1,N0+1:M1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S122)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder2341(M1,N1,N0,M1,M2,N3,N1,M2,
     & N0,M1,M2,N3,N1,M2,M1,N1,S122,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z225(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z225)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z225,-1.000)
       call
     & sum134625(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z225, 1.000)
       deallocate(Z225)
       deallocate(S122)
C
       allocate(D1(N1+1:N3,M1+1:N2,M2+1:N3,N1+1:M2))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,M1,N2,M2,N3,N1,M2,VBPHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S123(M1+1:N1,M1+1:N2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S123)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder2341(M1,N1,M1,N2,M2,N3,N1,M2,
     & M1,N2,M2,N3,N1,M2,M1,N1,S123,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z226(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z226)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z226,-1.000)
       call
     & sum134625(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z226, 1.000)
       deallocate(Z226)
       deallocate(S123)
C
       allocate(D1(N1+1:N3,N0+1:M1,N2+1:M2,N1+1:M2))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,N0,M1,N2,M2,N1,M2,VBPHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S124(M1+1:N1,N0+1:M1,N2+1:M2,N1+1:M2))
       I1=K9*K0*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S124)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder2341(M1,N1,N0,M1,N2,M2,N1,M2,
     & N0,M1,N2,M2,N1,M2,M1,N1,S124,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z227(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z227)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z227,-1.000)
       call
     & sum134625(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z227, 1.000)
       deallocate(Z227)
       deallocate(S124)
C
       allocate(D1(N1+1:N3,M1+1:N2,N2+1:M2,N1+1:M2))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,M1,N2,N2,M2,N1,M2,VBPHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S125(M1+1:N1,M1+1:N2,N2+1:M2,N1+1:M2))
       I1=K9*K0*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S125)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder2341(M1,N1,M1,N2,N2,M2,N1,M2,
     & M1,N2,N2,M2,N1,M2,M1,N1,S125,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z228(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z228)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z228,-1.000)
       call
     & sum134625(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z228, 1.000)
       deallocate(Z228)
       deallocate(S125)
C
       allocate(D1(N1+1:N3,N0+1:M1,M2+1:N3,N1+1:M2))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,N0,M1,M2,N3,N1,M2,VBPHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S126(M1+1:N1,N0+1:M1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S126)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder2341(M1,N1,N0,M1,M2,N3,N1,M2,
     & N0,M1,M2,N3,N1,M2,M1,N1,S126,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z229(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z229)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z229, 1.000)
       call
     & sum124635(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z229,-1.000)
       deallocate(Z229)
       deallocate(S126)
C
       allocate(D1(N1+1:N3,M1+1:N2,M2+1:N3,N1+1:M2))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,M1,N2,M2,N3,N1,M2,VBPHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S127(M1+1:N1,M1+1:N2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S127)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder2341(M1,N1,M1,N2,M2,N3,N1,M2,
     & M1,N2,M2,N3,N1,M2,M1,N1,S127,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z230(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z230)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z230, 1.000)
       call
     & sum124635(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z230,-1.000)
       deallocate(Z230)
       deallocate(S127)
C
       allocate(D1(N1+1:N3,N0+1:M1,N2+1:M2,N1+1:M2))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,N0,M1,N2,M2,N1,M2,VBPHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S128(M1+1:N1,N0+1:M1,N2+1:M2,N1+1:M2))
       I1=K9*K0*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S128)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder2341(M1,N1,N0,M1,N2,M2,N1,M2,
     & N0,M1,N2,M2,N1,M2,M1,N1,S128,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z231(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z231)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z231, 1.000)
       call
     & sum124635(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z231,-1.000)
       deallocate(Z231)
       deallocate(S128)
C
       allocate(D1(N1+1:N3,M1+1:N2,N2+1:M2,N1+1:M2))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,M1,N2,N2,M2,N1,M2,VBPHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S129(M1+1:N1,M1+1:N2,N2+1:M2,N1+1:M2))
       I1=K9*K0*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S129)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder2341(M1,N1,M1,N2,N2,M2,N1,M2,
     & M1,N2,N2,M2,N1,M2,M1,N1,S129,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z232(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z232)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z232, 1.000)
       call
     & sum124635(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z232,-1.000)
       deallocate(Z232)
       deallocate(S129)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N0,M1,M2,N3,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S130(M2+1:N3,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S130)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder2314(M2,N3,N0,M1,M2,N3,N0,M1,
     & N0,M1,M2,N3,M2,N3,N0,M1,S130,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z233(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K7*K9*K9
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z233)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum235614(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z233,-1.000)
       deallocate(Z233)
       deallocate(S130)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,M1,N2,M2,N3,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S131(M2+1:N3,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S131)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder2314(M2,N3,M1,N2,M2,N3,N0,M1,
     & M1,N2,M2,N3,M2,N3,N0,M1,S131,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z234(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K7*K9*K9
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z234)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum235614(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z234,-1.000)
       deallocate(Z234)
       deallocate(S131)
C
       allocate(D1(N0+1:N1,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N0,M1,N2,M2,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S132(M2+1:N3,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S132)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder2314(M2,N3,N0,M1,N2,M2,N0,M1,
     & N0,M1,N2,M2,M2,N3,N0,M1,S132,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z235(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K7*K9*K9
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z235)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum235614(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z235,-1.000)
       deallocate(Z235)
       deallocate(S132)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,M1,N2,N2,M2,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S133(M2+1:N3,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S133)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder2314(M2,N3,M1,N2,N2,M2,N0,M1,
     & M1,N2,N2,M2,M2,N3,N0,M1,S133,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z236(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K7*K9*K9
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z236)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum235614(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z236,-1.000)
       deallocate(Z236)
       deallocate(S133)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N0,M1,M2,N3,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S134(N1+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S134)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder2314(N1,M2,N0,M1,M2,N3,N0,M1,
     & N0,M1,M2,N3,N1,M2,N0,M1,S134,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z237(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z237)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z237, 1.000)
       deallocate(Z237)
       deallocate(S134)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,M1,N2,M2,N3,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S135(N1+1:M2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S135)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder2314(N1,M2,M1,N2,M2,N3,N0,M1,
     & M1,N2,M2,N3,N1,M2,N0,M1,S135,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z238(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z238)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z238, 1.000)
       deallocate(Z238)
       deallocate(S135)
C
       allocate(D1(N0+1:N1,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N0,M1,N2,M2,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S136(N1+1:M2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S136)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder2314(N1,M2,N0,M1,N2,M2,N0,M1,
     & N0,M1,N2,M2,N1,M2,N0,M1,S136,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z239(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z239)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z239, 1.000)
       deallocate(Z239)
       deallocate(S136)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,M1,N2,N2,M2,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S137(N1+1:M2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S137)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder2314(N1,M2,M1,N2,N2,M2,N0,M1,
     & M1,N2,N2,M2,N1,M2,N0,M1,S137,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z240(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z240)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z240, 1.000)
       deallocate(Z240)
       deallocate(S137)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N0,M1,M2,N3,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S138(N1+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S138)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder2314(N1,M2,N0,M1,M2,N3,N0,M1,
     & N0,M1,M2,N3,N1,M2,N0,M1,S138,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z241(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z241)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z241,-1.000)
       deallocate(Z241)
       deallocate(S138)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,M1,N2,M2,N3,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S139(N1+1:M2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S139)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder2314(N1,M2,M1,N2,M2,N3,N0,M1,
     & M1,N2,M2,N3,N1,M2,N0,M1,S139,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z242(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z242)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z242,-1.000)
       deallocate(Z242)
       deallocate(S139)
C
       allocate(D1(N0+1:N1,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N0,M1,N2,M2,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S140(N1+1:M2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S140)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder2314(N1,M2,N0,M1,N2,M2,N0,M1,
     & N0,M1,N2,M2,N1,M2,N0,M1,S140,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z243(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z243)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z243,-1.000)
       deallocate(Z243)
       deallocate(S140)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,M1,N2,N2,M2,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S141(N1+1:M2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S141)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder2314(N1,M2,M1,N2,N2,M2,N0,M1,
     & M1,N2,N2,M2,N1,M2,N0,M1,S141,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z244(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z244)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z244,-1.000)
       deallocate(Z244)
       deallocate(S141)
C
       allocate(D1(N1+1:N3,N0+1:M1,M2+1:N3,M2+1:N3))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,N0,M1,M2,N3,M2,N3,VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S142(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S142)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,M1,M2,N3,M2,N3,
     & N0,M1,M2,N3,M2,N3,N0,M1,S142,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z245(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K7*K9*K9
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z245)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum235614(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z245, 1.000)
       deallocate(Z245)
       deallocate(S142)
C
       allocate(D1(N1+1:N3,M1+1:N2,M2+1:N3,M2+1:N3))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,M1,N2,M2,N3,M2,N3,VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S143(N0+1:M1,M1+1:N2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K8
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S143)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,M1,N2,M2,N3,M2,N3,
     & M1,N2,M2,N3,M2,N3,N0,M1,S143,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z246(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K7*K9*K9
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z246)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum235614(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z246, 1.000)
       deallocate(Z246)
       deallocate(S143)
C
       allocate(D1(N1+1:N3,N0+1:M1,N2+1:M2,M2+1:N3))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,N0,M1,N2,M2,M2,N3,VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S144(N0+1:M1,N0+1:M1,N2+1:M2,M2+1:N3))
       I1=K6*K0*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S144)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,M1,N2,M2,M2,N3,
     & N0,M1,N2,M2,M2,N3,N0,M1,S144,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z247(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K7*K9*K9
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z247)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum235614(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z247, 1.000)
       deallocate(Z247)
       deallocate(S144)
C
       allocate(D1(N1+1:N3,M1+1:N2,N2+1:M2,M2+1:N3))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,M1,N2,N2,M2,M2,N3,VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S145(N0+1:M1,M1+1:N2,N2+1:M2,M2+1:N3))
       I1=K6*K0*K8
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S145)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,M1,N2,N2,M2,M2,N3,
     & M1,N2,N2,M2,M2,N3,N0,M1,S145,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z248(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K7*K9*K9
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z248)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum235614(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z248, 1.000)
       deallocate(Z248)
       deallocate(S145)
C
       allocate(D1(N1+1:N3,N0+1:M1,M2+1:N3,N1+1:M2))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,N0,M1,M2,N3,N1,M2,VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S146(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S146)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,M1,M2,N3,N1,M2,
     & N0,M1,M2,N3,N1,M2,N0,M1,S146,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z249(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z249)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z249,-1.000)
       deallocate(Z249)
       deallocate(S146)
C
       allocate(D1(N1+1:N3,M1+1:N2,M2+1:N3,N1+1:M2))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,M1,N2,M2,N3,N1,M2,VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S147(N0+1:M1,M1+1:N2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K8
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S147)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder2341(N0,M1,M1,N2,M2,N3,N1,M2,
     & M1,N2,M2,N3,N1,M2,N0,M1,S147,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z250(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z250)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z250,-1.000)
       deallocate(Z250)
       deallocate(S147)
C
       allocate(D1(N1+1:N3,N0+1:M1,N2+1:M2,N1+1:M2))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,N0,M1,N2,M2,N1,M2,VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S148(N0+1:M1,N0+1:M1,N2+1:M2,N1+1:M2))
       I1=K9*K0*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S148)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,M1,N2,M2,N1,M2,
     & N0,M1,N2,M2,N1,M2,N0,M1,S148,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z251(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z251)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z251,-1.000)
       deallocate(Z251)
       deallocate(S148)
C
       allocate(D1(N1+1:N3,M1+1:N2,N2+1:M2,N1+1:M2))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,M1,N2,N2,M2,N1,M2,VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S149(N0+1:M1,M1+1:N2,N2+1:M2,N1+1:M2))
       I1=K9*K0*K8
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S149)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder2341(N0,M1,M1,N2,N2,M2,N1,M2,
     & M1,N2,N2,M2,N1,M2,N0,M1,S149,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z252(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z252)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z252,-1.000)
       deallocate(Z252)
       deallocate(S149)
C
       allocate(D1(N1+1:N3,N0+1:M1,M2+1:N3,N1+1:M2))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,N0,M1,M2,N3,N1,M2,VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S150(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S150)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,M1,M2,N3,N1,M2,
     & N0,M1,M2,N3,N1,M2,N0,M1,S150,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z253(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z253)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z253, 1.000)
       deallocate(Z253)
       deallocate(S150)
C
       allocate(D1(N1+1:N3,M1+1:N2,M2+1:N3,N1+1:M2))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,M1,N2,M2,N3,N1,M2,VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S151(N0+1:M1,M1+1:N2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K8
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S151)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder2341(N0,M1,M1,N2,M2,N3,N1,M2,
     & M1,N2,M2,N3,N1,M2,N0,M1,S151,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z254(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z254)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z254, 1.000)
       deallocate(Z254)
       deallocate(S151)
C
       allocate(D1(N1+1:N3,N0+1:M1,N2+1:M2,N1+1:M2))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,N0,M1,N2,M2,N1,M2,VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S152(N0+1:M1,N0+1:M1,N2+1:M2,N1+1:M2))
       I1=K9*K0*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S152)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,M1,N2,M2,N1,M2,
     & N0,M1,N2,M2,N1,M2,N0,M1,S152,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z255(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z255)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z255, 1.000)
       deallocate(Z255)
       deallocate(S152)
C
       allocate(D1(N1+1:N3,M1+1:N2,N2+1:M2,N1+1:M2))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,M1,N2,N2,M2,N1,M2,VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S153(N0+1:M1,M1+1:N2,N2+1:M2,N1+1:M2))
       I1=K9*K0*K8
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S153)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder2341(N0,M1,M1,N2,N2,M2,N1,M2,
     & M1,N2,N2,M2,N1,M2,N0,M1,S153,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z256(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z256)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z256, 1.000)
       deallocate(Z256)
       deallocate(S153)
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
       X13=X13+Q21
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
       X14=X14+Q22
       deallocate(Q22)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N0+1:M1))
       call reorder3142(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,N0,M1,VBHHHP,D1)
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
       X15=X15+Q23
       deallocate(Q23)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N0+1:M1))
       call reorder3142(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q24(M1+1:N1,N0+1:M1))
       I1=K5*K7
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q24)
       deallocate(D1)
       deallocate(B2)
C
       X16=X16+Q24
       deallocate(Q24)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,M2+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,N3,M2,N3,M2,N3,VBPHPP,D1)
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
       X17=X17+Q25
       deallocate(Q25)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,M2+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,N3,N1,M2,M2,N3,VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q26(N1+1:M2,M2+1:N3))
       I1=K6*K9
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q26)
       deallocate(D1)
       deallocate(B2)
C
       X18=X18+Q26
       deallocate(Q26)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,N3,M2,N3,N1,M2,VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q27(M2+1:N3,N1+1:M2))
       I1=K9*K6
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q27)
       deallocate(D1)
       deallocate(B2)
C
       X19=X19+Q27
       deallocate(Q27)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,N3,N1,M2,N1,M2,VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q28(N1+1:M2,N1+1:M2))
       I1=K9*K9
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q28)
       deallocate(D1)
       deallocate(B2)
C
       X20=X20+Q28
       deallocate(Q28)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,N3,M2,N3,N1,M2,VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q29(M2+1:N3,N1+1:M2))
       I1=K9*K6
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q29)
       deallocate(D1)
       deallocate(B2)
C
       X21=X21+Q29
       deallocate(Q29)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,N3,N1,M2,N1,M2,VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q30(N1+1:M2,N1+1:M2))
       I1=K9*K9
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q30)
       deallocate(D1)
       deallocate(B2)
C
       X22=X22+Q30
       deallocate(Q30)
C
       allocate(B1(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,FAHP,B1)
       allocate(D2(N0+1:N1,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,M2,N1,M2,N0,M1,t2A,D2)
       allocate(S154(N1+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K9
       I3=K1
       call EGEMM(I1,I2,I3,B1,D2,S154)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N1,N3,N1,M2,N1,M2,N0,M1,X12,S154,-1.000)
       deallocate(S154)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,FAHP,B1)
       allocate(D2(N1+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,M2,M1,N1,M1,N1,t2A,D2)
       allocate(S155(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S155)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X2,S155,-1.000)
       deallocate(S155)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,FAHP,B1)
       allocate(D2(N1+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,M2,M1,N1,M1,N1,t2A,D2)
       allocate(S156(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S156)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X3,S156,-1.000)
       deallocate(S156)
C
       allocate(B1(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,FAHP,B1)
       allocate(D2(N0+1:N1,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,M2,N1,M2,M1,N1,t2A,D2)
       allocate(S157(N1+1:M2,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K9
       I3=K1
       call EGEMM(I1,I2,I3,B1,D2,S157)
       deallocate(B1)
       deallocate(D2)
C
       allocate(X87(N1+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       X87=0.0d0
       call
     & sum2341(N1,N3,N1,M2,N1,M2,M1,N1,X87,S157, 1.000)
       deallocate(S157)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,FAHP,B1)
       allocate(D2(N1+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,M2,N0,M1,M1,N1,t2A,D2)
       allocate(S158(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S158)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X5,S158,-1.000)
       deallocate(S158)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,FAHP,B1)
       allocate(D2(N1+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,M2,N0,M1,M1,N1,t2A,D2)
       allocate(S159(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S159)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X6,S159,-1.000)
       deallocate(S159)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M1+1:N1))
       call reorder3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,M1,N1,VAHHHP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:M2,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,M2,N1,M2,t2A,D2)
       allocate(S160(N1+1:M2,N1+1:M2,N1+1:N3,M1+1:N1))
       I1=K7*K3
       I2=K9*K9
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S160)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N1,N3,N1,M2,N1,M2,M1,N1,X9,S160, 0.500)
       deallocate(S160)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M1+1:N1))
       call reorder3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,M1,N1,VAHHHP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,M2,N3,N1,M2,t2A,D2)
       allocate(S161(M2+1:N3,N1+1:M2,N1+1:N3,M1+1:N1))
       I1=K7*K3
       I2=K9*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S161)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N1,N3,M2,N3,N1,M2,M1,N1,X8,S161, 0.500)
       deallocate(S161)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M1+1:N1))
       call reorder3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,M1,N1,VAHHHP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,M2,N3,N1,M2,t2A,D2)
       allocate(S162(M2+1:N3,N1+1:M2,N1+1:N3,M1+1:N1))
       I1=K7*K3
       I2=K9*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S162)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N1,N3,M2,N3,N1,M2,M1,N1,X7,S162, 0.500)
       deallocate(S162)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M1+1:N1))
       call reorder4132(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,M1,N1,VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S163(N1+1:M2,N0+1:M1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S163)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N1,M2,N0,M1,M1,N1,X6,S163,-1.000)
       deallocate(S163)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M1+1:N1))
       call reorder3142(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,M1,N1,VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S164(N1+1:M2,N0+1:M1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S164)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N1,M2,N0,M1,M1,N1,X5,S164, 1.000)
       deallocate(S164)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M1+1:N1))
       call reorder3142(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,M1,N1,VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N1,t2A,D2)
       allocate(S165(M2+1:N3,M1+1:N1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S165)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,M2,N3,M1,N1,M1,N1,X76,S165, 1.000)
       deallocate(S165)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M1+1:N1))
       call reorder3142(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,M1,N1,VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2A,D2)
       allocate(S166(M2+1:N3,N0+1:M1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S166)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,M2,N3,N0,M1,M1,N1,X4,S166, 1.000)
       deallocate(S166)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M1+1:N1))
       call reorder3142(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,M1,N1,VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S167(N1+1:M2,M1+1:N1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S167)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N1,M2,M1,N1,M1,N1,X77,S167, 1.000)
       deallocate(S167)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M1+1:N1))
       call reorder4132(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,M1,N1,VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S168(N1+1:M2,M1+1:N1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S168)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N1,M2,M1,N1,M1,N1,X78,S168,-1.000)
       deallocate(S168)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:M1))
       call reorder3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N0,M1,VAHHHP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:M2,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,M2,N1,M2,t2A,D2)
       allocate(S169(N1+1:M2,N1+1:M2,N1+1:N3,N0+1:M1))
       I1=K5*K3
       I2=K9*K9
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S169)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N1,N3,N1,M2,N1,M2,N0,M1,X12,S169, 0.500)
       deallocate(S169)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:M1))
       call reorder3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N0,M1,VAHHHP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,M2,N3,N1,M2,t2A,D2)
       allocate(S170(M2+1:N3,N1+1:M2,N1+1:N3,N0+1:M1))
       I1=K5*K3
       I2=K9*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S170)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N1,N3,M2,N3,N1,M2,N0,M1,X11,S170, 0.500)
       deallocate(S170)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:M1))
       call reorder3412(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N0,M1,VAHHHP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,M2,N3,N1,M2,t2A,D2)
       allocate(S171(M2+1:N3,N1+1:M2,N1+1:N3,N0+1:M1))
       I1=K5*K3
       I2=K9*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S171)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N1,N3,M2,N3,N1,M2,N0,M1,X10,S171, 0.500)
       deallocate(S171)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,M2+1:N3))
       call reorder4213(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,M2,N3,VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S172(N1+1:M2,N0+1:M1,N1+1:N3,M2+1:N3))
       I1=K6*K3
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S172)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N1,N3,M2,N3,N1,M2,N0,M1,X11,S172, 1.000)
       deallocate(S172)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,M2+1:N3))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,M2,N3,VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S173(N1+1:M2,N0+1:M1,N1+1:N3,M2+1:N3))
       I1=K6*K3
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S173)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N1,N3,M2,N3,N1,M2,N0,M1,X10,S173,-1.000)
       deallocate(S173)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,M2+1:N3))
       call reorder1243(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N1,N3,N0,N1,M2,N3,VAHPPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,M1+1:N1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,M1,N1,M1,N1,t2A,D2)
       allocate(S174(M1+1:N1,M1+1:N1,N0+1:N1,M2+1:N3))
       I1=K6*K1
       I2=K7*K7
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S174)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,N1,M2,N3,M1,N1,M1,N1,X1,S174, 0.500)
       deallocate(S174)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder4213(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,M2,VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S175(N1+1:M2,N0+1:M1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S175)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N1,N3,N1,M2,N1,M2,N0,M1,X12,S175, 1.000)
       deallocate(S175)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder4213(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,M2,VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S176(N1+1:M2,N0+1:M1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S176)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N1,N3,N1,M2,N1,M2,N0,M1,X12,S176,-1.000)
       deallocate(S176)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,M2,VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2A,D2)
       allocate(S177(M2+1:N3,N0+1:M1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S177)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N1,N3,M2,N3,N1,M2,N0,M1,X10,S177, 1.000)
       deallocate(S177)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:M2))
       call reorder1243(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N1,N3,N0,N1,N1,M2,VAHPPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,M1+1:N1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,M1,N1,M1,N1,t2A,D2)
       allocate(S178(M1+1:N1,M1+1:N1,N0+1:N1,N1+1:M2))
       I1=K9*K1
       I2=K7*K7
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S178)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,N1,N1,M2,M1,N1,M1,N1,X2,S178, 0.500)
       deallocate(S178)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,M2,VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2A,D2)
       allocate(S179(M2+1:N3,N0+1:M1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S179)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N1,N3,M2,N3,N1,M2,N0,M1,X11,S179, 1.000)
       deallocate(S179)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:M2))
       call reorder1243(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N1,N3,N0,N1,N1,M2,VAHPPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,M1+1:N1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,M1,N1,M1,N1,t2A,D2)
       allocate(S180(M1+1:N1,M1+1:N1,N0+1:N1,N1+1:M2))
       I1=K9*K1
       I2=K7*K7
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S180)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,N1,N1,M2,M1,N1,M1,N1,X3,S180, 0.500)
       deallocate(S180)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder4132(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N0,M1,VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S181(N1+1:M2,M1+1:N1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S181)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X88(N0+1:N1,N1+1:M2,N0+1:M1,M1+1:N1))
       X88=0.0d0
       call
     & sum2413(N0,N1,N1,M2,N0,M1,M1,N1,X88,S181, 1.000)
       deallocate(S181)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder3142(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N0,M1,VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S182(N1+1:M2,M1+1:N1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S182)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X89(N0+1:N1,N1+1:M2,N0+1:M1,M1+1:N1))
       X89=0.0d0
       call
     & sum2413(N0,N1,N1,M2,N0,M1,M1,N1,X89,S182, 1.000)
       deallocate(S182)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder3142(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N0,M1,VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N1,t2A,D2)
       allocate(S183(M2+1:N3,M1+1:N1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S183)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N1,M2,N3,N0,M1,M1,N1,X4,S183,-1.000)
       deallocate(S183)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,M2+1:N3))
       call reorder4213(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,M2,N3,VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S184(N1+1:M2,M1+1:N1,N1+1:N3,M2+1:N3))
       I1=K6*K3
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S184)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X90(N1+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       X90=0.0d0
       call
     & sum3412(N1,N3,M2,N3,N1,M2,M1,N1,X90,S184, 1.000)
       deallocate(S184)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,M2+1:N3))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,M2,N3,VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S185(N1+1:M2,M1+1:N1,N1+1:N3,M2+1:N3))
       I1=K6*K3
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S185)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X91(N1+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       X91=0.0d0
       call
     & sum3412(N1,N3,M2,N3,N1,M2,M1,N1,X91,S185, 1.000)
       deallocate(S185)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,M2+1:N3))
       call reorder1243(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N1,N3,N0,N1,M2,N3,VAHPPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,M1,M1,N1,t2A,D2)
       allocate(S186(N0+1:M1,M1+1:N1,N0+1:N1,M2+1:N3))
       I1=K6*K1
       I2=K7*K5
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S186)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,N1,M2,N3,N0,M1,M1,N1,X4,S186, 0.500)
       deallocate(S186)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder4213(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,M2,VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S187(N1+1:M2,M1+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S187)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N1,N3,N1,M2,N1,M2,M1,N1,X87,S187,-1.000)
       deallocate(S187)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder4213(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,M2,VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S188(N1+1:M2,M1+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S188)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N1,N3,N1,M2,N1,M2,M1,N1,X87,S188, 1.000)
       deallocate(S188)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,M2,VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N1,t2A,D2)
       allocate(S189(M2+1:N3,M1+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S189)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N1,N3,M2,N3,N1,M2,M1,N1,X91,S189,-1.000)
       deallocate(S189)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:M2))
       call reorder1243(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N1,N3,N0,N1,N1,M2,VAHPPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,M1,M1,N1,t2A,D2)
       allocate(S190(N0+1:M1,M1+1:N1,N0+1:N1,N1+1:M2))
       I1=K9*K1
       I2=K7*K5
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S190)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,N1,N1,M2,N0,M1,M1,N1,X5,S190, 0.500)
       deallocate(S190)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,M2,VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N1,t2A,D2)
       allocate(S191(M2+1:N3,M1+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S191)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N1,N3,M2,N3,N1,M2,M1,N1,X90,S191, 1.000)
       deallocate(S191)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:M2))
       call reorder1243(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N1,N3,N0,N1,N1,M2,VAHPPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,M1,M1,N1,t2A,D2)
       allocate(S192(N0+1:M1,M1+1:N1,N0+1:N1,N1+1:M2))
       I1=K9*K1
       I2=K7*K5
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S192)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,N1,N1,M2,N0,M1,M1,N1,X6,S192, 0.500)
       deallocate(S192)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M1+1:N1))
       call reorder3142(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,M1,N1,VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S193(N1+1:M2,N0+1:M1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S193)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N1,M2,N0,M1,M1,N1,X6,S193, 1.000)
       deallocate(S193)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M1+1:N1))
       call reorder3142(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,M1,N1,VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S194(N1+1:M2,N0+1:M1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S194)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N1,M2,N0,M1,M1,N1,X5,S194, 1.000)
       deallocate(S194)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M1+1:N1))
       call reorder3142(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,M1,N1,VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S195(M2+1:N3,N0+1:M1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S195)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,M2,N3,N0,M1,M1,N1,X4,S195, 1.000)
       deallocate(S195)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M1+1:N1))
       call reorder3142(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,M1,N1,VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S196(N1+1:M2,M1+1:N1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S196)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N1,M2,M1,N1,M1,N1,X78,S196, 1.000)
       deallocate(S196)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M1+1:N1))
       call reorder3142(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,M1,N1,VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S197(N1+1:M2,M1+1:N1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S197)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N1,M2,M1,N1,M1,N1,X77,S197, 1.000)
       deallocate(S197)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M1+1:N1))
       call reorder3142(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,M1,N1,VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,M1,N1,t2B,D2)
       allocate(S198(M2+1:N3,M1+1:N1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S198)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,M2,N3,M1,N1,M1,N1,X76,S198, 1.000)
       deallocate(S198)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,M2+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,N3,N1,N3,M2,N3,VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S199(N1+1:M2,N0+1:M1,N1+1:N3,M2+1:N3))
       I1=K6*K3
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S199)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N1,N3,M2,N3,N1,M2,N0,M1,X11,S199, 1.000)
       deallocate(S199)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,M2+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,N3,N1,N3,M2,N3,VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S200(N1+1:M2,N0+1:M1,N1+1:N3,M2+1:N3))
       I1=K6*K3
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S200)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N1,N3,M2,N3,N1,M2,N0,M1,X10,S200, 1.000)
       deallocate(S200)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,N3,N1,N3,N1,M2,VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S201(N1+1:M2,N0+1:M1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S201)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N1,N3,N1,M2,N1,M2,N0,M1,X12,S201, 1.000)
       deallocate(S201)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,N3,N1,N3,N1,M2,VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S202(N1+1:M2,N0+1:M1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S202)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N1,N3,N1,M2,N1,M2,N0,M1,X12,S202,-1.000)
       deallocate(S202)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,N3,N1,N3,N1,M2,VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S203(M2+1:N3,N0+1:M1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S203)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N1,N3,M2,N3,N1,M2,N0,M1,X10,S203,-1.000)
       deallocate(S203)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,N3,N1,N3,N1,M2,VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S204(M2+1:N3,N0+1:M1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S204)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N1,N3,M2,N3,N1,M2,N0,M1,X11,S204,-1.000)
       deallocate(S204)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder3142(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N0,M1,VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S205(N1+1:M2,M1+1:N1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S205)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N1,N1,M2,N0,M1,M1,N1,X88,S205,-1.000)
       deallocate(S205)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder3142(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N0,M1,VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S206(N1+1:M2,M1+1:N1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S206)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N1,N1,M2,N0,M1,M1,N1,X89,S206, 1.000)
       deallocate(S206)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder3142(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N0,M1,VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,M1,N1,t2B,D2)
       allocate(S207(M2+1:N3,M1+1:N1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S207)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X92(N0+1:N1,M2+1:N3,N0+1:M1,M1+1:N1))
       X92=0.0d0
       call
     & sum2413(N0,N1,M2,N3,N0,M1,M1,N1,X92,S207, 1.000)
       deallocate(S207)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,M2+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,N3,N1,N3,M2,N3,VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S208(N1+1:M2,M1+1:N1,N1+1:N3,M2+1:N3))
       I1=K6*K3
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S208)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N1,N3,M2,N3,N1,M2,M1,N1,X90,S208, 1.000)
       deallocate(S208)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,M2+1:N3))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,N3,N1,N3,M2,N3,VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S209(N1+1:M2,M1+1:N1,N1+1:N3,M2+1:N3))
       I1=K6*K3
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S209)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N1,N3,M2,N3,N1,M2,M1,N1,X91,S209,-1.000)
       deallocate(S209)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,N3,N1,N3,N1,M2,VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S210(N1+1:M2,M1+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S210)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N1,N3,N1,M2,N1,M2,M1,N1,X87,S210,-1.000)
       deallocate(S210)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,N3,N1,N3,N1,M2,VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S211(N1+1:M2,M1+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S211)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N1,N3,N1,M2,N1,M2,M1,N1,X87,S211, 1.000)
       deallocate(S211)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,N3,N1,N3,N1,M2,VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,M1,N1,t2B,D2)
       allocate(S212(M2+1:N3,M1+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S212)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N1,N3,M2,N3,N1,M2,M1,N1,X91,S212, 1.000)
       deallocate(S212)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,N3,N1,N3,N1,M2,VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,M1,N1,t2B,D2)
       allocate(S213(M2+1:N3,M1+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S213)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N1,N3,M2,N3,N1,M2,M1,N1,X90,S213,-1.000)
       deallocate(S213)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M1,N1,M2,N3,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder461235(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,N1,M2,N1,M2,N0,M1,t3A,F2)
       allocate(S214(N1+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K9
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S214)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N1,M2,N1,M2,N0,M1,X12,S214,-1.000)
       deallocate(S214)
C
       allocate(D1(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M1,N1,M2,N3,N1,N3,VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder561234(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M1,N1,M2,N3,N1,M2,N1,M2,N0,M1,t3A,F2)
       allocate(S215(N1+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K9
       I3=K6*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S215)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N1,M2,N1,M2,N0,M1,X12,S215, 0.500)
       deallocate(S215)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M1,N1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder461235(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M1,N1,N1,M2,N1,M2,N1,M2,N0,M1,t3A,F2)
       allocate(S216(N1+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K9
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S216)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N1,M2,N1,M2,N0,M1,X12,S216,-1.000)
       deallocate(S216)
C
       allocate(D1(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M1,N1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder561234(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M1,N1,N1,M2,N1,M2,N1,M2,N0,M1,t3A,F2)
       allocate(S217(N1+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K9
       I3=K9*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S217)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N1,M2,N1,M2,N0,M1,X12,S217, 0.500)
       deallocate(S217)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M1,N1,M2,N3,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder461235(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,M2,N3,N1,M2,N0,M1,t3A,F2)
       allocate(S218(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S218)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,N0,M1,X11,S218,-1.000)
       deallocate(S218)
C
       allocate(D1(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M1,N1,M2,N3,N1,N3,VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder561234(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M1,N1,M2,N3,M2,N3,N1,M2,N0,M1,t3A,F2)
       allocate(S219(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K6*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S219)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,N0,M1,X11,S219, 0.500)
       deallocate(S219)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M1,N1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder462135(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M1,N1,N1,M2,M2,N3,N1,M2,N0,M1,t3A,F2)
       allocate(S220(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S220)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,N0,M1,X11,S220, 1.000)
       deallocate(S220)
C
       allocate(D1(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M1,N1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder562134(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M1,N1,N1,M2,M2,N3,N1,M2,N0,M1,t3A,F2)
       allocate(S221(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K9*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S221)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,N0,M1,X11,S221,-0.500)
       deallocate(S221)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M1,N1,M2,N3,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder461235(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,M2,N3,N1,M2,N0,M1,t3A,F2)
       allocate(S222(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S222)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,N0,M1,X10,S222, 1.000)
       deallocate(S222)
C
       allocate(D1(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M1,N1,M2,N3,N1,N3,VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder561234(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M1,N1,M2,N3,M2,N3,N1,M2,N0,M1,t3A,F2)
       allocate(S223(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K6*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S223)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,N0,M1,X10,S223,-0.500)
       deallocate(S223)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M1,N1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder462135(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M1,N1,N1,M2,M2,N3,N1,M2,N0,M1,t3A,F2)
       allocate(S224(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S224)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,N0,M1,X10,S224,-1.000)
       deallocate(S224)
C
       allocate(D1(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M1,N1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder562134(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M1,N1,N1,M2,M2,N3,N1,M2,N0,M1,t3A,F2)
       allocate(S225(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K9*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S225)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,N0,M1,X10,S225, 0.500)
       deallocate(S225)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,M1,M1,N1,VAHHPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,M1+1:N1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,M1,N1,M1,N1,t2A,D2)
       allocate(S226(M1+1:N1,M1+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5
       I2=K7*K7
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S226)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X23(N0+1:M1,M1+1:N1,M1+1:N1,M1+1:N1))
       X23=0.0d0
       call
     & sum3412(N0,M1,M1,N1,M1,N1,M1,N1,X23,S226, 0.500)
       deallocate(S226)
C
       allocate(D1(N1+1:N3,N1+1:N3,M1+1:N1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,M1,N1,M1,N1,VAHHPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,M1+1:N1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,M1,N1,M1,N1,t2A,D2)
       allocate(S227(M1+1:N1,M1+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7
       I2=K7*K7
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S227)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X24(M1+1:N1,M1+1:N1,M1+1:N1,M1+1:N1))
       X24=0.0d0
       call
     & sum3412(M1,N1,M1,N1,M1,N1,M1,N1,X24,S227, 0.500)
       deallocate(S227)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,N0,M1,M2,N3,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N0,M1,M2,N3,N1,M2,N1,M2,M1,N1,t3A,F2)
       allocate(S228(N1+1:M2,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K9
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S228)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N1,M2,N1,M2,M1,N1,X87,S228,-0.500)
       deallocate(S228)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M1,N1,M2,N3,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,N1,M2,N1,M2,M1,N1,t3A,F2)
       allocate(S229(N1+1:M2,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K9
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S229)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N1,M2,N1,M2,M1,N1,X87,S229,-1.000)
       deallocate(S229)
C
       allocate(D1(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M1,N1,M2,N3,N1,N3,VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M1,N1,M2,N3,N1,M2,N1,M2,M1,N1,t3A,F2)
       allocate(S230(N1+1:M2,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K9
       I3=K6*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S230)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N1,M2,N1,M2,M1,N1,X87,S230,-0.500)
       deallocate(S230)
C
       allocate(D1(N0+1:M1,N0+1:M1,N1+1:M2,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,N0,M1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N0,M1,N1,M2,N1,M2,N1,M2,M1,N1,t3A,F2)
       allocate(S231(N1+1:M2,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K9
       I3=K9*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S231)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N1,M2,N1,M2,M1,N1,X87,S231,-0.500)
       deallocate(S231)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M1,N1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M1,N1,N1,M2,N1,M2,N1,M2,M1,N1,t3A,F2)
       allocate(S232(N1+1:M2,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K9
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S232)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N1,M2,N1,M2,M1,N1,X87,S232,-1.000)
       deallocate(S232)
C
       allocate(D1(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M1,N1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M1,N1,N1,M2,N1,M2,N1,M2,M1,N1,t3A,F2)
       allocate(S233(N1+1:M2,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K9
       I3=K9*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S233)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N1,M2,N1,M2,M1,N1,X87,S233,-0.500)
       deallocate(S233)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,N0,M1,M2,N3,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder451236(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N0,M1,M2,N3,M2,N3,N1,M2,M1,N1,t3A,F2)
       allocate(S234(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S234)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N1,X90,S234, 0.500)
       deallocate(S234)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M1,N1,M2,N3,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder451236(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,M2,N3,N1,M2,M1,N1,t3A,F2)
       allocate(S235(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S235)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N1,X90,S235, 1.000)
       deallocate(S235)
C
       allocate(D1(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M1,N1,M2,N3,N1,N3,VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder451236(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M1,N1,M2,N3,M2,N3,N1,M2,M1,N1,t3A,F2)
       allocate(S236(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S236)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N1,X90,S236, 0.500)
       deallocate(S236)
C
       allocate(D1(N0+1:M1,N0+1:M1,N1+1:M2,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,N0,M1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder452136(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N0,M1,N1,M2,M2,N3,N1,M2,M1,N1,t3A,F2)
       allocate(S237(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K9*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S237)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N1,X90,S237,-0.500)
       deallocate(S237)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M1,N1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder452136(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M1,N1,N1,M2,M2,N3,N1,M2,M1,N1,t3A,F2)
       allocate(S238(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S238)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N1,X90,S238,-1.000)
       deallocate(S238)
C
       allocate(D1(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M1,N1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder452136(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M1,N1,N1,M2,M2,N3,N1,M2,M1,N1,t3A,F2)
       allocate(S239(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K9*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S239)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N1,X90,S239,-0.500)
       deallocate(S239)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,N0,M1,M2,N3,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder451236(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N0,M1,M2,N3,M2,N3,N1,M2,M1,N1,t3A,F2)
       allocate(S240(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S240)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N1,X91,S240, 0.500)
       deallocate(S240)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M1,N1,M2,N3,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder451236(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,M2,N3,N1,M2,M1,N1,t3A,F2)
       allocate(S241(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S241)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N1,X91,S241, 1.000)
       deallocate(S241)
C
       allocate(D1(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M1,N1,M2,N3,N1,N3,VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder451236(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M1,N1,M2,N3,M2,N3,N1,M2,M1,N1,t3A,F2)
       allocate(S242(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S242)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N1,X91,S242, 0.500)
       deallocate(S242)
C
       allocate(D1(N0+1:M1,N0+1:M1,N1+1:M2,N1+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,N0,M1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder452136(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N0,M1,N1,M2,M2,N3,N1,M2,M1,N1,t3A,F2)
       allocate(S243(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K9*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S243)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N1,X91,S243,-0.500)
       deallocate(S243)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M1,N1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder452136(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M1,N1,N1,M2,M2,N3,N1,M2,M1,N1,t3A,F2)
       allocate(S244(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K9*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S244)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N1,X91,S244,-1.000)
       deallocate(S244)
C
       allocate(D1(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M1,N1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder452136(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M1,N1,N1,M2,M2,N3,N1,M2,M1,N1,t3A,F2)
       allocate(S245(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K9*K7*K7
       call EGEMM(I1,I2,I3,D1,F2,S245)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N1,X91,S245,-0.500)
       deallocate(S245)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,M1,N0,M1,VAHHPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,M1,M1,N1,t2A,D2)
       allocate(S246(N0+1:M1,M1+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K7*K5
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S246)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N0,M1,N0,M1,M1,N1,X25,S246, 0.500)
       deallocate(S246)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,M1,M1,N1,VAHHPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,M1,M1,N1,t2A,D2)
       allocate(S247(N0+1:M1,M1+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5
       I2=K7*K5
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S247)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M1,N1,N0,M1,M1,N1,X26,S247, 0.500)
       deallocate(S247)
C
       allocate(D1(N1+1:N3,N1+1:N3,M1+1:N1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,M1,N1,M1,N1,VAHHPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,M1,M1,N1,t2A,D2)
       allocate(S248(N0+1:M1,M1+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7
       I2=K7*K5
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S248)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,M1,N1,N0,M1,M1,N1,X27,S248, 0.500)
       deallocate(S248)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder4123(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(S249(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S249)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X88,S249, 0.500)
       deallocate(S249)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder4123(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,M2,N3,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder512346(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(S250(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K6*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S250)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X88,S250,-0.500)
       deallocate(S250)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder4123(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(S251(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S251)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X88,S251, 1.000)
       deallocate(S251)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder4123(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder512346(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(S252(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S252)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X88,S252,-1.000)
       deallocate(S252)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder4123(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,N1,M2,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N1,M2,N1,M2,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(S253(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K9*K5
       call EGEMM(I1,I2,I3,D1,F2,S253)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X88,S253, 0.500)
       deallocate(S253)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder4123(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,N1,M2,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder512346(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,N1,M2,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(S254(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K9*K7
       call EGEMM(I1,I2,I3,D1,F2,S254)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X88,S254,-0.500)
       deallocate(S254)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(S255(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S255)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X89,S255, 0.500)
       deallocate(S255)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,M2,N3,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder512346(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(S256(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K6*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S256)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X89,S256,-0.500)
       deallocate(S256)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(S257(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S257)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X89,S257, 1.000)
       deallocate(S257)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder512346(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(S258(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S258)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X89,S258,-1.000)
       deallocate(S258)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,N1,M2,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N1,M2,N1,M2,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(S259(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K9*K5
       call EGEMM(I1,I2,I3,D1,F2,S259)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X89,S259, 0.500)
       deallocate(S259)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,N1,M2,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder512346(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,N1,M2,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(S260(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K9*K7
       call EGEMM(I1,I2,I3,D1,F2,S260)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X89,S260,-0.500)
       deallocate(S260)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N1,t2A,D2)
       allocate(S261(M2+1:N3,M1+1:N1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S261)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,M2,N3,M1,N1,X28,S261,-1.000)
       deallocate(S261)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N1,t2A,D2)
       allocate(S262(M2+1:N3,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S262)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,M2,N3,M2,N3,M1,N1,X29,S262,-1.000)
       deallocate(S262)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N1,t2A,D2)
       allocate(S263(M2+1:N3,M1+1:N1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S263)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N1,M2,M2,N3,M1,N1,X30,S263,-1.000)
       deallocate(S263)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N1,t2A,D2)
       allocate(S264(M2+1:N3,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S264)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,N1,M2,M2,N3,M1,N1,X31,S264,-1.000)
       deallocate(S264)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder413256(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,M2,N3,N0,M1,M1,N1,t3A,F2)
       allocate(S265(M2+1:N3,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K6
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S265)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,N0,M1,M1,N1,X92,S265,-1.000)
       deallocate(S265)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder513246(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,M2,N3,N0,M1,M1,N1,t3A,F2)
       allocate(S266(M2+1:N3,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K6
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S266)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,N0,M1,M1,N1,X92,S266, 1.000)
       deallocate(S266)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,N1,M2,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder423156(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N1,M2,N1,M2,M2,N3,N0,M1,M1,N1,t3A,F2)
       allocate(S267(M2+1:N3,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K6
       I3=K9*K9*K5
       call EGEMM(I1,I2,I3,D1,F2,S267)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,N0,M1,M1,N1,X92,S267, 0.500)
       deallocate(S267)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,N1,M2,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder523146(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,N1,M2,M2,N3,N0,M1,M1,N1,t3A,F2)
       allocate(S268(M2+1:N3,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K6
       I3=K9*K9*K7
       call EGEMM(I1,I2,I3,D1,F2,S268)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,N0,M1,M1,N1,X92,S268,-0.500)
       deallocate(S268)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S269(N1+1:M2,M1+1:N1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S269)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,N1,M2,M1,N1,X32,S269,-1.000)
       deallocate(S269)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S270(N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S270)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,M2,N3,N1,M2,M1,N1,X33,S270,-1.000)
       deallocate(S270)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S271(N1+1:M2,M1+1:N1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S271)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N1,M2,N1,M2,M1,N1,X34,S271,-1.000)
       deallocate(S271)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S272(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S272)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,N1,M2,N1,M2,M1,N1,X35,S272,-1.000)
       deallocate(S272)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S273(N1+1:M2,M1+1:N1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S273)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3412(N1,M2,M1,N1,N0,M1,M2,N3,
     & N0,M1,M2,N3,N1,M2,M1,N1,S273,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z386(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z386)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z386, 1.000)
       call
     & sum124635(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z386,-1.000)
       deallocate(Z386)
       deallocate(S273)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S274(N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S274)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3412(N1,M2,M1,N1,M1,N1,M2,N3,
     & M1,N1,M2,N3,N1,M2,M1,N1,S274,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder512346(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z387(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z387)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z387,-1.000)
       call
     & sum124635(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z387, 1.000)
       deallocate(Z387)
       deallocate(S274)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S275(N1+1:M2,M1+1:N1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S275)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder3412(N1,M2,M1,N1,N0,M1,N1,M2,
     & N0,M1,N1,M2,N1,M2,M1,N1,S275,D1)
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder421356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N1,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z388(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,D1,F2,Z388)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z388,-1.000)
       call
     & sum124635(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z388, 1.000)
       deallocate(Z388)
       deallocate(S275)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S276(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S276)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder3412(N1,M2,M1,N1,M1,N1,N1,M2,
     & M1,N1,N1,M2,N1,M2,M1,N1,S276,D1)
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder521346(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z389(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,D1,F2,Z389)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z389, 1.000)
       call
     & sum124635(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z389,-1.000)
       deallocate(Z389)
       deallocate(S276)
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
     & sum21(N0,M1,M1,N1,X13,Q31, 0.500)
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
     & sum21(M1,N1,M1,N1,X14,Q32, 0.500)
       deallocate(Q32)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder4123(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(S277(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S277)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X3,S277, 0.500)
       deallocate(S277)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder4123(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,M2,N3,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(S278(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K6*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S278)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X3,S278, 0.500)
       deallocate(S278)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder4123(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(S279(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S279)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X3,S279, 1.000)
       deallocate(S279)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder4123(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(S280(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S280)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X3,S280, 1.000)
       deallocate(S280)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder4123(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,N1,M2,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N1,M2,N1,M2,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(S281(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K9*K9*K5
       call EGEMM(I1,I2,I3,D1,F2,S281)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X3,S281, 0.500)
       deallocate(S281)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder4123(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,N1,M2,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,N1,M2,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(S282(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K9*K9*K7
       call EGEMM(I1,I2,I3,D1,F2,S282)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X3,S282, 0.500)
       deallocate(S282)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(S283(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S283)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X2,S283,-0.500)
       deallocate(S283)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,M2,N3,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(S284(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K6*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S284)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X2,S284,-0.500)
       deallocate(S284)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(S285(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S285)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X2,S285,-1.000)
       deallocate(S285)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(S286(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S286)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X2,S286,-1.000)
       deallocate(S286)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,N1,M2,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N1,M2,N1,M2,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(S287(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K9*K9*K5
       call EGEMM(I1,I2,I3,D1,F2,S287)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X2,S287,-0.500)
       deallocate(S287)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,N1,M2,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,N1,M2,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(S288(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K9*K9*K7
       call EGEMM(I1,I2,I3,D1,F2,S288)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X2,S288,-0.500)
       deallocate(S288)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2A,D2)
       allocate(S289(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S289)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,M2,N3,N0,M1,X40,S289,-1.000)
       deallocate(S289)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2A,D2)
       allocate(S290(M2+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S290)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,M2,N3,M2,N3,N0,M1,X41,S290,-1.000)
       deallocate(S290)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2A,D2)
       allocate(S291(M2+1:N3,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S291)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N1,M2,M2,N3,N0,M1,X42,S291,-1.000)
       deallocate(S291)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2A,D2)
       allocate(S292(M2+1:N3,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S292)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,N1,M2,M2,N3,N0,M1,X43,S292,-1.000)
       deallocate(S292)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder413256(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,M2,N3,M1,N1,M1,N1,t3A,F2)
       allocate(S293(M2+1:N3,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K6
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S293)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,M1,N1,M1,N1,X1,S293, 1.000)
       deallocate(S293)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder413256(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,M2,N3,M1,N1,M1,N1,t3A,F2)
       allocate(S294(M2+1:N3,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K6
       I3=K9*K6*K7
       call EGEMM(I1,I2,I3,D1,F2,S294)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,M1,N1,M1,N1,X1,S294, 1.000)
       deallocate(S294)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,N1,M2,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N1+1:M2,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder423156(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N1,M2,N1,M2,M2,N3,M1,N1,M1,N1,t3A,F2)
       allocate(S295(M2+1:N3,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K6
       I3=K9*K9*K5
       call EGEMM(I1,I2,I3,D1,F2,S295)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,M1,N1,M1,N1,X1,S295,-0.500)
       deallocate(S295)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,N1,M2,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder423156(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,N1,M2,M2,N3,M1,N1,M1,N1,t3A,F2)
       allocate(S296(M2+1:N3,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K6
       I3=K9*K9*K7
       call EGEMM(I1,I2,I3,D1,F2,S296)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,M1,N1,M1,N1,X1,S296,-0.500)
       deallocate(S296)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S297(N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S297)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,N1,M2,N0,M1,X44,S297,-1.000)
       deallocate(S297)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S298(N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S298)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,M2,N3,N1,M2,N0,M1,X45,S298,-1.000)
       deallocate(S298)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S299(N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S299)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N1,M2,N1,M2,N0,M1,X46,S299,-1.000)
       deallocate(S299)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S300(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S300)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,N1,M2,N1,M2,N0,M1,X47,S300,-1.000)
       deallocate(S300)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S301(N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S301)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3412(N1,M2,N0,M1,N0,M1,M2,N3,
     & N0,M1,M2,N3,N1,M2,N0,M1,S301,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(Z416(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z416)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z416, 1.000)
       deallocate(Z416)
       deallocate(S301)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S302(N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S302)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3412(N1,M2,N0,M1,M1,N1,M2,N3,
     & M1,N1,M2,N3,N1,M2,N0,M1,S302,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(Z417(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,D1,F2,Z417)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z417, 1.000)
       deallocate(Z417)
       deallocate(S302)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S303(N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S303)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder3412(N1,M2,N0,M1,N0,M1,N1,M2,
     & N0,M1,N1,M2,N1,M2,N0,M1,S303,D1)
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder421356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N1,M2,M2,N3,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(Z418(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,D1,F2,Z418)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z418,-1.000)
       deallocate(Z418)
       deallocate(S303)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S304(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S304)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder3412(N1,M2,N0,M1,M1,N1,N1,M2,
     & M1,N1,N1,M2,N1,M2,N0,M1,S304,D1)
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder421356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,M2,N3,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(Z419(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,D1,F2,Z419)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z419,-1.000)
       deallocate(Z419)
       deallocate(S304)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,M1,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,M1,t2A,D2)
       allocate(Q33(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K3*K3*K1
       call EGEMM(I1,I2,I3,D1,D2,Q33)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N0,M1,N0,M1,X15,Q33, 0.500)
       deallocate(Q33)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,M1,N1,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,M1,t2A,D2)
       allocate(Q34(N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5
       I3=K3*K3*K1
       call EGEMM(I1,I2,I3,D1,D2,Q34)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M1,N1,N0,M1,X16,Q34, 0.500)
       deallocate(Q34)
C
       allocate(D1(N0+1:N1,N0+1:N1,M2+1:N3,M2+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,M2,N3,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,M2,N3,N1,M2,t2A,D2)
       allocate(S305(M2+1:N3,N1+1:M2,M2+1:N3,M2+1:N3))
       I1=K6*K6
       I2=K9*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S305)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M2,N3,M2,N3,M2,N3,N1,M2,X52,S305, 0.500)
       deallocate(S305)
C
       allocate(D1(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,M2,N3,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,M2,N3,N1,M2,t2A,D2)
       allocate(S306(M2+1:N3,N1+1:M2,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K9*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S306)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M2,N3,N1,M2,M2,N3,N1,M2,X53,S306, 0.500)
       deallocate(S306)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:M2,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,M2,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,M2,N3,N1,M2,t2A,D2)
       allocate(S307(M2+1:N3,N1+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9
       I2=K9*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S307)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N1,M2,N1,M2,M2,N3,N1,M2,X54,S307, 0.500)
       deallocate(S307)
C
       allocate(D1(N0+1:N1,N0+1:N1,M2+1:N3,M2+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,M2,N3,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,M2,N3,N1,M2,t2A,D2)
       allocate(S308(M2+1:N3,N1+1:M2,M2+1:N3,M2+1:N3))
       I1=K6*K6
       I2=K9*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S308)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M2,N3,M2,N3,M2,N3,N1,M2,X55,S308, 0.500)
       deallocate(S308)
C
       allocate(D1(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,M2,N3,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,M2,N3,N1,M2,t2A,D2)
       allocate(S309(M2+1:N3,N1+1:M2,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K9*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S309)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M2,N3,N1,M2,M2,N3,N1,M2,X56,S309, 0.500)
       deallocate(S309)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:M2,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,M2,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,M2,N3,N1,M2,t2A,D2)
       allocate(S310(M2+1:N3,N1+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9
       I2=K9*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S310)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N1,M2,N1,M2,M2,N3,N1,M2,X57,S310, 0.500)
       deallocate(S310)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,M2,N3,t2A,D2)
       allocate(Q35(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K3*K1*K1
       call EGEMM(I1,I2,I3,D1,D2,Q35)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M2,N3,M2,N3,X17,Q35,-0.500)
       deallocate(Q35)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,M2,N3,t2A,D2)
       allocate(Q36(M2+1:N3,N1+1:M2))
       I1=K9
       I2=K6
       I3=K3*K1*K1
       call EGEMM(I1,I2,I3,D1,D2,Q36)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N1,M2,M2,N3,X18,Q36,-0.500)
       deallocate(Q36)
C
       allocate(D1(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,M2,N3,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:M2,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,M2,N1,M2,t2A,D2)
       allocate(S311(N1+1:M2,N1+1:M2,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K9*K9
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S311)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M2,N3,N1,M2,N1,M2,N1,M2,X58,S311, 0.500)
       deallocate(S311)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:M2,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,M2,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:M2,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,M2,N1,M2,t2A,D2)
       allocate(S312(N1+1:M2,N1+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9
       I2=K9*K9
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S312)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N1,M2,N1,M2,N1,M2,N1,M2,X59,S312, 0.500)
       deallocate(S312)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,M2,t2A,D2)
       allocate(Q37(N1+1:M2,M2+1:N3))
       I1=K6
       I2=K9
       I3=K3*K1*K1
       call EGEMM(I1,I2,I3,D1,D2,Q37)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M2,N3,N1,M2,X19,Q37,-0.500)
       deallocate(Q37)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,M2,t2A,D2)
       allocate(Q38(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K3*K1*K1
       call EGEMM(I1,I2,I3,D1,D2,Q38)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N1,M2,N1,M2,X20,Q38,-0.500)
       deallocate(Q38)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,M2,t2A,D2)
       allocate(Q39(N1+1:M2,M2+1:N3))
       I1=K6
       I2=K9
       I3=K3*K1*K1
       call EGEMM(I1,I2,I3,D1,D2,Q39)
       deallocate(D1)
       deallocate(D2)
C
       allocate(B1(M2+1:N3,N1+1:M2))
       call reorder21(N1,M2,M2,N3,
     & M2,N3,N1,M2,Q39,B1)
       allocate(F2(M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,M1+1:N1))
       call reorder123456(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,M1,N1,t3A,F2)
       allocate(Z434(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K5*K9*K6
       I3=K6
       call EGEMM(I1,I2,I3,B1,F2,Z434)
       deallocate(B1)
       deallocate(F2)
C
       call
     & sum124563(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z434, 0.500)
       deallocate(Z434)
       deallocate(Q39)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,M2,t2A,D2)
       allocate(Q40(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K3*K1*K1
       call EGEMM(I1,I2,I3,D1,D2,Q40)
       deallocate(D1)
       deallocate(D2)
C
       allocate(B1(N1+1:M2,N1+1:M2))
       call reorder21(N1,M2,N1,M2,
     & N1,M2,N1,M2,Q40,B1)
       allocate(F2(N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,M1+1:N1))
       call reorder213456(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N1,M2,M2,N3,N1,M2,N0,M1,M1,N1,M1,N1,t3A,F2)
       allocate(Z435(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K5*K9*K6
       I3=K9
       call EGEMM(I1,I2,I3,B1,F2,Z435)
       deallocate(B1)
       deallocate(F2)
C
       call
     & sum124563(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z435,-0.500)
       deallocate(Z435)
       deallocate(Q40)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder451236(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,N0,M1,M2,N3,N1,M2,N1,M2,N0,M1,t3B4,F2)
       allocate(S313(N1+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K9
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S313)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N1,M2,N1,M2,N0,M1,X12,S313,-1.000)
       deallocate(S313)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,N1,M2,N1,M2,N0,M1,t3B1,F2)
       allocate(S314(N1+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K9
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S314)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N1,M2,N1,M2,N0,M1,X12,S314, 1.000)
       deallocate(S314)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,M2,N3,N1,M2,N1,M2,N0,M1,t3B1,F2)
       allocate(S315(N1+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K9
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S315)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N1,M2,N1,M2,N0,M1,X12,S315, 1.000)
       deallocate(S315)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder451236(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,N0,M1,N2,M2,N1,M2,N1,M2,N0,M1,t3B4,F2)
       allocate(S316(N1+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K9
       I3=K0*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S316)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N1,M2,N1,M2,N0,M1,X12,S316,-1.000)
       deallocate(S316)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,N2,M2,N1,M2,N1,M2,N0,M1,t3B1,F2)
       allocate(S317(N1+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K9
       I3=K0*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S317)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N1,M2,N1,M2,N0,M1,X12,S317, 1.000)
       deallocate(S317)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,N2,M2,N1,M2,N1,M2,N0,M1,t3B1,F2)
       allocate(S318(N1+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K9
       I3=K0*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S318)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N1,M2,N1,M2,N0,M1,X12,S318, 1.000)
       deallocate(S318)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder451236(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,N0,M1,M2,N3,M2,N3,N1,M2,N0,M1,t3B4,F2)
       allocate(S319(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S319)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,N0,M1,X11,S319,-1.000)
       deallocate(S319)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,M2,N3,N1,M2,N0,M1,t3B1,F2)
       allocate(S320(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S320)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,N0,M1,X11,S320, 1.000)
       deallocate(S320)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,M2,N3,M2,N3,N1,M2,N0,M1,t3B1,F2)
       allocate(S321(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S321)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,N0,M1,X11,S321, 1.000)
       deallocate(S321)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder451236(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,N0,M1,N2,M2,M2,N3,N1,M2,N0,M1,t3B4,F2)
       allocate(S322(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K0*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S322)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,N0,M1,X11,S322,-1.000)
       deallocate(S322)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,N2,M2,M2,N3,N1,M2,N0,M1,t3B1,F2)
       allocate(S323(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K0*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S323)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,N0,M1,X11,S323, 1.000)
       deallocate(S323)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,N2,M2,M2,N3,N1,M2,N0,M1,t3B1,F2)
       allocate(S324(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K0*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S324)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,N0,M1,X11,S324, 1.000)
       deallocate(S324)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder451236(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,N0,M1,M2,N3,M2,N3,N1,M2,N0,M1,t3B4,F2)
       allocate(S325(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S325)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,N0,M1,X10,S325,-1.000)
       deallocate(S325)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,M2,N3,N1,M2,N0,M1,t3B1,F2)
       allocate(S326(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S326)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,N0,M1,X10,S326, 1.000)
       deallocate(S326)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,M2,N3,M2,N3,N1,M2,N0,M1,t3B1,F2)
       allocate(S327(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S327)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,N0,M1,X10,S327, 1.000)
       deallocate(S327)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder451236(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,N0,M1,N2,M2,M2,N3,N1,M2,N0,M1,t3B4,F2)
       allocate(S328(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K0*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S328)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,N0,M1,X10,S328,-1.000)
       deallocate(S328)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,N2,M2,M2,N3,N1,M2,N0,M1,t3B1,F2)
       allocate(S329(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K0*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S329)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,N0,M1,X10,S329, 1.000)
       deallocate(S329)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,N2,M2,M2,N3,N1,M2,N0,M1,t3B1,F2)
       allocate(S330(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K6
       I3=K0*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S330)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,N0,M1,X10,S330, 1.000)
       deallocate(S330)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N0,M1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N0,M1,M2,N3,N1,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(S331(N1+1:M2,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K9
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S331)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N1,M2,N1,M2,M1,N1,X87,S331, 1.000)
       deallocate(S331)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N0,M1,M2,N3,N1,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(S332(N1+1:M2,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K9
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S332)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N1,M2,N1,M2,M1,N1,X87,S332, 1.000)
       deallocate(S332)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,N1,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(S333(N1+1:M2,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K9
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S333)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N1,M2,N1,M2,M1,N1,X87,S333, 1.000)
       deallocate(S333)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,M2,N3,N1,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(S334(N1+1:M2,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K9
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S334)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N1,M2,N1,M2,M1,N1,X87,S334, 1.000)
       deallocate(S334)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N0,M1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N0,M1,N2,M2,N1,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(S335(N1+1:M2,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K9
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S335)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N1,M2,N1,M2,M1,N1,X87,S335, 1.000)
       deallocate(S335)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N0,M1,N2,M2,N1,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(S336(N1+1:M2,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K9
       I3=K0*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S336)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N1,M2,N1,M2,M1,N1,X87,S336, 1.000)
       deallocate(S336)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,N2,M2,N1,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(S337(N1+1:M2,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K9
       I3=K0*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S337)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N1,M2,N1,M2,M1,N1,X87,S337, 1.000)
       deallocate(S337)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,N2,M2,N1,M2,N1,M2,M1,N1,t3B1,F2)
       allocate(S338(N1+1:M2,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K9
       I3=K0*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S338)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N1,M2,N1,M2,M1,N1,X87,S338, 1.000)
       deallocate(S338)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N0,M1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N0,M1,M2,N3,M2,N3,N1,M2,M1,N1,t3B1,F2)
       allocate(S339(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S339)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N1,X90,S339,-1.000)
       deallocate(S339)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N0,M1,M2,N3,M2,N3,N1,M2,M1,N1,t3B1,F2)
       allocate(S340(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S340)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N1,X90,S340,-1.000)
       deallocate(S340)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,M2,N3,N1,M2,M1,N1,t3B1,F2)
       allocate(S341(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S341)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N1,X90,S341,-1.000)
       deallocate(S341)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,M2,N3,M2,N3,N1,M2,M1,N1,t3B1,F2)
       allocate(S342(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S342)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N1,X90,S342,-1.000)
       deallocate(S342)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N0,M1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N0,M1,N2,M2,M2,N3,N1,M2,M1,N1,t3B1,F2)
       allocate(S343(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S343)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N1,X90,S343,-1.000)
       deallocate(S343)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N0,M1,N2,M2,M2,N3,N1,M2,M1,N1,t3B1,F2)
       allocate(S344(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K0*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S344)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N1,X90,S344,-1.000)
       deallocate(S344)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,N2,M2,M2,N3,N1,M2,M1,N1,t3B1,F2)
       allocate(S345(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K0*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S345)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N1,X90,S345,-1.000)
       deallocate(S345)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,N2,M2,M2,N3,N1,M2,M1,N1,t3B1,F2)
       allocate(S346(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K0*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S346)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N1,X90,S346,-1.000)
       deallocate(S346)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N0,M1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N0,M1,M2,N3,M2,N3,N1,M2,M1,N1,t3B1,F2)
       allocate(S347(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S347)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N1,X91,S347, 1.000)
       deallocate(S347)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N0,M1,M2,N3,M2,N3,N1,M2,M1,N1,t3B1,F2)
       allocate(S348(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S348)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N1,X91,S348, 1.000)
       deallocate(S348)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,M2,N3,N1,M2,M1,N1,t3B1,F2)
       allocate(S349(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S349)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N1,X91,S349, 1.000)
       deallocate(S349)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,M2,N3,M2,N3,N1,M2,M1,N1,t3B1,F2)
       allocate(S350(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K6*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S350)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N1,X91,S350, 1.000)
       deallocate(S350)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N0,M1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N0,M1,N2,M2,M2,N3,N1,M2,M1,N1,t3B1,F2)
       allocate(S351(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K0*K5*K5
       call EGEMM(I1,I2,I3,D1,F2,S351)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N1,X91,S351, 1.000)
       deallocate(S351)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N0,M1,N2,M2,M2,N3,N1,M2,M1,N1,t3B1,F2)
       allocate(S352(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K0*K5*K8
       call EGEMM(I1,I2,I3,D1,F2,S352)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N1,X91,S352, 1.000)
       deallocate(S352)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,N2,M2,M2,N3,N1,M2,M1,N1,t3B1,F2)
       allocate(S353(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K0*K7*K5
       call EGEMM(I1,I2,I3,D1,F2,S353)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N1,X91,S353, 1.000)
       deallocate(S353)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder451236(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,N2,M2,M2,N3,N1,M2,M1,N1,t3B1,F2)
       allocate(S354(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K6
       I3=K0*K7*K8
       call EGEMM(I1,I2,I3,D1,F2,S354)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,M2,N3,N1,M2,M1,N1,X91,S354, 1.000)
       deallocate(S354)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(S355(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S355)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X88,S355,-1.000)
       deallocate(S355)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(S356(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S356)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X88,S356,-1.000)
       deallocate(S356)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(S357(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S357)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X88,S357,-1.000)
       deallocate(S357)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(S358(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K6*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S358)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X88,S358,-1.000)
       deallocate(S358)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(S359(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S359)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X88,S359,-1.000)
       deallocate(S359)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(S360(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S360)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X88,S360,-1.000)
       deallocate(S360)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(S361(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S361)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X88,S361,-1.000)
       deallocate(S361)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(S362(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S362)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X88,S362,-1.000)
       deallocate(S362)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(S363(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S363)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X89,S363, 1.000)
       deallocate(S363)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(S364(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S364)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X89,S364, 1.000)
       deallocate(S364)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(S365(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S365)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X89,S365, 1.000)
       deallocate(S365)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(S366(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K6*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S366)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X89,S366, 1.000)
       deallocate(S366)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(S367(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S367)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X89,S367, 1.000)
       deallocate(S367)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(S368(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S368)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X89,S368, 1.000)
       deallocate(S368)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(S369(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S369)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X89,S369, 1.000)
       deallocate(S369)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(S370(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K9*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S370)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X89,S370, 1.000)
       deallocate(S370)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N1,t2A,D2)
       allocate(S371(M2+1:N3,M1+1:N1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S371)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X93(N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N1))
       X93=0.0d0
       call
     & sum3412(N0,M1,M2,N3,M2,N3,M1,N1,X93,S371, 1.000)
       deallocate(S371)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N1,t2A,D2)
       allocate(S372(M2+1:N3,M1+1:N1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S372)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X94(M1+1:N2,M2+1:N3,M2+1:N3,M1+1:N1))
       X94=0.0d0
       call
     & sum3412(M1,N2,M2,N3,M2,N3,M1,N1,X94,S372, 1.000)
       deallocate(S372)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N1,t2A,D2)
       allocate(S373(M2+1:N3,M1+1:N1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S373)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X95(N0+1:M1,N2+1:M2,M2+1:N3,M1+1:N1))
       X95=0.0d0
       call
     & sum3412(N0,M1,N2,M2,M2,N3,M1,N1,X95,S373, 1.000)
       deallocate(S373)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N1,t2A,D2)
       allocate(S374(M2+1:N3,M1+1:N1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S374)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X96(M1+1:N2,N2+1:M2,M2+1:N3,M1+1:N1))
       X96=0.0d0
       call
     & sum3412(M1,N2,N2,M2,M2,N3,M1,N1,X96,S374, 1.000)
       deallocate(S374)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder412356(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,M2,N3,N0,M1,M1,N1,t3B3,F2)
       allocate(S375(M2+1:N3,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K6
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S375)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,N0,M1,M1,N1,X92,S375, 1.000)
       deallocate(S375)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder412356(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,M2,N3,N0,M1,M1,N1,t3B3,F2)
       allocate(S376(M2+1:N3,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K6
       I3=K6*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S376)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,N0,M1,M1,N1,X92,S376, 1.000)
       deallocate(S376)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder413256(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,M2,N3,N0,M1,M1,N1,t3B1,F2)
       allocate(S377(M2+1:N3,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K6
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S377)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,N0,M1,M1,N1,X92,S377,-1.000)
       deallocate(S377)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder413256(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,N1,M2,M2,N3,N0,M1,M1,N1,t3B1,F2)
       allocate(S378(M2+1:N3,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K6
       I3=K9*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S378)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,N0,M1,M1,N1,X92,S378,-1.000)
       deallocate(S378)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder413256(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,N1,M2,M2,N3,N0,M1,M1,N1,t3B1,F2)
       allocate(S379(M2+1:N3,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K6
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S379)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,N0,M1,M1,N1,X92,S379,-1.000)
       deallocate(S379)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder413256(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,N1,M2,M2,N3,N0,M1,M1,N1,t3B1,F2)
       allocate(S380(M2+1:N3,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K6
       I3=K9*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S380)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,N0,M1,M1,N1,X92,S380,-1.000)
       deallocate(S380)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S381(N1+1:M2,M1+1:N1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S381)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X97(N0+1:M1,M2+1:N3,N1+1:M2,M1+1:N1))
       X97=0.0d0
       call
     & sum3412(N0,M1,M2,N3,N1,M2,M1,N1,X97,S381, 1.000)
       deallocate(S381)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S382(N1+1:M2,M1+1:N1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S382)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X98(M1+1:N2,M2+1:N3,N1+1:M2,M1+1:N1))
       X98=0.0d0
       call
     & sum3412(M1,N2,M2,N3,N1,M2,M1,N1,X98,S382, 1.000)
       deallocate(S382)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S383(N1+1:M2,M1+1:N1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S383)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X99(N0+1:M1,N2+1:M2,N1+1:M2,M1+1:N1))
       X99=0.0d0
       call
     & sum3412(N0,M1,N2,M2,N1,M2,M1,N1,X99,S383, 1.000)
       deallocate(S383)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S384(N1+1:M2,M1+1:N1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S384)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X100(M1+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       X100=0.0d0
       call
     & sum3412(M1,N2,N2,M2,N1,M2,M1,N1,X100,S384, 1.000)
       deallocate(S384)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S385(N1+1:M2,M1+1:N1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S385)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X101(N0+1:M1,M2+1:N3,N1+1:M2,M1+1:N1))
       X101=0.0d0
       call
     & sum3412(N0,M1,M2,N3,N1,M2,M1,N1,X101,S385, 1.000)
       deallocate(S385)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S386(N1+1:M2,M1+1:N1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S386)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X102(M1+1:N2,M2+1:N3,N1+1:M2,M1+1:N1))
       X102=0.0d0
       call
     & sum3412(M1,N2,M2,N3,N1,M2,M1,N1,X102,S386, 1.000)
       deallocate(S386)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S387(N1+1:M2,M1+1:N1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S387)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X103(N0+1:M1,N2+1:M2,N1+1:M2,M1+1:N1))
       X103=0.0d0
       call
     & sum3412(N0,M1,N2,M2,N1,M2,M1,N1,X103,S387, 1.000)
       deallocate(S387)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S388(N1+1:M2,M1+1:N1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S388)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X104(M1+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       X104=0.0d0
       call
     & sum3412(M1,N2,N2,M2,N1,M2,M1,N1,X104,S388, 1.000)
       deallocate(S388)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(S389(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S389)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X3,S389,-1.000)
       deallocate(S389)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(S390(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S390)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X3,S390,-1.000)
       deallocate(S390)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(S391(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S391)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X3,S391,-1.000)
       deallocate(S391)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(S392(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K6*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S392)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X3,S392,-1.000)
       deallocate(S392)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(S393(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S393)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X3,S393,-1.000)
       deallocate(S393)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(S394(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K9*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S394)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X3,S394,-1.000)
       deallocate(S394)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(S395(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S395)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X3,S395,-1.000)
       deallocate(S395)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(S396(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K9*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S396)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X3,S396,-1.000)
       deallocate(S396)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(S397(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K6*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S397)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X2,S397,-1.000)
       deallocate(S397)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(S398(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K6*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S398)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X2,S398,-1.000)
       deallocate(S398)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(S399(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S399)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X2,S399,-1.000)
       deallocate(S399)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(S400(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K6*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S400)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X2,S400,-1.000)
       deallocate(S400)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(S401(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S401)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X2,S401,-1.000)
       deallocate(S401)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(S402(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K9*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S402)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X2,S402,-1.000)
       deallocate(S402)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(S403(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S403)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X2,S403,-1.000)
       deallocate(S403)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(S404(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K9*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S404)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X2,S404,-1.000)
       deallocate(S404)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2A,D2)
       allocate(S405(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S405)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X105(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:M1))
       X105=0.0d0
       call
     & sum3412(N0,M1,M2,N3,M2,N3,N0,M1,X105,S405, 1.000)
       deallocate(S405)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2A,D2)
       allocate(S406(M2+1:N3,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S406)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X106(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:M1))
       X106=0.0d0
       call
     & sum3412(M1,N2,M2,N3,M2,N3,N0,M1,X106,S406, 1.000)
       deallocate(S406)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2A,D2)
       allocate(S407(M2+1:N3,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S407)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X107(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:M1))
       X107=0.0d0
       call
     & sum3412(N0,M1,N2,M2,M2,N3,N0,M1,X107,S407, 1.000)
       deallocate(S407)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2A,D2)
       allocate(S408(M2+1:N3,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S408)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X108(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:M1))
       X108=0.0d0
       call
     & sum3412(M1,N2,N2,M2,M2,N3,N0,M1,X108,S408, 1.000)
       deallocate(S408)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder412356(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,M2,N3,M1,N1,M1,N1,t3B3,F2)
       allocate(S409(M2+1:N3,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K6
       I3=K6*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S409)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,M1,N1,M1,N1,X1,S409,-1.000)
       deallocate(S409)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder412356(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,M2,N3,M1,N1,M1,N1,t3B3,F2)
       allocate(S410(M2+1:N3,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K6
       I3=K6*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S410)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,M1,N1,M1,N1,X1,S410,-1.000)
       deallocate(S410)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder413256(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,M2,N3,M1,N1,M1,N1,t3B1,F2)
       allocate(S411(M2+1:N3,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K6
       I3=K9*K6*K5
       call EGEMM(I1,I2,I3,D1,F2,S411)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,M1,N1,M1,N1,X1,S411, 1.000)
       deallocate(S411)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder413256(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,N1,M2,M2,N3,M1,N1,M1,N1,t3B1,F2)
       allocate(S412(M2+1:N3,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K6
       I3=K9*K6*K8
       call EGEMM(I1,I2,I3,D1,F2,S412)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,M1,N1,M1,N1,X1,S412, 1.000)
       deallocate(S412)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder413256(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,N1,M2,M2,N3,M1,N1,M1,N1,t3B1,F2)
       allocate(S413(M2+1:N3,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K6
       I3=K9*K0*K5
       call EGEMM(I1,I2,I3,D1,F2,S413)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,M1,N1,M1,N1,X1,S413, 1.000)
       deallocate(S413)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder413256(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,N1,M2,M2,N3,M1,N1,M1,N1,t3B1,F2)
       allocate(S414(M2+1:N3,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K6
       I3=K9*K0*K8
       call EGEMM(I1,I2,I3,D1,F2,S414)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,M2,N3,M1,N1,M1,N1,X1,S414, 1.000)
       deallocate(S414)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S415(N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S415)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X109(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       X109=0.0d0
       call
     & sum3412(N0,M1,M2,N3,N1,M2,N0,M1,X109,S415, 1.000)
       deallocate(S415)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S416(N1+1:M2,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S416)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X110(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:M1))
       X110=0.0d0
       call
     & sum3412(M1,N2,M2,N3,N1,M2,N0,M1,X110,S416, 1.000)
       deallocate(S416)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S417(N1+1:M2,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S417)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X111(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:M1))
       X111=0.0d0
       call
     & sum3412(N0,M1,N2,M2,N1,M2,N0,M1,X111,S417, 1.000)
       deallocate(S417)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S418(N1+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S418)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X112(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:M1))
       X112=0.0d0
       call
     & sum3412(M1,N2,N2,M2,N1,M2,N0,M1,X112,S418, 1.000)
       deallocate(S418)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S419(N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S419)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X113(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       X113=0.0d0
       call
     & sum3412(N0,M1,M2,N3,N1,M2,N0,M1,X113,S419, 1.000)
       deallocate(S419)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S420(N1+1:M2,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S420)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X114(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:M1))
       X114=0.0d0
       call
     & sum3412(M1,N2,M2,N3,N1,M2,N0,M1,X114,S420, 1.000)
       deallocate(S420)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S421(N1+1:M2,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S421)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X115(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:M1))
       X115=0.0d0
       call
     & sum3412(N0,M1,N2,M2,N1,M2,N0,M1,X115,S421, 1.000)
       deallocate(S421)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S422(N1+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S422)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X116(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:M1))
       X116=0.0d0
       call
     & sum3412(M1,N2,N2,M2,N1,M2,N0,M1,X116,S422, 1.000)
       deallocate(S422)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,M1,N1,t2B,D2)
       allocate(S423(M2+1:N3,M1+1:N1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S423)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,M2,N3,M1,N1,X28,S423,-1.000)
       deallocate(S423)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,M1,N1,t2B,D2)
       allocate(S424(M2+1:N3,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S424)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,M2,N3,M2,N3,M1,N1,X29,S424,-1.000)
       deallocate(S424)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,M1,N1,t2B,D2)
       allocate(S425(M2+1:N3,M1+1:N1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S425)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N1,M2,M2,N3,M1,N1,X30,S425,-1.000)
       deallocate(S425)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,M1,N1,t2B,D2)
       allocate(S426(M2+1:N3,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S426)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,N1,M2,M2,N3,M1,N1,X31,S426,-1.000)
       deallocate(S426)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S427(N1+1:M2,M1+1:N1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S427)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,N1,M2,M1,N1,X32,S427,-1.000)
       deallocate(S427)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S428(N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S428)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,M2,N3,N1,M2,M1,N1,X33,S428,-1.000)
       deallocate(S428)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S429(N1+1:M2,M1+1:N1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S429)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N1,M2,N1,M2,M1,N1,X34,S429,-1.000)
       deallocate(S429)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S430(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S430)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,N1,M2,N1,M2,M1,N1,X35,S430,-1.000)
       deallocate(S430)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S431(N1+1:M2,M1+1:N1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S431)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,N1,M2,M1,N1,X36,S431,-1.000)
       deallocate(S431)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,M1,M2,N3,N1,M2,M1,N1,X36,VAHPHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z36(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X36,F2,Z36)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z36,-1.000)
       call
     & sum124635(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z36, 1.000)
       deallocate(Z36)
       deallocate(X36)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S432(N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S432)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,M2,N3,N1,M2,M1,N1,X37,S432,-1.000)
       deallocate(S432)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,M2,N3,N1,M2,M1,N1,X37,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder512346(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z37(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X37,F2,Z37)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z37, 1.000)
       call
     & sum124635(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z37,-1.000)
       deallocate(Z37)
       deallocate(X37)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S433(N1+1:M2,M1+1:N1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S433)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N1,M2,N1,M2,M1,N1,X38,S433,-1.000)
       deallocate(S433)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,M1,N1,M2,N1,M2,M1,N1,X38,VAHPHP, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder421356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N1,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z38(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X38,F2,Z38)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z38, 1.000)
       call
     & sum124635(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z38,-1.000)
       deallocate(Z38)
       deallocate(X38)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S434(N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S434)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,N1,M2,N1,M2,M1,N1,X39,S434,-1.000)
       deallocate(S434)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,N1,M2,N1,M2,M1,N1,X39,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder521346(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z39(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X39,F2,Z39)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z39,-1.000)
       call
     & sum124635(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z39, 1.000)
       deallocate(Z39)
       deallocate(X39)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,M1,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,M1,N1,t2B,D2)
       allocate(Q41(M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7
       I3=K3*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q41)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N0,M1,M1,N1,X13,Q41, 1.000)
       deallocate(Q41)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,M1,N1,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,M1,N1,t2B,D2)
       allocate(Q42(M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7
       I3=K3*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q42)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M1,N1,M1,N1,X14,Q42, 1.000)
       deallocate(Q42)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S435(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S435)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,M2,N3,N0,M1,X40,S435,-1.000)
       deallocate(S435)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S436(M2+1:N3,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S436)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,M2,N3,M2,N3,N0,M1,X41,S436,-1.000)
       deallocate(S436)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S437(M2+1:N3,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S437)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N1,M2,M2,N3,N0,M1,X42,S437,-1.000)
       deallocate(S437)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S438(M2+1:N3,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S438)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,N1,M2,M2,N3,N0,M1,X43,S438,-1.000)
       deallocate(S438)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S439(N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S439)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,N1,M2,N0,M1,X44,S439,-1.000)
       deallocate(S439)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S440(N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S440)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,M2,N3,N1,M2,N0,M1,X45,S440,-1.000)
       deallocate(S440)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S441(N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S441)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N1,M2,N1,M2,N0,M1,X46,S441,-1.000)
       deallocate(S441)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S442(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S442)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,N1,M2,N1,M2,N0,M1,X47,S442,-1.000)
       deallocate(S442)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S443(N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S443)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,N1,M2,N0,M1,X48,S443,-1.000)
       deallocate(S443)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,M1,X48,VAHPHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(Z48(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X48,F2,Z48)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z48,-1.000)
       deallocate(Z48)
       deallocate(X48)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S444(N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S444)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,M2,N3,N1,M2,N0,M1,X49,S444,-1.000)
       deallocate(S444)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,M1,X49,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(Z49(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X49,F2,Z49)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z49,-1.000)
       deallocate(Z49)
       deallocate(X49)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S445(N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S445)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N1,M2,N1,M2,N0,M1,X50,S445,-1.000)
       deallocate(S445)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,M1,N1,M2,N1,M2,N0,M1,X50,VAHPHP, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder421356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N1,M2,M2,N3,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(Z50(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X50,F2,Z50)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z50, 1.000)
       deallocate(Z50)
       deallocate(X50)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S446(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S446)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,N1,M2,N1,M2,N0,M1,X51,S446,-1.000)
       deallocate(S446)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,N1,M2,N1,M2,N0,M1,X51,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder421356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,M2,N3,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(Z51(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X51,F2,Z51)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z51, 1.000)
       deallocate(Z51)
       deallocate(X51)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,M1,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,M1,t2B,D2)
       allocate(Q43(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K3*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q43)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N0,M1,N0,M1,X15,Q43, 1.000)
       deallocate(Q43)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,M1,N1,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,M1,t2B,D2)
       allocate(Q44(N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5
       I3=K3*K4*K2
       call EGEMM(I1,I2,I3,D1,D2,Q44)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M1,N1,N0,M1,X16,Q44, 1.000)
       deallocate(Q44)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,M2,N3,t2B,D2)
       allocate(Q45(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K4*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q45)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M2,N3,M2,N3,X17,Q45,-1.000)
       deallocate(Q45)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,M2,N3,t2B,D2)
       allocate(Q46(M2+1:N3,N1+1:M2))
       I1=K9
       I2=K6
       I3=K4*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q46)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N1,M2,M2,N3,X18,Q46,-1.000)
       deallocate(Q46)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,M2,t2B,D2)
       allocate(Q47(N1+1:M2,M2+1:N3))
       I1=K6
       I2=K9
       I3=K4*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q47)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M2,N3,N1,M2,X19,Q47,-1.000)
       deallocate(Q47)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,M2,t2B,D2)
       allocate(Q48(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K4*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q48)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N1,M2,N1,M2,X20,Q48,-1.000)
       deallocate(Q48)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,M2,t2B,D2)
       allocate(Q49(N1+1:M2,M2+1:N3))
       I1=K6
       I2=K9
       I3=K4*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q49)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M2,N3,N1,M2,X21,Q49,-1.000)
       deallocate(Q49)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,M2,t2B,D2)
       allocate(Q50(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K4*K1*K2
       call EGEMM(I1,I2,I3,D1,D2,Q50)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N1,M2,N1,M2,X22,Q50,-1.000)
       deallocate(Q50)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,M1,N1,t2B,D2)
       allocate(S447(M2+1:N3,M1+1:N1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S447)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X60(N0+1:M1,M2+1:N3,M2+1:N3,M1+1:N1))
       X60=0.0d0
       call
     & sum3412(N0,M1,M2,N3,M2,N3,M1,N1,X60,S447, 1.000)
       deallocate(S447)
C
       call sumx1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,M2,N3,M2,N3,M1,N1,X60,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z60(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K5*K9*K9
       I3=K6*K5
       call EGEMM(I1,I2,I3,X60,F2,Z60)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z60, 1.000)
       call
     & sum234615(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z60,-1.000)
       deallocate(Z60)
       deallocate(X60)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,M1,N1,t2B,D2)
       allocate(S448(M2+1:N3,M1+1:N1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S448)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X61(M1+1:N2,M2+1:N3,M2+1:N3,M1+1:N1))
       X61=0.0d0
       call
     & sum3412(M1,N2,M2,N3,M2,N3,M1,N1,X61,S448, 1.000)
       deallocate(S448)
C
       call sumx1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N2,M2,N3,M2,N3,M1,N1,X61,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z61(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K5*K9*K9
       I3=K6*K8
       call EGEMM(I1,I2,I3,X61,F2,Z61)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z61, 1.000)
       call
     & sum234615(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z61,-1.000)
       deallocate(Z61)
       deallocate(X61)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,M1,N1,t2B,D2)
       allocate(S449(M2+1:N3,M1+1:N1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S449)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X62(N0+1:M1,N2+1:M2,M2+1:N3,M1+1:N1))
       X62=0.0d0
       call
     & sum3412(N0,M1,N2,M2,M2,N3,M1,N1,X62,S449, 1.000)
       deallocate(S449)
C
       call sumx1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,N2,M2,M2,N3,M1,N1,X62,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z62(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K5*K9*K9
       I3=K0*K5
       call EGEMM(I1,I2,I3,X62,F2,Z62)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z62, 1.000)
       call
     & sum234615(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z62,-1.000)
       deallocate(Z62)
       deallocate(X62)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,M1,N1,t2B,D2)
       allocate(S450(M2+1:N3,M1+1:N1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S450)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X63(M1+1:N2,N2+1:M2,M2+1:N3,M1+1:N1))
       X63=0.0d0
       call
     & sum3412(M1,N2,N2,M2,M2,N3,M1,N1,X63,S450, 1.000)
       deallocate(S450)
C
       call sumx1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N2,N2,M2,M2,N3,M1,N1,X63,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z63(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K5*K9*K9
       I3=K0*K8
       call EGEMM(I1,I2,I3,X63,F2,Z63)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z63, 1.000)
       call
     & sum234615(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z63,-1.000)
       deallocate(Z63)
       deallocate(X63)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S451(N1+1:M2,M1+1:N1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S451)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X64(N0+1:M1,M2+1:N3,N1+1:M2,M1+1:N1))
       X64=0.0d0
       call
     & sum3412(N0,M1,M2,N3,N1,M2,M1,N1,X64,S451, 1.000)
       deallocate(S451)
C
       call sumx1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,M2,N3,N1,M2,M1,N1,X64,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z64(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X64,F2,Z64)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z64,-1.000)
       call
     & sum134625(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z64, 1.000)
       deallocate(Z64)
       deallocate(X64)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S452(N1+1:M2,M1+1:N1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S452)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X65(M1+1:N2,M2+1:N3,N1+1:M2,M1+1:N1))
       X65=0.0d0
       call
     & sum3412(M1,N2,M2,N3,N1,M2,M1,N1,X65,S452, 1.000)
       deallocate(S452)
C
       call sumx1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N2,M2,N3,N1,M2,M1,N1,X65,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z65(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X65,F2,Z65)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z65,-1.000)
       call
     & sum134625(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z65, 1.000)
       deallocate(Z65)
       deallocate(X65)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S453(N1+1:M2,M1+1:N1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S453)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X66(N0+1:M1,N2+1:M2,N1+1:M2,M1+1:N1))
       X66=0.0d0
       call
     & sum3412(N0,M1,N2,M2,N1,M2,M1,N1,X66,S453, 1.000)
       deallocate(S453)
C
       call sumx1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,N2,M2,N1,M2,M1,N1,X66,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z66(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,X66,F2,Z66)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z66,-1.000)
       call
     & sum134625(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z66, 1.000)
       deallocate(Z66)
       deallocate(X66)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S454(N1+1:M2,M1+1:N1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S454)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X67(M1+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       X67=0.0d0
       call
     & sum3412(M1,N2,N2,M2,N1,M2,M1,N1,X67,S454, 1.000)
       deallocate(S454)
C
       call sumx1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N2,N2,M2,N1,M2,M1,N1,X67,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z67(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X67,F2,Z67)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z67,-1.000)
       call
     & sum134625(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z67, 1.000)
       deallocate(Z67)
       deallocate(X67)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S455(N1+1:M2,M1+1:N1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S455)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,N1,M2,M1,N1,X101,S455, 1.000)
       deallocate(S455)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S456(N1+1:M2,M1+1:N1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S456)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,M2,N3,N1,M2,M1,N1,X102,S456, 1.000)
       deallocate(S456)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S457(N1+1:M2,M1+1:N1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S457)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N2,M2,N1,M2,M1,N1,X103,S457, 1.000)
       deallocate(S457)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S458(N1+1:M2,M1+1:N1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S458)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,N2,M2,N1,M2,M1,N1,X104,S458, 1.000)
       deallocate(S458)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S459(M2+1:N3,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S459)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X68(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:M1))
       X68=0.0d0
       call
     & sum3412(N0,M1,M2,N3,M2,N3,N0,M1,X68,S459, 1.000)
       deallocate(S459)
C
       call sumx1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,M1,X68,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z72(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K7*K9*K9
       I3=K6*K5
       call EGEMM(I1,I2,I3,X68,F2,Z72)
       deallocate(F2)
C
       call
     & sum235614(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z72, 1.000)
       deallocate(Z72)
       deallocate(X68)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S460(M2+1:N3,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S460)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X69(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:M1))
       X69=0.0d0
       call
     & sum3412(M1,N2,M2,N3,M2,N3,N0,M1,X69,S460, 1.000)
       deallocate(S460)
C
       call sumx1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N2,M2,N3,M2,N3,N0,M1,X69,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z73(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K7*K9*K9
       I3=K6*K8
       call EGEMM(I1,I2,I3,X69,F2,Z73)
       deallocate(F2)
C
       call
     & sum235614(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z73, 1.000)
       deallocate(Z73)
       deallocate(X69)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S461(M2+1:N3,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S461)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X70(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:M1))
       X70=0.0d0
       call
     & sum3412(N0,M1,N2,M2,M2,N3,N0,M1,X70,S461, 1.000)
       deallocate(S461)
C
       call sumx1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,N2,M2,M2,N3,N0,M1,X70,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z74(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K7*K9*K9
       I3=K0*K5
       call EGEMM(I1,I2,I3,X70,F2,Z74)
       deallocate(F2)
C
       call
     & sum235614(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z74, 1.000)
       deallocate(Z74)
       deallocate(X70)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S462(M2+1:N3,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S462)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X71(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:M1))
       X71=0.0d0
       call
     & sum3412(M1,N2,N2,M2,M2,N3,N0,M1,X71,S462, 1.000)
       deallocate(S462)
C
       call sumx1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N2,N2,M2,M2,N3,N0,M1,X71,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z75(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K7*K9*K9
       I3=K0*K8
       call EGEMM(I1,I2,I3,X71,F2,Z75)
       deallocate(F2)
C
       call
     & sum235614(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z75, 1.000)
       deallocate(Z75)
       deallocate(X71)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S463(N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S463)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X72(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       X72=0.0d0
       call
     & sum3412(N0,M1,M2,N3,N1,M2,N0,M1,X72,S463, 1.000)
       deallocate(S463)
C
       call sumx1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,M1,X72,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z76(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X72,F2,Z76)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z76,-1.000)
       deallocate(Z76)
       deallocate(X72)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S464(N1+1:M2,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S464)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X73(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:M1))
       X73=0.0d0
       call
     & sum3412(M1,N2,M2,N3,N1,M2,N0,M1,X73,S464, 1.000)
       deallocate(S464)
C
       call sumx1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N2,M2,N3,N1,M2,N0,M1,X73,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z77(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X73,F2,Z77)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z77,-1.000)
       deallocate(Z77)
       deallocate(X73)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S465(N1+1:M2,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S465)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X74(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:M1))
       X74=0.0d0
       call
     & sum3412(N0,M1,N2,M2,N1,M2,N0,M1,X74,S465, 1.000)
       deallocate(S465)
C
       call sumx1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,N2,M2,N1,M2,N0,M1,X74,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z78(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,X74,F2,Z78)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z78,-1.000)
       deallocate(Z78)
       deallocate(X74)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S466(N1+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S466)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X75(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:M1))
       X75=0.0d0
       call
     & sum3412(M1,N2,N2,M2,N1,M2,N0,M1,X75,S466, 1.000)
       deallocate(S466)
C
       call sumx1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N2,N2,M2,N1,M2,N0,M1,X75,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z79(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X75,F2,Z79)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z79,-1.000)
       deallocate(Z79)
       deallocate(X75)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S467(N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S467)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,N1,M2,N0,M1,X113,S467, 1.000)
       deallocate(S467)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S468(N1+1:M2,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S468)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,M2,N3,N1,M2,N0,M1,X114,S468, 1.000)
       deallocate(S468)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S469(N1+1:M2,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S469)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N2,M2,N1,M2,N0,M1,X115,S469, 1.000)
       deallocate(S469)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S470(N1+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S470)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,N2,M2,N1,M2,N0,M1,X116,S470, 1.000)
       deallocate(S470)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,M1+1:N1))
       call reorder1342(N1,N3,N0,N1,N0,N1,N0,N1,
     & N1,N3,N0,N1,N0,N1,M1,N1,VAHHHP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S471(M1+1:N1,N0+1:N1,N0+1:N1,M1+1:N1))
       I1=K7*K1*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S471)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N1,M1+1:N1,M1+1:N1))
       call reorder2314(M1,N1,N0,N1,N0,N1,M1,N1,
     & N0,N1,N0,N1,M1,N1,M1,N1,S471,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S473(N1+1:M2,N0+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S473)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N1,M2,M1,N1,M1,N1,X77,S473,-1.000)
       deallocate(S473)
C
       allocate(D1(N0+1:N1,N0+1:N1,M1+1:N1,M1+1:N1))
       call reorder3214(M1,N1,N0,N1,N0,N1,M1,N1,
     & N0,N1,N0,N1,M1,N1,M1,N1,S471,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S474(N1+1:M2,N0+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S474)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N1,M2,M1,N1,M1,N1,X78,S474, 1.000)
       deallocate(S474)
C
       allocate(D1(N0+1:N1,N0+1:N1,M1+1:N1,M1+1:N1))
       call reorder2314(M1,N1,N0,N1,N0,N1,M1,N1,
     & N0,N1,N0,N1,M1,N1,M1,N1,S471,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S472(M2+1:N3,N0+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S472)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,M2,N3,M1,N1,M1,N1,X76,S472,-1.000)
       deallocate(S472)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,M1+1:N1))
       call reorder1342(N1,N3,N0,N1,N0,N1,N0,N1,
     & N1,N3,N0,N1,N0,N1,M1,N1,VAHHHP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S475(N0+1:M1,N0+1:N1,N0+1:N1,M1+1:N1))
       I1=K7*K1*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S475)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder2314(N0,M1,N0,N1,N0,N1,M1,N1,
     & N0,N1,N0,N1,N0,M1,M1,N1,S475,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S477(N1+1:M2,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S477)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N1,M2,N0,M1,M1,N1,X5,S477,-1.000)
       deallocate(S477)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder3214(N0,M1,N0,N1,N0,N1,M1,N1,
     & N0,N1,N0,N1,N0,M1,M1,N1,S475,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S478(N1+1:M2,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S478)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N1,M2,N0,M1,M1,N1,X6,S478, 1.000)
       deallocate(S478)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder2314(N0,M1,N0,N1,N0,N1,M1,N1,
     & N0,N1,N0,N1,N0,M1,M1,N1,S475,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S476(M2+1:N3,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S476)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,M2,N3,N0,M1,M1,N1,X4,S476,-1.000)
       deallocate(S476)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M1+1:N1))
       call reorder4312(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,M1,N1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S479(N1+1:M2,N0+1:N1,N1+1:N3,M1+1:N1))
       I1=K7*K3*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S479)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder2314(N1,M2,N0,N1,N1,N3,M1,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,S479,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S483(N1+1:M2,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S483)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,N3,N1,M2,N1,M2,M1,N1,X9,S483,-1.000)
       deallocate(S483)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder2314(N1,M2,N0,N1,N1,N3,M1,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,S479,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S480(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S480)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,N1,M2,M1,N1,X7,S480, 1.000)
       deallocate(S480)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M1+1:N1))
       call reorder4312(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,M1,N1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S481(N1+1:M2,N0+1:N1,N1+1:N3,M1+1:N1))
       I1=K7*K3*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S481)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder2314(N1,M2,N0,N1,N1,N3,M1,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,S481,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S482(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S482)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,N1,M2,M1,N1,X8,S482, 1.000)
       deallocate(S482)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder2413(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N0,N1,N1,N3,M2,N3,VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S484(M1+1:N1,N0+1:N1,N1+1:N3,M2+1:N3))
       I1=K6*K3*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S484)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N1+1:N3,N0+1:N1,M2+1:N3,M1+1:N1))
       call reorder3241(M1,N1,N0,N1,N1,N3,M2,N3,
     & N1,N3,N0,N1,M2,N3,M1,N1,S484,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S494(N0+1:M1,N0+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S494)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N1,M2,N3,N0,M1,M1,N1,X4,S494, 1.000)
       deallocate(S494)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder2341(M1,N1,N0,N1,N1,N3,M2,N3,
     & N0,N1,N1,N3,M2,N3,M1,N1,S484,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S497(N1+1:M2,N1+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S497)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,N3,M2,N3,N1,M2,M1,N1,X7,S497,-1.000)
       deallocate(S497)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder2341(M1,N1,N0,N1,N1,N3,M2,N3,
     & N0,N1,N1,N3,M2,N3,M1,N1,S484,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S498(N1+1:M2,N1+1:N3,M2+1:N3,M1+1:N1))
       I1=K7*K6*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S498)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,N3,M2,N3,N1,M2,M1,N1,X8,S498,-1.000)
       deallocate(S498)
C
       allocate(D1(N1+1:N3,N0+1:N1,M2+1:N3,M1+1:N1))
       call reorder3241(M1,N1,N0,N1,N1,N3,M2,N3,
     & N1,N3,N0,N1,M2,N3,M1,N1,S484,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S485(M1+1:N1,N0+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S485)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N1,M2,N3,M1,N1,M1,N1,X1,S485, 1.000)
       deallocate(S485)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N0+1:M1))
       call reorder1342(N1,N3,N0,N1,N0,N1,N0,N1,
     & N1,N3,N0,N1,N0,N1,N0,M1,VAHHHP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S490(M1+1:N1,N0+1:N1,N0+1:N1,N0+1:M1))
       I1=K5*K1*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S490)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder3241(M1,N1,N0,N1,N0,N1,N0,M1,
     & N0,N1,N0,N1,N0,M1,M1,N1,S490,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S493(N1+1:M2,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S493)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N1,M2,N0,M1,M1,N1,X6,S493,-1.000)
       deallocate(S493)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder2341(M1,N1,N0,N1,N0,N1,N0,M1,
     & N0,N1,N0,N1,N0,M1,M1,N1,S490,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S491(M2+1:N3,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S491)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,M2,N3,N0,M1,M1,N1,X4,S491, 1.000)
       deallocate(S491)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder2341(M1,N1,N0,N1,N0,N1,N0,M1,
     & N0,N1,N0,N1,N0,M1,M1,N1,S490,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S492(N1+1:M2,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S492)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N1,M2,N0,M1,M1,N1,X5,S492, 1.000)
       deallocate(S492)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder2413(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N0,N1,N1,N3,N1,M2,VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S486(M1+1:N1,N0+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S486)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:M2,M1+1:N1))
       call reorder3241(M1,N1,N0,N1,N1,N3,N1,M2,
     & N1,N3,N0,N1,N1,M2,M1,N1,S486,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S487(M1+1:N1,N0+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S487)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N1,N1,M2,M1,N1,M1,N1,X2,S487, 1.000)
       deallocate(S487)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder2341(M1,N1,N0,N1,N1,N3,N1,M2,
     & N0,N1,N1,N3,N1,M2,M1,N1,S486,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S499(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S499)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,N1,M2,M1,N1,X7,S499, 1.000)
       deallocate(S499)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder2341(M1,N1,N0,N1,N1,N3,N1,M2,
     & N0,N1,N1,N3,N1,M2,M1,N1,S486,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S501(N1+1:M2,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S501)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,N3,N1,M2,N1,M2,M1,N1,X9,S501,-1.000)
       deallocate(S501)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:M2,M1+1:N1))
       call reorder3241(M1,N1,N0,N1,N1,N3,N1,M2,
     & N1,N3,N0,N1,N1,M2,M1,N1,S486,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S495(N0+1:M1,N0+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S495)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N1,N1,M2,N0,M1,M1,N1,X5,S495, 1.000)
       deallocate(S495)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder2413(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N0,N1,N1,N3,N1,M2,VAHPPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S488(M1+1:N1,N0+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S488)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder2341(M1,N1,N0,N1,N1,N3,N1,M2,
     & N0,N1,N1,N3,N1,M2,M1,N1,S488,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S500(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S500)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,N1,M2,M1,N1,X8,S500, 1.000)
       deallocate(S500)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:M2,M1+1:N1))
       call reorder3241(M1,N1,N0,N1,N1,N3,N1,M2,
     & N1,N3,N0,N1,N1,M2,M1,N1,S488,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S496(N0+1:M1,N0+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S496)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N1,N1,M2,N0,M1,M1,N1,X6,S496, 1.000)
       deallocate(S496)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder2341(M1,N1,N0,N1,N1,N3,N1,M2,
     & N0,N1,N1,N3,N1,M2,M1,N1,S488,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S502(N1+1:M2,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S502)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,N1,M2,N1,M2,M1,N1,X9,S502, 1.000)
       deallocate(S502)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:M2,M1+1:N1))
       call reorder3241(M1,N1,N0,N1,N1,N3,N1,M2,
     & N1,N3,N0,N1,N1,M2,M1,N1,S488,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S489(M1+1:N1,N0+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S489)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N1,N1,M2,M1,N1,M1,N1,X3,S489, 1.000)
       deallocate(S489)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:M1))
       call reorder4312(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N0,M1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S503(N1+1:M2,N0+1:N1,N1+1:N3,N0+1:M1))
       I1=K5*K3*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S503)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder2314(N1,M2,N0,N1,N1,N3,N0,M1,
     & N0,N1,N1,N3,N1,M2,N0,M1,S503,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S507(N1+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S507)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,N3,N1,M2,N1,M2,N0,M1,X12,S507,-1.000)
       deallocate(S507)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder2314(N1,M2,N0,N1,N1,N3,N0,M1,
     & N0,N1,N1,N3,N1,M2,N0,M1,S503,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S504(M2+1:N3,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S504)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,N1,M2,N0,M1,X10,S504, 1.000)
       deallocate(S504)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:M1))
       call reorder4312(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N0,M1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S505(N1+1:M2,N0+1:N1,N1+1:N3,N0+1:M1))
       I1=K5*K3*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S505)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder2314(N1,M2,N0,N1,N1,N3,N0,M1,
     & N0,N1,N1,N3,N1,M2,N0,M1,S505,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S506(M2+1:N3,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S506)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,N1,M2,N0,M1,X11,S506, 1.000)
       deallocate(S506)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder1423(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N0,N1,N1,N3,M2,N3,VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S508(N0+1:M1,N0+1:N1,N1+1:N3,M2+1:N3))
       I1=K6*K3*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S508)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N1,N1,N3,M2,N3,
     & N0,N1,N1,N3,M2,N3,N0,M1,S508,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S510(N1+1:M2,N1+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S510)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,N3,M2,N3,N1,M2,N0,M1,X11,S510, 1.000)
       deallocate(S510)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N1,N1,N3,M2,N3,
     & N0,N1,N1,N3,M2,N3,N0,M1,S508,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S509(N1+1:M2,N1+1:N3,M2+1:N3,N0+1:M1))
       I1=K5*K6*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S509)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,N3,M2,N3,N1,M2,N0,M1,X10,S509, 1.000)
       deallocate(S509)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder1423(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N0,N1,N1,N3,N1,M2,VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S511(N0+1:M1,N0+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S511)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N1,N1,N3,N1,M2,
     & N0,N1,N1,N3,N1,M2,N0,M1,S511,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S515(N1+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S515)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,N3,N1,M2,N1,M2,N0,M1,X12,S515, 1.000)
       deallocate(S515)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N1,N1,N3,N1,M2,
     & N0,N1,N1,N3,N1,M2,N0,M1,S511,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S512(M2+1:N3,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S512)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,N1,M2,N0,M1,X10,S512,-1.000)
       deallocate(S512)
C
       allocate(D1(N1+1:N3,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder1423(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N0,N1,N1,N3,N1,M2,VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S513(N0+1:M1,N0+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S513)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N1,N1,N3,N1,M2,
     & N0,N1,N1,N3,N1,M2,N0,M1,S513,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S516(N1+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S516)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,N1,M2,N1,M2,N0,M1,X12,S516,-1.000)
       deallocate(S516)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N1,N1,N3,N1,M2,
     & N0,N1,N1,N3,N1,M2,N0,M1,S513,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S514(M2+1:N3,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S514)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,N1,M2,N0,M1,X11,S514,-1.000)
       deallocate(S514)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,N1+1:N3))
       call reorder2341(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,M1,M1,N1,N1,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S517(M1+1:N1,N0+1:M1,M1+1:N1,N1+1:N3))
       I1=K3*K7*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S517)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,M1+1:N1))
       call reorder4231(M1,N1,N0,M1,M1,N1,N1,N3,
     & N1,N3,N0,M1,M1,N1,M1,N1,S517,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S523(N0+1:M1,N0+1:M1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S523)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M1,N1,N0,M1,M1,N1,X26,S523, 1.000)
       deallocate(S523)
C
       call sumx3412(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,M1,M1,N1,N0,M1,M1,N1,X26,VAHHHH, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,N1,M2,N1,M2,M1,N1,t3A,F2)
       allocate(Z26(M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5
       I2=K7*K9*K9*K6
       I3=K7*K5
       call EGEMM(I1,I2,I3,X26,F2,Z26)
       deallocate(F2)
C
       call
     & sum123546(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z26,-1.000)
       call
     & sum123645(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z26, 1.000)
       deallocate(Z26)
       deallocate(X26)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,M1+1:N1))
       call reorder4231(M1,N1,N0,M1,M1,N1,N1,N3,
     & N1,N3,N0,M1,M1,N1,M1,N1,S517,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S518(M1+1:N1,N0+1:M1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S518)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M1,N1,M1,N1,M1,N1,X23,S518, 1.000)
       deallocate(S518)
C
       call sumx3412(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,M1,M1,N1,M1,N1,M1,N1,X23,VAHHHH, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder461235(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,N1,M2,N1,M2,N0,M1,t3A,F2)
       allocate(Z23(M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M1+1:N1))
       I1=K7*K7
       I2=K5*K9*K9*K6
       I3=K7*K5
       call EGEMM(I1,I2,I3,X23,F2,Z23)
       deallocate(F2)
C
       V3A=V3A-Z23
       deallocate(Z23)
       deallocate(X23)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:M1,N1+1:N3))
       call reorder2341(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,M1,N0,M1,N1,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S521(M1+1:N1,N0+1:M1,N0+1:M1,N1+1:N3))
       I1=K3*K5*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S521)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:M1,M1+1:N1))
       call reorder4231(M1,N1,N0,M1,N0,M1,N1,N3,
     & N1,N3,N0,M1,N0,M1,M1,N1,S521,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S522(N0+1:M1,N0+1:M1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S522)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N0,M1,N0,M1,M1,N1,X25,S522, 1.000)
       deallocate(S522)
C
       call sumx3412(N0,N1,N0,N1,N0,N1,N0,N1,
     & N0,M1,N0,M1,N0,M1,M1,N1,X25,VAHHHH, 1.000)
C
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N0,M1,M2,N3,N1,M2,N1,M2,M1,N1,t3A,F2)
       allocate(Z25(M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5
       I2=K7*K9*K9*K6
       I3=K5*K5
       call EGEMM(I1,I2,I3,X25,F2,Z25)
       deallocate(F2)
C
       call
     & sum123546(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z25,-0.500)
       call
     & sum123645(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z25, 0.500)
       deallocate(Z25)
       deallocate(X25)
C
       allocate(D1(N1+1:N3,M1+1:N1,M1+1:N1,N1+1:N3))
       call reorder2341(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,M1,N1,M1,N1,N1,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S519(M1+1:N1,M1+1:N1,M1+1:N1,N1+1:N3))
       I1=K3*K7*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S519)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N1+1:N3,M1+1:N1,M1+1:N1,M1+1:N1))
       call reorder4231(M1,N1,M1,N1,M1,N1,N1,N3,
     & N1,N3,M1,N1,M1,N1,M1,N1,S519,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S524(N0+1:M1,M1+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S524)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,M1,N1,N0,M1,M1,N1,X27,S524, 1.000)
       deallocate(S524)
C
       call sumx3412(N0,N1,N0,N1,N0,N1,N0,N1,
     & M1,N1,M1,N1,N0,M1,M1,N1,X27,VAHHHH, 1.000)
C
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder451236(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M1,N1,M2,N3,N1,M2,N1,M2,M1,N1,t3A,F2)
       allocate(Z27(M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5
       I2=K7*K9*K9*K6
       I3=K7*K7
       call EGEMM(I1,I2,I3,X27,F2,Z27)
       deallocate(F2)
C
       call
     & sum123546(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z27,-0.500)
       call
     & sum123645(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z27, 0.500)
       deallocate(Z27)
       deallocate(X27)
C
       allocate(D1(N1+1:N3,M1+1:N1,M1+1:N1,M1+1:N1))
       call reorder4231(M1,N1,M1,N1,M1,N1,N1,N3,
     & N1,N3,M1,N1,M1,N1,M1,N1,S519,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S520(M1+1:N1,M1+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S520)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,M1,N1,M1,N1,M1,N1,X24,S520, 1.000)
       deallocate(S520)
C
       call sumx3412(N0,N1,N0,N1,N0,N1,N0,N1,
     & M1,N1,M1,N1,M1,N1,M1,N1,X24,VAHHHH, 1.000)
C
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder561234(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M1,N1,M2,N3,N1,M2,N1,M2,N0,M1,t3A,F2)
       allocate(Z24(M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M1+1:N1))
       I1=K7*K7
       I2=K5*K9*K9*K6
       I3=K7*K7
       call EGEMM(I1,I2,I3,X24,F2,Z24)
       deallocate(F2)
C
       V3A=V3A+0.500*Z24
       deallocate(Z24)
       deallocate(X24)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:M1,M2+1:N3))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,N1,N0,M1,M2,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S525(M1+1:N1,N0+1:N1,N0+1:M1,M2+1:N3))
       I1=K6*K5*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S525)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,M1+1:N1))
       call reorder2341(M1,N1,N0,N1,N0,M1,M2,N3,
     & N0,N1,N0,M1,M2,N3,M1,N1,S525,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S533(N1+1:M2,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S533)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,N1,M2,M1,N1,X32,S533, 1.000)
       deallocate(S533)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,M1,M2,N3,N1,M2,M1,N1,X32,VAHPHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z32(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X32,F2,Z32)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z32, 1.000)
       call
     & sum134625(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z32,-1.000)
       deallocate(Z32)
       deallocate(X32)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,M1+1:N1))
       call reorder2341(M1,N1,N0,N1,N0,M1,M2,N3,
     & N0,N1,N0,M1,M2,N3,M1,N1,S525,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S526(M2+1:N3,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S526)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,M2,N3,M1,N1,X28,S526, 1.000)
       deallocate(S526)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,M1,M2,N3,M2,N3,M1,N1,X28,VAHPHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z28(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K5*K9*K9
       I3=K6*K5
       call EGEMM(I1,I2,I3,X28,F2,Z28)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z28,-1.000)
       call
     & sum234615(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z28, 1.000)
       deallocate(Z28)
       deallocate(X28)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:M1,N1+1:M2))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,N1,N0,M1,N1,M2,VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S529(M1+1:N1,N0+1:N1,N0+1:M1,N1+1:M2))
       I1=K9*K5*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S529)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,M1+1:N1))
       call reorder2341(M1,N1,N0,N1,N0,M1,N1,M2,
     & N0,N1,N0,M1,N1,M2,M1,N1,S529,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S535(N1+1:M2,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S535)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N1,M2,N1,M2,M1,N1,X34,S535, 1.000)
       deallocate(S535)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,M1,N1,M2,N1,M2,M1,N1,X34,VAHPHP, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder421356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N1,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z34(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X34,F2,Z34)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z34,-1.000)
       call
     & sum134625(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z34, 1.000)
       deallocate(Z34)
       deallocate(X34)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,M1+1:N1))
       call reorder2341(M1,N1,N0,N1,N0,M1,N1,M2,
     & N0,N1,N0,M1,N1,M2,M1,N1,S529,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S530(M2+1:N3,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S530)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N1,M2,M2,N3,M1,N1,X30,S530, 1.000)
       deallocate(S530)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,M1,N1,M2,M2,N3,M1,N1,X30,VAHPHP, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N1,M2,N1,M2,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z30(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K5*K9*K9
       I3=K9*K5
       call EGEMM(I1,I2,I3,X30,F2,Z30)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z30,-1.000)
       call
     & sum234615(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z30, 1.000)
       deallocate(Z30)
       deallocate(X30)
C
       allocate(D1(N1+1:N3,N0+1:N1,M1+1:N1,M2+1:N3))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,N1,M1,N1,M2,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S527(M1+1:N1,N0+1:N1,M1+1:N1,M2+1:N3))
       I1=K6*K7*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S527)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder2341(M1,N1,N0,N1,M1,N1,M2,N3,
     & N0,N1,M1,N1,M2,N3,M1,N1,S527,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S534(N1+1:M2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S534)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,M2,N3,N1,M2,M1,N1,X33,S534, 1.000)
       deallocate(S534)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,M2,N3,N1,M2,M1,N1,X33,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder512346(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z33(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X33,F2,Z33)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z33,-1.000)
       call
     & sum134625(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z33, 1.000)
       deallocate(Z33)
       deallocate(X33)
C
       allocate(D1(N0+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder2341(M1,N1,N0,N1,M1,N1,M2,N3,
     & N0,N1,M1,N1,M2,N3,M1,N1,S527,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S528(M2+1:N3,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K7
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S528)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,M2,N3,M2,N3,M1,N1,X29,S528, 1.000)
       deallocate(S528)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,M2,N3,M2,N3,M1,N1,X29,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder512346(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z29(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K5*K9*K9
       I3=K6*K7
       call EGEMM(I1,I2,I3,X29,F2,Z29)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z29, 1.000)
       call
     & sum234615(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z29,-1.000)
       deallocate(Z29)
       deallocate(X29)
C
       allocate(D1(N1+1:N3,N0+1:N1,M1+1:N1,N1+1:M2))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,N1,M1,N1,N1,M2,VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S531(M1+1:N1,N0+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S531)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       call reorder2341(M1,N1,N0,N1,M1,N1,N1,M2,
     & N0,N1,M1,N1,N1,M2,M1,N1,S531,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S536(N1+1:M2,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S536)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,N1,M2,N1,M2,M1,N1,X35,S536, 1.000)
       deallocate(S536)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,N1,M2,N1,M2,M1,N1,X35,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder521346(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z35(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X35,F2,Z35)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z35, 1.000)
       call
     & sum134625(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z35,-1.000)
       deallocate(Z35)
       deallocate(X35)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       call reorder2341(M1,N1,N0,N1,M1,N1,N1,M2,
     & N0,N1,M1,N1,N1,M2,M1,N1,S531,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S532(M2+1:N3,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K7
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S532)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,N1,M2,M2,N3,M1,N1,X31,S532, 1.000)
       deallocate(S532)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,N1,M2,M2,N3,M1,N1,X31,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder512346(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,N1,M2,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z31(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K5*K9*K9
       I3=K9*K7
       call EGEMM(I1,I2,I3,X31,F2,Z31)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z31, 1.000)
       call
     & sum234615(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z31,-1.000)
       deallocate(Z31)
       deallocate(X31)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:N1,M2+1:N3))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,M1,N0,N1,M2,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S537(M1+1:N1,N0+1:M1,N0+1:N1,M2+1:N3))
       I1=K6*K1*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S537)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,M1+1:N1))
       call reorder3241(M1,N1,N0,M1,N0,N1,M2,N3,
     & N0,N1,N0,M1,M2,N3,M1,N1,S537,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S538(N1+1:M2,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S538)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,N1,M2,M1,N1,X79,S538,-1.000)
       deallocate(S538)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z140(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X79,F2,Z140)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z140,-1.000)
       call
     & sum124635(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z140, 1.000)
       deallocate(Z140)
       deallocate(X79)
C
       allocate(D1(N1+1:N3,M1+1:N1,N0+1:N1,M2+1:N3))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,M1,N1,N0,N1,M2,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S539(M1+1:N1,M1+1:N1,N0+1:N1,M2+1:N3))
       I1=K6*K1*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S539)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N1,M2+1:N3,M1+1:N1))
       call reorder3241(M1,N1,M1,N1,N0,N1,M2,N3,
     & N0,N1,M1,N1,M2,N3,M1,N1,S539,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S540(N1+1:M2,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S540)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,M2,N3,N1,M2,M1,N1,X80,S540,-1.000)
       deallocate(S540)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder512346(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z141(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X80,F2,Z141)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z141, 1.000)
       call
     & sum124635(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z141,-1.000)
       deallocate(Z141)
       deallocate(X80)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:N1,N1+1:M2))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,M1,N0,N1,N1,M2,VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S541(M1+1:N1,N0+1:M1,N0+1:N1,N1+1:M2))
       I1=K9*K1*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S541)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,M1+1:N1))
       call reorder3241(M1,N1,N0,M1,N0,N1,N1,M2,
     & N0,N1,N0,M1,N1,M2,M1,N1,S541,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S542(N1+1:M2,N0+1:M1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S542)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N1,M2,N1,M2,M1,N1,X81,S542,-1.000)
       deallocate(S542)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder421356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N1,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z142(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X81,F2,Z142)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z142, 1.000)
       call
     & sum124635(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z142,-1.000)
       deallocate(Z142)
       deallocate(X81)
C
       allocate(D1(N1+1:N3,M1+1:N1,N0+1:N1,N1+1:M2))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,M1,N1,N0,N1,N1,M2,VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S543(M1+1:N1,M1+1:N1,N0+1:N1,N1+1:M2))
       I1=K9*K1*K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S543)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,M1+1:N1))
       call reorder3241(M1,N1,M1,N1,N0,N1,N1,M2,
     & N0,N1,M1,N1,N1,M2,M1,N1,S543,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S544(N1+1:M2,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S544)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,N1,M2,N1,M2,M1,N1,X82,S544,-1.000)
       deallocate(S544)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder521346(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z143(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X82,F2,Z143)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z143,-1.000)
       call
     & sum124635(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z143, 1.000)
       deallocate(Z143)
       deallocate(X82)
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
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(Q55(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q55)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(N0,M1,N0,M1,X15,Q55, 1.000)
       deallocate(Q55)
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
     & sum21(N0,M1,M1,N1,X13,Q52, 1.000)
       deallocate(Q52)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:M1,M2+1:N3))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,N1,N0,M1,M2,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S545(N0+1:M1,N0+1:N1,N0+1:M1,M2+1:N3))
       I1=K6*K5*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S545)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N1,N0,M1,M2,N3,
     & N0,N1,N0,M1,M2,N3,N0,M1,S545,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S553(N1+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S553)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,N1,M2,N0,M1,X44,S553, 1.000)
       deallocate(S553)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,M1,X44,VAHPHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(Z44(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X44,F2,Z44)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z44, 1.000)
       deallocate(Z44)
       deallocate(X44)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N1,N0,M1,M2,N3,
     & N0,N1,N0,M1,M2,N3,N0,M1,S545,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S546(M2+1:N3,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S546)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,M2,N3,N0,M1,X40,S546, 1.000)
       deallocate(S546)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,M1,X40,VAHPHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(Z40(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K7*K9*K9
       I3=K6*K5
       call EGEMM(I1,I2,I3,X40,F2,Z40)
       deallocate(F2)
C
       call
     & sum235614(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z40,-1.000)
       deallocate(Z40)
       deallocate(X40)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:M1,N1+1:M2))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,N1,N0,M1,N1,M2,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S549(N0+1:M1,N0+1:N1,N0+1:M1,N1+1:M2))
       I1=K9*K5*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S549)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N1,N0,M1,N1,M2,
     & N0,N1,N0,M1,N1,M2,N0,M1,S549,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S555(N1+1:M2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S555)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N1,M2,N1,M2,N0,M1,X46,S555, 1.000)
       deallocate(S555)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,M1,N1,M2,N1,M2,N0,M1,X46,VAHPHP, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder421356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N1,M2,M2,N3,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(Z46(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X46,F2,Z46)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z46,-1.000)
       deallocate(Z46)
       deallocate(X46)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N1,N0,M1,N1,M2,
     & N0,N1,N0,M1,N1,M2,N0,M1,S549,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S550(M2+1:N3,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S550)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N1,M2,M2,N3,N0,M1,X42,S550, 1.000)
       deallocate(S550)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,M1,N1,M2,M2,N3,N0,M1,X42,VAHPHP, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N1,M2,N1,M2,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(Z42(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K7*K9*K9
       I3=K9*K5
       call EGEMM(I1,I2,I3,X42,F2,Z42)
       deallocate(F2)
C
       call
     & sum235614(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z42,-1.000)
       deallocate(Z42)
       deallocate(X42)
C
       allocate(D1(N1+1:N3,N0+1:N1,M1+1:N1,M2+1:N3))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,N1,M1,N1,M2,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S547(N0+1:M1,N0+1:N1,M1+1:N1,M2+1:N3))
       I1=K6*K7*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S547)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N1,M1,N1,M2,N3,
     & N0,N1,M1,N1,M2,N3,N0,M1,S547,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S554(N1+1:M2,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S554)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,M2,N3,N1,M2,N0,M1,X45,S554, 1.000)
       deallocate(S554)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,M1,X45,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(Z45(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X45,F2,Z45)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z45, 1.000)
       deallocate(Z45)
       deallocate(X45)
C
       allocate(D1(N0+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N1,M1,N1,M2,N3,
     & N0,N1,M1,N1,M2,N3,N0,M1,S547,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S548(M2+1:N3,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S548)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,M2,N3,M2,N3,N0,M1,X41,S548, 1.000)
       deallocate(S548)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,M2,N3,M2,N3,N0,M1,X41,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(Z41(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K7*K9*K9
       I3=K6*K7
       call EGEMM(I1,I2,I3,X41,F2,Z41)
       deallocate(F2)
C
       call
     & sum235614(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z41,-1.000)
       deallocate(Z41)
       deallocate(X41)
C
       allocate(D1(N1+1:N3,N0+1:N1,M1+1:N1,N1+1:M2))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,N1,M1,N1,N1,M2,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S551(N0+1:M1,N0+1:N1,M1+1:N1,N1+1:M2))
       I1=K9*K7*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S551)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N1,M1,N1,N1,M2,
     & N0,N1,M1,N1,N1,M2,N0,M1,S551,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S556(N1+1:M2,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S556)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,N1,M2,N1,M2,N0,M1,X47,S556, 1.000)
       deallocate(S556)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,N1,M2,N1,M2,N0,M1,X47,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder421356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,M2,N3,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(Z47(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X47,F2,Z47)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z47,-1.000)
       deallocate(Z47)
       deallocate(X47)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N1,M1,N1,N1,M2,
     & N0,N1,M1,N1,N1,M2,N0,M1,S551,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S552(M2+1:N3,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S552)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,N1,M2,M2,N3,N0,M1,X43,S552, 1.000)
       deallocate(S552)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,N1,M2,M2,N3,N0,M1,X43,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,N1,M2,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(Z43(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K7*K9*K9
       I3=K9*K7
       call EGEMM(I1,I2,I3,X43,F2,Z43)
       deallocate(F2)
C
       call
     & sum235614(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z43,-1.000)
       deallocate(Z43)
       deallocate(X43)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:N1,M2+1:N3))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,M1,N0,N1,M2,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S557(N0+1:M1,N0+1:M1,N0+1:N1,M2+1:N3))
       I1=K6*K1*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S557)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder3241(N0,M1,N0,M1,N0,N1,M2,N3,
     & N0,N1,N0,M1,M2,N3,N0,M1,S557,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S558(N1+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S558)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,N1,M2,N0,M1,X83,S558,-1.000)
       deallocate(S558)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(Z169(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X83,F2,Z169)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z169,-1.000)
       deallocate(Z169)
       deallocate(X83)
C
       allocate(D1(N1+1:N3,M1+1:N1,N0+1:N1,M2+1:N3))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,M1,N1,N0,N1,M2,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S559(N0+1:M1,M1+1:N1,N0+1:N1,M2+1:N3))
       I1=K6*K1*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S559)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder3241(N0,M1,M1,N1,N0,N1,M2,N3,
     & N0,N1,M1,N1,M2,N3,N0,M1,S559,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S560(N1+1:M2,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S560)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,M2,N3,N1,M2,N0,M1,X84,S560,-1.000)
       deallocate(S560)
C
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(Z170(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K6*K7
       call EGEMM(I1,I2,I3,X84,F2,Z170)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z170,-1.000)
       deallocate(Z170)
       deallocate(X84)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:N1,N1+1:M2))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,M1,N0,N1,N1,M2,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S561(N0+1:M1,N0+1:M1,N0+1:N1,N1+1:M2))
       I1=K9*K1*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S561)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder3241(N0,M1,N0,M1,N0,N1,N1,M2,
     & N0,N1,N0,M1,N1,M2,N0,M1,S561,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S562(N1+1:M2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S562)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N1,M2,N1,M2,N0,M1,X85,S562,-1.000)
       deallocate(S562)
C
       allocate(F2(N0+1:M1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder421356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,N1,M2,M2,N3,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(Z171(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K9*K5
       call EGEMM(I1,I2,I3,X85,F2,Z171)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z171, 1.000)
       deallocate(Z171)
       deallocate(X85)
C
       allocate(D1(N1+1:N3,M1+1:N1,N0+1:N1,N1+1:M2))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,M1,N1,N0,N1,N1,M2,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S563(N0+1:M1,M1+1:N1,N0+1:N1,N1+1:M2))
       I1=K9*K1*K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S563)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder3241(N0,M1,M1,N1,N0,N1,N1,M2,
     & N0,N1,M1,N1,N1,M2,N0,M1,S563,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S564(N1+1:M2,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S564)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,N1,M2,N1,M2,N0,M1,X86,S564,-1.000)
       deallocate(S564)
C
       allocate(F2(M1+1:N1,N1+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder421356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,N1,M2,M2,N3,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(Z172(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K9*K7
       call EGEMM(I1,I2,I3,X86,F2,Z172)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z172, 1.000)
       deallocate(Z172)
       deallocate(X86)
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
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(Q56(N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q56)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(M1,N1,N0,M1,X16,Q56, 1.000)
       deallocate(Q56)
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
     & sum21(M1,N1,M1,N1,X14,Q54, 1.000)
       deallocate(Q54)
C
       allocate(D1(N0+1:N1,N0+1:N1,M2+1:N3,M2+1:N3))
       call reorder4312(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,M2,N3,M2,N3,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S565(N1+1:M2,N0+1:N1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S565)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M2+1:N3,M2+1:N3,N1+1:M2))
       call reorder2341(N1,M2,N0,N1,M2,N3,M2,N3,
     & N0,N1,M2,N3,M2,N3,N1,M2,S565,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S566(M2+1:N3,M2+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6*K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S566)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,M2,N3,M2,N3,N1,M2,X52,S566, 1.000)
       deallocate(S566)
C
       call sumx1234(N1,N3,N1,N3,N1,N3,N1,M2,
     & M2,N3,M2,N3,M2,N3,N1,M2,X52,VAAPPP, 1.000)
C
       allocate(F2(M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,M1+1:N1))
       call reorder123456(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,M1,N1,t3A,F2)
       allocate(Z52(N1+1:M2,N0+1:M1,M1+1:N1,M1+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K7*K7*K5*K9
       I3=K6*K6
       call EGEMM(I1,I2,I3,X52,F2,Z52)
       deallocate(F2)
C
       call
     & sum345612(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z52, 0.500)
       deallocate(Z52)
       deallocate(X52)
C
       allocate(D1(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder4312(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,M2,N3,N1,M2,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S567(N1+1:M2,N0+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S567)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder2341(N1,M2,N0,N1,M2,N3,N1,M2,
     & N0,N1,M2,N3,N1,M2,N1,M2,S567,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S577(N1+1:M2,M2+1:N3,N1+1:M2,N1+1:M2))
       I1=K9*K9*K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S577)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M2,N3,N1,M2,N1,M2,N1,M2,X58,S577,-1.000)
       deallocate(S577)
C
       call sumx1234(N1,N3,N1,N3,N1,N3,N1,M2,
     & M2,N3,N1,M2,N1,M2,N1,M2,X58,VAAPPP, 1.000)
C
       allocate(F2(M2+1:N3,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1,M1+1:N1))
       call reorder132456(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M2,N3,N1,M2,M2,N3,N0,M1,M1,N1,M1,N1,t3A,F2)
       allocate(Z58(M2+1:N3,N0+1:M1,M1+1:N1,M1+1:N1,N1+1:M2,N1+1:M2))
       I1=K9*K9
       I2=K7*K7*K5*K6
       I3=K9*K6
       call EGEMM(I1,I2,I3,X58,F2,Z58)
       deallocate(F2)
C
       call
     & sum145623(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z58,-1.000)
       deallocate(Z58)
       deallocate(X58)
C
       allocate(D1(N0+1:N1,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder2341(N1,M2,N0,N1,M2,N3,N1,M2,
     & N0,N1,M2,N3,N1,M2,N1,M2,S567,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S568(M2+1:N3,M2+1:N3,N1+1:M2,N1+1:M2))
       I1=K9*K9*K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S568)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,N1,M2,M2,N3,N1,M2,X53,S568, 1.000)
       deallocate(S568)
C
       call sumx1234(N1,N3,N1,N3,N1,N3,N1,M2,
     & M2,N3,N1,M2,M2,N3,N1,M2,X53,VAAPPP, 1.000)
C
       allocate(F2(M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M1+1:N1))
       call reorder123456(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,t3A,F2)
       allocate(Z53(N1+1:M2,N0+1:M1,M1+1:N1,M1+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K7*K7*K5*K9
       I3=K9*K6
       call EGEMM(I1,I2,I3,X53,F2,Z53)
       deallocate(F2)
C
       call
     & sum345612(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z53, 1.000)
       deallocate(Z53)
       deallocate(X53)
C
       allocate(D1(N0+1:N1,N0+1:N1,M2+1:N3,M2+1:N3))
       call reorder4312(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,M2,N3,M2,N3,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S571(N1+1:M2,N0+1:N1,M2+1:N3,M2+1:N3))
       I1=K6*K6*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S571)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M2+1:N3,M2+1:N3,N1+1:M2))
       call reorder2341(N1,M2,N0,N1,M2,N3,M2,N3,
     & N0,N1,M2,N3,M2,N3,N1,M2,S571,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S572(M2+1:N3,M2+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6*K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S572)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,M2,N3,M2,N3,N1,M2,X55,S572, 1.000)
       deallocate(S572)
C
       call sumx1234(N1,N3,N1,N3,N1,N3,N1,M2,
     & M2,N3,M2,N3,M2,N3,N1,M2,X55,VAAPPP, 1.000)
C
       allocate(F2(M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,M1+1:N1))
       call reorder123456(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,M1,N1,t3A,F2)
       allocate(Z55(N1+1:M2,N0+1:M1,M1+1:N1,M1+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K7*K7*K5*K9
       I3=K6*K6
       call EGEMM(I1,I2,I3,X55,F2,Z55)
       deallocate(F2)
C
       call
     & sum245613(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z55,-0.500)
       deallocate(Z55)
       deallocate(X55)
C
       allocate(D1(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder4312(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,M2,N3,N1,M2,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S573(N1+1:M2,N0+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S573)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder2341(N1,M2,N0,N1,M2,N3,N1,M2,
     & N0,N1,M2,N3,N1,M2,N1,M2,S573,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S574(M2+1:N3,M2+1:N3,N1+1:M2,N1+1:M2))
       I1=K9*K9*K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S574)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,N1,M2,M2,N3,N1,M2,X56,S574, 1.000)
       deallocate(S574)
C
       call sumx1234(N1,N3,N1,N3,N1,N3,N1,M2,
     & M2,N3,N1,M2,M2,N3,N1,M2,X56,VAAPPP, 1.000)
C
       allocate(F2(M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M1+1:N1))
       call reorder123456(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,t3A,F2)
       allocate(Z56(N1+1:M2,N0+1:M1,M1+1:N1,M1+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K7*K7*K5*K9
       I3=K9*K6
       call EGEMM(I1,I2,I3,X56,F2,Z56)
       deallocate(F2)
C
       call
     & sum245613(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z56,-1.000)
       deallocate(Z56)
       deallocate(X56)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:M2,N1+1:M2))
       call reorder4312(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,M2,N1,M2,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S575(N1+1:M2,N0+1:N1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S575)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:M2,N1+1:M2,N1+1:M2))
       call reorder2341(N1,M2,N0,N1,N1,M2,N1,M2,
     & N0,N1,N1,M2,N1,M2,N1,M2,S575,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S576(M2+1:N3,N1+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9*K9
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S576)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,M2,N1,M2,M2,N3,N1,M2,X57,S576, 1.000)
       deallocate(S576)
C
       call sumx1234(N1,N3,N1,N3,N1,N3,N1,M2,
     & N1,M2,N1,M2,M2,N3,N1,M2,X57,VAAPPP, 1.000)
C
       allocate(F2(N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M1+1:N1))
       call reorder123456(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N1,M2,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,t3A,F2)
       allocate(Z57(N1+1:M2,N0+1:M1,M1+1:N1,M1+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K7*K7*K5*K9
       I3=K9*K9
       call EGEMM(I1,I2,I3,X57,F2,Z57)
       deallocate(F2)
C
       call
     & sum245613(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z57,-0.500)
       deallocate(Z57)
       deallocate(X57)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M2+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,M2,N3,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q57(N0+1:N1,M2+1:N3))
       I1=K6*K1
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q57)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(Q61(N1+1:M2,M2+1:N3))
       I1=K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,Q57,B2,Q61)
       deallocate(B2)
C
       call
     & sum21(M2,N3,N1,M2,X19,Q61,-1.000)
       deallocate(Q61)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(Q63(N1+1:M2,M2+1:N3))
       I1=K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,Q57,B2,Q63)
       deallocate(B2)
C
       call
     & sum21(M2,N3,N1,M2,X21,Q63,-1.000)
       deallocate(Q63)
C
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(Q58(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,Q57,B2,Q58)
       deallocate(B2)
C
       call
     & sum21(M2,N3,M2,N3,X17,Q58,-1.000)
       deallocate(Q58)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:M2,N1+1:M2))
       call reorder4312(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,M2,N1,M2,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S569(N1+1:M2,N0+1:N1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S569)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:M2,N1+1:M2,N1+1:M2))
       call reorder2341(N1,M2,N0,N1,N1,M2,N1,M2,
     & N0,N1,N1,M2,N1,M2,N1,M2,S569,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S570(M2+1:N3,N1+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9*K9
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S570)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,M2,N1,M2,M2,N3,N1,M2,X54,S570, 1.000)
       deallocate(S570)
C
       call sumx1234(N1,N3,N1,N3,N1,N3,N1,M2,
     & N1,M2,N1,M2,M2,N3,N1,M2,X54,VAAPPP, 1.000)
C
       allocate(F2(N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M1+1:N1))
       call reorder123456(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N1,M2,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,t3A,F2)
       allocate(Z54(N1+1:M2,N0+1:M1,M1+1:N1,M1+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K7*K7*K5*K9
       I3=K9*K9
       call EGEMM(I1,I2,I3,X54,F2,Z54)
       deallocate(F2)
C
       call
     & sum345612(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z54, 0.500)
       deallocate(Z54)
       deallocate(X54)
C
       allocate(D1(N0+1:N1,N1+1:M2,N1+1:M2,N1+1:M2))
       call reorder2341(N1,M2,N0,N1,N1,M2,N1,M2,
     & N0,N1,N1,M2,N1,M2,N1,M2,S569,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S578(N1+1:M2,N1+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9*K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S578)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N1,M2,N1,M2,N1,M2,N1,M2,X59,S578,-1.000)
       deallocate(S578)
C
       call sumx1234(N1,N3,N1,N3,N1,N3,N1,M2,
     & N1,M2,N1,M2,N1,M2,N1,M2,X59,VAAPPP, 1.000)
C
       allocate(F2(N1+1:M2,N1+1:M2,M2+1:N3,N0+1:M1,M1+1:N1,M1+1:N1))
       call reorder231456(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N1,M2,N1,M2,M2,N3,N0,M1,M1,N1,M1,N1,t3A,F2)
       allocate(Z59(M2+1:N3,N0+1:M1,M1+1:N1,M1+1:N1,N1+1:M2,N1+1:M2))
       I1=K9*K9
       I2=K7*K7*K5*K6
       I3=K9*K9
       call EGEMM(I1,I2,I3,X59,F2,Z59)
       deallocate(F2)
C
       call
     & sum145623(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z59, 0.500)
       deallocate(Z59)
       deallocate(X59)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:M2))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N1,M2,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q59(N0+1:N1,N1+1:M2))
       I1=K9*K1
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q59)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(Q60(M2+1:N3,N1+1:M2))
       I1=K9
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,Q59,B2,Q60)
       deallocate(B2)
C
       call
     & sum21(N1,M2,M2,N3,X18,Q60,-1.000)
       deallocate(Q60)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(Q64(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,Q59,B2,Q64)
       deallocate(B2)
C
       call
     & sum21(N1,M2,N1,M2,X22,Q64,-1.000)
       deallocate(Q64)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(Q62(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,Q59,B2,Q62)
       deallocate(B2)
C
       call
     & sum21(N1,M2,N1,M2,X20,Q62,-1.000)
       deallocate(Q62)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:N1,M2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,N0,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S579(M1+1:N1,N0+1:M1,N0+1:N1,M2+1:N3))
       I1=K6*K1*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S579)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,M1+1:N1))
       call reorder3241(M1,N1,N0,M1,N0,N1,M2,N3,
     & N0,N1,N0,M1,M2,N3,M1,N1,S579,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S587(N1+1:M2,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S587)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,N1,M2,M1,N1,X97,S587,-1.000)
       deallocate(S587)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z504(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X97,F2,Z504)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z504,-1.000)
       call
     & sum134625(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z504, 1.000)
       deallocate(Z504)
       deallocate(X97)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,M1+1:N1))
       call reorder3241(M1,N1,N0,M1,N0,N1,M2,N3,
     & N0,N1,N0,M1,M2,N3,M1,N1,S579,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S591(N1+1:M2,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S591)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,N1,M2,M1,N1,X101,S591,-1.000)
       deallocate(S591)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z508(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X101,F2,Z508)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z508, 1.000)
       call
     & sum124635(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z508,-1.000)
       deallocate(Z508)
       deallocate(X101)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,M1+1:N1))
       call reorder3241(M1,N1,N0,M1,N0,N1,M2,N3,
     & N0,N1,N0,M1,M2,N3,M1,N1,S579,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S580(M2+1:N3,N0+1:M1,M2+1:N3,M1+1:N1))
       I1=K7*K6*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S580)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,M2,N3,M1,N1,X93,S580,-1.000)
       deallocate(S580)
C
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z494(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K5*K9*K9
       I3=K6*K5
       call EGEMM(I1,I2,I3,X93,F2,Z494)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z494, 1.000)
       call
     & sum234615(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z494,-1.000)
       deallocate(Z494)
       deallocate(X93)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:N1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,N0,N1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S585(M1+1:N1,M1+1:N2,N0+1:N1,N2+1:M2))
       I1=K0*K1*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S585)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,M1+1:N1))
       call reorder3241(M1,N1,M1,N2,N0,N1,N2,M2,
     & N0,N1,M1,N2,N2,M2,M1,N1,S585,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S590(N1+1:M2,M1+1:N2,N2+1:M2,M1+1:N1))
       I1=K7*K0*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S590)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,N2,M2,N1,M2,M1,N1,X100,S590,-1.000)
       deallocate(S590)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z507(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X100,F2,Z507)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z507,-1.000)
       call
     & sum134625(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z507, 1.000)
       deallocate(Z507)
       deallocate(X100)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,M1+1:N1))
       call reorder3241(M1,N1,M1,N2,N0,N1,N2,M2,
     & N0,N1,M1,N2,N2,M2,M1,N1,S585,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S594(N1+1:M2,M1+1:N2,N2+1:M2,M1+1:N1))
       I1=K7*K0*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S594)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,N2,M2,N1,M2,M1,N1,X104,S594,-1.000)
       deallocate(S594)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z511(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X104,F2,Z511)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z511, 1.000)
       call
     & sum124635(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z511,-1.000)
       deallocate(Z511)
       deallocate(X104)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,M1+1:N1))
       call reorder3241(M1,N1,M1,N2,N0,N1,N2,M2,
     & N0,N1,M1,N2,N2,M2,M1,N1,S585,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S586(M2+1:N3,M1+1:N2,N2+1:M2,M1+1:N1))
       I1=K7*K0*K8
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S586)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,N2,M2,M2,N3,M1,N1,X96,S586,-1.000)
       deallocate(S586)
C
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z497(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K5*K9*K9
       I3=K0*K8
       call EGEMM(I1,I2,I3,X96,F2,Z497)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z497, 1.000)
       call
     & sum234615(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z497,-1.000)
       deallocate(Z497)
       deallocate(X96)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:N1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,N0,N1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S583(M1+1:N1,N0+1:M1,N0+1:N1,N2+1:M2))
       I1=K0*K1*K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S583)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,N2+1:M2,M1+1:N1))
       call reorder3241(M1,N1,N0,M1,N0,N1,N2,M2,
     & N0,N1,N0,M1,N2,M2,M1,N1,S583,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S584(M2+1:N3,N0+1:M1,N2+1:M2,M1+1:N1))
       I1=K7*K0*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S584)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N2,M2,M2,N3,M1,N1,X95,S584,-1.000)
       deallocate(S584)
C
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z496(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K5*K9*K9
       I3=K0*K5
       call EGEMM(I1,I2,I3,X95,F2,Z496)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z496, 1.000)
       call
     & sum234615(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z496,-1.000)
       deallocate(Z496)
       deallocate(X95)
C
       allocate(D1(N0+1:N1,N0+1:M1,N2+1:M2,M1+1:N1))
       call reorder3241(M1,N1,N0,M1,N0,N1,N2,M2,
     & N0,N1,N0,M1,N2,M2,M1,N1,S583,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S593(N1+1:M2,N0+1:M1,N2+1:M2,M1+1:N1))
       I1=K7*K0*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S593)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N2,M2,N1,M2,M1,N1,X103,S593,-1.000)
       deallocate(S593)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z510(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,X103,F2,Z510)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z510, 1.000)
       call
     & sum124635(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z510,-1.000)
       deallocate(Z510)
       deallocate(X103)
C
       allocate(D1(N0+1:N1,N0+1:M1,N2+1:M2,M1+1:N1))
       call reorder3241(M1,N1,N0,M1,N0,N1,N2,M2,
     & N0,N1,N0,M1,N2,M2,M1,N1,S583,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S589(N1+1:M2,N0+1:M1,N2+1:M2,M1+1:N1))
       I1=K7*K0*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S589)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N2,M2,N1,M2,M1,N1,X99,S589,-1.000)
       deallocate(S589)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z506(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,X99,F2,Z506)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z506,-1.000)
       call
     & sum134625(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z506, 1.000)
       deallocate(Z506)
       deallocate(X99)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:N1,M2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,N0,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S581(M1+1:N1,M1+1:N2,N0+1:N1,M2+1:N3))
       I1=K6*K1*K8
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S581)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,M1+1:N1))
       call reorder3241(M1,N1,M1,N2,N0,N1,M2,N3,
     & N0,N1,M1,N2,M2,N3,M1,N1,S581,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S582(M2+1:N3,M1+1:N2,M2+1:N3,M1+1:N1))
       I1=K7*K6*K8
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S582)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M2,N3,M2,N3,M1,N1,X94,S582,-1.000)
       deallocate(S582)
C
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z495(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,M1+1:N1))
       I1=K7*K6
       I2=K7*K5*K9*K9
       I3=K6*K8
       call EGEMM(I1,I2,I3,X94,F2,Z495)
       deallocate(F2)
C
       call
     & sum234516(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z495, 1.000)
       call
     & sum234615(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z495,-1.000)
       deallocate(Z495)
       deallocate(X94)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,M1+1:N1))
       call reorder3241(M1,N1,M1,N2,N0,N1,M2,N3,
     & N0,N1,M1,N2,M2,N3,M1,N1,S581,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S588(N1+1:M2,M1+1:N2,M2+1:N3,M1+1:N1))
       I1=K7*K6*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S588)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M2,N3,N1,M2,M1,N1,X98,S588,-1.000)
       deallocate(S588)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z505(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X98,F2,Z505)
       deallocate(F2)
C
       call
     & sum134526(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z505,-1.000)
       call
     & sum134625(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z505, 1.000)
       deallocate(Z505)
       deallocate(X98)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,M1+1:N1))
       call reorder3241(M1,N1,M1,N2,N0,N1,M2,N3,
     & N0,N1,M1,N2,M2,N3,M1,N1,S581,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S592(N1+1:M2,M1+1:N2,M2+1:N3,M1+1:N1))
       I1=K7*K6*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S592)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M2,N3,N1,M2,M1,N1,X102,S592,-1.000)
       deallocate(S592)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z509(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X102,F2,Z509)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z509, 1.000)
       call
     & sum124635(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z509,-1.000)
       deallocate(Z509)
       deallocate(X102)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:N1,M2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,N0,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S595(N0+1:M1,N0+1:M1,N0+1:N1,M2+1:N3))
       I1=K6*K1*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S595)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder3241(N0,M1,N0,M1,N0,N1,M2,N3,
     & N0,N1,N0,M1,M2,N3,N0,M1,S595,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S603(N1+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S603)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,N1,M2,N0,M1,X109,S603,-1.000)
       deallocate(S603)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z538(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X109,F2,Z538)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z538,-1.000)
       deallocate(Z538)
       deallocate(X109)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder3241(N0,M1,N0,M1,N0,N1,M2,N3,
     & N0,N1,N0,M1,M2,N3,N0,M1,S595,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S607(N1+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S607)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,N1,M2,N0,M1,X113,S607,-1.000)
       deallocate(S607)
C
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z542(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,X113,F2,Z542)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z542, 1.000)
       deallocate(Z542)
       deallocate(X113)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder3241(N0,M1,N0,M1,N0,N1,M2,N3,
     & N0,N1,N0,M1,M2,N3,N0,M1,S595,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S596(M2+1:N3,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S596)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,M2,N3,N0,M1,X105,S596,-1.000)
       deallocate(S596)
C
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z528(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K7*K9*K9
       I3=K6*K5
       call EGEMM(I1,I2,I3,X105,F2,Z528)
       deallocate(F2)
C
       call
     & sum235614(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z528, 1.000)
       deallocate(Z528)
       deallocate(X105)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:N1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,N0,N1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S601(N0+1:M1,M1+1:N2,N0+1:N1,N2+1:M2))
       I1=K0*K1*K8
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S601)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder3241(N0,M1,M1,N2,N0,N1,N2,M2,
     & N0,N1,M1,N2,N2,M2,N0,M1,S601,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S606(N1+1:M2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S606)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,N2,M2,N1,M2,N0,M1,X112,S606,-1.000)
       deallocate(S606)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z541(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X112,F2,Z541)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z541,-1.000)
       deallocate(Z541)
       deallocate(X112)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder3241(N0,M1,M1,N2,N0,N1,N2,M2,
     & N0,N1,M1,N2,N2,M2,N0,M1,S601,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S610(N1+1:M2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S610)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,N2,M2,N1,M2,N0,M1,X116,S610,-1.000)
       deallocate(S610)
C
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z545(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,X116,F2,Z545)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z545, 1.000)
       deallocate(Z545)
       deallocate(X116)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder3241(N0,M1,M1,N2,N0,N1,N2,M2,
     & N0,N1,M1,N2,N2,M2,N0,M1,S601,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S602(M2+1:N3,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S602)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,N2,M2,M2,N3,N0,M1,X108,S602,-1.000)
       deallocate(S602)
C
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z531(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K7*K9*K9
       I3=K0*K8
       call EGEMM(I1,I2,I3,X108,F2,Z531)
       deallocate(F2)
C
       call
     & sum235614(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z531, 1.000)
       deallocate(Z531)
       deallocate(X108)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:N1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,N0,N1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S599(N0+1:M1,N0+1:M1,N0+1:N1,N2+1:M2))
       I1=K0*K1*K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S599)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder3241(N0,M1,N0,M1,N0,N1,N2,M2,
     & N0,N1,N0,M1,N2,M2,N0,M1,S599,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S600(M2+1:N3,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S600)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N2,M2,M2,N3,N0,M1,X107,S600,-1.000)
       deallocate(S600)
C
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z530(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K7*K9*K9
       I3=K0*K5
       call EGEMM(I1,I2,I3,X107,F2,Z530)
       deallocate(F2)
C
       call
     & sum235614(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z530, 1.000)
       deallocate(Z530)
       deallocate(X107)
C
       allocate(D1(N0+1:N1,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder3241(N0,M1,N0,M1,N0,N1,N2,M2,
     & N0,N1,N0,M1,N2,M2,N0,M1,S599,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S609(N1+1:M2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S609)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N2,M2,N1,M2,N0,M1,X115,S609,-1.000)
       deallocate(S609)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z544(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,X115,F2,Z544)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z544, 1.000)
       deallocate(Z544)
       deallocate(X115)
C
       allocate(D1(N0+1:N1,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder3241(N0,M1,N0,M1,N0,N1,N2,M2,
     & N0,N1,N0,M1,N2,M2,N0,M1,S599,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S605(N1+1:M2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S605)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N2,M2,N1,M2,N0,M1,X111,S605,-1.000)
       deallocate(S605)
C
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z540(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,X111,F2,Z540)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z540,-1.000)
       deallocate(Z540)
       deallocate(X111)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:N1,M2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,N0,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S597(N0+1:M1,M1+1:N2,N0+1:N1,M2+1:N3))
       I1=K6*K1*K8
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S597)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder3241(N0,M1,M1,N2,N0,N1,M2,N3,
     & N0,N1,M1,N2,M2,N3,N0,M1,S597,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S598(M2+1:N3,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S598)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M2,N3,M2,N3,N0,M1,X106,S598,-1.000)
       deallocate(S598)
C
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,N1,M2,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z529(N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6
       I2=K7*K7*K9*K9
       I3=K6*K8
       call EGEMM(I1,I2,I3,X106,F2,Z529)
       deallocate(F2)
C
       call
     & sum235614(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z529, 1.000)
       deallocate(Z529)
       deallocate(X106)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder3241(N0,M1,M1,N2,N0,N1,M2,N3,
     & N0,N1,M1,N2,M2,N3,N0,M1,S597,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S604(N1+1:M2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S604)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M2,N3,N1,M2,N0,M1,X110,S604,-1.000)
       deallocate(S604)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z539(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X110,F2,Z539)
       deallocate(F2)
C
       call
     & sum135624(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z539,-1.000)
       deallocate(Z539)
       deallocate(X110)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder3241(N0,M1,M1,N2,N0,N1,M2,N3,
     & N0,N1,M1,N2,M2,N3,N0,M1,S597,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S608(N1+1:M2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S608)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M2,N3,N1,M2,N0,M1,X114,S608,-1.000)
       deallocate(S608)
C
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z543(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,X114,F2,Z543)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z543, 1.000)
       deallocate(Z543)
       deallocate(X114)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,N1,N3,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q65(N0+1:M1,N1+1:N3))
       I1=K3*K5
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q65)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,N0+1:M1))
       call reorder21(N0,M1,N1,N3,
     & N1,N3,N0,M1,Q65,B1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(Q69(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q69)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(N0,M1,N0,M1,X15,Q69, 1.000)
       deallocate(Q69)
C
       call sumx21(N0,N1,N0,N1,
     & N0,M1,N0,M1,X15,FAHH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(Z15(M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7*K7*K9*K9*K6
       I3=K5
       call EGEMM(I1,I2,I3,X15,F2,Z15)
       deallocate(F2)
C
       call
     & sum123564(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z15,-1.000)
       deallocate(Z15)
       deallocate(X15)
C
       allocate(B1(N1+1:N3,N0+1:M1))
       call reorder21(N0,M1,N1,N3,
     & N1,N3,N0,M1,Q65,B1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(Q66(M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q66)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(N0,M1,M1,N1,X13,Q66, 1.000)
       deallocate(Q66)
C
       call sumx21(N0,N1,N0,N1,
     & N0,M1,M1,N1,X13,FAHH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N0,M1,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z13(M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7*K5*K9*K9*K6
       I3=K5
       call EGEMM(I1,I2,I3,X13,F2,Z13)
       deallocate(F2)
C
       V3A=V3A-Z13
       call
     & sum123465(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z13, 1.000)
       deallocate(Z13)
       deallocate(X13)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,N1,N3,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q67(M1+1:N1,N1+1:N3))
       I1=K3*K7
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q67)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,M1+1:N1))
       call reorder21(M1,N1,N1,N3,
     & N1,N3,M1,N1,Q67,B1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(Q70(N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q70)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(M1,N1,N0,M1,X16,Q70, 1.000)
       deallocate(Q70)
C
       call sumx21(N0,N1,N0,N1,
     & M1,N1,N0,M1,X16,FAHH, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,N1,M2,M1,N1,M1,N1,t3A,F2)
       allocate(Z16(M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N1,M1+1:N1,N0+1:M1))
       I1=K5
       I2=K7*K7*K9*K9*K6
       I3=K7
       call EGEMM(I1,I2,I3,X16,F2,Z16)
       deallocate(F2)
C
       call
     & sum123564(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z16,-1.000)
       deallocate(Z16)
       deallocate(X16)
C
       allocate(B1(N1+1:N3,M1+1:N1))
       call reorder21(M1,N1,N1,N3,
     & N1,N3,M1,N1,Q67,B1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(Q68(M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,B1,B2,Q68)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(M1,N1,M1,N1,X14,Q68, 1.000)
       deallocate(Q68)
C
       call sumx21(N0,N1,N0,N1,
     & M1,N1,M1,N1,X14,FAHH, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder512346(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,t3A,F2)
       allocate(Z14(M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M1+1:N1))
       I1=K7
       I2=K7*K5*K9*K9*K6
       I3=K7
       call EGEMM(I1,I2,I3,X14,F2,Z14)
       deallocate(F2)
C
       V3A=V3A+Z14
       call
     & sum123465(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z14,-1.000)
       deallocate(Z14)
       deallocate(X14)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q71(N0+1:N1,M2+1:N3))
       I1=K6*K1
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q71)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(Q75(N1+1:M2,M2+1:N3))
       I1=K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,Q71,B2,Q75)
       deallocate(B2)
C
       call
     & sum21(M2,N3,N1,M2,X19,Q75,-1.000)
       deallocate(Q75)
C
       call sumx12(N1,N3,N1,N3,
     & M2,N3,N1,M2,X19,FAPP, 1.000)
C
       allocate(F2(M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,M1+1:N1))
       call reorder123456(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,M1,N1,t3A,F2)
       allocate(Z19(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K5*K9*K6
       I3=K6
       call EGEMM(I1,I2,I3,X19,F2,Z19)
       deallocate(F2)
C
       call
     & sum134562(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z19,-1.000)
       deallocate(Z19)
       deallocate(X19)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(Q77(N1+1:M2,M2+1:N3))
       I1=K6
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,Q71,B2,Q77)
       deallocate(B2)
C
       call
     & sum21(M2,N3,N1,M2,X21,Q77,-1.000)
       deallocate(Q77)
C
       call sumx12(N1,N3,N1,N3,
     & M2,N3,N1,M2,X21,FAPP, 1.000)
C
       allocate(F2(M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,M1+1:N1))
       call reorder123456(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,M1,N1,t3A,F2)
       allocate(Z21(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K5*K9*K6
       I3=K6
       call EGEMM(I1,I2,I3,X21,F2,Z21)
       deallocate(F2)
C
       call
     & sum124563(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z21, 1.000)
       deallocate(Z21)
       deallocate(X21)
C
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(Q72(M2+1:N3,M2+1:N3))
       I1=K6
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,Q71,B2,Q72)
       deallocate(B2)
C
       call
     & sum21(M2,N3,M2,N3,X17,Q72,-1.000)
       deallocate(Q72)
C
       call sumx12(N1,N3,N1,N3,
     & M2,N3,M2,N3,X17,FAPP, 1.000)
C
       allocate(F2(M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M1+1:N1))
       call reorder123456(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,t3A,F2)
       allocate(Z17(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K7*K5*K9*K9
       I3=K6
       call EGEMM(I1,I2,I3,X17,F2,Z17)
       deallocate(F2)
C
       call
     & sum234561(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z17, 1.000)
       deallocate(Z17)
       deallocate(X17)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q73(N0+1:N1,N1+1:M2))
       I1=K9*K1
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q73)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(Q74(M2+1:N3,N1+1:M2))
       I1=K9
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,Q73,B2,Q74)
       deallocate(B2)
C
       call
     & sum21(N1,M2,M2,N3,X18,Q74,-1.000)
       deallocate(Q74)
C
       call sumx12(N1,N3,N1,N3,
     & N1,M2,M2,N3,X18,FAPP, 1.000)
C
       allocate(F2(N1+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M1+1:N1))
       call reorder123456(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N1,M2,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,t3A,F2)
       allocate(Z18(N1+1:M2,N1+1:M2,N0+1:M1,M1+1:N1,M1+1:N1,M2+1:N3))
       I1=K6
       I2=K7*K7*K5*K9*K9
       I3=K9
       call EGEMM(I1,I2,I3,X18,F2,Z18)
       deallocate(F2)
C
       call
     & sum234561(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z18, 1.000)
       deallocate(Z18)
       deallocate(X18)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(Q78(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,Q73,B2,Q78)
       deallocate(B2)
C
       call
     & sum21(N1,M2,N1,M2,X22,Q78,-1.000)
       deallocate(Q78)
C
       call sumx12(N1,N3,N1,N3,
     & N1,M2,N1,M2,X22,FAPP, 1.000)
C
       allocate(F2(N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,M1+1:N1))
       call reorder213456(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N1,M2,M2,N3,N1,M2,N0,M1,M1,N1,M1,N1,t3A,F2)
       allocate(Z22(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K5*K9*K6
       I3=K9
       call EGEMM(I1,I2,I3,X22,F2,Z22)
       deallocate(F2)
C
       call
     & sum124563(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z22,-1.000)
       deallocate(Z22)
       deallocate(X22)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(Q76(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,Q73,B2,Q76)
       deallocate(B2)
C
       call
     & sum21(N1,M2,N1,M2,X20,Q76,-1.000)
       deallocate(Q76)
C
       call sumx12(N1,N3,N1,N3,
     & N1,M2,N1,M2,X20,FAPP, 1.000)
C
       allocate(F2(N1+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,M1+1:N1))
       call reorder213456(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & N1,M2,M2,N3,N1,M2,N0,M1,M1,N1,M1,N1,t3A,F2)
       allocate(Z20(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,M1+1:N1,N1+1:M2))
       I1=K9
       I2=K7*K7*K5*K9*K6
       I3=K9
       call EGEMM(I1,I2,I3,X20,F2,Z20)
       deallocate(F2)
C
       call
     & sum134562(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z20, 1.000)
       deallocate(Z20)
       deallocate(X20)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,N1,N0,N1,N1,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S611(M1+1:N1,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S611)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M1+1:N1))
       call reorder2341(M1,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N0,N1,N1,N3,M1,N1,S611,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,M2,N3,N1,M2,t2A,D2)
       allocate(S613(M2+1:N3,N1+1:M2,N1+1:N3,M1+1:N1))
       I1=K7*K3
       I2=K9*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S613)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N1,N3,M2,N3,N1,M2,M1,N1,X8,S613,-0.500)
       deallocate(S613)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M1+1:N1))
       call reorder2341(M1,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N0,N1,N1,N3,M1,N1,S611,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,M2,N3,N1,M2,t2A,D2)
       allocate(S614(M2+1:N3,N1+1:M2,N1+1:N3,M1+1:N1))
       I1=K7*K3
       I2=K9*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S614)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N1,N3,M2,N3,N1,M2,M1,N1,X7,S614,-0.500)
       deallocate(S614)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M1+1:N1))
       call reorder3421(M1,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N1,M1,N1,S611,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S615(N1+1:M2,N0+1:M1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S615)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N1,M2,N0,M1,M1,N1,X6,S615, 1.000)
       deallocate(S615)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M1+1:N1))
       call reorder2431(M1,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N1,M1,N1,S611,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S616(N1+1:M2,N0+1:M1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S616)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N1,M2,N0,M1,M1,N1,X5,S616,-1.000)
       deallocate(S616)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M1+1:N1))
       call reorder2431(M1,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N1,M1,N1,S611,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N1,t2A,D2)
       allocate(S617(M2+1:N3,M1+1:N1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S617)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,M2,N3,M1,N1,M1,N1,X76,S617,-1.000)
       deallocate(S617)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M1+1:N1))
       call reorder2431(M1,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N1,M1,N1,S611,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2A,D2)
       allocate(S618(M2+1:N3,N0+1:M1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S618)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,M2,N3,N0,M1,M1,N1,X4,S618,-1.000)
       deallocate(S618)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M1+1:N1))
       call reorder2431(M1,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N1,M1,N1,S611,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S619(N1+1:M2,M1+1:N1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S619)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N1,M2,M1,N1,M1,N1,X77,S619,-1.000)
       deallocate(S619)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M1+1:N1))
       call reorder3421(M1,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N1,M1,N1,S611,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S620(N1+1:M2,M1+1:N1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S620)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N1,M2,M1,N1,M1,N1,X78,S620, 1.000)
       deallocate(S620)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,M1+1:N1))
       call reorder4231(M1,N1,N0,N1,N0,N1,N1,N3,
     & N1,N3,N0,N1,N0,N1,M1,N1,S611,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S695(M1+1:N1,N0+1:N1,N0+1:N1,M1+1:N1))
       I1=K7*K1*K1
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S695)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N1,M1+1:N1,M1+1:N1))
       call reorder2314(M1,N1,N0,N1,N0,N1,M1,N1,
     & N0,N1,N0,N1,M1,N1,M1,N1,S695,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S696(M2+1:N3,N0+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S696)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,M2,N3,M1,N1,M1,N1,X1,S696, 1.000)
       deallocate(S696)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,M1+1:N1))
       call reorder4231(M1,N1,N0,N1,N0,N1,N1,N3,
     & N1,N3,N0,N1,N0,N1,M1,N1,S611,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S699(N0+1:M1,N0+1:N1,N0+1:N1,M1+1:N1))
       I1=K7*K1*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S699)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder2314(N0,M1,N0,N1,N0,N1,M1,N1,
     & N0,N1,N0,N1,N0,M1,M1,N1,S699,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S700(M2+1:N3,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S700)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,M2,N3,N0,M1,M1,N1,X4,S700, 1.000)
       deallocate(S700)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M1+1:N1))
       call reorder3241(M1,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N0,N1,N1,N3,M1,N1,S611,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S703(N1+1:M2,N0+1:N1,N1+1:N3,M1+1:N1))
       I1=K7*K3*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S703)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder2314(N1,M2,N0,N1,N1,N3,M1,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,S703,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S704(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S704)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,N1,M2,M1,N1,X7,S704,-1.000)
       deallocate(S704)
C
       call sumx2314(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,M2,N3,N1,M2,M1,N1,X7,VAHPPP, 1.000)
C
       allocate(D2(N1+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,M2,N0,M1,M1,N1,t2A,D2)
       allocate(Z7(N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K6
       I2=K7*K5*K9
       I3=K3
       call EGEMM(I1,I2,I3,X7,D2,Z7)
       deallocate(D2)
C
       call
     & sum345126(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z7,-1.000)
       call
     & sum346125(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z7, 1.000)
       deallocate(Z7)
       deallocate(X7)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M1+1:N1))
       call reorder3241(M1,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N0,N1,N1,N3,M1,N1,S611,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S705(N1+1:M2,N0+1:N1,N1+1:N3,M1+1:N1))
       I1=K7*K3*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S705)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder2314(N1,M2,N0,N1,N1,N3,M1,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,S705,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S706(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S706)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,N1,M2,M1,N1,X8,S706,-1.000)
       deallocate(S706)
C
       call sumx2314(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,M2,N3,N1,M2,M1,N1,X8,VAHPPP, 1.000)
C
       allocate(D2(N1+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,M2,N0,M1,M1,N1,t2A,D2)
       allocate(Z8(N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K6
       I2=K7*K5*K9
       I3=K3
       call EGEMM(I1,I2,I3,X8,D2,Z8)
       deallocate(D2)
C
       call
     & sum245136(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z8, 1.000)
       call
     & sum246135(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z8,-1.000)
       deallocate(Z8)
       deallocate(X8)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M1+1:N1))
       call reorder2341(M1,N1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N0,N1,N1,N3,M1,N1,S611,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:M2,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,M2,N1,M2,t2A,D2)
       allocate(S612(N1+1:M2,N1+1:M2,N1+1:N3,M1+1:N1))
       I1=K7*K3
       I2=K9*K9
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S612)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N1,N3,N1,M2,N1,M2,M1,N1,X9,S612,-0.500)
       deallocate(S612)
C
       allocate(D1(N0+1:N1,N0+1:N1,M1+1:N1,M1+1:N1))
       call reorder2314(M1,N1,N0,N1,N0,N1,M1,N1,
     & N0,N1,N0,N1,M1,N1,M1,N1,S695,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S697(N1+1:M2,N0+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S697)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N1,M2,M1,N1,M1,N1,X2,S697, 1.000)
       deallocate(S697)
C
       allocate(D1(N0+1:N1,N0+1:N1,M1+1:N1,M1+1:N1))
       call reorder3214(M1,N1,N0,N1,N0,N1,M1,N1,
     & N0,N1,N0,N1,M1,N1,M1,N1,S695,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S698(N1+1:M2,N0+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S698)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N1,M2,M1,N1,M1,N1,X3,S698,-1.000)
       deallocate(S698)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder2314(N0,M1,N0,N1,N0,N1,M1,N1,
     & N0,N1,N0,N1,N0,M1,M1,N1,S699,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S701(N1+1:M2,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S701)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N1,M2,N0,M1,M1,N1,X5,S701, 1.000)
       deallocate(S701)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder3214(N0,M1,N0,N1,N0,N1,M1,N1,
     & N0,N1,N0,N1,N0,M1,M1,N1,S699,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S702(N1+1:M2,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S702)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N1,M2,N0,M1,M1,N1,X6,S702,-1.000)
       deallocate(S702)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder2314(N1,M2,N0,N1,N1,N3,M1,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,S705,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S707(N1+1:M2,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S707)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,N1,M2,N1,M2,M1,N1,X9,S707,-1.000)
       deallocate(S707)
C
       call sumx2314(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N1,M2,N1,M2,M1,N1,X9,VAHPPP, 1.000)
C
       allocate(D2(N1+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,M2,N3,N0,M1,M1,N1,t2A,D2)
       allocate(Z9(M2+1:N3,N0+1:M1,M1+1:N1,N1+1:M2,N1+1:M2,M1+1:N1))
       I1=K7*K9*K9
       I2=K7*K5*K6
       I3=K3
       call EGEMM(I1,I2,I3,X9,D2,Z9)
       deallocate(D2)
C
       call
     & sum145236(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z9,-1.000)
       call
     & sum146235(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z9, 1.000)
       deallocate(Z9)
       deallocate(X9)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder4312(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S634(N1+1:M2,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S634)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder2431(N1,M2,N0,N1,N1,N3,N1,N3,
     & N0,N1,N1,N3,N1,N3,N1,M2,S634,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S651(N1+1:M2,M1+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S651)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N1,N3,N1,M2,N1,M2,M1,N1,X87,S651,-1.000)
       deallocate(S651)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder2341(N1,M2,N0,N1,N1,N3,N1,N3,
     & N0,N1,N1,N3,N1,N3,N1,M2,S634,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N1,t2A,D2)
       allocate(S654(M2+1:N3,M1+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S654)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N1,N3,M2,N3,N1,M2,M1,N1,X91,S654,-1.000)
       deallocate(S654)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:M2))
       call reorder3421(N1,M2,N0,N1,N1,N3,N1,N3,
     & N1,N3,N1,N3,N0,N1,N1,M2,S634,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,M1,M1,N1,t2A,D2)
       allocate(S655(N0+1:M1,M1+1:N1,N0+1:N1,N1+1:M2))
       I1=K9*K1
       I2=K7*K5
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S655)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,N1,N1,M2,N0,M1,M1,N1,X5,S655, 0.500)
       deallocate(S655)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder2341(N1,M2,N0,N1,N1,N3,N1,N3,
     & N0,N1,N1,N3,N1,N3,N1,M2,S634,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2A,D2)
       allocate(S635(M2+1:N3,N0+1:M1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S635)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N1,N3,M2,N3,N1,M2,N0,M1,X10,S635, 1.000)
       deallocate(S635)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,VAHHPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,M1+1:N1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,M1,N1,M1,N1,t2A,D2)
       allocate(S629(M1+1:N1,M1+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K7*K7
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S629)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N0+1:N1,M1+1:N1,M1+1:N1))
       call reorder3412(M1,N1,M1,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,M1,N1,M1,N1,S629,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S636(N1+1:M2,N0+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S636)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N1,M2,M1,N1,M1,N1,X2,S636,-0.500)
       deallocate(S636)
C
       allocate(D1(N0+1:N1,N0+1:N1,M1+1:N1,M1+1:N1))
       call reorder3412(M1,N1,M1,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,M1,N1,M1,N1,S629,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S630(M2+1:N3,N0+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S630)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,M2,N3,M1,N1,M1,N1,X1,S630,-0.500)
       deallocate(S630)
C
       call sumx2134(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,M2,N3,M1,N1,M1,N1,X1,VAHHHP, 1.000)
C
       allocate(D2(N0+1:N1,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,M2,N1,M2,N0,M1,t2A,D2)
       allocate(Z1(N1+1:M2,N1+1:M2,N0+1:M1,M2+1:N3,M1+1:N1,M1+1:N1))
       I1=K7*K7*K6
       I2=K5*K9*K9
       I3=K1
       call EGEMM(I1,I2,I3,X1,D2,Z1)
       deallocate(D2)
C
       call
     & sum234156(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z1,-1.000)
       deallocate(Z1)
       deallocate(X1)
C
       allocate(D1(N0+1:N1,N0+1:N1,M1+1:N1,M1+1:N1))
       call reorder4312(M1,N1,M1,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,M1,N1,M1,N1,S629,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S640(N1+1:M2,N0+1:N1,M1+1:N1,M1+1:N1))
       I1=K7*K7*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S640)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N1,M2,M1,N1,M1,N1,X3,S640, 0.500)
       deallocate(S640)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       call reorder1342(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,N1,N0,N1,N1,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S621(N0+1:M1,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1*K1
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S621)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder2431(N0,M1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N1,N0,M1,S621,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N1,t2A,D2)
       allocate(S644(M2+1:N3,M1+1:N1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S644)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N1,M2,N3,N0,M1,M1,N1,X4,S644, 1.000)
       deallocate(S644)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N0,N1,N1,N3,N0,M1,S621,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:M2,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,M2,N1,M2,t2A,D2)
       allocate(S622(N1+1:M2,N1+1:M2,N1+1:N3,N0+1:M1))
       I1=K5*K3
       I2=K9*K9
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S622)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N1,N3,N1,M2,N1,M2,N0,M1,X12,S622,-0.500)
       deallocate(S622)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N0,N1,N1,N3,N0,M1,S621,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,M2,N3,N1,M2,t2A,D2)
       allocate(S623(M2+1:N3,N1+1:M2,N1+1:N3,N0+1:M1))
       I1=K5*K3
       I2=K9*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S623)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N1,N3,M2,N3,N1,M2,N0,M1,X11,S623,-0.500)
       deallocate(S623)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N0,N1,N1,N3,N0,M1,S621,D1)
       allocate(D2(N0+1:N1,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,M2,N3,N1,M2,t2A,D2)
       allocate(S624(M2+1:N3,N1+1:M2,N1+1:N3,N0+1:M1))
       I1=K5*K3
       I2=K9*K6
       I3=K1*K1
       call EGEMM(I1,I2,I3,D1,D2,S624)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N1,N3,M2,N3,N1,M2,N0,M1,X10,S624,-0.500)
       deallocate(S624)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder3421(N0,M1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N1,N0,M1,S621,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S642(N1+1:M2,M1+1:N1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S642)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N1,N1,M2,N0,M1,M1,N1,X88,S642,-1.000)
       deallocate(S642)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:M1))
       call reorder3241(N0,M1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N0,N1,N1,N3,N0,M1,S621,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S708(N1+1:M2,N0+1:N1,N1+1:N3,N0+1:M1))
       I1=K5*K3*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S708)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder2314(N1,M2,N0,N1,N1,N3,N0,M1,
     & N0,N1,N1,N3,N1,M2,N0,M1,S708,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S709(M2+1:N3,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S709)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,N1,M2,N0,M1,X10,S709,-1.000)
       deallocate(S709)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N0+1:M1))
       call reorder3241(N0,M1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N0,N1,N1,N3,N0,M1,S621,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S710(N1+1:M2,N0+1:N1,N1+1:N3,N0+1:M1))
       I1=K5*K3*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S710)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder2314(N1,M2,N0,N1,N1,N3,N0,M1,
     & N0,N1,N1,N3,N1,M2,N0,M1,S710,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S711(M2+1:N3,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S711)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,N1,M2,N0,M1,X11,S711,-1.000)
       deallocate(S711)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder2431(N0,M1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N1,N0,M1,S621,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S643(N1+1:M2,M1+1:N1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S643)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N1,N1,M2,N0,M1,M1,N1,X89,S643,-1.000)
       deallocate(S643)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder2314(N1,M2,N0,N1,N1,N3,N0,M1,
     & N0,N1,N1,N3,N1,M2,N0,M1,S710,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S712(N1+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S712)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,N1,M2,N1,M2,N0,M1,X12,S712,-1.000)
       deallocate(S712)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N1,N3,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q79(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K3*K1
       call EGEMM1(I1,I3,D1,B2,Q79)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q79,B1)
       allocate(D2(N1+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,M2,N0,M1,M1,N1,t2A,D2)
       allocate(S657(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S657)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X5,S657,-1.000)
       deallocate(S657)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q79,B1)
       allocate(D2(N1+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,M2,N0,M1,M1,N1,t2A,D2)
       allocate(S659(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S659)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X6,S659,-1.000)
       deallocate(S659)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q79,B1)
       allocate(D2(N1+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,M2,M1,N1,M1,N1,t2A,D2)
       allocate(S641(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S641)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X3,S641,-1.000)
       deallocate(S641)
C
       allocate(D2(N0+1:N1,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,M2,N1,M2,N0,M1,t2A,D2)
       allocate(S633(N1+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K9
       I3=K1
       call EGEMM(I1,I2,I3,Q79,D2,S633)
       deallocate(D2)
C
       call
     & sum2341(N1,N3,N1,M2,N1,M2,N0,M1,X12,S633,-1.000)
       deallocate(S633)
C
       allocate(D2(N0+1:N1,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,M2,N1,M2,M1,N1,t2A,D2)
       allocate(S653(N1+1:M2,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K9
       I3=K1
       call EGEMM(I1,I2,I3,Q79,D2,S653)
       deallocate(D2)
C
       call
     & sum2341(N1,N3,N1,M2,N1,M2,M1,N1,X87,S653, 1.000)
       deallocate(S653)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q79,B1)
       allocate(D2(N1+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,M2,M1,N1,M1,N1,t2A,D2)
       allocate(S639(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S639)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X2,S639,-1.000)
       deallocate(S639)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,N2,N0,N1,N2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,M1+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,M1,N1,t1A,B2)
       allocate(S660(M1+1:N1,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4*K1*K2
       I2=K7
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S660)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M1+1:N1))
       call reorder2431(M1,N1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N2,N3,N0,N1,M1,N1,S660,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S663(M2+1:N3,N0+1:M1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S663)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,M2,N3,N0,M1,M1,N1,X4,S663, 1.000)
       deallocate(S663)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M1+1:N1))
       call reorder2431(M1,N1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N2,N3,N0,N1,M1,N1,S660,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S664(N1+1:M2,M1+1:N1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S664)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N1,M2,M1,N1,M1,N1,X78,S664, 1.000)
       deallocate(S664)
C
       allocate(D2(N0+1:N1,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,M2,N3,N1,M2,N0,M1,t2A,D2)
       allocate(Z89(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:M2,M1+1:N1,M1+1:N1))
       I1=K7*K7*K9
       I2=K5*K9*K6
       I3=K1
       call EGEMM(I1,I2,I3,X78,D2,Z89)
       deallocate(D2)
C
       call
     & sum124356(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z89,-1.000)
       call
     & sum124365(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z89, 1.000)
       deallocate(Z89)
       deallocate(X78)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M1+1:N1))
       call reorder2431(M1,N1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N2,N3,N0,N1,M1,N1,S660,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S665(N1+1:M2,M1+1:N1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S665)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N1,M2,M1,N1,M1,N1,X77,S665, 1.000)
       deallocate(S665)
C
       allocate(D2(N0+1:N1,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,M2,N3,N1,M2,N0,M1,t2A,D2)
       allocate(Z88(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:M2,M1+1:N1,M1+1:N1))
       I1=K7*K7*K9
       I2=K5*K9*K6
       I3=K1
       call EGEMM(I1,I2,I3,X77,D2,Z88)
       deallocate(D2)
C
       call
     & sum134256(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z88, 1.000)
       call
     & sum134265(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z88,-1.000)
       deallocate(Z88)
       deallocate(X77)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M1+1:N1))
       call reorder2431(M1,N1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N2,N3,N0,N1,M1,N1,S660,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,M1,N1,t2B,D2)
       allocate(S666(M2+1:N3,M1+1:N1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S666)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,M2,N3,M1,N1,M1,N1,X76,S666, 1.000)
       deallocate(S666)
C
       allocate(D2(N0+1:N1,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,M2,N1,M2,N0,M1,t2A,D2)
       allocate(Z87(N1+1:M2,N1+1:M2,N0+1:M1,M2+1:N3,M1+1:N1,M1+1:N1))
       I1=K7*K7*K6
       I2=K5*K9*K9
       I3=K1
       call EGEMM(I1,I2,I3,X76,D2,Z87)
       deallocate(D2)
C
       call
     & sum234156(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z87,-1.000)
       call
     & sum234165(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z87, 1.000)
       deallocate(Z87)
       deallocate(X76)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M1+1:N1))
       call reorder2431(M1,N1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N2,N3,N0,N1,M1,N1,S660,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S661(N1+1:M2,N0+1:M1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S661)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N1,M2,N0,M1,M1,N1,X6,S661, 1.000)
       deallocate(S661)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M1+1:N1))
       call reorder2431(M1,N1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N2,N3,N0,N1,M1,N1,S660,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S662(N1+1:M2,N0+1:M1,N0+1:N1,M1+1:N1))
       I1=K7*K1
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S662)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N1,M2,N0,M1,M1,N1,X5,S662, 1.000)
       deallocate(S662)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N1,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S669(N1+1:M2,N0+1:M1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S669)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3412(N1,M2,N0,M1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N1,M2,N0,M1,S669,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S672(N1+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S672)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,N3,N1,M2,N1,M2,N0,M1,X12,S672, 1.000)
       deallocate(S672)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3412(N1,M2,N0,M1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N1,M2,N0,M1,S669,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S670(M2+1:N3,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S670)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,N1,M2,N0,M1,X10,S670,-1.000)
       deallocate(S670)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N1,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S667(N1+1:M2,N0+1:M1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K5*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S667)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3412(N1,M2,N0,M1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N1,M2,N0,M1,S667,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S668(M2+1:N3,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S668)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,N1,M2,N0,M1,X11,S668,-1.000)
       deallocate(S668)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3412(N1,M2,N0,M1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N1,M2,N0,M1,S667,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S671(N1+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S671)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,N1,M2,N1,M2,N0,M1,X12,S671,-1.000)
       deallocate(S671)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:N3,N1+1:N3))
       call reorder4312(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N0,N2,N2,N3,N1,N3,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S675(N1+1:M2,N0+1:N2,N2+1:N3,N1+1:N3))
       I1=K3*K4*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S675)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder2341(N1,M2,N0,N2,N2,N3,N1,N3,
     & N0,N2,N2,N3,N1,N3,N1,M2,S675,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S686(N1+1:M2,M1+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S686)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N1,N3,N1,M2,N1,M2,M1,N1,X87,S686,-1.000)
       deallocate(S686)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder2341(N1,M2,N0,N2,N2,N3,N1,N3,
     & N0,N2,N2,N3,N1,N3,N1,M2,S675,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,M1,N1,t2B,D2)
       allocate(S688(M2+1:N3,M1+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S688)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N1,N3,M2,N3,N1,M2,M1,N1,X90,S688, 1.000)
       deallocate(S688)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder2341(N1,M2,N0,N2,N2,N3,N1,N3,
     & N0,N2,N2,N3,N1,N3,N1,M2,S675,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S676(M2+1:N3,N0+1:M1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S676)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N1,N3,M2,N3,N1,M2,N0,M1,X11,S676, 1.000)
       deallocate(S676)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,N2,N0,N1,N2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S677(N0+1:M1,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4*K1*K2
       I2=K5
       I3=K3
       call EGEMM(I1,I2,I3,D1,B2,S677)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder2431(N0,M1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N2,N3,N0,N1,N0,M1,S677,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S678(N1+1:M2,M1+1:N1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S678)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N1,N1,M2,N0,M1,M1,N1,X88,S678,-1.000)
       deallocate(S678)
C
       allocate(D2(N0+1:N1,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,M2,N3,N1,M2,M1,N1,t2A,D2)
       allocate(Z294(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:M2,N0+1:M1,M1+1:N1))
       I1=K7*K5*K9
       I2=K7*K9*K6
       I3=K1
       call EGEMM(I1,I2,I3,X88,D2,Z294)
       deallocate(D2)
C
       call
     & sum126345(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z294,-1.000)
       call
     & sum125346(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z294, 1.000)
       deallocate(Z294)
       deallocate(X88)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder2431(N0,M1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N2,N3,N0,N1,N0,M1,S677,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S679(N1+1:M2,M1+1:N1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S679)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N1,N1,M2,N0,M1,M1,N1,X89,S679, 1.000)
       deallocate(S679)
C
       allocate(D2(N0+1:N1,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,M2,N3,N1,M2,M1,N1,t2A,D2)
       allocate(Z295(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:M2,N0+1:M1,M1+1:N1))
       I1=K7*K5*K9
       I2=K7*K9*K6
       I3=K1
       call EGEMM(I1,I2,I3,X89,D2,Z295)
       deallocate(D2)
C
       call
     & sum136245(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z295,-1.000)
       call
     & sum135246(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z295, 1.000)
       deallocate(Z295)
       deallocate(X89)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder2431(N0,M1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N2,N3,N0,N1,N0,M1,S677,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,M1,N1,t2B,D2)
       allocate(S680(M2+1:N3,M1+1:N1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S680)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N1,M2,N3,N0,M1,M1,N1,X92,S680, 1.000)
       deallocate(S680)
C
       allocate(D2(N0+1:N1,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,M2,N1,M2,M1,N1,t2A,D2)
       allocate(Z320(N1+1:M2,N1+1:M2,M1+1:N1,M2+1:N3,N0+1:M1,M1+1:N1))
       I1=K7*K5*K6
       I2=K7*K9*K9
       I3=K1
       call EGEMM(I1,I2,I3,X92,D2,Z320)
       deallocate(D2)
C
       call
     & sum236145(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z320, 1.000)
       call
     & sum235146(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z320,-1.000)
       deallocate(Z320)
       deallocate(X92)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:N3,N1+1:N3))
       call reorder4312(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N0,N2,N2,N3,N1,N3,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S673(N1+1:M2,N0+1:N2,N2+1:N3,N1+1:N3))
       I1=K3*K4*K2
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S673)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder2341(N1,M2,N0,N2,N2,N3,N1,N3,
     & N0,N2,N2,N3,N1,N3,N1,M2,S673,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,M1,N1,t2B,D2)
       allocate(S687(M2+1:N3,M1+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K7*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S687)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N1,N3,M2,N3,N1,M2,M1,N1,X91,S687,-1.000)
       deallocate(S687)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder2341(N1,M2,N0,N2,N2,N3,N1,N3,
     & N0,N2,N2,N3,N1,N3,N1,M2,S673,D1)
       allocate(D2(N0+1:N2,N2+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M2,N3,N0,M1,t2B,D2)
       allocate(S674(M2+1:N3,N0+1:M1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K5*K6
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S674)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N1,N3,M2,N3,N1,M2,N0,M1,X10,S674, 1.000)
       deallocate(S674)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder2341(N1,M2,N0,N2,N2,N3,N1,N3,
     & N0,N2,N2,N3,N1,N3,N1,M2,S673,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S685(N1+1:M2,M1+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S685)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N1,N3,N1,M2,N1,M2,M1,N1,X87,S685, 1.000)
       deallocate(S685)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N1,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S683(N1+1:M2,M1+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S683)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3412(N1,M2,M1,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N1,M2,M1,N1,S683,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S684(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S684)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,N1,M2,M1,N1,X91,S684, 1.000)
       deallocate(S684)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N1,N3,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q80(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K4*K2
       call EGEMM1(I1,I3,D1,B2,Q80)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q80,B1)
       allocate(D2(N1+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,M2,M1,N1,M1,N1,t2A,D2)
       allocate(S690(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S690)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X2,S690,-1.000)
       deallocate(S690)
C
       call sumx2134(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,M2,M1,N1,M1,N1,X2,VAHHHP, 1.000)
C
       allocate(D2(N0+1:N1,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,M2,N3,N1,M2,N0,M1,t2A,D2)
       allocate(Z2(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:M2,M1+1:N1,M1+1:N1))
       I1=K7*K7*K9
       I2=K5*K9*K6
       I3=K1
       call EGEMM(I1,I2,I3,X2,D2,Z2)
       deallocate(D2)
C
       call
     & sum134256(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z2, 1.000)
       deallocate(Z2)
       deallocate(X2)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q80,B1)
       allocate(D2(N1+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,M2,M1,N1,M1,N1,t2A,D2)
       allocate(S691(N1+1:M2,M1+1:N1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K7*K9
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S691)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,N1,M2,M1,N1,M1,N1,X3,S691,-1.000)
       deallocate(S691)
C
       call sumx2134(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,M2,M1,N1,M1,N1,X3,VAHHHP, 1.000)
C
       allocate(D2(N0+1:N1,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,M2,N3,N1,M2,N0,M1,t2A,D2)
       allocate(Z3(M2+1:N3,N1+1:M2,N0+1:M1,N1+1:M2,M1+1:N1,M1+1:N1))
       I1=K7*K7*K9
       I2=K5*K9*K6
       I3=K1
       call EGEMM(I1,I2,I3,X3,D2,Z3)
       deallocate(D2)
C
       call
     & sum124356(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z3,-1.000)
       deallocate(Z3)
       deallocate(X3)
C
       allocate(D2(N0+1:N1,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,M2,N1,M2,M1,N1,t2A,D2)
       allocate(S692(N1+1:M2,N1+1:M2,M1+1:N1,N1+1:N3))
       I1=K3
       I2=K7*K9*K9
       I3=K1
       call EGEMM(I1,I2,I3,Q80,D2,S692)
       deallocate(D2)
C
       call
     & sum2341(N1,N3,N1,M2,N1,M2,M1,N1,X87,S692, 1.000)
       deallocate(S692)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q80,B1)
       allocate(D2(N1+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,M2,N0,M1,M1,N1,t2A,D2)
       allocate(S693(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S693)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X5,S693,-1.000)
       deallocate(S693)
C
       call sumx2134(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,M2,N0,M1,M1,N1,X5,VAHHHP, 1.000)
C
       allocate(D2(N0+1:N1,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,M2,N3,N1,M2,M1,N1,t2A,D2)
       allocate(Z5(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:M2,N0+1:M1,M1+1:N1))
       I1=K7*K5*K9
       I2=K7*K9*K6
       I3=K1
       call EGEMM(I1,I2,I3,X5,D2,Z5)
       deallocate(D2)
C
       call
     & sum135246(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z5,-1.000)
       call
     & sum136245(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z5, 1.000)
       deallocate(Z5)
       deallocate(X5)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q80,B1)
       allocate(D2(N1+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,M2,N0,M1,M1,N1,t2A,D2)
       allocate(S694(N1+1:M2,N0+1:M1,M1+1:N1,N0+1:N1))
       I1=K1
       I2=K7*K5*K9
       I3=K3
       call EGEMM(I1,I2,I3,B1,D2,S694)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,N1,M2,N0,M1,M1,N1,X6,S694,-1.000)
       deallocate(S694)
C
       allocate(D2(N0+1:N1,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,M2,N1,M2,N0,M1,t2A,D2)
       allocate(S689(N1+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K9
       I3=K1
       call EGEMM(I1,I2,I3,Q80,D2,S689)
       deallocate(D2)
C
       call
     & sum2341(N1,N3,N1,M2,N1,M2,N0,M1,X12,S689,-1.000)
       deallocate(S689)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N1,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S645(N1+1:M2,M1+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S645)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3412(N1,M2,M1,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N1,M2,M1,N1,S645,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S646(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S646)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,N1,M2,M1,N1,X90,S646,-1.000)
       deallocate(S646)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder4132(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N1,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S647(N1+1:M2,M1+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S647)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3412(N1,M2,M1,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N1,M2,M1,N1,S647,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S648(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S648)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,N1,M2,M1,N1,X91,S648,-1.000)
       deallocate(S648)
C
       allocate(D2(N1+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,M2,N0,M1,M1,N1,t2A,D2)
       allocate(Z298(N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K6
       I2=K7*K5*K9
       I3=K3
       call EGEMM(I1,I2,I3,X91,D2,Z298)
       deallocate(D2)
C
       call
     & sum346125(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z298,-1.000)
       call
     & sum345126(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z298, 1.000)
       deallocate(Z298)
       deallocate(X91)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,N1,N0,N1,VAHHPP,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,M1,M1,N1,t2A,D2)
       allocate(S649(N0+1:M1,M1+1:N1,N0+1:N1,N0+1:N1))
       I1=K1*K1
       I2=K7*K5
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S649)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N0+1:N1,N0+1:M1,M1+1:N1))
       call reorder3412(N0,M1,M1,N1,N0,N1,N0,N1,
     & N0,N1,N0,N1,N0,M1,M1,N1,S649,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S650(M2+1:N3,N0+1:N1,N0+1:M1,M1+1:N1))
       I1=K7*K5*K1
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S650)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,M2,N3,N0,M1,M1,N1,X4,S650,-0.500)
       deallocate(S650)
C
       call sumx2134(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,M2,N3,N0,M1,M1,N1,X4,VAHHHP, 1.000)
C
       allocate(D2(N0+1:N1,N1+1:M2,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,M2,N1,M2,M1,N1,t2A,D2)
       allocate(Z4(N1+1:M2,N1+1:M2,M1+1:N1,M2+1:N3,N0+1:M1,M1+1:N1))
       I1=K7*K5*K6
       I2=K7*K9*K9
       I3=K1
       call EGEMM(I1,I2,I3,X4,D2,Z4)
       deallocate(D2)
C
       call
     & sum235146(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z4, 1.000)
       call
     & sum236145(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z4,-1.000)
       deallocate(Z4)
       deallocate(X4)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N1,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S625(N1+1:M2,N0+1:M1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S625)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3412(N1,M2,N0,M1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N1,M2,N0,M1,S625,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S631(N1+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S631)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,N1,M2,N1,M2,N0,M1,X12,S631,-1.000)
       deallocate(S631)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3412(N1,M2,N0,M1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N1,M2,N0,M1,S625,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S626(M2+1:N3,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S626)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,N1,M2,N0,M1,X11,S626,-1.000)
       deallocate(S626)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:N3))
       call reorder4312(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,N3,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S637(N1+1:M2,N0+1:N1,N1+1:N3,N1+1:N3))
       I1=K3*K3*K1
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S637)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder2341(N1,M2,N0,N1,N1,N3,N1,N3,
     & N0,N1,N1,N3,N1,N3,N1,M2,S637,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,M1,N1,t2A,D2)
       allocate(S656(M2+1:N3,M1+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K7*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S656)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N1,N3,M2,N3,N1,M2,M1,N1,X90,S656, 1.000)
       deallocate(S656)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder2431(N1,M2,N0,N1,N1,N3,N1,N3,
     & N0,N1,N1,N3,N1,N3,N1,M2,S637,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,M1,N1,t2A,D2)
       allocate(S652(N1+1:M2,M1+1:N1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K7*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S652)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N1,N3,N1,M2,N1,M2,M1,N1,X87,S652, 1.000)
       deallocate(S652)
C
       allocate(D2(N1+1:N3,M2+1:N3,N0+1:M1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,M2,N3,N0,M1,M1,N1,t2A,D2)
       allocate(Z270(M2+1:N3,N0+1:M1,M1+1:N1,N1+1:M2,N1+1:M2,M1+1:N1))
       I1=K7*K9*K9
       I2=K7*K5*K6
       I3=K3
       call EGEMM(I1,I2,I3,X87,D2,Z270)
       deallocate(D2)
C
       call
     & sum146235(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z270,-1.000)
       call
     & sum145236(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z270, 1.000)
       deallocate(Z270)
       deallocate(X87)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder2341(N1,M2,N0,N1,N1,N3,N1,N3,
     & N0,N1,N1,N3,N1,N3,N1,M2,S637,D1)
       allocate(D2(N0+1:N1,N1+1:N3,M2+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M2,N3,N0,M1,t2A,D2)
       allocate(S638(M2+1:N3,N0+1:M1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K5*K6
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S638)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N1,N3,M2,N3,N1,M2,N0,M1,X11,S638, 1.000)
       deallocate(S638)
C
       call sumx2314(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,M2,N3,N1,M2,N0,M1,X11,VAHPPP, 1.000)
C
       allocate(D2(N1+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,M2,M1,N1,M1,N1,t2A,D2)
       allocate(Z11(N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K6
       I2=K7*K7*K9
       I3=K3
       call EGEMM(I1,I2,I3,X11,D2,Z11)
       deallocate(D2)
C
       call
     & sum256134(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z11, 1.000)
       deallocate(Z11)
       deallocate(X11)
C
       allocate(D1(N1+1:N3,N1+1:N3,N0+1:N1,N1+1:M2))
       call reorder3421(N1,M2,N0,N1,N1,N3,N1,N3,
     & N1,N3,N1,N3,N0,N1,N1,M2,S637,D1)
       allocate(D2(N1+1:N3,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,N3,N0,M1,M1,N1,t2A,D2)
       allocate(S658(N0+1:M1,M1+1:N1,N0+1:N1,N1+1:M2))
       I1=K9*K1
       I2=K7*K5
       I3=K3*K3
       call EGEMM(I1,I2,I3,D1,D2,S658)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,N1,N1,M2,N0,M1,M1,N1,X6,S658, 0.500)
       deallocate(S658)
C
       call sumx2134(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,M2,N0,M1,M1,N1,X6,VAHHHP, 1.000)
C
       allocate(D2(N0+1:N1,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,M2,N3,N1,M2,M1,N1,t2A,D2)
       allocate(Z6(M2+1:N3,N1+1:M2,M1+1:N1,N1+1:M2,N0+1:M1,M1+1:N1))
       I1=K7*K5*K9
       I2=K7*K9*K6
       I3=K1
       call EGEMM(I1,I2,I3,X6,D2,Z6)
       deallocate(D2)
C
       call
     & sum125346(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z6, 1.000)
       call
     & sum126345(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z6,-1.000)
       deallocate(Z6)
       deallocate(X6)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder4132(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N1,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S627(N1+1:M2,N0+1:M1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K5*K9
       I3=K3*K1
       call EGEMM(I1,I2,I3,D1,D2,S627)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3412(N1,M2,N0,M1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N1,M2,N0,M1,S627,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S628(M2+1:N3,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S628)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,M2,N3,N1,M2,N0,M1,X10,S628, 1.000)
       deallocate(S628)
C
       call sumx2314(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,M2,N3,N1,M2,N0,M1,X10,VAHPPP, 1.000)
C
       allocate(D2(N1+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,M2,M1,N1,M1,N1,t2A,D2)
       allocate(Z10(N1+1:M2,M1+1:N1,M1+1:N1,M2+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K6
       I2=K7*K7*K9
       I3=K3
       call EGEMM(I1,I2,I3,X10,D2,Z10)
       deallocate(D2)
C
       call
     & sum356124(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z10,-1.000)
       deallocate(Z10)
       deallocate(X10)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3412(N1,M2,N0,M1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N1,M2,N0,M1,S627,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S632(N1+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K9
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S632)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,N3,N1,M2,N1,M2,N0,M1,X12,S632,-1.000)
       deallocate(S632)
C
       call sumx2314(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N1,M2,N1,M2,N0,M1,X12,VAHPPP, 1.000)
C
       allocate(D2(N1+1:N3,M2+1:N3,M1+1:N1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,M2,N3,M1,N1,M1,N1,t2A,D2)
       allocate(Z12(M2+1:N3,M1+1:N1,M1+1:N1,N1+1:M2,N1+1:M2,N0+1:M1))
       I1=K5*K9*K9
       I2=K7*K7*K6
       I3=K3
       call EGEMM(I1,I2,I3,X12,D2,Z12)
       deallocate(D2)
C
       call
     & sum156234(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z12,-1.000)
       deallocate(Z12)
       deallocate(X12)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N1,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,M1,N1,t2B,D2)
       allocate(S681(N1+1:M2,M1+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K7*K9
       I3=K4*K2
       call EGEMM(I1,I2,I3,D1,D2,S681)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:M2,M1+1:N1))
       call reorder3412(N1,M2,M1,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N1,M2,M1,N1,S681,D1)
       allocate(B2(N0+1:N1,M2+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,M2,N3,t1A,B2)
       allocate(S682(M2+1:N3,N1+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K3
       I2=K6
       I3=K1
       call EGEMM(I1,I2,I3,D1,B2,S682)
       deallocate(D1)
       deallocate(B2)
       deallocate(S681)
C
       call
     & sum2134(N1,N3,M2,N3,N1,M2,M1,N1,X90,S682,-1.000)
       deallocate(S682)
C
       allocate(D2(N1+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder1234(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N1,M2,N0,M1,M1,N1,t2A,D2)
       allocate(Z297(N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2,M1+1:N1))
       I1=K7*K9*K6
       I2=K7*K5*K9
       I3=K3
       call EGEMM(I1,I2,I3,X90,D2,Z297)
       deallocate(D2)
C
       call
     & sum246135(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z297,-1.000)
       call
     & sum245136(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z297, 1.000)
       deallocate(Z297)
       deallocate(X90)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,M2,N3,N1,M2,M1,N1,VBHPPH,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z68(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z68)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z68, 1.000)
       call
     & sum124635(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z68,-1.000)
       deallocate(Z68)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,M1+1:N1))
       call reorder1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N2,M2,N3,N1,M2,M1,N1,VBHPPH,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z69(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z69)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z69, 1.000)
       call
     & sum124635(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z69,-1.000)
       deallocate(Z69)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,N2,M2,N1,M2,M1,N1,VBHPPH,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z70(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z70)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z70, 1.000)
       call
     & sum124635(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z70,-1.000)
       deallocate(Z70)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,M1+1:N1))
       call reorder1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N2,N2,M2,N1,M2,M1,N1,VBHPPH,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,N1,M2,N0,M1,M1,N1,t3B1,F2)
       allocate(Z71(M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2,M1+1:N1))
       I1=K7*K9
       I2=K7*K5*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z71)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z71, 1.000)
       call
     & sum124635(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z71,-1.000)
       deallocate(Z71)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,M1,VBHPPH,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z80(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K6*K5
       call EGEMM(I1,I2,I3,D1,F2,Z80)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z80, 1.000)
       deallocate(Z80)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N2,M2,N3,N1,M2,N0,M1,VBHPPH,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M2,N3,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z81(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K6*K8
       call EGEMM(I1,I2,I3,D1,F2,Z81)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z81, 1.000)
       deallocate(Z81)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,N2,M2,N1,M2,N0,M1,VBHPPH,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,N2,M2,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z82(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K0*K5
       call EGEMM(I1,I2,I3,D1,F2,Z82)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z82, 1.000)
       deallocate(Z82)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N2,N2,M2,N1,M2,N0,M1,VBHPPH,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1))
       call reorder412356(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,N2,M2,M2,N3,N1,M2,M1,N1,M1,N1,t3B1,F2)
       allocate(Z83(M2+1:N3,N1+1:M2,M1+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K7*K7*K9*K6
       I3=K0*K8
       call EGEMM(I1,I2,I3,D1,F2,Z83)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum125634(M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,V3A,Z83, 1.000)
       deallocate(Z83)
C
       call sumx3(N1,N3,N1,N3,N1,M2,N0,N1,N0,N1,M1,N1,
     & M2,N3,N1,M2,N1,M2,N0,M1,M1,N1,M1,N1,HT3A,V3A,1.0)
       deallocate(V3A)
C
       end
