       subroutine t3C010111_update(N0,N1,N2,N3,HT3C1,shift,
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
       real*8,allocatable::S28(:,:,:,:)
       real*8,allocatable::S27(:,:,:,:)
       real*8,allocatable::S29(:,:,:,:)
       real*8,allocatable::S30(:,:,:,:)
       real*8,allocatable::S31(:,:,:,:)
       real*8,allocatable::Q9(:,:)
       real*8,allocatable::S32(:,:,:,:)
       real*8,allocatable::S33(:,:,:,:)
       real*8,allocatable::S34(:,:,:,:)
       real*8,allocatable::S35(:,:,:,:)
       real*8,allocatable::Q10(:,:)
       real*8,allocatable::Q11(:,:)
       real*8,allocatable::S36(:,:,:,:)
       real*8,allocatable::S38(:,:,:,:)
       real*8,allocatable::Q12(:,:)
       real*8,allocatable::S37(:,:,:,:)
       real*8,allocatable::S39(:,:,:,:)
       real*8,allocatable::Q13(:,:)
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
       real*8,allocatable::Q14(:,:)
       real*8,allocatable::Q15(:,:)
       real*8,allocatable::Q16(:,:)
       real*8,allocatable::Q17(:,:)
       real*8,allocatable::Q18(:,:)
       real*8,allocatable::Q19(:,:)
       real*8,allocatable::Q20(:,:)
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
       real*8,allocatable::Q21(:,:)
       real*8,allocatable::Q22(:,:)
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
       real*8,allocatable::Q23(:,:)
       real*8,allocatable::Q24(:,:)
       real*8,allocatable::S133(:,:,:,:)
       real*8,allocatable::S134(:,:,:,:)
       real*8,allocatable::S135(:,:,:,:)
       real*8,allocatable::S136(:,:,:,:)
       real*8,allocatable::S137(:,:,:,:)
       real*8,allocatable::S138(:,:,:,:)
       real*8,allocatable::Q25(:,:)
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
       real*8,allocatable::Q26(:,:)
       real*8,allocatable::Q27(:,:)
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
       real*8,allocatable::Q28(:,:)
       real*8,allocatable::Q29(:,:)
       real*8,allocatable::S164(:,:,:,:)
       real*8,allocatable::S165(:,:,:,:)
       real*8,allocatable::S166(:,:,:,:)
       real*8,allocatable::Q30(:,:)
       real*8,allocatable::Q31(:,:)
       real*8,allocatable::S167(:,:,:,:)
       real*8,allocatable::S168(:,:,:,:)
       real*8,allocatable::S169(:,:,:,:)
       real*8,allocatable::S170(:,:,:,:)
       real*8,allocatable::S171(:,:,:,:)
       real*8,allocatable::S172(:,:,:,:)
       real*8,allocatable::S173(:,:,:,:)
       real*8,allocatable::Q32(:,:)
       real*8,allocatable::Q33(:,:)
       real*8,allocatable::Q34(:,:)
       real*8,allocatable::Q35(:,:)
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
       real*8,allocatable::Q36(:,:)
       real*8,allocatable::Q37(:,:)
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
       real*8,allocatable::Q38(:,:)
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
       real*8,allocatable::Q39(:,:)
       real*8,allocatable::Q40(:,:)
       real*8,allocatable::S419(:,:,:,:)
       real*8,allocatable::S420(:,:,:,:)
       real*8,allocatable::S421(:,:,:,:)
       real*8,allocatable::S422(:,:,:,:)
       real*8,allocatable::Q41(:,:)
       real*8,allocatable::Q42(:,:)
       real*8,allocatable::S423(:,:,:,:)
       real*8,allocatable::S424(:,:,:,:)
       real*8,allocatable::S425(:,:,:,:)
       real*8,allocatable::S426(:,:,:,:)
       real*8,allocatable::Q43(:,:)
       real*8,allocatable::Q44(:,:)
       real*8,allocatable::Q45(:,:)
       real*8,allocatable::Q46(:,:)
       real*8,allocatable::S427(:,:,:,:)
       real*8,allocatable::S428(:,:,:,:)
       real*8,allocatable::S429(:,:,:,:)
       real*8,allocatable::S430(:,:,:,:)
       real*8,allocatable::S431(:,:,:,:)
       real*8,allocatable::S432(:,:,:,:)
       real*8,allocatable::S433(:,:,:,:)
       real*8,allocatable::S434(:,:,:,:)
       real*8,allocatable::S435(:,:,:,:)
       real*8,allocatable::S436(:,:,:,:)
       real*8,allocatable::S437(:,:,:,:)
       real*8,allocatable::S439(:,:,:,:)
       real*8,allocatable::S441(:,:,:,:)
       real*8,allocatable::S438(:,:,:,:)
       real*8,allocatable::S440(:,:,:,:)
       real*8,allocatable::S442(:,:,:,:)
       real*8,allocatable::S443(:,:,:,:)
       real*8,allocatable::S444(:,:,:,:)
       real*8,allocatable::S445(:,:,:,:)
       real*8,allocatable::S446(:,:,:,:)
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
       real*8,allocatable::S472(:,:,:,:)
       real*8,allocatable::S473(:,:,:,:)
       real*8,allocatable::S474(:,:,:,:)
       real*8,allocatable::S475(:,:,:,:)
       real*8,allocatable::S476(:,:,:,:)
       real*8,allocatable::S477(:,:,:,:)
       real*8,allocatable::S478(:,:,:,:)
       real*8,allocatable::S479(:,:,:,:)
       real*8,allocatable::S480(:,:,:,:)
       real*8,allocatable::S481(:,:,:,:)
       real*8,allocatable::S482(:,:,:,:)
       real*8,allocatable::S483(:,:,:,:)
       real*8,allocatable::S484(:,:,:,:)
       real*8,allocatable::S485(:,:,:,:)
       real*8,allocatable::S486(:,:,:,:)
       real*8,allocatable::S487(:,:,:,:)
       real*8,allocatable::S488(:,:,:,:)
       real*8,allocatable::S489(:,:,:,:)
       real*8,allocatable::S490(:,:,:,:)
       real*8,allocatable::S491(:,:,:,:)
       real*8,allocatable::S492(:,:,:,:)
       real*8,allocatable::S493(:,:,:,:)
       real*8,allocatable::S494(:,:,:,:)
       real*8,allocatable::S495(:,:,:,:)
       real*8,allocatable::S496(:,:,:,:)
       real*8,allocatable::S497(:,:,:,:)
       real*8,allocatable::S498(:,:,:,:)
       real*8,allocatable::S499(:,:,:,:)
       real*8,allocatable::Q47(:,:)
       real*8,allocatable::S500(:,:,:,:)
       real*8,allocatable::S501(:,:,:,:)
       real*8,allocatable::S502(:,:,:,:)
       real*8,allocatable::S503(:,:,:,:)
       real*8,allocatable::S504(:,:,:,:)
       real*8,allocatable::S505(:,:,:,:)
       real*8,allocatable::S506(:,:,:,:)
       real*8,allocatable::S507(:,:,:,:)
       real*8,allocatable::S508(:,:,:,:)
       real*8,allocatable::S509(:,:,:,:)
       real*8,allocatable::S510(:,:,:,:)
       real*8,allocatable::S511(:,:,:,:)
       real*8,allocatable::S512(:,:,:,:)
       real*8,allocatable::S513(:,:,:,:)
       real*8,allocatable::Q48(:,:)
       real*8,allocatable::Q49(:,:)
       real*8,allocatable::S514(:,:,:,:)
       real*8,allocatable::S515(:,:,:,:)
       real*8,allocatable::S516(:,:,:,:)
       real*8,allocatable::Q50(:,:)
       real*8,allocatable::Q51(:,:)
       real*8,allocatable::Q52(:,:)
       real*8,allocatable::Q53(:,:)
       real*8,allocatable::S517(:,:,:,:)
       real*8,allocatable::S539(:,:,:,:)
       real*8,allocatable::S540(:,:,:,:)
       real*8,allocatable::S518(:,:,:,:)
       real*8,allocatable::S521(:,:,:,:)
       real*8,allocatable::S541(:,:,:,:)
       real*8,allocatable::S542(:,:,:,:)
       real*8,allocatable::S545(:,:,:,:)
       real*8,allocatable::S546(:,:,:,:)
       real*8,allocatable::S522(:,:,:,:)
       real*8,allocatable::Q56(:,:)
       real*8,allocatable::Q57(:,:)
       real*8,allocatable::Q58(:,:)
       real*8,allocatable::Q59(:,:)
       real*8,allocatable::Q60(:,:)
       real*8,allocatable::Q61(:,:)
       real*8,allocatable::S531(:,:,:,:)
       real*8,allocatable::S532(:,:,:,:)
       real*8,allocatable::S533(:,:,:,:)
       real*8,allocatable::S534(:,:,:,:)
       real*8,allocatable::S535(:,:,:,:)
       real*8,allocatable::S536(:,:,:,:)
       real*8,allocatable::S537(:,:,:,:)
       real*8,allocatable::S538(:,:,:,:)
       real*8,allocatable::S519(:,:,:,:)
       real*8,allocatable::S543(:,:,:,:)
       real*8,allocatable::S544(:,:,:,:)
       real*8,allocatable::S520(:,:,:,:)
       real*8,allocatable::S527(:,:,:,:)
       real*8,allocatable::S528(:,:,:,:)
       real*8,allocatable::S523(:,:,:,:)
       real*8,allocatable::S524(:,:,:,:)
       real*8,allocatable::S525(:,:,:,:)
       real*8,allocatable::S526(:,:,:,:)
       real*8,allocatable::S529(:,:,:,:)
       real*8,allocatable::S530(:,:,:,:)
       real*8,allocatable::Q54(:,:)
       real*8,allocatable::Q55(:,:)
       real*8,allocatable::S547(:,:,:,:)
       real*8,allocatable::S549(:,:,:,:)
       real*8,allocatable::S548(:,:,:,:)
       real*8,allocatable::S550(:,:,:,:)
       real*8,allocatable::S551(:,:,:,:)
       real*8,allocatable::S552(:,:,:,:)
       real*8,allocatable::S553(:,:,:,:)
       real*8,allocatable::S554(:,:,:,:)
       real*8,allocatable::S556(:,:,:,:)
       real*8,allocatable::S555(:,:,:,:)
       real*8,allocatable::S557(:,:,:,:)
       real*8,allocatable::S558(:,:,:,:)
       real*8,allocatable::S559(:,:,:,:)
       real*8,allocatable::S560(:,:,:,:)
       real*8,allocatable::S561(:,:,:,:)
       real*8,allocatable::S666(:,:,:,:)
       real*8,allocatable::S667(:,:,:,:)
       real*8,allocatable::S562(:,:,:,:)
       real*8,allocatable::S565(:,:,:,:)
       real*8,allocatable::S566(:,:,:,:)
       real*8,allocatable::S567(:,:,:,:)
       real*8,allocatable::S568(:,:,:,:)
       real*8,allocatable::S571(:,:,:,:)
       real*8,allocatable::S572(:,:,:,:)
       real*8,allocatable::S569(:,:,:,:)
       real*8,allocatable::S570(:,:,:,:)
       real*8,allocatable::S575(:,:,:,:)
       real*8,allocatable::S582(:,:,:,:)
       real*8,allocatable::S576(:,:,:,:)
       real*8,allocatable::S579(:,:,:,:)
       real*8,allocatable::S580(:,:,:,:)
       real*8,allocatable::S573(:,:,:,:)
       real*8,allocatable::S574(:,:,:,:)
       real*8,allocatable::S581(:,:,:,:)
       real*8,allocatable::S577(:,:,:,:)
       real*8,allocatable::S578(:,:,:,:)
       real*8,allocatable::S583(:,:,:,:)
       real*8,allocatable::S586(:,:,:,:)
       real*8,allocatable::S593(:,:,:,:)
       real*8,allocatable::S587(:,:,:,:)
       real*8,allocatable::S590(:,:,:,:)
       real*8,allocatable::S595(:,:,:,:)
       real*8,allocatable::S591(:,:,:,:)
       real*8,allocatable::S588(:,:,:,:)
       real*8,allocatable::S594(:,:,:,:)
       real*8,allocatable::S589(:,:,:,:)
       real*8,allocatable::S584(:,:,:,:)
       real*8,allocatable::S585(:,:,:,:)
       real*8,allocatable::S592(:,:,:,:)
       real*8,allocatable::Q64(:,:)
       real*8,allocatable::Q65(:,:)
       real*8,allocatable::S596(:,:,:,:)
       real*8,allocatable::S597(:,:,:,:)
       real*8,allocatable::S598(:,:,:,:)
       real*8,allocatable::S599(:,:,:,:)
       real*8,allocatable::Q66(:,:)
       real*8,allocatable::Q70(:,:)
       real*8,allocatable::Q67(:,:)
       real*8,allocatable::S602(:,:,:,:)
       real*8,allocatable::S603(:,:,:,:)
       real*8,allocatable::S604(:,:,:,:)
       real*8,allocatable::S605(:,:,:,:)
       real*8,allocatable::S606(:,:,:,:)
       real*8,allocatable::S607(:,:,:,:)
       real*8,allocatable::Q68(:,:)
       real*8,allocatable::Q69(:,:)
       real*8,allocatable::S600(:,:,:,:)
       real*8,allocatable::S601(:,:,:,:)
       real*8,allocatable::S608(:,:,:,:)
       real*8,allocatable::S616(:,:,:,:)
       real*8,allocatable::S609(:,:,:,:)
       real*8,allocatable::S612(:,:,:,:)
       real*8,allocatable::S618(:,:,:,:)
       real*8,allocatable::S613(:,:,:,:)
       real*8,allocatable::Q71(:,:)
       real*8,allocatable::Q75(:,:)
       real*8,allocatable::Q72(:,:)
       real*8,allocatable::S610(:,:,:,:)
       real*8,allocatable::S617(:,:,:,:)
       real*8,allocatable::S611(:,:,:,:)
       real*8,allocatable::S614(:,:,:,:)
       real*8,allocatable::S619(:,:,:,:)
       real*8,allocatable::S615(:,:,:,:)
       real*8,allocatable::Q73(:,:)
       real*8,allocatable::Q76(:,:)
       real*8,allocatable::Q74(:,:)
       real*8,allocatable::Q77(:,:)
       real*8,allocatable::Q78(:,:)
       real*8,allocatable::Q79(:,:)
       real*8,allocatable::Q80(:,:)
       real*8,allocatable::S620(:,:,:,:)
       real*8,allocatable::S622(:,:,:,:)
       real*8,allocatable::S623(:,:,:,:)
       real*8,allocatable::S624(:,:,:,:)
       real*8,allocatable::S621(:,:,:,:)
       real*8,allocatable::S625(:,:,:,:)
       real*8,allocatable::S626(:,:,:,:)
       real*8,allocatable::Q81(:,:)
       real*8,allocatable::S630(:,:,:,:)
       real*8,allocatable::S633(:,:,:,:)
       real*8,allocatable::S636(:,:,:,:)
       real*8,allocatable::S627(:,:,:,:)
       real*8,allocatable::S628(:,:,:,:)
       real*8,allocatable::S629(:,:,:,:)
       real*8,allocatable::S634(:,:,:,:)
       real*8,allocatable::S635(:,:,:,:)
       real*8,allocatable::S631(:,:,:,:)
       real*8,allocatable::S632(:,:,:,:)
       real*8,allocatable::S637(:,:,:,:)
       real*8,allocatable::S639(:,:,:,:)
       real*8,allocatable::S640(:,:,:,:)
       real*8,allocatable::S641(:,:,:,:)
       real*8,allocatable::S642(:,:,:,:)
       real*8,allocatable::S643(:,:,:,:)
       real*8,allocatable::S644(:,:,:,:)
       real*8,allocatable::S645(:,:,:,:)
       real*8,allocatable::S811(:,:,:,:)
       real*8,allocatable::S812(:,:,:,:)
       real*8,allocatable::S815(:,:,:,:)
       real*8,allocatable::S817(:,:,:,:)
       real*8,allocatable::S813(:,:,:,:)
       real*8,allocatable::S814(:,:,:,:)
       real*8,allocatable::S638(:,:,:,:)
       real*8,allocatable::S818(:,:,:,:)
       real*8,allocatable::S819(:,:,:,:)
       real*8,allocatable::S816(:,:,:,:)
       real*8,allocatable::S820(:,:,:,:)
       real*8,allocatable::S821(:,:,:,:)
       real*8,allocatable::S660(:,:,:,:)
       real*8,allocatable::S752(:,:,:,:)
       real*8,allocatable::S769(:,:,:,:)
       real*8,allocatable::S661(:,:,:,:)
       real*8,allocatable::Q82(:,:)
       real*8,allocatable::S662(:,:,:,:)
       real*8,allocatable::S652(:,:,:,:)
       real*8,allocatable::S659(:,:,:,:)
       real*8,allocatable::S665(:,:,:,:)
       real*8,allocatable::S563(:,:,:,:)
       real*8,allocatable::S564(:,:,:,:)
       real*8,allocatable::S668(:,:,:,:)
       real*8,allocatable::S669(:,:,:,:)
       real*8,allocatable::S673(:,:,:,:)
       real*8,allocatable::S675(:,:,:,:)
       real*8,allocatable::S674(:,:,:,:)
       real*8,allocatable::S676(:,:,:,:)
       real*8,allocatable::S678(:,:,:,:)
       real*8,allocatable::S677(:,:,:,:)
       real*8,allocatable::S679(:,:,:,:)
       real*8,allocatable::S680(:,:,:,:)
       real*8,allocatable::S681(:,:,:,:)
       real*8,allocatable::S683(:,:,:,:)
       real*8,allocatable::S682(:,:,:,:)
       real*8,allocatable::S684(:,:,:,:)
       real*8,allocatable::S688(:,:,:,:)
       real*8,allocatable::S685(:,:,:,:)
       real*8,allocatable::S686(:,:,:,:)
       real*8,allocatable::S689(:,:,:,:)
       real*8,allocatable::S687(:,:,:,:)
       real*8,allocatable::S690(:,:,:,:)
       real*8,allocatable::S691(:,:,:,:)
       real*8,allocatable::S692(:,:,:,:)
       real*8,allocatable::S693(:,:,:,:)
       real*8,allocatable::S694(:,:,:,:)
       real*8,allocatable::S695(:,:,:,:)
       real*8,allocatable::S696(:,:,:,:)
       real*8,allocatable::S700(:,:,:,:)
       real*8,allocatable::S697(:,:,:,:)
       real*8,allocatable::S698(:,:,:,:)
       real*8,allocatable::S701(:,:,:,:)
       real*8,allocatable::S699(:,:,:,:)
       real*8,allocatable::S702(:,:,:,:)
       real*8,allocatable::S710(:,:,:,:)
       real*8,allocatable::S703(:,:,:,:)
       real*8,allocatable::S706(:,:,:,:)
       real*8,allocatable::S712(:,:,:,:)
       real*8,allocatable::S707(:,:,:,:)
       real*8,allocatable::S704(:,:,:,:)
       real*8,allocatable::S711(:,:,:,:)
       real*8,allocatable::S705(:,:,:,:)
       real*8,allocatable::S708(:,:,:,:)
       real*8,allocatable::S713(:,:,:,:)
       real*8,allocatable::S709(:,:,:,:)
       real*8,allocatable::S714(:,:,:,:)
       real*8,allocatable::S715(:,:,:,:)
       real*8,allocatable::S716(:,:,:,:)
       real*8,allocatable::S717(:,:,:,:)
       real*8,allocatable::S718(:,:,:,:)
       real*8,allocatable::S722(:,:,:,:)
       real*8,allocatable::S719(:,:,:,:)
       real*8,allocatable::S720(:,:,:,:)
       real*8,allocatable::S723(:,:,:,:)
       real*8,allocatable::S721(:,:,:,:)
       real*8,allocatable::Q83(:,:)
       real*8,allocatable::Q87(:,:)
       real*8,allocatable::Q84(:,:)
       real*8,allocatable::S726(:,:,:,:)
       real*8,allocatable::S733(:,:,:,:)
       real*8,allocatable::S727(:,:,:,:)
       real*8,allocatable::S730(:,:,:,:)
       real*8,allocatable::S735(:,:,:,:)
       real*8,allocatable::S731(:,:,:,:)
       real*8,allocatable::S728(:,:,:,:)
       real*8,allocatable::S734(:,:,:,:)
       real*8,allocatable::S729(:,:,:,:)
       real*8,allocatable::S724(:,:,:,:)
       real*8,allocatable::S725(:,:,:,:)
       real*8,allocatable::S732(:,:,:,:)
       real*8,allocatable::Q85(:,:)
       real*8,allocatable::Q86(:,:)
       real*8,allocatable::S736(:,:,:,:)
       real*8,allocatable::S737(:,:,:,:)
       real*8,allocatable::S738(:,:,:,:)
       real*8,allocatable::S739(:,:,:,:)
       real*8,allocatable::S740(:,:,:,:)
       real*8,allocatable::S741(:,:,:,:)
       real*8,allocatable::Q88(:,:)
       real*8,allocatable::Q92(:,:)
       real*8,allocatable::Q89(:,:)
       real*8,allocatable::Q90(:,:)
       real*8,allocatable::Q93(:,:)
       real*8,allocatable::Q91(:,:)
       real*8,allocatable::S742(:,:,:,:)
       real*8,allocatable::S749(:,:,:,:)
       real*8,allocatable::S750(:,:,:,:)
       real*8,allocatable::S751(:,:,:,:)
       real*8,allocatable::S753(:,:,:,:)
       real*8,allocatable::S822(:,:,:,:)
       real*8,allocatable::S823(:,:,:,:)
       real*8,allocatable::S743(:,:,:,:)
       real*8,allocatable::S824(:,:,:,:)
       real*8,allocatable::S670(:,:,:,:)
       real*8,allocatable::S672(:,:,:,:)
       real*8,allocatable::S671(:,:,:,:)
       real*8,allocatable::S663(:,:,:,:)
       real*8,allocatable::S664(:,:,:,:)
       real*8,allocatable::S771(:,:,:,:)
       real*8,allocatable::S754(:,:,:,:)
       real*8,allocatable::S757(:,:,:,:)
       real*8,allocatable::S762(:,:,:,:)
       real*8,allocatable::S758(:,:,:,:)
       real*8,allocatable::S650(:,:,:,:)
       real*8,allocatable::S766(:,:,:,:)
       real*8,allocatable::S651(:,:,:,:)
       real*8,allocatable::S761(:,:,:,:)
       real*8,allocatable::S764(:,:,:,:)
       real*8,allocatable::S765(:,:,:,:)
       real*8,allocatable::S759(:,:,:,:)
       real*8,allocatable::S760(:,:,:,:)
       real*8,allocatable::Q94(:,:)
       real*8,allocatable::S778(:,:,:,:)
       real*8,allocatable::S782(:,:,:,:)
       real*8,allocatable::S763(:,:,:,:)
       real*8,allocatable::S767(:,:,:,:)
       real*8,allocatable::S744(:,:,:,:)
       real*8,allocatable::S768(:,:,:,:)
       real*8,allocatable::S755(:,:,:,:)
       real*8,allocatable::S770(:,:,:,:)
       real*8,allocatable::S745(:,:,:,:)
       real*8,allocatable::S825(:,:,:,:)
       real*8,allocatable::S826(:,:,:,:)
       real*8,allocatable::S756(:,:,:,:)
       real*8,allocatable::S827(:,:,:,:)
       real*8,allocatable::S657(:,:,:,:)
       real*8,allocatable::S776(:,:,:,:)
       real*8,allocatable::S658(:,:,:,:)
       real*8,allocatable::S781(:,:,:,:)
       real*8,allocatable::S783(:,:,:,:)
       real*8,allocatable::S786(:,:,:,:)
       real*8,allocatable::S787(:,:,:,:)
       real*8,allocatable::S784(:,:,:,:)
       real*8,allocatable::S828(:,:,:,:)
       real*8,allocatable::S829(:,:,:,:)
       real*8,allocatable::S831(:,:,:,:)
       real*8,allocatable::S832(:,:,:,:)
       real*8,allocatable::S785(:,:,:,:)
       real*8,allocatable::S830(:,:,:,:)
       real*8,allocatable::S788(:,:,:,:)
       real*8,allocatable::S796(:,:,:,:)
       real*8,allocatable::S797(:,:,:,:)
       real*8,allocatable::S789(:,:,:,:)
       real*8,allocatable::S833(:,:,:,:)
       real*8,allocatable::S834(:,:,:,:)
       real*8,allocatable::S795(:,:,:,:)
       real*8,allocatable::Q95(:,:)
       real*8,allocatable::S808(:,:,:,:)
       real*8,allocatable::S810(:,:,:,:)
       real*8,allocatable::S794(:,:,:,:)
       real*8,allocatable::S802(:,:,:,:)
       real*8,allocatable::S803(:,:,:,:)
       real*8,allocatable::S807(:,:,:,:)
       real*8,allocatable::S804(:,:,:,:)
       real*8,allocatable::S805(:,:,:,:)
       real*8,allocatable::S809(:,:,:,:)
       real*8,allocatable::S806(:,:,:,:)
       real*8,allocatable::S648(:,:,:,:)
       real*8,allocatable::S649(:,:,:,:)
       real*8,allocatable::S774(:,:,:,:)
       real*8,allocatable::S775(:,:,:,:)
       real*8,allocatable::Q62(:,:)
       real*8,allocatable::Q63(:,:)
       real*8,allocatable::S653(:,:,:,:)
       real*8,allocatable::S654(:,:,:,:)
       real*8,allocatable::S655(:,:,:,:)
       real*8,allocatable::S656(:,:,:,:)
       real*8,allocatable::S798(:,:,:,:)
       real*8,allocatable::S799(:,:,:,:)
       real*8,allocatable::S772(:,:,:,:)
       real*8,allocatable::S773(:,:,:,:)
       real*8,allocatable::S777(:,:,:,:)
       real*8,allocatable::S746(:,:,:,:)
       real*8,allocatable::S747(:,:,:,:)
       real*8,allocatable::S748(:,:,:,:)
       real*8,allocatable::S779(:,:,:,:)
       real*8,allocatable::S780(:,:,:,:)
       real*8,allocatable::S790(:,:,:,:)
       real*8,allocatable::S791(:,:,:,:)
       real*8,allocatable::S792(:,:,:,:)
       real*8,allocatable::S793(:,:,:,:)
       real*8,allocatable::S646(:,:,:,:)
       real*8,allocatable::S647(:,:,:,:)
       real*8,allocatable::S800(:,:,:,:)
       real*8,allocatable::S801(:,:,:,:)
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
       real*8,allocatable::X29(:,:)
       real*8,allocatable::Z29(:,:,:,:,:,:)
       real*8,allocatable::X30(:,:)
       real*8,allocatable::Z30(:,:,:,:,:,:)
       real*8,allocatable::X31(:,:)
       real*8,allocatable::Z31(:,:,:,:,:,:)
       real*8,allocatable::X32(:,:)
       real*8,allocatable::Z32(:,:,:,:,:,:)
       real*8,allocatable::X33(:,:)
       real*8,allocatable::Z33(:,:,:,:,:,:)
       real*8,allocatable::X34(:,:)
       real*8,allocatable::Z34(:,:,:,:,:,:)
       real*8,allocatable::X35(:,:)
       real*8,allocatable::Z35(:,:,:,:,:,:)
       real*8,allocatable::X36(:,:)
       real*8,allocatable::Z36(:,:,:,:,:,:)
       real*8,allocatable::X37(:,:)
       real*8,allocatable::Z37(:,:,:,:,:,:)
       real*8,allocatable::X38(:,:)
       real*8,allocatable::Z38(:,:,:,:,:,:)
       real*8,allocatable::X39(:,:)
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
       real*8,allocatable::X70(:,:,:,:)
       real*8,allocatable::Z70(:,:,:,:,:,:)
       real*8,allocatable::X71(:,:,:,:)
       real*8,allocatable::Z71(:,:,:,:,:,:)
       real*8,allocatable::X72(:,:,:,:)
       real*8,allocatable::Z72(:,:,:,:,:,:)
       real*8,allocatable::X73(:,:,:,:)
       real*8,allocatable::Z73(:,:,:,:,:,:)
       real*8,allocatable::X74(:,:,:,:)
       real*8,allocatable::Z74(:,:,:,:,:,:)
       real*8,allocatable::X75(:,:,:,:)
       real*8,allocatable::Z75(:,:,:,:,:,:)
       real*8,allocatable::X76(:,:,:,:)
       real*8,allocatable::Z76(:,:,:,:,:,:)
       real*8,allocatable::X77(:,:,:,:)
       real*8,allocatable::Z77(:,:,:,:,:,:)
       real*8,allocatable::X78(:,:,:,:)
       real*8,allocatable::Z78(:,:,:,:,:,:)
       real*8,allocatable::X79(:,:,:,:)
       real*8,allocatable::Z79(:,:,:,:,:,:)
       real*8,allocatable::X80(:,:,:,:)
       real*8,allocatable::Z80(:,:,:,:,:,:)
       real*8,allocatable::X81(:,:,:,:)
       real*8,allocatable::Z81(:,:,:,:,:,:)
       real*8,allocatable::X82(:,:,:,:)
       real*8,allocatable::Z82(:,:,:,:,:,:)
       real*8,allocatable::X83(:,:,:,:)
       real*8,allocatable::Z83(:,:,:,:,:,:)
       real*8,allocatable::X84(:,:,:,:)
       real*8,allocatable::Z84(:,:,:,:,:,:)
       real*8,allocatable::X85(:,:,:,:)
       real*8,allocatable::Z85(:,:,:,:,:,:)
       real*8,allocatable::X86(:,:,:,:)
       real*8,allocatable::Z86(:,:,:,:,:,:)
       real*8,allocatable::X87(:,:,:,:)
       real*8,allocatable::Z87(:,:,:,:,:,:)
       real*8,allocatable::X88(:,:,:,:)
       real*8,allocatable::Z88(:,:,:,:,:,:)
       real*8,allocatable::X89(:,:,:,:)
       real*8,allocatable::Z89(:,:,:,:,:,:)
       real*8,allocatable::Z90(:,:,:,:,:,:)
       real*8,allocatable::Z91(:,:,:,:,:,:)
       real*8,allocatable::Z92(:,:,:,:,:,:)
       real*8,allocatable::Z93(:,:,:,:,:,:)
       real*8,allocatable::Z114(:,:,:,:,:,:)
       real*8,allocatable::Z115(:,:,:,:,:,:)
       real*8,allocatable::Z116(:,:,:,:,:,:)
       real*8,allocatable::Z117(:,:,:,:,:,:)
       real*8,allocatable::Z120(:,:,:,:,:,:)
       real*8,allocatable::Z121(:,:,:,:,:,:)
       real*8,allocatable::Z122(:,:,:,:,:,:)
       real*8,allocatable::Z123(:,:,:,:,:,:)
       real*8,allocatable::X90(:,:,:,:)
       real*8,allocatable::Z133(:,:,:,:,:,:)
       real*8,allocatable::X91(:,:,:,:)
       real*8,allocatable::Z137(:,:,:,:,:,:)
       real*8,allocatable::X92(:,:,:,:)
       real*8,allocatable::Z135(:,:,:,:,:,:)
       real*8,allocatable::X93(:,:,:,:)
       real*8,allocatable::Z139(:,:,:,:,:,:)
       real*8,allocatable::X94(:,:,:,:)
       real*8,allocatable::Z134(:,:,:,:,:,:)
       real*8,allocatable::X95(:,:,:,:)
       real*8,allocatable::Z138(:,:,:,:,:,:)
       real*8,allocatable::X96(:,:,:,:)
       real*8,allocatable::Z136(:,:,:,:,:,:)
       real*8,allocatable::X97(:,:,:,:)
       real*8,allocatable::Z140(:,:,:,:,:,:)
       real*8,allocatable::X98(:,:,:,:)
       real*8,allocatable::Z141(:,:,:,:,:,:)
       real*8,allocatable::X99(:,:,:,:)
       real*8,allocatable::Z142(:,:,:,:,:,:)
       real*8,allocatable::X100(:,:)
       real*8,allocatable::Z143(:,:,:,:,:,:)
       real*8,allocatable::X101(:,:,:,:)
       real*8,allocatable::Z144(:,:,:,:,:,:)
       real*8,allocatable::X102(:,:,:,:)
       real*8,allocatable::Z145(:,:,:,:,:,:)
       real*8,allocatable::X103(:,:,:,:)
       real*8,allocatable::Z146(:,:,:,:,:,:)
       real*8,allocatable::X104(:,:,:,:)
       real*8,allocatable::Z147(:,:,:,:,:,:)
       real*8,allocatable::X105(:,:)
       real*8,allocatable::Z148(:,:,:,:,:,:)
       real*8,allocatable::X106(:,:)
       real*8,allocatable::Z149(:,:,:,:,:,:)
       real*8,allocatable::X107(:,:)
       real*8,allocatable::Z154(:,:,:,:,:,:)
       real*8,allocatable::X108(:,:)
       real*8,allocatable::Z160(:,:,:,:,:,:)
       real*8,allocatable::X109(:,:)
       real*8,allocatable::Z155(:,:,:,:,:,:)
       real*8,allocatable::X110(:,:)
       real*8,allocatable::Z161(:,:,:,:,:,:)
       real*8,allocatable::Z162(:,:,:,:,:,:)
       real*8,allocatable::Z163(:,:,:,:,:,:)
       real*8,allocatable::Z164(:,:,:,:,:,:)
       real*8,allocatable::Z165(:,:,:,:,:,:)
       real*8,allocatable::Z166(:,:,:,:,:,:)
       real*8,allocatable::Z167(:,:,:,:,:,:)
       real*8,allocatable::Z168(:,:,:,:,:,:)
       real*8,allocatable::Z169(:,:,:,:,:,:)
       real*8,allocatable::X111(:,:,:,:)
       real*8,allocatable::Z311(:,:,:,:,:,:)
       real*8,allocatable::X112(:,:,:,:)
       real*8,allocatable::Z312(:,:,:,:,:,:)
       real*8,allocatable::X113(:,:,:,:)
       real*8,allocatable::Z313(:,:,:,:,:,:)
       real*8,allocatable::X114(:,:,:,:)
       real*8,allocatable::Z314(:,:,:,:,:,:)
       real*8,allocatable::Z317(:,:,:,:,:,:)
       real*8,allocatable::Z318(:,:,:,:,:,:)
       real*8,allocatable::X115(:,:,:,:)
       real*8,allocatable::Z319(:,:,:,:,:,:)
       real*8,allocatable::X116(:,:,:,:)
       real*8,allocatable::Z320(:,:,:,:,:,:)
       real*8,allocatable::X117(:,:,:,:)
       real*8,allocatable::Z321(:,:,:,:,:,:)
       real*8,allocatable::X118(:,:,:,:)
       real*8,allocatable::Z322(:,:,:,:,:,:)
       real*8,allocatable::Z528(:,:,:,:,:,:)
       real*8,allocatable::Z529(:,:,:,:,:,:)
       real*8,allocatable::Z534(:,:,:,:,:,:)
       real*8,allocatable::Z535(:,:,:,:,:,:)
       real*8,allocatable::Z547(:,:,:,:,:,:)
       real*8,allocatable::Z548(:,:,:,:,:,:)
       real*8,allocatable::Z549(:,:,:,:,:,:)
       real*8,allocatable::Z550(:,:,:,:,:,:)
       real*8,allocatable::Z559(:,:,:,:,:,:)
       real*8,allocatable::Z560(:,:,:,:,:,:)
       real*8,allocatable::Z561(:,:,:,:,:,:)
       real*8,allocatable::Z562(:,:,:,:,:,:)
C
       allocate(V3C(N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       V3C=0.0d0
C
       allocate(D1(N1+1:N3,N0+1:N1,N2+1:M2,M1+1:N2))
       call reorder2431(N0,N2,N1,N3,N2,N3,N0,N1,
     & N1,N3,N0,N1,N2,M2,M1,N2,VBHPPH,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S1(N0+1:M1,N0+1:N1,N2+1:M2,M1+1:N2))
       I1=K8*K0*K1
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S1)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X1(N0+1:N1,N2+1:M2,M1+1:N2,N0+1:M1))
       X1=0.0d0
       call
     & sum4123(N0,N1,N2,M2,M1,N2,N0,M1,X1,S1, 1.000)
C
       allocate(X2(N0+1:N1,N2+1:M2,M1+1:N2,N0+1:M1))
       X2=0.0d0
       call
     & sum4123(N0,N1,N2,M2,M1,N2,N0,M1,X2,S1, 1.000)
       deallocate(S1)
C
       allocate(D1(N1+1:N3,N0+1:N1,N2+1:M2,N0+1:M1))
       call reorder2431(N0,N2,N1,N3,N2,N3,N0,N1,
     & N1,N3,N0,N1,N2,M2,N0,M1,VBHPPH,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S2(N0+1:M1,N0+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K1
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S2)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X3(N0+1:N1,N2+1:M2,N0+1:M1,N0+1:M1))
       X3=0.0d0
       call
     & sum4123(N0,N1,N2,M2,N0,M1,N0,M1,X3,S2, 1.000)
C
       allocate(X4(N0+1:N1,N2+1:M2,N0+1:M1,N0+1:M1))
       X4=0.0d0
       call
     & sum4123(N0,N1,N2,M2,N0,M1,N0,M1,X4,S2, 1.000)
       deallocate(S2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,VBHPPH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S3(N1+1:M2,N1+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K3
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S3)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X5(N1+1:N3,N2+1:M2,N1+1:M2,M1+1:N2))
       X5=0.0d0
       call
     & sum3124(N1,N3,N2,M2,N1,M2,M1,N2,X5,S3,-1.000)
C
       allocate(X6(N1+1:N3,N2+1:M2,N1+1:M2,M1+1:N2))
       X6=0.0d0
       call
     & sum3124(N1,N3,N2,M2,N1,M2,M1,N2,X6,S3,-1.000)
       deallocate(S3)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,VBHPPH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S4(N1+1:M2,N1+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K3
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S4)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X7(N1+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       X7=0.0d0
       call
     & sum3124(N1,N3,N2,M2,N1,M2,N0,M1,X7,S4,-1.000)
C
       allocate(X8(N1+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       X8=0.0d0
       call
     & sum3124(N1,N3,N2,M2,N1,M2,N0,M1,X8,S4,-1.000)
       deallocate(S4)
C
       allocate(D1(N0+1:N1,N0+1:N2,M1+1:N2,N0+1:M1))
       call reorder4312(N0,N2,N0,N1,N0,N2,N0,N1,
     & N0,N1,N0,N2,M1,N2,N0,M1,VBHHHH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S5(N1+1:M2,N0+1:N2,M1+1:N2,N0+1:M1))
       I1=K5*K8*K2
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S5)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X13(N0+1:N2,N1+1:M2,M1+1:N2,N0+1:M1))
       X13=0.0d0
       call
     & sum2134(N0,N2,N1,M2,M1,N2,N0,M1,X13,S5,-1.000)
       deallocate(S5)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:M1,N0+1:M1))
       call reorder4312(N0,N2,N0,N1,N0,N2,N0,N1,
     & N0,N1,N0,N2,N0,M1,N0,M1,VBHHHH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S6(N1+1:M2,N0+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K2
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S6)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X14(N0+1:N2,N1+1:M2,N0+1:M1,N0+1:M1))
       X14=0.0d0
       call
     & sum2134(N0,N2,N1,M2,N0,M1,N0,M1,X14,S6,-1.000)
       deallocate(S6)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder4132(N2,N3,N0,N1,N2,N3,N0,N1,
     & N0,N1,N2,N3,N2,M2,N0,M1,VBHPHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S7(N1+1:M2,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S7)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X15(N2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       X15=0.0d0
       call
     & sum3124(N2,N3,N2,M2,N1,M2,N0,M1,X15,S7,-1.000)
C
       allocate(X16(N2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       X16=0.0d0
       call
     & sum3124(N2,N3,N2,M2,N1,M2,N0,M1,X16,S7,-1.000)
       deallocate(S7)
C
       allocate(D1(N1+1:N3,N0+1:N2,N1+1:M2,M1+1:N2))
       call reorder2341(N0,N2,N1,N3,N0,N2,N1,N3,
     & N1,N3,N0,N2,N1,M2,M1,N2,VBPHPH,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S8(N0+1:M1,N0+1:N2,N1+1:M2,M1+1:N2))
       I1=K8*K9*K2
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S8)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,N2,N1,M2,M1,N2,N0,M1,X13,S8, 1.000)
       deallocate(S8)
C
       allocate(D1(N1+1:N3,N0+1:N2,N1+1:M2,N0+1:M1))
       call reorder2341(N0,N2,N1,N3,N0,N2,N1,N3,
     & N1,N3,N0,N2,N1,M2,N0,M1,VBPHPH,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S9(N0+1:M1,N0+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9*K2
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S9)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,N2,N1,M2,N0,M1,N0,M1,X14,S9, 1.000)
       deallocate(S9)
C
       allocate(D1(N1+1:N3,N2+1:N3,N2+1:M2,N1+1:M2))
       call reorder2134(N2,N3,N1,N3,N2,N3,N1,M2,
     & N1,N3,N2,N3,N2,M2,N1,M2,VBAPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S10(N0+1:M1,N2+1:N3,N2+1:M2,N1+1:M2))
       I1=K9*K0*K4
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S10)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N2,N3,N2,M2,N1,M2,N0,M1,X15,S10, 1.000)
C
       call
     & sum4123(N2,N3,N2,M2,N1,M2,N0,M1,X16,S10, 1.000)
       deallocate(S10)
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
       allocate(X29(N0+1:M1,N0+1:M1))
       X29=0.0d0
       call
     & sum21(N0,M1,N0,M1,X29,Q1, 1.000)
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
       allocate(X30(M1+1:N1,N0+1:M1))
       X30=0.0d0
       call
     & sum21(M1,N1,N0,M1,X30,Q2, 1.000)
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
       call DMATMAT(I1,I2,I3,B1,B2,Q3)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X31(M2+1:N3,N1+1:M2))
       X31=0.0d0
       call
     & sum21(M2,N3,N1,M2,X31,Q3,-1.000)
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
       call DMATMAT(I1,I2,I3,B1,B2,Q4)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X32(N1+1:M2,N1+1:M2))
       X32=0.0d0
       call
     & sum21(N1,M2,N1,M2,X32,Q4,-1.000)
       deallocate(Q4)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder4312(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,M1,M2,N3,N0,M1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S11(N1+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S11)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder2314(N1,M2,N0,M1,M2,N3,N0,M1,
     & N0,M1,M2,N3,N1,M2,N0,M1,S11,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z114(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K0
       I3=K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z114)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z114,-1.000)
       deallocate(Z114)
       deallocate(S11)
C
       allocate(D1(N0+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder4312(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,M1,N1,M2,N3,N0,M1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S12(N1+1:M2,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S12)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder2314(N1,M2,M1,N1,M2,N3,N0,M1,
     & M1,N1,M2,N3,N1,M2,N0,M1,S12,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z115(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K0
       I3=K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,Z115)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z115,-1.000)
       deallocate(Z115)
       deallocate(S12)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder4312(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N0,M1,N1,M2,N0,M1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S13(N1+1:M2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S13)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder2314(N1,M2,N0,M1,N1,M2,N0,M1,
     & N0,M1,N1,M2,N1,M2,N0,M1,S13,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N1,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z116(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K0
       I3=K9*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z116)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z116,-1.000)
       deallocate(Z116)
       deallocate(S13)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder4312(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,M1,N1,N1,M2,N0,M1,VAHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S14(N1+1:M2,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S14)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder2314(N1,M2,M1,N1,N1,M2,N0,M1,
     & M1,N1,N1,M2,N1,M2,N0,M1,S14,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N1,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z117(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K0
       I3=K9*K7
       call DMATMAT(I1,I2,I3,D1,F2,Z117)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z117,-1.000)
       deallocate(Z117)
       deallocate(S14)
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
       X29=X29+Q5
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
       X30=X30+Q6
       deallocate(Q6)
C
       allocate(D1(N1+1:N3,N0+1:M1,M2+1:N3,N1+1:M2))
       call reorder2413(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N0,M1,M2,N3,N1,M2,VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S15(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K5
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S15)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,M1,M2,N3,N1,M2,
     & N0,M1,M2,N3,N1,M2,N0,M1,S15,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z120(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K0
       I3=K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z120)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z120,-1.000)
       deallocate(Z120)
       deallocate(S15)
C
       allocate(D1(N1+1:N3,M1+1:N1,M2+1:N3,N1+1:M2))
       call reorder2413(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,M1,N1,M2,N3,N1,M2,VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S16(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K7
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S16)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder2341(N0,M1,M1,N1,M2,N3,N1,M2,
     & M1,N1,M2,N3,N1,M2,N0,M1,S16,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z121(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K0
       I3=K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,Z121)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z121,-1.000)
       deallocate(Z121)
       deallocate(S16)
C
       allocate(D1(N1+1:N3,N0+1:M1,N1+1:M2,N1+1:M2))
       call reorder2413(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,N0,M1,N1,M2,N1,M2,VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S17(N0+1:M1,N0+1:M1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K5
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S17)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,M1,N1,M2,N1,M2,
     & N0,M1,N1,M2,N1,M2,N0,M1,S17,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N1,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z122(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K0
       I3=K9*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z122)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z122,-1.000)
       deallocate(Z122)
       deallocate(S17)
C
       allocate(D1(N1+1:N3,M1+1:N1,N1+1:M2,N1+1:M2))
       call reorder2413(N1,N3,N1,N3,N1,N3,N0,N1,
     & N1,N3,M1,N1,N1,M2,N1,M2,VAHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S18(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K7
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S18)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:M1))
       call reorder2341(N0,M1,M1,N1,N1,M2,N1,M2,
     & M1,N1,N1,M2,N1,M2,N0,M1,S18,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N1,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z123(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K0
       I3=K9*K7
       call DMATMAT(I1,I2,I3,D1,F2,Z123)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z123,-1.000)
       deallocate(Z123)
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
       call DMATVEC(I1,I3,D1,B2,Q7)
       deallocate(D1)
       deallocate(B2)
C
       X31=X31-Q7
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
       call DMATVEC(I1,I3,D1,B2,Q8)
       deallocate(D1)
       deallocate(B2)
C
       X32=X32-Q8
       deallocate(Q8)
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
       allocate(X44(M1+1:N2,N0+1:M1,M1+1:N2,N0+1:M1))
       X44=0.0d0
       call
     & sum4123(M1,N2,N0,M1,M1,N2,N0,M1,X44,S19, 1.000)
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
       allocate(X45(N0+1:M1,M1+1:N1,M1+1:N2,N0+1:M1))
       X45=0.0d0
       call
     & sum4123(N0,M1,M1,N1,M1,N2,N0,M1,X45,S20, 1.000)
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
       allocate(X46(M1+1:N2,M1+1:N1,M1+1:N2,N0+1:M1))
       X46=0.0d0
       call
     & sum4123(M1,N2,M1,N1,M1,N2,N0,M1,X46,S21, 1.000)
       deallocate(S21)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:M1,N0+1:M1))
       call reorder2341(N0,N2,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,N0,M1,N0,M1,VBHHPH,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S22(N0+1:M1,N0+1:M1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K5
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S22)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X47(N0+1:M1,N0+1:M1,N0+1:M1,N0+1:M1))
       X47=0.0d0
       call
     & sum4123(N0,M1,N0,M1,N0,M1,N0,M1,X47,S22, 1.000)
       deallocate(S22)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:M1,N0+1:M1))
       call reorder2341(N0,N2,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,N0,M1,N0,M1,VBHHPH,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S23(N0+1:M1,M1+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K8
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S23)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X48(M1+1:N2,N0+1:M1,N0+1:M1,N0+1:M1))
       X48=0.0d0
       call
     & sum4123(M1,N2,N0,M1,N0,M1,N0,M1,X48,S23, 1.000)
       deallocate(S23)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,N0+1:M1))
       call reorder2341(N0,N2,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,M1,N1,N0,M1,VBHHPH,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S24(N0+1:M1,N0+1:M1,M1+1:N1,N0+1:M1))
       I1=K5*K7*K5
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S24)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X49(N0+1:M1,M1+1:N1,N0+1:M1,N0+1:M1))
       X49=0.0d0
       call
     & sum4123(N0,M1,M1,N1,N0,M1,N0,M1,X49,S24, 1.000)
       deallocate(S24)
C
       allocate(D1(N1+1:N3,M1+1:N2,M1+1:N1,N0+1:M1))
       call reorder2341(N0,N2,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,M1,N1,N0,M1,VBHHPH,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S25(N0+1:M1,M1+1:N2,M1+1:N1,N0+1:M1))
       I1=K5*K7*K8
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S25)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X50(M1+1:N2,M1+1:N1,N0+1:M1,N0+1:M1))
       X50=0.0d0
       call
     & sum4123(M1,N2,M1,N1,N0,M1,N0,M1,X50,S25, 1.000)
       deallocate(S25)
C
       allocate(D1(N1+1:N3,N0+1:M1,M2+1:N3,N2+1:M2))
       call reorder2413(N2,N3,N1,N3,N2,N3,N0,N1,
     & N1,N3,N0,M1,M2,N3,N2,M2,VBHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S26(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K5
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S26)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X90(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       X90=0.0d0
       call
     & sum4123(N0,M1,M2,N3,N2,M2,N0,M1,X90,S26, 1.000)
C
       allocate(X91(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       X91=0.0d0
       call
     & sum4123(N0,M1,M2,N3,N2,M2,N0,M1,X91,S26, 1.000)
       deallocate(S26)
C
       allocate(D1(N1+1:N3,N0+1:M1,N2+1:M2,N2+1:M2))
       call reorder2413(N2,N3,N1,N3,N2,N3,N0,N1,
     & N1,N3,N0,M1,N2,M2,N2,M2,VBHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S28(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:M2))
       I1=K0*K0*K5
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S28)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X92(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:M1))
       X92=0.0d0
       call
     & sum4123(N0,M1,N2,M2,N2,M2,N0,M1,X92,S28, 1.000)
C
       allocate(X93(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:M1))
       X93=0.0d0
       call
     & sum4123(N0,M1,N2,M2,N2,M2,N0,M1,X93,S28, 1.000)
       deallocate(S28)
C
       allocate(D1(N1+1:N3,M1+1:N1,M2+1:N3,N2+1:M2))
       call reorder2413(N2,N3,N1,N3,N2,N3,N0,N1,
     & N1,N3,M1,N1,M2,N3,N2,M2,VBHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S27(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K7
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S27)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X94(M1+1:N1,M2+1:N3,N2+1:M2,N0+1:M1))
       X94=0.0d0
       call
     & sum4123(M1,N1,M2,N3,N2,M2,N0,M1,X94,S27, 1.000)
C
       allocate(X95(M1+1:N1,M2+1:N3,N2+1:M2,N0+1:M1))
       X95=0.0d0
       call
     & sum4123(M1,N1,M2,N3,N2,M2,N0,M1,X95,S27, 1.000)
       deallocate(S27)
C
       allocate(D1(N1+1:N3,M1+1:N1,N2+1:M2,N2+1:M2))
       call reorder2413(N2,N3,N1,N3,N2,N3,N0,N1,
     & N1,N3,M1,N1,N2,M2,N2,M2,VBHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S29(N0+1:M1,M1+1:N1,N2+1:M2,N2+1:M2))
       I1=K0*K0*K7
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S29)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X96(M1+1:N1,N2+1:M2,N2+1:M2,N0+1:M1))
       X96=0.0d0
       call
     & sum4123(M1,N1,N2,M2,N2,M2,N0,M1,X96,S29, 1.000)
C
       allocate(X97(M1+1:N1,N2+1:M2,N2+1:M2,N0+1:M1))
       X97=0.0d0
       call
     & sum4123(M1,N1,N2,M2,N2,M2,N0,M1,X97,S29, 1.000)
       deallocate(S29)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder4321(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,M1,N2,M2,N3,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S30(N1+1:M2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S30)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X98(M1+1:N2,M2+1:N3,N1+1:M2,M1+1:N2))
       X98=0.0d0
       call
     & sum3124(M1,N2,M2,N3,N1,M2,M1,N2,X98,S30, 1.000)
       deallocate(S30)
C
       allocate(D1(N0+1:N1,M1+1:N2,N1+1:M2,M1+1:N2))
       call reorder4321(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,M1,N2,N1,M2,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S31(N1+1:M2,M1+1:N2,N1+1:M2,M1+1:N2))
       I1=K8*K9*K8
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S31)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X99(M1+1:N2,N1+1:M2,N1+1:M2,M1+1:N2))
       X99=0.0d0
       call
     & sum3124(M1,N2,N1,M2,N1,M2,M1,N2,X99,S31, 1.000)
       deallocate(S31)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M1+1:N2))
       call reorder4231(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q9(M1+1:N2,M1+1:N2))
       I1=K8*K8
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q9)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X100(M1+1:N2,M1+1:N2))
       X100=0.0d0
       X100=X100+Q9
       deallocate(Q9)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder4321(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N0,M1,M2,N3,N0,M1,VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S32(N1+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S32)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X101(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       X101=0.0d0
       call
     & sum3124(N0,M1,M2,N3,N1,M2,N0,M1,X101,S32, 1.000)
       deallocate(S32)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder4321(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,M1,N2,M2,N3,N0,M1,VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S33(N1+1:M2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S33)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X102(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:M1))
       X102=0.0d0
       call
     & sum3124(M1,N2,M2,N3,N1,M2,N0,M1,X102,S33, 1.000)
       deallocate(S33)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder4321(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N0,M1,N1,M2,N0,M1,VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S34(N1+1:M2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S34)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X103(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:M1))
       X103=0.0d0
       call
     & sum3124(N0,M1,N1,M2,N1,M2,N0,M1,X103,S34, 1.000)
       deallocate(S34)
C
       allocate(D1(N0+1:N1,M1+1:N2,N1+1:M2,N0+1:M1))
       call reorder4321(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,M1,N2,N1,M2,N0,M1,VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S35(N1+1:M2,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9*K8
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S35)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X104(M1+1:N2,N1+1:M2,N1+1:M2,N0+1:M1))
       X104=0.0d0
       call
     & sum3124(M1,N2,N1,M2,N1,M2,N0,M1,X104,S35, 1.000)
       deallocate(S35)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder4231(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,N0,M1,VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q10(N0+1:M1,N0+1:M1))
       I1=K5*K5
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q10)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X105(N0+1:M1,N0+1:M1))
       X105=0.0d0
       X105=X105+Q10
       deallocate(Q10)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N0+1:M1))
       call reorder4231(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,N0,M1,VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q11(M1+1:N2,N0+1:M1))
       I1=K5*K8
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q11)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X106(M1+1:N2,N0+1:M1))
       X106=0.0d0
       X106=X106+Q11
       deallocate(Q11)
C
       allocate(D1(N0+1:N1,M2+1:N3,M2+1:N3,N2+1:M2))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,M2,N3,M2,N3,N2,M2,VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S36(N1+1:M2,M2+1:N3,M2+1:N3,N2+1:M2))
       I1=K0*K6*K6
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S36)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X65(M2+1:N3,M2+1:N3,N2+1:M2,N1+1:M2))
       X65=0.0d0
       call
     & sum4123(M2,N3,M2,N3,N2,M2,N1,M2,X65,S36,-1.000)
C
       allocate(X69(M2+1:N3,M2+1:N3,N2+1:M2,N1+1:M2))
       X69=0.0d0
       call
     & sum4123(M2,N3,M2,N3,N2,M2,N1,M2,X69,S36,-1.000)
       deallocate(S36)
C
       allocate(D1(N0+1:N1,M2+1:N3,N1+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,M2,N3,N1,M2,N2,M2,VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S38(N1+1:M2,M2+1:N3,N1+1:M2,N2+1:M2))
       I1=K0*K9*K6
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S38)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X67(M2+1:N3,N1+1:M2,N2+1:M2,N1+1:M2))
       X67=0.0d0
       call
     & sum4123(M2,N3,N1,M2,N2,M2,N1,M2,X67,S38,-1.000)
C
       allocate(X71(M2+1:N3,N1+1:M2,N2+1:M2,N1+1:M2))
       X71=0.0d0
       call
     & sum4123(M2,N3,N1,M2,N2,M2,N1,M2,X71,S38,-1.000)
       deallocate(S38)
C
       allocate(D1(N0+1:N1,N1+1:N3,M2+1:N3,N2+1:M2))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,M2,N3,N2,M2,VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q12(M2+1:N3,N2+1:M2))
       I1=K0*K6
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q12)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X107(M2+1:N3,N2+1:M2))
       X107=0.0d0
       X107=X107+Q12
C
       allocate(X108(M2+1:N3,N2+1:M2))
       X108=0.0d0
       X108=X108+Q12
       deallocate(Q12)
C
       allocate(D1(N0+1:N1,N2+1:M2,M2+1:N3,N2+1:M2))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N2,M2,M2,N3,N2,M2,VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S37(N1+1:M2,N2+1:M2,M2+1:N3,N2+1:M2))
       I1=K0*K6*K0
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S37)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X66(N2+1:M2,M2+1:N3,N2+1:M2,N1+1:M2))
       X66=0.0d0
       call
     & sum4123(N2,M2,M2,N3,N2,M2,N1,M2,X66,S37,-1.000)
C
       allocate(X70(N2+1:M2,M2+1:N3,N2+1:M2,N1+1:M2))
       X70=0.0d0
       call
     & sum4123(N2,M2,M2,N3,N2,M2,N1,M2,X70,S37,-1.000)
       deallocate(S37)
C
       allocate(D1(N0+1:N1,N2+1:M2,N1+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N2,M2,N1,M2,N2,M2,VBHPPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S39(N1+1:M2,N2+1:M2,N1+1:M2,N2+1:M2))
       I1=K0*K9*K0
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S39)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X68(N2+1:M2,N1+1:M2,N2+1:M2,N1+1:M2))
       X68=0.0d0
       call
     & sum4123(N2,M2,N1,M2,N2,M2,N1,M2,X68,S39,-1.000)
C
       allocate(X72(N2+1:M2,N1+1:M2,N2+1:M2,N1+1:M2))
       X72=0.0d0
       call
     & sum4123(N2,M2,N1,M2,N2,M2,N1,M2,X72,S39,-1.000)
       deallocate(S39)
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
       allocate(X109(N2+1:M2,N2+1:M2))
       X109=0.0d0
       X109=X109+Q13
C
       allocate(X110(N2+1:M2,N2+1:M2))
       X110=0.0d0
       X110=X110+Q13
       deallocate(Q13)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N0,M1,M2,N3,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S40(N1+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S40)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder2314(N1,M2,N0,M1,M2,N3,N0,M1,
     & N0,M1,M2,N3,N1,M2,N0,M1,S40,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z162(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K0
       I3=K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z162)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z162,-1.000)
       deallocate(Z162)
       deallocate(S40)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,M1,N2,M2,N3,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S41(N1+1:M2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S41)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder2314(N1,M2,M1,N2,M2,N3,N0,M1,
     & M1,N2,M2,N3,N1,M2,N0,M1,S41,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder512346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z163(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K0
       I3=K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z163)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z163, 1.000)
       deallocate(Z163)
       deallocate(S41)
C
       allocate(D1(N0+1:N1,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N0,M1,N2,M2,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S42(N1+1:M2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S42)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder2314(N1,M2,N0,M1,N2,M2,N0,M1,
     & N0,M1,N2,M2,N1,M2,N0,M1,S42,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z164(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K0
       I3=K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z164)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z164,-1.000)
       deallocate(Z164)
       deallocate(S42)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,M1,N2,N2,M2,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S43(N1+1:M2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S43)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder2314(N1,M2,M1,N2,N2,M2,N0,M1,
     & M1,N2,N2,M2,N1,M2,N0,M1,S43,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder512346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z165(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K0
       I3=K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z165)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z165, 1.000)
       deallocate(Z165)
       deallocate(S43)
C
       allocate(D1(N1+1:N3,N0+1:M1,M2+1:N3,N1+1:M2))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,N0,M1,M2,N3,N1,M2,VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S44(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K5
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S44)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,M1,M2,N3,N1,M2,
     & N0,M1,M2,N3,N1,M2,N0,M1,S44,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z166(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K0
       I3=K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z166)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z166, 1.000)
       deallocate(Z166)
       deallocate(S44)
C
       allocate(D1(N1+1:N3,M1+1:N2,M2+1:N3,N1+1:M2))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,M1,N2,M2,N3,N1,M2,VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S45(N0+1:M1,M1+1:N2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K8
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S45)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder2341(N0,M1,M1,N2,M2,N3,N1,M2,
     & M1,N2,M2,N3,N1,M2,N0,M1,S45,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder512346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z167(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K0
       I3=K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z167)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z167,-1.000)
       deallocate(Z167)
       deallocate(S45)
C
       allocate(D1(N1+1:N3,N0+1:M1,N2+1:M2,N1+1:M2))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,N0,M1,N2,M2,N1,M2,VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S46(N0+1:M1,N0+1:M1,N2+1:M2,N1+1:M2))
       I1=K9*K0*K5
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S46)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,M1,N2,M2,N1,M2,
     & N0,M1,N2,M2,N1,M2,N0,M1,S46,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z168(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K0
       I3=K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z168)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z168, 1.000)
       deallocate(Z168)
       deallocate(S46)
C
       allocate(D1(N1+1:N3,M1+1:N2,N2+1:M2,N1+1:M2))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,M1,N2,N2,M2,N1,M2,VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S47(N0+1:M1,M1+1:N2,N2+1:M2,N1+1:M2))
       I1=K9*K0*K8
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S47)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder2341(N0,M1,M1,N2,N2,M2,N1,M2,
     & M1,N2,N2,M2,N1,M2,N0,M1,S47,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder512346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z169(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K0
       I3=K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z169)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z169,-1.000)
       deallocate(Z169)
       deallocate(S47)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,N0+1:M1))
       call reorder3412(N0,N2,N0,N1,N0,N2,N0,N1,
     & N0,N2,N0,N1,M1,N2,N0,M1,VBHHHH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S48(N2+1:M2,N0+1:N1,M1+1:N2,N0+1:M1))
       I1=K5*K8*K1
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S48)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N2,M2,M1,N2,N0,M1,X1,S48,-1.000)
       deallocate(S48)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,N0+1:M1))
       call reorder3412(N0,N2,N0,N1,N0,N2,N0,N1,
     & N0,N2,N0,N1,M1,N2,N0,M1,VBHHHH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S49(N2+1:M2,N0+1:N1,M1+1:N2,N0+1:M1))
       I1=K5*K8*K1
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S49)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N2,M2,M1,N2,N0,M1,X2,S49,-1.000)
       deallocate(S49)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:M2,N0+1:M1))
       call reorder1432(N2,N3,N0,N1,N2,N3,N0,N1,
     & N2,N3,N0,N1,N2,M2,N0,M1,VBHPHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S50(M1+1:N2,N0+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K1
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S50)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N1,N2,M2,M1,N2,N0,M1,X1,S50, 1.000)
       deallocate(S50)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:M2,N0+1:M1))
       call reorder1432(N2,N3,N0,N1,N2,N3,N0,N1,
     & N2,N3,N0,N1,N2,M2,N0,M1,VBHPHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S51(M1+1:N2,N0+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K1
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S51)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N1,N2,M2,M1,N2,N0,M1,X2,S51, 1.000)
       deallocate(S51)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:M1,N0+1:M1))
       call reorder3412(N0,N2,N0,N1,N0,N2,N0,N1,
     & N0,N2,N0,N1,N0,M1,N0,M1,VBHHHH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S52(N2+1:M2,N0+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K1
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S52)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N2,M2,N0,M1,N0,M1,X3,S52,-1.000)
       deallocate(S52)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:M1,N0+1:M1))
       call reorder3412(N0,N2,N0,N1,N0,N2,N0,N1,
     & N0,N2,N0,N1,N0,M1,N0,M1,VBHHHH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S53(N2+1:M2,N0+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K1
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S53)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N2,M2,N0,M1,N0,M1,X4,S53,-1.000)
       deallocate(S53)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:M2,N0+1:M1))
       call reorder1432(N2,N3,N0,N1,N2,N3,N0,N1,
     & N2,N3,N0,N1,N2,M2,N0,M1,VBHPHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S54(N0+1:M1,N0+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K1
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S54)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N1,N2,M2,N0,M1,N0,M1,X3,S54, 1.000)
       deallocate(S54)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:M2,N0+1:M1))
       call reorder1432(N2,N3,N0,N1,N2,N3,N0,N1,
     & N2,N3,N0,N1,N2,M2,N0,M1,VBHPHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S55(N0+1:M1,N0+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K1
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S55)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N1,N2,M2,N0,M1,N0,M1,X4,S55, 1.000)
       deallocate(S55)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,M1+1:N2))
       call reorder3241(N0,N2,N1,N3,N0,N2,N1,N3,
     & N0,N2,N1,N3,N1,M2,M1,N2,VBPHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S56(N2+1:M2,N1+1:N3,N1+1:M2,M1+1:N2))
       I1=K8*K9*K3
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S56)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,N2,M2,N1,M2,M1,N2,X5,S56,-1.000)
       deallocate(S56)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,M1+1:N2))
       call reorder3241(N0,N2,N1,N3,N0,N2,N1,N3,
     & N0,N2,N1,N3,N1,M2,M1,N2,VBPHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S57(N2+1:M2,N1+1:N3,N1+1:M2,M1+1:N2))
       I1=K8*K9*K3
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S57)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,N2,M2,N1,M2,M1,N2,X6,S57,-1.000)
       deallocate(S57)
C
       allocate(D1(N2+1:N3,N1+1:N3,N2+1:M2,N1+1:M2))
       call reorder1234(N2,N3,N1,N3,N2,N3,N1,M2,
     & N2,N3,N1,N3,N2,M2,N1,M2,VBAPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S58(M1+1:N2,N1+1:N3,N2+1:M2,N1+1:M2))
       I1=K9*K0*K3
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S58)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N1,N3,N2,M2,N1,M2,M1,N2,X5,S58, 1.000)
       deallocate(S58)
C
       allocate(D1(N2+1:N3,N1+1:N3,N2+1:M2,N1+1:M2))
       call reorder1234(N2,N3,N1,N3,N2,N3,N1,M2,
     & N2,N3,N1,N3,N2,M2,N1,M2,VBAPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S59(M1+1:N2,N1+1:N3,N2+1:M2,N1+1:M2))
       I1=K9*K0*K3
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S59)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N1,N3,N2,M2,N1,M2,M1,N2,X6,S59, 1.000)
       deallocate(S59)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3241(N0,N2,N1,N3,N0,N2,N1,N3,
     & N0,N2,N1,N3,N1,M2,N0,M1,VBPHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S60(N2+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S60)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,N2,M2,N1,M2,N0,M1,X7,S60,-1.000)
       deallocate(S60)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3241(N0,N2,N1,N3,N0,N2,N1,N3,
     & N0,N2,N1,N3,N1,M2,N0,M1,VBPHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S61(N2+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S61)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,N2,M2,N1,M2,N0,M1,X8,S61,-1.000)
       deallocate(S61)
C
       allocate(D1(N2+1:N3,N1+1:N3,N2+1:M2,N1+1:M2))
       call reorder1234(N2,N3,N1,N3,N2,N3,N1,M2,
     & N2,N3,N1,N3,N2,M2,N1,M2,VBAPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S62(N0+1:M1,N1+1:N3,N2+1:M2,N1+1:M2))
       I1=K9*K0*K3
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S62)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N1,N3,N2,M2,N1,M2,N0,M1,X7,S62, 1.000)
       deallocate(S62)
C
       allocate(D1(N2+1:N3,N1+1:N3,N2+1:M2,N1+1:M2))
       call reorder1234(N2,N3,N1,N3,N2,N3,N1,M2,
     & N2,N3,N1,N3,N2,M2,N1,M2,VBAPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S63(N0+1:M1,N1+1:N3,N2+1:M2,N1+1:M2))
       I1=K9*K0*K3
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S63)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N1,N3,N2,M2,N1,M2,N0,M1,X8,S63, 1.000)
       deallocate(S63)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,M1,M1,N2,VCHHHH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S64(N2+1:M2,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S64)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X9(N0+1:N2,N2+1:M2,N0+1:M1,M1+1:N2))
       X9=0.0d0
       call
     & sum2134(N0,N2,N2,M2,N0,M1,M1,N2,X9,S64,-1.000)
       deallocate(S64)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,M1,M1,N2,VCHHHH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S65(N2+1:M2,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S65)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X10(N0+1:N2,N2+1:M2,N0+1:M1,M1+1:N2))
       X10=0.0d0
       call
     & sum2134(N0,N2,N2,M2,N0,M1,M1,N2,X10,S65,-1.000)
       deallocate(S65)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:M2,M1+1:N2))
       call reorder1432(N2,N3,N0,N2,N2,N3,N0,N2,
     & N2,N3,N0,N2,N2,M2,M1,N2,VCHPHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S66(N0+1:M1,N0+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K2
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S66)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N2,N2,M2,N0,M1,M1,N2,X9,S66, 1.000)
       deallocate(S66)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:M2,M1+1:N2))
       call reorder1432(N2,N3,N0,N2,N2,N3,N0,N2,
     & N2,N3,N0,N2,N2,M2,M1,N2,VCHPHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S67(N0+1:M1,N0+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K2
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S67)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N2,N2,M2,N0,M1,M1,N2,X10,S67, 1.000)
       deallocate(S67)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder4132(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,VCHPHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S68(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S68)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X11(N2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       X11=0.0d0
       call
     & sum3124(N2,N3,N2,M2,N2,M2,M1,N2,X11,S68,-1.000)
       deallocate(S68)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder4132(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,VCHPHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S69(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S69)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,N2,M2,M1,N2,X11,S69, 1.000)
       deallocate(S69)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:M2,N0+1:M1))
       call reorder1432(N2,N3,N0,N2,N2,N3,N0,N2,
     & N2,N3,N0,N2,N2,M2,N0,M1,VCHPHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S70(M1+1:N2,N0+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K2
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S70)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,N2,N2,M2,N0,M1,M1,N2,X9,S70,-1.000)
       deallocate(S70)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:M2,N0+1:M1))
       call reorder1432(N2,N3,N0,N2,N2,N3,N0,N2,
     & N2,N3,N0,N2,N2,M2,N0,M1,VCHPHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S71(M1+1:N2,N0+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K2
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S71)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,N2,N2,M2,N0,M1,M1,N2,X10,S71,-1.000)
       deallocate(S71)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:M2,N2+1:M2))
       call reorder1234(N2,N3,N2,N3,N2,N3,N2,M2,
     & N2,N3,N2,N3,N2,M2,N2,M2,VCAPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S72(M1+1:N2,N2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K4
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S72)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N2,N3,N2,M2,N2,M2,M1,N2,X11,S72,-1.000)
       deallocate(S72)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder4132(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,VCHPHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S73(N2+1:M2,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S73)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X12(N2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1))
       X12=0.0d0
       call
     & sum3124(N2,N3,N2,M2,N2,M2,N0,M1,X12,S73,-1.000)
       deallocate(S73)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder4132(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,VCHPHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S74(N2+1:M2,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S74)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,N2,M2,N0,M1,X12,S74, 1.000)
       deallocate(S74)
C
       allocate(D1(N2+1:N3,N2+1:N3,N2+1:M2,N2+1:M2))
       call reorder1234(N2,N3,N2,N3,N2,N3,N2,M2,
     & N2,N3,N2,N3,N2,M2,N2,M2,VCAPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S75(N0+1:M1,N2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K4
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S75)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N2,N3,N2,M2,N2,M2,N0,M1,X12,S75,-1.000)
       deallocate(S75)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:M2,N0+1:M1))
       call reorder3124(N0,N2,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,N2,N1,M2,N0,M1,VBHPPH,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S76(M1+1:N2,N0+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9*K2
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S76)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N2,N1,M2,M1,N2,N0,M1,X13,S76, 1.000)
       deallocate(S76)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:M2,N0+1:M1))
       call reorder3124(N0,N2,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,N2,N1,M2,N0,M1,VBHPPH,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S77(N0+1:M1,N0+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9*K2
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S77)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N2,N1,M2,N0,M1,N0,M1,X14,S77, 1.000)
       deallocate(S77)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,VBHPPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S78(N2+1:M2,N2+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K4
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S78)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,N1,M2,N0,M1,X15,S78,-1.000)
       deallocate(S78)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,VBHPPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S79(N2+1:M2,N2+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K4
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S79)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,N1,M2,N0,M1,X16,S79,-1.000)
       deallocate(S79)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,M2,N3,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S80(N2+1:M2,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K7
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S80)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X17(M1+1:N1,M2+1:N3,N2+1:M2,M1+1:N2))
       X17=0.0d0
       call
     & sum3124(M1,N1,M2,N3,N2,M2,M1,N2,X17,S80,-1.000)
       deallocate(S80)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,N1,M2,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S81(N2+1:M2,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K7
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S81)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X18(M1+1:N1,N1+1:M2,N2+1:M2,M1+1:N2))
       X18=0.0d0
       call
     & sum3124(M1,N1,N1,M2,N2,M2,M1,N2,X18,S81,-1.000)
       deallocate(S81)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,M2,N3,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S82(N2+1:M2,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K7
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S82)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X19(M1+1:N1,M2+1:N3,N2+1:M2,M1+1:N2))
       X19=0.0d0
       call
     & sum3124(M1,N1,M2,N3,N2,M2,M1,N2,X19,S82,-1.000)
       deallocate(S82)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,N1,M2,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S83(N2+1:M2,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K7
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S83)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X20(M1+1:N1,N1+1:M2,N2+1:M2,M1+1:N2))
       X20=0.0d0
       call
     & sum3124(M1,N1,N1,M2,N2,M2,M1,N2,X20,S83,-1.000)
       deallocate(S83)
C
       allocate(D1(N2+1:N3,M1+1:N1,M2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,M1,N1,M2,N3,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S84(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K7
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S84)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,M2,N3,N2,M2,M1,N2,X17,S84, 1.000)
       deallocate(S84)
C
       allocate(D1(N2+1:N3,M1+1:N1,N1+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,M1,N1,N1,M2,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S85(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:M2))
       I1=K0*K9*K7
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S85)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,N1,M2,N2,M2,M1,N2,X18,S85, 1.000)
       deallocate(S85)
C
       allocate(D1(N2+1:N3,M1+1:N1,M2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,M1,N1,M2,N3,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S86(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K7
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S86)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,M2,N3,N2,M2,M1,N2,X19,S86, 1.000)
       deallocate(S86)
C
       allocate(D1(N2+1:N3,M1+1:N1,N1+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,M1,N1,N1,M2,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S87(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:M2))
       I1=K0*K9*K7
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S87)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,N1,M2,N2,M2,M1,N2,X20,S87, 1.000)
       deallocate(S87)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,M1,M2,N3,N0,M1,VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S88(N2+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S88)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X21(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       X21=0.0d0
       call
     & sum3124(N0,M1,M2,N3,N2,M2,N0,M1,X21,S88,-1.000)
       deallocate(S88)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,M2,N3,N0,M1,VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S89(N2+1:M2,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S89)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X22(M1+1:N1,M2+1:N3,N2+1:M2,N0+1:M1))
       X22=0.0d0
       call
     & sum3124(M1,N1,M2,N3,N2,M2,N0,M1,X22,S89,-1.000)
       deallocate(S89)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,M1,N1,M2,N0,M1,VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S90(N2+1:M2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S90)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X23(N0+1:M1,N1+1:M2,N2+1:M2,N0+1:M1))
       X23=0.0d0
       call
     & sum3124(N0,M1,N1,M2,N2,M2,N0,M1,X23,S90,-1.000)
       deallocate(S90)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,N1,M2,N0,M1,VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S91(N2+1:M2,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S91)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X24(M1+1:N1,N1+1:M2,N2+1:M2,N0+1:M1))
       X24=0.0d0
       call
     & sum3124(M1,N1,N1,M2,N2,M2,N0,M1,X24,S91,-1.000)
       deallocate(S91)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,M1,M2,N3,N0,M1,VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S92(N2+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S92)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X25(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       X25=0.0d0
       call
     & sum3124(N0,M1,M2,N3,N2,M2,N0,M1,X25,S92,-1.000)
       deallocate(S92)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,M2,N3,N0,M1,VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S93(N2+1:M2,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S93)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X26(M1+1:N1,M2+1:N3,N2+1:M2,N0+1:M1))
       X26=0.0d0
       call
     & sum3124(M1,N1,M2,N3,N2,M2,N0,M1,X26,S93,-1.000)
       deallocate(S93)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,M1,N1,M2,N0,M1,VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S94(N2+1:M2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S94)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X27(N0+1:M1,N1+1:M2,N2+1:M2,N0+1:M1))
       X27=0.0d0
       call
     & sum3124(N0,M1,N1,M2,N2,M2,N0,M1,X27,S94,-1.000)
       deallocate(S94)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,M1,N1,N1,M2,N0,M1,VBHHPH,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S95(N2+1:M2,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S95)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X28(M1+1:N1,N1+1:M2,N2+1:M2,N0+1:M1))
       X28=0.0d0
       call
     & sum3124(M1,N1,N1,M2,N2,M2,N0,M1,X28,S95,-1.000)
       deallocate(S95)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,M1,M2,N3,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S96(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K5
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S96)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,M2,N3,N2,M2,N0,M1,X21,S96, 1.000)
       deallocate(S96)
C
       allocate(D1(N2+1:N3,M1+1:N1,M2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,M1,N1,M2,N3,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S97(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K7
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S97)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,M2,N3,N2,M2,N0,M1,X22,S97, 1.000)
       deallocate(S97)
C
       allocate(D1(N2+1:N3,N0+1:M1,N1+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,M1,N1,M2,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S98(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:M2))
       I1=K0*K9*K5
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S98)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,N1,M2,N2,M2,N0,M1,X23,S98, 1.000)
       deallocate(S98)
C
       allocate(D1(N2+1:N3,M1+1:N1,N1+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,M1,N1,N1,M2,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S99(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:M2))
       I1=K0*K9*K7
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S99)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,N1,M2,N2,M2,N0,M1,X24,S99, 1.000)
       deallocate(S99)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,M1,M2,N3,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S100(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K5
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S100)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,M2,N3,N2,M2,N0,M1,X25,S100, 1.000)
       deallocate(S100)
C
       allocate(D1(N2+1:N3,M1+1:N1,M2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,M1,N1,M2,N3,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S101(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K7
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S101)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,M2,N3,N2,M2,N0,M1,X26,S101, 1.000)
       deallocate(S101)
C
       allocate(D1(N2+1:N3,N0+1:M1,N1+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,M1,N1,M2,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S102(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:M2))
       I1=K0*K9*K5
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S102)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,N1,M2,N2,M2,N0,M1,X27,S102, 1.000)
       deallocate(S102)
C
       allocate(D1(N2+1:N3,M1+1:N1,N1+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,M1,N1,N1,M2,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S103(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:M2))
       I1=K0*K9*K7
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S103)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N1,N1,M2,N2,M2,N0,M1,X28,S103, 1.000)
       deallocate(S103)
C
       allocate(B1(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,FBHP,B1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(Q14(M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,B1,B2,Q14)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X33(M1+1:N2,M1+1:N2))
       X33=0.0d0
       call
     & sum21(M1,N2,M1,N2,X33,Q14, 1.000)
       deallocate(Q14)
C
       allocate(B1(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,FBHP,B1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(Q15(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,B1,B2,Q15)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X34(N0+1:M1,N0+1:M1))
       X34=0.0d0
       call
     & sum21(N0,M1,N0,M1,X34,Q15, 1.000)
       deallocate(Q15)
C
       allocate(B1(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,FBHP,B1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(Q16(N0+1:M1,M1+1:N2))
       I1=K8
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,B1,B2,Q16)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X35(M1+1:N2,N0+1:M1))
       X35=0.0d0
       call
     & sum21(M1,N2,N0,M1,X35,Q16, 1.000)
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
       call DMATMAT(I1,I2,I3,B1,B2,Q17)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X36(M2+1:N3,N2+1:M2))
       X36=0.0d0
       call
     & sum21(M2,N3,N2,M2,X36,Q17,-1.000)
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
       allocate(X37(N2+1:M2,N2+1:M2))
       X37=0.0d0
       call
     & sum21(N2,M2,N2,M2,X37,Q18,-1.000)
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
       call DMATMAT(I1,I2,I3,B1,B2,Q19)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X38(M2+1:N3,N2+1:M2))
       X38=0.0d0
       call
     & sum21(M2,N3,N2,M2,X38,Q19,-1.000)
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
       call DMATMAT(I1,I2,I3,B1,B2,Q20)
       deallocate(B1)
       deallocate(B2)
C
       allocate(X39(N2+1:M2,N2+1:M2))
       X39=0.0d0
       call
     & sum21(N2,M2,N2,M2,X39,Q20,-1.000)
       deallocate(Q20)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:M1,N0+1:M1))
       call reorder1342(N2,N3,N0,N1,N0,N2,N0,N1,
     & N2,N3,M1,N2,N0,M1,N0,M1,VBHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S104(M1+1:N2,M1+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K8
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S104)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,N0,M1,M1,N2,N0,M1,X44,S104, 1.000)
       deallocate(S104)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N1,N0+1:M1))
       call reorder1342(N2,N3,N0,N1,N0,N2,N0,N1,
     & N2,N3,N0,M1,M1,N1,N0,M1,VBHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S105(M1+1:N2,N0+1:M1,M1+1:N1,N0+1:M1))
       I1=K5*K7*K5
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S105)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M1,N1,M1,N2,N0,M1,X45,S105, 1.000)
       deallocate(S105)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N1,N0+1:M1))
       call reorder1342(N2,N3,N0,N1,N0,N2,N0,N1,
     & N2,N3,M1,N2,M1,N1,N0,M1,VBHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S106(M1+1:N2,M1+1:N2,M1+1:N1,N0+1:M1))
       I1=K5*K7*K8
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S106)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M1,N1,M1,N2,N0,M1,X46,S106, 1.000)
       deallocate(S106)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:M1,N0+1:M1))
       call reorder1342(N2,N3,N0,N1,N0,N2,N0,N1,
     & N2,N3,N0,M1,N0,M1,N0,M1,VBHHHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S107(N0+1:M1,N0+1:M1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K5
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S107)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N0,M1,N0,M1,N0,M1,X47,S107, 1.000)
       deallocate(S107)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:M1,N0+1:M1))
       call reorder1342(N2,N3,N0,N1,N0,N2,N0,N1,
     & N2,N3,M1,N2,N0,M1,N0,M1,VBHHHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S108(N0+1:M1,M1+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K8
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S108)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,N0,M1,N0,M1,N0,M1,X48,S108, 1.000)
       deallocate(S108)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N1,N0+1:M1))
       call reorder1342(N2,N3,N0,N1,N0,N2,N0,N1,
     & N2,N3,N0,M1,M1,N1,N0,M1,VBHHHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S109(N0+1:M1,N0+1:M1,M1+1:N1,N0+1:M1))
       I1=K5*K7*K5
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S109)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M1,N1,N0,M1,N0,M1,X49,S109, 1.000)
       deallocate(S109)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N1,N0+1:M1))
       call reorder1342(N2,N3,N0,N1,N0,N2,N0,N1,
     & N2,N3,M1,N2,M1,N1,N0,M1,VBHHHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S110(N0+1:M1,M1+1:N2,M1+1:N1,N0+1:M1))
       I1=K5*K7*K8
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S110)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M1,N1,N0,M1,N0,M1,X50,S110, 1.000)
       deallocate(S110)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N0,M1,M2,N3,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S111(N2+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S111)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X51(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       X51=0.0d0
       call
     & sum3124(N0,M1,M2,N3,N2,M2,N0,M1,X51,S111,-1.000)
       deallocate(S111)
C
       call sumx2431(N2,N3,N0,N1,N2,N3,N0,N1,
     & N0,M1,M2,N3,N2,M2,N0,M1,X51,VBHPHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2))
       call reorder612345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N2,M2,N1,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z51(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K9*K0
       I3=K6*K5
       call DMATMAT(I1,I2,I3,X51,F2,Z51)
       deallocate(F2)
C
       call
     & sum234516(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z51,-1.000)
       deallocate(Z51)
       deallocate(X51)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,M1,N1,M2,N3,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S112(N2+1:M2,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S112)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X52(M1+1:N1,M2+1:N3,N2+1:M2,N0+1:M1))
       X52=0.0d0
       call
     & sum3124(M1,N1,M2,N3,N2,M2,N0,M1,X52,S112,-1.000)
       deallocate(S112)
C
       call sumx2431(N2,N3,N0,N1,N2,N3,N0,N1,
     & M1,N1,M2,N3,N2,M2,N0,M1,X52,VBHPHP, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2))
       call reorder612345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,N2,M2,N1,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z52(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K9*K0
       I3=K6*K7
       call DMATMAT(I1,I2,I3,X52,F2,Z52)
       deallocate(F2)
C
       call
     & sum234516(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z52,-1.000)
       deallocate(Z52)
       deallocate(X52)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N0,M1,N2,M2,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S113(N2+1:M2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S113)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X53(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:M1))
       X53=0.0d0
       call
     & sum3124(N0,M1,N2,M2,N2,M2,N0,M1,X53,S113,-1.000)
       deallocate(S113)
C
       call sumx2431(N2,N3,N0,N1,N2,N3,N0,N1,
     & N0,M1,N2,M2,N2,M2,N0,M1,X53,VBHPHP, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2))
       call reorder612345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z53(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K9*K0
       I3=K0*K5
       call DMATMAT(I1,I2,I3,X53,F2,Z53)
       deallocate(F2)
C
       call
     & sum234516(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z53,-1.000)
       deallocate(Z53)
       deallocate(X53)
C
       allocate(D1(N0+1:N2,M1+1:N1,N2+1:M2,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,M1,N1,N2,M2,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S114(N2+1:M2,M1+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K7
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S114)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X54(M1+1:N1,N2+1:M2,N2+1:M2,N0+1:M1))
       X54=0.0d0
       call
     & sum3124(M1,N1,N2,M2,N2,M2,N0,M1,X54,S114,-1.000)
       deallocate(S114)
C
       call sumx2431(N2,N3,N0,N1,N2,N3,N0,N1,
     & M1,N1,N2,M2,N2,M2,N0,M1,X54,VBHPHP, 1.000)
C
       allocate(F2(M1+1:N1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2))
       call reorder612345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z54(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K9*K0
       I3=K0*K7
       call DMATMAT(I1,I2,I3,X54,F2,Z54)
       deallocate(F2)
C
       call
     & sum234516(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z54,-1.000)
       deallocate(Z54)
       deallocate(X54)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N0,M1,M2,N3,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S115(N2+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S115)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X55(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       X55=0.0d0
       call
     & sum3124(N0,M1,M2,N3,N2,M2,N0,M1,X55,S115,-1.000)
       deallocate(S115)
C
       call sumx2431(N2,N3,N0,N1,N2,N3,N0,N1,
     & N0,M1,M2,N3,N2,M2,N0,M1,X55,VBHPHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2))
       call reorder612345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N2,M2,N1,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z55(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K9*K0
       I3=K6*K5
       call DMATMAT(I1,I2,I3,X55,F2,Z55)
       deallocate(F2)
C
       call
     & sum134526(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z55, 1.000)
       deallocate(Z55)
       deallocate(X55)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,M1,N1,M2,N3,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S116(N2+1:M2,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S116)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X56(M1+1:N1,M2+1:N3,N2+1:M2,N0+1:M1))
       X56=0.0d0
       call
     & sum3124(M1,N1,M2,N3,N2,M2,N0,M1,X56,S116,-1.000)
       deallocate(S116)
C
       call sumx2431(N2,N3,N0,N1,N2,N3,N0,N1,
     & M1,N1,M2,N3,N2,M2,N0,M1,X56,VBHPHP, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2))
       call reorder612345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,N2,M2,N1,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z56(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K9*K0
       I3=K6*K7
       call DMATMAT(I1,I2,I3,X56,F2,Z56)
       deallocate(F2)
C
       call
     & sum134526(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z56, 1.000)
       deallocate(Z56)
       deallocate(X56)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N0,M1,N2,M2,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S117(N2+1:M2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S117)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X57(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:M1))
       X57=0.0d0
       call
     & sum3124(N0,M1,N2,M2,N2,M2,N0,M1,X57,S117,-1.000)
       deallocate(S117)
C
       call sumx2431(N2,N3,N0,N1,N2,N3,N0,N1,
     & N0,M1,N2,M2,N2,M2,N0,M1,X57,VBHPHP, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2))
       call reorder612345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z57(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K9*K0
       I3=K0*K5
       call DMATMAT(I1,I2,I3,X57,F2,Z57)
       deallocate(F2)
C
       call
     & sum134526(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z57, 1.000)
       deallocate(Z57)
       deallocate(X57)
C
       allocate(D1(N0+1:N2,M1+1:N1,N2+1:M2,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,M1,N1,N2,M2,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S118(N2+1:M2,M1+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K7
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S118)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X58(M1+1:N1,N2+1:M2,N2+1:M2,N0+1:M1))
       X58=0.0d0
       call
     & sum3124(M1,N1,N2,M2,N2,M2,N0,M1,X58,S118,-1.000)
       deallocate(S118)
C
       call sumx2431(N2,N3,N0,N1,N2,N3,N0,N1,
     & M1,N1,N2,M2,N2,M2,N0,M1,X58,VBHPHP, 1.000)
C
       allocate(F2(M1+1:N1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2))
       call reorder612345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z58(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K9*K0
       I3=K0*K7
       call DMATMAT(I1,I2,I3,X58,F2,Z58)
       deallocate(F2)
C
       call
     & sum134526(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z58, 1.000)
       deallocate(Z58)
       deallocate(X58)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N0+1:M1))
       call reorder3142(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q21(N0+1:M1,N0+1:M1))
       I1=K5*K5
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q21)
       deallocate(D1)
       deallocate(B2)
C
       X29=X29+Q21
       deallocate(Q21)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N0+1:M1))
       call reorder3142(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q22(M1+1:N1,N0+1:M1))
       I1=K5*K7
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q22)
       deallocate(D1)
       deallocate(B2)
C
       X30=X30+Q22
       deallocate(Q22)
C
       allocate(D1(N2+1:N3,M1+1:N2,M2+1:N3,N1+1:M2))
       call reorder1324(N2,N3,N1,N3,N0,N2,N1,N3,
     & N2,N3,M1,N2,M2,N3,N1,M2,VBPHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S119(M1+1:N2,M1+1:N2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K8
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S119)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X59(M1+1:N2,M2+1:N3,N1+1:M2,M1+1:N2))
       X59=0.0d0
       call
     & sum4123(M1,N2,M2,N3,N1,M2,M1,N2,X59,S119, 1.000)
       deallocate(S119)
C
       call sumx4213(N0,N2,N1,N3,N0,N2,N1,N3,
     & M1,N2,M2,N3,N1,M2,M1,N2,X59,VBPHPH, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder531246(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N2,M2,N2,M2,N0,M1,N0,M1,t3C1,F2)
       allocate(Z59(N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N2))
       I1=K8*K9
       I2=K5*K5*K0*K0
       I3=K6*K8
       call DMATMAT(I1,I2,I3,X59,F2,Z59)
       deallocate(F2)
C
       call
     & sum124635(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z59,-1.000)
       deallocate(Z59)
       deallocate(X59)
C
       allocate(D1(N2+1:N3,M1+1:N2,N1+1:M2,N1+1:M2))
       call reorder1324(N2,N3,N1,N3,N0,N2,N1,N3,
     & N2,N3,M1,N2,N1,M2,N1,M2,VBPHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S120(M1+1:N2,M1+1:N2,N1+1:M2,N1+1:M2))
       I1=K9*K9*K8
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S120)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X60(M1+1:N2,N1+1:M2,N1+1:M2,M1+1:N2))
       X60=0.0d0
       call
     & sum4123(M1,N2,N1,M2,N1,M2,M1,N2,X60,S120, 1.000)
       deallocate(S120)
C
       call sumx4213(N0,N2,N1,N3,N0,N2,N1,N3,
     & M1,N2,N1,M2,N1,M2,M1,N2,X60,VBPHPH, 1.000)
C
       allocate(F2(M1+1:N2,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder531246(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N1,M2,N2,M2,N2,M2,N0,M1,N0,M1,t3C1,F2)
       allocate(Z60(N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N2))
       I1=K8*K9
       I2=K5*K5*K0*K0
       I3=K9*K8
       call DMATMAT(I1,I2,I3,X60,F2,Z60)
       deallocate(F2)
C
       call
     & sum124635(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z60,-1.000)
       deallocate(Z60)
       deallocate(X60)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,N1+1:M2))
       call reorder1324(N2,N3,N1,N3,N0,N2,N1,N3,
     & N2,N3,N0,M1,M2,N3,N1,M2,VBPHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S121(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:M2))
       I1=K9*K6*K5
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S121)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X61(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       X61=0.0d0
       call
     & sum4123(N0,M1,M2,N3,N1,M2,N0,M1,X61,S121, 1.000)
       deallocate(S121)
C
       call sumx4213(N0,N2,N1,N3,N0,N2,N1,N3,
     & N0,M1,M2,N3,N1,M2,N0,M1,X61,VBPHPH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder431256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N2,M2,N2,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z61(N2+1:M2,N2+1:M2,M1+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K5*K8*K0*K0
       I3=K6*K5
       call DMATMAT(I1,I2,I3,X61,F2,Z61)
       deallocate(F2)
C
       call
     & sum125634(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z61,-1.000)
       deallocate(Z61)
       deallocate(X61)
C
       allocate(D1(N2+1:N3,M1+1:N2,M2+1:N3,N1+1:M2))
       call reorder1324(N2,N3,N1,N3,N0,N2,N1,N3,
     & N2,N3,M1,N2,M2,N3,N1,M2,VBPHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S122(N0+1:M1,M1+1:N2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K8
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S122)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X62(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:M1))
       X62=0.0d0
       call
     & sum4123(M1,N2,M2,N3,N1,M2,N0,M1,X62,S122, 1.000)
       deallocate(S122)
C
       call sumx4213(N0,N2,N1,N3,N0,N2,N1,N3,
     & M1,N2,M2,N3,N1,M2,N0,M1,X62,VBPHPH, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder431256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N2,M2,N2,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z62(N2+1:M2,N2+1:M2,M1+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K5*K8*K0*K0
       I3=K6*K8
       call DMATMAT(I1,I2,I3,X62,F2,Z62)
       deallocate(F2)
C
       call
     & sum125634(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z62,-1.000)
       deallocate(Z62)
       deallocate(X62)
C
       allocate(D1(N2+1:N3,N0+1:M1,N1+1:M2,N1+1:M2))
       call reorder1324(N2,N3,N1,N3,N0,N2,N1,N3,
     & N2,N3,N0,M1,N1,M2,N1,M2,VBPHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S123(N0+1:M1,N0+1:M1,N1+1:M2,N1+1:M2))
       I1=K9*K9*K5
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S123)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X63(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:M1))
       X63=0.0d0
       call
     & sum4123(N0,M1,N1,M2,N1,M2,N0,M1,X63,S123, 1.000)
       deallocate(S123)
C
       call sumx4213(N0,N2,N1,N3,N0,N2,N1,N3,
     & N0,M1,N1,M2,N1,M2,N0,M1,X63,VBPHPH, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder431256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N1,M2,N2,M2,N2,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z63(N2+1:M2,N2+1:M2,M1+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K5*K8*K0*K0
       I3=K9*K5
       call DMATMAT(I1,I2,I3,X63,F2,Z63)
       deallocate(F2)
C
       call
     & sum125634(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z63,-1.000)
       deallocate(Z63)
       deallocate(X63)
C
       allocate(D1(N2+1:N3,M1+1:N2,N1+1:M2,N1+1:M2))
       call reorder1324(N2,N3,N1,N3,N0,N2,N1,N3,
     & N2,N3,M1,N2,N1,M2,N1,M2,VBPHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S124(N0+1:M1,M1+1:N2,N1+1:M2,N1+1:M2))
       I1=K9*K9*K8
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S124)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X64(M1+1:N2,N1+1:M2,N1+1:M2,N0+1:M1))
       X64=0.0d0
       call
     & sum4123(M1,N2,N1,M2,N1,M2,N0,M1,X64,S124, 1.000)
       deallocate(S124)
C
       call sumx4213(N0,N2,N1,N3,N0,N2,N1,N3,
     & M1,N2,N1,M2,N1,M2,N0,M1,X64,VBPHPH, 1.000)
C
       allocate(F2(M1+1:N2,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder431256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N1,M2,N2,M2,N2,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z64(N2+1:M2,N2+1:M2,M1+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K5*K8*K0*K0
       I3=K9*K8
       call DMATMAT(I1,I2,I3,X64,F2,Z64)
       deallocate(F2)
C
       call
     & sum125634(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z64,-1.000)
       deallocate(Z64)
       deallocate(X64)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,M2,N3,M2,N3,N1,M2,VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S125(N2+1:M2,M2+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6*K6
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S125)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,M2,N3,N2,M2,N1,M2,X65,S125,-1.000)
       deallocate(S125)
C
       allocate(D1(N0+1:N2,N2+1:M2,M2+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,M2,M2,N3,N1,M2,VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S126(N2+1:M2,N2+1:M2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K0
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S126)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,M2,M2,N3,N2,M2,N1,M2,X66,S126,-1.000)
       deallocate(S126)
C
       allocate(D1(N0+1:N2,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,M2,N3,N1,M2,N1,M2,VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S127(N2+1:M2,M2+1:N3,N1+1:M2,N1+1:M2))
       I1=K9*K9*K6
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S127)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,N1,M2,N2,M2,N1,M2,X67,S127,-1.000)
       deallocate(S127)
C
       allocate(D1(N0+1:N2,N2+1:M2,N1+1:M2,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,M2,N1,M2,N1,M2,VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S128(N2+1:M2,N2+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9*K0
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S128)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,M2,N1,M2,N2,M2,N1,M2,X68,S128,-1.000)
       deallocate(S128)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,M2,N3,M2,N3,N1,M2,VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S129(N2+1:M2,M2+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6*K6
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S129)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,M2,N3,N2,M2,N1,M2,X69,S129,-1.000)
       deallocate(S129)
C
       allocate(D1(N0+1:N2,N2+1:M2,M2+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,M2,M2,N3,N1,M2,VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S130(N2+1:M2,N2+1:M2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K0
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S130)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,M2,M2,N3,N2,M2,N1,M2,X70,S130,-1.000)
       deallocate(S130)
C
       allocate(D1(N0+1:N2,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,M2,N3,N1,M2,N1,M2,VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S131(N2+1:M2,M2+1:N3,N1+1:M2,N1+1:M2))
       I1=K9*K9*K6
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S131)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,N1,M2,N2,M2,N1,M2,X71,S131,-1.000)
       deallocate(S131)
C
       allocate(D1(N0+1:N2,N2+1:M2,N1+1:M2,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,M2,N1,M2,N1,M2,VBPHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S132(N2+1:M2,N2+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9*K0
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S132)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,M2,N1,M2,N2,M2,N1,M2,X72,S132,-1.000)
       deallocate(S132)
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
       call DMATVEC(I1,I3,D1,B2,Q23)
       deallocate(D1)
       deallocate(B2)
C
       X31=X31+Q23
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
       call DMATVEC(I1,I3,D1,B2,Q24)
       deallocate(D1)
       deallocate(B2)
C
       X32=X32+Q24
       deallocate(Q24)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder1342(N2,N3,N0,N2,N0,N2,N0,N2,
     & N2,N3,N0,M1,M1,N2,M1,N2,VCHHHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S133(N0+1:M1,N0+1:M1,M1+1:N2,M1+1:N2))
       I1=K8*K8*K5
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S133)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X73(N0+1:M1,M1+1:N2,N0+1:M1,M1+1:N2))
       X73=0.0d0
       call
     & sum3124(N0,M1,M1,N2,N0,M1,M1,N2,X73,S133, 1.000)
       deallocate(S133)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder1342(N2,N3,N0,N2,N0,N2,N0,N2,
     & N2,N3,M1,N2,M1,N2,M1,N2,VCHHHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S134(N0+1:M1,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K8
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S134)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X74(M1+1:N2,M1+1:N2,N0+1:M1,M1+1:N2))
       X74=0.0d0
       call
     & sum3124(M1,N2,M1,N2,N0,M1,M1,N2,X74,S134, 1.000)
       deallocate(S134)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,M1,N2,M2,N3,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S135(N2+1:M2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S135)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X75(M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       X75=0.0d0
       call
     & sum3124(M1,N2,M2,N3,N2,M2,M1,N2,X75,S135,-1.000)
       deallocate(S135)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,M1,N2,N2,M2,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S136(N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K8
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S136)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X76(M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       X76=0.0d0
       call
     & sum3124(M1,N2,N2,M2,N2,M2,M1,N2,X76,S136,-1.000)
       deallocate(S136)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,M1,N2,M2,N3,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S137(N2+1:M2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S137)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X77(M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       X77=0.0d0
       call
     & sum3124(M1,N2,M2,N3,N2,M2,M1,N2,X77,S137,-1.000)
       deallocate(S137)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,M1,N2,N2,M2,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S138(N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K8
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S138)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X78(M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       X78=0.0d0
       call
     & sum3124(M1,N2,N2,M2,N2,M2,M1,N2,X78,S138,-1.000)
       deallocate(S138)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder3142(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q25(M1+1:N2,M1+1:N2))
       I1=K8*K8
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q25)
       deallocate(D1)
       deallocate(B2)
C
       X33=X33+Q25
       deallocate(Q25)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder1342(N2,N3,N0,N2,N0,N2,N0,N2,
     & N2,N3,N0,M1,M1,N2,N0,M1,VCHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S139(M1+1:N2,N0+1:M1,M1+1:N2,N0+1:M1))
       I1=K5*K8*K5
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S139)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,M1,N2,N0,M1,M1,N2,X73,S139,-1.000)
       deallocate(S139)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,N0+1:M1))
       call reorder1342(N2,N3,N0,N2,N0,N2,N0,N2,
     & N2,N3,M1,N2,M1,N2,N0,M1,VCHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S140(M1+1:N2,M1+1:N2,M1+1:N2,N0+1:M1))
       I1=K5*K8*K8
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S140)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N2,M1,N2,N0,M1,M1,N2,X74,S140,-1.000)
       deallocate(S140)
C
       allocate(D1(N2+1:N3,M1+1:N2,M2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,M1,N2,M2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S141(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6*K8
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S141)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N2,M2,N3,N2,M2,M1,N2,X75,S141,-1.000)
       deallocate(S141)
C
       allocate(D1(N2+1:N3,M1+1:N2,N2+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,M1,N2,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S142(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K8
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S142)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N2,N2,M2,N2,M2,M1,N2,X76,S142,-1.000)
       deallocate(S142)
C
       allocate(D1(N2+1:N3,M1+1:N2,M2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,M1,N2,M2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S143(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6*K8
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S143)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N2,M2,N3,N2,M2,M1,N2,X77,S143,-1.000)
       deallocate(S143)
C
       allocate(D1(N2+1:N3,M1+1:N2,N2+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,M1,N2,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S144(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K8
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S144)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N2,N2,M2,N2,M2,M1,N2,X78,S144,-1.000)
       deallocate(S144)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,M1,M2,N3,N0,M1,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S145(N2+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S145)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X79(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       X79=0.0d0
       call
     & sum3124(N0,M1,M2,N3,N2,M2,N0,M1,X79,S145,-1.000)
       deallocate(S145)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,M1,N2,M2,N3,N0,M1,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S146(N2+1:M2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S146)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X80(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:M1))
       X80=0.0d0
       call
     & sum3124(M1,N2,M2,N3,N2,M2,N0,M1,X80,S146,-1.000)
       deallocate(S146)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,M1,N2,M2,N0,M1,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S147(N2+1:M2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S147)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X81(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:M1))
       X81=0.0d0
       call
     & sum3124(N0,M1,N2,M2,N2,M2,N0,M1,X81,S147,-1.000)
       deallocate(S147)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,M1,N2,N2,M2,N0,M1,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S148(N2+1:M2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S148)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X82(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:M1))
       X82=0.0d0
       call
     & sum3124(M1,N2,N2,M2,N2,M2,N0,M1,X82,S148,-1.000)
       deallocate(S148)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,M1,M2,N3,N0,M1,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S149(N2+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S149)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X83(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       X83=0.0d0
       call
     & sum3124(N0,M1,M2,N3,N2,M2,N0,M1,X83,S149,-1.000)
       deallocate(S149)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,M1,N2,M2,N3,N0,M1,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S150(N2+1:M2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S150)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X84(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:M1))
       X84=0.0d0
       call
     & sum3124(M1,N2,M2,N3,N2,M2,N0,M1,X84,S150,-1.000)
       deallocate(S150)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,M1,N2,M2,N0,M1,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S151(N2+1:M2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S151)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X85(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:M1))
       X85=0.0d0
       call
     & sum3124(N0,M1,N2,M2,N2,M2,N0,M1,X85,S151,-1.000)
       deallocate(S151)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,M1,N2,N2,M2,N0,M1,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S152(N2+1:M2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S152)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X86(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:M1))
       X86=0.0d0
       call
     & sum3124(M1,N2,N2,M2,N2,M2,N0,M1,X86,S152,-1.000)
       deallocate(S152)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N0+1:M1))
       call reorder3142(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,N0,M1,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q26(N0+1:M1,N0+1:M1))
       I1=K5*K5
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q26)
       deallocate(D1)
       deallocate(B2)
C
       X34=X34+Q26
       deallocate(Q26)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N0+1:M1))
       call reorder3142(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,N0,M1,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q27(M1+1:N2,N0+1:M1))
       I1=K5*K8
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q27)
       deallocate(D1)
       deallocate(B2)
C
       X35=X35+Q27
       deallocate(Q27)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,M1,M2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S153(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K5
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S153)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,M2,N3,N2,M2,N0,M1,X79,S153,-1.000)
       deallocate(S153)
C
       allocate(D1(N2+1:N3,M1+1:N2,M2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,M1,N2,M2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S154(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6*K8
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S154)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N2,M2,N3,N2,M2,N0,M1,X80,S154,-1.000)
       deallocate(S154)
C
       allocate(D1(N2+1:N3,N0+1:M1,N2+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,M1,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S155(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:M2))
       I1=K0*K0*K5
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S155)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,N2,M2,N2,M2,N0,M1,X81,S155,-1.000)
       deallocate(S155)
C
       allocate(D1(N2+1:N3,M1+1:N2,N2+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,M1,N2,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S156(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K8
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S156)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N2,N2,M2,N2,M2,N0,M1,X82,S156,-1.000)
       deallocate(S156)
C
       allocate(D1(N2+1:N3,N0+1:M1,M2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,M1,M2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S157(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2))
       I1=K0*K6*K5
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S157)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,M2,N3,N2,M2,N0,M1,X83,S157,-1.000)
       deallocate(S157)
C
       allocate(D1(N2+1:N3,M1+1:N2,M2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,M1,N2,M2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S158(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6*K8
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S158)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N2,M2,N3,N2,M2,N0,M1,X84,S158,-1.000)
       deallocate(S158)
C
       allocate(D1(N2+1:N3,N0+1:M1,N2+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,M1,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S159(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:M2))
       I1=K0*K0*K5
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S159)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(N0,M1,N2,M2,N2,M2,N0,M1,X85,S159,-1.000)
       deallocate(S159)
C
       allocate(D1(N2+1:N3,M1+1:N2,N2+1:M2,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,M1,N2,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S160(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K8
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S160)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum4123(M1,N2,N2,M2,N2,M2,N0,M1,X86,S160,-1.000)
       deallocate(S160)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,M2,N3,M2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S161(N2+1:M2,M2+1:N3,M2+1:N3,N2+1:M2))
       I1=K0*K6*K6
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S161)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X87(M2+1:N3,M2+1:N3,N2+1:M2,N2+1:M2))
       X87=0.0d0
       call
     & sum4123(M2,N3,M2,N3,N2,M2,N2,M2,X87,S161,-1.000)
       deallocate(S161)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,M2,N3,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S162(N2+1:M2,M2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K6
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S162)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X88(M2+1:N3,N2+1:M2,N2+1:M2,N2+1:M2))
       X88=0.0d0
       call
     & sum4123(M2,N3,N2,M2,N2,M2,N2,M2,X88,S162,-1.000)
       deallocate(S162)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,M2,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S163(N2+1:M2,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K0
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S163)
       deallocate(D1)
       deallocate(B2)
C
       allocate(X89(N2+1:M2,N2+1:M2,N2+1:M2,N2+1:M2))
       X89=0.0d0
       call
     & sum4123(N2,M2,N2,M2,N2,M2,N2,M2,X89,S163,-1.000)
       deallocate(S163)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,M2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q28(M2+1:N3,N2+1:M2))
       I1=K0*K6
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q28)
       deallocate(D1)
       deallocate(B2)
C
       X36=X36-Q28
       deallocate(Q28)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q29(N2+1:M2,N2+1:M2))
       I1=K0*K0
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q29)
       deallocate(D1)
       deallocate(B2)
C
       X37=X37-Q29
       deallocate(Q29)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,M2,N3,M2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S164(N2+1:M2,M2+1:N3,M2+1:N3,N2+1:M2))
       I1=K0*K6*K6
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S164)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,M2,N3,N2,M2,N2,M2,X87,S164, 1.000)
       deallocate(S164)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,M2,N3,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S165(N2+1:M2,M2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K6
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S165)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,N2,M2,N2,M2,N2,M2,X88,S165, 1.000)
       deallocate(S165)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,M2,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S166(N2+1:M2,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K0
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S166)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,M2,N2,M2,N2,M2,N2,M2,X89,S166, 1.000)
       deallocate(S166)
C
       allocate(D1(N0+1:N2,N2+1:N3,M2+1:N3,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,M2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q30(M2+1:N3,N2+1:M2))
       I1=K0*K6
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q30)
       deallocate(D1)
       deallocate(B2)
C
       X38=X38-Q30
       deallocate(Q30)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,M2,N2,M2,VCHPPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q31(N2+1:M2,N2+1:M2))
       I1=K0*K0
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q31)
       deallocate(D1)
       deallocate(B2)
C
       X39=X39-Q31
       deallocate(Q31)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder4231(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,M1,N2,VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S167(N1+1:M2,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K9
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S167)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N2,N1,M2,M1,N2,N0,M1,X13,S167, 1.000)
       deallocate(S167)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:M1))
       call reorder4231(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N0,M1,VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S168(N1+1:M2,N0+1:M1,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K5*K9
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S168)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N2,N1,M2,N0,M1,N0,M1,X14,S168, 1.000)
       deallocate(S168)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,N3,N2,M2,VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S169(N1+1:M2,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K9
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S169)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,N3,N2,M2,N1,M2,N0,M1,X15,S169, 1.000)
C
       call
     & sum3412(N2,N3,N2,M2,N1,M2,N0,M1,X16,S169, 1.000)
       deallocate(S169)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S170(N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K9
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S170)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X111(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       X111=0.0d0
       call
     & sum3412(N0,M1,M2,N3,N1,M2,N0,M1,X111,S170, 1.000)
       deallocate(S170)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S171(N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K9
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S171)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X112(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:M1))
       X112=0.0d0
       call
     & sum3412(M1,N1,M2,N3,N1,M2,N0,M1,X112,S171, 1.000)
       deallocate(S171)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S172(N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K9
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S172)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X113(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:M1))
       X113=0.0d0
       call
     & sum3412(N0,M1,N1,M2,N1,M2,N0,M1,X113,S172, 1.000)
       deallocate(S172)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder4231(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S173(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K5*K9
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S173)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X114(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:M1))
       X114=0.0d0
       call
     & sum3412(M1,N1,N1,M2,N1,M2,N0,M1,X114,S173, 1.000)
       deallocate(S173)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,M1,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,M1,t2A,D2)
       allocate(Q32(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K3*K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,Q32)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N0,M1,N0,M1,X29,Q32, 0.500)
       deallocate(Q32)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,M1+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,M1,N1,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,N3,N0,M1,t2A,D2)
       allocate(Q33(N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5
       I3=K3*K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,Q33)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M1,N1,N0,M1,X30,Q33, 0.500)
       deallocate(Q33)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,M2,t2A,D2)
       allocate(Q34(N1+1:M2,M2+1:N3))
       I1=K6
       I2=K9
       I3=K3*K1*K1
       call DMATMAT(I1,I2,I3,D1,D2,Q34)
       deallocate(D1)
       deallocate(D2)
C
       allocate(B1(M2+1:N3,N1+1:M2))
       call reorder21(N1,M2,M2,N3,
     & M2,N3,N1,M2,Q34,B1)
       allocate(F2(M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder312456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,N0,M1,t3C1,F2)
       allocate(Z317(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9
       I2=K5*K8*K5*K0*K0
       I3=K6
       call DMATMAT(I1,I2,I3,B1,F2,Z317)
       deallocate(B1)
       deallocate(F2)
C
       call
     & sum124563(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z317, 0.500)
       deallocate(Z317)
       deallocate(Q34)
C
       allocate(D1(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N0+1:N1,N1+1:N3,N1+1:M2))
       call reorder3412(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N0,N1,N1,N3,N1,M2,t2A,D2)
       allocate(Q35(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K3*K1*K1
       call DMATMAT(I1,I2,I3,D1,D2,Q35)
       deallocate(D1)
       deallocate(D2)
C
       allocate(B1(N1+1:M2,N1+1:M2))
       call reorder21(N1,M2,N1,M2,
     & N1,M2,N1,M2,Q35,B1)
       allocate(F2(N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder312456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N1,M2,N2,M2,N2,M2,N0,M1,M1,N2,N0,M1,t3C1,F2)
       allocate(Z318(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9
       I2=K5*K8*K5*K0*K0
       I3=K9
       call DMATMAT(I1,I2,I3,B1,F2,Z318)
       deallocate(B1)
       deallocate(F2)
C
       call
     & sum124563(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z318, 0.500)
       deallocate(Z318)
       deallocate(Q35)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S174(N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K9
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S174)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X115(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       X115=0.0d0
       call
     & sum3412(N0,M1,M2,N3,N1,M2,N0,M1,X115,S174, 1.000)
       deallocate(S174)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S175(N1+1:M2,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K9
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S175)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X116(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:M1))
       X116=0.0d0
       call
     & sum3412(M1,N2,M2,N3,N1,M2,N0,M1,X116,S175, 1.000)
       deallocate(S175)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S176(N1+1:M2,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K9
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S176)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X117(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:M1))
       X117=0.0d0
       call
     & sum3412(N0,M1,N2,M2,N1,M2,N0,M1,X117,S176, 1.000)
       deallocate(S176)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S177(N1+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K9
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S177)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X118(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:M1))
       X118=0.0d0
       call
     & sum3412(M1,N2,N2,M2,N1,M2,N0,M1,X118,S177, 1.000)
       deallocate(S177)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,FAHP,B1)
       allocate(D2(N1+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,M2,M1,N2,N0,M1,t2B,D2)
       allocate(S178(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K3
       call DMATMAT(I1,I2,I3,B1,D2,S178)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X1,S178, 1.000)
       deallocate(S178)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,FAHP,B1)
       allocate(D2(N1+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,M2,M1,N2,N0,M1,t2B,D2)
       allocate(S179(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K3
       call DMATMAT(I1,I2,I3,B1,D2,S179)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X2,S179, 1.000)
       deallocate(S179)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,FAHP,B1)
       allocate(D2(N1+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,M2,N0,M1,N0,M1,t2B,D2)
       allocate(S180(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K0
       I3=K3
       call DMATMAT(I1,I2,I3,B1,D2,S180)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,N2,M2,N0,M1,N0,M1,X3,S180, 1.000)
       deallocate(S180)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,N1,FAHP,B1)
       allocate(D2(N1+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,M2,N0,M1,N0,M1,t2B,D2)
       allocate(S181(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K0
       I3=K3
       call DMATMAT(I1,I2,I3,B1,D2,S181)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,N2,M2,N0,M1,N0,M1,X4,S181, 1.000)
       deallocate(S181)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder3142(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N0,M1,VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S182(N2+1:M2,N0+1:M1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K5*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S182)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N2,M2,N0,M1,N0,M1,X4,S182, 1.000)
       deallocate(S182)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder3142(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N0,M1,VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S183(N2+1:M2,M1+1:N2,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K8*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S183)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N2,M2,M1,N2,N0,M1,X1,S183, 1.000)
       deallocate(S183)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder3142(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N0,M1,VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S184(N2+1:M2,N0+1:M1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K5*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S184)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N2,M2,N0,M1,N0,M1,X3,S184, 1.000)
       deallocate(S184)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder3142(N1,N3,N0,N1,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N0,M1,VAHHHP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S185(N2+1:M2,M1+1:N2,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K8*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S185)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N2,M2,M1,N2,N0,M1,X2,S185, 1.000)
       deallocate(S185)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,M2,VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S186(N2+1:M2,N0+1:M1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K5*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S186)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N1,N3,N2,M2,N1,M2,N0,M1,X8,S186,-1.000)
       deallocate(S186)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,M2,VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S187(N2+1:M2,N0+1:M1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K5*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S187)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N1,N3,N2,M2,N1,M2,N0,M1,X7,S187,-1.000)
       deallocate(S187)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,M2,VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S188(N2+1:M2,M1+1:N2,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K8*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S188)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N1,N3,N2,M2,N1,M2,M1,N2,X6,S188,-1.000)
       deallocate(S188)
C
       allocate(D1(N0+1:N1,N1+1:N3,N1+1:N3,N1+1:M2))
       call reorder4123(N1,N3,N1,N3,N1,N3,N0,N1,
     & N0,N1,N1,N3,N1,N3,N1,M2,VAHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S189(N2+1:M2,M1+1:N2,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K8*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S189)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N1,N3,N2,M2,N1,M2,M1,N2,X5,S189,-1.000)
       deallocate(S189)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,M1,N2,VBHHPH,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,N1,M2,t2B,D2)
       allocate(S190(N2+1:M2,N1+1:M2,N1+1:N3,M1+1:N2))
       I1=K8*K3
       I2=K9*K0
       I3=K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,S190)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N1,N3,N2,M2,N1,M2,M1,N2,X6,S190, 1.000)
       deallocate(S190)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,M1+1:N2))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,M1,N2,VBHHPH,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,N1,M2,t2B,D2)
       allocate(S191(N2+1:M2,N1+1:M2,N1+1:N3,M1+1:N2))
       I1=K8*K3
       I2=K9*K0
       I3=K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,S191)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N1,N3,N2,M2,N1,M2,M1,N2,X5,S191, 1.000)
       deallocate(S191)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder4231(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,M1,N2,VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S192(N2+1:M2,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S192)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,N2,M2,N0,M1,M1,N2,X10,S192, 1.000)
       deallocate(S192)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,M1+1:N2))
       call reorder3241(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N0,N1,M1,N2,VBHHPH,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S193(N2+1:M2,N0+1:M1,N0+1:N1,M1+1:N2))
       I1=K8*K1
       I2=K5*K0
       I3=K3*K2
       call DMATMAT(I1,I2,I3,D1,D2,S193)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N1,N2,M2,M1,N2,N0,M1,X1,S193,-1.000)
       deallocate(S193)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder4231(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,M1,N2,VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S194(N2+1:M2,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S194)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,N2,M2,N0,M1,M1,N2,X9,S194, 1.000)
       deallocate(S194)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,M1+1:N2))
       call reorder3241(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N0,N1,M1,N2,VBHHPH,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S195(N2+1:M2,N0+1:M1,N0+1:N1,M1+1:N2))
       I1=K8*K1
       I2=K5*K0
       I3=K3*K2
       call DMATMAT(I1,I2,I3,D1,D2,S195)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N1,N2,M2,M1,N2,N0,M1,X2,S195,-1.000)
       deallocate(S195)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:M1))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N0,M1,VBHHPH,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,N1,M2,t2B,D2)
       allocate(S196(N2+1:M2,N1+1:M2,N1+1:N3,N0+1:M1))
       I1=K5*K3
       I2=K9*K0
       I3=K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,S196)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N1,N3,N2,M2,N1,M2,N0,M1,X8,S196, 1.000)
       deallocate(S196)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:M1))
       call reorder3421(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N0,M1,VBHHPH,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,N1,M2,t2B,D2)
       allocate(S197(N2+1:M2,N1+1:M2,N1+1:N3,N0+1:M1))
       I1=K5*K3
       I2=K9*K0
       I3=K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,S197)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N1,N3,N2,M2,N1,M2,N0,M1,X7,S197, 1.000)
       deallocate(S197)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:M2))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N2,N3,N1,N3,N2,M2,VBHPPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S198(N1+1:M2,N0+1:M1,N1+1:N3,N2+1:M2))
       I1=K0*K3
       I2=K5*K9
       I3=K4*K1
       call DMATMAT(I1,I2,I3,D1,D2,S198)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N1,N3,N2,M2,N1,M2,N0,M1,X7,S198,-1.000)
       deallocate(S198)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,N3,N2,M2,VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S199(N2+1:M2,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S199)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,N3,N2,M2,N2,M2,N0,M1,X12,S199, 1.000)
       deallocate(S199)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N1,N2+1:M2))
       call reorder1243(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N1,N3,N0,N1,N2,M2,VBHPPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,M1+1:N2,N0+1:M1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,M1,N2,N0,M1,t2B,D2)
       allocate(S200(M1+1:N2,N0+1:M1,N0+1:N1,N2+1:M2))
       I1=K0*K1
       I2=K5*K8
       I3=K3*K4
       call DMATMAT(I1,I2,I3,D1,D2,S200)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,N1,N2,M2,M1,N2,N0,M1,X1,S200, 1.000)
       deallocate(S200)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:M2))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N2,N3,N1,N3,N2,M2,VBHPPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S201(N1+1:M2,N0+1:M1,N1+1:N3,N2+1:M2))
       I1=K0*K3
       I2=K5*K9
       I3=K4*K1
       call DMATMAT(I1,I2,I3,D1,D2,S201)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N1,N3,N2,M2,N1,M2,N0,M1,X8,S201,-1.000)
       deallocate(S201)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,N3,N2,M2,VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S202(N2+1:M2,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S202)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,N2,M2,N2,M2,N0,M1,X12,S202,-1.000)
       deallocate(S202)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N1,N2+1:M2))
       call reorder1243(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N1,N3,N0,N1,N2,M2,VBHPPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,M1+1:N2,N0+1:M1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,M1,N2,N0,M1,t2B,D2)
       allocate(S203(M1+1:N2,N0+1:M1,N0+1:N1,N2+1:M2))
       I1=K0*K1
       I2=K5*K8
       I3=K3*K4
       call DMATMAT(I1,I2,I3,D1,D2,S203)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,N1,N2,M2,M1,N2,N0,M1,X2,S203, 1.000)
       deallocate(S203)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:M1))
       call reorder4231(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N0,M1,VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S204(N2+1:M2,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S204)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N2,N2,M2,N0,M1,M1,N2,X10,S204,-1.000)
       deallocate(S204)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder3241(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N0,N1,N0,M1,VBHHPH,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S205(N2+1:M2,N0+1:M1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K5*K0
       I3=K3*K2
       call DMATMAT(I1,I2,I3,D1,D2,S205)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N1,N2,M2,N0,M1,N0,M1,X3,S205,-1.000)
       deallocate(S205)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:M1))
       call reorder4231(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N0,M1,VBHHPH,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S206(N2+1:M2,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S206)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N2,N2,M2,N0,M1,M1,N2,X9,S206,-1.000)
       deallocate(S206)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder3241(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N0,N1,N0,M1,VBHHPH,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S207(N2+1:M2,N0+1:M1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K5*K0
       I3=K3*K2
       call DMATMAT(I1,I2,I3,D1,D2,S207)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N1,N2,M2,N0,M1,N0,M1,X4,S207,-1.000)
       deallocate(S207)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:M2))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N2,N3,N1,N3,N2,M2,VBHPPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,M2,M1,N2,t2B,D2)
       allocate(S208(N1+1:M2,M1+1:N2,N1+1:N3,N2+1:M2))
       I1=K0*K3
       I2=K8*K9
       I3=K4*K1
       call DMATMAT(I1,I2,I3,D1,D2,S208)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N1,N3,N2,M2,N1,M2,M1,N2,X5,S208,-1.000)
       deallocate(S208)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,N3,N2,M2,VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S209(N2+1:M2,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S209)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,N3,N2,M2,N2,M2,M1,N2,X11,S209, 1.000)
       deallocate(S209)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N1,N2+1:M2))
       call reorder1243(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N1,N3,N0,N1,N2,M2,VBHPPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,M1,N0,M1,t2B,D2)
       allocate(S210(N0+1:M1,N0+1:M1,N0+1:N1,N2+1:M2))
       I1=K0*K1
       I2=K5*K5
       I3=K3*K4
       call DMATMAT(I1,I2,I3,D1,D2,S210)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,N1,N2,M2,N0,M1,N0,M1,X3,S210, 1.000)
       deallocate(S210)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N2+1:M2))
       call reorder4123(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N2,N3,N1,N3,N2,M2,VBHPPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,M2,M1,N2,t2B,D2)
       allocate(S211(N1+1:M2,M1+1:N2,N1+1:N3,N2+1:M2))
       I1=K0*K3
       I2=K8*K9
       I3=K4*K1
       call DMATMAT(I1,I2,I3,D1,D2,S211)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N1,N3,N2,M2,N1,M2,M1,N2,X6,S211,-1.000)
       deallocate(S211)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:N3,N2+1:M2))
       call reorder4213(N2,N3,N1,N3,N2,N3,N0,N1,
     & N0,N1,N1,N3,N2,N3,N2,M2,VBHPPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S212(N2+1:M2,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S212)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,N2,M2,N2,M2,M1,N2,X11,S212,-1.000)
       deallocate(S212)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N1,N2+1:M2))
       call reorder1243(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N1,N3,N0,N1,N2,M2,VBHPPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,M1,N0,M1,t2B,D2)
       allocate(S213(N0+1:M1,N0+1:M1,N0+1:N1,N2+1:M2))
       I1=K0*K1
       I2=K5*K5
       I3=K3*K4
       call DMATMAT(I1,I2,I3,D1,D2,S213)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,N1,N2,M2,N0,M1,N0,M1,X4,S213, 1.000)
       deallocate(S213)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,FBHP,B1)
       allocate(D2(N2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,M2,M1,N2,N0,M1,t2B,D2)
       allocate(S214(N1+1:M2,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K9
       I3=K4
       call DMATMAT(I1,I2,I3,B1,D2,S214)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N2,N1,M2,M1,N2,N0,M1,X13,S214, 1.000)
       deallocate(S214)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,FBHP,B1)
       allocate(D2(N2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,M2,N0,M1,N0,M1,t2B,D2)
       allocate(S215(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K9
       I3=K4
       call DMATMAT(I1,I2,I3,B1,D2,S215)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N2,N1,M2,N0,M1,N0,M1,X14,S215, 1.000)
       deallocate(S215)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,FBHP,B1)
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,N0,M1,M1,N2,t2C,D2)
       allocate(S216(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K4
       call DMATMAT(I1,I2,I3,B1,D2,S216)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X10,S216,-1.000)
       deallocate(S216)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,N2,FBHP,B1)
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,N0,M1,M1,N2,t2C,D2)
       allocate(S217(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K4
       call DMATMAT(I1,I2,I3,B1,D2,S217)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X9,S217,-1.000)
       deallocate(S217)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder3142(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N0,M1,VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S218(N2+1:M2,N0+1:M1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K5*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S218)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N2,M2,N0,M1,N0,M1,X4,S218, 1.000)
       deallocate(S218)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder3142(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N0,M1,VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S219(N2+1:M2,N0+1:M1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K5*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S219)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N2,M2,N0,M1,N0,M1,X3,S219, 1.000)
       deallocate(S219)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:M1))
       call reorder4132(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N2,N3,N0,N2,N0,M1,VBHHHP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,M2,M1,N2,t2B,D2)
       allocate(S220(N1+1:M2,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K9
       I3=K4*K1
       call DMATMAT(I1,I2,I3,D1,D2,S220)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,N1,M2,M1,N2,N0,M1,X13,S220,-1.000)
       deallocate(S220)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder3142(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N0,M1,VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S221(N2+1:M2,M1+1:N2,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K8*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S221)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N2,M2,M1,N2,N0,M1,X2,S221, 1.000)
       deallocate(S221)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder3142(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N0,M1,VBHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S222(N2+1:M2,M1+1:N2,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K8*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S222)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N2,M2,M1,N2,N0,M1,X1,S222, 1.000)
       deallocate(S222)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:M1))
       call reorder4132(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N2,N3,N0,N2,N0,M1,VBHHHP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S223(N1+1:M2,N0+1:M1,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K5*K9
       I3=K4*K1
       call DMATMAT(I1,I2,I3,D1,D2,S223)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,N1,M2,N0,M1,N0,M1,X14,S223,-1.000)
       deallocate(S223)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N0,M1,VBHHHP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,N1,M2,t2B,D2)
       allocate(S224(N2+1:M2,N1+1:M2,N2+1:N3,N0+1:M1))
       I1=K5*K4
       I2=K9*K0
       I3=K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,S224)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N2,N3,N2,M2,N1,M2,N0,M1,X15,S224, 1.000)
       deallocate(S224)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:M1))
       call reorder3412(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N0,M1,VBHHHP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,N1,M2,t2B,D2)
       allocate(S225(N2+1:M2,N1+1:M2,N2+1:N3,N0+1:M1))
       I1=K5*K4
       I2=K9*K0
       I3=K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,S225)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N2,N3,N2,M2,N1,M2,N0,M1,X16,S225, 1.000)
       deallocate(S225)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,N3,N1,N3,N1,M2,VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S226(N2+1:M2,N0+1:M1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K5*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S226)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N1,N3,N2,M2,N1,M2,N0,M1,X8,S226, 1.000)
       deallocate(S226)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,N3,N1,N3,N1,M2,VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S227(N2+1:M2,N0+1:M1,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K5*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S227)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N1,N3,N2,M2,N1,M2,N0,M1,X7,S227, 1.000)
       deallocate(S227)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:M2))
       call reorder1234(N2,N3,N1,N3,N0,N2,N1,N3,
     & N2,N3,N1,N3,N0,N2,N1,M2,VBPHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,M1+1:N2,N0+1:M1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,M1,N2,N0,M1,t2B,D2)
       allocate(S228(M1+1:N2,N0+1:M1,N0+1:N2,N1+1:M2))
       I1=K9*K2
       I2=K5*K8
       I3=K3*K4
       call DMATMAT(I1,I2,I3,D1,D2,S228)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,N2,N1,M2,M1,N2,N0,M1,X13,S228, 1.000)
       deallocate(S228)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,N3,N1,N3,N1,M2,VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S229(N2+1:M2,M1+1:N2,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K8*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S229)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N1,N3,N2,M2,N1,M2,M1,N2,X6,S229, 1.000)
       deallocate(S229)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N1+1:M2))
       call reorder3124(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N2,N3,N1,N3,N1,M2,VBPHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S230(N2+1:M2,M1+1:N2,N1+1:N3,N1+1:M2))
       I1=K9*K3
       I2=K8*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S230)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N1,N3,N2,M2,N1,M2,M1,N2,X5,S230, 1.000)
       deallocate(S230)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N1+1:M2))
       call reorder1234(N2,N3,N1,N3,N0,N2,N1,N3,
     & N2,N3,N1,N3,N0,N2,N1,M2,VBPHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,M1,N0,M1,t2B,D2)
       allocate(S231(N0+1:M1,N0+1:M1,N0+1:N2,N1+1:M2))
       I1=K9*K2
       I2=K5*K5
       I3=K3*K4
       call DMATMAT(I1,I2,I3,D1,D2,S231)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,N2,N1,M2,N0,M1,N0,M1,X14,S231, 1.000)
       deallocate(S231)
C
       allocate(D1(N0+1:N2,N1+1:N3,N2+1:N3,N1+1:M2))
       call reorder3214(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N1,N3,N2,N3,N1,M2,VBPHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S232(N2+1:M2,N0+1:M1,N2+1:N3,N1+1:M2))
       I1=K9*K4
       I2=K5*K0
       I3=K3*K2
       call DMATMAT(I1,I2,I3,D1,D2,S232)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,N2,M2,N1,M2,N0,M1,X15,S232,-1.000)
       deallocate(S232)
C
       allocate(D1(N0+1:N2,N1+1:N3,N2+1:N3,N1+1:M2))
       call reorder3214(N2,N3,N1,N3,N0,N2,N1,N3,
     & N0,N2,N1,N3,N2,N3,N1,M2,VBPHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S233(N2+1:M2,N0+1:M1,N2+1:N3,N1+1:M2))
       I1=K9*K4
       I2=K5*K0
       I3=K3*K2
       call DMATMAT(I1,I2,I3,D1,D2,S233)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,N2,M2,N1,M2,N0,M1,X16,S233,-1.000)
       deallocate(S233)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,M1,N2,VCHHHP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,M2,N2,M2,t2C,D2)
       allocate(S234(N2+1:M2,N2+1:M2,N2+1:N3,M1+1:N2))
       I1=K8*K4
       I2=K0*K0
       I3=K2*K2
       call DMATMAT(I1,I2,I3,D1,D2,S234)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N2,N3,N2,M2,N2,M2,M1,N2,X11,S234, 0.500)
       deallocate(S234)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder3142(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,M1,N2,VCHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S235(N2+1:M2,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S235)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,N2,M2,N0,M1,M1,N2,X10,S235, 1.000)
       deallocate(S235)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder3142(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,M1,N2,VCHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S236(N2+1:M2,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S236)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,N2,M2,N0,M1,M1,N2,X9,S236, 1.000)
       deallocate(S236)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder4132(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,M1,N2,VCHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S237(N1+1:M2,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K9
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S237)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N2,N1,M2,M1,N2,N0,M1,X13,S237,-1.000)
       deallocate(S237)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:M1))
       call reorder3412(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N0,M1,VCHHHP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,M2,N2,M2,t2C,D2)
       allocate(S238(N2+1:M2,N2+1:M2,N2+1:N3,N0+1:M1))
       I1=K5*K4
       I2=K0*K0
       I3=K2*K2
       call DMATMAT(I1,I2,I3,D1,D2,S238)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N2,N3,N2,M2,N2,M2,N0,M1,X12,S238, 0.500)
       deallocate(S238)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,M2,VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S239(N2+1:M2,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S239)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,N3,N2,M2,N2,M2,N0,M1,X12,S239,-1.000)
       deallocate(S239)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,M2,VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S240(N2+1:M2,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S240)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,N2,M2,N2,M2,N0,M1,X12,S240, 1.000)
       deallocate(S240)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:M1))
       call reorder3142(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N0,M1,VCHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S241(N2+1:M2,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S241)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N2,N2,M2,N0,M1,M1,N2,X10,S241,-1.000)
       deallocate(S241)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:M1))
       call reorder3142(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N0,M1,VCHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S242(N2+1:M2,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S242)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N2,N2,M2,N0,M1,M1,N2,X9,S242,-1.000)
       deallocate(S242)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:M1))
       call reorder4132(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N0,M1,VCHHHP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S243(N1+1:M2,N0+1:M1,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K5*K9
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S243)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N2,N1,M2,N0,M1,N0,M1,X14,S243,-1.000)
       deallocate(S243)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,M2,VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S244(N2+1:M2,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S244)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,N3,N2,M2,N2,M2,M1,N2,X11,S244,-1.000)
       deallocate(S244)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder4123(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,M2,VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S245(N2+1:M2,M1+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K8*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S245)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N2,N3,N2,M2,N2,M2,M1,N2,X11,S245, 1.000)
       deallocate(S245)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N2+1:M2))
       call reorder1243(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N2,N3,N0,N2,N2,M2,VCHPPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:M1,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,M1,M1,N2,t2C,D2)
       allocate(S246(N0+1:M1,M1+1:N2,N0+1:N2,N2+1:M2))
       I1=K0*K2
       I2=K8*K5
       I3=K4*K4
       call DMATMAT(I1,I2,I3,D1,D2,S246)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,N2,N2,M2,N0,M1,M1,N2,X9,S246, 0.500)
       deallocate(S246)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder4213(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,M2,VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S247(N1+1:M2,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K9
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S247)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,N3,N2,M2,N1,M2,N0,M1,X15,S247, 1.000)
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
       call DMATMAT(I1,I2,I3,D1,D2,S248)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,N2,N2,M2,N0,M1,M1,N2,X10,S248, 0.500)
       deallocate(S248)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N2+1:M2))
       call reorder4213(N2,N3,N2,N3,N2,N3,N0,N2,
     & N0,N2,N2,N3,N2,N3,N2,M2,VCHPPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S249(N1+1:M2,N0+1:M1,N2+1:N3,N2+1:M2))
       I1=K0*K4
       I2=K5*K9
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S249)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,N3,N2,M2,N1,M2,N0,M1,X16,S249, 1.000)
       deallocate(S249)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M1,N1,M2,N3,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder562134(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,N2,M2,N1,M2,N0,M1,t3B1,F2)
       allocate(S250(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K6*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,S250)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,N0,M1,X8,S250, 1.000)
       deallocate(S250)
C
       allocate(D1(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M1,N1,M2,N3,N1,N3,VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder562134(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M1,N1,M2,N3,N2,M2,N1,M2,N0,M1,t3B1,F2)
       allocate(S251(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K6*K7*K7
       call DMATMAT(I1,I2,I3,D1,F2,S251)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,N0,M1,X8,S251, 0.500)
       deallocate(S251)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M1,N1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder562134(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,N1,M2,N2,M2,N1,M2,N0,M1,t3B1,F2)
       allocate(S252(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K9*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,S252)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,N0,M1,X8,S252, 1.000)
       deallocate(S252)
C
       allocate(D1(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M1,N1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder562134(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M1,N1,N1,M2,N2,M2,N1,M2,N0,M1,t3B1,F2)
       allocate(S253(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K9*K7*K7
       call DMATMAT(I1,I2,I3,D1,F2,S253)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,N0,M1,X8,S253, 0.500)
       deallocate(S253)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M1,N1,M2,N3,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder562134(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,N2,M2,N1,M2,N0,M1,t3B1,F2)
       allocate(S254(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K6*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,S254)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,N0,M1,X7,S254, 1.000)
       deallocate(S254)
C
       allocate(D1(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M1,N1,M2,N3,N1,N3,VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder562134(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M1,N1,M2,N3,N2,M2,N1,M2,N0,M1,t3B1,F2)
       allocate(S255(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K6*K7*K7
       call DMATMAT(I1,I2,I3,D1,F2,S255)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,N0,M1,X7,S255, 0.500)
       deallocate(S255)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M1,N1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder562134(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,N1,M2,N2,M2,N1,M2,N0,M1,t3B1,F2)
       allocate(S256(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K9*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,S256)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,N0,M1,X7,S256, 1.000)
       deallocate(S256)
C
       allocate(D1(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M1,N1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder562134(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M1,N1,N1,M2,N2,M2,N1,M2,N0,M1,t3B1,F2)
       allocate(S257(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K9*K7*K7
       call DMATMAT(I1,I2,I3,D1,F2,S257)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,N0,M1,X7,S257, 0.500)
       deallocate(S257)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,N0,M1,M2,N3,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder562134(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & N0,M1,N0,M1,M2,N3,N2,M2,N1,M2,M1,N2,t3B4,F2)
       allocate(S258(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K6*K5*K5
       call DMATMAT(I1,I2,I3,D1,F2,S258)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X6,S258, 0.500)
       deallocate(S258)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M1,N1,M2,N3,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder562134(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,N2,M2,N1,M2,M1,N2,t3B1,F2)
       allocate(S259(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K6*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,S259)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X6,S259, 1.000)
       deallocate(S259)
C
       allocate(D1(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M1,N1,M2,N3,N1,N3,VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder562134(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M1,N1,M2,N3,N2,M2,N1,M2,M1,N2,t3B1,F2)
       allocate(S260(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K6*K7*K7
       call DMATMAT(I1,I2,I3,D1,F2,S260)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X6,S260, 0.500)
       deallocate(S260)
C
       allocate(D1(N0+1:M1,N0+1:M1,N1+1:M2,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,N0,M1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder562134(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & N0,M1,N0,M1,N1,M2,N2,M2,N1,M2,M1,N2,t3B4,F2)
       allocate(S261(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K9*K5*K5
       call DMATMAT(I1,I2,I3,D1,F2,S261)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X6,S261, 0.500)
       deallocate(S261)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M1,N1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder562134(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,N1,M2,N2,M2,N1,M2,M1,N2,t3B1,F2)
       allocate(S262(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K9*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,S262)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X6,S262, 1.000)
       deallocate(S262)
C
       allocate(D1(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M1,N1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder562134(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M1,N1,N1,M2,N2,M2,N1,M2,M1,N2,t3B1,F2)
       allocate(S263(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K9*K7*K7
       call DMATMAT(I1,I2,I3,D1,F2,S263)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X6,S263, 0.500)
       deallocate(S263)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,N0,M1,M2,N3,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder562134(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & N0,M1,N0,M1,M2,N3,N2,M2,N1,M2,M1,N2,t3B4,F2)
       allocate(S264(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K6*K5*K5
       call DMATMAT(I1,I2,I3,D1,F2,S264)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X5,S264, 0.500)
       deallocate(S264)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M1,N1,M2,N3,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder562134(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,N2,M2,N1,M2,M1,N2,t3B1,F2)
       allocate(S265(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K6*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,S265)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X5,S265, 1.000)
       deallocate(S265)
C
       allocate(D1(M1+1:N1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M1,N1,M2,N3,N1,N3,VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder562134(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M1,N1,M2,N3,N2,M2,N1,M2,M1,N2,t3B1,F2)
       allocate(S266(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K6*K7*K7
       call DMATMAT(I1,I2,I3,D1,F2,S266)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X5,S266, 0.500)
       deallocate(S266)
C
       allocate(D1(N0+1:M1,N0+1:M1,N1+1:M2,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,N0,M1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder562134(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & N0,M1,N0,M1,N1,M2,N2,M2,N1,M2,M1,N2,t3B4,F2)
       allocate(S267(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K9*K5*K5
       call DMATMAT(I1,I2,I3,D1,F2,S267)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X5,S267, 0.500)
       deallocate(S267)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M1,N1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder562134(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,N1,M2,N2,M2,N1,M2,M1,N2,t3B1,F2)
       allocate(S268(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K9*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,S268)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X5,S268, 1.000)
       deallocate(S268)
C
       allocate(D1(M1+1:N1,M1+1:N1,N1+1:M2,N1+1:N3))
       call reorder3421(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M1,N1,N1,M2,N1,N3,VAHHPP,D1)
       allocate(F2(M1+1:N1,M1+1:N1,N1+1:M2,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder562134(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M1,N1,N1,M2,N2,M2,N1,M2,M1,N2,t3B1,F2)
       allocate(S269(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K9*K7*K7
       call DMATMAT(I1,I2,I3,D1,F2,S269)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X5,S269, 0.500)
       deallocate(S269)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,M2,N3,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder623145(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,N0,M1,N0,M1,t3B3,F2)
       allocate(S270(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K0
       I3=K6*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,S270)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,N0,M1,N0,M1,X4,S270,-0.500)
       deallocate(S270)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder623145(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,N2,M2,N0,M1,N0,M1,t3B1,F2)
       allocate(S271(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K0
       I3=K9*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,S271)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,N0,M1,N0,M1,X4,S271,-1.000)
       deallocate(S271)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,N1,M2,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder623145(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,N1,M2,N1,M2,N2,M2,N0,M1,N0,M1,t3B1,F2)
       allocate(S272(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K0
       I3=K9*K9*K7
       call DMATMAT(I1,I2,I3,D1,F2,S272)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,N0,M1,N0,M1,X4,S272,-0.500)
       deallocate(S272)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S273(N2+1:M2,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K8*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S273)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,M2,N3,N2,M2,M1,N2,X17,S273, 1.000)
       deallocate(S273)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S274(N2+1:M2,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K8*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S274)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,N1,M2,N2,M2,M1,N2,X18,S274, 1.000)
       deallocate(S274)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,M2,N3,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder623145(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,N0,M1,N0,M1,t3B3,F2)
       allocate(S275(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K0
       I3=K6*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,S275)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,N0,M1,N0,M1,X3,S275,-0.500)
       deallocate(S275)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder623145(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,N2,M2,N0,M1,N0,M1,t3B1,F2)
       allocate(S276(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K0
       I3=K9*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,S276)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,N0,M1,N0,M1,X3,S276,-1.000)
       deallocate(S276)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,N1,M2,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder623145(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,N1,M2,N1,M2,N2,M2,N0,M1,N0,M1,t3B1,F2)
       allocate(S277(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K0
       I3=K9*K9*K7
       call DMATMAT(I1,I2,I3,D1,F2,S277)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,N0,M1,N0,M1,X3,S277,-0.500)
       deallocate(S277)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S278(N2+1:M2,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K8*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S278)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,M2,N3,N2,M2,M1,N2,X19,S278, 1.000)
       deallocate(S278)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S279(N2+1:M2,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K8*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S279)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,N1,M2,N2,M2,M1,N2,X20,S279, 1.000)
       deallocate(S279)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder523146(N2,M2,M2,N3,M2,N3,M1,N2,N0,M1,N0,M1,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,N0,M1,t3B2,F2)
       allocate(S280(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K6*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S280)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X2,S280, 0.500)
       deallocate(S280)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,M2,N3,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder623145(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,M1,N2,N0,M1,t3B3,F2)
       allocate(S281(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K6*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,S281)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X2,S281,-0.500)
       deallocate(S281)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder523146(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & N0,M1,M2,N3,N1,M2,N2,M2,M1,N2,N0,M1,t3B4,F2)
       allocate(S282(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K9*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S282)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X2,S282, 1.000)
       deallocate(S282)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder623145(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,N2,M2,M1,N2,N0,M1,t3B1,F2)
       allocate(S283(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K9*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,S283)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X2,S283,-1.000)
       deallocate(S283)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,N1,M2,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N1+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder523146(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & N0,M1,N1,M2,N1,M2,N2,M2,M1,N2,N0,M1,t3B4,F2)
       allocate(S284(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K9*K9*K5
       call DMATMAT(I1,I2,I3,D1,F2,S284)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X2,S284, 0.500)
       deallocate(S284)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,N1,M2,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder623145(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,N1,M2,N1,M2,N2,M2,M1,N2,N0,M1,t3B1,F2)
       allocate(S285(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K9*K9*K7
       call DMATMAT(I1,I2,I3,D1,F2,S285)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X2,S285,-0.500)
       deallocate(S285)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S286(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S286)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,N2,M2,N0,M1,X21,S286, 1.000)
       deallocate(S286)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S287(N2+1:M2,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S287)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,M2,N3,N2,M2,N0,M1,X22,S287, 1.000)
       deallocate(S287)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S288(N2+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S288)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N1,M2,N2,M2,N0,M1,X23,S288, 1.000)
       deallocate(S288)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S289(N2+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K5*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S289)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,N1,M2,N2,M2,N0,M1,X24,S289, 1.000)
       deallocate(S289)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder523146(N2,M2,M2,N3,M2,N3,M1,N2,N0,M1,N0,M1,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,N0,M1,t3B2,F2)
       allocate(S290(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K6*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S290)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X1,S290, 0.500)
       deallocate(S290)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,M2,N3,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder623145(N2,M2,M2,N3,M2,N3,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,M1,N2,N0,M1,t3B3,F2)
       allocate(S291(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K6*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,S291)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X1,S291,-0.500)
       deallocate(S291)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder523146(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & N0,M1,M2,N3,N1,M2,N2,M2,M1,N2,N0,M1,t3B4,F2)
       allocate(S292(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K9*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S292)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X1,S292, 1.000)
       deallocate(S292)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder623145(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,N2,M2,M1,N2,N0,M1,t3B1,F2)
       allocate(S293(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K9*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,S293)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X1,S293,-1.000)
       deallocate(S293)
C
       allocate(D1(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,M1,N1,M2,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(N0+1:M1,N1+1:M2,N1+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder523146(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & N0,M1,N1,M2,N1,M2,N2,M2,M1,N2,N0,M1,t3B4,F2)
       allocate(S294(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K9*K9*K5
       call DMATMAT(I1,I2,I3,D1,F2,S294)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X1,S294, 0.500)
       deallocate(S294)
C
       allocate(D1(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & M1,N1,N1,M2,N1,M2,N0,N1,VAHHPP,D1)
       allocate(F2(M1+1:N1,N1+1:M2,N1+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder623145(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,N1,M2,N1,M2,N2,M2,M1,N2,N0,M1,t3B1,F2)
       allocate(S295(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K9*K9*K7
       call DMATMAT(I1,I2,I3,D1,F2,S295)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X1,S295,-0.500)
       deallocate(S295)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S296(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S296)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,N2,M2,N0,M1,X25,S296, 1.000)
       deallocate(S296)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,M2,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S297(N2+1:M2,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S297)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,M2,N3,N2,M2,N0,M1,X26,S297, 1.000)
       deallocate(S297)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:M2))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S298(N2+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S298)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N1,M2,N2,M2,N0,M1,X27,S298, 1.000)
       deallocate(S298)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,N1,M2,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S299(N2+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K5*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S299)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,N1,M2,N2,M2,N0,M1,X28,S299, 1.000)
       deallocate(S299)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder561234(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N2,M2,N1,M2,N0,M1,t3C1,F2)
       allocate(S300(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K6*K5*K8
       call DMATMAT(I1,I2,I3,D1,F2,S300)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,N0,M1,X8,S300, 1.000)
       deallocate(S300)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & N0,M1,M1,N1,M2,N3,N2,M2,N1,M2,N0,M1,t3C4,F2)
       allocate(S301(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K6*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,S301)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,N0,M1,X8,S301,-1.000)
       deallocate(S301)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder561234(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N2,M2,N1,M2,N0,M1,t3C1,F2)
       allocate(S302(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K6*K7*K8
       call DMATMAT(I1,I2,I3,D1,F2,S302)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,N0,M1,X8,S302, 1.000)
       deallocate(S302)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder561234(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,N2,M2,N1,M2,N0,M1,t3C1,F2)
       allocate(S303(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K0*K5*K8
       call DMATMAT(I1,I2,I3,D1,F2,S303)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,N0,M1,X8,S303, 1.000)
       deallocate(S303)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & N0,M1,M1,N1,N2,M2,N2,M2,N1,M2,N0,M1,t3C4,F2)
       allocate(S304(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K0*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,S304)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,N0,M1,X8,S304,-1.000)
       deallocate(S304)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder561234(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,N2,M2,N1,M2,N0,M1,t3C1,F2)
       allocate(S305(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K0*K7*K8
       call DMATMAT(I1,I2,I3,D1,F2,S305)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,N0,M1,X8,S305, 1.000)
       deallocate(S305)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder561234(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N2,M2,N1,M2,N0,M1,t3C1,F2)
       allocate(S306(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K6*K5*K8
       call DMATMAT(I1,I2,I3,D1,F2,S306)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,N0,M1,X7,S306, 1.000)
       deallocate(S306)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & N0,M1,M1,N1,M2,N3,N2,M2,N1,M2,N0,M1,t3C4,F2)
       allocate(S307(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K6*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,S307)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,N0,M1,X7,S307,-1.000)
       deallocate(S307)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder561234(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N2,M2,N1,M2,N0,M1,t3C1,F2)
       allocate(S308(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K6*K7*K8
       call DMATMAT(I1,I2,I3,D1,F2,S308)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,N0,M1,X7,S308, 1.000)
       deallocate(S308)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder561234(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,N2,M2,N1,M2,N0,M1,t3C1,F2)
       allocate(S309(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K0*K5*K8
       call DMATMAT(I1,I2,I3,D1,F2,S309)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,N0,M1,X7,S309, 1.000)
       deallocate(S309)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & N0,M1,M1,N1,N2,M2,N2,M2,N1,M2,N0,M1,t3C4,F2)
       allocate(S310(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K0*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,S310)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,N0,M1,X7,S310,-1.000)
       deallocate(S310)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder561234(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,N2,M2,N1,M2,N0,M1,t3C1,F2)
       allocate(S311(N2+1:M2,N1+1:M2,N0+1:M1,N1+1:N3))
       I1=K3
       I2=K5*K9*K0
       I3=K0*K7*K8
       call DMATMAT(I1,I2,I3,D1,F2,S311)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,N0,M1,X7,S311, 1.000)
       deallocate(S311)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder563124(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N2,M2,N2,M2,N0,M1,t3C1,F2)
       allocate(S312(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K6*K5*K8
       call DMATMAT(I1,I2,I3,D1,F2,S312)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,N0,M1,X12,S312, 1.000)
       deallocate(S312)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & N0,M1,M1,N1,M2,N3,N2,M2,N2,M2,N0,M1,t3C4,F2)
       allocate(S313(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K6*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,S313)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,N0,M1,X12,S313,-1.000)
       deallocate(S313)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder563124(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N2,M2,N2,M2,N0,M1,t3C1,F2)
       allocate(S314(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K6*K7*K8
       call DMATMAT(I1,I2,I3,D1,F2,S314)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,N0,M1,X12,S314, 1.000)
       deallocate(S314)
C
       allocate(D1(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder563124(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,N1,M2,N2,M2,N2,M2,N0,M1,t3C1,F2)
       allocate(S315(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K9*K5*K8
       call DMATMAT(I1,I2,I3,D1,F2,S315)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,N0,M1,X12,S315, 1.000)
       deallocate(S315)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & N0,M1,M1,N1,N1,M2,N2,M2,N2,M2,N0,M1,t3C4,F2)
       allocate(S316(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K9*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,S316)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,N0,M1,X12,S316,-1.000)
       deallocate(S316)
C
       allocate(D1(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder563124(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,N1,M2,N2,M2,N2,M2,N0,M1,t3C1,F2)
       allocate(S317(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K9*K7*K8
       call DMATMAT(I1,I2,I3,D1,F2,S317)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,N0,M1,X12,S317, 1.000)
       deallocate(S317)
C
       allocate(D1(N2+1:N3,N1+1:N3,M1+1:N2,N0+1:M1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,M1,N2,N0,M1,VBHHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,M1+1:N2,N0+1:M1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,M1,N2,N0,M1,t2B,D2)
       allocate(S318(M1+1:N2,N0+1:M1,M1+1:N2,N0+1:M1))
       I1=K5*K8
       I2=K5*K8
       I3=K3*K4
       call DMATMAT(I1,I2,I3,D1,D2,S318)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,N0,M1,M1,N2,N0,M1,X44,S318, 1.000)
       deallocate(S318)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,M1,M1,N1,VBHHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,M1+1:N2,N0+1:M1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,M1,N2,N0,M1,t2B,D2)
       allocate(S319(M1+1:N2,N0+1:M1,N0+1:M1,M1+1:N1))
       I1=K7*K5
       I2=K5*K8
       I3=K3*K4
       call DMATMAT(I1,I2,I3,D1,D2,S319)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M1,N1,M1,N2,N0,M1,X45,S319, 1.000)
       deallocate(S319)
C
       allocate(D1(N2+1:N3,N1+1:N3,M1+1:N2,M1+1:N1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,M1,N2,M1,N1,VBHHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,M1+1:N2,N0+1:M1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,M1,N2,N0,M1,t2B,D2)
       allocate(S320(M1+1:N2,N0+1:M1,M1+1:N2,M1+1:N1))
       I1=K7*K8
       I2=K5*K8
       I3=K3*K4
       call DMATMAT(I1,I2,I3,D1,D2,S320)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,M1,N1,M1,N2,N0,M1,X46,S320, 1.000)
       deallocate(S320)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N0,M1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N0,M1,M2,N3,N2,M2,N1,M2,M1,N2,t3C1,F2)
       allocate(S321(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K6*K5*K5
       call DMATMAT(I1,I2,I3,D1,F2,S321)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X6,S321,-1.000)
       deallocate(S321)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N2,M2,N1,M2,M1,N2,t3C1,F2)
       allocate(S322(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K6*K5*K8
       call DMATMAT(I1,I2,I3,D1,F2,S322)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X6,S322,-1.000)
       deallocate(S322)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N2,M2,N1,M2,M1,N2,t3C1,F2)
       allocate(S323(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K6*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,S323)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X6,S323,-1.000)
       deallocate(S323)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N2,M2,N1,M2,M1,N2,t3C1,F2)
       allocate(S324(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K6*K7*K8
       call DMATMAT(I1,I2,I3,D1,F2,S324)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X6,S324,-1.000)
       deallocate(S324)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N0,M1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N0,M1,N2,M2,N2,M2,N1,M2,M1,N2,t3C1,F2)
       allocate(S325(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K0*K5*K5
       call DMATMAT(I1,I2,I3,D1,F2,S325)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X6,S325,-1.000)
       deallocate(S325)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,N2,M2,N1,M2,M1,N2,t3C1,F2)
       allocate(S326(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K0*K5*K8
       call DMATMAT(I1,I2,I3,D1,F2,S326)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X6,S326,-1.000)
       deallocate(S326)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N1,N2,M2,N2,M2,N1,M2,M1,N2,t3C1,F2)
       allocate(S327(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K0*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,S327)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X6,S327,-1.000)
       deallocate(S327)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,N2,M2,N1,M2,M1,N2,t3C1,F2)
       allocate(S328(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K0*K7*K8
       call DMATMAT(I1,I2,I3,D1,F2,S328)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X6,S328,-1.000)
       deallocate(S328)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N0,M1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N0,M1,M2,N3,N2,M2,N1,M2,M1,N2,t3C1,F2)
       allocate(S329(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K6*K5*K5
       call DMATMAT(I1,I2,I3,D1,F2,S329)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X5,S329,-1.000)
       deallocate(S329)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N2,M2,N1,M2,M1,N2,t3C1,F2)
       allocate(S330(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K6*K5*K8
       call DMATMAT(I1,I2,I3,D1,F2,S330)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X5,S330,-1.000)
       deallocate(S330)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N2,M2,N1,M2,M1,N2,t3C1,F2)
       allocate(S331(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K6*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,S331)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X5,S331,-1.000)
       deallocate(S331)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N2,M2,N1,M2,M1,N2,t3C1,F2)
       allocate(S332(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K6*K7*K8
       call DMATMAT(I1,I2,I3,D1,F2,S332)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X5,S332,-1.000)
       deallocate(S332)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N0,M1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N0,M1,N2,M2,N2,M2,N1,M2,M1,N2,t3C1,F2)
       allocate(S333(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K0*K5*K5
       call DMATMAT(I1,I2,I3,D1,F2,S333)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X5,S333,-1.000)
       deallocate(S333)
C
       allocate(D1(M1+1:N2,N0+1:M1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,N2,M2,N1,M2,M1,N2,t3C1,F2)
       allocate(S334(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K0*K5*K8
       call DMATMAT(I1,I2,I3,D1,F2,S334)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X5,S334,-1.000)
       deallocate(S334)
C
       allocate(D1(N0+1:M1,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N1,N2,M2,N2,M2,N1,M2,M1,N2,t3C1,F2)
       allocate(S335(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K0*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,S335)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X5,S335,-1.000)
       deallocate(S335)
C
       allocate(D1(M1+1:N2,M1+1:N1,N2+1:M2,N1+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,N1,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,N2,M2,N1,M2,M1,N2,t3C1,F2)
       allocate(S336(N2+1:M2,N1+1:M2,M1+1:N2,N1+1:N3))
       I1=K3
       I2=K8*K9*K0
       I3=K0*K7*K8
       call DMATMAT(I1,I2,I3,D1,F2,S336)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N1,N3,N2,M2,N1,M2,M1,N2,X5,S336,-1.000)
       deallocate(S336)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N0,M1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N0,M1,M2,N3,N2,M2,N2,M2,M1,N2,t3C1,F2)
       allocate(S337(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K6*K5*K5
       call DMATMAT(I1,I2,I3,D1,F2,S337)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X11,S337,-1.000)
       deallocate(S337)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N2,M2,N2,M2,M1,N2,t3C1,F2)
       allocate(S338(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K6*K5*K8
       call DMATMAT(I1,I2,I3,D1,F2,S338)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X11,S338,-1.000)
       deallocate(S338)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N2,M2,N2,M2,M1,N2,t3C1,F2)
       allocate(S339(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K6*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,S339)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X11,S339,-1.000)
       deallocate(S339)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N2,M2,N2,M2,M1,N2,t3C1,F2)
       allocate(S340(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K6*K7*K8
       call DMATMAT(I1,I2,I3,D1,F2,S340)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X11,S340,-1.000)
       deallocate(S340)
C
       allocate(D1(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N0,M1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N0,M1,N1,M2,N2,M2,N2,M2,M1,N2,t3C1,F2)
       allocate(S341(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K9*K5*K5
       call DMATMAT(I1,I2,I3,D1,F2,S341)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X11,S341,-1.000)
       deallocate(S341)
C
       allocate(D1(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,N1,M2,N2,M2,N2,M2,M1,N2,t3C1,F2)
       allocate(S342(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K9*K5*K8
       call DMATMAT(I1,I2,I3,D1,F2,S342)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X11,S342,-1.000)
       deallocate(S342)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N1,N1,M2,N2,M2,N2,M2,M1,N2,t3C1,F2)
       allocate(S343(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K9*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,S343)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X11,S343,-1.000)
       deallocate(S343)
C
       allocate(D1(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder463125(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,N1,M2,N2,M2,N2,M2,M1,N2,t3C1,F2)
       allocate(S344(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K9*K7*K8
       call DMATMAT(I1,I2,I3,D1,F2,S344)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X11,S344,-1.000)
       deallocate(S344)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,M1,N0,M1,VBHHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,M1,N0,M1,t2B,D2)
       allocate(S345(N0+1:M1,N0+1:M1,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K5*K5
       I3=K3*K4
       call DMATMAT(I1,I2,I3,D1,D2,S345)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N0,M1,N0,M1,N0,M1,X47,S345, 1.000)
       deallocate(S345)
C
       allocate(D1(N2+1:N3,N1+1:N3,M1+1:N2,N0+1:M1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,M1,N2,N0,M1,VBHHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,M1,N0,M1,t2B,D2)
       allocate(S346(N0+1:M1,N0+1:M1,M1+1:N2,N0+1:M1))
       I1=K5*K8
       I2=K5*K5
       I3=K3*K4
       call DMATMAT(I1,I2,I3,D1,D2,S346)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,N0,M1,N0,M1,N0,M1,X48,S346, 1.000)
       deallocate(S346)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:M1,M1+1:N1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,M1,M1,N1,VBHHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,M1,N0,M1,t2B,D2)
       allocate(S347(N0+1:M1,N0+1:M1,N0+1:M1,M1+1:N1))
       I1=K7*K5
       I2=K5*K5
       I3=K3*K4
       call DMATMAT(I1,I2,I3,D1,D2,S347)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M1,N1,N0,M1,N0,M1,X49,S347, 1.000)
       deallocate(S347)
C
       allocate(D1(N2+1:N3,N1+1:N3,M1+1:N2,M1+1:N1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,M1,N2,M1,N1,VBHHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,M1,N0,M1,t2B,D2)
       allocate(S348(N0+1:M1,N0+1:M1,M1+1:N2,M1+1:N1))
       I1=K7*K8
       I2=K5*K5
       I3=K3*K4
       call DMATMAT(I1,I2,I3,D1,D2,S348)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,M1,N1,N0,M1,N0,M1,X50,S348, 1.000)
       deallocate(S348)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(S349(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K6*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S349)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X10,S349,-1.000)
       deallocate(S349)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(S350(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K6*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,S350)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X10,S350,-1.000)
       deallocate(S350)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(S351(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K6*K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,S351)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X10,S351,-1.000)
       deallocate(S351)
C
       allocate(D1(M1+1:N1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,N2,M2,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,M2,N3,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(S352(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K6*K0*K7
       call DMATMAT(I1,I2,I3,D1,F2,S352)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X10,S352,-1.000)
       deallocate(S352)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(S353(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K9*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S353)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X10,S353,-1.000)
       deallocate(S353)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,N1,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(S354(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K9*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,S354)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X10,S354,-1.000)
       deallocate(S354)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(S355(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K9*K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,S355)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X10,S355,-1.000)
       deallocate(S355)
C
       allocate(D1(M1+1:N1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,N2,M2,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,N1,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(S356(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K9*K0*K7
       call DMATMAT(I1,I2,I3,D1,F2,S356)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X10,S356,-1.000)
       deallocate(S356)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder3241(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S357(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K0
       I3=K3*K2
       call DMATMAT(I1,I2,I3,D1,D2,S357)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,N2,M2,N0,M1,X90,S357,-1.000)
       deallocate(S357)
C
       allocate(D1(N0+1:N2,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder3241(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,M1,N1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S358(N2+1:M2,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K0
       I3=K3*K2
       call DMATMAT(I1,I2,I3,D1,D2,S358)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,M2,N3,N2,M2,N0,M1,X94,S358,-1.000)
       deallocate(S358)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder3241(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N0,M1,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S359(N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K0
       I3=K3*K2
       call DMATMAT(I1,I2,I3,D1,D2,S359)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N2,M2,N2,M2,N0,M1,X92,S359,-1.000)
       deallocate(S359)
C
       allocate(D1(N0+1:N2,N1+1:N3,M1+1:N1,N2+1:M2))
       call reorder3241(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,M1,N1,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S360(N2+1:M2,N0+1:M1,M1+1:N1,N2+1:M2))
       I1=K0*K7
       I2=K5*K0
       I3=K3*K2
       call DMATMAT(I1,I2,I3,D1,D2,S360)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,N2,M2,N2,M2,N0,M1,X96,S360,-1.000)
       deallocate(S360)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(S361(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K6*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S361)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X9,S361,-1.000)
       deallocate(S361)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(S362(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K6*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,S362)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X9,S362,-1.000)
       deallocate(S362)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(S363(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K6*K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,S363)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X9,S363,-1.000)
       deallocate(S363)
C
       allocate(D1(M1+1:N1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,N2,M2,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,M2,N3,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(S364(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K6*K0*K7
       call DMATMAT(I1,I2,I3,D1,F2,S364)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X9,S364,-1.000)
       deallocate(S364)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(S365(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K9*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S365)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X9,S365,-1.000)
       deallocate(S365)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,N1,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(S366(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K9*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,S366)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X9,S366,-1.000)
       deallocate(S366)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(S367(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K9*K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,S367)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X9,S367,-1.000)
       deallocate(S367)
C
       allocate(D1(M1+1:N1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,N2,M2,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder613245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,N1,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(S368(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K9*K0*K7
       call DMATMAT(I1,I2,I3,D1,F2,S368)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X9,S368,-1.000)
       deallocate(S368)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder3241(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S369(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K0
       I3=K3*K2
       call DMATMAT(I1,I2,I3,D1,D2,S369)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,N2,M2,N0,M1,X91,S369,-1.000)
       deallocate(S369)
C
       allocate(D1(N0+1:N2,N1+1:N3,M1+1:N1,M2+1:N3))
       call reorder3241(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,M1,N1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S370(N2+1:M2,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K0
       I3=K3*K2
       call DMATMAT(I1,I2,I3,D1,D2,S370)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,M2,N3,N2,M2,N0,M1,X95,S370,-1.000)
       deallocate(S370)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder3241(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N0,M1,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S371(N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K0
       I3=K3*K2
       call DMATMAT(I1,I2,I3,D1,D2,S371)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N2,M2,N2,M2,N0,M1,X93,S371,-1.000)
       deallocate(S371)
C
       allocate(D1(N0+1:N2,N1+1:N3,M1+1:N1,N2+1:M2))
       call reorder3241(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,M1,N1,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S372(N2+1:M2,N0+1:M1,M1+1:N1,N2+1:M2))
       I1=K0*K7
       I2=K5*K0
       I3=K3*K2
       call DMATMAT(I1,I2,I3,D1,D2,S372)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,N2,M2,N2,M2,N0,M1,X97,S372,-1.000)
       deallocate(S372)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S373(N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K9
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S373)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X40(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       X40=0.0d0
       call
     & sum3412(N0,M1,M2,N3,N1,M2,N0,M1,X40,S373,-1.000)
       deallocate(S373)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,M1,X40,VAHPHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z40(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K0
       I3=K6*K5
       call DMATMAT(I1,I2,I3,X40,F2,Z40)
       deallocate(F2)
C
       call
     & sum124536(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z40,-1.000)
       deallocate(Z40)
       deallocate(X40)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S374(N1+1:M2,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K9
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S374)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X41(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:M1))
       X41=0.0d0
       call
     & sum3412(M1,N1,M2,N3,N1,M2,N0,M1,X41,S374,-1.000)
       deallocate(S374)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,M1,X41,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z41(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K0
       I3=K6*K7
       call DMATMAT(I1,I2,I3,X41,F2,Z41)
       deallocate(F2)
C
       call
     & sum124536(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z41,-1.000)
       deallocate(Z41)
       deallocate(X41)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S375(N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K9
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S375)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X42(N0+1:M1,N1+1:M2,N1+1:M2,N0+1:M1))
       X42=0.0d0
       call
     & sum3412(N0,M1,N1,M2,N1,M2,N0,M1,X42,S375,-1.000)
       deallocate(S375)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & N0,M1,N1,M2,N1,M2,N0,M1,X42,VAHPHP, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N1,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z42(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K0
       I3=K9*K5
       call DMATMAT(I1,I2,I3,X42,F2,Z42)
       deallocate(F2)
C
       call
     & sum124536(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z42,-1.000)
       deallocate(Z42)
       deallocate(X42)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S376(N1+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K5*K9
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S376)
       deallocate(D1)
       deallocate(D2)
C
       allocate(X43(M1+1:N1,N1+1:M2,N1+1:M2,N0+1:M1))
       X43=0.0d0
       call
     & sum3412(M1,N1,N1,M2,N1,M2,N0,M1,X43,S376,-1.000)
       deallocate(S376)
C
       call sumx2431(N1,N3,N0,N1,N1,N3,N0,N1,
     & M1,N1,N1,M2,N1,M2,N0,M1,X43,VAHPHP, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N1,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z43(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K0
       I3=K9*K7
       call DMATMAT(I1,I2,I3,X43,F2,Z43)
       deallocate(F2)
C
       call
     & sum124536(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z43,-1.000)
       deallocate(Z43)
       deallocate(X43)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,M1,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,M1,t2B,D2)
       allocate(Q36(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K3*K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,Q36)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N0,M1,N0,M1,X29,Q36, 1.000)
       deallocate(Q36)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:N3,M1+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,M1,N1,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,N3,N0,M1,t2B,D2)
       allocate(Q37(N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5
       I3=K3*K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,Q37)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M1,N1,N0,M1,X30,Q37, 1.000)
       deallocate(Q37)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder513246(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,M2,N3,N2,M2,N0,M1,N0,M1,t3C1,F2)
       allocate(S377(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K0
       I3=K6*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,S377)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,N0,M1,N0,M1,X4,S377,-1.000)
       deallocate(S377)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder513246(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,N2,M2,N0,M1,N0,M1,t3C1,F2)
       allocate(S378(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K0
       I3=K6*K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,S378)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,N0,M1,N0,M1,X4,S378,-1.000)
       deallocate(S378)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder513246(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N1,M2,N2,M2,N0,M1,N0,M1,t3C1,F2)
       allocate(S379(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K0
       I3=K9*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,S379)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,N0,M1,N0,M1,X4,S379,-1.000)
       deallocate(S379)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder513246(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,N1,M2,N2,M2,N0,M1,N0,M1,t3C1,F2)
       allocate(S380(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K0
       I3=K9*K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,S380)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,N0,M1,N0,M1,X4,S380,-1.000)
       deallocate(S380)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S381(N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S381)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,M1,N2,M2,N3,
     & M1,N2,M2,N3,N2,M2,M1,N2,S381,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder512346(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N2,M2,N1,M2,N0,M1,N0,M1,t3C1,F2)
       allocate(Z528(N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K5*K9*K0
       I3=K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z528)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234615(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z528, 1.000)
       deallocate(Z528)
       deallocate(S381)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S382(N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K8*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S382)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,M1,N2,N2,M2,
     & M1,N2,N2,M2,N2,M2,M1,N2,S382,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder512346(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,N2,M2,N1,M2,N0,M1,N0,M1,t3C1,F2)
       allocate(Z529(N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K5*K9*K0
       I3=K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z529)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum234615(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z529, 1.000)
       deallocate(Z529)
       deallocate(S382)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder513246(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,M2,N3,N2,M2,N0,M1,N0,M1,t3C1,F2)
       allocate(S383(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K0
       I3=K6*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,S383)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,N0,M1,N0,M1,X3,S383,-1.000)
       deallocate(S383)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder513246(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,N2,M2,N0,M1,N0,M1,t3C1,F2)
       allocate(S384(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K0
       I3=K6*K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,S384)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,N0,M1,N0,M1,X3,S384,-1.000)
       deallocate(S384)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder513246(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N1,M2,N2,M2,N0,M1,N0,M1,t3C1,F2)
       allocate(S385(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K0
       I3=K9*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,S385)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,N0,M1,N0,M1,X3,S385,-1.000)
       deallocate(S385)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder513246(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,N1,M2,N2,M2,N0,M1,N0,M1,t3C1,F2)
       allocate(S386(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K0
       I3=K9*K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,S386)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,N0,M1,N0,M1,X3,S386,-1.000)
       deallocate(S386)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S387(N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S387)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,M1,N2,M2,N3,
     & M1,N2,M2,N3,N2,M2,M1,N2,S387,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder512346(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N2,M2,N1,M2,N0,M1,N0,M1,t3C1,F2)
       allocate(Z534(N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K5*K9*K0
       I3=K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z534)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum134625(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z534,-1.000)
       deallocate(Z534)
       deallocate(S387)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S388(N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K8*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S388)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,M1,N2,N2,M2,
     & M1,N2,N2,M2,N2,M2,M1,N2,S388,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder512346(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,N2,M2,N1,M2,N0,M1,N0,M1,t3C1,F2)
       allocate(Z535(N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K5*K9*K0
       I3=K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z535)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum134625(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z535,-1.000)
       deallocate(Z535)
       deallocate(S388)
C
       allocate(D1(N0+1:N1,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder4132(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,M1,N2,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,M2,M1,N2,t2B,D2)
       allocate(S389(N1+1:M2,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K9
       I3=K4*K1
       call DMATMAT(I1,I2,I3,D1,D2,S389)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,M2,N3,N1,M2,M1,N2,X98,S389, 1.000)
       deallocate(S389)
C
       allocate(D1(N0+1:N1,N2+1:N3,M1+1:N2,N1+1:M2))
       call reorder4132(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,M1,N2,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,M2,M1,N2,t2B,D2)
       allocate(S390(N1+1:M2,M1+1:N2,M1+1:N2,N1+1:M2))
       I1=K9*K8
       I2=K8*K9
       I3=K4*K1
       call DMATMAT(I1,I2,I3,D1,D2,S390)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,N1,M2,N1,M2,M1,N2,X99,S390, 1.000)
       deallocate(S390)
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
       call DMATMAT(I1,I2,I3,D1,D2,Q38)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M1,N2,M1,N2,X100,Q38, 1.000)
       deallocate(Q38)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(S391(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K6*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S391)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X2,S391, 1.000)
       deallocate(S391)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(S392(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K6*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,S392)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X2,S392, 1.000)
       deallocate(S392)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N2,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(S393(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K6*K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,S393)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X2,S393, 1.000)
       deallocate(S393)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,N2,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(S394(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K6*K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,S394)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X2,S394, 1.000)
       deallocate(S394)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N2,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(S395(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K9*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S395)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X2,S395, 1.000)
       deallocate(S395)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N1,M2,N2,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(S396(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K9*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,S396)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X2,S396, 1.000)
       deallocate(S396)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N2,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(S397(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K9*K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,S397)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X2,S397, 1.000)
       deallocate(S397)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,N1,M2,N2,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(S398(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K9*K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,S398)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X2,S398, 1.000)
       deallocate(S398)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S399(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S399)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3412(N2,M2,N0,M1,N0,M1,M2,N3,
     & N0,M1,M2,N3,N2,M2,N0,M1,S399,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N2,M2,N1,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z547(N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K5*K8*K9*K0
       I3=K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z547)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum235614(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z547, 1.000)
       deallocate(Z547)
       deallocate(S399)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S400(N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S400)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3412(N2,M2,N0,M1,M1,N2,M2,N3,
     & M1,N2,M2,N3,N2,M2,N0,M1,S400,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N2,M2,N1,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z548(N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K5*K8*K9*K0
       I3=K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z548)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum235614(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z548, 1.000)
       deallocate(Z548)
       deallocate(S400)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S401(N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S401)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder3412(N2,M2,N0,M1,N0,M1,N2,M2,
     & N0,M1,N2,M2,N2,M2,N0,M1,S401,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N2,M2,N1,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z549(N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K5*K8*K9*K0
       I3=K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z549)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum235614(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z549, 1.000)
       deallocate(Z549)
       deallocate(S401)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S402(N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S402)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder3412(N2,M2,N0,M1,M1,N2,N2,M2,
     & M1,N2,N2,M2,N2,M2,N0,M1,S402,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,N2,M2,N1,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z550(N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K5*K8*K9*K0
       I3=K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z550)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum235614(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z550, 1.000)
       deallocate(Z550)
       deallocate(S402)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N2,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(S403(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K6*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S403)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X1,S403, 1.000)
       deallocate(S403)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,M2,N3,N2,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(S404(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K6*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,S404)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X1,S404, 1.000)
       deallocate(S404)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N2,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(S405(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K6*K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,S405)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X1,S405, 1.000)
       deallocate(S405)
C
       allocate(D1(M1+1:N2,N2+1:M2,M2+1:N3,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,M2+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,M2,N3,N2,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(S406(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K6*K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,S406)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X1,S406, 1.000)
       deallocate(S406)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N2,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(S407(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K9*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S407)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X1,S407, 1.000)
       deallocate(S407)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M2,N3,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N1+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N1,M2,N2,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(S408(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K9*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,S408)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X1,S408, 1.000)
       deallocate(S408)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N2,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(S409(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K9*K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,S409)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X1,S409, 1.000)
       deallocate(S409)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:N1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N2,M2,N1,M2,N0,N1,VBHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N1+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder413256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,N1,M2,N2,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(S410(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K9*K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,S410)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X1,S410, 1.000)
       deallocate(S410)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S411(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S411)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3412(N2,M2,N0,M1,N0,M1,M2,N3,
     & N0,M1,M2,N3,N2,M2,N0,M1,S411,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N2,M2,N1,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z559(N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K5*K8*K9*K0
       I3=K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z559)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum135624(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z559,-1.000)
       deallocate(Z559)
       deallocate(S411)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S412(N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S412)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3412(N2,M2,N0,M1,M1,N2,M2,N3,
     & M1,N2,M2,N3,N2,M2,N0,M1,S412,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N2,M2,N1,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z560(N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K5*K8*K9*K0
       I3=K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z560)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum135624(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z560,-1.000)
       deallocate(Z560)
       deallocate(S412)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S413(N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S413)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder3412(N2,M2,N0,M1,N0,M1,N2,M2,
     & N0,M1,N2,M2,N2,M2,N0,M1,S413,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N2,M2,N1,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z561(N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K5*K8*K9*K0
       I3=K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z561)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum135624(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z561,-1.000)
       deallocate(Z561)
       deallocate(S413)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S414(N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S414)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder3412(N2,M2,N0,M1,M1,N2,N2,M2,
     & M1,N2,N2,M2,N2,M2,N0,M1,S414,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,N2,M2,N1,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z562(N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K5*K8*K9*K0
       I3=K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z562)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum135624(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z562,-1.000)
       deallocate(Z562)
       deallocate(S414)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder4132(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S415(N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K9
       I3=K4*K1
       call DMATMAT(I1,I2,I3,D1,D2,S415)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,N1,M2,N0,M1,X101,S415, 1.000)
       deallocate(S415)
C
       allocate(D1(N0+1:N1,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder4132(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,M1,N2,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S416(N1+1:M2,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K9
       I3=K4*K1
       call DMATMAT(I1,I2,I3,D1,D2,S416)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,M2,N3,N1,M2,N0,M1,X102,S416, 1.000)
       deallocate(S416)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder4132(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N0,M1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S417(N1+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K9
       I3=K4*K1
       call DMATMAT(I1,I2,I3,D1,D2,S417)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N1,M2,N1,M2,N0,M1,X103,S417, 1.000)
       deallocate(S417)
C
       allocate(D1(N0+1:N1,N2+1:N3,M1+1:N2,N1+1:M2))
       call reorder4132(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,M1,N2,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S418(N1+1:M2,N0+1:M1,M1+1:N2,N1+1:M2))
       I1=K9*K8
       I2=K5*K9
       I3=K4*K1
       call DMATMAT(I1,I2,I3,D1,D2,S418)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,N1,M2,N1,M2,N0,M1,X104,S418, 1.000)
       deallocate(S418)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:M1))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,M1,VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:M1))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,M1,t2B,D2)
       allocate(Q39(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K3*K4*K1
       call DMATMAT(I1,I2,I3,D1,D2,Q39)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N0,M1,N0,M1,X105,Q39, 1.000)
       deallocate(Q39)
C
       allocate(D1(N0+1:N1,N2+1:N3,N1+1:N3,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,M1,N2,VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:M1))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,N3,N0,M1,t2B,D2)
       allocate(Q40(N0+1:M1,M1+1:N2))
       I1=K8
       I2=K5
       I3=K3*K4*K1
       call DMATMAT(I1,I2,I3,D1,D2,Q40)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M1,N2,N0,M1,X106,Q40, 1.000)
       deallocate(Q40)
C
       allocate(D1(N0+1:N2,N0+1:N1,M2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,M2,N3,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,N1,M2,t2B,D2)
       allocate(S419(N2+1:M2,N1+1:M2,M2+1:N3,M2+1:N3))
       I1=K6*K6
       I2=K9*K0
       I3=K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,S419)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M2,N3,M2,N3,N2,M2,N1,M2,X65,S419, 1.000)
       deallocate(S419)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:M2,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,N1,M2,t2B,D2)
       allocate(S420(N2+1:M2,N1+1:M2,N2+1:M2,M2+1:N3))
       I1=K6*K0
       I2=K9*K0
       I3=K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,S420)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,M2,M2,N3,N2,M2,N1,M2,X66,S420, 1.000)
       deallocate(S420)
C
       allocate(D1(N0+1:N2,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,M2,N3,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,N1,M2,t2B,D2)
       allocate(S421(N2+1:M2,N1+1:M2,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K9*K0
       I3=K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,S421)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M2,N3,N1,M2,N2,M2,N1,M2,X67,S421, 1.000)
       deallocate(S421)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,N1,M2,t2B,D2)
       allocate(S422(N2+1:M2,N1+1:M2,N2+1:M2,N1+1:M2))
       I1=K9*K0
       I2=K9*K0
       I3=K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,S422)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,M2,N1,M2,N2,M2,N1,M2,X68,S422, 1.000)
       deallocate(S422)
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
       call DMATMAT(I1,I2,I3,D1,D2,Q41)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M2,N3,N2,M2,X107,Q41,-1.000)
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
       call DMATMAT(I1,I2,I3,D1,D2,Q42)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N2,M2,N2,M2,X109,Q42,-1.000)
       deallocate(Q42)
C
       allocate(D1(N0+1:N2,N0+1:N1,M2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,M2,N3,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,N1,M2,t2B,D2)
       allocate(S423(N2+1:M2,N1+1:M2,M2+1:N3,M2+1:N3))
       I1=K6*K6
       I2=K9*K0
       I3=K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,S423)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M2,N3,M2,N3,N2,M2,N1,M2,X69,S423, 1.000)
       deallocate(S423)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:M2,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,N1,M2,t2B,D2)
       allocate(S424(N2+1:M2,N1+1:M2,N2+1:M2,M2+1:N3))
       I1=K6*K0
       I2=K9*K0
       I3=K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,S424)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,M2,M2,N3,N2,M2,N1,M2,X70,S424, 1.000)
       deallocate(S424)
C
       allocate(D1(N0+1:N2,N0+1:N1,M2+1:N3,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,M2,N3,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,N1,M2,t2B,D2)
       allocate(S425(N2+1:M2,N1+1:M2,M2+1:N3,N1+1:M2))
       I1=K9*K6
       I2=K9*K0
       I3=K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,S425)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M2,N3,N1,M2,N2,M2,N1,M2,X71,S425, 1.000)
       deallocate(S425)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,N1,M2,t2B,D2)
       allocate(S426(N2+1:M2,N1+1:M2,N2+1:M2,N1+1:M2))
       I1=K9*K0
       I2=K9*K0
       I3=K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,S426)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,M2,N1,M2,N2,M2,N1,M2,X72,S426, 1.000)
       deallocate(S426)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,M2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,M2,t2B,D2)
       allocate(Q43(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K3*K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,Q43)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M2,N3,N2,M2,X108,Q43,-1.000)
       deallocate(Q43)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N1,N3,N2,M2,t2B,D2)
       allocate(Q44(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K3*K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,Q44)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N2,M2,N2,M2,X110,Q44,-1.000)
       deallocate(Q44)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,M2,t2B,D2)
       allocate(Q45(N1+1:M2,M2+1:N3))
       I1=K6
       I2=K9
       I3=K4*K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,Q45)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M2,N3,N1,M2,X31,Q45,-1.000)
       deallocate(Q45)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:N3,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,N3,N1,M2,t2B,D2)
       allocate(Q46(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K4*K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,Q46)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N1,M2,N1,M2,X32,Q46,-1.000)
       deallocate(Q46)
C
       allocate(D1(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M1,N2,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M1,N2,M2,N3,N2,M2,N2,M2,N0,M1,t3D,F2)
       allocate(S427(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K6*K8*K5
       call DMATMAT(I1,I2,I3,D1,F2,S427)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,N0,M1,X12,S427, 1.000)
       deallocate(S427)
C
       allocate(D1(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M1,N2,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder561234(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M1,N2,M2,N3,N2,M2,N2,M2,N0,M1,t3D,F2)
       allocate(S428(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K6*K8*K8
       call DMATMAT(I1,I2,I3,D1,F2,S428)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,N0,M1,X12,S428,-0.500)
       deallocate(S428)
C
       allocate(D1(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M1,N2,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M1,N2,N2,M2,N2,M2,N2,M2,N0,M1,t3D,F2)
       allocate(S429(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K0*K8*K5
       call DMATMAT(I1,I2,I3,D1,F2,S429)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,N0,M1,X12,S429, 1.000)
       deallocate(S429)
C
       allocate(D1(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M1,N2,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder561234(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M1,N2,N2,M2,N2,M2,N2,M2,N0,M1,t3D,F2)
       allocate(S430(N2+1:M2,N2+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K0*K0
       I3=K0*K8*K8
       call DMATMAT(I1,I2,I3,D1,F2,S430)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,N0,M1,X12,S430,-0.500)
       deallocate(S430)
C
       allocate(D1(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,N0,M1,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N0,M1,M2,N3,N2,M2,N2,M2,M1,N2,t3D,F2)
       allocate(S431(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K6*K5*K5
       call DMATMAT(I1,I2,I3,D1,F2,S431)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X11,S431,-0.500)
       deallocate(S431)
C
       allocate(D1(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M1,N2,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M1,N2,M2,N3,N2,M2,N2,M2,M1,N2,t3D,F2)
       allocate(S432(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K6*K8*K5
       call DMATMAT(I1,I2,I3,D1,F2,S432)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X11,S432,-1.000)
       deallocate(S432)
C
       allocate(D1(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M1,N2,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M1,N2,M2,N3,N2,M2,N2,M2,M1,N2,t3D,F2)
       allocate(S433(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K6*K8*K8
       call DMATMAT(I1,I2,I3,D1,F2,S433)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X11,S433,-0.500)
       deallocate(S433)
C
       allocate(D1(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,N0,M1,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N0,M1,N2,M2,N2,M2,N2,M2,M1,N2,t3D,F2)
       allocate(S434(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K0*K5*K5
       call DMATMAT(I1,I2,I3,D1,F2,S434)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X11,S434,-0.500)
       deallocate(S434)
C
       allocate(D1(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M1,N2,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M1,N2,N2,M2,N2,M2,N2,M2,M1,N2,t3D,F2)
       allocate(S435(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K0*K8*K5
       call DMATMAT(I1,I2,I3,D1,F2,S435)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X11,S435,-1.000)
       deallocate(S435)
C
       allocate(D1(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M1,N2,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder451236(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M1,N2,N2,M2,N2,M2,N2,M2,M1,N2,t3D,F2)
       allocate(S436(N2+1:M2,N2+1:M2,M1+1:N2,N2+1:N3))
       I1=K4
       I2=K8*K0*K0
       I3=K0*K8*K8
       call DMATMAT(I1,I2,I3,D1,F2,S436)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N2,M2,M1,N2,X11,S436,-0.500)
       deallocate(S436)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M2,N3,M2,N3,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(S437(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K6*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S437)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X9,S437,-0.500)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X10,S437,-0.500)
       deallocate(S437)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(S439(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K0*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S439)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X9,S439,-1.000)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X10,S439,-1.000)
       deallocate(S439)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,N2,M2,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(S441(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K0*K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,S441)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X9,S441,-0.500)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X10,S441,-0.500)
       deallocate(S441)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M2,N3,M2,N3,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder512346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(S438(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K6*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,S438)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X9,S438, 0.500)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X10,S438, 0.500)
       deallocate(S438)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder512346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(S440(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K0*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,S440)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X9,S440, 1.000)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X10,S440, 1.000)
       deallocate(S440)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,N2,M2,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder512346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(S442(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K0*K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,S442)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X9,S442, 0.500)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X10,S442, 0.500)
       deallocate(S442)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S443(N1+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K9
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S443)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,N1,M2,N0,M1,X115,S443, 1.000)
       deallocate(S443)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S444(N1+1:M2,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K9
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S444)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,M2,N3,N1,M2,N0,M1,X116,S444, 1.000)
       deallocate(S444)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S445(N1+1:M2,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K9
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S445)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N2,M2,N1,M2,N0,M1,X117,S445, 1.000)
       deallocate(S445)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S446(N1+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K9
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S446)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,N2,M2,N1,M2,N0,M1,X118,S446, 1.000)
       deallocate(S446)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder452136(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,N0,M1,M2,N3,N2,M2,N1,M2,N0,M1,t3B4,F2)
       allocate(S447(N2+1:M2,N1+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K9*K0
       I3=K6*K5*K8
       call DMATMAT(I1,I2,I3,D1,F2,S447)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N1,M2,N0,M1,X16,S447,-1.000)
       deallocate(S447)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder462135(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,N2,M2,N1,M2,N0,M1,t3B1,F2)
       allocate(S448(N2+1:M2,N1+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K9*K0
       I3=K6*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,S448)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N1,M2,N0,M1,X16,S448, 1.000)
       deallocate(S448)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder462135(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,M2,N3,N2,M2,N1,M2,N0,M1,t3B1,F2)
       allocate(S449(N2+1:M2,N1+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K9*K0
       I3=K6*K7*K8
       call DMATMAT(I1,I2,I3,D1,F2,S449)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N1,M2,N0,M1,X16,S449, 1.000)
       deallocate(S449)
C
       allocate(D1(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder452136(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,N0,M1,N1,M2,N2,M2,N1,M2,N0,M1,t3B4,F2)
       allocate(S450(N2+1:M2,N1+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K9*K0
       I3=K9*K5*K8
       call DMATMAT(I1,I2,I3,D1,F2,S450)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N1,M2,N0,M1,X16,S450,-1.000)
       deallocate(S450)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder462135(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,N1,M2,N2,M2,N1,M2,N0,M1,t3B1,F2)
       allocate(S451(N2+1:M2,N1+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K9*K0
       I3=K9*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,S451)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N1,M2,N0,M1,X16,S451, 1.000)
       deallocate(S451)
C
       allocate(D1(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder462135(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,N1,M2,N2,M2,N1,M2,N0,M1,t3B1,F2)
       allocate(S452(N2+1:M2,N1+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K9*K0
       I3=K9*K7*K8
       call DMATMAT(I1,I2,I3,D1,F2,S452)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N1,M2,N0,M1,X16,S452, 1.000)
       deallocate(S452)
C
       allocate(D1(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder452136(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,N0,M1,M2,N3,N2,M2,N1,M2,N0,M1,t3B4,F2)
       allocate(S453(N2+1:M2,N1+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K9*K0
       I3=K6*K5*K8
       call DMATMAT(I1,I2,I3,D1,F2,S453)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N1,M2,N0,M1,X15,S453,-1.000)
       deallocate(S453)
C
       allocate(D1(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder462135(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,M2,N3,N2,M2,N1,M2,N0,M1,t3B1,F2)
       allocate(S454(N2+1:M2,N1+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K9*K0
       I3=K6*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,S454)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N1,M2,N0,M1,X15,S454, 1.000)
       deallocate(S454)
C
       allocate(D1(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,M2,N3,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder462135(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,M2,N3,N2,M2,N1,M2,N0,M1,t3B1,F2)
       allocate(S455(N2+1:M2,N1+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K9*K0
       I3=K6*K7*K8
       call DMATMAT(I1,I2,I3,D1,F2,S455)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N1,M2,N0,M1,X15,S455, 1.000)
       deallocate(S455)
C
       allocate(D1(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,N0,M1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,N0+1:M1,N1+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder452136(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & M1,N2,N0,M1,N1,M2,N2,M2,N1,M2,N0,M1,t3B4,F2)
       allocate(S456(N2+1:M2,N1+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K9*K0
       I3=K9*K5*K8
       call DMATMAT(I1,I2,I3,D1,F2,S456)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N1,M2,N0,M1,X15,S456,-1.000)
       deallocate(S456)
C
       allocate(D1(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M1,N1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N1,N1+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder462135(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & N0,M1,M1,N1,N1,M2,N2,M2,N1,M2,N0,M1,t3B1,F2)
       allocate(S457(N2+1:M2,N1+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K9*K0
       I3=K9*K7*K5
       call DMATMAT(I1,I2,I3,D1,F2,S457)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N1,M2,N0,M1,X15,S457, 1.000)
       deallocate(S457)
C
       allocate(D1(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N2,M1,N1,N1,M2,N2,N3,VBHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N1,N1+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder462135(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N2,M1,N1,N1,M2,N2,M2,N1,M2,N0,M1,t3B1,F2)
       allocate(S458(N2+1:M2,N1+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K9*K0
       I3=K9*K7*K8
       call DMATMAT(I1,I2,I3,D1,F2,S458)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N1,M2,N0,M1,X15,S458, 1.000)
       deallocate(S458)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder612345(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,t3B1,F2)
       allocate(S459(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K9
       I3=K6*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,S459)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N1,M2,N0,M1,N0,M1,X14,S459,-1.000)
       deallocate(S459)
C
       allocate(D1(M1+1:N1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,N2,M2,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder612345(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,N2,M2,M2,N3,N1,M2,N0,M1,N0,M1,t3B1,F2)
       allocate(S460(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K9
       I3=K6*K0*K7
       call DMATMAT(I1,I2,I3,D1,F2,S460)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N1,M2,N0,M1,N0,M1,X14,S460,-1.000)
       deallocate(S460)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder612345(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,N1,M2,N0,M1,N0,M1,t3B1,F2)
       allocate(S461(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K9
       I3=K9*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,S461)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N1,M2,N0,M1,N0,M1,X14,S461,-1.000)
       deallocate(S461)
C
       allocate(D1(M1+1:N1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,N2,M2,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,N1+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder612345(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,N2,M2,N1,M2,N1,M2,N0,M1,N0,M1,t3B1,F2)
       allocate(S462(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K9
       I3=K9*K0*K7
       call DMATMAT(I1,I2,I3,D1,F2,S462)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N1,M2,N0,M1,N0,M1,X14,S462,-1.000)
       deallocate(S462)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S463(N2+1:M2,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K8*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S463)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,M2,N3,N2,M2,M1,N2,X17,S463, 1.000)
       deallocate(S463)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S464(N2+1:M2,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K8*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S464)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,N1,M2,N2,M2,M1,N2,X18,S464, 1.000)
       deallocate(S464)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S465(N2+1:M2,M1+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K8*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S465)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,M2,N3,N2,M2,M1,N2,X19,S465, 1.000)
       deallocate(S465)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S466(N2+1:M2,M1+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K8*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S466)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,N1,M2,N2,M2,M1,N2,X20,S466, 1.000)
       deallocate(S466)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder512346(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & N0,M1,M2,N3,M2,N3,N1,M2,M1,N2,N0,M1,t3B4,F2)
       allocate(S467(N1+1:M2,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K9
       I3=K6*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S467)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N1,M2,M1,N2,N0,M1,X13,S467, 1.000)
       deallocate(S467)
C
       allocate(D1(M1+1:N1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,M2,N3,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder612345(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,M2,N3,N1,M2,M1,N2,N0,M1,t3B1,F2)
       allocate(S468(N1+1:M2,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K9
       I3=K6*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,S468)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N1,M2,M1,N2,N0,M1,X13,S468,-1.000)
       deallocate(S468)
C
       allocate(D1(N0+1:M1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder512346(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & N0,M1,N2,M2,M2,N3,N1,M2,M1,N2,N0,M1,t3B4,F2)
       allocate(S469(N1+1:M2,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K9
       I3=K6*K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,S469)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N1,M2,M1,N2,N0,M1,X13,S469, 1.000)
       deallocate(S469)
C
       allocate(D1(M1+1:N1,N2+1:M2,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,N2,M2,M2,N3,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,M2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder612345(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,N2,M2,M2,N3,N1,M2,M1,N2,N0,M1,t3B1,F2)
       allocate(S470(N1+1:M2,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K9
       I3=K6*K0*K7
       call DMATMAT(I1,I2,I3,D1,F2,S470)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N1,M2,M1,N2,N0,M1,X13,S470,-1.000)
       deallocate(S470)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder512346(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & N0,M1,M2,N3,N1,M2,N1,M2,M1,N2,N0,M1,t3B4,F2)
       allocate(S471(N1+1:M2,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K9
       I3=K9*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S471)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N1,M2,M1,N2,N0,M1,X13,S471, 1.000)
       deallocate(S471)
C
       allocate(D1(M1+1:N1,M2+1:N3,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,M2,N3,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,M2+1:N3,N1+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder612345(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,N1,M2,N1,M2,M1,N2,N0,M1,t3B1,F2)
       allocate(S472(N1+1:M2,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K9
       I3=K9*K6*K7
       call DMATMAT(I1,I2,I3,D1,F2,S472)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N1,M2,M1,N2,N0,M1,X13,S472,-1.000)
       deallocate(S472)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,M1,N2,M2,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder512346(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & N0,M1,N2,M2,N1,M2,N1,M2,M1,N2,N0,M1,t3B4,F2)
       allocate(S473(N1+1:M2,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K9
       I3=K9*K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,S473)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N1,M2,M1,N2,N0,M1,X13,S473, 1.000)
       deallocate(S473)
C
       allocate(D1(M1+1:N1,N2+1:M2,N1+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & M1,N1,N2,M2,N1,M2,N0,N2,VBHHPP,D1)
       allocate(F2(M1+1:N1,N2+1:M2,N1+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder612345(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,N2,M2,N1,M2,N1,M2,M1,N2,N0,M1,t3B1,F2)
       allocate(S474(N1+1:M2,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K9
       I3=K9*K0*K7
       call DMATMAT(I1,I2,I3,D1,F2,S474)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N1,M2,M1,N2,N0,M1,X13,S474,-1.000)
       deallocate(S474)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S475(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S475)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,N2,M2,N0,M1,X21,S475, 1.000)
       deallocate(S475)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S476(N2+1:M2,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S476)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,M2,N3,N2,M2,N0,M1,X22,S476, 1.000)
       deallocate(S476)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S477(N2+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S477)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N1,M2,N2,M2,N0,M1,X23,S477, 1.000)
       deallocate(S477)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S478(N2+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K5*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S478)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,N1,M2,N2,M2,N0,M1,X24,S478, 1.000)
       deallocate(S478)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S479(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S479)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,N2,M2,N0,M1,X25,S479, 1.000)
       deallocate(S479)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,M2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S480(N2+1:M2,N0+1:M1,M1+1:N1,M2+1:N3))
       I1=K6*K7
       I2=K5*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S480)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,M2,N3,N2,M2,N0,M1,X26,S480, 1.000)
       deallocate(S480)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S481(N2+1:M2,N0+1:M1,N0+1:M1,N1+1:M2))
       I1=K9*K5
       I2=K5*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S481)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N1,M2,N2,M2,N0,M1,X27,S481, 1.000)
       deallocate(S481)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,N1,M2,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S482(N2+1:M2,N0+1:M1,M1+1:N1,N1+1:M2))
       I1=K9*K7
       I2=K5*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S482)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N1,N1,M2,N2,M2,N0,M1,X28,S482, 1.000)
       deallocate(S482)
C
       allocate(D1(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M1,N2,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder451236(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N2,M2,N3,N2,M2,N1,M2,N0,M1,t3C1,F2)
       allocate(S483(N2+1:M2,N1+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K9*K0
       I3=K6*K8*K5
       call DMATMAT(I1,I2,I3,D1,F2,S483)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N1,M2,N0,M1,X16,S483, 1.000)
       deallocate(S483)
C
       allocate(D1(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M1,N2,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder451236(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N2,M2,N3,N2,M2,N1,M2,N0,M1,t3C1,F2)
       allocate(S484(N2+1:M2,N1+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K9*K0
       I3=K6*K8*K8
       call DMATMAT(I1,I2,I3,D1,F2,S484)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N1,M2,N0,M1,X16,S484, 0.500)
       deallocate(S484)
C
       allocate(D1(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M1,N2,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder451236(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N2,N2,M2,N2,M2,N1,M2,N0,M1,t3C1,F2)
       allocate(S485(N2+1:M2,N1+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K9*K0
       I3=K0*K8*K5
       call DMATMAT(I1,I2,I3,D1,F2,S485)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N1,M2,N0,M1,X16,S485, 1.000)
       deallocate(S485)
C
       allocate(D1(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M1,N2,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder451236(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N2,N2,M2,N2,M2,N1,M2,N0,M1,t3C1,F2)
       allocate(S486(N2+1:M2,N1+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K9*K0
       I3=K0*K8*K8
       call DMATMAT(I1,I2,I3,D1,F2,S486)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N1,M2,N0,M1,X16,S486, 0.500)
       deallocate(S486)
C
       allocate(D1(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M1,N2,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder451236(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N2,M2,N3,N2,M2,N1,M2,N0,M1,t3C1,F2)
       allocate(S487(N2+1:M2,N1+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K9*K0
       I3=K6*K8*K5
       call DMATMAT(I1,I2,I3,D1,F2,S487)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N1,M2,N0,M1,X15,S487, 1.000)
       deallocate(S487)
C
       allocate(D1(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M1,N2,M2,N3,N2,N3,VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder451236(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N2,M2,N3,N2,M2,N1,M2,N0,M1,t3C1,F2)
       allocate(S488(N2+1:M2,N1+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K9*K0
       I3=K6*K8*K8
       call DMATMAT(I1,I2,I3,D1,F2,S488)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N1,M2,N0,M1,X15,S488, 0.500)
       deallocate(S488)
C
       allocate(D1(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M1,N2,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder451236(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N2,N2,M2,N2,M2,N1,M2,N0,M1,t3C1,F2)
       allocate(S489(N2+1:M2,N1+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K9*K0
       I3=K0*K8*K5
       call DMATMAT(I1,I2,I3,D1,F2,S489)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N1,M2,N0,M1,X15,S489, 1.000)
       deallocate(S489)
C
       allocate(D1(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:N3))
       call reorder3421(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M1,N2,N2,M2,N2,N3,VCHHPP,D1)
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder451236(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N2,N2,M2,N2,M2,N1,M2,N0,M1,t3C1,F2)
       allocate(S490(N2+1:M2,N1+1:M2,N0+1:M1,N2+1:N3))
       I1=K4
       I2=K5*K9*K0
       I3=K0*K8*K8
       call DMATMAT(I1,I2,I3,D1,F2,S490)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N2,N3,N2,M2,N1,M2,N0,M1,X15,S490, 0.500)
       deallocate(S490)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:M1,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,M1,M1,N2,VCHHPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:M1,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,M1,M1,N2,t2C,D2)
       allocate(S491(N0+1:M1,M1+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5
       I2=K8*K5
       I3=K4*K4
       call DMATMAT(I1,I2,I3,D1,D2,S491)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M1,N2,N0,M1,M1,N2,X73,S491, 0.500)
       deallocate(S491)
C
       allocate(D1(N2+1:N3,N2+1:N3,M1+1:N2,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,M1,N2,M1,N2,VCHHPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:M1,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,M1,M1,N2,t2C,D2)
       allocate(S492(N0+1:M1,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8
       I2=K8*K5
       I3=K4*K4
       call DMATMAT(I1,I2,I3,D1,D2,S492)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,M1,N2,N0,M1,M1,N2,X74,S492, 0.500)
       deallocate(S492)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M2,N3,M2,N3,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder512346(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,M2,N3,N1,M2,N0,M1,N0,M1,t3C3,F2)
       allocate(S493(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K9
       I3=K6*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,S493)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N1,M2,N0,M1,N0,M1,X14,S493, 0.500)
       deallocate(S493)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder512346(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N2,M2,N1,M2,N0,M1,N0,M1,t3C1,F2)
       allocate(S494(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K9
       I3=K0*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,S494)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N1,M2,N0,M1,N0,M1,X14,S494, 1.000)
       deallocate(S494)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,N2,M2,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder512346(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,N2,M2,N1,M2,N0,M1,N0,M1,t3C1,F2)
       allocate(S495(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K9
       I3=K0*K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,S495)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N1,M2,N0,M1,N0,M1,X14,S495, 0.500)
       deallocate(S495)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S496(N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S496)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,M2,N3,N2,M2,M1,N2,X75,S496,-1.000)
       deallocate(S496)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S497(N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K8*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S497)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,N2,M2,N2,M2,M1,N2,X76,S497,-1.000)
       deallocate(S497)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S498(N2+1:M2,M1+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K8*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S498)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,M2,N3,N2,M2,M1,N2,X77,S498,-1.000)
       deallocate(S498)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S499(N2+1:M2,M1+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K8*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S499)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,N2,M2,N2,M2,M1,N2,X78,S499,-1.000)
       deallocate(S499)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,M1,N2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,M1,N2,t2C,D2)
       allocate(Q47(M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8
       I3=K4*K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,Q47)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M1,N2,M1,N2,X33,Q47, 0.500)
       deallocate(Q47)
C
       allocate(D1(N0+1:M1,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M2,N3,M2,N3,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder412356(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,M2,N3,N1,M2,M1,N2,N0,M1,t3C3,F2)
       allocate(S500(N1+1:M2,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K9
       I3=K6*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S500)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N1,M2,M1,N2,N0,M1,X13,S500,-0.500)
       deallocate(S500)
C
       allocate(D1(M1+1:N2,M2+1:N3,M2+1:N3,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M2,N3,M2,N3,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,M2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder412356(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,M2,N3,N1,M2,M1,N2,N0,M1,t3C3,F2)
       allocate(S501(N1+1:M2,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K9
       I3=K6*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,S501)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N1,M2,M1,N2,N0,M1,X13,S501,-0.500)
       deallocate(S501)
C
       allocate(D1(N0+1:M1,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N2,M2,N1,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(S502(N1+1:M2,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K9
       I3=K0*K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,S502)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N1,M2,M1,N2,N0,M1,X13,S502,-1.000)
       deallocate(S502)
C
       allocate(D1(M1+1:N2,M2+1:N3,N2+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,M2,N3,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N2,M2,N1,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(S503(N1+1:M2,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K9
       I3=K0*K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,S503)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N1,M2,M1,N2,N0,M1,X13,S503,-1.000)
       deallocate(S503)
C
       allocate(D1(N0+1:M1,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,M1,N2,M2,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N2,M2,N1,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(S504(N1+1:M2,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K9
       I3=K0*K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,S504)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N1,M2,M1,N2,N0,M1,X13,S504,-0.500)
       deallocate(S504)
C
       allocate(D1(M1+1:N2,N2+1:M2,N2+1:M2,N0+1:N2))
       call reorder4123(N2,N3,N2,N3,N0,N2,N0,N2,
     & M1,N2,N2,M2,N2,M2,N0,N2,VCHHPP,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,N2,M2,N1,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(S505(N1+1:M2,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K9
       I3=K0*K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,S505)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum2341(N0,N2,N1,M2,M1,N2,N0,M1,X13,S505,-0.500)
       deallocate(S505)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S506(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S506)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,N2,M2,N0,M1,X79,S506,-1.000)
       deallocate(S506)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S507(N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S507)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,M2,N3,N2,M2,N0,M1,X80,S507,-1.000)
       deallocate(S507)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S508(N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S508)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N2,M2,N2,M2,N0,M1,X81,S508,-1.000)
       deallocate(S508)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S509(N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S509)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,N2,M2,N2,M2,N0,M1,X82,S509,-1.000)
       deallocate(S509)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,M2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S510(N2+1:M2,N0+1:M1,N0+1:M1,M2+1:N3))
       I1=K6*K5
       I2=K5*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S510)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,M2,N3,N2,M2,N0,M1,X83,S510,-1.000)
       deallocate(S510)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,M2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S511(N2+1:M2,N0+1:M1,M1+1:N2,M2+1:N3))
       I1=K6*K8
       I2=K5*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S511)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,M2,N3,N2,M2,N0,M1,X84,S511,-1.000)
       deallocate(S511)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S512(N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2))
       I1=K0*K5
       I2=K5*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S512)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N0,M1,N2,M2,N2,M2,N0,M1,X85,S512,-1.000)
       deallocate(S512)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S513(N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2))
       I1=K0*K8
       I2=K5*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S513)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M1,N2,N2,M2,N2,M2,N0,M1,X86,S513,-1.000)
       deallocate(S513)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,M1,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,M1,t2C,D2)
       allocate(Q48(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K4*K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,Q48)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N0,M1,N0,M1,X34,Q48, 0.500)
       deallocate(Q48)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:N3,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,M1,N2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,N3,N0,M1,t2C,D2)
       allocate(Q49(N0+1:M1,M1+1:N2))
       I1=K8
       I2=K5
       I3=K4*K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,Q49)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M1,N2,N0,M1,X35,Q49, 0.500)
       deallocate(Q49)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,M2,N2,M2,t2C,D2)
       allocate(S514(N2+1:M2,N2+1:M2,M2+1:N3,M2+1:N3))
       I1=K6*K6
       I2=K0*K0
       I3=K2*K2
       call DMATMAT(I1,I2,I3,D1,D2,S514)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M2,N3,M2,N3,N2,M2,N2,M2,X87,S514, 0.500)
       deallocate(S514)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,M2,N2,M2,t2C,D2)
       allocate(S515(N2+1:M2,N2+1:M2,M2+1:N3,N2+1:M2))
       I1=K0*K6
       I2=K0*K0
       I3=K2*K2
       call DMATMAT(I1,I2,I3,D1,D2,S515)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(M2,N3,N2,M2,N2,M2,N2,M2,X88,S515, 0.500)
       deallocate(S515)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,M2,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,M2,N2,M2,t2C,D2)
       allocate(S516(N2+1:M2,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0
       I2=K0*K0
       I3=K2*K2
       call DMATMAT(I1,I2,I3,D1,D2,S516)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum3412(N2,M2,N2,M2,N2,M2,N2,M2,X89,S516, 0.500)
       deallocate(S516)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,M2,t2C,D2)
       allocate(Q50(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K4*K2*K2
       call DMATMAT(I1,I2,I3,D1,D2,Q50)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M2,N3,N2,M2,X36,Q50,-0.500)
       deallocate(Q50)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,M2,t2C,D2)
       allocate(Q51(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K4*K2*K2
       call DMATMAT(I1,I2,I3,D1,D2,Q51)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N2,M2,N2,M2,X37,Q51,-0.500)
       deallocate(Q51)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M2+1:N3))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,M2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,M2,t2C,D2)
       allocate(Q52(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K4*K2*K2
       call DMATMAT(I1,I2,I3,D1,D2,Q52)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(M2,N3,N2,M2,X38,Q52,-0.500)
       deallocate(Q52)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,M2,VCHHPP,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N2,M2,t2C,D2)
       allocate(Q53(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K4*K2*K2
       call DMATMAT(I1,I2,I3,D1,D2,Q53)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum21(N2,M2,N2,M2,X39,Q53,-0.500)
       deallocate(Q53)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,M1+1:N2))
       call reorder2341(N0,N2,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,N2,N0,N1,M1,N2,VBHHPH,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S517(N0+1:M1,N0+1:N2,N0+1:N1,M1+1:N2))
       I1=K8*K1*K2
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S517)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N0,N1,M1,N2,
     & N0,N2,N0,N1,M1,N2,N0,M1,S517,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S539(N2+1:M2,N0+1:N1,M1+1:N2,N0+1:M1))
       I1=K5*K8*K1
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S539)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N2,M2,M1,N2,N0,M1,X1,S539,-1.000)
       deallocate(S539)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N0,N1,M1,N2,
     & N0,N2,N0,N1,M1,N2,N0,M1,S517,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S540(N2+1:M2,N0+1:N1,M1+1:N2,N0+1:M1))
       I1=K5*K8*K1
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S540)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N2,M2,M1,N2,N0,M1,X2,S540,-1.000)
       deallocate(S540)
C
       allocate(D1(N0+1:N1,N0+1:N2,M1+1:N2,N0+1:M1))
       call reorder3241(N0,M1,N0,N2,N0,N1,M1,N2,
     & N0,N1,N0,N2,M1,N2,N0,M1,S517,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S518(N1+1:M2,N0+1:N2,M1+1:N2,N0+1:M1))
       I1=K5*K8*K2
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S518)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,N1,M2,M1,N2,N0,M1,X13,S518,-1.000)
       deallocate(S518)
C
       allocate(D1(N1+1:N3,N0+1:N1,N2+1:N3,N2+1:M2))
       call reorder2413(N2,N3,N1,N3,N2,N3,N0,N1,
     & N1,N3,N0,N1,N2,N3,N2,M2,VBHPPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S521(N0+1:M1,N0+1:N1,N2+1:N3,N2+1:M2))
       I1=K0*K4*K1
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S521)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:M2,N0+1:M1))
       call reorder3241(N0,M1,N0,N1,N2,N3,N2,M2,
     & N2,N3,N0,N1,N2,M2,N0,M1,S521,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S541(M1+1:N2,N0+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K1
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S541)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N1,N2,M2,M1,N2,N0,M1,X1,S541, 1.000)
       deallocate(S541)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:M2,N0+1:M1))
       call reorder3241(N0,M1,N0,N1,N2,N3,N2,M2,
     & N2,N3,N0,N1,N2,M2,N0,M1,S521,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S542(M1+1:N2,N0+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K1
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S542)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N1,N2,M2,M1,N2,N0,M1,X2,S542, 1.000)
       deallocate(S542)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:M2,N0+1:M1))
       call reorder3241(N0,M1,N0,N1,N2,N3,N2,M2,
     & N2,N3,N0,N1,N2,M2,N0,M1,S521,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S545(N0+1:M1,N0+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K1
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S545)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N1,N2,M2,N0,M1,N0,M1,X3,S545, 1.000)
       deallocate(S545)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:M2,N0+1:M1))
       call reorder3241(N0,M1,N0,N1,N2,N3,N2,M2,
     & N2,N3,N0,N1,N2,M2,N0,M1,S521,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S546(N0+1:M1,N0+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K1
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S546)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N1,N2,M2,N0,M1,N0,M1,X4,S546, 1.000)
       deallocate(S546)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N1,N2,N3,N2,M2,
     & N0,N1,N2,N3,N2,M2,N0,M1,S521,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S522(N1+1:M2,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S522)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,N3,N2,M2,N1,M2,N0,M1,X15,S522,-1.000)
C
       call
     & sum3124(N2,N3,N2,M2,N1,M2,N0,M1,X16,S522,-1.000)
       deallocate(S522)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N1,N1+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,M1,N1,N1,N3,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q56(M1+1:N1,N1+1:N3))
       I1=K3*K7
       I3=K3*K1
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
     & sum21(M1,N1,N0,M1,X30,Q57, 1.000)
       deallocate(Q57)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,M2+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,M2,N3,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q58(N0+1:N1,M2+1:N3))
       I1=K6*K1
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q58)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(Q59(N1+1:M2,M2+1:N3))
       I1=K6
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,Q58,B2,Q59)
       deallocate(B2)
C
       call
     & sum21(M2,N3,N1,M2,X31,Q59,-1.000)
       deallocate(Q59)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:M2))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N1,M2,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q60(N0+1:N1,N1+1:M2))
       I1=K9*K1
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q60)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(Q61(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,Q60,B2,Q61)
       deallocate(B2)
C
       call
     & sum21(N1,M2,N1,M2,X32,Q61,-1.000)
       deallocate(Q61)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:N1,M2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,N0,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S531(N0+1:M1,N0+1:M1,N0+1:N1,M2+1:N3))
       I1=K6*K1*K5
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S531)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder3241(N0,M1,N0,M1,N0,N1,M2,N3,
     & N0,N1,N0,M1,M2,N3,N0,M1,S531,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S532(N1+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S532)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,N1,M2,N0,M1,X115,S532,-1.000)
       deallocate(S532)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z319(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K0
       I3=K6*K5
       call DMATMAT(I1,I2,I3,X115,F2,Z319)
       deallocate(F2)
C
       call
     & sum124536(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z319, 1.000)
       deallocate(Z319)
       deallocate(X115)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:N1,M2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,N0,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S533(N0+1:M1,M1+1:N2,N0+1:N1,M2+1:N3))
       I1=K6*K1*K8
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S533)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder3241(N0,M1,M1,N2,N0,N1,M2,N3,
     & N0,N1,M1,N2,M2,N3,N0,M1,S533,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S534(N1+1:M2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S534)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M2,N3,N1,M2,N0,M1,X116,S534,-1.000)
       deallocate(S534)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder512346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z320(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K0
       I3=K6*K8
       call DMATMAT(I1,I2,I3,X116,F2,Z320)
       deallocate(F2)
C
       call
     & sum124536(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z320,-1.000)
       deallocate(Z320)
       deallocate(X116)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:N1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,N0,N1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S535(N0+1:M1,N0+1:M1,N0+1:N1,N2+1:M2))
       I1=K0*K1*K5
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S535)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder3241(N0,M1,N0,M1,N0,N1,N2,M2,
     & N0,N1,N0,M1,N2,M2,N0,M1,S535,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S536(N1+1:M2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S536)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N2,M2,N1,M2,N0,M1,X117,S536,-1.000)
       deallocate(S536)
C
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z321(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K0
       I3=K0*K5
       call DMATMAT(I1,I2,I3,X117,F2,Z321)
       deallocate(F2)
C
       call
     & sum124536(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z321, 1.000)
       deallocate(Z321)
       deallocate(X117)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:N1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,N0,N1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S537(N0+1:M1,M1+1:N2,N0+1:N1,N2+1:M2))
       I1=K0*K1*K8
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S537)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder3241(N0,M1,M1,N2,N0,N1,N2,M2,
     & N0,N1,M1,N2,N2,M2,N0,M1,S537,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S538(N1+1:M2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S538)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,N2,M2,N1,M2,N0,M1,X118,S538,-1.000)
       deallocate(S538)
C
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder512346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z322(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K0
       I3=K0*K8
       call DMATMAT(I1,I2,I3,X118,F2,Z322)
       deallocate(F2)
C
       call
     & sum124536(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z322,-1.000)
       deallocate(Z322)
       deallocate(X118)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,N0+1:M1))
       call reorder2341(N0,N2,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,N2,N0,N1,N0,M1,VBHHPH,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S519(N0+1:M1,N0+1:N2,N0+1:N1,N0+1:M1))
       I1=K5*K1*K2
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S519)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:M1,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N0,N1,N0,M1,
     & N0,N2,N0,N1,N0,M1,N0,M1,S519,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S543(N2+1:M2,N0+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K1
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S543)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N2,M2,N0,M1,N0,M1,X3,S543,-1.000)
       deallocate(S543)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:M1,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N0,N1,N0,M1,
     & N0,N2,N0,N1,N0,M1,N0,M1,S519,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S544(N2+1:M2,N0+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K1
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S544)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N2,M2,N0,M1,N0,M1,X4,S544,-1.000)
       deallocate(S544)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:M1,N0+1:M1))
       call reorder3241(N0,M1,N0,N2,N0,N1,N0,M1,
     & N0,N1,N0,N2,N0,M1,N0,M1,S519,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S520(N1+1:M2,N0+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K2
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S520)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,N1,M2,N0,M1,N0,M1,X14,S520,-1.000)
       deallocate(S520)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:N1,N1+1:M2))
       call reorder2341(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,M1,N0,N1,N1,M2,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S527(N0+1:M1,N0+1:M1,N0+1:N1,N1+1:M2))
       I1=K9*K1*K5
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S527)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder3241(N0,M1,N0,M1,N0,N1,N1,M2,
     & N0,N1,N0,M1,N1,M2,N0,M1,S527,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S528(N1+1:M2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S528)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N1,M2,N1,M2,N0,M1,X113,S528,-1.000)
       deallocate(S528)
C
       allocate(F2(N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N1,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z313(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K0
       I3=K9*K5
       call DMATMAT(I1,I2,I3,X113,F2,Z313)
       deallocate(F2)
C
       call
     & sum124536(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z313, 1.000)
       deallocate(Z313)
       deallocate(X113)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:N1,M2+1:N3))
       call reorder2341(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,M1,N0,N1,M2,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S523(N0+1:M1,N0+1:M1,N0+1:N1,M2+1:N3))
       I1=K6*K1*K5
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S523)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder3241(N0,M1,N0,M1,N0,N1,M2,N3,
     & N0,N1,N0,M1,M2,N3,N0,M1,S523,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S524(N1+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S524)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,N1,M2,N0,M1,X111,S524,-1.000)
       deallocate(S524)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z311(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K0
       I3=K6*K5
       call DMATMAT(I1,I2,I3,X111,F2,Z311)
       deallocate(F2)
C
       call
     & sum124536(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z311, 1.000)
       deallocate(Z311)
       deallocate(X111)
C
       allocate(D1(N1+1:N3,M1+1:N1,N0+1:N1,M2+1:N3))
       call reorder2341(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,M1,N1,N0,N1,M2,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S525(N0+1:M1,M1+1:N1,N0+1:N1,M2+1:N3))
       I1=K6*K1*K7
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S525)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder3241(N0,M1,M1,N1,N0,N1,M2,N3,
     & N0,N1,M1,N1,M2,N3,N0,M1,S525,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S526(N1+1:M2,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S526)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,M2,N3,N1,M2,N0,M1,X112,S526,-1.000)
       deallocate(S526)
C
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z312(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K0
       I3=K6*K7
       call DMATMAT(I1,I2,I3,X112,F2,Z312)
       deallocate(F2)
C
       call
     & sum124536(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z312, 1.000)
       deallocate(Z312)
       deallocate(X112)
C
       allocate(D1(N1+1:N3,M1+1:N1,N0+1:N1,N1+1:M2))
       call reorder2341(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,M1,N1,N0,N1,N1,M2,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S529(N0+1:M1,M1+1:N1,N0+1:N1,N1+1:M2))
       I1=K9*K1*K7
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S529)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder3241(N0,M1,M1,N1,N0,N1,N1,M2,
     & N0,N1,M1,N1,N1,M2,N0,M1,S529,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S530(N1+1:M2,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S530)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,N1,M2,N1,M2,N0,M1,X114,S530,-1.000)
       deallocate(S530)
C
       allocate(F2(M1+1:N1,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder631245(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N1,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z314(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K0
       I3=K9*K7
       call DMATMAT(I1,I2,I3,X114,F2,Z314)
       deallocate(F2)
C
       call
     & sum124536(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z314, 1.000)
       deallocate(Z314)
       deallocate(X114)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N1+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,M1,N1,N3,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q54(N0+1:M1,N1+1:N3))
       I1=K3*K5
       I3=K3*K1
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
     & sum21(N0,M1,N0,M1,X29,Q55, 1.000)
       deallocate(Q55)
C
       allocate(D1(N0+1:N1,N0+1:N2,N1+1:N3,M1+1:N2))
       call reorder4321(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N0,N2,N1,N3,M1,N2,VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S547(N1+1:M2,N0+1:N2,N1+1:N3,M1+1:N2))
       I1=K8*K3*K2
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S547)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,M1+1:N2))
       call reorder2314(N1,M2,N0,N2,N1,N3,M1,N2,
     & N0,N2,N1,N3,N1,M2,M1,N2,S547,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S549(N2+1:M2,N1+1:N3,N1+1:M2,M1+1:N2))
       I1=K8*K9*K3
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S549)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,N2,M2,N1,M2,M1,N2,X6,S549, 1.000)
       deallocate(S549)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,M1+1:N2))
       call reorder2314(N1,M2,N0,N2,N1,N3,M1,N2,
     & N0,N2,N1,N3,N1,M2,M1,N2,S547,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S548(N2+1:M2,N1+1:N3,N1+1:M2,M1+1:N2))
       I1=K8*K9*K3
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S548)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,N2,M2,N1,M2,M1,N2,X5,S548, 1.000)
       deallocate(S548)
C
       allocate(D1(N2+1:N3,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,N1,N1,N3,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S550(M1+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       I1=K0*K3*K1
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S550)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N1,N1,N3,N2,M2,
     & N0,N1,N1,N3,N2,M2,M1,N2,S550,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S551(N1+1:M2,N1+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K3
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S551)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,N3,N2,M2,N1,M2,M1,N2,X5,S551,-1.000)
       deallocate(S551)
C
       allocate(D1(N2+1:N3,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,N1,N1,N3,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S552(M1+1:N2,N0+1:N1,N1+1:N3,N2+1:M2))
       I1=K0*K3*K1
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S552)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N1,N1,N3,N2,M2,
     & N0,N1,N1,N3,N2,M2,M1,N2,S552,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S553(N1+1:M2,N1+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K3
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S553)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,N3,N2,M2,N1,M2,M1,N2,X6,S553,-1.000)
       deallocate(S553)
C
       allocate(D1(N0+1:N1,N0+1:N2,N1+1:N3,N0+1:M1))
       call reorder4321(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N1,N0,N2,N1,N3,N0,M1,VBHHPH,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S554(N1+1:M2,N0+1:N2,N1+1:N3,N0+1:M1))
       I1=K5*K3*K2
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S554)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder2314(N1,M2,N0,N2,N1,N3,N0,M1,
     & N0,N2,N1,N3,N1,M2,N0,M1,S554,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S556(N2+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S556)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,N2,M2,N1,M2,N0,M1,X8,S556, 1.000)
       deallocate(S556)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder2314(N1,M2,N0,N2,N1,N3,N0,M1,
     & N0,N2,N1,N3,N1,M2,N0,M1,S554,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S555(N2+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S555)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,N2,M2,N1,M2,N0,M1,X7,S555, 1.000)
       deallocate(S555)
C
       allocate(D1(N2+1:N3,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,N1,N1,N3,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S557(N0+1:M1,N0+1:N1,N1+1:N3,N2+1:M2))
       I1=K0*K3*K1
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S557)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N1,N1,N3,N2,M2,
     & N0,N1,N1,N3,N2,M2,N0,M1,S557,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S558(N1+1:M2,N1+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K3
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S558)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,N3,N2,M2,N1,M2,N0,M1,X7,S558,-1.000)
       deallocate(S558)
C
       allocate(D1(N2+1:N3,N0+1:N1,N1+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N0,N1,N1,N3,N2,M2,VBHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S559(N0+1:M1,N0+1:N1,N1+1:N3,N2+1:M2))
       I1=K0*K3*K1
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S559)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N1,N1,N3,N2,M2,
     & N0,N1,N1,N3,N2,M2,N0,M1,S559,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S560(N1+1:M2,N1+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K3
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S560)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,N3,N2,M2,N1,M2,N0,M1,X8,S560,-1.000)
       deallocate(S560)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:M1))
       call reorder1342(N2,N3,N0,N1,N0,N2,N0,N1,
     & N2,N3,N0,N2,N0,N1,N0,M1,VBHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S561(M1+1:N2,N0+1:N2,N0+1:N1,N0+1:M1))
       I1=K5*K1*K2
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S561)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,N0+1:M1))
       call reorder2314(M1,N2,N0,N2,N0,N1,N0,M1,
     & N0,N2,N0,N1,M1,N2,N0,M1,S561,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S666(N2+1:M2,N0+1:N1,M1+1:N2,N0+1:M1))
       I1=K5*K8*K1
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S666)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N2,M2,M1,N2,N0,M1,X1,S666,-1.000)
       deallocate(S666)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,N0+1:M1))
       call reorder2314(M1,N2,N0,N2,N0,N1,N0,M1,
     & N0,N2,N0,N1,M1,N2,N0,M1,S561,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S667(N2+1:M2,N0+1:N1,M1+1:N2,N0+1:M1))
       I1=K5*K8*K1
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S667)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N2,M2,M1,N2,N0,M1,X2,S667,-1.000)
       deallocate(S667)
C
       allocate(D1(N0+1:N1,N0+1:N2,M1+1:N2,N0+1:M1))
       call reorder3214(M1,N2,N0,N2,N0,N1,N0,M1,
     & N0,N1,N0,N2,M1,N2,N0,M1,S561,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S562(N1+1:M2,N0+1:N2,M1+1:N2,N0+1:M1))
       I1=K5*K8*K2
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S562)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,N1,M2,M1,N2,N0,M1,X13,S562,-1.000)
       deallocate(S562)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:N3,N0+1:M1))
       call reorder4312(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N0,N2,N2,N3,N0,M1,VBHHHP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S565(N1+1:M2,N0+1:N2,N2+1:N3,N0+1:M1))
       I1=K5*K4*K2
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S565)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder2314(N1,M2,N0,N2,N2,N3,N0,M1,
     & N0,N2,N2,N3,N1,M2,N0,M1,S565,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S566(N2+1:M2,N2+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K4
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S566)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,N1,M2,N0,M1,X15,S566, 1.000)
       deallocate(S566)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder2314(N1,M2,N0,N2,N2,N3,N0,M1,
     & N0,N2,N2,N3,N1,M2,N0,M1,S565,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S567(N2+1:M2,N2+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K4
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S567)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,N1,M2,N0,M1,X16,S567, 1.000)
       deallocate(S567)
C
       allocate(D1(N1+1:N3,N0+1:N2,N2+1:N3,N1+1:M2))
       call reorder2314(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,N0,N2,N2,N3,N1,M2,VBPHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S568(N0+1:M1,N0+1:N2,N2+1:N3,N1+1:M2))
       I1=K9*K4*K2
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S568)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N2,N3,N1,M2,
     & N0,N2,N2,N3,N1,M2,N0,M1,S568,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S571(N2+1:M2,N2+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K4
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S571)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,N1,M2,N0,M1,X15,S571,-1.000)
       deallocate(S571)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N2,N3,N1,M2,
     & N0,N2,N2,N3,N1,M2,N0,M1,S568,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S572(N2+1:M2,N2+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K4
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S572)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,N1,M2,N0,M1,X16,S572,-1.000)
       deallocate(S572)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:M2,N0+1:M1))
       call reorder3241(N0,M1,N0,N2,N2,N3,N1,M2,
     & N2,N3,N0,N2,N1,M2,N0,M1,S568,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S569(M1+1:N2,N0+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9*K2
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S569)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N2,N1,M2,M1,N2,N0,M1,X13,S569, 1.000)
       deallocate(S569)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:M2,N0+1:M1))
       call reorder3241(N0,M1,N0,N2,N2,N3,N1,M2,
     & N2,N3,N0,N2,N1,M2,N0,M1,S568,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S570(N0+1:M1,N0+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9*K2
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S570)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N2,N1,M2,N0,M1,N0,M1,X14,S570, 1.000)
       deallocate(S570)
C
       allocate(D1(N1+1:N3,N0+1:M1,M1+1:N1,N2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,M1,N1,N2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S575(N0+1:M1,N0+1:M1,M1+1:N1,N2+1:N3))
       I1=K4*K7*K5
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S575)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N1,N0+1:M1))
       call reorder4231(N0,M1,N0,M1,M1,N1,N2,N3,
     & N2,N3,N0,M1,M1,N1,N0,M1,S575,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S582(N0+1:M1,N0+1:M1,M1+1:N1,N0+1:M1))
       I1=K5*K7*K5
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S582)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M1,N1,N0,M1,N0,M1,X49,S582, 1.000)
       deallocate(S582)
C
       call sumx3412(N0,N2,N0,N1,N0,N2,N0,N1,
     & N0,M1,M1,N1,N0,M1,N0,M1,X49,VBHHHH, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N1,N2,M2,N2,M2,N1,M2,M1,N2,t3C1,F2)
       allocate(Z49(N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K8*K9*K0*K0
       I3=K7*K5
       call DMATMAT(I1,I2,I3,X49,F2,Z49)
       deallocate(F2)
C
       call
     & sum123546(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z49, 1.000)
       deallocate(Z49)
       deallocate(X49)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N1,N0+1:M1))
       call reorder4231(N0,M1,N0,M1,M1,N1,N2,N3,
     & N2,N3,N0,M1,M1,N1,N0,M1,S575,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S576(M1+1:N2,N0+1:M1,M1+1:N1,N0+1:M1))
       I1=K5*K7*K5
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S576)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M1,N1,M1,N2,N0,M1,X45,S576, 1.000)
       deallocate(S576)
C
       call sumx3412(N0,N2,N0,N1,N0,N2,N0,N1,
     & N0,M1,M1,N1,M1,N2,N0,M1,X45,VBHHHH, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,M1,N0,M1,M1,N1,
     & N0,M1,M1,N1,N2,M2,N2,M2,N1,M2,N0,M1,t3C4,F2)
       allocate(Z45(N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       I1=K5*K8
       I2=K5*K9*K0*K0
       I3=K7*K5
       call DMATMAT(I1,I2,I3,X45,F2,Z45)
       deallocate(F2)
C
       V3C=V3C-Z45
       deallocate(Z45)
       deallocate(X45)
C
       allocate(D1(N1+1:N3,N0+1:M1,N0+1:M1,N2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,M1,N0,M1,N2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S579(N0+1:M1,N0+1:M1,N0+1:M1,N2+1:N3))
       I1=K4*K5*K5
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S579)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:M1,N0+1:M1))
       call reorder4231(N0,M1,N0,M1,N0,M1,N2,N3,
     & N2,N3,N0,M1,N0,M1,N0,M1,S579,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S580(N0+1:M1,N0+1:M1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K5
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S580)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N0,M1,N0,M1,N0,M1,X47,S580, 1.000)
       deallocate(S580)
C
       call sumx3412(N0,N2,N0,N1,N0,N2,N0,N1,
     & N0,M1,N0,M1,N0,M1,N0,M1,X47,VBHHHH, 1.000)
C
       allocate(F2(N0+1:M1,N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N0,M1,N2,M2,N2,M2,N1,M2,M1,N2,t3C1,F2)
       allocate(Z47(N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K8*K9*K0*K0
       I3=K5*K5
       call DMATMAT(I1,I2,I3,X47,F2,Z47)
       deallocate(F2)
C
       call
     & sum123546(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z47, 1.000)
       deallocate(Z47)
       deallocate(X47)
C
       allocate(D1(N1+1:N3,M1+1:N2,N0+1:M1,N2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,N0,M1,N2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S573(N0+1:M1,M1+1:N2,N0+1:M1,N2+1:N3))
       I1=K4*K5*K8
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S573)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:M1,N0+1:M1))
       call reorder4231(N0,M1,M1,N2,N0,M1,N2,N3,
     & N2,N3,M1,N2,N0,M1,N0,M1,S573,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S574(M1+1:N2,M1+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K8
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S574)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,N0,M1,M1,N2,N0,M1,X44,S574, 1.000)
       deallocate(S574)
C
       call sumx3412(N0,N2,N0,N1,N0,N2,N0,N1,
     & M1,N2,N0,M1,M1,N2,N0,M1,X44,VBHHHH, 1.000)
C
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder561234(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,N2,M2,N1,M2,N0,M1,t3C1,F2)
       allocate(Z44(N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       I1=K5*K8
       I2=K5*K9*K0*K0
       I3=K5*K8
       call DMATMAT(I1,I2,I3,X44,F2,Z44)
       deallocate(F2)
C
       V3C=V3C+Z44
       deallocate(Z44)
       deallocate(X44)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:M1,N0+1:M1))
       call reorder4231(N0,M1,M1,N2,N0,M1,N2,N3,
     & N2,N3,M1,N2,N0,M1,N0,M1,S573,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S581(N0+1:M1,M1+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K8
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S581)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,N0,M1,N0,M1,N0,M1,X48,S581, 1.000)
       deallocate(S581)
C
       call sumx3412(N0,N2,N0,N1,N0,N2,N0,N1,
     & M1,N2,N0,M1,N0,M1,N0,M1,X48,VBHHHH, 1.000)
C
       allocate(F2(M1+1:N2,N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N0,M1,N2,M2,N2,M2,N1,M2,M1,N2,t3C1,F2)
       allocate(Z48(N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K8*K9*K0*K0
       I3=K5*K8
       call DMATMAT(I1,I2,I3,X48,F2,Z48)
       deallocate(F2)
C
       call
     & sum123546(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z48, 1.000)
       deallocate(Z48)
       deallocate(X48)
C
       allocate(D1(N1+1:N3,M1+1:N2,M1+1:N1,N2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,M1,N2,M1,N1,N2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S577(N0+1:M1,M1+1:N2,M1+1:N1,N2+1:N3))
       I1=K4*K7*K8
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S577)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N1,N0+1:M1))
       call reorder4231(N0,M1,M1,N2,M1,N1,N2,N3,
     & N2,N3,M1,N2,M1,N1,N0,M1,S577,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S578(M1+1:N2,M1+1:N2,M1+1:N1,N0+1:M1))
       I1=K5*K7*K8
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S578)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M1,N1,M1,N2,N0,M1,X46,S578, 1.000)
       deallocate(S578)
C
       call sumx3412(N0,N2,N0,N1,N0,N2,N0,N1,
     & M1,N2,M1,N1,M1,N2,N0,M1,X46,VBHHHH, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder561234(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,N2,M2,N1,M2,N0,M1,t3C1,F2)
       allocate(Z46(N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       I1=K5*K8
       I2=K5*K9*K0*K0
       I3=K7*K8
       call DMATMAT(I1,I2,I3,X46,F2,Z46)
       deallocate(F2)
C
       V3C=V3C+Z46
       deallocate(Z46)
       deallocate(X46)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N1,N0+1:M1))
       call reorder4231(N0,M1,M1,N2,M1,N1,N2,N3,
     & N2,N3,M1,N2,M1,N1,N0,M1,S577,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S583(N0+1:M1,M1+1:N2,M1+1:N1,N0+1:M1))
       I1=K5*K7*K8
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S583)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M1,N1,N0,M1,N0,M1,X50,S583, 1.000)
       deallocate(S583)
C
       call sumx3412(N0,N2,N0,N1,N0,N2,N0,N1,
     & M1,N2,M1,N1,N0,M1,N0,M1,X50,VBHHHH, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder461235(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N1,N2,M2,N2,M2,N1,M2,M1,N2,t3C1,F2)
       allocate(Z50(N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5
       I2=K8*K9*K0*K0
       I3=K7*K8
       call DMATMAT(I1,I2,I3,X50,F2,Z50)
       deallocate(F2)
C
       call
     & sum123546(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z50, 1.000)
       deallocate(Z50)
       deallocate(X50)
C
       allocate(D1(N1+1:N3,N0+1:N2,M1+1:N1,M2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,N2,M1,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S586(N0+1:M1,N0+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7*K2
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S586)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,M1,N1,M2,N3,
     & N0,N2,M1,N1,M2,N3,N0,M1,S586,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S593(N2+1:M2,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S593)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,M2,N3,N2,M2,N0,M1,X95,S593,-1.000)
       deallocate(S593)
C
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2))
       call reorder612345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,N2,M2,N1,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z138(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K9*K0
       I3=K6*K7
       call DMATMAT(I1,I2,I3,X95,F2,Z138)
       deallocate(F2)
C
       call
     & sum134526(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z138, 1.000)
       deallocate(Z138)
       deallocate(X95)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,M1,N1,M2,N3,
     & N0,N2,M1,N1,M2,N3,N0,M1,S586,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S587(N2+1:M2,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S587)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,M2,N3,N2,M2,N0,M1,X94,S587,-1.000)
       deallocate(S587)
C
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2))
       call reorder612345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,M2,N3,N2,M2,N1,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z134(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K9*K0
       I3=K6*K7
       call DMATMAT(I1,I2,I3,X94,F2,Z134)
       deallocate(F2)
C
       call
     & sum234516(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z134,-1.000)
       deallocate(Z134)
       deallocate(X94)
C
       allocate(D1(N1+1:N3,N0+1:N2,M1+1:N1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,N2,M1,N1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S590(N0+1:M1,N0+1:N2,M1+1:N1,N2+1:M2))
       I1=K0*K7*K2
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S590)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N1,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,M1,N1,N2,M2,
     & N0,N2,M1,N1,N2,M2,N0,M1,S590,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S595(N2+1:M2,M1+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K7
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S595)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,N2,M2,N2,M2,N0,M1,X97,S595,-1.000)
       deallocate(S595)
C
       allocate(F2(M1+1:N1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2))
       call reorder612345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z140(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K9*K0
       I3=K0*K7
       call DMATMAT(I1,I2,I3,X97,F2,Z140)
       deallocate(F2)
C
       call
     & sum134526(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z140, 1.000)
       deallocate(Z140)
       deallocate(X97)
C
       allocate(D1(N0+1:N2,M1+1:N1,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,M1,N1,N2,M2,
     & N0,N2,M1,N1,N2,M2,N0,M1,S590,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S591(N2+1:M2,M1+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K7
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S591)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,N2,M2,N2,M2,N0,M1,X96,S591,-1.000)
       deallocate(S591)
C
       allocate(F2(M1+1:N1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2))
       call reorder612345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z136(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K9*K0
       I3=K0*K7
       call DMATMAT(I1,I2,I3,X96,F2,Z136)
       deallocate(F2)
C
       call
     & sum234516(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z136,-1.000)
       deallocate(Z136)
       deallocate(X96)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:M1,N2+1:M2))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,N2,N0,M1,N2,M2,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S588(N0+1:M1,N0+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5*K2
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S588)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N0,M1,N2,M2,
     & N0,N2,N0,M1,N2,M2,N0,M1,S588,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S594(N2+1:M2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S594)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N2,M2,N2,M2,N0,M1,X93,S594,-1.000)
       deallocate(S594)
C
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2))
       call reorder612345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z139(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K9*K0
       I3=K0*K5
       call DMATMAT(I1,I2,I3,X93,F2,Z139)
       deallocate(F2)
C
       call
     & sum134526(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z139, 1.000)
       deallocate(Z139)
       deallocate(X93)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N0,M1,N2,M2,
     & N0,N2,N0,M1,N2,M2,N0,M1,S588,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S589(N2+1:M2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S589)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N2,M2,N2,M2,N0,M1,X92,S589,-1.000)
       deallocate(S589)
C
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2))
       call reorder612345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z135(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K9*K0
       I3=K0*K5
       call DMATMAT(I1,I2,I3,X92,F2,Z135)
       deallocate(F2)
C
       call
     & sum234516(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z135,-1.000)
       deallocate(Z135)
       deallocate(X92)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:M1,M2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,N2,N0,M1,M2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S584(N0+1:M1,N0+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5*K2
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S584)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N0,M1,M2,N3,
     & N0,N2,N0,M1,M2,N3,N0,M1,S584,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S585(N2+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S585)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,N2,M2,N0,M1,X90,S585,-1.000)
       deallocate(S585)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2))
       call reorder612345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N2,M2,N1,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z133(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K9*K0
       I3=K6*K5
       call DMATMAT(I1,I2,I3,X90,F2,Z133)
       deallocate(F2)
C
       call
     & sum234516(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z133,-1.000)
       deallocate(Z133)
       deallocate(X90)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N0,M1,M2,N3,
     & N0,N2,N0,M1,M2,N3,N0,M1,S584,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S592(N2+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S592)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,N2,M2,N0,M1,X91,S592,-1.000)
       deallocate(S592)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2))
       call reorder612345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N2,M2,N1,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z137(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K8*K5*K9*K0
       I3=K6*K5
       call DMATMAT(I1,I2,I3,X91,F2,Z137)
       deallocate(F2)
C
       call
     & sum134526(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z137, 1.000)
       deallocate(Z137)
       deallocate(X91)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N1,N1+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,M1,N1,N1,N3,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q64(M1+1:N1,N1+1:N3))
       I1=K3*K7
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q64)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,M1+1:N1))
       call reorder21(M1,N1,N1,N3,
     & N1,N3,M1,N1,Q64,B1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(Q65(N0+1:M1,M1+1:N1))
       I1=K7
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,B1,B2,Q65)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(M1,N1,N0,M1,X30,Q65, 1.000)
       deallocate(Q65)
C
       call sumx21(N0,N1,N0,N1,
     & M1,N1,N0,M1,X30,FAHH, 1.000)
C
       allocate(F2(M1+1:N1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2))
       call reorder612345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N1,N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z30(N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8*K5*K9*K0*K0
       I3=K7
       call DMATMAT(I1,I2,I3,X30,F2,Z30)
       deallocate(F2)
C
       V3C=V3C-Z30
       deallocate(Z30)
       deallocate(X30)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:N1,M2+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,M1,N2,N0,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S596(M1+1:N2,M1+1:N2,N0+1:N1,M2+1:N3))
       I1=K6*K1*K8
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S596)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder3241(M1,N2,M1,N2,N0,N1,M2,N3,
     & N0,N1,M1,N2,M2,N3,M1,N2,S596,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S597(N1+1:M2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S597)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M2,N3,N1,M2,M1,N2,X98,S597, 1.000)
       deallocate(S597)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder531246(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N2,M2,N2,M2,N0,M1,N0,M1,t3C1,F2)
       allocate(Z141(N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N2))
       I1=K8*K9
       I2=K5*K5*K0*K0
       I3=K6*K8
       call DMATMAT(I1,I2,I3,X98,F2,Z141)
       deallocate(F2)
C
       call
     & sum124635(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z141, 1.000)
       deallocate(Z141)
       deallocate(X98)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:N1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,M1,N2,N0,N1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S598(M1+1:N2,M1+1:N2,N0+1:N1,N1+1:M2))
       I1=K9*K1*K8
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S598)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N2,N1+1:M2,M1+1:N2))
       call reorder3241(M1,N2,M1,N2,N0,N1,N1,M2,
     & N0,N1,M1,N2,N1,M2,M1,N2,S598,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S599(N1+1:M2,M1+1:N2,N1+1:M2,M1+1:N2))
       I1=K8*K9*K8
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S599)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,N1,M2,N1,M2,M1,N2,X99,S599, 1.000)
       deallocate(S599)
C
       allocate(F2(M1+1:N2,N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder531246(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N1,M2,N2,M2,N2,M2,N0,M1,N0,M1,t3C1,F2)
       allocate(Z142(N2+1:M2,N2+1:M2,N0+1:M1,N0+1:M1,N1+1:M2,M1+1:N2))
       I1=K8*K9
       I2=K5*K5*K0*K0
       I3=K9*K8
       call DMATMAT(I1,I2,I3,X99,F2,Z142)
       deallocate(F2)
C
       call
     & sum124635(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z142, 1.000)
       deallocate(Z142)
       deallocate(X99)
C
       allocate(D1(N0+1:N1,N1+1:N3,M1+1:N2,N2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,M1,N2,N2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q66(M1+1:N2,N2+1:N3))
       I1=K4*K8
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q66)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,M1+1:N2))
       call reorder21(M1,N2,N2,N3,
     & N2,N3,M1,N2,Q66,B1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(Q70(N0+1:M1,M1+1:N2))
       I1=K8
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,B1,B2,Q70)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(M1,N2,N0,M1,X106,Q70, 1.000)
       deallocate(Q70)
C
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,N2,M2,N1,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z149(N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K8*K9*K0*K0
       I3=K8
       call DMATMAT(I1,I2,I3,X106,F2,Z149)
       deallocate(F2)
C
       call
     & sum123564(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z149,-1.000)
       deallocate(Z149)
       deallocate(X106)
C
       allocate(B1(N2+1:N3,M1+1:N2))
       call reorder21(M1,N2,N2,N3,
     & N2,N3,M1,N2,Q66,B1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(Q67(M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,B1,B2,Q67)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(M1,N2,M1,N2,X100,Q67, 1.000)
       deallocate(Q67)
C
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder512346(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,N2,M2,N1,M2,N0,M1,N0,M1,t3C1,F2)
       allocate(Z143(N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N2))
       I1=K8
       I2=K5*K5*K9*K0*K0
       I3=K8
       call DMATMAT(I1,I2,I3,X100,F2,Z143)
       deallocate(F2)
C
       call
     & sum123465(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z143,-1.000)
       deallocate(Z143)
       deallocate(X100)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:N1,M2+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,M1,N2,N0,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S602(N0+1:M1,M1+1:N2,N0+1:N1,M2+1:N3))
       I1=K6*K1*K8
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S602)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder3241(N0,M1,M1,N2,N0,N1,M2,N3,
     & N0,N1,M1,N2,M2,N3,N0,M1,S602,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S603(N1+1:M2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S603)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M2,N3,N1,M2,N0,M1,X102,S603, 1.000)
       deallocate(S603)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder431256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N2,M2,N2,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z145(N2+1:M2,N2+1:M2,M1+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K5*K8*K0*K0
       I3=K6*K8
       call DMATMAT(I1,I2,I3,X102,F2,Z145)
       deallocate(F2)
C
       call
     & sum125634(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z145, 1.000)
       deallocate(Z145)
       deallocate(X102)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:N1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,M1,N0,N1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S604(N0+1:M1,N0+1:M1,N0+1:N1,N1+1:M2))
       I1=K9*K1*K5
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S604)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder3241(N0,M1,N0,M1,N0,N1,N1,M2,
     & N0,N1,N0,M1,N1,M2,N0,M1,S604,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S605(N1+1:M2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S605)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N1,M2,N1,M2,N0,M1,X103,S605, 1.000)
       deallocate(S605)
C
       allocate(F2(N0+1:M1,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder431256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N1,M2,N2,M2,N2,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z146(N2+1:M2,N2+1:M2,M1+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K5*K8*K0*K0
       I3=K9*K5
       call DMATMAT(I1,I2,I3,X103,F2,Z146)
       deallocate(F2)
C
       call
     & sum125634(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z146, 1.000)
       deallocate(Z146)
       deallocate(X103)
C
       allocate(D1(N2+1:N3,M1+1:N2,N0+1:N1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,M1,N2,N0,N1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S606(N0+1:M1,M1+1:N2,N0+1:N1,N1+1:M2))
       I1=K9*K1*K8
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S606)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,M1+1:N2,N1+1:M2,N0+1:M1))
       call reorder3241(N0,M1,M1,N2,N0,N1,N1,M2,
     & N0,N1,M1,N2,N1,M2,N0,M1,S606,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S607(N1+1:M2,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9*K8
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S607)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,N1,M2,N1,M2,N0,M1,X104,S607, 1.000)
       deallocate(S607)
C
       allocate(F2(M1+1:N2,N1+1:M2,N2+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder431256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N1,M2,N2,M2,N2,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z147(N2+1:M2,N2+1:M2,M1+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K5*K8*K0*K0
       I3=K9*K8
       call DMATMAT(I1,I2,I3,X104,F2,Z147)
       deallocate(F2)
C
       call
     & sum125634(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z147, 1.000)
       deallocate(Z147)
       deallocate(X104)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:M1,N2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,M1,N2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q68(N0+1:M1,N2+1:N3))
       I1=K4*K5
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q68)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:M1))
       call reorder21(N0,M1,N2,N3,
     & N2,N3,N0,M1,Q68,B1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(Q69(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,B1,B2,Q69)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(N0,M1,N0,M1,X105,Q69, 1.000)
       deallocate(Q69)
C
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N2,M2,N1,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z148(N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K8*K9*K0*K0
       I3=K5
       call DMATMAT(I1,I2,I3,X105,F2,Z148)
       deallocate(F2)
C
       call
     & sum123564(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z148,-1.000)
       deallocate(Z148)
       deallocate(X105)
C
       allocate(D1(N2+1:N3,N0+1:M1,N0+1:N1,M2+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,M1,N0,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S600(N0+1:M1,N0+1:M1,N0+1:N1,M2+1:N3))
       I1=K6*K1*K5
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S600)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder3241(N0,M1,N0,M1,N0,N1,M2,N3,
     & N0,N1,N0,M1,M2,N3,N0,M1,S600,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S601(N1+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S601)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,N1,M2,N0,M1,X101,S601, 1.000)
       deallocate(S601)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder431256(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N2,M2,N2,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z144(N2+1:M2,N2+1:M2,M1+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K5*K8*K0*K0
       I3=K6*K5
       call DMATMAT(I1,I2,I3,X101,F2,Z144)
       deallocate(F2)
C
       call
     & sum125634(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z144, 1.000)
       deallocate(Z144)
       deallocate(X101)
C
       allocate(D1(N0+1:N1,N0+1:N2,M2+1:N3,M2+1:N3))
       call reorder4312(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N0,N2,M2,N3,M2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S608(N1+1:M2,N0+1:N2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K2
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S608)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,N1+1:M2))
       call reorder2341(N1,M2,N0,N2,M2,N3,M2,N3,
     & N0,N2,M2,N3,M2,N3,N1,M2,S608,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S616(N2+1:M2,M2+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6*K6
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S616)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,M2,N3,N2,M2,N1,M2,X69,S616, 1.000)
       deallocate(S616)
C
       call sumx1234(N2,N3,N1,N3,N2,N3,N1,M2,
     & M2,N3,M2,N3,N2,M2,N1,M2,X69,VBAPPP, 1.000)
C
       allocate(F2(M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder132456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,N0,M1,t3C1,F2)
       allocate(Z69(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1,N2+1:M2,N1+1:M2))
       I1=K9*K0
       I2=K5*K8*K5*K0
       I3=K6*K6
       call DMATMAT(I1,I2,I3,X69,F2,Z69)
       deallocate(F2)
C
       call
     & sum145623(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z69,-1.000)
       deallocate(Z69)
       deallocate(X69)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,N1+1:M2))
       call reorder2341(N1,M2,N0,N2,M2,N3,M2,N3,
     & N0,N2,M2,N3,M2,N3,N1,M2,S608,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S609(N2+1:M2,M2+1:N3,M2+1:N3,N1+1:M2))
       I1=K9*K6*K6
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S609)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,M2,N3,N2,M2,N1,M2,X65,S609, 1.000)
       deallocate(S609)
C
       call sumx1234(N2,N3,N1,N3,N2,N3,N1,M2,
     & M2,N3,M2,N3,N2,M2,N1,M2,X65,VBAPPP, 1.000)
C
       allocate(F2(M2+1:N3,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder132456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M2,N3,M2,N3,N2,M2,N0,M1,M1,N2,N0,M1,t3C1,F2)
       allocate(Z65(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1,N2+1:M2,N1+1:M2))
       I1=K9*K0
       I2=K5*K8*K5*K0
       I3=K6*K6
       call DMATMAT(I1,I2,I3,X65,F2,Z65)
       deallocate(F2)
C
       call
     & sum245613(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z65, 1.000)
       deallocate(Z65)
       deallocate(X65)
C
       allocate(D1(N0+1:N1,N0+1:N2,M2+1:N3,N1+1:M2))
       call reorder4312(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N0,N2,M2,N3,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S612(N1+1:M2,N0+1:N2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K2
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S612)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder2341(N1,M2,N0,N2,M2,N3,N1,M2,
     & N0,N2,M2,N3,N1,M2,N1,M2,S612,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S618(N2+1:M2,M2+1:N3,N1+1:M2,N1+1:M2))
       I1=K9*K9*K6
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S618)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,N1,M2,N2,M2,N1,M2,X71,S618, 1.000)
       deallocate(S618)
C
       call sumx1234(N2,N3,N1,N3,N2,N3,N1,M2,
     & M2,N3,N1,M2,N2,M2,N1,M2,X71,VBAPPP, 1.000)
C
       allocate(F2(M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder132456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M2,N3,N1,M2,N2,M2,N0,M1,M1,N2,N0,M1,t3C1,F2)
       allocate(Z71(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1,N2+1:M2,N1+1:M2))
       I1=K9*K0
       I2=K5*K8*K5*K0
       I3=K9*K6
       call DMATMAT(I1,I2,I3,X71,F2,Z71)
       deallocate(F2)
C
       call
     & sum145623(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z71,-1.000)
       deallocate(Z71)
       deallocate(X71)
C
       allocate(D1(N0+1:N2,M2+1:N3,N1+1:M2,N1+1:M2))
       call reorder2341(N1,M2,N0,N2,M2,N3,N1,M2,
     & N0,N2,M2,N3,N1,M2,N1,M2,S612,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S613(N2+1:M2,M2+1:N3,N1+1:M2,N1+1:M2))
       I1=K9*K9*K6
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S613)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,N1,M2,N2,M2,N1,M2,X67,S613, 1.000)
       deallocate(S613)
C
       call sumx1234(N2,N3,N1,N3,N2,N3,N1,M2,
     & M2,N3,N1,M2,N2,M2,N1,M2,X67,VBAPPP, 1.000)
C
       allocate(F2(M2+1:N3,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder132456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M2,N3,N1,M2,N2,M2,N0,M1,M1,N2,N0,M1,t3C1,F2)
       allocate(Z67(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1,N2+1:M2,N1+1:M2))
       I1=K9*K0
       I2=K5*K8*K5*K0
       I3=K9*K6
       call DMATMAT(I1,I2,I3,X67,F2,Z67)
       deallocate(F2)
C
       call
     & sum245613(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z67, 1.000)
       deallocate(Z67)
       deallocate(X67)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,M2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q71(N0+1:N2,M2+1:N3))
       I1=K6*K2
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q71)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(Q75(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,Q71,B2,Q75)
       deallocate(B2)
C
       call
     & sum21(M2,N3,N2,M2,X108,Q75,-1.000)
       deallocate(Q75)
C
       allocate(F2(M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder123456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M2,N3,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,t3C1,F2)
       allocate(Z160(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0
       I2=K5*K8*K5*K9*K0
       I3=K6
       call DMATMAT(I1,I2,I3,X108,F2,Z160)
       deallocate(F2)
C
       call
     & sum134562(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z160,-1.000)
       deallocate(Z160)
       deallocate(X108)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(Q72(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,Q71,B2,Q72)
       deallocate(B2)
C
       call
     & sum21(M2,N3,N2,M2,X107,Q72,-1.000)
       deallocate(Q72)
C
       allocate(F2(M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder123456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M2,N3,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,t3C1,F2)
       allocate(Z154(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0
       I2=K5*K8*K5*K9*K0
       I3=K6
       call DMATMAT(I1,I2,I3,X107,F2,Z154)
       deallocate(F2)
C
       call
     & sum234561(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z154, 1.000)
       deallocate(Z154)
       deallocate(X107)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:M2,M2+1:N3))
       call reorder4312(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N0,N2,N2,M2,M2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S610(N1+1:M2,N0+1:N2,N2+1:M2,M2+1:N3))
       I1=K6*K0*K2
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S610)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:M2,M2+1:N3,N1+1:M2))
       call reorder2341(N1,M2,N0,N2,N2,M2,M2,N3,
     & N0,N2,N2,M2,M2,N3,N1,M2,S610,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S617(N2+1:M2,N2+1:M2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K0
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S617)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,M2,M2,N3,N2,M2,N1,M2,X70,S617, 1.000)
       deallocate(S617)
C
       call sumx1234(N2,N3,N1,N3,N2,N3,N1,M2,
     & N2,M2,M2,N3,N2,M2,N1,M2,X70,VBAPPP, 1.000)
C
       allocate(F2(N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder132456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N2,M2,M2,N3,N2,M2,N0,M1,M1,N2,N0,M1,t3C1,F2)
       allocate(Z70(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1,N2+1:M2,N1+1:M2))
       I1=K9*K0
       I2=K5*K8*K5*K0
       I3=K6*K0
       call DMATMAT(I1,I2,I3,X70,F2,Z70)
       deallocate(F2)
C
       call
     & sum145623(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z70,-1.000)
       deallocate(Z70)
       deallocate(X70)
C
       allocate(D1(N0+1:N2,N2+1:M2,M2+1:N3,N1+1:M2))
       call reorder2341(N1,M2,N0,N2,N2,M2,M2,N3,
     & N0,N2,N2,M2,M2,N3,N1,M2,S610,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S611(N2+1:M2,N2+1:M2,M2+1:N3,N1+1:M2))
       I1=K9*K6*K0
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S611)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,M2,M2,N3,N2,M2,N1,M2,X66,S611, 1.000)
       deallocate(S611)
C
       call sumx1234(N2,N3,N1,N3,N2,N3,N1,M2,
     & N2,M2,M2,N3,N2,M2,N1,M2,X66,VBAPPP, 1.000)
C
       allocate(F2(N2+1:M2,M2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder132456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N2,M2,M2,N3,N2,M2,N0,M1,M1,N2,N0,M1,t3C1,F2)
       allocate(Z66(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1,N2+1:M2,N1+1:M2))
       I1=K9*K0
       I2=K5*K8*K5*K0
       I3=K6*K0
       call DMATMAT(I1,I2,I3,X66,F2,Z66)
       deallocate(F2)
C
       call
     & sum245613(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z66, 1.000)
       deallocate(Z66)
       deallocate(X66)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:M2,N1+1:M2))
       call reorder4312(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N0,N2,N2,M2,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S614(N1+1:M2,N0+1:N2,N2+1:M2,N1+1:M2))
       I1=K9*K0*K2
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S614)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:M2,N1+1:M2,N1+1:M2))
       call reorder2341(N1,M2,N0,N2,N2,M2,N1,M2,
     & N0,N2,N2,M2,N1,M2,N1,M2,S614,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S619(N2+1:M2,N2+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9*K0
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S619)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,M2,N1,M2,N2,M2,N1,M2,X72,S619, 1.000)
       deallocate(S619)
C
       call sumx1234(N2,N3,N1,N3,N2,N3,N1,M2,
     & N2,M2,N1,M2,N2,M2,N1,M2,X72,VBAPPP, 1.000)
C
       allocate(F2(N2+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder132456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N2,M2,N1,M2,N2,M2,N0,M1,M1,N2,N0,M1,t3C1,F2)
       allocate(Z72(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1,N2+1:M2,N1+1:M2))
       I1=K9*K0
       I2=K5*K8*K5*K0
       I3=K9*K0
       call DMATMAT(I1,I2,I3,X72,F2,Z72)
       deallocate(F2)
C
       call
     & sum145623(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z72,-1.000)
       deallocate(Z72)
       deallocate(X72)
C
       allocate(D1(N0+1:N2,N2+1:M2,N1+1:M2,N1+1:M2))
       call reorder2341(N1,M2,N0,N2,N2,M2,N1,M2,
     & N0,N2,N2,M2,N1,M2,N1,M2,S614,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S615(N2+1:M2,N2+1:M2,N1+1:M2,N1+1:M2))
       I1=K9*K9*K0
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S615)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,M2,N1,M2,N2,M2,N1,M2,X68,S615, 1.000)
       deallocate(S615)
C
       call sumx1234(N2,N3,N1,N3,N2,N3,N1,M2,
     & N2,M2,N1,M2,N2,M2,N1,M2,X68,VBAPPP, 1.000)
C
       allocate(F2(N2+1:M2,N1+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder132456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N2,M2,N1,M2,N2,M2,N0,M1,M1,N2,N0,M1,t3C1,F2)
       allocate(Z68(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1,N2+1:M2,N1+1:M2))
       I1=K9*K0
       I2=K5*K8*K5*K0
       I3=K9*K0
       call DMATMAT(I1,I2,I3,X68,F2,Z68)
       deallocate(F2)
C
       call
     & sum245613(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z68, 1.000)
       deallocate(Z68)
       deallocate(X68)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:M2))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N2,M2,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q73(N0+1:N2,N2+1:M2))
       I1=K0*K2
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q73)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(Q76(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,Q73,B2,Q76)
       deallocate(B2)
C
       call
     & sum21(N2,M2,N2,M2,X110,Q76,-1.000)
       deallocate(Q76)
C
       allocate(F2(N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder123456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,t3C1,F2)
       allocate(Z161(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0
       I2=K5*K8*K5*K9*K0
       I3=K0
       call DMATMAT(I1,I2,I3,X110,F2,Z161)
       deallocate(F2)
C
       call
     & sum134562(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z161,-1.000)
       deallocate(Z161)
       deallocate(X110)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(Q74(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,Q73,B2,Q74)
       deallocate(B2)
C
       call
     & sum21(N2,M2,N2,M2,X109,Q74,-1.000)
       deallocate(Q74)
C
       allocate(F2(N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder123456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,t3C1,F2)
       allocate(Z155(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0
       I2=K5*K8*K5*K9*K0
       I3=K0
       call DMATMAT(I1,I2,I3,X109,F2,Z155)
       deallocate(F2)
C
       call
     & sum234561(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z155, 1.000)
       deallocate(Z155)
       deallocate(X109)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,M2+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q77(N0+1:N1,M2+1:N3))
       I1=K6*K1
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q77)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(Q78(N1+1:M2,M2+1:N3))
       I1=K6
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,Q77,B2,Q78)
       deallocate(B2)
C
       call
     & sum21(M2,N3,N1,M2,X31,Q78,-1.000)
       deallocate(Q78)
C
       call sumx12(N1,N3,N1,N3,
     & M2,N3,N1,M2,X31,FAPP, 1.000)
C
       allocate(F2(M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder312456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,N0,M1,t3C1,F2)
       allocate(Z31(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9
       I2=K5*K8*K5*K0*K0
       I3=K6
       call DMATMAT(I1,I2,I3,X31,F2,Z31)
       deallocate(F2)
C
       call
     & sum124563(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z31, 1.000)
       deallocate(Z31)
       deallocate(X31)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:M2))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N1,M2,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q79(N0+1:N1,N1+1:M2))
       I1=K9*K1
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q79)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(Q80(N1+1:M2,N1+1:M2))
       I1=K9
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,Q79,B2,Q80)
       deallocate(B2)
C
       call
     & sum21(N1,M2,N1,M2,X32,Q80,-1.000)
       deallocate(Q80)
C
       call sumx12(N1,N3,N1,N3,
     & N1,M2,N1,M2,X32,FAPP, 1.000)
C
       allocate(F2(N1+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder312456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N1,M2,N2,M2,N2,M2,N0,M1,M1,N2,N0,M1,t3C1,F2)
       allocate(Z32(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N0+1:M1,N1+1:M2))
       I1=K9
       I2=K5*K8*K5*K0*K0
       I3=K9
       call DMATMAT(I1,I2,I3,X32,F2,Z32)
       deallocate(F2)
C
       call
     & sum124563(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z32, 1.000)
       deallocate(Z32)
       deallocate(X32)
C
       allocate(D1(N1+1:N3,N0+1:N1,N0+1:N1,N1+1:N3))
       call reorder2341(N1,N3,N1,N3,N0,N1,N0,N1,
     & N1,N3,N0,N1,N0,N1,N1,N3,VAHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S620(N0+1:M1,N0+1:N1,N0+1:N1,N1+1:N3))
       I1=K3*K1*K1
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S620)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder2431(N0,M1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N1,N0,M1,S620,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S622(N2+1:M2,M1+1:N2,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K8*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S622)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N2,M2,M1,N2,N0,M1,X1,S622, 1.000)
       deallocate(S622)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder2431(N0,M1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N1,N0,M1,S620,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S623(N2+1:M2,N0+1:M1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K5*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S623)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N2,M2,N0,M1,N0,M1,X3,S623, 1.000)
       deallocate(S623)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder2431(N0,M1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N1,N0,M1,S620,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S624(N2+1:M2,M1+1:N2,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K8*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S624)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N2,M2,M1,N2,N0,M1,X2,S624, 1.000)
       deallocate(S624)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N0+1:M1))
       call reorder2431(N0,M1,N0,N1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N1,N0,M1,S620,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S621(N2+1:M2,N0+1:M1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K5*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S621)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N2,M2,N0,M1,N0,M1,X4,S621, 1.000)
       deallocate(S621)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N1,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S625(N2+1:M2,N0+1:M1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K5*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S625)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder3412(N2,M2,N0,M1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N2,M2,N0,M1,S625,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S626(N1+1:M2,N1+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K3
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S626)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,N3,N2,M2,N1,M2,N0,M1,X8,S626,-1.000)
       deallocate(S626)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N1,N3,VAHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q81(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q81)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q81,B1)
       allocate(D2(N1+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,M2,M1,N2,N0,M1,t2B,D2)
       allocate(S630(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K3
       call DMATMAT(I1,I2,I3,B1,D2,S630)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X2,S630, 1.000)
       deallocate(S630)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q81,B1)
       allocate(D2(N1+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,M2,N0,M1,N0,M1,t2B,D2)
       allocate(S633(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K0
       I3=K3
       call DMATMAT(I1,I2,I3,B1,D2,S633)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,N2,M2,N0,M1,N0,M1,X3,S633, 1.000)
       deallocate(S633)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q81,B1)
       allocate(D2(N1+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,M2,N0,M1,N0,M1,t2B,D2)
       allocate(S636(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K0
       I3=K3
       call DMATMAT(I1,I2,I3,B1,D2,S636)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,N2,M2,N0,M1,N0,M1,X4,S636, 1.000)
       deallocate(S636)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q81,B1)
       allocate(D2(N1+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,M2,M1,N2,N0,M1,t2B,D2)
       allocate(S627(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K3
       call DMATMAT(I1,I2,I3,B1,D2,S627)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X1,S627, 1.000)
       deallocate(S627)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N1,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S628(N2+1:M2,N0+1:M1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K5*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S628)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder3412(N2,M2,N0,M1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N2,M2,N0,M1,S628,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S629(N1+1:M2,N1+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K3
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S629)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,N3,N2,M2,N1,M2,N0,M1,X7,S629,-1.000)
       deallocate(S629)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N1,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S634(N2+1:M2,M1+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K8*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S634)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N2,M2,M1,N2,S634,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S635(N1+1:M2,N1+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K3
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S635)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,N3,N2,M2,N1,M2,M1,N2,X5,S635,-1.000)
       deallocate(S635)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N0,N1,N1,N3,VAHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S631(N2+1:M2,M1+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K8*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S631)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N2,M2,M1,N2,S631,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S632(N1+1:M2,N1+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K3
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S632)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,N3,N2,M2,N1,M2,M1,N2,X6,S632,-1.000)
       deallocate(S632)
C
       allocate(D1(N1+1:N3,N0+1:N2,N0+1:N1,N2+1:N3))
       call reorder2341(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N0,N2,N0,N1,N2,N3,VBHHPP,D1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(S637(N0+1:M1,N0+1:N2,N0+1:N1,N2+1:N3))
       I1=K4*K1*K2
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,D1,B2,S637)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder2431(N0,M1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N2,N3,N0,N1,N0,M1,S637,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S639(N2+1:M2,N0+1:M1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K5*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S639)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N2,M2,N0,M1,N0,M1,X3,S639, 1.000)
       deallocate(S639)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:M1))
       call reorder3421(N0,M1,N0,N2,N0,N1,N2,N3,
     & N0,N1,N2,N3,N0,N2,N0,M1,S637,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,M2,M1,N2,t2B,D2)
       allocate(S640(N1+1:M2,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K9
       I3=K4*K1
       call DMATMAT(I1,I2,I3,D1,D2,S640)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,N1,M2,M1,N2,N0,M1,X13,S640,-1.000)
       deallocate(S640)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder2431(N0,M1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N2,N3,N0,N1,N0,M1,S637,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S641(N2+1:M2,M1+1:N2,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K8*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S641)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N2,M2,M1,N2,N0,M1,X2,S641, 1.000)
       deallocate(S641)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder2431(N0,M1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N2,N3,N0,N1,N0,M1,S637,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S642(N2+1:M2,M1+1:N2,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K8*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S642)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N2,M2,M1,N2,N0,M1,X1,S642, 1.000)
       deallocate(S642)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N0+1:M1))
       call reorder3421(N0,M1,N0,N2,N0,N1,N2,N3,
     & N0,N1,N2,N3,N0,N2,N0,M1,S637,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S643(N1+1:M2,N0+1:M1,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K5*K9
       I3=K4*K1
       call DMATMAT(I1,I2,I3,D1,D2,S643)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,N1,M2,N0,M1,N0,M1,X14,S643,-1.000)
       deallocate(S643)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N0,N1,N2,N3,N0,M1,S637,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,N1,M2,t2B,D2)
       allocate(S644(N2+1:M2,N1+1:M2,N2+1:N3,N0+1:M1))
       I1=K5*K4
       I2=K9*K0
       I3=K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,S644)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N2,N3,N2,M2,N1,M2,N0,M1,X15,S644, 1.000)
       deallocate(S644)
C
       allocate(D1(N0+1:N2,N0+1:N1,N2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N0,N1,N2,N3,N0,M1,S637,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,N1,M2,t2B,D2)
       allocate(S645(N2+1:M2,N1+1:M2,N2+1:N3,N0+1:M1))
       I1=K5*K4
       I2=K9*K0
       I3=K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,S645)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N2,N3,N2,M2,N1,M2,N0,M1,X16,S645, 1.000)
       deallocate(S645)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:M1))
       call reorder4231(N0,M1,N0,N2,N0,N1,N2,N3,
     & N2,N3,N0,N2,N0,N1,N0,M1,S637,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S811(M1+1:N2,N0+1:N2,N0+1:N1,N0+1:M1))
       I1=K5*K1*K2
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S811)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N2,M1+1:N2,N0+1:M1))
       call reorder3214(M1,N2,N0,N2,N0,N1,N0,M1,
     & N0,N1,N0,N2,M1,N2,N0,M1,S811,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S812(N1+1:M2,N0+1:N2,M1+1:N2,N0+1:M1))
       I1=K5*K8*K2
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S812)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,N1,M2,M1,N2,N0,M1,X13,S812,-1.000)
       deallocate(S812)
C
       allocate(D1(N0+1:N1,N0+1:N2,N2+1:N3,N0+1:M1))
       call reorder3241(N0,M1,N0,N2,N0,N1,N2,N3,
     & N0,N1,N0,N2,N2,N3,N0,M1,S637,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S815(N1+1:M2,N0+1:N2,N2+1:N3,N0+1:M1))
       I1=K5*K4*K2
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S815)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder2314(N1,M2,N0,N2,N2,N3,N0,M1,
     & N0,N2,N2,N3,N1,M2,N0,M1,S815,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S817(N2+1:M2,N2+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K4
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S817)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,N1,M2,N0,M1,X16,S817, 1.000)
       deallocate(S817)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:M1))
       call reorder4231(N0,M1,N0,N2,N0,N1,N2,N3,
     & N2,N3,N0,N2,N0,N1,N0,M1,S637,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S813(N0+1:M1,N0+1:N2,N0+1:N1,N0+1:M1))
       I1=K5*K1*K2
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S813)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:M1,N0+1:M1))
       call reorder3214(N0,M1,N0,N2,N0,N1,N0,M1,
     & N0,N1,N0,N2,N0,M1,N0,M1,S813,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S814(N1+1:M2,N0+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K2
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S814)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,N1,M2,N0,M1,N0,M1,X14,S814,-1.000)
       deallocate(S814)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N0+1:M1))
       call reorder2431(N0,M1,N0,N2,N0,N1,N2,N3,
     & N0,N2,N2,N3,N0,N1,N0,M1,S637,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S638(N2+1:M2,N0+1:M1,N0+1:N1,N0+1:M1))
       I1=K5*K1
       I2=K5*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S638)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N1,N2,M2,N0,M1,N0,M1,X4,S638, 1.000)
       deallocate(S638)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,N0+1:M1))
       call reorder2314(M1,N2,N0,N2,N0,N1,N0,M1,
     & N0,N2,N0,N1,M1,N2,N0,M1,S811,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S818(N2+1:M2,N0+1:N1,M1+1:N2,N0+1:M1))
       I1=K5*K8*K1
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S818)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N2,M2,M1,N2,N0,M1,X1,S818,-1.000)
       deallocate(S818)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,N0+1:M1))
       call reorder2314(M1,N2,N0,N2,N0,N1,N0,M1,
     & N0,N2,N0,N1,M1,N2,N0,M1,S811,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S819(N2+1:M2,N0+1:N1,M1+1:N2,N0+1:M1))
       I1=K5*K8*K1
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S819)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N2,M2,M1,N2,N0,M1,X2,S819,-1.000)
       deallocate(S819)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder2314(N1,M2,N0,N2,N2,N3,N0,M1,
     & N0,N2,N2,N3,N1,M2,N0,M1,S815,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S816(N2+1:M2,N2+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K4
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S816)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,N1,M2,N0,M1,X15,S816, 1.000)
       deallocate(S816)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:M1,N0+1:M1))
       call reorder2314(N0,M1,N0,N2,N0,N1,N0,M1,
     & N0,N2,N0,N1,N0,M1,N0,M1,S813,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S820(N2+1:M2,N0+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K1
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S820)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N2,M2,N0,M1,N0,M1,X3,S820,-1.000)
       deallocate(S820)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:M1,N0+1:M1))
       call reorder2314(N0,M1,N0,N2,N0,N1,N0,M1,
     & N0,N2,N0,N1,N0,M1,N0,M1,S813,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S821(N2+1:M2,N0+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K1
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S821)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N2,M2,N0,M1,N0,M1,X4,S821,-1.000)
       deallocate(S821)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,N2+1:N3))
       call reorder3241(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N0,N1,N2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S660(N2+1:M2,N0+1:M1,N0+1:N1,N2+1:N3))
       I1=K4*K1
       I2=K5*K0
       I3=K3*K2
       call DMATMAT(I1,I2,I3,D1,D2,S660)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:M2,N0+1:M1))
       call reorder4312(N2,M2,N0,M1,N0,N1,N2,N3,
     & N2,N3,N0,N1,N2,M2,N0,M1,S660,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S752(M1+1:N2,N0+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K1
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S752)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N1,N2,M2,M1,N2,N0,M1,X1,S752,-1.000)
       deallocate(S752)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:M2,N0+1:M1))
       call reorder4312(N2,M2,N0,M1,N0,N1,N2,N3,
     & N2,N3,N0,N1,N2,M2,N0,M1,S660,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S769(N0+1:M1,N0+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K1
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S769)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N1,N2,M2,N0,M1,N0,M1,X3,S769,-1.000)
       deallocate(S769)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3412(N2,M2,N0,M1,N0,N1,N2,N3,
     & N0,N1,N2,N3,N2,M2,N0,M1,S660,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S661(N1+1:M2,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S661)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,N3,N2,M2,N1,M2,N0,M1,X15,S661, 1.000)
       deallocate(S661)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N2,N3,VBHHPP,D1)
       allocate(B2(N0+1:N1,N1+1:N3))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,N3,t1A,B2)
       allocate(Q82(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K3*K1
       call DMATVEC(I1,I3,D1,B2,Q82)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q82,B1)
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,N0,M1,M1,N2,t2C,D2)
       allocate(S662(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K4
       call DMATMAT(I1,I2,I3,B1,D2,S662)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X10,S662,-1.000)
       deallocate(S662)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q82,B1)
       allocate(D2(N2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,M2,M1,N2,N0,M1,t2B,D2)
       allocate(S652(N1+1:M2,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K9
       I3=K4
       call DMATMAT(I1,I2,I3,B1,D2,S652)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N2,N1,M2,M1,N2,N0,M1,X13,S652, 1.000)
       deallocate(S652)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q82,B1)
       allocate(D2(N2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,M2,N0,M1,N0,M1,t2B,D2)
       allocate(S659(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K9
       I3=K4
       call DMATMAT(I1,I2,I3,B1,D2,S659)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N2,N1,M2,N0,M1,N0,M1,X14,S659, 1.000)
       deallocate(S659)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q82,B1)
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,N0,M1,M1,N2,t2C,D2)
       allocate(S665(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K4
       call DMATMAT(I1,I2,I3,B1,D2,S665)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X9,S665,-1.000)
       deallocate(S665)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N0+1:M1))
       call reorder1342(N2,N3,N0,N1,N0,N2,N0,N1,
     & N2,N3,N0,N2,N0,N1,N0,M1,VBHHHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S563(N0+1:M1,N0+1:N2,N0+1:N1,N0+1:M1))
       I1=K5*K1*K2
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S563)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:M1,N0+1:M1))
       call reorder3214(N0,M1,N0,N2,N0,N1,N0,M1,
     & N0,N1,N0,N2,N0,M1,N0,M1,S563,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S564(N1+1:M2,N0+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K2
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S564)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,N1,M2,N0,M1,N0,M1,X14,S564,-1.000)
       deallocate(S564)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:M1,N0+1:M1))
       call reorder2314(N0,M1,N0,N2,N0,N1,N0,M1,
     & N0,N2,N0,N1,N0,M1,N0,M1,S563,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S668(N2+1:M2,N0+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K1
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S668)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N2,M2,N0,M1,N0,M1,X3,S668,-1.000)
       deallocate(S668)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:M1,N0+1:M1))
       call reorder2314(N0,M1,N0,N2,N0,N1,N0,M1,
     & N0,N2,N0,N1,N0,M1,N0,M1,S563,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S669(N2+1:M2,N0+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K1
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S669)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N2,M2,N0,M1,N0,M1,X4,S669,-1.000)
       deallocate(S669)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N1+1:M2))
       call reorder1324(N2,N3,N1,N3,N0,N2,N1,N3,
     & N2,N3,N0,N2,N1,N3,N1,M2,VBPHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S673(N0+1:M1,N0+1:N2,N1+1:N3,N1+1:M2))
       I1=K9*K3*K2
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S673)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N1,N3,N1,M2,
     & N0,N2,N1,N3,N1,M2,N0,M1,S673,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S675(N2+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S675)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,N2,M2,N1,M2,N0,M1,X8,S675,-1.000)
       deallocate(S675)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N1,N3,N1,M2,
     & N0,N2,N1,N3,N1,M2,N0,M1,S673,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S674(N2+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S674)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,N2,M2,N1,M2,N0,M1,X7,S674,-1.000)
       deallocate(S674)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,M1+1:N2))
       call reorder1342(N2,N3,N0,N2,N0,N2,N0,N2,
     & N2,N3,N0,N2,N0,N2,M1,N2,VCHHHP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S676(N0+1:M1,N0+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2*K2
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S676)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder2314(N0,M1,N0,N2,N0,N2,M1,N2,
     & N0,N2,N0,N2,N0,M1,M1,N2,S676,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S678(N2+1:M2,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S678)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,N2,M2,N0,M1,M1,N2,X10,S678,-1.000)
       deallocate(S678)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder2314(N0,M1,N0,N2,N0,N2,M1,N2,
     & N0,N2,N0,N2,N0,M1,M1,N2,S676,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S677(N2+1:M2,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S677)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,N2,M2,N0,M1,M1,N2,X9,S677,-1.000)
       deallocate(S677)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder4312(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,M1,N2,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S679(N2+1:M2,N0+1:N2,N2+1:N3,M1+1:N2))
       I1=K8*K4*K2
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S679)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder2314(N2,M2,N0,N2,N2,N3,M1,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,S679,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S680(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S680)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,N2,M2,M1,N2,X11,S680, 1.000)
       deallocate(S680)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N0+1:M1))
       call reorder1342(N2,N3,N0,N2,N0,N2,N0,N2,
     & N2,N3,N0,N2,N0,N2,N0,M1,VCHHHP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S681(M1+1:N2,N0+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2*K2
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S681)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,N2,N0,M1,
     & N0,N2,N0,N2,N0,M1,M1,N2,S681,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S683(N2+1:M2,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S683)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,N2,M2,N0,M1,M1,N2,X10,S683, 1.000)
       deallocate(S683)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,N2,N0,M1,
     & N0,N2,N0,N2,N0,M1,M1,N2,S681,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S682(N2+1:M2,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S682)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,N2,M2,N0,M1,M1,N2,X9,S682, 1.000)
       deallocate(S682)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder2413(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,N2,N2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S684(M1+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4*K2
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S684)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N2,N3,N2,M2,
     & N0,N2,N2,N3,N2,M2,M1,N2,S684,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S688(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S688)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,N3,N2,M2,N2,M2,M1,N2,X11,S688,-1.000)
       deallocate(S688)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:M2,M1+1:N2))
       call reorder3241(M1,N2,N0,N2,N2,N3,N2,M2,
     & N2,N3,N0,N2,N2,M2,M1,N2,S684,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S685(N0+1:M1,N0+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K2
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S685)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N2,N2,M2,N0,M1,M1,N2,X9,S685, 1.000)
       deallocate(S685)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder2413(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,N2,N2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S686(M1+1:N2,N0+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4*K2
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S686)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N2,N3,N2,M2,
     & N0,N2,N2,N3,N2,M2,M1,N2,S686,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S689(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S689)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,N2,M2,M1,N2,X11,S689, 1.000)
       deallocate(S689)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:M2,M1+1:N2))
       call reorder3241(M1,N2,N0,N2,N2,N3,N2,M2,
     & N2,N3,N0,N2,N2,M2,M1,N2,S686,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S687(N0+1:M1,N0+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K2
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S687)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N2,N2,M2,N0,M1,M1,N2,X10,S687, 1.000)
       deallocate(S687)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:M1))
       call reorder4312(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,N3,N0,M1,VCHHHP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S690(N2+1:M2,N0+1:N2,N2+1:N3,N0+1:M1))
       I1=K5*K4*K2
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S690)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder2314(N2,M2,N0,N2,N2,N3,N0,M1,
     & N0,N2,N2,N3,N2,M2,N0,M1,S690,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S691(N2+1:M2,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S691)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,N2,M2,N0,M1,X12,S691, 1.000)
       deallocate(S691)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,N2,N2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S692(N0+1:M1,N0+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4*K2
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S692)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N2,N3,N2,M2,
     & N0,N2,N2,N3,N2,M2,N0,M1,S692,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S693(N2+1:M2,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S693)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,N3,N2,M2,N2,M2,N0,M1,X12,S693, 1.000)
       deallocate(S693)
C
       allocate(D1(N2+1:N3,N0+1:N2,N2+1:N3,N2+1:M2))
       call reorder1423(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N0,N2,N2,N3,N2,M2,VCHPPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S694(N0+1:M1,N0+1:N2,N2+1:N3,N2+1:M2))
       I1=K0*K4*K2
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S694)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N2,N3,N2,M2,
     & N0,N2,N2,N3,N2,M2,N0,M1,S694,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S695(N2+1:M2,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S695)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,N2,M2,N0,M1,X12,S695,-1.000)
       deallocate(S695)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N1,M2+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,N2,M1,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S696(M1+1:N2,N0+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7*K2
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S696)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,M1,N1,M2,N3,
     & N0,N2,M1,N1,M2,N3,M1,N2,S696,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S700(N2+1:M2,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K7
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S700)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,M2,N3,N2,M2,M1,N2,X19,S700,-1.000)
       deallocate(S700)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,M2,N3,N2,M2,M1,N2,X19,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder621345(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,N2,M2,N1,M2,N0,M1,N0,M1,t3B1,F2)
       allocate(Z19(N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K5*K9*K0
       I3=K6*K7
       call DMATMAT(I1,I2,I3,X19,F2,Z19)
       deallocate(F2)
C
       call
     & sum134625(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z19,-1.000)
       deallocate(Z19)
       deallocate(X19)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,M1,N1,M2,N3,
     & N0,N2,M1,N1,M2,N3,M1,N2,S696,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S697(N2+1:M2,M1+1:N1,M2+1:N3,M1+1:N2))
       I1=K8*K6*K7
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S697)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,M2,N3,N2,M2,M1,N2,X17,S697,-1.000)
       deallocate(S697)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,M2,N3,N2,M2,M1,N2,X17,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder621345(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,N2,M2,N1,M2,N0,M1,N0,M1,t3B1,F2)
       allocate(Z17(N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K5*K9*K0
       I3=K6*K7
       call DMATMAT(I1,I2,I3,X17,F2,Z17)
       deallocate(F2)
C
       call
     & sum234615(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z17, 1.000)
       deallocate(Z17)
       deallocate(X17)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,N2,M1,N1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S698(M1+1:N2,N0+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7*K2
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S698)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,M1,N1,N1,M2,
     & N0,N2,M1,N1,N1,M2,M1,N2,S698,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S701(N2+1:M2,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K7
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S701)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,N1,M2,N2,M2,M1,N2,X20,S701,-1.000)
       deallocate(S701)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,N1,M2,N2,M2,M1,N2,X20,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder621345(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,N1,M2,N2,M2,N1,M2,N0,M1,N0,M1,t3B1,F2)
       allocate(Z20(N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K5*K9*K0
       I3=K9*K7
       call DMATMAT(I1,I2,I3,X20,F2,Z20)
       deallocate(F2)
C
       call
     & sum134625(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z20,-1.000)
       deallocate(Z20)
       deallocate(X20)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,M1,N1,N1,M2,
     & N0,N2,M1,N1,N1,M2,M1,N2,S698,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S699(N2+1:M2,M1+1:N1,N1+1:M2,M1+1:N2))
       I1=K8*K9*K7
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S699)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,N1,M2,N2,M2,M1,N2,X18,S699,-1.000)
       deallocate(S699)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,N1,M2,N2,M2,M1,N2,X18,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder621345(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,N1,M2,N2,M2,N1,M2,N0,M1,N0,M1,t3B1,F2)
       allocate(Z18(N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K5*K9*K0
       I3=K9*K7
       call DMATMAT(I1,I2,I3,X18,F2,Z18)
       deallocate(F2)
C
       call
     & sum234615(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z18, 1.000)
       deallocate(Z18)
       deallocate(X18)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,M2+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,N2,N0,M1,M2,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S702(N0+1:M1,N0+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5*K2
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S702)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N0,M1,M2,N3,
     & N0,N2,N0,M1,M2,N3,N0,M1,S702,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S710(N2+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S710)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,N2,M2,N0,M1,X25,S710,-1.000)
       deallocate(S710)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,M2,N3,N2,M2,N0,M1,X25,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder521346(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & N0,M1,M2,N3,N2,M2,N1,M2,M1,N2,N0,M1,t3B4,F2)
       allocate(Z25(N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K5*K8*K9*K0
       I3=K6*K5
       call DMATMAT(I1,I2,I3,X25,F2,Z25)
       deallocate(F2)
C
       call
     & sum135624(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z25,-1.000)
       deallocate(Z25)
       deallocate(X25)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N0,M1,M2,N3,
     & N0,N2,N0,M1,M2,N3,N0,M1,S702,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S703(N2+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S703)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,N2,M2,N0,M1,X21,S703,-1.000)
       deallocate(S703)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,M2,N3,N2,M2,N0,M1,X21,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder521346(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & N0,M1,M2,N3,N2,M2,N1,M2,M1,N2,N0,M1,t3B4,F2)
       allocate(Z21(N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K5*K8*K9*K0
       I3=K6*K5
       call DMATMAT(I1,I2,I3,X21,F2,Z21)
       deallocate(F2)
C
       call
     & sum235614(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z21, 1.000)
       deallocate(Z21)
       deallocate(X21)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,N2,N0,M1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S706(N0+1:M1,N0+1:N2,N0+1:M1,N1+1:M2))
       I1=K9*K5*K2
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S706)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N0,M1,N1,M2,
     & N0,N2,N0,M1,N1,M2,N0,M1,S706,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S712(N2+1:M2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S712)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N1,M2,N2,M2,N0,M1,X27,S712,-1.000)
       deallocate(S712)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,N1,M2,N2,M2,N0,M1,X27,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder521346(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & N0,M1,N1,M2,N2,M2,N1,M2,M1,N2,N0,M1,t3B4,F2)
       allocate(Z27(N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K5*K8*K9*K0
       I3=K9*K5
       call DMATMAT(I1,I2,I3,X27,F2,Z27)
       deallocate(F2)
C
       call
     & sum135624(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z27,-1.000)
       deallocate(Z27)
       deallocate(X27)
C
       allocate(D1(N0+1:N2,N0+1:M1,N1+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N0,M1,N1,M2,
     & N0,N2,N0,M1,N1,M2,N0,M1,S706,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S707(N2+1:M2,N0+1:M1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K5
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S707)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N1,M2,N2,M2,N0,M1,X23,S707,-1.000)
       deallocate(S707)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,N1,M2,N2,M2,N0,M1,X23,VBHPPH, 1.000)
C
       allocate(F2(N0+1:M1,N1+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder521346(N2,N3,N1,N3,N1,M2,M1,N2,N0,M1,N0,M1,
     & N0,M1,N1,M2,N2,M2,N1,M2,M1,N2,N0,M1,t3B4,F2)
       allocate(Z23(N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K5*K8*K9*K0
       I3=K9*K5
       call DMATMAT(I1,I2,I3,X23,F2,Z23)
       deallocate(F2)
C
       call
     & sum235614(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z23, 1.000)
       deallocate(Z23)
       deallocate(X23)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N1,M2+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,N2,M1,N1,M2,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S704(N0+1:M1,N0+1:N2,M1+1:N1,M2+1:N3))
       I1=K6*K7*K2
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S704)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,M1,N1,M2,N3,
     & N0,N2,M1,N1,M2,N3,N0,M1,S704,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S711(N2+1:M2,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S711)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,M2,N3,N2,M2,N0,M1,X26,S711,-1.000)
       deallocate(S711)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,M2,N3,N2,M2,N0,M1,X26,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder621345(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,N2,M2,N1,M2,M1,N2,N0,M1,t3B1,F2)
       allocate(Z26(N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K5*K8*K9*K0
       I3=K6*K7
       call DMATMAT(I1,I2,I3,X26,F2,Z26)
       deallocate(F2)
C
       call
     & sum135624(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z26, 1.000)
       deallocate(Z26)
       deallocate(X26)
C
       allocate(D1(N0+1:N2,M1+1:N1,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,M1,N1,M2,N3,
     & N0,N2,M1,N1,M2,N3,N0,M1,S704,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S705(N2+1:M2,M1+1:N1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K7
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S705)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,M2,N3,N2,M2,N0,M1,X22,S705,-1.000)
       deallocate(S705)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,M2,N3,N2,M2,N0,M1,X22,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder621345(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,M2,N3,N2,M2,N1,M2,M1,N2,N0,M1,t3B1,F2)
       allocate(Z22(N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K5*K8*K9*K0
       I3=K6*K7
       call DMATMAT(I1,I2,I3,X22,F2,Z22)
       deallocate(F2)
C
       call
     & sum235614(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z22,-1.000)
       deallocate(Z22)
       deallocate(X22)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N1,N1+1:M2))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,N2,M1,N1,N1,M2,VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S708(N0+1:M1,N0+1:N2,M1+1:N1,N1+1:M2))
       I1=K9*K7*K2
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S708)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,M1,N1,N1,M2,
     & N0,N2,M1,N1,N1,M2,N0,M1,S708,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S713(N2+1:M2,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S713)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,N1,M2,N2,M2,N0,M1,X28,S713,-1.000)
       deallocate(S713)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,N1,M2,N2,M2,N0,M1,X28,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder621345(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,N1,M2,N2,M2,N1,M2,M1,N2,N0,M1,t3B1,F2)
       allocate(Z28(N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K5*K8*K9*K0
       I3=K9*K7
       call DMATMAT(I1,I2,I3,X28,F2,Z28)
       deallocate(F2)
C
       call
     & sum135624(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z28, 1.000)
       deallocate(Z28)
       deallocate(X28)
C
       allocate(D1(N0+1:N2,M1+1:N1,N1+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,M1,N1,N1,M2,
     & N0,N2,M1,N1,N1,M2,N0,M1,S708,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S709(N2+1:M2,M1+1:N1,N1+1:M2,N0+1:M1))
       I1=K5*K9*K7
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S709)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N1,N1,M2,N2,M2,N0,M1,X24,S709,-1.000)
       deallocate(S709)
C
       call sumx4231(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N1,N1,M2,N2,M2,N0,M1,X24,VBHPPH, 1.000)
C
       allocate(F2(M1+1:N1,N1+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder621345(N2,N3,N1,N3,N1,M2,N0,N2,N0,N1,M1,N1,
     & M1,N1,N1,M2,N2,M2,N1,M2,M1,N2,N0,M1,t3B1,F2)
       allocate(Z24(N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K5*K8*K9*K0
       I3=K9*K7
       call DMATMAT(I1,I2,I3,X24,F2,Z24)
       deallocate(F2)
C
       call
     & sum235614(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z24,-1.000)
       deallocate(Z24)
       deallocate(X24)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,N2+1:N3))
       call reorder2341(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,M1,M1,N2,N2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S714(M1+1:N2,N0+1:M1,M1+1:N2,N2+1:N3))
       I1=K4*K8*K5
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S714)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,N0+1:M1,M1+1:N2,M1+1:N2))
       call reorder4231(M1,N2,N0,M1,M1,N2,N2,N3,
     & N2,N3,N0,M1,M1,N2,M1,N2,S714,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S715(N0+1:M1,N0+1:M1,M1+1:N2,M1+1:N2))
       I1=K8*K8*K5
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S715)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M1,N2,N0,M1,M1,N2,X73,S715, 1.000)
       deallocate(S715)
C
       call sumx3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & N0,M1,M1,N2,N0,M1,M1,N2,X73,VCHHHH, 1.000)
C
       allocate(F2(N0+1:M1,M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder451236(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M1,N2,N2,M2,N2,M2,N1,M2,N0,M1,t3C1,F2)
       allocate(Z73(N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N2))
       I1=K8*K5
       I2=K5*K9*K0*K0
       I3=K8*K5
       call DMATMAT(I1,I2,I3,X73,F2,Z73)
       deallocate(F2)
C
       call
     & sum123645(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z73, 1.000)
       deallocate(Z73)
       deallocate(X73)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,N2+1:N3))
       call reorder2341(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,M1,N2,M1,N2,N2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S716(M1+1:N2,M1+1:N2,M1+1:N2,N2+1:N3))
       I1=K4*K8*K8
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S716)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N2+1:N3,M1+1:N2,M1+1:N2,M1+1:N2))
       call reorder4231(M1,N2,M1,N2,M1,N2,N2,N3,
     & N2,N3,M1,N2,M1,N2,M1,N2,S716,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S717(N0+1:M1,M1+1:N2,M1+1:N2,M1+1:N2))
       I1=K8*K8*K8
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S717)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M1,N2,N0,M1,M1,N2,X74,S717, 1.000)
       deallocate(S717)
C
       call sumx3412(N0,N2,N0,N2,N0,N2,N0,N2,
     & M1,N2,M1,N2,N0,M1,M1,N2,X74,VCHHHH, 1.000)
C
       allocate(F2(M1+1:N2,M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder451236(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M1,N2,N2,M2,N2,M2,N1,M2,N0,M1,t3C1,F2)
       allocate(Z74(N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N2))
       I1=K8*K5
       I2=K5*K9*K0*K0
       I3=K8*K8
       call DMATMAT(I1,I2,I3,X74,F2,Z74)
       deallocate(F2)
C
       call
     & sum123645(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z74, 0.500)
       deallocate(Z74)
       deallocate(X74)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N2,M2+1:N3))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,N2,M1,N2,M2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S718(M1+1:N2,N0+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8*K2
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S718)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,M1,N2,M2,N3,
     & N0,N2,M1,N2,M2,N3,M1,N2,S718,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S722(N2+1:M2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S722)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M2,N3,N2,M2,M1,N2,X77,S722, 1.000)
       deallocate(S722)
C
       call sumx2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,M2,N3,N2,M2,M1,N2,X77,VCHPHP, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder512346(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N2,M2,N1,M2,N0,M1,N0,M1,t3C1,F2)
       allocate(Z77(N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K5*K9*K0
       I3=K6*K8
       call DMATMAT(I1,I2,I3,X77,F2,Z77)
       deallocate(F2)
C
       call
     & sum134625(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z77, 1.000)
       deallocate(Z77)
       deallocate(X77)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,M1,N2,M2,N3,
     & N0,N2,M1,N2,M2,N3,M1,N2,S718,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S719(N2+1:M2,M1+1:N2,M2+1:N3,M1+1:N2))
       I1=K8*K6*K8
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S719)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M2,N3,N2,M2,M1,N2,X75,S719, 1.000)
       deallocate(S719)
C
       call sumx2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,M2,N3,N2,M2,M1,N2,X75,VCHPHP, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder512346(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N2,M2,N1,M2,N0,M1,N0,M1,t3C1,F2)
       allocate(Z75(N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K5*K9*K0
       I3=K6*K8
       call DMATMAT(I1,I2,I3,X75,F2,Z75)
       deallocate(F2)
C
       call
     & sum234615(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z75,-1.000)
       deallocate(Z75)
       deallocate(X75)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N2,N2+1:M2))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,N2,M1,N2,N2,M2,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S720(M1+1:N2,N0+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8*K2
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S720)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,M1,N2,N2,M2,
     & N0,N2,M1,N2,N2,M2,M1,N2,S720,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S723(N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K8
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S723)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,N2,M2,N2,M2,M1,N2,X78,S723, 1.000)
       deallocate(S723)
C
       call sumx2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,N2,M2,N2,M2,M1,N2,X78,VCHPHP, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder512346(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,N2,M2,N1,M2,N0,M1,N0,M1,t3C1,F2)
       allocate(Z78(N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K5*K9*K0
       I3=K0*K8
       call DMATMAT(I1,I2,I3,X78,F2,Z78)
       deallocate(F2)
C
       call
     & sum134625(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z78, 1.000)
       deallocate(Z78)
       deallocate(X78)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,M1,N2,N2,M2,
     & N0,N2,M1,N2,N2,M2,M1,N2,S720,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S721(N2+1:M2,M1+1:N2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K8
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S721)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,N2,M2,N2,M2,M1,N2,X76,S721, 1.000)
       deallocate(S721)
C
       call sumx2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,N2,M2,N2,M2,M1,N2,X76,VCHPHP, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder512346(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,N2,M2,N1,M2,N0,M1,N0,M1,t3C1,F2)
       allocate(Z76(N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,M1+1:N2))
       I1=K8*K0
       I2=K5*K5*K9*K0
       I3=K0*K8
       call DMATMAT(I1,I2,I3,X76,F2,Z76)
       deallocate(F2)
C
       call
     & sum234615(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z76,-1.000)
       deallocate(Z76)
       deallocate(X76)
C
       allocate(D1(N0+1:N2,N2+1:N3,M1+1:N2,N2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,M1,N2,N2,N3,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q83(M1+1:N2,N2+1:N3))
       I1=K4*K8
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q83)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,M1+1:N2))
       call reorder21(M1,N2,N2,N3,
     & N2,N3,M1,N2,Q83,B1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(Q87(N0+1:M1,M1+1:N2))
       I1=K8
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,B1,B2,Q87)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(M1,N2,N0,M1,X35,Q87, 1.000)
       deallocate(Q87)
C
       call sumx21(N0,N2,N0,N2,
     & M1,N2,N0,M1,X35,FBHH, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,N2,M2,N1,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z35(N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K8*K9*K0*K0
       I3=K8
       call DMATMAT(I1,I2,I3,X35,F2,Z35)
       deallocate(F2)
C
       call
     & sum123564(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z35,-1.000)
       deallocate(Z35)
       deallocate(X35)
C
       allocate(B1(N2+1:N3,M1+1:N2))
       call reorder21(M1,N2,N2,N3,
     & N2,N3,M1,N2,Q83,B1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(Q84(M1+1:N2,M1+1:N2))
       I1=K8
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,B1,B2,Q84)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(M1,N2,M1,N2,X33,Q84, 1.000)
       deallocate(Q84)
C
       call sumx21(N0,N2,N0,N2,
     & M1,N2,M1,N2,X33,FBHH, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder512346(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,N2,M2,N1,M2,N0,M1,N0,M1,t3C1,F2)
       allocate(Z33(N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,N0+1:M1,M1+1:N2))
       I1=K8
       I2=K5*K5*K9*K0*K0
       I3=K8
       call DMATMAT(I1,I2,I3,X33,F2,Z33)
       deallocate(F2)
C
       call
     & sum123465(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z33,-1.000)
       deallocate(Z33)
       deallocate(X33)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N2,M2+1:N3))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,N2,M1,N2,M2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S726(N0+1:M1,N0+1:N2,M1+1:N2,M2+1:N3))
       I1=K6*K8*K2
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S726)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,M1,N2,M2,N3,
     & N0,N2,M1,N2,M2,N3,N0,M1,S726,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S733(N2+1:M2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S733)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M2,N3,N2,M2,N0,M1,X84,S733, 1.000)
       deallocate(S733)
C
       call sumx2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,M2,N3,N2,M2,N0,M1,X84,VCHPHP, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N2,M2,N1,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z84(N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K5*K8*K9*K0
       I3=K6*K8
       call DMATMAT(I1,I2,I3,X84,F2,Z84)
       deallocate(F2)
C
       call
     & sum135624(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z84, 1.000)
       deallocate(Z84)
       deallocate(X84)
C
       allocate(D1(N0+1:N2,M1+1:N2,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,M1,N2,M2,N3,
     & N0,N2,M1,N2,M2,N3,N0,M1,S726,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S727(N2+1:M2,M1+1:N2,M2+1:N3,N0+1:M1))
       I1=K5*K6*K8
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S727)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,M2,N3,N2,M2,N0,M1,X80,S727, 1.000)
       deallocate(S727)
C
       call sumx2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,M2,N3,N2,M2,N0,M1,X80,VCHPHP, 1.000)
C
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,M2,N3,N2,M2,N1,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z80(N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K5*K8*K9*K0
       I3=K6*K8
       call DMATMAT(I1,I2,I3,X80,F2,Z80)
       deallocate(F2)
C
       call
     & sum235614(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z80,-1.000)
       deallocate(Z80)
       deallocate(X80)
C
       allocate(D1(N2+1:N3,N0+1:N2,M1+1:N2,N2+1:M2))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,N2,M1,N2,N2,M2,VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S730(N0+1:M1,N0+1:N2,M1+1:N2,N2+1:M2))
       I1=K0*K8*K2
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S730)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,M1,N2,N2,M2,
     & N0,N2,M1,N2,N2,M2,N0,M1,S730,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S735(N2+1:M2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S735)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,N2,M2,N2,M2,N0,M1,X86,S735, 1.000)
       deallocate(S735)
C
       call sumx2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,N2,M2,N2,M2,N0,M1,X86,VCHPHP, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,N2,M2,N1,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z86(N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K5*K8*K9*K0
       I3=K0*K8
       call DMATMAT(I1,I2,I3,X86,F2,Z86)
       deallocate(F2)
C
       call
     & sum135624(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z86, 1.000)
       deallocate(Z86)
       deallocate(X86)
C
       allocate(D1(N0+1:N2,M1+1:N2,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,M1,N2,N2,M2,
     & N0,N2,M1,N2,N2,M2,N0,M1,S730,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S731(N2+1:M2,M1+1:N2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K8
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S731)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M1,N2,N2,M2,N2,M2,N0,M1,X82,S731, 1.000)
       deallocate(S731)
C
       call sumx2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & M1,N2,N2,M2,N2,M2,N0,M1,X82,VCHPHP, 1.000)
C
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M1,N2,N2,M2,N2,M2,N1,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z82(N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K5*K8*K9*K0
       I3=K0*K8
       call DMATMAT(I1,I2,I3,X82,F2,Z82)
       deallocate(F2)
C
       call
     & sum235614(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z82,-1.000)
       deallocate(Z82)
       deallocate(X82)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,N2+1:M2))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,N2,N0,M1,N2,M2,VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S728(N0+1:M1,N0+1:N2,N0+1:M1,N2+1:M2))
       I1=K0*K5*K2
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S728)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N0,M1,N2,M2,
     & N0,N2,N0,M1,N2,M2,N0,M1,S728,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S734(N2+1:M2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S734)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N2,M2,N2,M2,N0,M1,X85,S734, 1.000)
       deallocate(S734)
C
       call sumx2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,M1,N2,M2,N2,M2,N0,M1,X85,VCHPHP, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N2,M2,N1,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z85(N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K5*K8*K9*K0
       I3=K0*K5
       call DMATMAT(I1,I2,I3,X85,F2,Z85)
       deallocate(F2)
C
       call
     & sum135624(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z85, 1.000)
       deallocate(Z85)
       deallocate(X85)
C
       allocate(D1(N0+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N0,M1,N2,M2,
     & N0,N2,N0,M1,N2,M2,N0,M1,S728,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S729(N2+1:M2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K5
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S729)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,N2,M2,N2,M2,N0,M1,X81,S729, 1.000)
       deallocate(S729)
C
       call sumx2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,M1,N2,M2,N2,M2,N0,M1,X81,VCHPHP, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N2,M2,N1,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z81(N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K5*K8*K9*K0
       I3=K0*K5
       call DMATMAT(I1,I2,I3,X81,F2,Z81)
       deallocate(F2)
C
       call
     & sum235614(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z81,-1.000)
       deallocate(Z81)
       deallocate(X81)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:M1,M2+1:N3))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,N2,N0,M1,M2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S724(N0+1:M1,N0+1:N2,N0+1:M1,M2+1:N3))
       I1=K6*K5*K2
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S724)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N0,M1,M2,N3,
     & N0,N2,N0,M1,M2,N3,N0,M1,S724,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S725(N2+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S725)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,N2,M2,N0,M1,X79,S725, 1.000)
       deallocate(S725)
C
       call sumx2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,M1,M2,N3,N2,M2,N0,M1,X79,VCHPHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N2,M2,N1,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z79(N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K5*K8*K9*K0
       I3=K6*K5
       call DMATMAT(I1,I2,I3,X79,F2,Z79)
       deallocate(F2)
C
       call
     & sum235614(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z79,-1.000)
       deallocate(Z79)
       deallocate(X79)
C
       allocate(D1(N0+1:N2,N0+1:M1,M2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N0,M1,M2,N3,
     & N0,N2,N0,M1,M2,N3,N0,M1,S724,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S732(N2+1:M2,N0+1:M1,M2+1:N3,N0+1:M1))
       I1=K5*K6*K5
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S732)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,M1,M2,N3,N2,M2,N0,M1,X83,S732, 1.000)
       deallocate(S732)
C
       call sumx2431(N2,N3,N0,N2,N2,N3,N0,N2,
     & N0,M1,M2,N3,N2,M2,N0,M1,X83,VCHPHP, 1.000)
C
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,M2,N3,N2,M2,N1,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z83(N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,N2+1:M2,N0+1:M1))
       I1=K5*K0
       I2=K5*K8*K9*K0
       I3=K6*K5
       call DMATMAT(I1,I2,I3,X83,F2,Z83)
       deallocate(F2)
C
       call
     & sum135624(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z83, 1.000)
       deallocate(Z83)
       deallocate(X83)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,M1,N2,N3,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q85(N0+1:M1,N2+1:N3))
       I1=K4*K5
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q85)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:M1))
       call reorder21(N0,M1,N2,N3,
     & N2,N3,N0,M1,Q85,B1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(Q86(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,B1,B2,Q86)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(N0,M1,N0,M1,X34,Q86, 1.000)
       deallocate(Q86)
C
       call sumx21(N0,N2,N0,N2,
     & N0,M1,N0,M1,X34,FBHH, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder412356(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N2,M2,N1,M2,M1,N2,N0,M1,t3C1,F2)
       allocate(Z34(N2+1:M2,N2+1:M2,N1+1:M2,M1+1:N2,N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5*K8*K9*K0*K0
       I3=K5
       call DMATMAT(I1,I2,I3,X34,F2,Z34)
       deallocate(F2)
C
       call
     & sum123564(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z34,-1.000)
       deallocate(Z34)
       deallocate(X34)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,M2+1:N3))
       call reorder4312(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,M2,N3,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S736(N2+1:M2,N0+1:N2,M2+1:N3,M2+1:N3))
       I1=K6*K6*K2
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S736)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M2+1:N3,M2+1:N3,N2+1:M2))
       call reorder2341(N2,M2,N0,N2,M2,N3,M2,N3,
     & N0,N2,M2,N3,M2,N3,N2,M2,S736,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S737(N2+1:M2,M2+1:N3,M2+1:N3,N2+1:M2))
       I1=K0*K6*K6
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S737)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,M2,N3,N2,M2,N2,M2,X87,S737, 1.000)
       deallocate(S737)
C
       call sumx1234(N2,N3,N2,N3,N2,N3,N2,M2,
     & M2,N3,M2,N3,N2,M2,N2,M2,X87,VCAPPP, 1.000)
C
       allocate(F2(M2+1:N3,M2+1:N3,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder123456(M2,N3,M2,N3,N1,M2,N0,N2,M1,N2,N0,N1,
     & M2,N3,M2,N3,N1,M2,N0,M1,M1,N2,N0,M1,t3C3,F2)
       allocate(Z87(N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1,N2+1:M2,N2+1:M2))
       I1=K0*K0
       I2=K5*K8*K5*K9
       I3=K6*K6
       call DMATMAT(I1,I2,I3,X87,F2,Z87)
       deallocate(F2)
C
       call
     & sum345612(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z87, 0.500)
       deallocate(Z87)
       deallocate(X87)
C
       allocate(D1(N0+1:N2,N0+1:N2,M2+1:N3,N2+1:M2))
       call reorder4312(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,M2,N3,N2,M2,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S738(N2+1:M2,N0+1:N2,M2+1:N3,N2+1:M2))
       I1=K0*K6*K2
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S738)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,M2+1:N3,N2+1:M2,N2+1:M2))
       call reorder2341(N2,M2,N0,N2,M2,N3,N2,M2,
     & N0,N2,M2,N3,N2,M2,N2,M2,S738,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S739(N2+1:M2,M2+1:N3,N2+1:M2,N2+1:M2))
       I1=K0*K0*K6
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S739)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(M2,N3,N2,M2,N2,M2,N2,M2,X88,S739, 1.000)
       deallocate(S739)
C
       call sumx1234(N2,N3,N2,N3,N2,N3,N2,M2,
     & M2,N3,N2,M2,N2,M2,N2,M2,X88,VCAPPP, 1.000)
C
       allocate(F2(M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder123456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M2,N3,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,t3C1,F2)
       allocate(Z88(N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1,N2+1:M2,N2+1:M2))
       I1=K0*K0
       I2=K5*K8*K5*K9
       I3=K0*K6
       call DMATMAT(I1,I2,I3,X88,F2,Z88)
       deallocate(F2)
C
       call
     & sum345612(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z88, 1.000)
       deallocate(Z88)
       deallocate(X88)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder4312(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,M2,N2,M2,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S740(N2+1:M2,N0+1:N2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K2
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S740)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:M2,N2+1:M2,N2+1:M2))
       call reorder2341(N2,M2,N0,N2,N2,M2,N2,M2,
     & N0,N2,N2,M2,N2,M2,N2,M2,S740,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S741(N2+1:M2,N2+1:M2,N2+1:M2,N2+1:M2))
       I1=K0*K0*K0
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S741)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,M2,N2,M2,N2,M2,N2,M2,X89,S741, 1.000)
       deallocate(S741)
C
       call sumx1234(N2,N3,N2,N3,N2,N3,N2,M2,
     & N2,M2,N2,M2,N2,M2,N2,M2,X89,VCAPPP, 1.000)
C
       allocate(F2(N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder123456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,t3C1,F2)
       allocate(Z89(N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1,N2+1:M2,N2+1:M2))
       I1=K0*K0
       I2=K5*K8*K5*K9
       I3=K0*K0
       call DMATMAT(I1,I2,I3,X89,F2,Z89)
       deallocate(F2)
C
       call
     & sum345612(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z89, 0.500)
       deallocate(Z89)
       deallocate(X89)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,M2,N3,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q88(N0+1:N2,M2+1:N3))
       I1=K6*K2
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q88)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(Q92(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,Q88,B2,Q92)
       deallocate(B2)
C
       call
     & sum21(M2,N3,N2,M2,X38,Q92,-1.000)
       deallocate(Q92)
C
       call sumx12(N2,N3,N2,N3,
     & M2,N3,N2,M2,X38,FBPP, 1.000)
C
       allocate(F2(M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder123456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M2,N3,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,t3C1,F2)
       allocate(Z38(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0
       I2=K5*K8*K5*K9*K0
       I3=K6
       call DMATMAT(I1,I2,I3,X38,F2,Z38)
       deallocate(F2)
C
       call
     & sum134562(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z38,-1.000)
       deallocate(Z38)
       deallocate(X38)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(Q89(N2+1:M2,M2+1:N3))
       I1=K6
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,Q88,B2,Q89)
       deallocate(B2)
C
       call
     & sum21(M2,N3,N2,M2,X36,Q89,-1.000)
       deallocate(Q89)
C
       call sumx12(N2,N3,N2,N3,
     & M2,N3,N2,M2,X36,FBPP, 1.000)
C
       allocate(F2(M2+1:N3,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder123456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & M2,N3,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,t3C1,F2)
       allocate(Z36(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0
       I2=K5*K8*K5*K9*K0
       I3=K6
       call DMATMAT(I1,I2,I3,X36,F2,Z36)
       deallocate(F2)
C
       call
     & sum234561(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z36, 1.000)
       deallocate(Z36)
       deallocate(X36)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:M2))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N2,M2,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q90(N0+1:N2,N2+1:M2))
       I1=K0*K2
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q90)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(Q93(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,Q90,B2,Q93)
       deallocate(B2)
C
       call
     & sum21(N2,M2,N2,M2,X39,Q93,-1.000)
       deallocate(Q93)
C
       call sumx12(N2,N3,N2,N3,
     & N2,M2,N2,M2,X39,FBPP, 1.000)
C
       allocate(F2(N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder123456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,t3C1,F2)
       allocate(Z39(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0
       I2=K5*K8*K5*K9*K0
       I3=K0
       call DMATMAT(I1,I2,I3,X39,F2,Z39)
       deallocate(F2)
C
       call
     & sum134562(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z39,-1.000)
       deallocate(Z39)
       deallocate(X39)
C
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(Q91(N2+1:M2,N2+1:M2))
       I1=K0
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,Q90,B2,Q91)
       deallocate(B2)
C
       call
     & sum21(N2,M2,N2,M2,X37,Q91,-1.000)
       deallocate(Q91)
C
       call sumx12(N2,N3,N2,N3,
     & N2,M2,N2,M2,X37,FBPP, 1.000)
C
       allocate(F2(N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       call reorder123456(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,t3C1,F2)
       allocate(Z37(N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1,N2+1:M2))
       I1=K0
       I2=K5*K8*K5*K9*K0
       I3=K0
       call DMATMAT(I1,I2,I3,X37,F2,Z37)
       deallocate(F2)
C
       call
     & sum234561(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z37, 1.000)
       deallocate(Z37)
       deallocate(X37)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,N2,N0,N1,N1,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S742(M1+1:N2,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1*K2
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S742)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,N1,N1,N3,
     & N0,N2,N0,N1,N1,N3,M1,N2,S742,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,N1,M2,t2B,D2)
       allocate(S749(N2+1:M2,N1+1:M2,N1+1:N3,M1+1:N2))
       I1=K8*K3
       I2=K9*K0
       I3=K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,S749)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N1,N3,N2,M2,N1,M2,M1,N2,X6,S749, 1.000)
       deallocate(S749)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,N1,N1,N3,
     & N0,N2,N0,N1,N1,N3,M1,N2,S742,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,N1,M2,t2B,D2)
       allocate(S750(N2+1:M2,N1+1:M2,N1+1:N3,M1+1:N2))
       I1=K8*K3
       I2=K9*K0
       I3=K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,S750)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N1,N3,N2,M2,N1,M2,M1,N2,X5,S750, 1.000)
       deallocate(S750)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder3421(M1,N2,N0,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N2,M1,N2,S742,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S751(N2+1:M2,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S751)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,N2,M2,N0,M1,M1,N2,X10,S751, 1.000)
       deallocate(S751)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder3421(M1,N2,N0,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N2,M1,N2,S742,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S753(N2+1:M2,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S753)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,N2,M2,N0,M1,M1,N2,X9,S753, 1.000)
       deallocate(S753)
C
       allocate(D1(N0+1:N1,N0+1:N2,N1+1:N3,M1+1:N2))
       call reorder3241(M1,N2,N0,N2,N0,N1,N1,N3,
     & N0,N1,N0,N2,N1,N3,M1,N2,S742,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S822(N1+1:M2,N0+1:N2,N1+1:N3,M1+1:N2))
       I1=K8*K3*K2
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S822)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,M1+1:N2))
       call reorder2314(N1,M2,N0,N2,N1,N3,M1,N2,
     & N0,N2,N1,N3,N1,M2,M1,N2,S822,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S823(N2+1:M2,N1+1:N3,N1+1:M2,M1+1:N2))
       I1=K8*K9*K3
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S823)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,N2,M2,N1,M2,M1,N2,X5,S823, 1.000)
       deallocate(S823)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,M1+1:N2))
       call reorder3421(M1,N2,N0,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N2,M1,N2,S742,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S743(N1+1:M2,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K9
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S743)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N2,N1,M2,M1,N2,N0,M1,X13,S743, 1.000)
       deallocate(S743)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,M1+1:N2))
       call reorder2314(N1,M2,N0,N2,N1,N3,M1,N2,
     & N0,N2,N1,N3,N1,M2,M1,N2,S822,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S824(N2+1:M2,N1+1:N3,N1+1:M2,M1+1:N2))
       I1=K8*K9*K3
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S824)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,N2,M2,N1,M2,M1,N2,X6,S824, 1.000)
       deallocate(S824)
C
       allocate(D1(N2+1:N3,N0+1:N2,N1+1:N3,N1+1:M2))
       call reorder1324(N2,N3,N1,N3,N0,N2,N1,N3,
     & N2,N3,N0,N2,N1,N3,N1,M2,VBPHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S670(M1+1:N2,N0+1:N2,N1+1:N3,N1+1:M2))
       I1=K9*K3*K2
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S670)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N1,N3,N1,M2,
     & N0,N2,N1,N3,N1,M2,M1,N2,S670,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S672(N2+1:M2,N1+1:N3,N1+1:M2,M1+1:N2))
       I1=K8*K9*K3
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S672)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,N2,M2,N1,M2,M1,N2,X6,S672,-1.000)
       deallocate(S672)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N1,N3,N1,M2,
     & N0,N2,N1,N3,N1,M2,M1,N2,S670,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S671(N2+1:M2,N1+1:N3,N1+1:M2,M1+1:N2))
       I1=K8*K9*K3
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S671)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,N2,M2,N1,M2,M1,N2,X5,S671,-1.000)
       deallocate(S671)
C
       allocate(D1(N0+1:N2,N1+1:N3,N0+1:N1,N2+1:N3))
       call reorder3241(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N0,N1,N2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder3214(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S663(N2+1:M2,N0+1:M1,N0+1:N1,N2+1:N3))
       I1=K4*K1
       I2=K5*K0
       I3=K3*K2
       call DMATMAT(I1,I2,I3,D1,D2,S663)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3412(N2,M2,N0,M1,N0,N1,N2,N3,
     & N0,N1,N2,N3,N2,M2,N0,M1,S663,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S664(N1+1:M2,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S664)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,N3,N2,M2,N1,M2,N0,M1,X16,S664, 1.000)
       deallocate(S664)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:M2,N0+1:M1))
       call reorder4312(N2,M2,N0,M1,N0,N1,N2,N3,
     & N2,N3,N0,N1,N2,M2,N0,M1,S663,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S771(N0+1:M1,N0+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K1
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S771)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N1,N2,M2,N0,M1,N0,M1,X4,S771,-1.000)
       deallocate(S771)
C
       allocate(D1(N2+1:N3,N0+1:N1,N2+1:M2,N0+1:M1))
       call reorder4312(N2,M2,N0,M1,N0,N1,N2,N3,
     & N2,N3,N0,N1,N2,M2,N0,M1,S663,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S754(M1+1:N2,N0+1:N1,N2+1:M2,N0+1:M1))
       I1=K5*K0*K1
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S754)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N0,N1,N2,M2,M1,N2,N0,M1,X2,S754,-1.000)
       deallocate(S754)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N1+1:N3))
       call reorder4132(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N0,N2,N1,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S757(N1+1:M2,N0+1:M1,N0+1:N2,N1+1:N3))
       I1=K3*K2
       I2=K5*K9
       I3=K4*K1
       call DMATMAT(I1,I2,I3,D1,D2,S757)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3412(N1,M2,N0,M1,N0,N2,N1,N3,
     & N0,N2,N1,N3,N1,M2,N0,M1,S757,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S762(N2+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S762)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,N2,M2,N1,M2,N0,M1,X8,S762, 1.000)
       deallocate(S762)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3412(N1,M2,N0,M1,N0,N2,N1,N3,
     & N0,N2,N1,N3,N1,M2,N0,M1,S757,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S758(N2+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S758)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,N2,M2,N1,M2,N0,M1,X7,S758, 1.000)
       deallocate(S758)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N2,N0,N1,VBHHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,M1+1:N2,N0+1:M1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,M1,N2,N0,M1,t2B,D2)
       allocate(S650(M1+1:N2,N0+1:M1,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K5*K8
       I3=K3*K4
       call DMATMAT(I1,I2,I3,D1,D2,S650)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,N0+1:M1))
       call reorder3412(M1,N2,N0,M1,N0,N2,N0,N1,
     & N0,N2,N0,N1,M1,N2,N0,M1,S650,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S766(N2+1:M2,N0+1:N1,M1+1:N2,N0+1:M1))
       I1=K5*K8*K1
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S766)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N2,M2,M1,N2,N0,M1,X2,S766,-1.000)
       deallocate(S766)
C
       allocate(D1(N0+1:N1,N0+1:N2,M1+1:N2,N0+1:M1))
       call reorder4312(M1,N2,N0,M1,N0,N2,N0,N1,
     & N0,N1,N0,N2,M1,N2,N0,M1,S650,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S651(N1+1:M2,N0+1:N2,M1+1:N2,N0+1:M1))
       I1=K5*K8*K2
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S651)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,N1,M2,M1,N2,N0,M1,X13,S651,-1.000)
       deallocate(S651)
C
       allocate(D1(N0+1:N2,N0+1:N1,M1+1:N2,N0+1:M1))
       call reorder3412(M1,N2,N0,M1,N0,N2,N0,N1,
     & N0,N2,N0,N1,M1,N2,N0,M1,S650,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S761(N2+1:M2,N0+1:N1,M1+1:N2,N0+1:M1))
       I1=K5*K8*K1
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S761)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N2,M2,M1,N2,N0,M1,X1,S761,-1.000)
       deallocate(S761)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S764(N2+1:M2,N0+1:M1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K5*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S764)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3412(N2,M2,N0,M1,N0,N2,N2,N3,
     & N0,N2,N2,N3,N2,M2,N0,M1,S764,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S765(N2+1:M2,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S765)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,N3,N2,M2,N2,M2,N0,M1,X12,S765, 1.000)
       deallocate(S765)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,N0,M1,t2B,D2)
       allocate(S759(N2+1:M2,N0+1:M1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K5*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S759)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3412(N2,M2,N0,M1,N0,N2,N2,N3,
     & N0,N2,N2,N3,N2,M2,N0,M1,S759,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S760(N2+1:M2,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S760)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,N2,M2,N0,M1,X12,S760,-1.000)
       deallocate(S760)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N1,N3,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q94(N0+1:N1,N1+1:N3))
       I1=K3*K1
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q94)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q94,B1)
       allocate(D2(N1+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,M2,N0,M1,N0,M1,t2B,D2)
       allocate(S778(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K0
       I3=K3
       call DMATMAT(I1,I2,I3,B1,D2,S778)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,N2,M2,N0,M1,N0,M1,X3,S778, 1.000)
       deallocate(S778)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q94,B1)
       allocate(D2(N1+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,M2,N0,M1,N0,M1,t2B,D2)
       allocate(S782(N2+1:M2,N0+1:M1,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K5*K0
       I3=K3
       call DMATMAT(I1,I2,I3,B1,D2,S782)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,N2,M2,N0,M1,N0,M1,X4,S782, 1.000)
       deallocate(S782)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q94,B1)
       allocate(D2(N1+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,M2,M1,N2,N0,M1,t2B,D2)
       allocate(S763(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K3
       call DMATMAT(I1,I2,I3,B1,D2,S763)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X1,S763, 1.000)
       deallocate(S763)
C
       call sumx2134(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N2,M2,M1,N2,N0,M1,X1,VBHHHP, 1.000)
C
       allocate(D2(N0+1:N1,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,M2,N1,M2,N0,M1,t2B,D2)
       allocate(Z1(N2+1:M2,N1+1:M2,N0+1:M1,N2+1:M2,M1+1:N2,N0+1:M1))
       I1=K5*K8*K0
       I2=K5*K9*K0
       I3=K1
       call DMATMAT(I1,I2,I3,X1,D2,Z1)
       deallocate(D2)
C
       call
     & sum234156(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z1, 1.000)
       deallocate(Z1)
       deallocate(X1)
C
       allocate(B1(N1+1:N3,N0+1:N1))
       call reorder21(N0,N1,N1,N3,
     & N1,N3,N0,N1,Q94,B1)
       allocate(D2(N1+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,M2,M1,N2,N0,M1,t2B,D2)
       allocate(S767(N2+1:M2,M1+1:N2,N0+1:M1,N0+1:N1))
       I1=K1
       I2=K5*K8*K0
       I3=K3
       call DMATMAT(I1,I2,I3,B1,D2,S767)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N1,N2,M2,M1,N2,N0,M1,X2,S767, 1.000)
       deallocate(S767)
C
       call sumx2134(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N2,M2,M1,N2,N0,M1,X2,VBHHHP, 1.000)
C
       allocate(D2(N0+1:N1,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,M2,N1,M2,N0,M1,t2B,D2)
       allocate(Z2(N2+1:M2,N1+1:M2,N0+1:M1,N2+1:M2,M1+1:N2,N0+1:M1))
       I1=K5*K8*K0
       I2=K5*K9*K0
       I3=K1
       call DMATMAT(I1,I2,I3,X2,D2,Z2)
       deallocate(D2)
C
       call
     & sum134256(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z2,-1.000)
       deallocate(Z2)
       deallocate(X2)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N1,N1+1:N3))
       call reorder1342(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N0,N2,N0,N1,N1,N3,VBHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S744(N0+1:M1,N0+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1*K2
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S744)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:M1))
       call reorder3421(N0,M1,N0,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N2,N0,M1,S744,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S768(N2+1:M2,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S768)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N2,N2,M2,N0,M1,M1,N2,X10,S768,-1.000)
       deallocate(S768)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N0,N1,N1,N3,
     & N0,N2,N0,N1,N1,N3,N0,M1,S744,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,N1,M2,t2B,D2)
       allocate(S755(N2+1:M2,N1+1:M2,N1+1:N3,N0+1:M1))
       I1=K5*K3
       I2=K9*K0
       I3=K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,S755)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N1,N3,N2,M2,N1,M2,N0,M1,X8,S755, 1.000)
       deallocate(S755)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:M1))
       call reorder3421(N0,M1,N0,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N2,N0,M1,S744,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S770(N2+1:M2,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S770)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N2,N2,M2,N0,M1,M1,N2,X9,S770,-1.000)
       deallocate(S770)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N0+1:M1))
       call reorder3421(N0,M1,N0,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N0,N2,N0,M1,S744,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S745(N1+1:M2,N0+1:M1,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K5*K9
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S745)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N2,N1,M2,N0,M1,N0,M1,X14,S745, 1.000)
       deallocate(S745)
C
       allocate(D1(N0+1:N1,N0+1:N2,N1+1:N3,N0+1:M1))
       call reorder3241(N0,M1,N0,N2,N0,N1,N1,N3,
     & N0,N1,N0,N2,N1,N3,N0,M1,S744,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S825(N1+1:M2,N0+1:N2,N1+1:N3,N0+1:M1))
       I1=K5*K3*K2
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S825)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder2314(N1,M2,N0,N2,N1,N3,N0,M1,
     & N0,N2,N1,N3,N1,M2,N0,M1,S825,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S826(N2+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S826)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,N2,M2,N1,M2,N0,M1,X7,S826, 1.000)
       deallocate(S826)
C
       allocate(D1(N0+1:N2,N0+1:N1,N1+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N0,N1,N1,N3,
     & N0,N2,N0,N1,N1,N3,N0,M1,S744,D1)
       allocate(D2(N0+1:N2,N0+1:N1,N2+1:M2,N1+1:M2))
       call reorder3412(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N0,N1,N2,M2,N1,M2,t2B,D2)
       allocate(S756(N2+1:M2,N1+1:M2,N1+1:N3,N0+1:M1))
       I1=K5*K3
       I2=K9*K0
       I3=K1*K2
       call DMATMAT(I1,I2,I3,D1,D2,S756)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N1,N3,N2,M2,N1,M2,N0,M1,X7,S756, 1.000)
       deallocate(S756)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder2314(N1,M2,N0,N2,N1,N3,N0,M1,
     & N0,N2,N1,N3,N1,M2,N0,M1,S825,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S827(N2+1:M2,N1+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K3
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S827)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,N2,M2,N1,M2,N0,M1,X8,S827, 1.000)
       deallocate(S827)
C
       allocate(D1(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,N2,N0,N1,VBHHPP,D1)
       allocate(D2(N2+1:N3,N1+1:N3,N0+1:M1,N0+1:M1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,N3,N0,M1,N0,M1,t2B,D2)
       allocate(S657(N0+1:M1,N0+1:M1,N0+1:N2,N0+1:N1))
       I1=K1*K2
       I2=K5*K5
       I3=K3*K4
       call DMATMAT(I1,I2,I3,D1,D2,S657)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:M1,N0+1:M1))
       call reorder3412(N0,M1,N0,M1,N0,N2,N0,N1,
     & N0,N2,N0,N1,N0,M1,N0,M1,S657,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S776(N2+1:M2,N0+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K1
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S776)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N2,M2,N0,M1,N0,M1,X3,S776,-1.000)
       deallocate(S776)
C
       call sumx2134(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N2,M2,N0,M1,N0,M1,X3,VBHHHP, 1.000)
C
       allocate(D2(N0+1:N1,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,M2,N1,M2,M1,N2,t2B,D2)
       allocate(Z3(N2+1:M2,N1+1:M2,M1+1:N2,N2+1:M2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K0
       I2=K8*K9*K0
       I3=K1
       call DMATMAT(I1,I2,I3,X3,D2,Z3)
       deallocate(D2)
C
       call
     & sum235146(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z3,-1.000)
       deallocate(Z3)
       deallocate(X3)
C
       allocate(D1(N0+1:N1,N0+1:N2,N0+1:M1,N0+1:M1))
       call reorder4312(N0,M1,N0,M1,N0,N2,N0,N1,
     & N0,N1,N0,N2,N0,M1,N0,M1,S657,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S658(N1+1:M2,N0+1:N2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K2
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S658)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,N1,M2,N0,M1,N0,M1,X14,S658,-1.000)
       deallocate(S658)
C
       allocate(D1(N0+1:N2,N0+1:N1,N0+1:M1,N0+1:M1))
       call reorder3412(N0,M1,N0,M1,N0,N2,N0,N1,
     & N0,N2,N0,N1,N0,M1,N0,M1,S657,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S781(N2+1:M2,N0+1:N1,N0+1:M1,N0+1:M1))
       I1=K5*K5*K1
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S781)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N1,N2,M2,N0,M1,N0,M1,X4,S781,-1.000)
       deallocate(S781)
C
       call sumx2134(N2,N3,N0,N1,N0,N2,N0,N1,
     & N0,N1,N2,M2,N0,M1,N0,M1,X4,VBHHHP, 1.000)
C
       allocate(D2(N0+1:N1,N2+1:M2,N1+1:M2,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,M2,N1,M2,M1,N2,t2B,D2)
       allocate(Z4(N2+1:M2,N1+1:M2,M1+1:N2,N2+1:M2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K0
       I2=K8*K9*K0
       I3=K1
       call DMATMAT(I1,I2,I3,X4,D2,Z4)
       deallocate(D2)
C
       call
     & sum135246(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z4, 1.000)
       deallocate(Z4)
       deallocate(X4)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,N2,N0,N2,N2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,M1+1:N2))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,M1,N2,t1B,B2)
       allocate(S783(M1+1:N2,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2*K2
       I2=K8
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S783)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder2431(M1,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N0,N2,M1,N2,S783,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S786(N2+1:M2,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S786)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,N2,M2,N0,M1,M1,N2,X9,S786,-1.000)
       deallocate(S786)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder3421(M1,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N0,N2,M1,N2,S783,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S787(N1+1:M2,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K9
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S787)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N2,N1,M2,M1,N2,N0,M1,X13,S787, 1.000)
       deallocate(S787)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder2341(M1,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,M1,N2,S783,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,M2,N2,M2,t2C,D2)
       allocate(S784(N2+1:M2,N2+1:M2,N2+1:N3,M1+1:N2))
       I1=K8*K4
       I2=K0*K0
       I3=K2*K2
       call DMATMAT(I1,I2,I3,D1,D2,S784)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N2,N3,N2,M2,N2,M2,M1,N2,X11,S784,-0.500)
       deallocate(S784)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,M1+1:N2))
       call reorder4231(M1,N2,N0,N2,N0,N2,N2,N3,
     & N2,N3,N0,N2,N0,N2,M1,N2,S783,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S828(N0+1:M1,N0+1:N2,N0+1:N2,M1+1:N2))
       I1=K8*K2*K2
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S828)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder2314(N0,M1,N0,N2,N0,N2,M1,N2,
     & N0,N2,N0,N2,N0,M1,M1,N2,S828,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S829(N2+1:M2,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S829)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,N2,M2,N0,M1,M1,N2,X9,S829, 1.000)
       deallocate(S829)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,M1+1:N2))
       call reorder3241(M1,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,M1,N2,S783,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S831(N2+1:M2,N0+1:N2,N2+1:N3,M1+1:N2))
       I1=K8*K4*K2
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S831)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder2314(N2,M2,N0,N2,N2,N3,M1,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,S831,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S832(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S832)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,N2,M2,M1,N2,X11,S832,-1.000)
       deallocate(S832)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,M1+1:N2))
       call reorder2431(M1,N2,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N0,N2,M1,N2,S783,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S785(N2+1:M2,N0+1:M1,N0+1:N2,M1+1:N2))
       I1=K8*K2
       I2=K5*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S785)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N0,N2,N2,M2,N0,M1,M1,N2,X10,S785,-1.000)
       deallocate(S785)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder2314(N0,M1,N0,N2,N0,N2,M1,N2,
     & N0,N2,N0,N2,N0,M1,M1,N2,S828,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S830(N2+1:M2,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S830)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,N2,M2,N0,M1,M1,N2,X10,S830, 1.000)
       deallocate(S830)
C
       allocate(D1(N2+1:N3,N0+1:N2,N0+1:N2,N2+1:N3))
       call reorder1342(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N0,N2,N0,N2,N2,N3,VCHHPP,D1)
       allocate(B2(N2+1:N3,N0+1:M1))
       call reorder12(N2,N3,N0,N2,
     & N2,N3,N0,M1,t1B,B2)
       allocate(S788(N0+1:M1,N0+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2*K2
       I2=K5
       I3=K4
       call DMATMAT(I1,I2,I3,D1,B2,S788)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:M1))
       call reorder2431(N0,M1,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N0,N2,N0,M1,S788,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S796(N2+1:M2,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S796)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N2,N2,M2,N0,M1,M1,N2,X9,S796, 1.000)
       deallocate(S796)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:M1))
       call reorder3421(N0,M1,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N0,N2,N0,M1,S788,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S797(N1+1:M2,N0+1:M1,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K5*K9
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S797)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N2,N1,M2,N0,M1,N0,M1,X14,S797, 1.000)
       deallocate(S797)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:M1))
       call reorder2341(N0,M1,N0,N2,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,N0,M1,S788,D1)
       allocate(D2(N0+1:N2,N0+1:N2,N2+1:M2,N2+1:M2))
       call reorder3412(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N0,N2,N2,M2,N2,M2,t2C,D2)
       allocate(S789(N2+1:M2,N2+1:M2,N2+1:N3,N0+1:M1))
       I1=K5*K4
       I2=K0*K0
       I3=K2*K2
       call DMATMAT(I1,I2,I3,D1,D2,S789)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2314(N2,N3,N2,M2,N2,M2,N0,M1,X12,S789,-0.500)
       deallocate(S789)
C
       allocate(D1(N0+1:N2,N0+1:N2,N2+1:N3,N0+1:M1))
       call reorder3241(N0,M1,N0,N2,N0,N2,N2,N3,
     & N0,N2,N0,N2,N2,N3,N0,M1,S788,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S833(N2+1:M2,N0+1:N2,N2+1:N3,N0+1:M1))
       I1=K5*K4*K2
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S833)
       deallocate(D1)
       deallocate(B2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder2314(N2,M2,N0,N2,N2,N3,N0,M1,
     & N0,N2,N2,N3,N2,M2,N0,M1,S833,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S834(N2+1:M2,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S834)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,N2,M2,N0,M1,X12,S834,-1.000)
       deallocate(S834)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N0+1:M1))
       call reorder2431(N0,M1,N0,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N0,N2,N0,M1,S788,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S795(N2+1:M2,M1+1:N2,N0+1:N2,N0+1:M1))
       I1=K5*K2
       I2=K8*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S795)
       deallocate(D1)
       deallocate(D2)
C
       call
     & sum2413(N0,N2,N2,M2,N0,M1,M1,N2,X10,S795, 1.000)
       deallocate(S795)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N2,N3,VCHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q95(N0+1:N2,N2+1:N3))
       I1=K4*K2
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q95)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q95,B1)
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,N0,M1,M1,N2,t2C,D2)
       allocate(S808(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K4
       call DMATMAT(I1,I2,I3,B1,D2,S808)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X10,S808,-1.000)
       deallocate(S808)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q95,B1)
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,N0,M1,M1,N2,t2C,D2)
       allocate(S810(N2+1:M2,N0+1:M1,M1+1:N2,N0+1:N2))
       I1=K2
       I2=K8*K5*K0
       I3=K4
       call DMATMAT(I1,I2,I3,B1,D2,S810)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N2,N2,M2,N0,M1,M1,N2,X9,S810,-1.000)
       deallocate(S810)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q95,B1)
       allocate(D2(N2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,M2,M1,N2,N0,M1,t2B,D2)
       allocate(S794(N1+1:M2,M1+1:N2,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K8*K9
       I3=K4
       call DMATMAT(I1,I2,I3,B1,D2,S794)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N2,N1,M2,M1,N2,N0,M1,X13,S794, 1.000)
       deallocate(S794)
C
       call sumx1234(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,M2,M1,N2,N0,M1,X13,VBHHPH, 1.000)
C
       allocate(D2(N0+1:N2,N2+1:M2,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,M2,N2,M2,N0,M1,t2C,D2)
       allocate(Z13(N2+1:M2,N2+1:M2,N0+1:M1,N1+1:M2,M1+1:N2,N0+1:M1))
       I1=K5*K8*K9
       I2=K5*K0*K0
       I3=K2
       call DMATMAT(I1,I2,I3,X13,D2,Z13)
       deallocate(D2)
C
       call
     & sum124356(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z13, 1.000)
       deallocate(Z13)
       deallocate(X13)
C
       allocate(B1(N2+1:N3,N0+1:N2))
       call reorder21(N0,N2,N2,N3,
     & N2,N3,N0,N2,Q95,B1)
       allocate(D2(N2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,M2,N0,M1,N0,M1,t2B,D2)
       allocate(S802(N1+1:M2,N0+1:M1,N0+1:M1,N0+1:N2))
       I1=K2
       I2=K5*K5*K9
       I3=K4
       call DMATMAT(I1,I2,I3,B1,D2,S802)
       deallocate(B1)
       deallocate(D2)
C
       call
     & sum2341(N0,N2,N1,M2,N0,M1,N0,M1,X14,S802, 1.000)
       deallocate(S802)
C
       call sumx1234(N0,N2,N1,N3,N0,N2,N0,N1,
     & N0,N2,N1,M2,N0,M1,N0,M1,X14,VBHHPH, 1.000)
C
       allocate(D2(N0+1:N2,N2+1:M2,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,M2,N2,M2,M1,N2,t2C,D2)
       allocate(Z14(N2+1:M2,N2+1:M2,M1+1:N2,N1+1:M2,N0+1:M1,N0+1:M1))
       I1=K5*K5*K9
       I2=K8*K0*K0
       I3=K2
       call DMATMAT(I1,I2,I3,X14,D2,Z14)
       deallocate(D2)
C
       call
     & sum125346(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z14,-1.000)
       deallocate(Z14)
       deallocate(X14)
C
       allocate(D1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,N2,N0,N2,VCHHPP,D1)
       allocate(D2(N2+1:N3,N2+1:N3,N0+1:M1,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,N3,N0,M1,M1,N2,t2C,D2)
       allocate(S803(N0+1:M1,M1+1:N2,N0+1:N2,N0+1:N2))
       I1=K2*K2
       I2=K8*K5
       I3=K4*K4
       call DMATMAT(I1,I2,I3,D1,D2,S803)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder3412(N0,M1,M1,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,M1,M1,N2,S803,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S807(N2+1:M2,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S807)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,N2,M2,N0,M1,M1,N2,X10,S807,-0.500)
       deallocate(S807)
C
       call sumx2134(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,M2,N0,M1,M1,N2,X10,VCHHHP, 1.000)
C
       allocate(D2(N0+1:N2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,M2,N1,M2,N0,M1,t2B,D2)
       allocate(Z10(N2+1:M2,N1+1:M2,N0+1:M1,N2+1:M2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K0
       I2=K5*K9*K0
       I3=K2
       call DMATMAT(I1,I2,I3,X10,D2,Z10)
       deallocate(D2)
C
       call
     & sum136245(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z10, 1.000)
       deallocate(Z10)
       deallocate(X10)
C
       allocate(D1(N0+1:N2,N0+1:N2,N0+1:M1,M1+1:N2))
       call reorder3412(N0,M1,M1,N2,N0,N2,N0,N2,
     & N0,N2,N0,N2,N0,M1,M1,N2,S803,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S804(N2+1:M2,N0+1:N2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K2
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S804)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N0,N2,N2,M2,N0,M1,M1,N2,X9,S804,-0.500)
       deallocate(S804)
C
       call sumx2134(N2,N3,N0,N2,N0,N2,N0,N2,
     & N0,N2,N2,M2,N0,M1,M1,N2,X9,VCHHHP, 1.000)
C
       allocate(D2(N0+1:N2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,M2,N1,M2,N0,M1,t2B,D2)
       allocate(Z9(N2+1:M2,N1+1:M2,N0+1:M1,N2+1:M2,N0+1:M1,M1+1:N2))
       I1=K8*K5*K0
       I2=K5*K9*K0
       I3=K2
       call DMATMAT(I1,I2,I3,X9,D2,Z9)
       deallocate(D2)
C
       call
     & sum236145(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z9,-1.000)
       deallocate(Z9)
       deallocate(X9)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder4231(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N1,M2,N0,M1,t2B,D2)
       allocate(S805(N1+1:M2,N0+1:M1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K5*K9
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S805)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3412(N1,M2,N0,M1,N0,N2,N2,N3,
     & N0,N2,N2,N3,N1,M2,N0,M1,S805,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S809(N2+1:M2,N2+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K4
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S809)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,N1,M2,N0,M1,X16,S809,-1.000)
       deallocate(S809)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3412(N1,M2,N0,M1,N0,N2,N2,N3,
     & N0,N2,N2,N3,N1,M2,N0,M1,S805,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S806(N2+1:M2,N2+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K4
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S806)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,N1,M2,N0,M1,X15,S806,-1.000)
       deallocate(S806)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N1,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S648(N2+1:M2,N0+1:M1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K5*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S648)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder3412(N2,M2,N0,M1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N2,M2,N0,M1,S648,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S649(N1+1:M2,N1+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K3
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S649)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,N3,N2,M2,N1,M2,N0,M1,X7,S649,-1.000)
       deallocate(S649)
C
       call sumx2341(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,N2,M2,N1,M2,N0,M1,X7,VBPHPP, 1.000)
C
       allocate(D2(N1+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,M2,M1,N2,N0,M1,t2B,D2)
       allocate(Z7(N2+1:M2,M1+1:N2,N0+1:M1,N2+1:M2,N1+1:M2,N0+1:M1))
       I1=K5*K9*K0
       I2=K5*K8*K0
       I3=K3
       call DMATMAT(I1,I2,I3,X7,D2,Z7)
       deallocate(D2)
C
       call
     & sum256134(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z7, 1.000)
       deallocate(Z7)
       deallocate(X7)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S774(N2+1:M2,M1+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K8*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S774)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N2,M2,M1,N2,S774,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S775(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S775)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,N2,M2,M1,N2,X11,S775,-1.000)
       deallocate(S775)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:M1,N1+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,M1,N1,N3,VBHHPP,D1)
       allocate(B2(N0+1:N2,N2+1:N3))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,N3,t1B,B2)
       allocate(Q62(N0+1:M1,N1+1:N3))
       I1=K3*K5
       I3=K4*K2
       call DMATVEC(I1,I3,D1,B2,Q62)
       deallocate(D1)
       deallocate(B2)
C
       allocate(B1(N1+1:N3,N0+1:M1))
       call reorder21(N0,M1,N1,N3,
     & N1,N3,N0,M1,Q62,B1)
       allocate(B2(N1+1:N3,N0+1:M1))
       call reorder12(N1,N3,N0,N1,
     & N1,N3,N0,M1,t1A,B2)
       allocate(Q63(N0+1:M1,N0+1:M1))
       I1=K5
       I2=K5
       I3=K3
       call DMATMAT(I1,I2,I3,B1,B2,Q63)
       deallocate(B1)
       deallocate(B2)
C
       call
     & sum21(N0,M1,N0,M1,X29,Q63, 1.000)
       deallocate(Q63)
C
       call sumx21(N0,N1,N0,N1,
     & N0,M1,N0,M1,X29,FAHH, 1.000)
C
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2))
       call reorder612345(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N0,M1,N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,t3C1,F2)
       allocate(Z29(N2+1:M2,N2+1:M2,N1+1:M2,N0+1:M1,M1+1:N2,N0+1:M1))
       I1=K5
       I2=K8*K5*K9*K0*K0
       I3=K5
       call DMATMAT(I1,I2,I3,X29,F2,Z29)
       deallocate(F2)
C
       V3C=V3C-Z29
       deallocate(Z29)
       deallocate(X29)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N1,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S653(N2+1:M2,M1+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K8*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S653)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N2,M2,M1,N2,S653,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S654(N1+1:M2,N1+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K3
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S654)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,N3,N2,M2,N1,M2,M1,N2,X6,S654,-1.000)
       deallocate(S654)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N1,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S655(N2+1:M2,M1+1:N2,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K8*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S655)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,N1,N1,N3,
     & N0,N1,N1,N3,N2,M2,M1,N2,S655,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S656(N1+1:M2,N1+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K3
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S656)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,N3,N2,M2,N1,M2,M1,N2,X5,S656,-1.000)
       deallocate(S656)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder4132(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S798(N2+1:M2,M1+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K8*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S798)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N2,M2,M1,N2,S798,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S799(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S799)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,N2,M2,M1,N2,X11,S799, 1.000)
       deallocate(S799)
C
       allocate(D1(N0+1:N1,N2+1:N3,N0+1:N2,N1+1:N3))
       call reorder4132(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N0,N2,N1,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N2+1:N3,N1+1:M2,M1+1:N2))
       call reorder4123(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N2,N3,N1,M2,M1,N2,t2B,D2)
       allocate(S772(N1+1:M2,M1+1:N2,N0+1:N2,N1+1:N3))
       I1=K3*K2
       I2=K8*K9
       I3=K4*K1
       call DMATMAT(I1,I2,I3,D1,D2,S772)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,M1+1:N2))
       call reorder3412(N1,M2,M1,N2,N0,N2,N1,N3,
     & N0,N2,N1,N3,N1,M2,M1,N2,S772,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S773(N2+1:M2,N1+1:N3,N1+1:M2,M1+1:N2))
       I1=K8*K9*K3
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S773)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,N2,M2,N1,M2,M1,N2,X5,S773, 1.000)
       deallocate(S773)
C
       call sumx2341(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,N2,M2,N1,M2,M1,N2,X5,VBPHPP, 1.000)
C
       allocate(D2(N1+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,M2,N0,M1,N0,M1,t2B,D2)
       allocate(Z5(N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,N1+1:M2,M1+1:N2))
       I1=K8*K9*K0
       I2=K5*K5*K0
       I3=K3
       call DMATMAT(I1,I2,I3,X5,D2,Z5)
       deallocate(D2)
C
       call
     & sum246135(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z5,-1.000)
       deallocate(Z5)
       deallocate(X5)
C
       allocate(D1(N0+1:N2,N1+1:N3,N1+1:M2,M1+1:N2))
       call reorder3412(N1,M2,M1,N2,N0,N2,N1,N3,
     & N0,N2,N1,N3,N1,M2,M1,N2,S772,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S777(N2+1:M2,N1+1:N3,N1+1:M2,M1+1:N2))
       I1=K8*K9*K3
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S777)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N1,N3,N2,M2,N1,M2,M1,N2,X6,S777, 1.000)
       deallocate(S777)
C
       call sumx2341(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,N2,M2,N1,M2,M1,N2,X6,VBPHPP, 1.000)
C
       allocate(D2(N1+1:N3,N2+1:M2,N0+1:M1,N0+1:M1))
       call reorder2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,M2,N0,M1,N0,M1,t2B,D2)
       allocate(Z6(N2+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,N1+1:M2,M1+1:N2))
       I1=K8*K9*K0
       I2=K5*K5*K0
       I3=K3
       call DMATMAT(I1,I2,I3,X6,D2,Z6)
       deallocate(D2)
C
       call
     & sum146235(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z6, 1.000)
       deallocate(Z6)
       deallocate(X6)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N1+1:M2,N0+1:M1))
       call reorder3124(N1,N3,N1,N3,N0,N1,N0,N1,
     & N0,N1,N1,N3,N1,M2,N0,M1,t2A,D2)
       allocate(S746(N1+1:M2,N0+1:M1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K5*K9
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S746)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3412(N1,M2,N0,M1,N0,N2,N2,N3,
     & N0,N2,N2,N3,N1,M2,N0,M1,S746,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S747(N2+1:M2,N2+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K4
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S747)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,N1,M2,N0,M1,X15,S747,-1.000)
       deallocate(S747)
C
       call sumx2314(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N2,M2,N1,M2,N0,M1,X15,VBHPPP, 1.000)
C
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,N0,M1,M1,N2,t2C,D2)
       allocate(Z15(N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N1+1:M2,N0+1:M1))
       I1=K5*K9*K0
       I2=K8*K5*K0
       I3=K4
       call DMATMAT(I1,I2,I3,X15,D2,Z15)
       deallocate(D2)
C
       call
     & sum245136(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z15, 1.000)
       deallocate(Z15)
       deallocate(X15)
C
       allocate(D1(N0+1:N2,N2+1:N3,N1+1:M2,N0+1:M1))
       call reorder3412(N1,M2,N0,M1,N0,N2,N2,N3,
     & N0,N2,N2,N3,N1,M2,N0,M1,S746,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S748(N2+1:M2,N2+1:N3,N1+1:M2,N0+1:M1))
       I1=K5*K9*K4
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S748)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,N1,M2,N0,M1,X16,S748,-1.000)
       deallocate(S748)
C
       call sumx2314(N2,N3,N1,N3,N2,N3,N0,N1,
     & N2,N3,N2,M2,N1,M2,N0,M1,X16,VBHPPP, 1.000)
C
       allocate(D2(N2+1:N3,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder1234(N2,N3,N2,N3,N0,N2,N0,N2,
     & N2,N3,N2,M2,N0,M1,M1,N2,t2C,D2)
       allocate(Z16(N2+1:M2,N0+1:M1,M1+1:N2,N2+1:M2,N1+1:M2,N0+1:M1))
       I1=K5*K9*K0
       I2=K8*K5*K0
       I3=K4
       call DMATMAT(I1,I2,I3,X16,D2,Z16)
       deallocate(D2)
C
       call
     & sum145236(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z16,-1.000)
       deallocate(Z16)
       deallocate(X16)
C
       allocate(D1(N0+1:N1,N1+1:N3,N0+1:N2,N2+1:N3))
       call reorder4231(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N0,N2,N2,N3,VBHHPP,D1)
       allocate(D2(N0+1:N1,N1+1:N3,N2+1:M2,M1+1:N2))
       call reorder4213(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N1,N1,N3,N2,M2,M1,N2,t2B,D2)
       allocate(S779(N2+1:M2,M1+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K8*K0
       I3=K3*K1
       call DMATMAT(I1,I2,I3,D1,D2,S779)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N2,M2,M1,N2,S779,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S780(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S780)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,N3,N2,M2,N2,M2,M1,N2,X11,S780, 1.000)
       deallocate(S780)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder4132(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S790(N2+1:M2,N0+1:M1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K5*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S790)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3412(N2,M2,N0,M1,N0,N2,N2,N3,
     & N0,N2,N2,N3,N2,M2,N0,M1,S790,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S791(N2+1:M2,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S791)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum2134(N2,N3,N2,M2,N2,M2,N0,M1,X12,S791, 1.000)
       deallocate(S791)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S792(N2+1:M2,N0+1:M1,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K5*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S792)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3412(N2,M2,N0,M1,N0,N2,N2,N3,
     & N0,N2,N2,N3,N2,M2,N0,M1,S792,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S793(N2+1:M2,N2+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K4
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S793)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N2,N3,N2,M2,N2,M2,N0,M1,X12,S793, 1.000)
       deallocate(S793)
C
       call sumx2314(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N2,M2,N2,M2,N0,M1,X12,VCHPPP, 1.000)
C
       allocate(D2(N2+1:N3,N1+1:M2,M1+1:N2,N0+1:M1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,M2,M1,N2,N0,M1,t2B,D2)
       allocate(Z12(N1+1:M2,M1+1:N2,N0+1:M1,N2+1:M2,N2+1:M2,N0+1:M1))
       I1=K5*K0*K0
       I2=K5*K8*K9
       I3=K4
       call DMATMAT(I1,I2,I3,X12,D2,Z12)
       deallocate(D2)
C
       call
     & sum356124(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z12,-1.000)
       deallocate(Z12)
       deallocate(X12)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N1,N1+1:N3))
       call reorder3142(N2,N3,N1,N3,N0,N2,N0,N1,
     & N0,N2,N2,N3,N0,N1,N1,N3,VBHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,N0+1:M1))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,N0,M1,t2C,D2)
       allocate(S646(N2+1:M2,N0+1:M1,N0+1:N1,N1+1:N3))
       I1=K3*K1
       I2=K5*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S646)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N1,N1+1:N3,N2+1:M2,N0+1:M1))
       call reorder3412(N2,M2,N0,M1,N0,N1,N1,N3,
     & N0,N1,N1,N3,N2,M2,N0,M1,S646,D1)
       allocate(B2(N0+1:N1,N1+1:M2))
       call reorder21(N1,N3,N0,N1,
     & N0,N1,N1,M2,t1A,B2)
       allocate(S647(N1+1:M2,N1+1:N3,N2+1:M2,N0+1:M1))
       I1=K5*K0*K3
       I2=K9
       I3=K1
       call DMATMAT(I1,I2,I3,D1,B2,S647)
       deallocate(D1)
       deallocate(B2)
C
       call
     & sum3124(N1,N3,N2,M2,N1,M2,N0,M1,X8,S647,-1.000)
       deallocate(S647)
C
       call sumx2341(N2,N3,N1,N3,N0,N2,N1,N3,
     & N1,N3,N2,M2,N1,M2,N0,M1,X8,VBPHPP, 1.000)
C
       allocate(D2(N1+1:N3,N2+1:M2,M1+1:N2,N0+1:M1))
       call reorder2134(N2,N3,N1,N3,N0,N2,N0,N1,
     & N1,N3,N2,M2,M1,N2,N0,M1,t2B,D2)
       allocate(Z8(N2+1:M2,M1+1:N2,N0+1:M1,N2+1:M2,N1+1:M2,N0+1:M1))
       I1=K5*K9*K0
       I2=K5*K8*K0
       I3=K3
       call DMATMAT(I1,I2,I3,X8,D2,Z8)
       deallocate(D2)
C
       call
     & sum156234(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z8,-1.000)
       deallocate(Z8)
       deallocate(X8)
C
       allocate(D1(N0+1:N2,N2+1:N3,N0+1:N2,N2+1:N3))
       call reorder3142(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N0,N2,N2,N3,VCHHPP,D1)
       allocate(D2(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3124(N2,N3,N2,N3,N0,N2,N0,N2,
     & N0,N2,N2,N3,N2,M2,M1,N2,t2C,D2)
       allocate(S800(N2+1:M2,M1+1:N2,N0+1:N2,N2+1:N3))
       I1=K4*K2
       I2=K8*K0
       I3=K4*K2
       call DMATMAT(I1,I2,I3,D1,D2,S800)
       deallocate(D1)
       deallocate(D2)
C
       allocate(D1(N0+1:N2,N2+1:N3,N2+1:M2,M1+1:N2))
       call reorder3412(N2,M2,M1,N2,N0,N2,N2,N3,
     & N0,N2,N2,N3,N2,M2,M1,N2,S800,D1)
       allocate(B2(N0+1:N2,N2+1:M2))
       call reorder21(N2,N3,N0,N2,
     & N0,N2,N2,M2,t1B,B2)
       allocate(S801(N2+1:M2,N2+1:N3,N2+1:M2,M1+1:N2))
       I1=K8*K0*K4
       I2=K0
       I3=K2
       call DMATMAT(I1,I2,I3,D1,B2,S801)
       deallocate(D1)
       deallocate(B2)
       deallocate(S800)
C
       call
     & sum3124(N2,N3,N2,M2,N2,M2,M1,N2,X11,S801, 1.000)
       deallocate(S801)
C
       call sumx2314(N2,N3,N2,N3,N2,N3,N0,N2,
     & N2,N3,N2,M2,N2,M2,M1,N2,X11,VCHPPP, 1.000)
C
       allocate(D2(N2+1:N3,N1+1:M2,N0+1:M1,N0+1:M1))
       call reorder1234(N2,N3,N1,N3,N0,N2,N0,N1,
     & N2,N3,N1,M2,N0,M1,N0,M1,t2B,D2)
       allocate(Z11(N1+1:M2,N0+1:M1,N0+1:M1,N2+1:M2,N2+1:M2,M1+1:N2))
       I1=K8*K0*K0
       I2=K5*K5*K9
       I3=K4
       call DMATMAT(I1,I2,I3,X11,D2,Z11)
       deallocate(D2)
C
       call
     & sum346125(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z11, 1.000)
       deallocate(Z11)
       deallocate(X11)
C
       allocate(D1(N0+1:M1,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,M2,N3,N1,M2,N0,M1,VBHPPH,D1)
       allocate(F2(N0+1:M1,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z90(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K0
       I3=K6*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z90)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z90, 1.000)
       deallocate(Z90)
C
       allocate(D1(M1+1:N2,M2+1:N3,N1+1:M2,N0+1:M1))
       call reorder1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N2,M2,N3,N1,M2,N0,M1,VBHPPH,D1)
       allocate(F2(M1+1:N2,M2+1:N3,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder512346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,M2,N3,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z91(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K0
       I3=K6*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z91)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z91,-1.000)
       deallocate(Z91)
C
       allocate(D1(N0+1:M1,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & N0,M1,N2,M2,N1,M2,N0,M1,VBHPPH,D1)
       allocate(F2(N0+1:M1,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder412356(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & N0,M1,N2,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z92(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K0
       I3=K0*K5
       call DMATMAT(I1,I2,I3,D1,F2,Z92)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z92, 1.000)
       deallocate(Z92)
C
       allocate(D1(M1+1:N2,N2+1:M2,N1+1:M2,N0+1:M1))
       call reorder1324(N0,N2,N1,N3,N2,N3,N0,N1,
     & M1,N2,N2,M2,N1,M2,N0,M1,VBHPPH,D1)
       allocate(F2(M1+1:N2,N2+1:M2,N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2))
       call reorder512346(N2,N3,N2,N3,N2,M2,N0,N2,N0,N2,M1,N2,
     & M1,N2,N2,M2,N2,M2,N2,M2,N0,M1,M1,N2,t3D,F2)
       allocate(Z93(N2+1:M2,N2+1:M2,N0+1:M1,M1+1:N2,N1+1:M2,N0+1:M1))
       I1=K5*K9
       I2=K8*K5*K0*K0
       I3=K0*K8
       call DMATMAT(I1,I2,I3,D1,F2,Z93)
       deallocate(D1)
       deallocate(F2)
C
       call
     & sum124536(N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,V3C,Z93,-1.000)
       deallocate(Z93)
C
       call sumx3(N2,N3,N2,M2,N1,N3,N0,N2,M1,N2,N0,N1,
     & N2,M2,N2,M2,N1,M2,N0,M1,M1,N2,N0,M1,HT3C1,V3C,1.0)
       deallocate(V3C)
C
       end
