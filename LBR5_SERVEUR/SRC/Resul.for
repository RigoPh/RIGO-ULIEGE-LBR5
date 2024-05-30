      subroutine resul(wya,tya,dya,hya,ksa,wxr,txr,dxr,hxr,ksr,
     *                 delta,eta1,lamb,e1,nz,mt,kst,
     *                 sigy1,epsa,epsr,entr,ityp1,
     *                 q,jjk,nel,
     *                 eff,eff2,temp,temp2,temp3,
     *                 conc,z2,sxm,spk,sxmm,is)

      
      
      use param_section, temp_sxm => sxm,temp_=>temp,IS_=>IS !pour éviter conflit
      
	

      implicit double precision(a-h,o-z)
      double precision lamb,lamb2
      character *18 nom2(15)
	dimension dc(6,9,3),eff(9690),eff2(2040),
     *          temp(1530),temp2(1530),temp3(300),
     *          conc(750),z2(2295),
     *          sxm(iptmax),daa(9),dbb(9),dcc(9)
      dimension sxmm(neto),spk(neto)
	integer*4 trav(3)
      
c*******************************************************************************
c	ordre ds sens1(jj,4,9,neto) avec jj=1 à 9
c     data nom/'w','wø','wøø','wøøø','u','uø','v','vø ','vøø'/
c		      1   2    3     4      5   6    7   8     9 
c	ordre ds sens2(j,iptmax=4,9,neto) avec j=1 à 15
      data nom2/'sbord z=0 x=l/2','tbord z=0 x=0','sbord z=-d/2 x=l/2',
     *         'sbord z=+d/2 x=l/2','tcadre jas x=0','scadre jas x=l/2',
     *         'tcadre jab x=0','scadre jab x=l/2','scadre sem x=l/2',
     *         'traid jas x=0' ,'traid jab x=0'   ,'sraid sem x=l/2',
     *         'ttrav jas x=0' ,'ttrav jab x=0'   ,'strav sem x=l/2'/
c	ordre ds sens3(jj,4,9,neto) avec jj=1 à 20
       ! 1 et 2  dsx=d(sx/dx) et dsy dans borde  en z= 0
       ! 3 et 4  dsx=d(sx/dx) et dsy dans borde  en z= -d/2
       ! 5 et 6  dsx=d(sx/dx) et dsy dans borde  en z= +d/2
       ! 7 et 8  dsy=d(sy/dx) et dtxy  jas cadre en x=l/2
       ! 9 et10  dsy=d(sy/dx) et dtxy  jab cadre en x=l/2
       !11 et12  dsy=d(sy/dx) et dtxy  sem cadre en x=l/2
       !13 et14  dtxy(1) et dtxy(2)  jas raid en x=0 
       !15 et16  dtxy(1) et dtxy(2)  jab raid en x=0 
       !17 et18  dtxy(1) et dtxy(2)  jas trav en x=0 
       !19 et20  dtxy(1) et dtxy(2)  jab trav en x=0 
       !21       d(sx moyen) pour la restriction de paik


c*******************************************************************************
c     subroutine result 
c     ===================
c     subroutine de mise en forme des resultats pour 5 points de l'axe  
c     des x a partir du vecteur z2,resultat du calcul sur les 31+20 de  
c     l'axe des y(phi).                                                 
c     les resultats sont contenus dans les vecteurs eff,eff2 et conc 
c                                                                       
c    dernières modifications: ph. rigo
c	- contraintes tangentielles dans les aiguilles,  7-09-1993  
c	- contraintes en z=(+/-)delta/2 (eff2),            ??-1993  
c	- modif pour lbr5 (subr opti)                   23-08-1994  
c     - calcul des sensibilités sur les contraintes   20-04-1995 & 3-5-95
c          (solution dans sens2)
c     - corrections des traid et ttrav (temp2,temp3)   9-02-1996
c     - test si l'épaisseur semelle traverse = 0      14-04-1997
c     - correction sig trav jab (sensibilité)         13-02-1998
c     - suppression du common/di						23-03-1999
c     - cumul des sensibilites (termes de fourier)	15-02-2001
c     - combiner resul avec stiff (voir pt10):        27-05-2003
c                                                                                                                                           
c    modif :27-5-2003				      créer: thèse de doctorat (1988)
c                                                                       
c*******************************************************************************

      sh=e1/(2.*(1.+eta1))
      sn=e1/(1.-eta1*eta1)
      lamb2=lamb*lamb
c cadres/aiguilles
      ksaa=(-1)**ksa
      tm=wya*tya
      tmr=tm+dya*hya
      sm=(delta/2.+hya+tya)*ksaa
      sma=sm-tya*ksaa
      smb=(delta/2.)*ksaa
      smc=(delta/2.+hya+tya/2.)*ksaa
      smd=(delta/2.+hya/2.)*ksaa
      tpb=tm*smc
      tpr=tpb+dya*hya*smd
c raidisseurs
      ksrr=(-1)**ksr
      tmrx=wxr*txr
      tmrr=tmrx+dxr*hxr
      smr=(delta/2.+hxr+txr)*ksrr
      smar=smr-txr*ksrr
      smbr=(delta/2.)*ksrr
      smcr=(delta/2.+hxr+txr/2.)*ksrr
      smdr=(delta/2.+hxr/2.)*ksrr
      tpbr=tmrx*smcr
      tprr=tpbr+dxr*hxr*smdr

c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                                       pre00340
c                                                                       
c     calcul des deformations u,v,w et contraintes (eff, eff2, conc)                                                                    
c                                                                       
c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                                       pre00340
c
c    ordre des variables classées dans eff (1 à 38)
c    --------------------------------------------------
c       1       v               l=2     u                l=3    w
c       4       dw/dy             5     nx                 6    ny
c       7       nxy               8     nyx                9    mx
c      10       my               11     mxy               12    myx
c      13       qx               14     qy                15    ry
c      16       rx

c      17	sig y bordage	 18	sig x bordage	  19	tau xy bordage
c      20	sig comp bordage

c      21	sig y jas (aig)	 22   sig y jab (aig)	  23	tau jas (aig) 
c      24	tau jab   (aig)	 25   sig comp jas (aig)  26	sig comp jab (aig)
c      27	sig y sem (aig)	 28   tau sem   (aig)     29    sig comp sem (aig)

c      30 sig x jas (raid) 	  31   sig x jab (raid)	    32	sig x sem (raid)  
c      33 tau   jas (raid) 	  34   tau jab (raid)       35	tau sem (raid)   
c      36 sig comp jas (raid)   37   sig comp jab (raid)  38  sig comp sem (raid)

c    ordre des variables classées dans eff2 (1 à 8) 
c    --------------------------------------------------
c      1	sig y bord.(z=+d/2)  2	sig x bord.(z=+d/2)  3	tau xy bord.(z=+d/2) 
c      4	sig comp bord.(z=+d/2) 
c      5	sig y bord.(z=-d/2)  6	sig x bord.(z=-d/2)  7	tau xy bord.(z=-d/2)
c      8	sig comp bord.(z=-d/2)

c    ordre des variables classées dans conc (1 à 8) 
c    --------------------------------------------------
c       1 nx    2 nxy    3 mx    4 mxy    5 qx    6 rx
c       7	sig x sem       8	sig x jas       9	sig x jab 
c      10	tyx   sem      11	txz   jas      12	txz   jab
c      13	sig comp sem   14	sig comp jas   15	sig comp jab

c    vecteur z2:contient 51 valeurs par variables (31 + 20 au droit des traverses)
c    --------------------------------------------------
c     z2(  0+1 à   51) : v     z2(153+1  à  204) : xo(1)        
c     z2( 51+1 à  102) : u     z2(204+1  à  255) : zo(1)      z2(1479+1 à  1530) : v°  
c     z2(102+1 à  153) : w     z2(255+1  à  306) : xo(2)      z2(1530+1 à  1581) : u°  
c                              z2(306+1  à  357) : zo(2)      z2(1581+1 à  1632) : w°  
c                              ... etc.                       z2(1632+1 à  1683) : v°°  
c                              z2(1071+1 à 1122) : xo(10)     z2(1683+1 à  1734) : u°°  
c                              z2(1122+1 à 1173) : zo(10)     z2(1734+1 à  1785) : w°°  
c                                                             z2(1785+1 à  1836) : v°°°  
c                              z2(1173+1 à 1224) : my         z2(1836+1 à  1887) : u°°°  
c                              z2(1224+1 à 1275) : ny         z2(1887+1 à  1938) : w°°°  
c                              z2(1275+1 à 1326) : ry         z2(1938+1 à  1989) : nx  
c                              z2(1326+1 à 1377) : nyx        z2(1989+1 à  2040) : nxy  
c                              z2(1377+1 à 1428) : qy         z2(2040+1 à  2091) : mx  
c                              z2(1428+1 à 1479) : w°         z2(2091+1 à  2142) : mxy  
c                                                             z2(2142+1 à  2193) : myx  
c                                                             z2(2193+1 à  2244) : qx  
c                                                             z2(2244+1 à  2295) : rx  

      kh=31+2*mt
      do 114 k=1,5
      sp=dsin(lamb*dis(k))
      sq=dcos(lamb*dis(k))
c
c     calcul en 5 pts selon x et en 31+20(max) pts selon y de:
c     --------------------------------------------------------
      do 112 i=1,kh
      ij=i+(k-1)*51
c     calcul de  v,u,w,dw/dy,nx,ny,nxy,nyx,mx,my,mxy,myx,qx,qy,ry,rx,
      eff(ij)=eff(ij)+z2(i)*sp                  ! v
      eff(ij+255)=eff(ij+255)+z2(i+51)*sq       ! u
      eff(ij+510)=eff(ij+510)+z2(i+102)*sp      ! w
      eff(ij+765)=eff(ij+765)+z2(i+1581)*sp     ! wø
      if(impr2.le.-1) goto 555                   !nov2006
      eff(ij+1020)=eff(ij+1020)+z2(i+1938)*sp   ! nx
      eff(ij+1275)=eff(ij+1275)+z2(i+1224)*sp   ! ny
      eff(ij+1530)=eff(ij+1530)+z2(i+1989)*sq   ! nxy
      eff(ij+1785)=eff(ij+1785)+z2(i+1326)*sq   ! nyx
      eff(ij+2040)=eff(ij+2040)+z2(i+2040)*sp   ! mx
      eff(ij+2295)=eff(ij+2295)+z2(i+1173)*sp   ! my
      eff(ij+2550)=eff(ij+2550)+z2(i+2091)*sq   ! mxy
      eff(ij+2805)=eff(ij+2805)+z2(i+2142)*sq   ! myx
      eff(ij+3060)=eff(ij+3060)+z2(i+2193)*sq   ! qx
      eff(ij+3315)=eff(ij+3315)+z2(i+1377)*sp   ! qy
      eff(ij+3570)=eff(ij+3570)+z2(i+1275)*sp   ! ry
      eff(ij+3825)=eff(ij+3825)+z2(i+2244)*sq   ! rx
  555 continue

c     le bordage
c     **********
c     calcul dans le bordage a mi epaisseur (z=0)   
c     - sigmay,sigmax,txy,sigma comp             
	if((ityp1.eq.1).or.(ityp1.eq.2)) then
       eff(ij+4080)=eff(ij+4080)+sn*(z2(i+1479)-lamb*eta1*z2(i+51)     ! sigmay
     *                              +z2(i+102)/q)*sp                   !
      else if((ityp1.eq.3).or.(ityp1.eq.4)) then ! sans contribution selon oy
       eff(ij+4080)=eff(ij+4080)+sn*(-lamb*eta1*z2(i+51))*sp           ! sigmay
	endif
      eff(ij+4335)=eff(ij+4335)+sn*(-lamb*z2(i+51)+eta1*(z2(i+1479)    ! sigmax
     *            +z2(i+102)/q))*sp                                   !
      eff(ij+4590)=eff(ij+4590)+sh*(z2(i+1530)+lamb*z2(i))*sq         ! txy
      if (nz.ne.jjk) goto 888                                         !
      eff(ij+4845)=dsqrt(eff(ij+4080)**2+eff(ij+4335)**2-eff(ij+4080)*! sigma comp
     *             eff(ij+4335)+3.d00*(eff(ij+4590)**2))                 
 888  continue                                                          

      if((impr2.le.-2).and.(iopti.eq.0)) goto 2888		!nov2006
c     calcul dans le bordage a la fibre positive  (z=+delta/2)  
c     - sigmay,sigmax,txy,sigma comp   
	if((ityp1.eq.1).or.(ityp1.eq.2)) then
       eff2(ij)=eff2(ij)+sn*(z2(i+1479)-lamb*eta1*z2(i+51)+z2(i+102)/q ! sigmay
     *           -(delta/2.)*( z2(i+1734)-eta1*lamb2*z2(i+102) ) )*sp
      else if((ityp1.eq.3).or.(ityp1.eq.4)) then ! sans contribution selon oy
       eff2(ij)=eff2(ij)+sn* ( -lamb*eta1*z2(i+51)                     ! sigmay
     *          -(delta/2.)*(-eta1*lamb2*z2(i+102) ) )*sp
	endif
      eff2(ij+255)=eff2(ij+255)+sn*(-lamb*z2(i+51)+eta1*(z2(i+1479)    ! sigmax
     *         +z2(i+102)/q)
     *         -(delta/2.)*( eta1*z2(i+1734)-lamb2*z2(i+102) ) )*sp
      eff2(ij+510)=eff2(ij+510)+sh*(z2(i+1530)+lamb*z2(i)             ! txy
     *                         -delta*lamb*z2(i+1581) )*sq
      if (nz.ne.jjk) goto 1888
      eff2(ij+765)=dsqrt(eff2(ij)**2+eff2(ij+255)**2-eff2(ij)*        ! sigma comp
     *             eff2(ij+255)+3.d0*(eff2(ij+510)**2))  
 1888  continue                                                         
c     calcul dans le bordage a la fibre negative (z=-delta/2) 
c     - sigmay,sigmax,txy,sigma comp   
	if((ityp1.eq.1).or.(ityp1.eq.2)) then
       eff2(ij+1020)=eff2(ij+1020)+sn*(z2(i+1479)-lamb*eta1*z2(i+51)   ! sigmay
     *           +z2(i+102)/q
     *           +(delta/2.)*( z2(i+1734)-eta1*lamb2*z2(i+102) ) )*sp
      else if((ityp1.eq.3).or.(ityp1.eq.4)) then ! sans contribution selon oy
       eff2(ij+1020)=eff2(ij+1020)+sn*(-lamb*eta1*z2(i+51)             ! sigmay
     *           +(delta/2.)*(-eta1*lamb2*z2(i+102) ) )*sp
	endif
      eff2(ij+1275)=eff2(ij+1275)+sn*(-lamb*z2(i+51)+eta1*(z2(i+1479)  ! sigmax
     *            +z2(i+102)/q)
     *           +(delta/2.)*( eta1*z2(i+1734)-lamb2*z2(i+102) ) )*sp
      eff2(ij+1530)=eff2(ij+1530)+sh*(z2(i+1530)+lamb*z2(i)           ! txy
     *                         +delta*lamb*z2(i+1581) )*sq
      if (nz.ne.jjk) goto 2888
      eff2(ij+1785)=dsqrt(eff2(ij+1020)**2+eff2(ij+1275)**2-          ! sigma comp
     *            eff2(ij+1020)*eff2(ij+1275)+3.d0*(eff2(ij+1530)**2))
 2888  continue                                                         

c            cadres/aiguilles
c            ****************
c   calcul de : sigmay jas, sigmay jab, tyz jas, tyz jab,               
c               sigma comp jas, sigma comp jab .... ds ames cadres
      if((indaig.eq.0).and.(iopti.eq.0)) goto 113
      if(dya.eq.0.)goto 889
      eff(ij+5100)=eff(ij+5100)+e1*(z2(i+1479)+z2(i+102)/q				! sigmay jas
     *             -sma*z2(i+1734))*sp
      eff(ij+5355)=eff(ij+5355)+e1*(z2(i+1479)+z2(i+102)/q				! sigmay jab
     *             -smb*z2(i+1734))*sp
      temp(ij)    =temp(ij)   +										! tyz(ry) jas =t1
     *     e1*( tm*(z2(i+1632)+z2(i+1581)/q)-tpb*z2(i+1887))*sp/dya
      temp(ij+255)=temp(ij+255)+										! tyz(ry) jab =t1
     *     e1*(tmr*(z2(i+1632)+z2(i+1581)/q)-tpr*z2(i+1887))*sp/dya
      temp(ij+510)=temp(ij+510)+ sh*dya*lamb*z2(i+1581)*sq			! tyz(mxy) jab = tyz(mxy) jas = t2
      if (nz==jjk) then
       eff(ij+5610)=dabs(temp(ij))    +dabs(temp(ij+510))				! tyz jas = t1+t2
       eff(ij+5865)=dabs(temp(ij+255))+dabs(temp(ij+510))				! tyz jab = t1+t2
       eff(ij+6120)=dsqrt(eff(ij+5100)**2+3.d0*(eff(ij+5610)**2))		! sigma comp jas
       eff(ij+6375)=dsqrt(eff(ij+5355)**2+3.d0*(eff(ij+5865)**2))		! sigma comp jab
      endif
 889  continue
c     calcul de : sigmay semelle,txy sem, sigma comp semelle cadre
      if(tya.eq.0.)goto 113
      eff(ij+6630)=eff(ij+6630) +										! sigmay semelle
     *              e1*(z2(i+1479)+z2(i+102)/q - sm*z2(i+1734))*sp
      temp(ij+765)=temp(ij+765)+ sp*e1*								! txy(ry) sem = t1
     *  ( tm*(z2(i+1632)+z2(i+1581)/q)/2. - tpb*z2(i+1887)/2. ) /tya
      temp(ij+1020)=temp(ij+1020)+									! txy(??) sem = t2
     *                   sq *sh*1.25*fam(1)*(z2(i+1530)-lamb*z2(i))
      temp(ij+1275)=temp(ij+1275)+ sq*sh*tya*lamb*z2(i+1581)			! txy(mxy) sem t3
      if (nz==jjk) then
       eff(ij+6885)=dabs(temp(ij+765))+dabs(temp(ij+1020))			! txy sem =t1+t2+t3
     *                                +dabs(temp(ij+1275)) 
       eff(ij+7140)=dsqrt(eff(ij+6630)**2+3.d0*(eff(ij+6885)**2))		! sigma comp sem aig.
	endif
 113  continue

c          raidisseurs
c         **************
c   calcul de : sigmax jas, sigmax jab, sigmax semelle,

      if((indrai.eq.0).and.(iopti.eq.0)) goto 112
      if(dxr.ne.0.) then
       eff(ij+7395)=eff(ij+7395)+									! sigmax jas
     *              e1*lamb* (-z2(i+51)+smar*lamb*z2(i+102))*sp
       eff(ij+7650)=eff(ij+7650)+									! sigmax jab
     *              e1*lamb* (-z2(i+51)+smbr*lamb*z2(i+102))*sp
	endif
      if(txr.ne.0.)then
       eff(ij+7905)=eff(ij+7905)+									! sigmax semelle
     *              e1*lamb* (-z2(i+51)+smr* lamb*z2(i+102))*sp       
      endif 
c   calcul de: txz jas, txz jab, tyx sem, 
      if(dxr.ne.0.) then
       temp2(ij)=temp2(ij)+ sh*dxr*lamb*z2(i+1581) *sq			! txz(mxy)jas = txz(mxy) jab
       temp2(ij+255)=temp2(ij+255)+								! txz(rx) jas
     *    (-lamb2*e1*(z2(i+51)*tmrx-lamb*tpbr*z2(i+102))/dxr) *sq	
       temp2(ij+510)=temp2(ij+510)+								! txz(rx) jab
     *    (-lamb2*e1*(z2(i+51)*tmrr-lamb*tprr*z2(i+102))/dxr) *sq    
	endif
      if(txr.ne.0.) then                                           
       temp2(ij+765) =temp2(ij+765) +  sh*txr*lamb*z2(i+1581) * sq		! tyx(rx) sem
       temp2(ij+1020)=temp2(ij+1020)+       
     * (-lamb2*e1* (z2(i+51)*tmrx/2.-lamb*tpbr*z2(i+102)/2.) /txr) * sq	! tyx(mxy) sem   
       temp2(ij+1275)=temp2(ij+1275)+
     *            ( (-lamb*z2(i)+z2(i+1530)) *const(11)*1.5/tmrx) * sq	! tyx(??) sem  
      endif     
c   calcul de : sigma comp jas, sigma comp jab , sigma comp semelle     
      if (nz.ne.jjk) goto 112                                         
      eff(ij+8160)=dabs(temp2(ij+255))+dabs(temp2(ij))			! txz jas
      eff(ij+8415)=dabs(temp2(ij+510))+dabs(temp2(ij))			! txz jab
      eff(ij+8925)=dsqrt(eff(ij+7395)**2+3.d0*(eff(ij+8160)**2))	! sigma comp jas
      eff(ij+9180)=dsqrt(eff(ij+7650)**2+3.d0*(eff(ij+8415)**2))	! sigma comp jab
      if(txr.ne.0.) then
       eff(ij+8670)=dabs(temp2(ij+765))+dabs(temp2(ij+1020))		! tyx sem
     *                                 +dabs(temp2(ij+1275))
       eff(ij+9435)=dsqrt(eff(ij+7905)**2+3.d0*(eff(ij+8670)**2))	! sigma comp semelle
	endif
  112 continue   ! boucle sur les 31+2*mt points (selon y)                                                
c
      if(mt.eq.0) goto 114
c
c     calcul des effets concentres aux niveaux des traverses pour 5 pts
c     selon x et a l'emplacement des traverses selon y.
c     sont classes par ordre croissant dans le vecteur conc.
c
c     ordre des variables classées dans conc (1 à 8)
c     --------------------------------------------------
c       1 nx    2 nxy    3 mx    4 mxy    5 qx    6 rx
c       7	sig x sem       8	sig x jas       9	sig x jab 
c      10	tyx   sem      11	txz   jas      12	txz   jab
c      13	sig comp sem   14	sig comp jas   15	sig comp jab

	kstt=(-1)**kst
      do 116 i=1,mt
      ji=6*(i-1)
      ij=i+(k-1)*10

      wyt=panneau(nel).wxtr(i)      ! larg    sem
      tyt=panneau(nel).txtr(i)      ! epaiss. sem
      hyt=panneau(nel).hxtr(i)      ! haut.   âme
      dyt=panneau(nel).dxtr(i)      ! epaiss. âme
	
      tmt =wyt*tyt
      tmrt=tmt+dyt*hyt
      smt =(delta/2.+hyt+tyt)*kstt
      smat=smt-tyt*kstt
      smbt=(delta/2.)*kstt
      tpbt=tmt*(smat+0.5*tyt*kstt)
      tprt=tpbt+dyt*hyt*(smat-0.5*hyt*kstt)
	
c     rem: conc(1 a 50) contient "nx conc." sans la composante selon x  
c            de nyx (= sx*dv/dy + d2u/d2y / lamb ) c.a.d. la            
c            reciproque de "nxy conc.                                   
      conc(ij)=conc(ij)+lamb*( -const(15+ji)*z2(2*i+81)               ! nx
     *                         +lamb*const(16+ji)*z2(2*i+132) ) *sp     
      conc(ij+50)=conc(ij+50)+ const(17+ji)*sq *( z2(2*i+30)*lamb     ! nxy
     *                       + z2(2*i+1560))                            
      conc(ij+100)=conc(ij+100)+lamb*(-lamb*const(18+ji)*z2(2*i+132)+ ! mx
     *   const(16+ji)*z2(2*i+81))*sp
      conc(ij+150)=conc(ij+150) +( lamb*const(19+ji)*z2(2*i+1611)+    ! mxy
     *           const(20+ji) * (z2(2*i+30)*lamb+z2(2*i+1560)) )*sq     
      conc(ij+200)=conc(ij+200)+(lamb2*(-lamb*const(18+ji)*z2(2*i+132)! qx
     *       +const(16+ji)*z2(2*i+81))+                                 
     *    const(8) *(z2(2*i+1561)-z2(2*i+1560)))*sq                     
      conc(ij+250)=conc(ij+250)+ sq *( const(19+ji)*lamb*z2(2*i+1764) ! rx (partiel)
     *  + const(14)*(z2(2*i+1561)-z2(2*i+1560))                         
     *  +  const(20+ji)*(lamb*z2(2*i+1509)+z2(2*i+1713))  )             
      if((tyt.gt.0.).and.(wyt.gt.0.)) then                                             
       conc(ij+300)=conc(ij+300)+  e1*lamb*								! sig x sem
     *                (-z2(2*i+81)+lamb*smt*z2(2*i+132))*sp
      endif
      conc(ij+350)=conc(ij+350)+e1*lamb*(-z2(2*i+81)						! sig x jas
     *                            +smat*lamb*z2(2*i+132))*sp
      conc(ij+400)=conc(ij+400)+e1*lamb*(-z2(2*i+81)						! sig x jab
     *                            +smbt*lamb*z2(2*i+132))*sp
      if((tyt.gt.0.).and.(wyt.gt.0.)) then
       temp3(ij)    =temp3(ij) + sq*										! tyx(nxy)  sem
     *    (const(17+ji)*(-lamb*z2(2*i+30)+z2(2*i+1560))*1.5/tmt)*fam(5)           
       temp3(ij+50) =temp3(ij+50)  + sq* sh*tyt*lamb*z2(2*i+1611)			! tyx(mxy)  sem
       temp3(ij+100)=temp3(ij+100) + sq* 
     *  (-lamb2* e1*(z2(2*i+81)*tmt/2.-lamb*tpbt*z2(2*i+132)/2.)) /tyt		! tyx(rx)  sem
      endif
      if(dyt.ne.0.) then
       temp3(ij+150)=temp3(ij+150)+ (-lamb**2*e1*(z2(2*i+81)*tmt-			! txz(rx) jas
     *            lamb*tpbt*z2(2*i+132))/dyt)    *sq                    
       temp3(ij+200)=temp3(ij+200)+ (-lamb**2*e1*(z2(2*i+81)*tmrt-			! txz(rx) jab
     *            lamb*tprt*z2(2*i+132))/dyt) *sq                    
       temp3(ij+250)=temp3(ij+250)+ sh*dyt*lamb*z2(2*i+1611) *sq			! txz(mxy)jas=txz(mxy)jab
      endif

      if(nz.eq.jjk) then                                           
       conc(ij+250)=conc(ij+250)+conc(ij+200)							! rx
       if((tyt.gt.0.).and.(wyt.gt.0.)) then
        conc(ij+450)=dabs(temp3(ij))+dabs(temp3(ij+50))				! tyx  sem
     *                              +dabs(temp3(ij+100))
        conc(ij+600)=dsqrt(conc(ij+300)**2+3.*conc(ij+450)**2)		! sig comp sem
	 endif
       conc(ij+500)=dabs(temp3(ij+150))+dabs(temp3(ij+250) )			! txz jas
       conc(ij+550)=dabs(temp3(ij+200))+dabs(temp3(ij+250) )			! txz jab
       conc(ij+650)=dsqrt(conc(ij+350)**2+3.*conc(ij+500)**2)			! sig comp jas
       conc(ij+700)=dsqrt(conc(ij+400)**2+3.*conc(ij+550)**2)			! sig comp jab
      endif

  116 continue  ! boucle sur les traverses
  114 continue  ! boucle sur les 5 coupes (selon x)

c     Sauvetage des valeurs nécessaires pour le calcul des restrictions de fatigue
	
	!panneau(nel).ix_stiffened_pannel(j)
	!panneau(nel).iy_stiffened_pannel(j)

	!Test pour voir si cas de charge fatigue
	trav(1) = 1
	trav(2) = 16
	trav(3) = 31
	if (is_loadcase_fatigue(nnsol(is)).eq.1) then !cas de charge fatigue

		!Compteur de cas de charge fatigue
		nbr_lc_fatigue = 0
		do i=1,is
			if (is_loadcase_fatigue(nnsol(i)).eq.1) nbr_lc_fatigue = 
     *												nbr_lc_fatigue + 1
		enddo
	
	  do i=1,panneau(nel).nbr_Fat_Stif
	      ij = trav(panneau(nel).iy_Stif(i)) + 
     *						(panneau(nel).ix_Stif(i)-1)*51
            sx_raid_sem(nbr_lc_fatigue,nel,panneau(nel).ix_Stif(i),
     *                  panneau(nel).iy_Stif(i)) = eff(ij + 7905)
            !	sx_loc_raid_sem(???,nel,i) = ???
	  enddo
	  do i=1,panneau(nel).nbr_Fat_Plate
	      ij = trav(panneau(nel).iy_Plate(i)) + 
     *						(panneau(nel).ix_Plate(i)-1)*51
            sx_plaque_top(nbr_lc_fatigue,nel,panneau(nel).ix_Plate(i),
     *                      panneau(nel).iy_Plate(i)) = eff2(ij + 255)
			sy_plaque_top(nbr_lc_fatigue,nel,panneau(nel).ix_Plate(i),
     *                      panneau(nel).iy_Plate(i)) = eff2(ij)
			sxy_plaque_top(nbr_lc_fatigue,nel,panneau(nel).ix_Plate(i),
     *                      panneau(nel).iy_Plate(i)) = eff2(ij + 510)
			sx_plaque_bott(nbr_lc_fatigue,nel,panneau(nel).ix_Plate(i),
     *                      panneau(nel).iy_Plate(i)) = eff2(ij + 1275)
			sy_plaque_bott(nbr_lc_fatigue,nel,panneau(nel).ix_Plate(i),
     *                      panneau(nel).iy_Plate(i)) = eff2(ij + 1020)
			sxy_plaque_bott(nbr_lc_fatigue,nel,panneau(nel).ix_Plate(i),
     *                      panneau(nel).iy_Plate(i)) = eff2(ij + 1530)
	  enddo
	  do i=1,panneau(nel).nbr_Fat_Plate_Frame
	      ij = trav(panneau(nel).iy_Plate_Frame(i)) + 
     *						(panneau(nel).ix_Plate_Frame(i)-1)*51
         sy_cadre_jab(nbr_lc_fatigue,nel,panneau(nel).ix_Plate_Frame(i),
     *                  panneau(nel).iy_Plate_Frame(i)) = eff(ij + 5355)
		 sxy_cadre_jab(nbr_lc_fatigue,nel,panneau(nel).ix_Plate_Frame(i),
     *                  panneau(nel).iy_Plate_Frame(i)) = eff(ij + 5865)
	  enddo
	  do i=1,panneau(nel).nbr_Fat_Frame
	      ij = trav(panneau(nel).iy_Frame(i)) + 
     *						(panneau(nel).ix_Frame(i)-1)*51
            sy_cadre_sem(nbr_lc_fatigue,nel,panneau(nel).ix_Frame(i),
     *                      panneau(nel).iy_Frame(i)) = eff(ij + 6630)
	  enddo
	
	endif
	
	! Boucle sur chaque point

	!sx_raid_sem(casdecharge,nel,ptf_fat)
	

c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  

c    contrainte  sx moyenne dans borde + raidisseurs  en x=l/2 (sigma x) 
c    -------------------------------------------------------------------
c     pour le calcul de la restriction de paik 
c       sxm(ipt) =   sx moyen  = contrainte moyenne (plaque et raid inclus) 
c     d(sxm)/dx  = d(sx moyen)/dx  = sens3(21,i,k,ipan) 

      if(nz==1) call annuld(sxm,iptmax)


c      sig(moyen) = nx/section        avec  nx(axial):
c                 = a u + b v° + c w
  !    nx = (d+ox) du/dx   + (d eta1/q) dv/dy + (d eta1/q) w  - hx  w''  (avec w' = dw/dx)
  !    nx =-(d+ox)lamb * u + (d eta1/q) dv    + (d eta1/q) w  + hx  lamb2 w
  !     !!! comme u est en cos, on a u' (en sin) = - lamb u 
c       avec
c        tmrr= section raidisseur
c        omt = section raidisseur + largeur effective (epsr)
c        hx  = moment statique
c        sn   = e1/(1.-eta1*eta1)    deja calcule ci-avant
c        ksrr = (-1)**ksr 
c	   lamb2= lamb*lamb
c        tmrr = section raid (ame + sem)

        omt=tmrr +epsr*delta     ! section raid + bordé
	  omt2=omt*omt
	  cof1=sn*eta1
	  hx=dabs(tprr) ! moment statique en z=0
        

	
	  !if (ispecial.eq.0) then
	    spp = dsin(lamb*dis(5))
	  !sq = dcos(lamb*dis(5))
	  !else
	  !  spp = dsin(lamb*dis(2))
	  !endif
	  
	  !spp=dsin(nz*pi/2.)
	
        aa=-lamb*(sn*delta*epsr+e1*tmrr)/omt   ! a/section
        bb=cof1*delta*epsr/omt				! b/section
	  cc=bb/q+ksrr*lamb2*e1*hx/omt			! c/section

      ipt=panneau(nel).ipts
	if (iopti==0) ipt=1 ! soit un seul pt
	sxmm(nel)=0.
      do i=1,ipt
	 ip=panneau(nel).ipts2(i)
	 if (iopti==0) ip=16 ! soit au milieu du panneau

	  sxm(i)=sxm(i) + spp *
     *           ( aa*z2(ip+51) + bb* z2(ip+1479) + cc *z2(ip+102) )
	  if(sxmm(nel).ge.sxm(i)) sxmm(nel)=sxm(i)

	enddo

c     resist. ult selon la form. empirique de paik (subr ulk incluse dans subr paik)
      if(nz==jjk) then
        call ulk(e1,sigy1,delta,epsa,hxr,dxr,wxr,txr,entr,spaik) !! entr et pas epsr
	  spk(nel)=spaik
	endif


c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
c                                                                       
c     calcul des sensibilites sur les contraintes  
c     --------------------------------------------
c	- [calcul de sens2(16,iptmax,9,neto) ] : contraintes bordage, aig., raid. et traverses  !avril2003
c     
c     dérivées des contraintes par rapport aux variables de conception. 
c                        
c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
c     boucle 284/283:boucle sur toutes les variables de conception;
c                  (il y a nbrxi variable de conception par panneau)
c     boucle 280 = boucle sur les points de calcul et les fonctions
c
c     sens2 est relatif au panneau étudié par bo2 cad le panneau nel,
c     mais il donne les sensibilités pour les variables de conception
c     de tous les panneaux (ipan=1,neto)

      if(iopti.eq.0) go to 549

	ipt=panneau(nel).ipts


	
!!!	if (ispecial.eq.0) then
!!!	  ix=5
!!!	  sp = dsin(lamb*dis(5))
!!!	  sq = dcos(lamb*dis(5))
!!!	else
!!!	  ix=2
!!!	  sp = dsin(lamb*dis(2))
!!!	  sq = dcos(lamb*dis(2))
!!!	endif
	!sp=dsin(nz*pi/2)	! sin(x=l/2)
	!sq=+1.0				! cos (x=0) = cos(x=l)

	
      do 284 ipan=1,neto
      nbrxi=nvar(ipan,iboat)

	call annuld(dc,162)
	if(nel.eq.ipan) then
c      dc(i,k,j) ; dc = matrice contenant les dérivées des coef tm,tmr,etc.
c         avec   i = nø de la contrainte (i=1,6)
c                        contr. 1  : 0             ;  contr. 2  : 0
c                        contr. 3  : dc(1,k,1)     ;  contr. 4  : dc(1,k,2)
c                        contr. 5/7: 0
c                        contr. 6  : dc(2,k,j=1à3) ;  contr. 9  : dc(2,k,j=1à3)
c                        contr. 8  : dc(3,k,j=1à3)
c                        contr.10  : dc(4,k,j=1à2)
c                        contr.11  : dc(5,k,j=1à2)
c                        contr.12  : dc(6,k,j=1)
c                k = nø de la variables de conception (1 à 9)
c                j = nø des paramètres (k=1 à 3 max)
c  rem dc=o si nel est différent de ipan (cad variables xi d'un autre panneau)

      dc(1,1,1)=-0.5                                     ! sig en -d/2 contr. 3
      dc(1,1,2)=+0.5                                     ! sig en +d/2 contr. 4

      dc(2,1,2)=ksaa/2.                                  ! saig jas-sem, contr. 6/9
      dc(2,1,3)=tm*ksaa/2.                               ! saig jas-sem, contr. 6/9
      dc(2,2,2)=ksaa                                     ! saig jas-sem, contr. 6/9
      dc(2,2,3)=tm*ksaa                                  ! saig jas-sem, contr. 6/9
      dc(2,4,1)=tya                                      ! saig jas-sem, contr. 6/9
      dc(2,4,3)=tya*smc                                  ! saig jas-sem, contr. 6/9
                                                         !
      dc(3,1,2)=ksaa/2.                                  ! saig jab, contr. 8
      dc(3,1,3)=tmr*ksaa/2.                              ! saig jab, contr. 8
      dc(3,2,1)=dya                                      ! saig jab, contr. 8
      dc(3,2,3)=(tmr+dya*delta/2.)*ksaa                  ! saig jab, contr. 8
      dc(3,3,1)=hya                                      ! saig jab, contr. 8
      dc(3,3,3)=hya*smd                                  ! saig jab, contr. 8
      dc(3,4,1)=tya                                      ! saig jab, contr. 8
      dc(3,4,3)=tya*smc                                  ! saig jab, contr. 8
                                                         !
      dc(4,1,2)=tmrx*ksrr/2.                             ! traid jas, contr. 10
      dc(4,6,2)=tmrx*ksrr                                ! traid jas, contr. 10
      dc(4,8,1)=txr                                      ! traid jas, contr. 10
      dc(4,8,2)=txr*smcr                                 ! traid jas, contr. 10
                                                         !
      dc(5,1,2)=tmrr*ksrr/2.                             ! traid jab, contr. 11
      dc(5,6,1)=dxr                                      ! traid jab, contr. 11
      dc(5,6,2)=(tmrr+dxr*delta/2.)*ksrr                 ! traid jab, contr. 11
      dc(5,7,1)=hxr                                      ! traid jab, contr. 11
      dc(5,7,2)=hxr*smdr                                 ! traid jab, contr. 11
      dc(5,8,1)=txr                                      ! traid jab, contr. 11
      dc(5,8,2)=txr*smcr                                 ! traid jab, contr. 11
                                                         !
      dc(6,1,1)=ksrr/2.                                  ! sraid sem, contr. 12
      dc(6,6,1)=ksrr                                     ! sraid sem, contr. 12
	endif

      do 283 kk=1,nbrxi
      k=nxit(kk,ipan,iboat)
 
c	nø 1	 sigma comp. (sx,sy) bordage (z=0)      en x=l/2  (en n/mm2)
c	nø 2	 sigma comp. (txy)   bordage (z=0)      en x=0
c	nø 3	 sigma comp. (sx,sy) bordage (z=-d/2)   en x=l/2
c	nø 4	 sigma comp. (sx,sy) bordage (z=+d/2)   en x=l/2

c	nø 5	 sigma comp. (tâme)   aiguille   jas     en x=0    (en n/mm2)
c	nø 6	 sigma comp. (sy,txy) aiguille   jas     en x=l/2
c	nø 7	 sigma comp. (tâme)   aiguille   jab     en x=0
c	nø 8	 sigma comp. (sy,txy) aiguille   jab     en x=l/2
c	nø 9	 sigma comp. (sy,txy) aiguille   semelle en x=l/2

c	nø 10	 sigma comp. (tâme)   raidisseur jab     en x=l/2    (en n/mm2)   !avril2003
c	nø 11	 sigma comp. (tâme)   raidisseur jab     en x=0
c	nø 12	 sigma comp. (sx)     raidisseur semelle en x=l/2

c	nø 13	 sigma comp. (tâme)   traverse jas       en x=0    (en n/mm2)
c	nø 14	 sigma comp. (tâme)   traverse jab       en x=0
c	nø 15	 sigma comp. (sx)     traverse semelle   en x=l/2
 
c  1- contrainte ds bordage en z=0 et x=l/2  (sigmax,sigmay,txy,sigma comp) 
c  --------------------------------------------------------------------------

      do 380 i=1,ipt
	ix=5
	sp = dsin(lamb*dis(ix))
	sq = dcos(lamb*dis(ix))                                             		

	ij=(ix-1)*51+panneau(nel).ipts2(i)

      sens3(1,i,k,ipan)= sens3(1,i,k,ipan)+     != dsx= d(sigmax)/dx
     *    sp* sn * ( - lamb * sens1(5,i,k,ipan)							! dsx= d(sigmax)/dx
     *               + eta1  * (sens1(8,i,k,ipan)+sens1(1,i,k,ipan)/q))
      sens3(2,i,k,ipan)= sens3(2,i,k,ipan) +    !=dsy
     *    sp* sn * ( sens1(8,i,k,ipan)									! dsy= d(sigmay)/dx
     *              - lamb*eta1*sens1(5,i,k,ipan) + sens1(1,i,k,ipan)/q)

	sens2(1,i,k,ipan)=  sens2(1,i,k,ipan) +
     *     sq* sh * ( sens1(6,i,k,ipan) + lamb  * sens1(7,i,k,ipan)  )	! dtxy=d(txy)

	if (nz==jjk) then
	 den=eff(ij+4845)
	 if(dabs(den).le.(+1.e-10)) den =1.e-10
       sens2(1,i,k,ipan)=
     *    0.5*( (2.*eff(ij+4335)-eff(ij+4080))*sens3(1,i,k,ipan) +			! d(sigma comp)
     *          (2.*eff(ij+4080)-eff(ij+4335))*sens3(2,i,k,ipan) +
     *		   6.*eff(ij+4590)*sens2(1,i,k,ipan) )/den
      endif

  380 continue

c  2- contrainte ds bordage en z=0 et x=0  (sigmax=0,sigmay=0,txy,sigma comp) 
c  --------------------------------------------------------------------------

      do 381 i=1,ipt
	
	ix=1
	sp = dsin(lamb*dis(ix))
	sq = dcos(lamb*dis(ix))
	                                             		
	ij= panneau(nel).ipts2(i)

      sens2(2,i,k,ipan)=  sens2(2,i,k,ipan) +
     *     sq* sh * ( sens1(6,i,k,ipan) + lamb  * sens1(7,i,k,ipan)  )	! dtxy=d(txy)

	if (nz==jjk) then
       den=eff(ij+4590)
       if(dabs(den).le.(+1.e-10)) den =1.e-10
       sens2(2,i,k,ipan)=1.732*sens2(2,i,k,ipan) *den/dabs(den)			! d(sigma comp)
      endif

  381 continue

c  3- contrainte ds bordage en z=-d/2 et x=l/2  (sigmax,sigmay,txy=0,sigma comp) 
c  --------------------------------------------------------------------------

      do 382 i=1,ipt
	
	ix=5
	sp = dsin(lamb*dis(ix))
	sq = dcos(lamb*dis(ix))
	                                          		
	ip=panneau(nel).ipts2(i)
	ij=4*51+ip

      sens3(3,i,k,ipan)= sens3(3,i,k,ipan) +							! d(sigmax)/dx
     *  sp* sn*( - lamb * sens1(5,i,k,ipan) 
     *           + eta1  * (sens1(8,i,k,ipan)+sens1(1,i,k,ipan)/q)
     * +(delta/2.)*( eta1*sens1(3,i,k,ipan)-lamb2*sens1(1,i,k,ipan)) 
     * - dc(1,k,1)*( eta1*z2(ip+1734)-lamb2*z2(ip+102))  ) 
      sens3(4,i,k,ipan)= sens3(4,i,k,ipan) +							 ! d(sigmay)/dx
     *   sp* sn*( sens1(8,i,k,ipan) 
     *         - lamb*eta1*sens1(5,i,k,ipan) + sens1(1,i,k,ipan)/q  
     * +(delta/2.)*( sens1(3,i,k,ipan)-eta1*lamb2*sens1(1,i,k,ipan))   
     * - dc(1,k,1)*( z2(ip+1734)-eta1*lamb2*z2(ip+102))  ) 

      if(nz==jjk) then
	 den=eff2(ij+1785)
	 if(dabs(den).le.(+1.e-10)) den =1.e-10
       sens2(3,i,k,ipan)=												! d(sigma comp)/dx
     *   0.5*( (2.*eff2(ij+1275)-eff2(ij+1020))*sens3(3,i,k,ipan)
     *        +(2.*eff2(ij+1020)-eff2(ij+1275))*sens3(4,i,k,ipan) )/den
      endif

  382 continue

c	endif

c  4- contrainte ds bordage en z=+d/2 et x=l/2  (sigmax,sigmay,txy=0,sigma comp) 
c  --------------------------------------------------------------------------

      do 383 i=1,ipt
	
	ix=5
	sp = dsin(lamb*dis(ix))
	sq = dcos(lamb*dis(ix))
	                                           		
	ip=panneau(nel).ipts2(i)
	ij=4*51+ip

      sens3(5,i,k,ipan)= sens3(5,i,k,ipan) +                          ! d(sigmax)/dx
     *   sp*sn* ( - lamb * sens1(5,i,k,ipan)  
     *             + eta1  * (sens1(8,i,k,ipan)+sens1(1,i,k,ipan)/q)
     * -(delta/2.)*( eta1*sens1(3,i,k,ipan)-lamb2*sens1(1,i,k,ipan))  
     * - dc(1,k,2)*( eta1*z2(ip+1734)-lamb2*z2(ip+102))   ) 
      sens3(6,i,k,ipan)=sens3(6,i,k,ipan) +                            ! d(sigmay)
     *   sp*sn* ( sens1(8,i,k,ipan)
     *            - lamb*eta1*sens1(5,i,k,ipan) + sens1(1,i,k,ipan)/q  
     * -(delta/2.)*( sens1(3,i,k,ipan)-eta1*lamb2*sens1(1,i,k,ipan))  
     * - dc(1,k,2)*( z2(ip+1734)-eta1*lamb2*z2(ip+102))  ) 

      if(nz==jjk) then
	  den=eff2(ij+765)
	  if(dabs(den).le.(+1.e-10)) den =1.e-10
        sens2(4,i,k,ipan) =										  ! d(sigma comp)
     *     0.5*( (2.*eff2(ij+255)-eff2(ij)    )*sens3(5,i,k,ipan) 
     *          +(2.*eff2(ij)    -eff2(ij+255))*sens3(6,i,k,ipan) )/den
	endif

  383 continue

c  5&7- contrainte ds cadres jas et jab en x=0 [txy (du à mxy) = b * wø'/q] 
c  --------------------------------------------------------------------------

      do 384 i=1,ipt
	
	ix=1
	sp = dsin(lamb*dis(ix))
	sq = dcos(lamb*dis(ix))
	                                        		
	ij=panneau(nel).ipts2(i)
	ip=ij

c	seul le terme en sq est pris en compte
      sens2(5,i,k,ipan)= sens2(5,i,k,ipan)							! d(tyz(mxy) jas)dx
     *                   + sq* sh*lamb*dya*sens1(2,i,k,ipan)
      if((k.eq.3).and.(nel.eq.ipan)) then
	 sens2(5,i,k,ipan)= sens2(5,i,k,ipan)+ sq*sh*lamb*z2(ip+1581)
	endif 

	if(nz==jjk) then
	 den=temp(ij+510)  ! temp = tyz(mxy) jas  calcule ci-avant dans resul
	 if(dabs(den).le.(+1.e-10)) den =1.e-10
       sens2(5,i,k,ipan) = 1.732 * sens2(5,i,k,ipan) * den/dabs(den)  ! d(comp jas)dx
       sens2(7,i,k,ipan) = sens2(5,i,k,ipan)							! d(comp jab)dx
	endif

  384 continue

c  6&8- contraintes ds cadres jas et jab en x=l/2 (sigma y, txy et scomp) 
c  --------------------------------------------------------------------------
      do 385 i=1,ipt

	ix=5
	sp = dsin(lamb*dis(ix))
	sq = dcos(lamb*dis(ix))

	ip=panneau(nel).ipts2(i)
	ij=(ix-1)*51+ip

	if (nel.eq.73) then
		nel=nel
	endif

      sens3(7,i,k,ipan)= sens3(7,i,k,ipan) +	sp*							!d(sigmay jas)/dx 
     *   e1*(sens1(8,i,k,ipan)+sens1(1,i,k,ipan)/q-sma*sens1(3,i,k,ipan)
     *                       -dc(2,k,2)*z2(ip+1734) )
      sens3(8,i,k,ipan)= sens3(8,i,k,ipan) +	sp*e1*						!d(tyz jas)/dx 
     *     (   tm * (sens1(9,i,k,ipan)+sens1(2,i,k,ipan)/q)          
     *       + dc(2,k,1) * (z2(ip+1632)+z2(ip+1581)/q)
     *       - tpb* sens1(4,i,k,ipan) -dc(2,k,3)*z2(ip+1887)  )/dya
	stt7 =sp *e1*(tm*(z2(ip+1632)+z2(ip+1581)/q) 
     *- tpb*z2(ip+1887))/dya	! tyz jas (non cumule)
c     stt7 = temp(ij) =tyz jas  mais non cumule
      if((k.eq.3).and.(nel.eq.ipan)) then
	  sens3(8,i,k,ipan)= sens3(8,i,k,ipan) - stt7/dya
	endif

      if(nz==jjk) then
	 den=eff(ij+6120)

	 if (nel.eq.73.and.ipan.eq.73) then
		nel=nel
	 endif

	 if(dabs(den).le.(+1.0e-10)) den =1.e-10
       sens2(6,i,k,ipan)=													! d(sigma comp jas)/dx (sensib)
     *  0.5* ( 2.*eff(ij+5100)*sens3(7,i,k,ipan) 
     *        +6.*temp(ij)    *sens3(8,i,k,ipan)  )/den     
      endif

      sens3(9,i,k,ipan)= sens3(9,i,k,ipan) +	sp*							!d(sigmay jab)/dx 
     *  e1*(sens1(8,i,k,ipan)+sens1(1,i,k,ipan)/q-smb*sens1(3,i,k,ipan) 
     *                      -dc(3,k,2)*z2(ip+1734)  )
      sens3(10,i,k,ipan)= sens3(10,i,k,ipan) + sp*e1*						!d(tyz jab)/dx = d(stt8)/dx
     *    (   tmr * (sens1(9,i,k,ipan)+sens1(2,i,k,ipan)/q)   
     *      + dc(3,k,1) * (z2(ip+1632)+z2(ip+1581)/q)
     *      - tpr* sens1(4,i,k,ipan) - dc(3,k,3)*z2(ip+1887)   )/dya
	stt8=sp *e1*(tmr*(z2(ip+1632)+z2(ip+1581)/q) 
     *- tpr*z2(ip+1887))/dya	!tyz jab (non cumule)
      if((k.eq.3).and.(nel.eq.ipan)) then
	  sens3(10,i,k,ipan)=sens3(10,i,k,ipan)- stt8/dya
	endif

      if(nz==jjk) then
	 den=eff(ij+6375)
	 if(dabs(den).le.(+1.e-10)) den =1.e-10
       sens2(8,i,k,ipan) =               ! d(sigma comp jab)/dx 
     *  0.5*(  2.*eff(ij+5355)*sens3( 9,i,k,ipan)  
     *       + 6.*temp(ij+255)*sens3(10,i,k,ipan) )/den   
      endif

  385 continue

c  9- contraintes ds cadres semelles en x=l/2 (sigma y, txy et scomp) 
c  ---------------------------------------------------------------

	ix=5
	sp = dsin(lamb*dis(ix))
	sq = dcos(lamb*dis(ix))

      do 386 i=1,ipt
	ip=panneau(nel).ipts2(i)
	ij=4*51+ip

c     eff(ij+6630)   = sigmay sem        
c	eff(ij+6885)   = sp * dabs(tyz(1)) + sq*dabs(tyz(2)) + sq* dabs(tyz(3)) semelle
c	temp(ij+765)   = tyz(ry) sem     cumule
c                    = sp*e1*(tm*(z2(ip+1632)+z2(ip+1581)/q)/2.-tpb*z2(ip+1887)/2.)/tya
      sens3(11,i,k,ipan)= sens3(11,i,k,ipan) + sp * e1 *					! d(sigmay sem)/dx
     *  ( sens1(8,i,k,ipan)+sens1(1,i,k,ipan)/q-sm*sens1(3,i,k,ipan)
     *   -dc(2,k,2)*z2(ip+1734)  )
      sens3(12,i,k,ipan)= sens3(12,i,k,ipan) +  sp * 0.5*e1* 				! d(tyz(ry) sem)/dx             
     *   (  tm * (sens1(9,i,k,ipan)+sens1(2,i,k,ipan)/q)       
     *    + dc(2,k,1) * (z2(ip+1632)+z2(ip+1581)/q)
     *    - tpb* sens1(4,i,k,ipan) - dc(2,k,3)*z2(ip+1887)  )/tya

      if(nz==jjk) then
	  den=eff(ij+7140)
	  if(dabs(den).le.(+1.e-10)) den =1.e-10
        sens2(9,i,k,ipan) =         ! d(sigma comp sem)/dx 
     *    0.5*( 2.*eff(ij+6630)*sens3(11,i,k,ipan) 
     *         +6.*temp(ij+765)*sens3(12,i,k,ipan))/den     
      endif

  386 continue

c  10- contraintes ds raidisseurs jab en x=l/2 (sigma x)       !avril2003 comme tout le point 10!!!
c  -----------------------------------------------------
	
	ix=5
	sp = dsin(lamb*dis(ix))
	sq = dcos(lamb*dis(ix))

      do 389 i=1,ipt
	ip=panneau(nel).ipts2(i)
	ij=4*51+ip

      sens2(10,i,k,ipan)= sens2(10,i,k,ipan) + sp* e1*lamb*		    ! d(sigmax jab)/dx (=sensib)
     *     ( - sens1(5,i,k,ipan)                                 
     *       + lamb*(smbr*sens1(1,i,k,ipan)+dc(6,k,1)*z2(ip+102)) )

	if (nz==jjk) then
	  den=eff(ij+7650) ! = sx jab raid
	  if(dabs(den).le.(+1.e-10)) den=1.e-10
        sens2(10,i,k,ipan)= sens2(10,i,k,ipan)*den/dabs(den)      ! d(sigma comp jab) (=sensib)
      endif

  389 continue



c   11- contraintes ds raidisseurs jab en x=0 (txy )           !avril2003 ainsi que les !!!
c  -------------------------------------------------
      do 387 i=1,ipt

	ix=1
	sp = dsin(lamb*dis(ix))
	sq = dcos(lamb*dis(ix))

	ip=panneau(nel).ipts2(i)
	ij=ip

      sens3(15,i,k,ipan)= sens3(15,i,k,ipan) - sq* lamb2*e1*			 ! d(tyz(rx)jab)/dx
     *    (  sens1(5,i,k,ipan)*tmrr + z2(ip+51)*dc(5,k,1)           
     *     - lamb*(tprr*sens1(1,i,k,ipan) + dc(5,k,2)*z2(ip+102)) ) /dxr 
      stt2=-sq * lamb2*e1* ( z2(ip+51)*tmrr-lamb*tprr*z2(ip+102) ) /dxr ! tyz (rx) jab (contraintes)
      if((k.eq.7).and.(nel.eq.ipan)) then
	 sens3(15,i,k,ipan)= sens3(15,i,k,ipan) -  stt2 /dxr            
	endif                                                   

      sens3(16,i,k,ipan)= sens3(16,i,k,ipan)								!d(tyz(mxy)jab)/dx  !avril2003 
     *                    + sh*lamb*dxr* sens1(2,i,k,ipan)                                   !avril2003
      if((k.eq.7).and.(nel.eq.ipan)) then
	 sens3(16,i,k,ipan)= sens3(16,i,k,ipan) + sq* sh*lamb*z2(ip+1581)	!d(tyz(mxy)jab)/dx  !avril2003
	endif
	
	if(nz==jjk) then
	  den =temp2(ij+510)
	  if(dabs(den).le.(+1.e-10)) den =1.e-10
	  den2=temp2(ij)
	  if(dabs(den2).le.(+1.e-10)) den2 =1.e-10
        sens2(11,i,k,ipan)=  sens3(15,i,k,ipan) * den /dabs(den)	! d(dabs(tyz(rx))+dabs(tyx(mxy))jab)/dx
     *                     + sens3(16,i,k,ipan) * den2/dabs(den2)      
        sens2(11,i,k,ipan) =1.732051*sens2(11,i,k,ipan)				! d(sigma comp jab)/dx 
      endif

  387 continue



c  12- contraintes ds raidisseurs sem en x=l/2 (sigma x) 
c  -------------------------------------------------------------------
      do 388 i=1,ipt

	ix=5
	sp = dsin(lamb*dis(ix))
	sq = dcos(lamb*dis(ix))

	ip=panneau(nel).ipts2(i)
	ij=4*51+ip

      sens2(12,i,k,ipan)= sens2(12,i,k,ipan) + sp* e1*lamb*		    ! d(sigmax sem)/dx (=sensib)
     *     ( - sens1(5,i,k,ipan)                                 
     *       + lamb*(smr*sens1(1,i,k,ipan)+dc(6,k,1)*z2(ip+102)) )

	if (nz==jjk) then
	  den=eff(ij+7905) ! = sx sem raid
	  if(dabs(den).le.(+1.e-10)) den=1.e-10
        sens2(12,i,k,ipan)= sens2(12,i,k,ipan)*den/dabs(den)          ! d(sigma comp sem) (=sensib)
      endif

  388 continue

c  boucle (391) sur les traverses
c  ==============================
      deriv=0.
      if(mt.eq.0) goto 392	

      do 391 i=1,ipt
	 it=panneau(nel).ipts3(i) ! no de la traverse concernee
       if(it.eq.0) goto 391	

      wyt=panneau(nel).wxtr(it)               ! larg.   sem
      tyt=panneau(nel).txtr(it)               ! epaiss. sem
      hyt=panneau(nel).hxtr(it)               ! haut.   âme
      dyt=panneau(nel).dxtr(it)            ! epaiss. âme
      tmt=wyt*tyt                                                       
      tmrt=tmt+dyt*hyt
      tpbt=tmt*(delta/2.+hyt+0.5*tyt)*kstt
      tprt=tpbt+dyt*hyt*(delta+hyt)*kstt/2.

c  13&14- contraintes ds traverses jas et jab en x=0 (txy ) 
c  ---------------------------------------------------------------

	ix=1
	sp = dsin(lamb*dis(ix))
	sq = dcos(lamb*dis(ix))

	ij=it ! car x=0
      if(k.eq.1) deriv=tmt*kstt/2.
      sens3(17,i,k,ipan)=sens3(17,i,k,ipan) - sq* lamb2*e1*			! d(tyz(rx) jas)dx
     *      ( sens1(5,i,k,ipan)*tmt -                      
     *        lamb*(tpbt*sens1(1,i,k,ipan)+deriv*z2(2*it+132))  ) /dyt 
      sens3(18,i,k,ipan)= sens3(18,i,k,ipan)							! d(tyz(mxy) jas)/dx
     *                  + sq* sh*dyt*lamb * sens1(2,i,k,ipan)	
      
	if (nz==jjk) then
	  den =temp3(ij+150)
	  if(dabs(den).le.(+1.e-10)) den =1.e-10
	  den2=temp3(ij+250)
	  if(dabs(den2).le.(+1.e-10)) den2 =1.e-10
        sens2(13,i,k,ipan)=   sens3(17,i,k,ipan) * den /dabs(den) 
     *                      + sens3(18,i,k,ipan) * den2/dabs(den2)	! d(abs[tyz(rx)]+abs[tyz(mxy)]jas)/dx 
        sens2(13,i,k,ipan) = 1.732051*sens2(13,i,k,ipan)              ! d(sigma comp jas)/dx 
      endif


	if(k.eq.1) deriv=tmrt*kstt/2.
      sens3(19,i,k,ipan)=sens3(19,i,k,ipan)  - sq* lamb2*e1*			!  d(tyz(rx)jab)/dx
     *      (  sens1(5,i,k,ipan)*tmrt              
     *       - lamb*(tprt*sens1(1,i,k,ipan)+deriv*z2(2*it+132))  ) /dyt 
      sens3(20,i,k,ipan)=sens3(18,i,k,ipan)							! d(tyz(mxy) jab)/dx

	if (nz==jjk) then
	  den =temp3(ij+200)
	  if(dabs(den).le.(+1.e-10)) den =1.e-10
	  den2=temp3(ij+250)
	  if(dabs(den2).le.(+1.e-10)) den2 =1.e-10
        sens2(14,i,k,ipan)=   sens3(19,i,k,ipan) * den /dabs(den) 
     *                      + sens3(20,i,k,ipan) * den2/dabs(den2)	! d(abs[tyz(rx)]+abs[tyz(mxy)]jab)/dx 
        sens2(14,i,k,ipan) = 1.732051*sens2(14,i,k,ipan)              ! d(sigma comp jab)/dx 
      endif

c  15- contraintes ds traverses sem en x=l/2 (sigma x) 
c  -------------------------------------------------------------------

	ix=5
	sp = dsin(lamb*dis(ix))
	sq = dcos(lamb*dis(ix))

	if(k.eq.1) deriv=kstt/2.
	ij=40+it

      sens2(15,i,k,ipan) = sens2(15,i,k,ipan) +
     *   sp* e1*lamb*(-sens1(5,i,k,ipan)                               ! d(sigmay sem)/dx (sensib)
     *            +lamb*(smt*sens1(1,i,k,ipan)+deriv*z2(2*it+132)) )

	if (nz==jjk) then
	  den=conc(ij+300)												! conc(ij+300) = sigmay sem trav
	  if(dabs(den).le.(+1.e-10)) den =1.e-10
        sens2(15,i,k,ipan)=sens2(15,i,k,ipan) *den/dabs(den)			! d(sigma comp sem)/dx  (sensib)
      endif

  391 continue ! boucle sur ipt (les pts de calcul des sensibilites sur le panneau)
  392 continue ! si pas de traverse

c  16'- contraintes ds raidisseurs jas en x=l/2 (sigma x)    !avril2003 comme tout le point 16'!!!
c  ------------------------------------------------------

	ix=5
	sp = dsin(lamb*dis(ix))
	sq = dcos(lamb*dis(ix))

      do 393 i=1,ipt
	ip=panneau(nel).ipts2(i)
	ij=4*51+ip

      sens2(16,i,k,ipan)= sens2(16,i,k,ipan) + sp* e1*lamb*		    ! d(sigmax jas)/dx (=sensib)
     *     ( - sens1(5,i,k,ipan)                                 
     *       + lamb*(smar*sens1(1,i,k,ipan)+dc(6,k,1)*z2(ip+102)) )

	if (nz==jjk) then
	  den=eff(ij+7395) ! = sx jas raid
	  if(dabs(den).le.(+1.e-10)) den=1.e-10
        sens2(16,i,k,ipan)= sens2(16,i,k,ipan)*den/dabs(den)          ! d(sigma comp jas) (=sensib)
      endif

  393 continue
 
c  16- contrainte  sx moyenne dans borde + raidisseurs  en x=l/2 (sigma x) 
c  -------------------------------------------------------------------
c     pour le calcul de la restriction de paik (cfr subr. sens4 dans subr contr)
       ! resultat dans sens3(21,..) et pas dans sens2
	 ! car il ne s'agit pas d'une contrainte de comparaison
c       sxm(ipt) =   sx moyen  = contrainte moyenne (plaque et raid inclus) 
c                    est calcule ci-avant
c     d(sxm)/dx  = d(sx moyen)/dx  = sens3(21,i,k,ipan) 

c      sig(moyen) = nx/section           cfr nx (axial):
c                 = aa u + bb v° + cc w
  !   nx = (d+ox) du/dx   + (d eta1/q) dv/dy + (d eta1/q) w  - hx  w''  (avec w' = dw/dx)
  !   nx =-(d+ox)lamb * u + (d eta1/q) v°    + (d eta1/q) w  + hx  lamb2 w
  !     !!! comme u est en cos, on a u'(x=l/2) = - lamb u(x=0)
c     avec
c       tmrr= section raidisseur
c       omt = section raidisseur + largeur effective (epsr)
c       hx  = moment statique

c    deja calcule ci-avant:
c       sn=e1/(1.-eta1*eta1)  
c       ksrr=(-1)**ksr 
c	  lamb2=lamb*lamb
c       tmrr = section raid (ame + sem)
c       omt=tmrr +epsr*delta     ! section raid + bordé
c	  omt2=omt*omt
c	  cof1=sn*eta1	
c       aa=-lamb*(sn*delta*epsr+e1*tmrr)/omt   ! a/section
c       bb=cof1*delta*epsr/omt				! b/section
c	  cc=bb/q+ksrr*lamb2*e1*hx/omt			! c/section
c       sxm(i)=sxm(i) + aa*z2(ip+51) + bb* z2(ip+1480) + cc *z2(ip+102)

      if(ipan.eq.nel) then  
          if(k.eq.1) then                        ! xi=delta
           r1=epsr*tmrr/omt2
	     dbb(k)=r1*cof1
	     daa(k)=-lamb*(sn-e1)*r1
	     dcc(k)=dbb(k)/q+ksrr*lamb2*e1*(0.5*omt*tmrr-hx*epsr)/omt2
          else if (k.eq.6) then                  ! xi=hxr
	     r1=-delta*epsr*dxr/omt2
	     dbb(k)=r1*cof1
	     daa(k)=-lamb*(sn-e1)*r1
	     dcc(k)=dbb(k)/q
     *             +ksrr*lamb2*e1*(omt*(tmrr+dxr*delta/2.)-hx*dxr)/omt2
          else if (k.eq.7) then                  ! xi=dxr=tweb
	     r1=-delta*epsr*hxr/omt2
	     dbb(k)=r1*cof1
	     daa(k)=-lamb*(sn-e1)*r1
	     dcc(k)=dbb(k)/q+ksrr*lamb2*e1*hxr*(omt*dabs(smdr)-hx)/omt2
          else if (k.eq.8) then                  ! xi=wxr =lsem.
	     r1=-delta*epsr*txr/omt2
	     dbb(k)=r1*cof1
	     daa(k)=-lamb*(sn-e1)*r1
	     dcc(k)=dbb(k)/q+ksrr*lamb2*e1*txr*(omt*dabs(smcr)-hx)/omt2
          else if (k.eq.9) then                  ! xi=epsr
	     r1=delta*tmrr/omt2
	     dbb(k)=r1*cof1
	     daa(k)=-lamb*(sn-e1)*r1
	     dcc(k)=dbb(k)/q-ksrr*lamb2*e1*hx*delta/omt2
	    else
	     dbb(k)=0.
	     daa(k)=0.
	     dcc(k)=0.
	    endif
	endif

      do i=1,ipt
	  ip=panneau(nel).ipts2(i)


c       ! 1er partie de   d(sx moyen)/dx 
        sens3(21,i,k,ipan)= sens3(21,i,k,ipan) + spp* 		! d(sx moyen)/dx 
     *				   (  aa*sens1(5,i,k,ipan)   
     *					+ bb*sens1(8,i,k,ipan) 
     *					+ cc*sens1(1,i,k,ipan) )
       ! 2eme partie de   d(sx moyen)/dx 
        if(ipan.eq.nel) then
          sens3(21,i,k,ipan)= sens3(21,i,k,ipan) + spp* 		 ! d(sx moyen)/dx suite
     *                  (   daa(k)*z2(ip+51) 
     *                    + dbb(k)*z2(ip+1479) 
     *                    + dcc(k)*z2(ip+102)     )    
        endif
	enddo
     
   
  283 continue ! boucle sur kk=1,nbrxi                                           		
  284 continue ! boucle sur les panneaux ipan (liste des variables de conception)
  
c ******************************************************************************
c ******************************************************************************
	
	if ((nz==jjk).and.(iprint.ge.1)) then
	 write(iu_11(iboat),*) ' sensibilites (contraintes) '
       write(iu_11(iboat),*) ' ************************** '

       do 281 ipan=1,neto
	   nbrxi=nvar(ipan,iboat)
       do 281 kk=1,nbrxi
         k=nxit(kk,ipan,iboat)
         write(iu_11(iboat),288) k,ipan
         write(iu_11(iboat),294) 
         do 290 j=1,12
  290    write(iu_11(iboat),282) nom2(j),(sens2(j,i,k,ipan),i=1,ipt)

         if(mt.ge.1) then
           do 292 j=13,15
  292      write(iu_11(iboat),282) nom2(j),(sens2(j,i,k,ipan),i=1,ipt)
         endif

         write(iu_11(iboat),282) 'contr. sx moyenne-raid',
     *                  (sens3(21,i,k,ipan),i=1,ipt)
   
  281  continue
      endif

c     fin de la boucle (549) relative aux calculs des sensibilités des contraintes
  549 continue
  
      return 
c ******************************************************************************
c 	les formats
c 	-----------
  282 format(a18,1x,5(1x,e13.6)) !avril2003
  288 format(' variable de conception nø',i3,' du panneau nø',i3/  !avril2003
     *         50(1h-))
  294 format('     fct.             pt.1          pt.2',
     *     '          pt.3          pt.4          pt.5')
      end
