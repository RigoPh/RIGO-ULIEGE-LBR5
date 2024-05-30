      subroutine coe(const,arg,coeff,lam,eta,eta2,mt,m,z,x,w,a,b,c,f,
     *               h,g,q,nbrxi,nxi,dcoeff,darg,const2,const3,
     *               xx,itype)											!février 2004

      use param_section,LAM_=>LAM,CONST_=>CONST,COEFF_=>COEFF,
     *DARG_=>DARG,DCOEFF_=>DCOEFF,CONST2_=>CONST2,CONST3_=>CONST3,Z_=>Z,
     *A_=>A,B_=>B,ITYPE_=>ITYPE,ETA_=>ETA
      
      implicit double precision(a-h,o-z)
      double precision lam(8)
      dimension const(74),arg(8),coeff(32,41),darg(8,9),
     *  dcoeff(32,33,9),const2(6,9),const3(2,10),nxi(9)
      dimension xx(400)
      call coee(const,arg,coeff,lam,eta,eta2,mt,m,z,x,w,a,b,c,f,
     *          h,g,impr,q,nbrxi,nxi,dcoeff,darg,const2,const3,iopti,
     *  xx(1) ,xx(5), xx(9),xx(13),xx(17),xx(21),xx(25),xx(29), ! au1 à au8
     *  xx(33),xx(41),xx(49),xx(57),                            !arg2 à arg5
     *  xx(65),xx(66),xx(67),xx(68),xx(69),xx(70),xx(71),xx(72),xx(73),
     *  xx(74),xx(75),xx(76),xx(77),xx(78),xx(79),xx(80),xx(81),xx(82),
     *  xx(83),xx(84),xx(85),xx(86),xx(87),xx(88),xx(89),xx(90),xx(91),
     *  xx(92),xx(93),xx(94),xx(95),xx(96),xx(97),xx(98),xx(99),xx(100),
     *  xx(101),xx(102),xx(103),xx(104),xx(105),xx(106),xx(107),xx(108),
     *  xx(109),xx(110),xx(111),xx(112),xx(113),xx(114),xx(115),xx(116),
     *  xx(117),xx(118),xx(119),xx(120),xx(121),xx(122),xx(123),xx(124),
     *  xx(125),xx(126),xx(127),xx(128),xx(129),xx(130),xx(131),xx(132),
     *  xx(133),xx(134),xx(135),xx(136),xx(137),xx(138),xx(139),xx(140),
     *  xx(141),xx(142),xx(143),xx(144),xx(145),xx(146),itype)
	return
	end

c***********************************************************************
c***********************************************************************

      subroutine coee(const,arg,coeff,lam,eta,eta2,mt,m,z,x,w,a,b,c,f,
     *           h,g,impr,q,nbrxi,nxi,dcoeff,darg,const2,const3,iopti,
     *  au1,au2,au3,au4,au5,au6,au7,au8,arg2,arg3,arg4,arg5,
     *  qb,qc,qd,qe,qf,qg,qh,qi,qj,qjj,qk,qk3,ql,ql2,qm,ss,tt,lamb,p,
     *  pp,r,rr,dp,dpp,dr,drr,aux1,aux2,aux3,aux4,aux5,aux6,aux7,an1,
     *  am1,aa,aaa,daa,daaa,bb,bbb,dbb,dbbb,sa,sb,sc,sd,se,sf,sg,sl,
     *  r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r14,r15,r16,r17,
     *  s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,itype)
	
	use param_section, ONLY : iu_11,iboat

      implicit double precision(a-h,o-z)
      double precision lam(8),lamb
      dimension const(74),arg(8),coeff(32,41),isign(41),darg(8,9),
     *  dcoeff(32,33,9),const2(6,9),const3(2,10),isign2(33),nxi(9)
      dimension au1(4),au2(4),au3(4),au4(4),au5(4),au6(4),au7(4),
     *          au8(4),arg2(8),arg3(8),arg4(8),arg5(8)
      data isign/1,-1,-1,1,1,-1,-1,1,1,-1,-1,-1,1,-1,1,1,
     *           1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
     *           1,-1,-1,1,-1/
      data isign2/1,-1,-1,1,-1,1,-1,1,1,-1,-1,1,-1,
     *            1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1/
c***********************************************************************
c     subroutine coe
c     subroutine de calcul des coefficients des elements de reduction
c     par combinaison des coefficients de l'equation de deplacement w.
c                                     créer (lbr3): thèse (ph. rigo)
c     modif : 12-6-95                       (lbr5): janvier 94
c             13-4-99						  (lbr5.1):avril  99
c***********************************************************************
c  input:
c      var. de concept. = epaisseur du bordage (k=1)
c      ---------------------------------------------
c        const(1) = d  -> const2(1,1)=d(d)/d(épaiss)
c        const(2) = k  -> const2(2,1)=d(k)/d(épaiss)
c        const(4) = hy -> const2(3,1)=d(hy)/d(épaiss)   cadre
c        const(6) = ry -> const2(4,1)=d(ry)/d(épaiss)   cadre
c        const(10)= hx -> const2(5,1)=d(hx)/d(épaiss)   raidisseur
c        const(12)= rx -> const2(6,1)=d(rx)/d(épaiss)   raidisseur

c        traverses  hx -> const3(1,mt)=d(h)/d(épaiss)
c                   rx -> const3(2,mt)=d(r)/d(épaiss)

c      var. de concept. relative aux aig.(k=2,5) et aux raid.(k=6,9).
c      --------------------------------------------------------------
c           cadres                         raidisseurs
c      constante      dérivée(k=2,5)   constante       dérivée(k=6,9)
c    1 const(3)= oy -> const2(1,k)     const( 9)= ox -> const2(1,k)
c    2 const(4)= hy -> const2(2,k)     const(10)= hx -> const2(2,k)
c    3 const(5)= sy -> const2(3,k)     const(11)= sx -> const2(3,k)
c    4 const(6)= ry -> const2(4,k)     const(12)= rx -> const2(4,k)
c    5 const(7)= ty -> const2(5,k)     const(13)= tx -> const2(5,k)
c    6 const(8)= ly -> const2(6,k)     const(14)= lx -> const2(6,k)

c      a= d (1+eta)/2+sx+sy         z= d (1-eta)/2+(sx+sy)
c      b= (k+rx)                    g= d*eta
c      c= (d+ox)                    h= d + 2(sx+sy)
c      x= (k+ry)                    f= lx + ly
c      w= (d+oy)                    eta2 = (1-eta)/2
c                                   eta3 = (1+eta)/2

c      au1(i)=2 a b           avec a=alpha=arg(2i-1) et b=béta=arg(2i)
c      au2(i)=a**2-b**2
c      au3(i)=-4(a**3 b - a b**3)
c      au4(i)=a**4+b**4-6 a**2 b**2
c      au5(i)=+(a**3 - 3 a b**2)
c      au6(i)= -(3 a**2 b - b**3)
c      au7(i)= a**5-10 a**3 b**2 + 5 a b**4
c      au8(i)= b**5-10 b**3 a**2 + 5 b a**4

c  output: (les coef. coeff correspondent à :
c     1 2 3  4  5   6   7  8  9  10  11  12 13 14 15 16 17   18  37  38   39 40  41
c     u v wø ny nx nyx nxy my mx myx mxy qy qx ry rx w x0dy z0dy wøø wøøø uø uøø vø

c     dérivées de coeff (càd dcoeff) sont dans l'ordre suivant:
c     1 2 3  4   5  6  7   8  9   10   11 12 13      14 15  à  32  33
c     u v wø ny nyx my ry  w  wøø wøøø uø vø  vøø    xo  zo    xo  zo


	if(itype.eq.5)then												!février 2004
	eta=0.000														!février 2004
	const(1)=0														!février 2004
	const(2)=0														!février 2004
	endif															!février 2004

      lamb=lam(1)                                                       
      eta3=(1.+eta)/2.                                 ! (1+u)/2
      qb=const(2)+const(13)                            ! k+tx
      qc=const(2)+const(7)                             ! k+ty
      qd=const(2)*(2.-eta)+const(13)+const(7)          ! k(2-u)+tx+ty
      qe=const(2)*(1.-eta)+const(13)                   ! k(1-u)+tx
      qf=const(2)*(1.-eta)+const(7)                    ! k(1-u)+ty
      qg=(z*lamb/q-lam(3)*const(10))                   ! z*l/q - hx l**3
      qh=lam(5)*const(10)+lam(3)*g/q                   ! hx l**5 + l**3 d*eta/q
      qi=w*c-g*h      
      qjj=-const(10)*lam(4)-g*lam(2)/q
      qj=qjj*a + w*c*lam(2)/q
      qk=c*const(4)*lam(2)+w*z/q
      qk3=3.*qk
      ql=w*qg
      ql2=2.*ql
      qm=z*qh
      ss=const(5)+const(11)						!sx+sy		!février 2004
      tt=const(7)+const(13)					    !tx+ty		!février 2004

      s1=c*eta2+z
      s2=w*eta2+z 		 	
      s3=c+w-g-eta*h  
      s4=4.*w*z
      s5=2.*lam(2)*qi
      s6=lamb*const(4)*a
      s7=w*eta2*lamb/q + qg
      s8=lamb*const(4)*eta3
      s9=5.*const(4)*z 
      s10=qjj*eta3 - (eta*a-(w+c))*lam(2)/q
      s11=const(4)*lam(2)+s2/q 
      s12=z*lam(3)*eta/q+eta2*qh 
      s13=const(9)*ss*lam(4)								!février 2004
      s14=const(3)*const(9)*lam(2)						!février 2004
      s15=const(3)*ss										!février 2004

      do 13 i=1,m 
      if(nbrxi.eq.0) then
        do 1 j=2*i-1,2*i
        arg2(j)=arg(j)*arg(j)
        arg3(j)=arg2(j)*arg(j)
        arg4(j)=arg2(j)*arg2(j)
   1    arg5(j)=arg3(j)*arg2(j)
        au1(i)=2.*arg(2*i-1)*arg(2*i)                             ! 2 a b
        au2(i)=arg2(2*i-1)-arg2(2*i)                              ! a**2-b**2
        au3(i)=-2.*au1(i)*au2(i)                                  ! -4(a**3 b - a b**3)
        au4(i)=arg4(2*i-1)+arg4(2*i)-6.*arg2(2*i-1)*arg2(2*i)     ! a**4+b**4-6 a**2 b**2
        au5(i)=arg3(2*i-1)-3.*arg(2*i-1)*arg2(2*i)                ! +(a**3 - 3 a b**2)
        au6(i)=arg3(2*i)-3.*arg(2*i)*arg2(2*i-1)                  ! -(3 a**2 b - b**3)
        au7(i)=arg5(2*i-1)-10.*arg3(2*i-1)*arg2(2*i)              !
     *                  +5.*arg(2*i-1)*arg4(2*i)                  ! a**5-10 a**3 b**2 + 5 a b**4
        au8(i)=arg5(2*i)-10.*arg3(2*i)*arg2(2*i-1)                !
     *                  +5.*arg(2*i)*arg4(2*i-1)                  ! b**5-10 b**3 a**2 + 5 b a**4
      else
c       ces coéf. sont déjà calculés dans subr complex (voir common/allong/)
        if(impr.ne.0) then
        write(iu_11(iboat),*) ' '
        write(iu_11(iboat),*) 'solution nø= ',i
        write(iu_11(iboat),*) '*****************'
        endif
      endif
      ia=2*i-1
      ib=2*i
      i1=8*i-7
      i2=8*i-6
      r1=au2(i)*w*lamb/q - au4(i)*const(4)*lamb + qh
      r2=-qjj*arg(ia) + au5(i)*w/q - au7(i)*const(4)
      r3= qjj*arg(ib)   + au6(i)*w/q + au8(i)*const(4)
      r4= lam(4)*c + au4(i)*w + au2(i)*2.*lam(2)*g 
      r5= au4(i)*z - au2(i)*lam(2)*c
      r6=au3(i)*z + au1(i)*lam(2)*c
      r7=-au1(i)*2.*lam(2)*g + au3(i)*w
      r8=au1(i)*w*lamb/q + au3(i)*lamb*const(4)
      r9=-arg(ia)*c*lam(2)/q + au5(i)*z/q
      r10=au5(i)*c*lam(2) - au7(i)*z
      r11= arg(ib)*c*lam(2)/q + au6(i)*z/q 
      r12= au6(i)*c*lam(2) + au8(i)*z
      r14=-au2(i)*lam(2)*w + lam(4)*z 
      r15=r14*lamb
      r16=-arg(ia)*w*lam(2)/q + au5(i)*const(4)*lam(2)
      r17= arg(ib)  *w*lam(2)/q + au6(i)*const(4)*lam(2)
      r18=-w*lam(3)*au2(i)+z*lam(5)
      r19=lam(2)*c*au5(i)-z*au7(i)
      r20=lam(2)*c*au6(i)+z*au8(i)

 	if(itype.ne.5)then												!février 2004
      am1=z* (w*au4(i)+c*lam(4)) - qi*lam(2)*au2(i)
      an1=w*z*au3(i) + qi*lam(2)*au1(i)
        aux1=am1*am1+an1*an1
        aux2=aux1*aux1                     !   calcul u
      p= ql*au2(i) - au4(i)*s6 + qm        !   ********
      r=-ql*au1(i) - au3(i)*s6
        aux3=p*am1+r*an1
        aux4=r*am1-p*an1
      coeff(i1,1)=aux3/aux1
      coeff(i2,1)=aux4/aux1
      aa=coeff(i1,1)
      bb=coeff(i2,1)

      pp=-arg(ia)*qj  + au5(i)*qk - au7(i)*const(4)*z    !   calcul v
      rr= arg(ib)*qj  + au6(i)*qk + au8(i)*const(4)*z    !   ********
      aux5=pp*am1+rr*an1
      aux6=rr*am1-pp*an1                                              
      coeff(i1,2)=aux5/aux1
      coeff(i2,2)=aux6/aux1
      aaa=coeff(i1,2)
      bbb=coeff(i2,2)
	else																	!février 2004			
	am1=s13-(s14*au2(i))+(s15*au4(i))										!février 2004
      an1=const(3)*ss*au3(i)+const(3)*const(9)*lam(2)*au1(i)					!février 2004
        aux1=am1*am1+an1*an1													!février 2004
        aux2=aux1*aux1					                    !   calcul u	!février 2004
      p=-const(10)*const(3)*lam(3)*au2(i)+const(10)*ss*lam(5) !   ********	!février 2004
     *	+((const(3)*ss*lamb*au2(i))/q)-const(4)*ss*lamb*au4(i)				!février 2004
      r=const(10)*const(3)*lam(3)*au1(i)										!février 2004
     *	-((const(3)*ss*lamb*au1(i))/q)-const(4)*ss*lamb*au3(i)				!février 2004
        aux3=p*am1+r*an1														!février 2004
        aux4=r*am1-p*an1														!février 2004
      coeff(i1,1)=aux3/aux1													!février 2004
      coeff(i2,1)=aux4/aux1													!février 2004
      aa=coeff(i1,1)															!février 2004
      bb=coeff(i2,1)															!février 2004
      pp=const(10)*ss*lam(4)*arg(ia)								!  calcul v	!février 2004
     *	+const(9)*const(4)*lam(2)*au5(i)+((const(3)*ss*au5(i))/q)	    !février 2004
     *	-const(4)*ss*au7(i)												!février 2004
      rr=-const(10)*ss*lam(4)*arg(ib)											!février 2004
     *	+((const(3)*const(9)*lam(2)*arg(ib))/q)							!février 2004
     *	+const(9)*const(4)*lam(2)*au6(i)+((const(3)*ss*au6(i))/q)		!février 2004  
     *	+const(4)*ss*au8(i)												!février 2004

        aux5=pp*am1+rr*an1													!février 2004
        aux6=rr*am1-pp*an1													!février 2004                                 
      coeff(i1,2)=aux5/aux1													!février 2004
      coeff(i2,2)=aux6/aux1													!février 2004
      aaa=coeff(i1,2)															!février 2004
      bbb=coeff(i2,2)															!février 2004
	endif																	!février 2004

      sa=-arg(ia)*aaa - arg(ib)*bbb
      sb=-arg(ia)*bbb + arg(ib)*aaa
      sc=-arg(ia)*aa  - arg(ib)*bb
      sd=-arg(ia)*bb  + arg(ib)*aa
      se= au2(i) *aaa + au1(i) *bbb
      sf= au2(i) *bbb - au1(i) *aaa

      if(iopti.eq.0) goto 567
      do 15 kk=1,nbrxi
      k=nxi(kk)
	if(itype.ne.5) then														!février 2004
      goto(16,17,17,17,17,18,18,18,18),k										!février 2004
	else																	!février 2004
	goto(25,25,25,25,25),k													!février 2004
	endif																	!février 2004

c     epaisseur du bordage (k=1)
  16  dam1=const2(1,1)*  ( lam(4)*s1+au4(i)*s2-lam(2)*au2(i)*s3 )
     *  + s4* (au5(i) *darg(ia,1)+au6(i) *darg(ib,1))
     *  - s5* (arg(ia)*darg(ia,1)-arg(ib)*darg(ib,1))
      dan1=s4* (au6(i) *darg(ia,1)-au5(i) *darg(ib,1))
     *   + s5* (arg(ib)*darg(ia,1)+arg(ia)*darg(ib,1))
     *   + const2(1,1) * ( s2*au3(i)+lam(2)*au1(i)*s3 )
      dp= ql2*  (arg(ia)*darg(ia,1)-arg(ib)*darg(ib,1))
     *  - 4.*s6*(au5(i) *darg(ia,1)+au6(i) *darg(ib,1))
     *  + const2(1,1) * (au2(i)*s7-au4(i)*s8+s12)
     *  + r18*const2(5,1)-lamb*a*au4(i)*const2(3,1)

      dr= - ql2 *  (arg(ia)*darg(ib,1)+arg(ib)*darg(ia,1))
     *    -4.*s6 * (au6(i) *darg(ia,1)- au5(i)*darg(ib,1))
     *    - const2(1,1)*( au1(i)*s7 + au3(i)*s8 )
     *    + w*lam(3)*au1(i)*const2(5,1)-lamb*a*au3(i)*const2(3,1)
      dpp= - qj*darg(ia,1) + qk3*(au2(i)*darg(ia,1)-au1(i)*darg(ib,1))
     *                     - s9 *(au4(i)*darg(ia,1)+au3(i)*darg(ib,1))
     * +const2(1,1)* (-arg(ia)*s10 + au5(i)*s11 - au7(i)*const(4)*eta2)
     * + lam(4)*a*arg(ia)*const2(5,1)+r19*const2(3,1)
      drr= qj*darg(ib,1) + qk3 *(-au2(i)*darg(ib,1)-au1(i)*darg(ia,1))
     *                   + s9  *( au4(i)*darg(ib,1)-au3(i)*darg(ia,1))
     * +const2(1,1)* (arg(ib)*s10 + au6(i)*s11 + au8(i)*const(4)*eta2)
     * - lam(4)*a*arg(ib)*const2(5,1)+r20*const2(3,1)
      goto 19

c     cadre - ame - semelle - entredistance (k=2,3,4,5)
   17 dam1= const2(3,k)* r4 + const2(1,k)* r5
     *  + s4*(au5(i) *darg(ia,k)+ au6(i) *darg(ib,k))
     *  - s5*(arg(ia)*darg(ia,k)- arg(ib)*darg(ib,k))
      dan1=s4* ( au6(i) *darg(ia,k)-au5(i) *darg(ib,k))
     *   + s5* ( arg(ib)*darg(ia,k)+arg(ia)*darg(ib,k))
     *   + const2(1,k)*r6   -  const2(3,k)*r7
      dp= ql2 * (arg(ia)*darg(ia,k)-arg(ib)*darg(ib,k))
     *  - 4.*s6*(au5(i) *darg(ia,k)+au6(i) *darg(ib,k))
     *+const2(3,k)*r1 +const2(1,k)*au2(i)*qg -const2(2,k)*au4(i)*lamb*a
      dr= - ql2 * (arg(ia)*darg(ib,k)+arg(ib)*darg(ia,k))
     *    - 4.*s6*(au6(i) *darg(ia,k)-au5(i) *darg(ib,k))
     *-const2(1,k)*qg*au1(i) -const2(2,k)*au3(i)*lamb*a -const2(3,k)*r8
      dpp=-darg(ia,k) * qj + qk3* (au2(i)*darg(ia,k)-au1(i)*darg(ib,k))
     *                     - s9 * (au4(i)*darg(ia,k)+au3(i)*darg(ib,k))
     *  + const2(1,k)*r9 + const2(2,k)*r10 + const2(3,k)*r2
      drr= darg(ib,k)*qj + qk3* (-au2(i)*darg(ib,k)-au1(i)*darg(ia,k))
     *                   + s9 * ( au4(i)*darg(ib,k)-au3(i)*darg(ia,k))
     *  + const2(1,k)*r11  + const2(2,k)*r12 + const2(3,k)*r3
      goto 19

c     raidisseur - ame - semelle - entredistance (k=6,7,8,9)
  18  dam1= const2(3,k)*r4    +const2(1,k)*r14
     *  + s4*( au5(i)   *darg(ia,k)+ au6(i) *darg(ib,k))
     *  - s5*(arg(ia)*darg(ia,k)-arg(ib)*darg(ib,k))
      dan1=s4* ( au6(i) *darg(ia,k)-  au5(i)  *darg(ib,k))
     *   + s5* (arg(ib)*darg(ia,k)+arg(ia)*darg(ib,k))
     *    + const2(1,k)* au1(i)*lam(2)*w  + const2(3,k)*r7
      dp= ql2 * (arg(ia)*darg(ia,k)-arg(ib)*darg(ib,k))
     *  - 4.*s6*(au5(i) *darg(ia,k)+au6(i) *darg(ib,k))
     *  + const2(3,k)*r1 + const2(2,k)*r15
      dr= - ql2 * (arg(ia)*darg(ib,k)+arg(ib)*darg(ia,k))
     *    - 4.*s6*(au6(i) *darg(ia,k)-au5(i) *darg(ib,k))
     *   + const2(2,k)* w*lam(3)*au1(i) - const2(3,k)*r8
      dpp=-darg(ia,k) * qj + qk3* (au2(i)*darg(ia,k)-au1(i)*darg(ib,k))
     *                     - s9 * (au4(i)*darg(ia,k)+au3(i)*darg(ib,k))
     *+ const2(1,k)*r16 +const2(2,k)*arg(ia)*lam(4)*a  +const2(3,k)*r2
      drr= darg(ib,k)*qj + qk3* (-au2(i)*darg(ib,k)-au1(i)*darg(ia,k))
     *                   + s9 * ( au4(i)*darg(ib,k)-au3(i)*darg(ia,k))
     *+ const2(1,k)*r17 -const2(2,k)*arg(ib)*lam(4)*a  +const2(3,k)*r3

	goto 19 !juillet04
c
c     epontille - ame - semelle - entredistance (k=1,2,3,4,5)				!février 2004
c
  25  dam1= const2(3,k)* (lam(4)*const(9)+au4(i)*const(3))				!février 2004
     *  + const2(1,k)* (au4(i)*ss - au2(i)*lam(2)*const(9))				!février 2004
     *  + 4.*const(3)*ss*(au5(i) *darg(ia,k)+ au6(i) *darg(ib,k))			!février 2004
     *  - 2.*lam(2)*const(9)*const(3)*(arg(ia)*darg(ia,k)					!février 2004
     *  - arg(ib)*darg(ib,k))												!février 2004
      dan1=(4.*const(3)*ss* ( au6(i) *darg(ia,k)-au5(i) *darg(ib,k))		!février 2004
     *   + 2.*lam(2)*const(9)*const(3)* ( arg(ib)*darg(ia,k)				!février 2004
     *   +arg(ia)*darg(ib,k))+ const2(1,k)*(au3(i)*ss						!février 2004
     *   +au1(i)*lam(2)*const(9))+const2(3,k)* au3(i)*const(3))			!février 2004
      dp=+2*const(3)*(ss*(lamb/q)-const(10)*lam(3)) *(arg(ia)*darg(ia,k)	!février 2004
     *  -arg(ib)*darg(ib,k))												!février 2004
     *  +const2(3,k)*(au2(i)*const(3)*lamb/q +lam(5)*const(10))			!février 2004
     *  +const2(1,k)*au2(i)*(ss*lamb/q - const(10)*lam(3))				!février 2004
     *  -const2(2,k)*ss*lamb*au4(i)-const(4)*const2(3,k)*lamb*au4(i)		!février 2004
     *  -const(4)*ss*lamb*4.*(au5(i)*darg(ia,k)+au6(i)*darg(ib,k))		!février 2004	
      dr=-2.*const(3)*(ss*(lamb/q)-const(10)*lam(3))*(arg(ia)*darg(ib,k)	!février 2004
     *  +arg(ib)*darg(ia,k))-const2(3,k)*au1(i)*const(3)*lamb/q			!février 2004
     *  -const2(1,k)*(ss*lamb/q - const(10)*lam(3))*au1(i)				!février 2004
     *  -const2(2,k)*ss*lamb*au3(i)-const(4)*const2(3,k)*lamb*au4(i)		!février 2004
     *  -const(4)*ss*lamb*4.*(au6(i)*darg(ia,k)-au5(i)*darg(ib,k))		!février 2004									
      dpp=-darg(ia,k)*(-ss*const(10)*lam(4)+const(3)*const(9)*lam(2)/q)	!février 2004
     *  +(3.*const(3)*ss/q)* (au2(i)*darg(ia,k)-au1(i)*darg(ib,k))		!février 2004	
     *  + const2(1,k)*(-arg(ia)*const(9)*lam(2)/q+au5(i)*ss/q)			!février 2004
     *  + const2(3,k)*(const(10)*lam(4)*arg(ia)+au5(i)*const(3)/q)		!février 2004
     *  + const(4)*(const(9)*lam(2)*3.*(au2(i)*darg(ia,k)					!février 2004
     *  -au1(i)*darg(ib,k))-const2(3,k)*au7(i)-ss*5.*(au4(i)*darg(ia,k)	!février 2004	
     *  -au3(i)*darg(ib,k)))+const2(2,k)*(const(9)*lam(2)*au5(i)			!février 2004
     *  -ss*au7(i))														!février 2004
      drr= darg(ib,k)*(-ss*const(10)*lam(4)+const(3)*const(9)*lam(2)/q)	!février 2004
     *  + (3.*const(3)*ss/q)* (-au2(i)*darg(ib,k)-au1(i)*darg(ia,k))		!février 2004
     *  + const2(1,k)*(arg(ib)*const(9)*lam(2)/q + au6(i)*ss/q)			!février 2004
     *  + const2(3,k)*(-const(10)*lam(4)*arg(ib)+au6(i)*const(3)/q)		!février 2004
     *  +const(4)*(-const(9)*lam(2)*3.*(au2(i)*darg(ib,k)					!février 2004
     *  +au1(i)*darg(ia,k))+const2(3,k)*au8(i)+ss*5.*(au4(i)*darg(ib,k)	!février 2004
     *   -au3(i)*darg(ia,k)))+const2(2,k)*(const(9)*lam(2)*au6(i)			!février 2004
     *  +ss*au8(i))														!février 2004

   19 aux7=am1*dam1+an1*dan1
      daa=( aux1*(p*dam1+am1*dp+r*dan1+an1*dr) - 2.*aux3*aux7 )/aux2
      dbb=( aux1*(r*dam1+am1*dr-p*dan1-an1*dp) - 2.*aux4*aux7 )/aux2
      dcoeff(i1,1,k)=daa                                            ! d(u)/dxi
      dcoeff(i2,1,k)=dbb
      daaa=(aux1*(pp*dam1+am1*dpp+rr*dan1+an1*drr)-2.*aux5*aux7)/aux2
      dbbb=(aux1*(rr*dam1+am1*drr-pp*dan1-an1*dpp)-2.*aux6*aux7)/aux2
      dcoeff(i1,2,k)=daaa                                           ! d(v)/dxi
      dcoeff(i2,2,k)=dbbb
      
      dcoeff(i1,3,k)=-darg(ia,k)                                    ! d(wø)/dxi
      dcoeff(i2,3,k)= darg(ib,k)
      dcoeff(i1,8,k)=0.                                             ! d(w)/dxi
      dcoeff(i2,8,k)=0.
      dcoeff(i1,9,k)= 2.*(arg(ia)*darg(ia,k)-arg(ib)*darg(ib,k))    ! d(wøø)/dxi
      dcoeff(i2,9,k)=-2.*(arg(ia)*darg(ib,k)+arg(ib)*darg(ia,k))
      dcoeff(i1,10,k)= 3.*(-au2(i) *darg(ia,k)+au1(i) *darg(ib,k))  ! d(wøøø)/dxi
      dcoeff(i2,10,k)= 3.*( au2(i) *darg(ib,k)+au1(i) *darg(ia,k))
      dcoeff(i1,11,k)=-( arg(ia)  *daa  +  arg(ib)  *dbb +          ! d(uø)/dxi
     *                  darg(ia,k)* aa  + darg(ib,k)* bb    )
      dcoeff(i2,11,k)=+(-arg(ia)  *dbb  +  arg(ib)  *daa -
     *                  darg(ia,k)* bb  + darg(ib,k)* aa    )
      dcoeff(i1,12,k)=-( arg(ia)  *daaa +  arg(ib)  *dbbb +         ! d(vø)/dxi
     *                  darg(ia,k)* aaa + darg(ib,k)* bbb   )
      dcoeff(i2,12,k)=+(-arg(ia)  *dbbb +  arg(ib)  *daaa -
     *                  darg(ia,k)* bbb + darg(ib,k)* aaa   )
      dcoeff(i1,13,k)=  au2(i)*daaa +  au1(i)*dbbb +                ! d(vøø)/dxi
     *         2.*( arg(ia)*darg(ia,k)-arg(ib)*darg(ib,k) )*aaa +
     *         2.*( arg(ia)*darg(ib,k)+arg(ib)*darg(ia,k) )*bbb 
      dcoeff(i2,13,k)=  au2(i)*dbbb -  au1(i)*daaa +
     *         2.*( arg(ia)*darg(ia,k)-arg(ib)*darg(ib,k) )*bbb -
     *         2.*( arg(ia)*darg(ib,k)+arg(ib)*darg(ia,k) )*aaa 

      if(mt.gt.0) then
      du2a=  au2(i)*daa +  au1(i)*dbb +                           ! d(uøø)/dxi
     *         2.*( arg(ia)*darg(ia,k)-arg(ib)*darg(ib,k) )*aa +
     *         2.*( arg(ia)*darg(ib,k)+arg(ib)*darg(ia,k) )*bb 
      du2b=  au2(i)*dbb -  au1(i)*daa +
     *         2.*( arg(ia)*darg(ia,k)-arg(ib)*darg(ib,k) )*bb -
     *         2.*( arg(ia)*darg(ib,k)+arg(ib)*darg(ia,k) )*aa 
      dhtr=0.
      drtr=0.
      do 1401 itra=1,mt
      jtrav=6*(itra-1)
      jjtrav=2*(itra-1)
      if(k.eq.1) then
        dhtr=const3(1,itra)
        drtr=const3(2,itra)
      endif
      dcoeff(i1,14+jjtrav,k)=-daa*lam(2)*const(15+jtrav)            !  d(xo)/dxi
     *        + lam(3)*dhtr
     *          + const(17+jtrav)*(lamb*dcoeff(i1,12,k)+du2a)
      dcoeff(i2,14+jjtrav,k)=-dbb*lam(2)*const(15+jtrav)            !  d(xo)/dxi
     *          + const(17+jtrav)*(lamb*dcoeff(i2,12,k)+du2b)
      dcoeff(i1,15+jjtrav,k)=-lam(2)*const(19+jtrav)*dcoeff(i1,9,k) !  d(zo)/dxi
     *         - lam(3)*(dhtr*aa+const(16+jtrav)*daa) + lam(4)*drtr   	
     *         - lamb*const(20+jtrav) * (lamb*dcoeff(i1,12,k)+du2a)
      dcoeff(i2,15+jjtrav,k)= -lam(2)*const(19+jtrav)*dcoeff(i2,9,k)
     *   - lam(3)*(dhtr*bb+const(16+jtrav)*dbb)
     *   - lamb*const(20+jtrav)*(lamb*dcoeff(i2,12,k)+du2b)
 1401 continue
      endif


	if(itype.ne.5) then														!février 2004
      goto(20,21,21,21,21,22,22,22,22),k
	else																	!février 2004
	goto(26,26,26,26,26),k													!février 2004
	endif																	!février 2004
c     bordage
   20 dcoeff(i1,4,k)=-w*( darg(ia,k)*aaa + arg(ia)*daaa         !  d(ny)/dxi
     *                  + darg(ib,k)*bbb + arg(ib)*dbbb )
     * - const(4)*2*( arg(ia)*darg(ia,k) - arg(ib)*darg(ib,k) ) 
     * + const2(1,k)*(1./q+sa) - lamb*eta*(const(1)*daa+aa*const2(1,k))
     * - const2(3,k)*au2(i)
      dcoeff(i2,4,k)=-w*( darg(ia,k)*bbb + arg(ia)*dbbb 	
     *                  - darg(ib,k)*aaa - arg(ib)*daaa )
     * + const(4)*2*( arg(ia)*darg(ib,k) + arg(ib)*darg(ia,k) )
     * + const2(1,k) * sb - lamb*eta*(const(1)*dbb+bb*const2(1,k))
     * + const2(3,k)*au1(i)
      dcoeff(i1,5,k)= const2(1,k)*eta2*(sc+lamb*aaa)           !  d(nxy)/dxi
     * + z*( -arg(ia)*daa - darg(ia,k)*aa
     *       -arg(ib)*dbb - darg(ib,k)*bb + lamb*daaa)
      dcoeff(i2,5,k)= const2(1,k)*eta2*(sd+lamb*bbb)  
     * + z*(  arg(ib)*daa + darg(ib,k)*aa
     *       -arg(ia)*dbb - darg(ia,k)*bb +lamb*dbbb)
      dcoeff(i1,6,k)= (arg(ia)*daaa + darg(ia,k)*aaa           ! d(my)/dxi
     *               + arg(ib)*dbbb + darg(ib,k)*bbb ) * const(4)
     * + x*2.* ( arg(ia)*darg(ia,k) - arg(ib)*darg(ib,k) ) 
     * + const2(2,k) * (au2(i)-lam(2)*eta)
     * + const2(4,k)*au2(i) - const2(3,k)*(1./q+sa)
      dcoeff(i2,6,k)=-(  arg(ib)*daaa + darg(ib,k)*aaa 	
     *                 - arg(ia)*dbbb - darg(ia,k)*bbb ) * const(4)
     * - x*2.* ( arg(ia)*darg(ib,k) + arg(ib)*darg(ia,k) ) 
     * - (const2(2,k)+const2(4,k)) * au1(i) - const2(3,k)*sb
      dcoeff(i1,7,k)= const2(2,k) * (lam(2)*(2.-eta)*arg(ia)-au5(i))  ! d(ry)/dxi
     * + const(4)* ( -2.*aaa* (arg(ia)*darg(ia,k)-arg(ib)*darg(ib,k))    
     *              - 2.*bbb* (arg(ia)*darg(ib,k)+arg(ib)*darg(ia,k)) 
     *              - daaa*au2(i) - dbbb*au1(i) + darg(ia,k)/q   )
     * + lamb*f* (-lamb*daaa + arg(ia)*daa + darg(ia,k)*aa 
     *                       + arg(ib)*dbb + darg(ib,k)*bb )
     * + lam(2)*qd*arg(ia) -3.*x* (darg(ia,k)*au2(i)-darg(ib,k)*au1(i))
     * + const2(3,k)*(arg(ia)/q-se)  - const2(4,k)*au5(i)
      dcoeff(i2,7,k)= const2(2,k) * (-lam(2)*(2.-eta)*arg(ib)-au6(i))
     * - const(4)* (  2.*bbb* (arg(ia)*darg(ia,k)-arg(ib)*darg(ib,k))
     *              - 2.*aaa* (arg(ia)*darg(ib,k)+arg(ib)*darg(ia,k))
     *              + dbbb*au2(i) - daaa*au1(i) + darg(ib,k)/q   )
     * + lamb*f* (-lamb*dbbb + arg(ia)*dbb + darg(ia,k)*bb
     *                       - arg(ib)*daa - darg(ib,k)*aa )
     * - lam(2)*qd*arg(ib) +3.*x* (darg(ib,k)*au2(i)+darg(ia,k)*au1(i))
     * - const2(3,k)*(arg(ib)/q+sf)  - const2(4,k)*au6(i)

      goto 23
c     cadres
   21 dcoeff(i1,4,k)=-w*( darg(ia,k)*aaa + arg(ia)*daaa    ! d(ny)/dxi
     *                  + darg(ib,k)*bbb + arg(ib)*dbbb )
     * - const(4)*2* ( arg(ia)*darg(ia,k) - arg(ib)*darg(ib,k) )	
     * + const2(1,k)*(1./q+sa) - daa*lamb*g - const2(2,k)*au2(i)
      dcoeff(i2,4,k)=-w*( darg(ia,k)*bbb + arg(ia)*dbbb 	
     *                  - darg(ib,k)*aaa - arg(ib)*daaa )
     * + const(4)*2* ( arg(ia)*darg(ib,k) + arg(ib)*darg(ia,k) )
     * + const2(1,k)*sb - dbb*lamb*g + const2(2,k)*au1(i)
      dcoeff(i1,5,k)= const2(3,k)* (sc+lamb*aaa)             ! d(nxy)/dxi
     * + z*( -arg(ia)*daa - darg(ia,k)*aa
     *       -arg(ib)*dbb - darg(ib,k)*bb + lamb*daaa)
      dcoeff(i2,5,k)= const2(3,k)* (sd+lamb*bbb)  
     * + z*(  arg(ib)*daa + darg(ib,k)*aa
     *       -arg(ia)*dbb - darg(ia,k)*bb +lamb*dbbb)
      dcoeff(i1,6,k)= ( arg(ia)*daaa + darg(ia,k)*aaa        ! d(my)/dxi
     *                + arg(ib)*dbbb + darg(ib,k)*bbb ) * const(4)
     * + x*2.* ( arg(ia)*darg(ia,k) - arg(ib)*darg(ib,k) ) 
     * + const2(4,k)*au2(i) - const2(2,k)*(1./q+sa)
      dcoeff(i2,6,k)=-(  arg(ib)*daaa + darg(ib,k)*aaa 	
     *                 - arg(ia)*dbbb - darg(ia,k)*bbb ) * const(4)
     * - x*2.* ( arg(ia)*darg(ib,k) + arg(ib)*darg(ia,k) ) 
     * - const2(4,k)*au1(i) - const2(2,k)*sb
      dcoeff(i1,7,k)= const2(5,k)*lam(2)*arg(ia) - const2(4,k)*au5(i) ! d(ry)/dxi
     * + const2(2,k)*(arg(ia)/q-se) + const2(6,k)*lamb*(sc+lamb*aaa)
     * + const(4)* ( -2.*aaa* (arg(ia)*darg(ia,k)-arg(ib)*darg(ib,k))    
     *              - 2.*bbb* (arg(ia)*darg(ib,k)+arg(ib)*darg(ia,k)) 
     *              - daaa*au2(i) - dbbb*au1(i) + darg(ia,k)/q   )
     * + lamb*f* (-lamb*daaa + arg(ia)*daa + darg(ia,k)*aa 
     *                       + arg(ib)*dbb + darg(ib,k)*bb )
     * + lam(2)*qd*arg(ia) -3.*x* (darg(ia,k)*au2(i)-darg(ib,k)*au1(i))
      dcoeff(i2,7,k)=-const2(5,k)*lam(2)*arg(ib)  -  const2(4,k)*au6(i)
     * - const2(2,k)*(arg(ib)/q+sf) - const2(6,k)*lamb*(sd+lamb*bbb)
     * - const(4)* (  2.*bbb* (arg(ia)*darg(ia,k)-arg(ib)*darg(ib,k))
     *              - 2.*aaa* (arg(ia)*darg(ib,k)+arg(ib)*darg(ia,k))
     *              + dbbb*au2(i) - daaa*au1(i) + darg(ib,k)/q   )
     * + lamb*f* (-lamb*dbbb + arg(ia)*dbb + darg(ia,k)*bb
     *                       - arg(ib)*daa - darg(ib,k)*aa )
     * - lam(2)*qd*arg(ib) +3.*x* (darg(ib,k)*au2(i)+darg(ia,k)*au1(i))

      goto 23
c     raidisseurs
   22 dcoeff(i1,4,k)=-w* ( darg(ia,k)*aaa + arg(ia)*daaa     ! d(ny)/dxi
     *                   + darg(ib,k)*bbb + arg(ib)*dbbb )
     * -const(4)*2* (arg(ia)*darg(ia,k)-arg(ib)*darg(ib,k)) -daa*lamb*g
      dcoeff(i2,4,k)=-w*( darg(ia,k)*bbb + arg(ia)*dbbb 	
     *                  - darg(ib,k)*aaa - arg(ib)*daaa )
     * +const(4)*2* (arg(ia)*darg(ib,k)+arg(ib)*darg(ia,k)) -dbb*lamb*g
      dcoeff(i1,5,k)= const2(3,k)*(sc+lamb*aaa)                ! d(nxy)/dxi
     *        + z*( -arg(ia)*daa - darg(ia,k)*aa
     *              -arg(ib)*dbb - darg(ib,k)*bb + lamb*daaa)
      dcoeff(i2,5,k)= const2(3,k)*(sd+lamb*bbb)  
     *        + z*(  arg(ib)*daa + darg(ib,k)*aa
     *              -arg(ia)*dbb - darg(ia,k)*bb +lamb*dbbb)
      dcoeff(i1,6,k)= ( arg(ia)*daaa + darg(ia,k)*aaa          ! d(my)/dxi
     *                + arg(ib)*dbbb + darg(ib,k)*bbb ) * const(4)
     *   + x*2.* ( arg(ia)*darg(ia,k) - arg(ib)*darg(ib,k) ) 
      dcoeff(i2,6,k)=-(  arg(ib)*daaa + darg(ib,k)*aaa 	
     *                 - arg(ia)*dbbb - darg(ia,k)*bbb ) * const(4)
     *   - x*2.* ( arg(ia)*darg(ib,k) + arg(ib)*darg(ia,k) ) 
      dcoeff(i1,7,k)= const2(5,k)* lam(2)*arg(ia)              ! d(ry)/dxi
     *              - const2(6,k)* lamb*(sc+lamb*aaa)
     * + const(4)* (- 2.*aaa* (arg(ia)*darg(ia,k)-arg(ib)*darg(ib,k))    
     *              - 2.*bbb* (arg(ia)*darg(ib,k)+arg(ib)*darg(ia,k)) 
     *              - daaa*au2(i) - dbbb*au1(i) + darg(ia,k)/q   )
     * + lamb*f* (-lamb*daaa + arg(ia)*daa + darg(ia,k)*aa 
     *                       + arg(ib)*dbb + darg(ib,k)*bb )
     * + lam(2)*qd*arg(ia) -3.*x* (darg(ia,k)*au2(i)-darg(ib,k)*au1(i))
      dcoeff(i2,7,k)= - const2(5,k)*lam(2)*arg(ib)
     *                - const2(6,k)*lamb*(sd+lamb*bbb)
     * - const(4)* (  2.*bbb* (arg(ia)*darg(ia,k)-arg(ib)*darg(ib,k))    
     *              - 2.*aaa* (arg(ia)*darg(ib,k)+arg(ib)*darg(ia,k)) 
     *              + dbbb*au2(i) - daaa*au1(i) + darg(ib,k)/q   )
     * + lamb*f* (-lamb*dbbb + arg(ia)*dbb + darg(ia,k)*bb 
     *                       - arg(ib)*daa - darg(ib,k)*aa )
     * - lam(2)*qd*arg(ib) +3.*x* (darg(ib,k)*au2(i)+darg(ia,k)*au1(i))
	goto 23						
c
c	epontilles																		!février 2004
 26	dcoeff(i1,4,k)=-const(3)*( darg(ia,k)*aaa + arg(ia)*daaa      ! d(ny)/dxi		!février 2004
     *                  + darg(ib,k)*bbb + arg(ib)*dbbb )								!février 2004
     * - const(4)*2.* ( arg(ia)*darg(ia,k) - arg(ib)*darg(ib,k) )						!février 2004
     * + const2(1,k)*(1./q+sa) - const2(2,k)*au2(i)									!février 2004		
      dcoeff(i2,4,k)=-const(3)*( darg(ia,k)*bbb + arg(ia)*dbbb 						!février 2004
     *                  - darg(ib,k)*aaa - arg(ib)*daaa )								!février 2004
     * + const(4)*2.* ( arg(ia)*darg(ib,k) + arg(ib)*darg(ia,k) )						!février 2004
     * + const2(1,k)*sb + const2(2,k)*au1(i)											!février 2004
      dcoeff(i1,5,k)= const2(3,k)* (sc+lamb*aaa)						! d(nxy)/dxi	!février 2004
     * + ss*( -arg(ia)*daa - darg(ia,k)*aa											!février 2004
     *       -arg(ib)*dbb - darg(ib,k)*bb + lamb*daaa)								!février 2004
      dcoeff(i2,5,k)= const2(3,k)* (sd+lamb*bbb)										!février 2004
     * + ss*(  arg(ib)*daa + darg(ib,k)*aa											!février 2004
     *       -arg(ia)*dbb - darg(ia,k)*bb +lamb*dbbb)									!février 2004
      dcoeff(i1,6,k)= ( arg(ia)*daaa + darg(ia,k)*aaa				  ! d(my)/dxi		!février 2004
     *                + arg(ib)*dbbb + darg(ib,k)*bbb ) * const(4)					!février 2004
     * + const(6)*2.* ( arg(ia)*darg(ia,k) - arg(ib)*darg(ib,k) )						!février 2004			
     * + const2(4,k)*au2(i) - const2(2,k)*(1./q+sa)									!février 2004
      dcoeff(i2,6,k)=-(  arg(ib)*daaa + darg(ib,k)*aaa 								!février 2004
     *                 - arg(ia)*dbbb - darg(ia,k)*bbb ) * const(4)					!février 2004
     * - const(6)*2.* ( arg(ia)*darg(ib,k) + arg(ib)*darg(ia,k) )						!février 2004
     * - const2(4,k)*au1(i) - const2(2,k)*sb											!février 2004
      dcoeff(i1,7,k)= const2(5,k)*lam(2)*arg(ia) - const2(4,k)*au5(i) 	! d(ry)/dxi	!février 2004
     * + const2(2,k)*(arg(ia)/q-se)													!février 2004
     * + const(4)* ( -2.*aaa* (arg(ia)*darg(ia,k)-arg(ib)*darg(ib,k))					!février 2004
     *              - 2.*bbb* (arg(ia)*darg(ib,k)+arg(ib)*darg(ia,k))					!février 2004
     *              - daaa*au2(i) - dbbb*au1(i) + darg(ia,k)/q   )					!février 2004
     * + lam(2)*tt*darg(ia,k) -3.*const(6)* (darg(ia,k)*au2(i)						!février 2004
     *  -darg(ib,k)*au1(i))															!février 2004
      dcoeff(i2,7,k)=-const2(5,k)*lam(2)*arg(ib)  -  const2(4,k)*au6(i)				!février 2004
     * - const2(2,k)*(arg(ib)/q+sf)													!février 2004
     * - const(4)* (  2.*bbb* (arg(ia)*darg(ia,k)-arg(ib)*darg(ib,k))					!février 2004
     *              - 2.*aaa* (arg(ia)*darg(ib,k)+arg(ib)*darg(ia,k))					!février 2004
     *              + dbbb*au2(i) - daaa*au1(i) + darg(ib,k)/q   )					!février 2004
     * - lam(2)*tt*darg(ib,k) +3.*const(6)* (darg(ib,k)*au2(i)						!février 2004
     *  +darg(ia,k)*au1(i))															!février 2004
   23 continue
      if(impr.ne.0) then
      write(iu_11(iboat),*) 'k= ',k
      write(iu_11(iboat),*) '-------'
      write(iu_11(iboat),*) 
     *' coef de d(u)/dxi  =',dcoeff(i1,1,k),dcoeff(i2,1,k)
      write(iu_11(iboat),*) 
     *' coef de d(v)/dxi  =',dcoeff(i1,2,k),dcoeff(i2,2,k)
      write(iu_11(iboat),*) 
     *' coef de d(wø)/dxi =',dcoeff(i1,3,k),dcoeff(i2,3,k)
      write(iu_11(iboat),*) 
     *' coef de d(ny)/dxi =',dcoeff(i1,4,k),dcoeff(i2,4,k)
      write(iu_11(iboat),*) 
     *' coef de d(nxy)/dxi=',dcoeff(i1,5,k),dcoeff(i2,5,k)
      write(iu_11(iboat),*) 
     *' coef de d(my)/dxi =',dcoeff(i1,6,k),dcoeff(i2,6,k)
      write(iu_11(iboat),*) 
     *' coef de d(ry)/dxi =',dcoeff(i1,7,k),dcoeff(i2,7,k)
      write(iu_11(iboat),*) 
     *' coef de d(w)/dxi  =',dcoeff(i1,8,k),dcoeff(i2,8,k)
      write(iu_11(iboat),*) 
     *' coef de d(wøø)/dxi=',dcoeff(i1,9,k),dcoeff(i2,9,k)
      write(iu_11(iboat),*) 
     *' coef ded(wøøø)/dxi=',dcoeff(i1,10,k),dcoeff(i2,10,k)
      write(iu_11(iboat),*) 
     *' coef de d(uø)/dxi =',dcoeff(i1,11,k),dcoeff(i2,11,k)
      write(iu_11(iboat),*) 
     *' coef de d(vø)/dxi =',dcoeff(i1,12,k),dcoeff(i2,12,k)
      write(iu_11(iboat),*) 
     *' coef de d(vøø)/dxi=',dcoeff(i1,13,k),dcoeff(i2,13,k)
      write(iu_11(iboat),*) 
     *' coef de d(xo)/dxi =',dcoeff(i1,14,k),dcoeff(i2,14,k)
      write(iu_11(iboat),*) 
     *' coef de d(zo)/dxi =',dcoeff(i1,15,k),dcoeff(i2,15,k)
      write(iu_11(iboat),*) ' '
      endif
      
   15 continue

 567  continue
      
	if (itype.ne.5) then												       !février 2004                      
      coeff(i1,3)=-arg(ia)                                            !  wø
      coeff(i2,3)= arg(ib)                                          	
      coeff(i1,4)=w*(1./q+sa)-lamb*g*aa - au2(i)*const(4)             !  ny
      coeff(i2,4)=w*sb       -lamb*g*bb + au1(i)*const(4)
      coeff(i1,5)=-c*lamb*aa + g*(sa+1./q) + lam(2)*const(10)         !  nx
      coeff(i2,5)=-c*lamb*bb + g*sb                                  	
      coeff(i1,6)= (sc + lamb*aaa)*z                                  !  nyx
      coeff(i2,6)= (sd + lamb*bbb)*z                                 	
      coeff(i1,7)=coeff(i1,6)                                         !  nxy
      coeff(i2,7)=coeff(i2,6)                                     	
      coeff(i1,8)=x*au2(i) - const(2)*eta*lam(2) - const(4)*(1./q+sa) !  my
      coeff(i2,8)=-au1(i)*x-const(4)*sb                              	
      coeff(i1,9)=-b*lam(2) + const(2)*eta*au2(i) + const(10)*lamb*aa !  mx
      coeff(i2,9)=-const(2)*eta*au1(i)+const(10)*lamb*bb             	
      coeff(i1,10)=-qf*lamb*arg(ia)+const(8) *(sc+lamb*aaa)
      coeff(i2,10)= qf*lamb*arg(ib)  +const(8) *(sd+lamb*bbb)
      coeff(i1,11)=-qe*lamb*arg(ia)+const(14)*(sc+lamb*aaa)
      coeff(i2,11)=+qe*lamb*arg(ib)  +const(14)*(sd+lamb*bbb)
      coeff(i1,12)= arg(ia)*qb*lam(2) - au5(i)*x
     *                -const(14)* lamb*(sc+lamb*aaa)
     *                -const(4) * (-arg(ia)/q+au2(i)*aaa+au1(i)*bbb)
      coeff(i2,12)=-arg(ib)*qb*lam(2) - au6(i)*x
     *                -const(14)* lamb*(sd+lamb*bbb)
     *                -const(4) * (arg(ib)/q+au2(i)*bbb-au1(i)*aaa)
      coeff(i1,13)= au2(i)*qc*lamb-b*lam(3)+aa*lam(2)*const(10)+
     *                 const(8)*(au2(i)*aa+au1(i)*bb-lamb*sa)
      coeff(i2,13)=-au1(i)*qc*lamb+bb*lam(2)*const(10)+
     *                  const(8)*(au2(i)*bb-au1(i)*aa-lamb*sb)          
      coeff(i1,14)= const(4) * (arg(ia)/q-se)                         !  ry
     *          -(aaa*lam(2)+lamb*sc)*f + lam(2)*arg(ia)*qd - x*au5(i)  
      coeff(i2,14)=-const(4) * (arg(ib)/q+sf)
     *          -(bbb*lam(2)+lamb*sd)*f - lam(2)*arg(ib)*qd - x*au6(i)
      coeff(i1,15)=qd*au2(i)*lamb-b*lam(3)+const(10)*aa*lam(2)
     *          + f * (au2(i)*aa+au1(i)*bb+lamb*sa)
      coeff(i2,15)=-qd*au1(i)*lamb+const(10)*bb*lam(2)
     *          + f * (au2(i)*bb-au1(i)*aa+lamb*sb)
      coeff(i1,16)=1.
      coeff(i2,16)=0.
      coeff(i1,37)=+au2(i)                             !  wøø
      coeff(i2,37)=-au1(i)                             !
      coeff(i1,38)=-au5(i)                             !  wøøø
      coeff(i2,38)=-au6(i)                             !
      coeff(i1,39)=sc                                  !  uø
      coeff(i2,39)=sd                                  !
      coeff(i1,40)=sa                                  !  vø
      coeff(i2,40)=sb                                  !
      coeff(i1,41)=se                                  !  vøø
      coeff(i2,41)=sf                                          	
	else																			!février 2004
      coeff(i1,3)=-arg(ia)										     !  wø			!février 2004
      coeff(i2,3)= arg(ib)									             			!février 2004
      coeff(i1,4)=const(3)*(sa+(1./q))- au2(i)*const(4)	             !  ny			!février 2004
      coeff(i2,4)=const(3)*sb + au1(i)*const(4)										!février 2004
      coeff(i1,5)=-const(9)*lamb*aa +lam(2)*const(10)		   		     !  nx			!février 2004
      coeff(i2,5)=-const(9)*lamb*bb								         			!février 2004
      coeff(i1,6)= (sc + lamb*aaa)*ss								     !  nyx			!février 2004
      coeff(i2,6)= (sd + lamb*bbb)*ss                               					!février 2004
      coeff(i1,7)=coeff(i1,6)									         !  nxy			!février 2004
      coeff(i2,7)=coeff(i2,6)										                  	!février 2004
      coeff(i1,8)=const(6)*au2(i) - const(4)*(1./q+sa)				 !  my	   		!février 2004
      coeff(i2,8)=-au1(i)*const(6)-const(4)*sb                           				!février 2004
      coeff(i1,9)=-const(12)*lam(2)+ const(10)*lamb*aa				 !  mx			!février 2004
      coeff(i2,9)=const(10)*lamb*bb             										!février 2004
      coeff(i1,10)=-const(7)*lamb*arg(ia)							     !myx			!février 2004
      coeff(i2,10)= const(7)*lamb*arg(ib)												!février 2004
      coeff(i1,11)=-const(13)*lamb*arg(ia)						     !mxy			!février 2004
      coeff(i2,11)=const(13)*lamb*arg(ib)												!février 2004
      coeff(i1,12)= arg(ia)*const(13)*lam(2) - au5(i)*const(6)		 !qy			!février 2004
     *               -const(4) * (-arg(ia)/q+au2(i)*aaa+au1(i)*bbb)					!février 2004
      coeff(i2,12)=-arg(ib)*const(13)*lam(2) - au6(i)*const(6)						!février 2004
     *               -const(4) * (arg(ib)/q+au2(i)*bbb-au1(i)*aaa)					!février 2004
      coeff(i1,13)= au2(i)*const(7)*lamb-const(12)*lam(3)              !qx			!février 2004
     *				+aa*lam(2)*const(10)											!février 2004
      coeff(i2,13)=-au1(i)*const(7)*lamb+bb*lam(2)*const(10)							!février 2004
      coeff(i1,14)= const(4) * (arg(ia)/q-se)						     !  ry			!février 2004
     *          + lam(2)*arg(ia)*tt - const(6)*au5(i)									!février 2004
      coeff(i2,14)=-const(4) * (arg(ib)/q+sf)											!février 2004
     *          - lam(2)*arg(ib)*tt - const(6)*au6(i)									!février 2004
      coeff(i1,15)=tt*au2(i)*lamb-const(12)*lam(3)+const(10)*aa*lam(2) !rx			!février 2004
      coeff(i2,15)=-tt*au1(i)*lamb+const(10)*bb*lam(2)				   				!février 2004
      coeff(i1,16)=1.													 !	w			!février 2004
      coeff(i2,16)=0.																	!février 2004
      coeff(i1,37)=+au2(i)											 !  wøø			!février 2004
      coeff(i2,37)=-au1(i)															!février 2004
      coeff(i1,38)=-au5(i)											 !  wøøø		!février 2004
      coeff(i2,38)=-au6(i)															!février 2004
      coeff(i1,39)=sc													 !  uø			!février 2004
      coeff(i2,39)=sd																	!février 2004
      coeff(i1,40)=sa													 !  vø			!février 2004
      coeff(i2,40)=sb																	!février 2004
      coeff(i1,41)=se													 !  vøø			!février 2004
      coeff(i2,41)=sf							                           				!février 2004
	endif																			!février 2004

      if(mt.eq.0) goto 13
      sg= au2(i)*aa  + au1(i)*bb
      sl= au2(i)*bb  - au1(i)*aa
      do 1400 itra=1,mt
      jtrav=6*(itra-1)
      jjtrav=2*(itra-1)
      coeff(i1,17+jjtrav)=-aa*lam(2)*const(15+jtrav)
     *          + lam(3)*const(16+jtrav)
     *          + const(17+jtrav)*(sa*lamb+sg)
      coeff(i2,17+jjtrav)=-bb*lam(2)*const(15+jtrav)
     *                        + const(17+jtrav)*(sb*lamb+sl)
      coeff(i1,18+jjtrav)=-au2(i)*lam(2)*const(19+jtrav)
     *         - aa*lam(3)*const(16+jtrav) + lam(4)*const(18+jtrav)
     *         - const(20+jtrav) * (lam(2)*sa+lamb*sg)
      coeff(i2,18+jjtrav)= au1(i)*lam(2)*const(19+jtrav)
     *   - bb*lam(3)*const(16+jtrav)
     *   - const(20+jtrav)* (sb*lam(2)+lamb*sl)
 1400 continue
   13 continue

      jtrav=16+mt*2
      do 952 i=1,m
        do 950 j=1,41
        if((j.gt.jtrav).and.(j.lt.37)) goto 950
        coeff(8*i-5,j)=-coeff(8*i-6,j)
        coeff(8*i-4,j)= coeff(8*i-7,j)
        coeff(8*i-3,j)= coeff(8*i-7,j)*isign(j)
        coeff(8*i-2,j)= coeff(8*i-6,j)*isign(j)
        coeff(8*i-1,j)=-coeff(8*i-2,j)
        coeff(8*i  ,j)= coeff(8*i-3,j)
  950   continue
        do 951 j=1,13+2*mt
        do 951 kk=1,nbrxi
        k=nxi(kk)
        dcoeff(8*i-5,j,k)=-dcoeff(8*i-6,j,k)
        dcoeff(8*i-4,j,k)= dcoeff(8*i-7,j,k)
        dcoeff(8*i-3,j,k)= dcoeff(8*i-7,j,k)*isign2(j)
        dcoeff(8*i-2,j,k)= dcoeff(8*i-6,j,k)*isign2(j)
        dcoeff(8*i-1,j,k)=-dcoeff(8*i-2,j,k)
  951   dcoeff(8*i  ,j,k)= dcoeff(8*i-3,j,k)
  952 continue

      mm=8*m
      if(impr.eq.0)goto5
      write(iu_11(iboat),43)
      do 56 j=1,41
      if((j.gt.jtrav).and.(j.lt.37)) goto 56
      write(iu_11(iboat),*) j
      write(iu_11(iboat),14)(coeff(i,j),i=1,mm)
   56 continue

   43 format(//' coefficients des fonctions exponentielles, ordonnees',
     1' comme suit',/' a cos(by) ,b cos(by) ,a sin(by) ,b sin(by)'/
     *' e cosb(2*pi-y) ,f cosb(2*pi-y) ,e sinb(2*pi-y) ,f sinb(2*pi-y)'/
     *' ils se referent successivement a :'/
     *' u v wø ny nx nyx nxy my mx myx mxy qy qx ry rx w '/
     *' x0dy z0dy  (maximum 10 groupes xody,zody)'/
     *' wøø,wøøø,uø,vø et vøø '//)
   14 format(8(1x,e14.7))
    5 return
      end
