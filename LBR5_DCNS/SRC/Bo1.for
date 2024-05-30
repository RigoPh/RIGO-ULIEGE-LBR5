      subroutine bo1(nel,width,ijk,ne,
     *               jlmax,ibat,ich,imom,ibusc,
     *               nbrxi,nxi,ipt,ypt,nsol,
     *               nc)
      use sharedvar
      implicit real*8(a-h,o-z)
      real*8    lamb,ietoil,jetoil,ketoil
      dimension equ(9),equ2(9),arg(8),
     *          cof(9),root(8),root1(8),
     *          ip(4),k(4),bid(5*nsol+12)

      dimension nxi(9),dequ(9),da(9),px(8),dpx(8,9)
      dimension ypt(iptmax),bid1(20,20), bid2(20,13)
      dimension alg(400),ilinea(8)
      dimension xi(nsol),xf(nsol)
      dimension dtemp(11286)
c     dtemp= vect de travail dtemp(1102)  pour calculer dish et 
c                            dtemp(11286) pour calculer dvarh

      dimension aux(400),zb(360)
	dimension bz(1710),by(1710)	                              !!!aout04
	dimension nf(22)								!!!aout04

c***********************************************************************
c***********************************************************************

c mise a zero d'abcd
      if(nel.eq.1)then
       do i=1,8
        abcd(i)=0.d00
       enddo
      endif

c     cfr subr ent et bo2
      do i=1,nsol
       npt(i)=0
      enddo
      read(99)abtr,phil,mt,teta,xi,xf,hxtr,wxtr,q,kse,kst,ksa,
     *        txtr,dxtr,ksr,a1,b1,c19,d1,delto,ivaria,const,const2,
     *        const3,bid,ibid

      if(ich.eq.1) read(98)
     *        (npt(i),i=1,nsol),
     *        (((cha(i,j,kk),i=1,100),j=1,3),kk=1,nsol),
     *        (((dcha(i,j,kk),i=1,100),j=1,nsol),kk=1,9)

c      var. de concept. = epaisseur du bordage (k=1)
c      ---------------------------------------------
c	 const(1)= d  -> const2(1,1)=d(d)/d(épaiss)			
c	 const(2)= k  -> const2(2,1)=d(k)/d(épaiss)			
c	 const(3)= hy -> const2(3,1)=d(hy)/d(épaiss)			
c	 const(4)= ry -> const2(4,1)=d(ry)/d(épaiss)			
c	 const(5)= hx -> const2(5,1)=d(hx)/d(épaiss)			
c	 const(6)= rx -> const2(6,1)=d(rx)/d(épaiss)			

c      var. de concept. relative aux cadres (k=2,5) et aux raid.(k=6,9) 
c      ---------------------------------------------------------------
c      constante	    dérivée(k=2,5)   constante	 dérivée(k=6,9)
c    1 const(3)= oy -> const2(1,k)     const( 9)= ox -> const2(1,k)				
c    2 const(4)= hy -> const2(2,k)     const(10)= hx -> const2(1,k)				
c    3 const(5)= sy -> const2(3,k)     const(11)= sx -> const2(1,k)				
c    4 const(6)= ry -> const2(4,k)     const(12)= rx -> const2(1,k)				
c    5 const(7)= ty -> const2(5,k)     const(13)= tx -> const2(1,k)				
c    6 const(8)= ly -> const2(6,k)     const(14)= lx -> const2(1,k)				
c
c     calcul des arguments des fonctions exponentielles
c     -------------------------------------------------
c     les valeurs absolues de leurs parties reelles et complexes sont
c     entreposees successivement dans le vecteur arg.
!
      eta1=eta(nel)
      ityp1=itype(nel)

      pi1=pi/180.   ! constant
      lamb=lam(1)
      eta2=(1.-eta1)/2.
      h=const(1)+2.*(const(5)+const(11))
      g=const(1)*eta1
      z1=(h-g)/2.
      a11=(h+g)/2.
      b11=const(2)+const(12)
      c=const(1)+const(9)
      d=2.*const(2)+const(7)+const(13)
      f=const(8)+const(14)
  
      if((ityp1.eq.1).or.(ityp1.eq.2)) then
        x=const(2)+const(6)  ! cas normal
        w=const(1)+const(3)
      else if((ityp1.eq.3).or.(ityp1.eq.4).or.(ityp1.eq.5)) then			!février 2004
        x=const(6)  ! la contribution transversale du bordé est supprimée
        w=const(3)
      else
        write(66,*) 'in bo1: ityp1 is wrong =',ityp1
        write(29,*) 'in bo1: itype is wrong =',ityp1								!bug
	  pause
        stop
      endif

      bc=b11*c-const(10)*const(10)
      xw=x*w-const(4)*const(4)
      cw=c*w-g*g
      bw=b11*w+x*c
      oxy=c+w
      dxy=oxy-2.*g*eta1
      rxy=b11+x
      wc=const(10)*w+const(4)*c
      gh=c*w-g*h
      ss=const(5)+const(11)                 ! sx+sy	!février 2004
      tt=const(7)+const(13)                 ! tx+ty	!février 2004


      if(ityp1.eq.5) then													!février 2004	 
c		cas d'une epontille												!février 2004
        aetoil=(const(9)*const(12)-(const(10)*const(10)))*ss			!février 2004
        cetoil=const(12)*const(3)*const(9)+const(9)*ss*tt				!février 2004
     *	                -const(3)*const(10)*const(10)						!février 2004
        detoil=0.d+00													!février 2004
        eetoil=0.d+00													!février 2004
        fetoil=(ss*(const(12)*const(3)+const(6)*const(9)				!février 2004
     *		+2*const(10)*const(4)))+tt*const(9)*const(3)		!février 2004
	jetoil=const(6)*const(3)*const(9)+ss*tt*const(3)				!février 2004
     *		-const(9)*const(4)*const(4)							!février 2004
        ketoil=ss*(const(3)*const(6)-const(4)*const(4))					!février 2004
        betoil=0.d+00													!février 2004
        getoil=0.d+00													!février 2004
        hetoil=0.d+00													!février 2004
        ietoil=0.d+00													!février 2004
      else																!février 2004
        aetoil=z1*bc
        cetoil=(b11*w+d*z1)*c - (g*f+w*const(10))*const(10) - b11*g*h
        fetoil=d*gh + f*wc + const(4)*const(10)*2.*a11 + z1*bw
c     getoil=0.  (cfr version p : nxy=nyx)
        jetoil=w*(c*x+d*z1) - const(4)*(const(4)*c+g*f) - g*h*x
        ketoil=z1*xw
        if(q.ge.(1.e5)) then
c         cas d'une plaque raidie  (rayon très grand > 10000 m)
          betoil=0.d+00
          detoil=0.d+00
          eetoil=0.d+00
          ietoil=0.d+00
        else
c         cas d'une coque raidie  (rayon <10000 m)
          betoil=2.*g*const(10)*z1/q
          detoil=z1*cw/(q*q)
          eetoil=-(f*cw+2.*z1*wc)/q
          ietoil=2.*const(4)*g*z1/q
        endif
      endif																!février 2004

c     calcul des coefficients (equ) de l'équation diff. du 8ème ordre
c        (voir dans subr. darg)
c     ------------------------------------------------------------------

      call assemb (lam,equ,aetoil,betoil,cetoil,detoil,eetoil,fetoil,
     *                     ietoil,jetoil,ketoil)

      if(impr.eq.0) goto 5
        write(66,4)  nel
        write(66,*) 'a*  b*  c* ',aetoil,betoil,cetoil,detoil
        write(66,*) 'd*  e*  f*,',detoil,eetoil,fetoil
        write(66,*) 'i*  j*  k*,',ietoil,jetoil,ketoil
        write(66,12) ijk,(equ(i),i=1,9)
   5  continue

c     resolution de l'equation differentielle du 8ème ordre (call polrt)
c     discussion des resultats  (call discu)
c     ------------------------------------------------------------------

      do 515 i=1,9
  515 equ2(i)=equ(i)
      if=1

      call polrt (equ2,cof,8,root,root1,ier)

      call discu (root,root1,ier,arg,if,m) 

      if(if.eq.0) then
      write(*,*)"error dans la résolution de l'équ. diff.(subr. discu)"
      write(*,*)"error solving diff. equation (subr. discu)"
      write(*,*)
      write(*,*)"verifier"
      write(*,*)"les dimensions des raidisseurs et cadres"
      write(*,*)"peuvent être trop petites."
      write(*,*)"check"
      write(*,*)"the dimensions of the stiffeners and frames"
      write(*,*)"are maybe to small."

      write(6970,*)"error dans la résolution de l'équ. 
     *diff.(subr. discu)"
      write(6970,*)"error solving diff. equation (subr. discu)"
      write(6970,*)
      write(6970,*)"verifier"
      write(6970,*)"les dimensions des raidisseurs et cadres"
      write(6970,*)"peuvent être trop petites."
      write(6970,*)"check"
      write(6970,*)"the dimensions of the stiffeners and frames"
      write(6970,*)"are maybe to small."


      write(66,*)"error dans la résolution de l'équ. diff.(subr. discu)"
      write(66,*)"error solving diff. equation (subr. discu)"
      write(66,*)
      write(66,*)"verifier"
      write(66,*)"les dimensions des raidisseurs et cadres"
      write(66,*)"peuvent être trop petites."
      write(66,*)"check"
      write(66,*)"the dimensions of the stiffeners and frames"
      write(66,*)"are maybe to small."
	write(29,*)'error dans la résolution de l''équ. diff.(subr.discu)'	!sept06				!bug																								
      write(29,*)'verifier les dimensions des raidisseurs et cadres		!sept06
     * peuvent être trop petites.'										!sept06														!bug
      write(29,*)															!sept06

        pause 'stop!'
        stop
      endif

      mm=2*m
      ms=2*mt
      do 789 i=1,mm
 789  argq(i)=arg(i)*q
      if(impr.eq.1) write(66,42) (arg(i),i=1,mm)

c     calcul des derivees des arg(i) par rapport aux var. de conception,
c     soit darg(i,k) avec i=1,m et k=1,nbrxi                                           
c     ------------------------------------------------------------------
      if(iopti.ge.1) then
        if((impr.ne.0).and.(nbrxi.ne.0)) write(66,644)
        it=1
        do 642  ik=1,nbrxi
          kk=nxi(ik)
          call dargsr(lam,dequ,q,const,a11,b11,c,d,f,h,g,x,w,z1,bc,cw,
     *                bw,
     *                xw,dxy,wc,oxy,rxy,gh,eta1,eta2,kk,da,nxi,nbrxi,
     *	          ss,tt,ityp1,
     *                const2)

          do 643 i=1,m
          call comple(equ,dequ,arg,darg,nbrxi,i,kk,
     *                disa(1),disa(33),it,alg(1))
  643     continue

          if(impr.ne.1) goto 642
          write(66,'(a30,i2)') 'variable de conception nø ',kk 
          write(66,'(a25,9e12.5)')'d(equ)/dxi  =',dequ 
          write(66,'(a25,4e12.5)')'d(alpha)/dxi=',(darg(2*i-1,kk),i=1,m)
          write(66,'(a25,4e12.5)')'d(béta )/dxi=',(darg(2*i,kk),i=1,m)
          it=it+1
  642   continue
      endif
	
c     calcul des coefficients des diverses grandeurs utiles (call coe)  
c     ---------------------------------------------------------------             
c     entrepose en coeff(i,j) les coefficients de exp(-a*y)*cos(b11*y),   
c     exp(-a*y)*sin(b11*y), exp(-a(2*pi-y))*cos(b11(2*pi-y)), exp(-a(2*pi-y))*
c     sin(b11(2*pi-y)),..., pour chaque racine complexe precedement definie
c     et pour chaque effet a considerer, correspondant respectivement a  
c     u,v,dw/dy,ny,nx,nyx,nxy,my,mx,myx,mxy,qy,qx,ry,rx,w et 10 fois    
c     (xodx,zodx)  pour un maximum de 10 traverses

      do 501 i=1,4
         sinu(i)=0.
         cosi(i)=0.
         expt(i)=0.
  501 continue
      do 500 i=1,m
        expt(i)=expo(argq(2*i-1)*2*pi)
  500 continue

      call coe(const,arg,coeff,lam,eta1,eta2,mt,m,z1,x,w,a11,b11,c,f,h,
     *         g,impr,q,nbrxi,nxi,dcoeff,darg,const2,const3,alg(1),
     *	   iopti,ityp1)

c     calcul des effets des quatre charges lineaires et de la charge
c     exterieure
c     --------------------------------------------------------------
c     ils sont entreposes dans disc(i),disb(i),disa(i),disd(i), avec 16
c     memoires pour chaque effet, et dans dish(i) avec 37 memoires pour
c     chaque effet.

      do 507 i=1,4
        k(i)=1
 507  continue
      goto(503,504,505),m-1
 503  ip(1)=1
      ip(2)=2
      ip(3)=9
      ip(4)=10
      k(3)=2
      k(4)=2
      sinu(1)=dsin(argq(2)*2.*pi)
      sinu(2)=dsin(argq(4)*2.*pi)
      cosi(1)=dcos(argq(2)*2.*pi)
      cosi(2)=dcos(argq(4)*2.*pi)
      goto 506
 504  ip(1)=1
      ip(2)=9
      ip(3)=17
      ip(4)=18
      k(2)=2
      k(3)=3
      k(4)=3
      sinu(3)=dsin(argq(6)*2.*pi)
      cosi(1)=dcos(argq(2)*2.*pi)
      cosi(2)=dcos(argq(4)*2.*pi)
      cosi(3)=dcos(argq(6)*2.*pi)
      goto 506
 505  ip(1)=1
      ip(2)=9
      ip(3)=17
      ip(4)=25
      k(2)=2
      k(3)=3
      k(4)=4
      cosi(1)=dcos(argq(2)*2.*pi)
      cosi(2)=dcos(argq(4)*2.*pi)
      cosi(3)=dcos(argq(6)*2.*pi)
      cosi(4)=dcos(argq(8)*2.*pi)
 506  continue

      if((impr.eq.1).and.(iopti.eq.0)) write(66,50)
      if((impr.eq.1).and.(iopti.ge.1)) write(66,51)
      q2pi = 2.*pi*q

      if(impr.eq.1) write(66,*) 'calcul relatif à disc'
      if(impr.eq.1) write(66,*) '______________________'

      call linea(m,coeff,dcoeff,2,6,3,14,disc,hyp,k,mt,ip,1,impr,q2pi,
     *           dpx,px,darg,nxi,nbrxi,ilinea(1),ilinea(5),iopti,
     *           sinu,cosi,expt)

      if(iopti.ge.1)then
       call dpar(m,px,coeff,dcoeff,dpx,dvarc,1,nxi,nbrxi,mt)
      endif

      if((impr.ne.0).and.(iopti.ge.1)) then
       do  2356 il=1,nbrxi
         l=nxi(il)
         write(66,*) 'var. de concept. nø',l
         write(66,47) (dpx(i,l),i=1,8)
   47    format('dpx=',5x,8(e13.6,1x))
         write(66,*) ' '
       do 2356 ii=1,13+2*mt
         write(66,123)ii,(dvarc(ii,l,i),i=1,16)
 2356  continue
  123  format('fct.',i2,3x,8(e13.6,1x)/,9x,8(e13.6,1x))
      endif


      if(impr.eq.1) write(66,*) 'calcul relatif à disb'
      if(impr.eq.1) write(66,*) '______________________'

      call linea(m,coeff,dcoeff,2,3,14,6,disb,hyp,k,mt,ip,1,impr,q2pi,
     *           dpx,px,darg,nxi,nbrxi,ilinea(1),ilinea(5),iopti,
     *           sinu,cosi,expt)

      if(iopti.ge.1)then
        call dpar(m,px,coeff,dcoeff,dpx,dvarb,1,nxi,nbrxi,mt)
      endif

      if(impr.eq.1) write(66,*) 'calcul relatif à disa'
      if(impr.eq.1) write(66,*) '______________________'
      call linea(m,coeff,dcoeff,16,1,8,4,disa,hyp,k,mt,ip,-1,impr,q2pi,
     *           dpx,px,darg,nxi,nbrxi,ilinea(1),ilinea(5),iopti,
     *           sinu,cosi,expt)

      if(iopti.ge.1)then
        call dpar(m,px,coeff,dcoeff,dpx,dvara,-1,nxi,nbrxi,mt)
      endif

      if(impr.eq.1) write(66,*) 'calcul - relatif à disd'
      if(impr.eq.1) write(66,*) '______________________'
      call linea(m,coeff,dcoeff,16,1,4,8,disd,hyp,k,mt,ip,-1,impr,q2pi,
     *           dpx,px,darg,nxi,nbrxi,ilinea(1),ilinea(5),iopti,
     *           sinu,cosi,expt)

      if(iopti.ge.1)then
        call dpar(m,px,coeff,dcoeff,dpx,dvard,-1,nxi,nbrxi,mt)
      endif

c ----------------------------------------------------------------------
c     calcul de vø,uø,wø, vøø,uøø,wø, vøøø,uøøø,wøøø                     
c     pour les cas de charge disa,disb,disc,disd   ( cfr subroutine  deriv)       
c      &                                                                 
c     complement au calcul des coefficients des diverses grandeurs      
c     utiles  c-a-d  nx,nxy,mx,mxy,myx,qx,rx   (cfr subroutine manq )
c ----------------------------------------------------------------------
c
      call linde(3,m,01,disa,arg,30)
      call linde(3,m,30,disa,arg,33)
      call linde(3,m,33,disa,arg,36)
      call linde(3,m,01,disb,arg,30)
      call linde(3,m,30,disb,arg,33)
      call linde(3,m,33,disb,arg,36)
      call linde(3,m,01,disc,arg,30)
      call linde(3,m,30,disc,arg,33)
      call linde(3,m,33,disc,arg,36)
      call linde(3,m,01,disd,arg,30)
      call linde(3,m,30,disd,arg,33)
      call linde(3,m,33,disd,arg,36)
      call manq(lam,eta1,const,disa,16,q,ityp1)				!février 2004
      call manq(lam,eta1,const,disb,16,q,ityp1)				!février 2004
      call manq(lam,eta1,const,disc,16,q,ityp1)				!février 2004
      call manq(lam,eta1,const,disd,16,q,ityp1)				!février 2004

c     integration de la pression hydraulique et charges de bord.
c     ===========================================================
c     (subroutines hydros, press, bord, mom )
c
      do i=1,1710  ! mise a zero (initialisation de dish)
         do is=1,nsol
            dish(i,is)=0.0
         enddo
      enddo

      if(iopti.ge.1) then   ! mise a zero (initialisation de dvarh)
        do 8427 is=1,nsol
        do 8427 ii=1,13+2*mt
        do 8427 kk=1,nbrxi
          k1=nxi(kk)
          do 8427 j=1,38
            dvarh(ii,k1,j,is)=0.
 8427   continue
      endif

c     subr press et hydros
	bkse=1.
      ze  =1.d-60
      if(kse.eq.1) bkse=-1.

      if(ibat.eq.1) then  !ibat=0 si terme pair (-->pas utile pour charge sym)
       if(kse.ne.0) then
        if(ivaria.eq.1) then  ! subr press et dpress
          do  is=1,nsol
            call press(arg,m,xi(is),xf(is),phil,disc,dish(1,is),q,ijk,
     *                 ms,bkse)
          enddo
          if((iopti.ge.1).and.(nbrxi.ge.1)) then
            do  is=1,nsol
              call dpress(arg,darg,m,xi(is),xf(is),phil,q,disc,dvarc,
     *                    dvarh(1,1,1,is),bkse,ijk,nxi,nbrxi,mt,
     *                    sinu,cosi,expt,nel)
	   enddo
          endif
        else    ! subr hydro et dhydro
	  do  is=1,nsol
            call hydro(arg,m,xi(is),xf(is),phil,disc,dish(1,is),teta,q,
     *                 ijk,ms,bkse,
     *                 sinu,cosi,expt)
	  enddo
          if((iopti.ge.1).and.(nbrxi.ge.1)) then
            do  is=1,nsol
            call dhydro(arg,darg,m,xi(is),xf(is),phil,q,disc,dvarc,
     *                  dvarh(1,1,1,is),bkse,ijk,teta,nxi,nbrxi,mt,
     *                  sinu,cosi,expt)
	    enddo
          endif
        endif  ! if(ivaria.eq.1)
       endif  ! if(kse.ne.0) 
      endif  ! if(ibat.eq.1)


      call annuld(dtemp,11286)   ! dtemp = dvarh(33,9,38)

c     ** boucle sur loc,locy et locz + mom (bateau et bus)
      do 5326 is=1,nsol    ! boucle sur les is cas de charge
	
      ! subr loc,locy et locz   
      if(ich.ne.0) then                                               
       call locy(arg,m,1.0d+00,phil,disa,dtemp(1),dish(1,is),teta,q,
     *           ijk,lamb,width,impr,ms,is,cha,npt,nsol,by,
     *           sinu,cosi,expt)

       call locz(arg,m,-1.0d+00,phil,disc,dtemp(1),dish(1,is),teta,q,
     *           ijk,lamb,width,impr,ms,is,cha,npt,nsol,bz,
     *           sinu,cosi,expt)

       call loc(arg,m,phil,disc,dtemp(1),dish(1,is),teta,q,
     *         ijk,lamb,width,impr,ms,is,cha,npt,bkse,nsol,
     *         sinu,cosi,expt)

       if((iopti.ge.1).and.(nbrxi.ge.1)) then
        call dlocy(arg,darg,m,+1.0d+00,phil,disa,dvara,dtemp(1),
     *             dvarh(1,1,1,is),
     *             teta,q,ijk,lamb,width,impr,cha,dcha,npt,nxi,nbrxi,mt,
     *             is,nsol,by,sinu,cosi,expt)

        call dlocz(arg,darg,m,-1.0d+00,phil,disc,dvarc,dtemp(1),
     *         dvarh(1,1,1,is),
     *         teta,q,ijk,lamb,width,impr,cha,dcha,npt,nxi,nbrxi,
     *         mt,is,nsol,bz,
     *         sinu,cosi,expt)

        call dloc(arg,darg,m,phil,disc,dvarc,dtemp(1),
     *         dvarh(1,1,1,is),teta,
     *         q,ijk,lamb,width,impr,cha,npt,bkse,nxi,nbrxi,mt,is,nsol,
     *         sinu,cosi,expt)

       endif
      endif  

c     lecture des données relatives aux moments de bords
      if(ityp1.eq.5) goto 5326								!coefficient de participation
      if(imom.eq.0) then
        call annuld(abcd,8)
      else
        read(97) abcd
      endif

c ************************************************************************
c     lecture des efforts nb et moments mb de bord(dans file busc.txt et busm.txt)
c     ! nb et mb  sont les mêmes pour tous les cas de charge !!!!
      if(ibusc.eq.0) goto 32

c     a) force nb (agissant dans le plan du bordé)
c        ---------
      if((nel.eq.1).and.(is.eq.1).and.(ijk.eq.1)) then
         open(96,file=nom//'busc.txt',status='old',err=657)
	   read(96,'(a1)') ibus ! skip ligne titre
	   goto 658
 657     write(*,*)'stop: nom du fichier (busc.txt) est incorrect'
         write(*,*)'    : error, (busc.txt) data file not valid'

         write(6970,*)'stop: nom du fichier (busc.txt) est incorrect'
         write(6970,*)'    : error, (busc.txt) data file not valid'

         write(29,*)'stop: nom du fichier (busc.txt) est incorrect'	!sept06				!bug
         stop  
 658     continue
      endif

      if(is.eq.1) then
       if((nel.eq.1).and.(ijk.gt.1)) then
         rewind 96
	   read(96,'(a1)') ibus ! skip ligne titre
	 endif
	 read(96,*)busc1,busc2,busc3,busc4,busc5,busc6,busc7,busc8
       if(ijk.eq.1) then
	   write(66,'(a,i3,8(1x,e11.4),a)')
     *       'nel=',nel,busc1,busc2,busc3,busc4,busc5,busc6,busc7,busc8,
     *       ' forces(n/m)'
	 endif
	endif

c	! charge en x**3
        abcd(1)=abcd(1)+ busc1   ! les efforts de buscage nb 
        abcd(5)=abcd(5)+ busc5   ! s'ajoutent aux effets du moment d'ensemble (subr bateau)
c	! charge en x**2
        abcd(2)=abcd(2)+ busc2
        abcd(6)=abcd(6)+ busc6
c	! charge en x
        abcd(3)=abcd(3)+ busc3
        abcd(7)=abcd(7)+ busc7
c	! charge uniforme
	  abcd(4)=abcd(4)+ busc4
        abcd(8)=abcd(8)+ busc8

c     b) moments mb
c        ----------
      if((nel.eq.1).and.(is.eq.1).and.(ijk.eq.1)) then
         open(95,file=nom//'busm.txt',status='old',err=659)
	   read(95,'(a1)') ibus ! skip ligne titre
	   goto 660
 659     write(*,*)'stop: nom du fichier (busm.txt) est incorrect'
         write(*,*)'    : error, (busm.txt) data file not valid'

         write(6970,*)'stop: nom du fichier (busm.txt) est incorrect'
         write(6970,*)'    : error, (busm.txt) data file not valid'

         write(29,*)'stop: nom du fichier (busm.txt) est incorrect'	!sept06				!bug
         stop  
 660     continue
      endif

      if(is.eq.1) then
       if((nel.eq.1).and.(ijk.gt.1)) then
         rewind 95
	   read(95,'(a1)') ibus ! skip ligne titre
	 endif
	 read(95,*)busm1,busm2,busm3,busm4,busm5,busm6,busm7,busm8
       if(ijk.eq.1) then
	   write(66,'(a,i3,8(1x,e11.4),a)')
     *     'nel=',nel,busm1,busm2,busm3,busm4,busm5,busm6,busm7,busm8,
     *     ' moments(n.m/m)'
	 endif
	endif

c     transfer moment (mb en n.m/m) --> charge (q en n/m /m)
c      avec  q= mb*(ijk*pi/l)  (n/m par mètre courant selon y)
c            q est du même type que la charge unitaire  disc
c            disc = 10000 sin(lamb x) selon -0z
        xmult = ijk*pi/width  ! avec mb développé sur 5% de l

c	! moment en x**3  : busm1 & busm5
c	! moment en x**2  : busm2 & busm6
c	! moment en x     : busm3 & busm7
c	! moment uniforme : busm4 & busm8

c     forces "q" concentrees entre x=0 et x=d=delto)
c     (on divise par 10000 car disb correspond à une charge unitaire de 10000)
c     (  avec disc = 10000 sin(lamb x) ) selon -oz
        e1=2.*dsin(ijk*pi*delto/width) /(ijk*pi*delto*10000.)  
c     forces "q" concentrees entre x=l-d et x=l)
        e2=2.*dsin(ijk*pi*(width-delto)/width) /(ijk*pi*delto*10000.)
        a2=xmult*(busm1*e1+busm5*e2)
        b2=xmult*(busm2*e1+busm6*e2)
	  c9=xmult*(busm3*e1+busm7*e2)
	  d2=xmult*(busm4*e1+busm8*e2)

      call mom(arg,m,a2,b2,c9,d2,phil,disc,dtemp,dish(1,is),q,ms)

      if((iopti.ge.1).and.(nbrxi.ge.1)) then
        call dmomsr(arg,darg,m,phil,disc,dvarc,dtemp,dvarh(1,1,1,is),
     *              teta,q,
     *              impr,nxi,nbrxi,mt,a2,b2,c9,d2,sinu,cosi,expt)
      endif

  32  continue

c *************fin lecture forces de buscage *****************************************

c     moment m1 + nb (forces concentrees entre x=0 et x=d=delto)
c     (on divise par 10000 car disb correspond à une charge unitaire de 10000)
c     (  avec disb = 10000 cos(lamb x) )selon ox
        e1=2.*dsin(ijk*pi*delto/width) /(ijk*pi*delto*10000.)  
c     moment m2 +nb (forces concentrees entre x=l-d et x=l)
        e2=2.*dsin(ijk*pi*(width-delto)/width) /(ijk*pi*delto*10000.)
c     write(6,*)' pour m1, e1=',e1,' pour m2, e2=',e2
        a2=abcd(1)*e1+abcd(5)*e2
        b2=abcd(2)*e1+abcd(6)*e2
	  c9=abcd(3)*e1+abcd(7)*e2
	  d2=abcd(4)*e1+abcd(8)*e2

      call mom(arg,m,a2,b2,c9,d2,phil,disb,dtemp,dish(1,is),q,ms)

      if((iopti.ge.1).and.(nbrxi.ge.1)) then
        call dmomsr(arg,darg,m,phil,disb,dvarb,dtemp,dvarh(1,1,1,is),
     *              teta,q,
     *              impr,nxi,nbrxi,mt,a2,b2,c9,d2,sinu,cosi,expt)
      endif

 5326 continue  ! ** fin de boucle sur les cas de charge

c ----------------------------------------------------------------------
c     calcul de vø,uø,wø, vøø,uøø,wø, vøøø,uøøø,wøøø  ( cfr subroutine  deriv)                   
c     pour les cas de charge                                       
c     dish,  disla,dislb,dislc,disld   et   dismla,disma,dismc,dismd                                 
c       +                                                                
c     complement au calcul des coefficients des diverses grandeurs      
c     utiles  c-a-d  nx,nxy,mx,mxy,myx,qx,rx   (cfr subroutine manq )
c ----------------------------------------------------------------------
      do is=1,nsol
        call deriv(3,m,01,dish(1,is),arg,30,q)
        call deriv(3,m,30,dish(1,is),arg,33,q)
        call deriv(3,m,33,dish(1,is),arg,36,q)
        call manq(lam,eta1,const,dish(1,is),38,q,ityp1)				!février 2004
      enddo
      if((ijk.ge.2).or.(impr.eq.0)) goto 315
      write(66,54)
      do 750 i=1,3
        j1=(i-1)*16+1
        write(66,55)(disc(j),disb(j),disa(j),disd(j),j=j1,(j1+4*m-1))
 750  continue

      if((kse.eq.0).and.(ich.eq.0).and.(imom.eq.0)) goto 315
      write(66,58)
      do is=1,nsol
        if(is.eq.1) then
           write(66,*) ' cas de charge nø',is
	     call pimpr(dish(1,is),ms,m)
	  endif
      enddo
  315 continue

  
      do ij=1,400
        aux(ij)=0.
      enddo
      do ij=1,360
        zb(ij)=0.
        hyp(ij)=0.
        ali(ij)=0.
      enddo

c  =====================================================================

c  expression partielle des dérivées des disa, disb, ... et dish (voir linea)
c  --------------------------------------------------------------------------
c	dvara(33,9,16), dvarb, dvarc et dvard (pour disc, disa, disb et disd) 
c          avec 13+2*mt variables (u v wø ny nyx my ry w wøø wøøø uø vø vøø +
c                       xo, zo),
c		        9 variables de conception,
c		  et   16 termes.
c       dvarh(33,9,38,is) avec 38 termes pour dish avec is le nø du cas de
c       charge (is=1,nsol)
        
c     voir subr dpar et subr dpress ou dhydro (ci-avant)

 	if((impr.ne.0).and.(iopti.ne.0).and.(nbrxi.ne.0)) then

       do 4598 is=1,nsol
      	 write(66,*)' cas de charge nø',is
      	 write(66,*)' ====================='
        do 4598 ivar=1,nbrxi
	 iv=nxi(ivar)
      	 write(66,*)' dérivée de dish , var. xi nø',iv
      	 write(66,*)' ---------------------------------'
	   do 4598 ii=1,13+2*mt
      	 write(66,'(a10,i3,6e12.5)')' fonct nø',ii,
     *                          (dvarh(ii,iv,i,is),i=1,6)
		 write(66,'(8e12.5)') (dvarh(ii,iv,i,is),i=7,14)
		 write(66,'(8e12.5)') (dvarh(ii,iv,i,is),i=15,22)
       	 write(66,'(8e12.5)') (dvarh(ii,iv,i,is),i=23,30)
 4598    continue

	endif
	
c***********************************************************************
c     partie ii:
c     -----------
c     début des calculs relatifs aux traverses
c***********************************************************************
      if(mt.eq.0) goto 622

c     calcul de la matrice du systeme de resolution des inconnues
c     hyperstatiques    (effets des traverses)
c     ------------------------------------------------------------------
      do i=1,2
        call inteq(disb,mt,m,i,1,aux)
        call inteq(disc,mt,m,i,2,aux)
      enddo

c     calcul des termes independants du systeme de resolution des       
c     inconnues hyperstatiques (effets des traverses)
c     -----------------------------------------------------------
c     charge exterieure (subroutines hyp1 et hyp2)
c     8 cas de charges aux bords du panneau (subroutine vector)

      if((kse.eq.0).and.(ich.eq.0).and.(imom.eq.0)) goto 6801
      call hyp1(dish(1,1),mt,teta,zb,1)
      do is=2,nsol
        is1= 8+is
        call hyp1(dish(1,is),mt,teta,zb,is1)
      enddo
 6801 continue

      do 64 kk=1,m
      do 64 i=1,mt
      sm =argq(2*kk-1)*abtr(i)*pi1
      sn =argq(2*kk)  *abtr(i)*pi1
      sp =argq(2*kk-1)*(phil-abtr(i))*pi1
      sq =argq(2*kk)  *(phil-abtr(i))*pi1
      smm=argq(2*kk-1)*(360.-abtr(i))*pi1
      snn=argq(2*kk)  *(360.-abtr(i))*pi1
      spp=argq(2*kk-1)*(360.-(phil-abtr(i)))*pi1
      sqq=argq(2*kk)  *(360.-(phil-abtr(i)))*pi1
      sm =expo(sm)
      sp =expo(sp)
      smm=expo(smm)
      spp=expo(spp)
      tm =sm *dcos(sn)
      tn =sm *dsin(sn)
      tp =sp *dcos(sq)
      tq =sp *dsin(sq)
      tmm=smm*dcos(snn)
      tnn=smm*dsin(snn)
      tpp=spp*dcos(sqq)
      tqq=spp*dsin(sqq)
      kh=8*kk+37+76*i

      if((kse.eq.0).and.(ich.eq.0).and.(imom.eq.0)) goto 6802
      call hyp2(dish(1,1),mt,zb,tm,tn,tp,tq,tmm,tnn,tpp,tqq,kh,i,1)
      do is=2,nsol
        is1= 8+is
        call hyp2(dish(1,is),mt,zb,tm,tn,tp,tq,tmm,tnn,tpp,tqq,kh,i,is1)
      enddo
6802  continue

      call vecto(disc,tm,tn,i,kk,mt,2,+1.0d+00,tmm,tnn,zb)
      call vecto(disc,tp,tq,i,kk,mt,3,+1.0d+00,tpp,tqq,zb)
      call vecto(disd,tm,tn,i,kk,mt,4,+1.0d+00,tmm,tnn,zb)
      call vecto(disd,tp,tq,i,kk,mt,5,-1.0d+00,tpp,tqq,zb)
      call vecto(disa,tm,tn,i,kk,mt,6,+1.0d+00,tmm,tnn,zb)
      call vecto(disa,tp,tq,i,kk,mt,7,-1.0d+00,tpp,tqq,zb)
      call vecto(disb,tm,tn,i,kk,mt,8,+1.0d+00,tmm,tnn,zb)
      call vecto(disb,tp,tq,i,kk,mt,9,+1.0d+00,tpp,tqq,zb)
      call plimit(disc,disb,+tm,+tn,tmm,tnn,i,kk,mt,01,03,ali)
      call plimit(disc,disb,+tp,+tq,tpp,tqq,i,kk,mt,02,03,ali)
      call plimit(disc,disb,+tm,+tn,tmm,tnn,i,kk,mt,03,02,ali)
      call plimit(disc,disb,+tp,+tq,tpp,tqq,i,kk,mt,04,02,ali)
      call plimit(disc,disb,+tm,+tn,tmm,tnn,i,kk,mt,05,24,ali)
      call plimit(disc,disb,+tp,+tq,tpp,tqq,i,kk,mt,06,24,ali)
      call plimit(disc,disb,+tm,+tn,tmm,tnn,i,kk,mt,07,25,ali)
      call plimit(disc,disb,+tp,+tq,tpp,tqq,i,kk,mt,08,25,ali)
      call plimit(disc,disb,-tm,-tn,-tmm,-tnn,i,kk,mt,09,27,ali)
      call plimit(disc,disb,+tp,+tq,+tpp,+tqq,i,kk,mt,10,27,ali)
      call plimit(disc,disb,-tm,-tn,-tmm,-tnn,i,kk,mt,11,26,ali)
      call plimit(disc,disb,+tp,+tq,+tpp,+tqq,i,kk,mt,12,26,ali)
      call plimit(disc,disb,-tm,-tn,-tmm,-tnn,i,kk,mt,13,01,ali)
      call plimit(disc,disb,+tp,+tq,+tpp,+tqq,i,kk,mt,14,01,ali)
      call plimit(disc,disb,-tm,-tn,-tmm,-tnn,i,kk,mt,15,29,ali)
      call plimit(disc,disb,+tp,+tq,+tpp,+tqq,i,kk,mt,16,29,ali)
   64 continue

c     resolution des effets hyperstatiques des traverses
c     ---------------------------------------------------
      nbrequ=8+nsol  ! + 2*4 (8 efforts de bord unitaires nb et mb)

      call resolu(ms,nbrequ,aux(1),zb(1),impr)
c     ============
c     après resolu, la matrice aux est la matrice inverse (a conserver)
c                et la matrice hyp contient les vecteurs solutions.
      if(impr.ge.1) then
        write(66,65)
        do 66 i=1,nbrequ
          ij=1+(i-1)*20
          jm=ij+ms-1
   66   write(66,67) (hyp(j),j=ij,jm)
      endif

  622 continue

      if((impr.ge.1).and.(mt.ge.1)) then
        write(66,*)' '
        write(66,*)'vecteur ali (w,u,..) pour les mt cas de charges'
        do 4599 ii=1,16
 4599   write(66,'(i2,3(8e14.5/))') ii,(ali(i+ii*20),i=1,ms)
 	endif

c     fin du calcul des inconnues hyperstatiques des traverses (partie ii)

c********************************************************************************
c     partie iii:
c     -----------
c   calcul des sensibilités (dérivées des déplac. par rapport aux var. de concept.
c********************************************************************************
c     calcul des dérivées des disa, disb, ... et dish aux points choisis
c     ------------------------------------------------------------------
c     sensa(13,10,9) avec 13 variables w,u,v,wø,my,ny,nyx,ry,wøø,wøøø,uø,vø,vøø
c	   		     et 10 points de calcul des sensibilités,
c                  idem avec sensb,sensc,sensd et sensh
c     ypt(10)  coordonnées selon oy des pts de calcul des sensibilités 

      if(iopti.ge.1) then
        if(mt.ge.1) then
          call dhyps(phil,teta,q,m,mt,ms,nbrxi,nxi,aux(1),
     *               bid1,bid2,nsol)
        endif
        call sens(phil,teta,q,m,mt,nbrxi,nxi,ipt,ypt,nsol)
      endif

c********************************************************************************
c     partie iv:
c     -----------
c     calcul de la matrice "d'assemblage" pour le calcul des inc. hyper. de bord
c********************************************************************************

      if(mt.eq.0) goto 202                                   		
c     calcul des deplacements et reactions  sur les bords (1er partie)  
c     (charge exterieure) - inf1 (effet des traverses)                  
c     -----------------------------------------------------------------
      call pinf1 (sum,ali,zb,hyp,0,0,ms)

      do is=2,nsol
        is1= is*20+140     ! is1 = donnée
        is2= (is-1)*16     ! is2 = solution
        call pinf1 (sum,ali,zb,hyp,is1,is2,ms)
      enddo
                                                                       
 202  continue

c     calcul des deplacements et reactions sur les bords (2ème partie)
c     (charge exterieure) - inf2 et inf3                          	
c     ------------------------------------------------------------------
      if(dabs(teta).ne.180.) goto 444
      c19=-1.
      s1=0.
      c9=-dcos(phil*pi1)
      s2=-dsin(phil*pi1)
      goto 446
444   if(dabs(teta).ne.90.) goto 445
      ii=1.
      if(teta.eq.-90.) ii=-1
      c19=0.
      s1=ii
      c9=-ii*dsin(phil*pi1)
      s2=ii*dcos(phil*pi1)
      goto 446
  445 c19=dcos(teta*pi1)
      s1=dsin(teta*pi1)
      c9=dcos((teta+phil)*pi1)
      s2=dsin((teta+phil)*pi1)
  446 ph=phil*pi1
      ph2=ph*ph
      ph3=ph2*ph
c
      if((kse.eq.0).and.(ich.eq.0).and.(imom.eq.0)) goto 6803
      do is=1,nsol
        is1= (is-1)*16
        call pinf2(zb,dish(1,is),s1,s2,c19,c9,ph,ph2,ph3,is1)
      enddo
 6803 continue


c     calcul des deplacements et reactions  sur les bords
c      (8 charges unitaires de bords)
c     --------------------------------------------------------------
      do 683 kk=1,400
  683 aux(kk)=0.

      do 79 kk=1,m
      sp =argq(2*kk-1)*phil*pi1
      sq =argq(2*kk)  *phil*pi1
      smm=argq(2*kk-1)*2*pi
      snn=argq(2*kk)  *2*pi
      spp=argq(2*kk-1)*(360.-phil)*pi1
      sqq=argq(2*kk)  *(360.-phil)*pi1
      sp =expo(sp)
      smm=expo(smm)
      spp=expo(spp)
      tm=1.
      tn=0.
      tp =sp *dcos(sq)
      tq =sp *dsin(sq)
      tmm=smm*dcos(snn)
      tnn=smm*dsin(snn)
      tpp=spp*dcos(sqq)
      tqq=spp*dsin(sqq)

      if((kse.eq.0).and.(ich.eq.0).and.(imom.eq.0)) goto 6804
      do is=1,nsol
        is1= (is-1)*16
        call pinf3(zb,dish(1,is),tp,tq,tmm,tnn,tpp,tqq,kk,is1)
      enddo
 6804 continue

      call pedge(tm,tn,tp,tq,tmm,tnn,tpp,tqq,
     *         kk,01,03,disa,disb,disc,disd,+1.0d+00,-1.0d+00,aux)
      call pedge(tm,tn,tp,tq,tmm,tnn,tpp,tqq,
     *         kk,03,02,disa,disb,disc,disd,+1.0d+00,-1.0d+00,aux)
      call pedge(tm,tn,tp,tq,tmm,tnn,tpp,tqq,
     *         kk,05,24,disa,disb,disc,disd,+1.0d+00,-1.0d+00,aux)
      call pedge(tm,tn,tp,tq,tmm,tnn,tpp,tqq,
     *         kk,07,25,disa,disb,disc,disd,+1.0d+00,-1.0d+00,aux)
      call pedge(tm,tn,tp,tq,tmm,tnn,tpp,tqq,
     *         kk,09,27,disa,disb,disc,disd,-1.0d+00,+1.0d+00,aux)
      call pedge(tm,tn,tp,tq,tmm,tnn,tpp,tqq,
     *         kk,11,26,disa,disb,disc,disd,-1.0d+00,+1.0d+00,aux)
      call pedge(tm,tn,tp,tq,tmm,tnn,tpp,tqq,
     *         kk,13,01,disa,disb,disc,disd,-1.0d+00,+1.0d+00,aux)
      call pedge(tm,tn,tp,tq,tmm,tnn,tpp,tqq,
     *         kk,15,29,disa,disb,disc,disd,-1.0d+00,+1.0d+00,aux)
   79 continue

      j1=1
      ii=1
 3002 do 3001 j=j1,j1+3
        aux(    2*j)= aux( 19+2*j)*ii
        aux(20 +2*j)= aux( -1+2*j)*ii
        aux(40 +2*j)=-aux( 59+2*j)*ii
        aux(60 +2*j)=-aux( 39+2*j)*ii
        aux(80 +2*j)=-aux( 99+2*j)*ii
        aux(100+2*j)=-aux( 79+2*j)*ii
        aux(120+2*j)= aux(139+2*j)*ii
        aux(140+2*j)= aux(119+2*j)*ii
 3001 continue
      if(j1.eq.5) goto 3003
        j1=5
        ii=-1
        goto 3002
 3003 continue
c
c     contribution des traverses à la matrice de rigidité
c     ---------------------------------------------------
      if(mt.gt.0) then
        do 75 j=1,8
        do 75 i=1,16
          ij=i+(j-1)*20
          sum=0.
          do 76 kk=1,ms
            kh=kk+i*20
            jm=kk+j*20
            sum=sum+ali(kh)*hyp(jm)
   76     continue
          aux(ij)=aux(ij)+sum/10000.
   75   continue
      endif

      if(impr.eq.0) goto 777
      write(66,4002) nel
      do 8 i=1,16
         write(66,4001) (aux((ii-1)*20+i),ii=1,8),
     *                  (zb(i+(ii-1)*16),ii=1,nsol)
    8 continue
  777 continue

c***********************************************************************
c     partie v:
c     -----------
c     stokage des resultats intermediaires
c***********************************************************************
      write(nel+100)disa,disb,disc,disd,argq,m,hyp,lamb,ijk

      if((kse.eq.0).and.(ich.eq.0).and.(imom.eq.0)) then
      else
        write(nel+100) dish ! ((dish(l,ll),l=1,1710),ll=1,nsol)
      endif

      if(iopti.ge.1) then
        write(nel+400) sensh,sensa, sensb, sensc, sensd,			!extension neto	!fev2007
     *                       sensaa,sensbb,senscc,sensdd,
     *              defa,defb,defc,defd,defaa,defbb,defcc,defdd
      backspace(nel+400)		!extension neto	!fev2007
      endif

c***********************************************************************
c     partie vi:
c     -----------
c     rangement des coefficients des equations de continuite et d'equilibre  
c     et resolution lors du passage pour le dernier panneau.
c***********************************************************************

      nvsmb=nsol
      call mdr(nel,ne,phil,nvsmb,ijk,jlmax,nsol,nc,aux,zb)
      return

c     ******************************************************************
c     formats
c     ******************************************************************
    4 format(1h1,43x,16h panneau numero ,i2,/45x,18(1h+) )
   12 format(    14h terme numero ,i2/' du developpement de la press',
     1'ion hydrostatique en serie de fourier'/1x,72(1h+),//' coefficient
     3s de l''equation caracteristique,'/' ordonnes par puissances ',
     4'croissantes',//(6(2x,e15.8)))
   42 format(/'arguments des fonctions exponentielles (alpha,béta)',
     *  /(4(2x,e14.7)))
   50 format(' résolutions des 4 cas de charge de base (c b a d)',
     *' dont le vecteur indépendant est 0 0 0 -5000'/' la matrice des',
     *' coeff. et le vect solution sont :'/)
   51 format(' résolutions des 4 cas de charge de base (c b a d)',
     *' dont le vecteur indépendant est 0 0 0 -5000'/' la matrice des',
     *' coeff., le vect solution (px) et les dérivées (dpx)'/
     * 'de ce vecteur par rapport aux var. de conception sont :'/)
   54 format(1h1,' coefficients des fonctions exponentielles,'/
     *' pour les 4 cas de charges lineaires de base',/1x,45(1h+),/
     *' chaque colonne contient les coefficients de exp(a*y)*cos(b*y),'/
     *' exp(a*y)*sin(b*y),exp(a(2*pi-y))*cos(b(..))et exp(a(..))*sin(b(.
     *..)),'/' ordonnes comme dans arg(i),successivement',/
     * ' pour v,u,w,xodx,zodx (maximum 10 groupes xodx,zodx),'/
     * ' my,ny,ry,nyx,qy,wø,  vø,uø,wø,vøø,uøø,wøø,vøøø,uøøø,wøøø et ',
     * 'nx,nxy,mx,mxy,myx,qx,rx.'//t4,'charge (c)',t21,'charge (b)',
     * t38,'charge (a)',t55,'charge (d)',//)
   55 format(16(4(2x,e15.8)/))
   58 format(1h1,' coefficients des fonctions exponentielles, pour la pr
     *ession hydrostatique'/1x,74(1h+)/' chaque groupe  contient success
     *ivement le terme independant, '/1x,'les coefficients de y, y**2 et
     * y**3, le coefficient de cos(teta+y), sin(teta+y)' /' de exp(a*y)*
     *cos(b*y),exp(a*y)*sin(b*y),exp(a*(phil-y))*cos(b*(phil-y)),...'/1x
     *,'de exp(a*(2pi-y)*cos(b*(2pi-y)),exp(..)*sin(..),..'/1x,
     *'de exp(a(2pi-(phil-y)))*cos(...),exp(..)*sin(..)'/1x,
     *'ordonnes comme dans arg(i)., successivement pour'/
     *' vo uo wo xodx zodx (10 groupes xodx zodx maximum),'/
     *' my ny ry nyx qy wø      vø,uø,wø,vøø,uøø,wøø,vøøø,uøøø,wøøø'
     *' et  nx nxy mx mxy myx qx rx.'//)
   65 format(/'effets hyperstatiques exerces par les traverses'/
     *48(1h+)/' ils sont ordonnes comme suit :'/
     *' - les lignes: phi et f successivement pour chaque traverses '/
     *' - les colonnes, charge exterieur, charge c sup, charge c inf,',
     *' idem avec les charges c, d et a'/
     *'   avec de plus les effets pour les 4 cas de charges de bords ',
     *'(a,0,0,0 // 0,b,0,0 // 0,0,c,0 // 0,0,0,d)'/)
   67 format(/6(1x,e11.4))
  644 format(/'dérivées des coef. de l''équ. diff. du 8éme ordre par',
     * ' rapport aux variables de conception'/85(1h-))
 2003 format(/' cas de charge de bord type a : a*(q*phil)**3=1000.,',
     * 'b=c=d=0'/t2,67(1h-)//)
 2004 format(/' cas de charge de bord type b : a=0, b(q*phil)**2=1000.,'
     *,' c=d=0 '/t2,67(1h-)//)
 2005 format(/' cas de charge de bord type c : a=0, b=0, c(q*phil)=1000.
     *, d=0'/t2,67(1h-)//)
 2006 format(/' cas de charge de bord type d : a=0, b=0, c=0, d=1000.'/
     *t2,62(1h-)//)
 2007 format(/,t5,30(1h*)/t5,1h*,t10,'les forces de bords',t35,1h*/t5,
     *1h*,t35,1h*/t5,30(1h*))
 2008 format(/,t5,30(1h*)/t5,1h*,t10,'les moments de bords',t35,1h*/t5,
     *1h*,t35,1h*/t5,30(1h*))
 4001 format(/4(1x,e11.4)/4(1x,e11.4)/9(1x,e11.4))
 4002 format(1h1,'pour le calcul des effets hyperstatiques de bord du ',
     *' panneau ',i3, ' (ordre c d a b)'/
     *' voici par ligne la matrice des coeff. des inc. hyperstatiques,'/
     *' puis les 9 termes indep. pour la p.h. et les 8 effets de bords :
     *'/' w(y=0),w(y=phil),u(y=0),u(..),my,..,ny,..,nyx,..,ry,..,'/
     *' v,..,dw/dy,...')
      end

