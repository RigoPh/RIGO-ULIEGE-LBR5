subroutine bo1(ijk,ibat,ich,nel)

use param_section

implicit double precision(a-h,o-z)

double precision    lamb,ietoil,jetoil,ketoil
dimension equ(9),equ2(9),arg(8),    &
          cof(9),root(8),root1(8),  &
          ip(4),k(4)

dimension dequ(9),da(9),px(8),dpx(8,9)
dimension bid1(20,20), bid2(20,13)
dimension alg(400),ilinea(8)
!      dimension xi(nsol),xf(nsol)
dimension dtemp(11286)
!dtemp= vect de travail dtemp(1102)  pour calculer dish et 
!                       dtemp(11286) pour calculer dvarh

dimension aux(2*mt_max*2*mt_max),zb(2*mt_max*(8+nsolmax))
dimension bz(1710),by(1710)						

dimension temp_(1710)
double precision ddtemp(100,9)

!***********************************************************************
!***********************************************************************

bid1(:,:)=0.d00
bid2(:,:)=0.d00

! mise a zero d'abcd
if(nel.eq.1)then
   do i=1,8
      abcd(i)=0.d00
   enddo
endif

!cfr subr ent et bo2
!do i=1,nsol
!   npt(i,nel)=0
!enddo

!rem !!! On lit phil (pas panneau(nel).phil) car dans l'écriture du fichier, on écrit : dabs(panneau(nel).phil) !!!

!read(iu_23(iboat))   panneau(nel).abtr(:),phil,panneau(nel).mt,teta,xi(:,nel),xf(:,nel),    &
!                     panneau(nel).hxtr(:),panneau(nel).wxtr(:),panneau(nel).q,panneau(nel).kse,panneau(nel).kst,panneau(nel).ksa, &
!                     panneau(nel).txtr(:),panneau(nel).dxtr(:),panneau(nel).ksr,aaa,bbb,ccc,ddd,delto,    &
!					 ivaria(nel),const,const2,const3,bid,ibid


phil = dabs(panneau(nel).phil)
teta = panneau(nel).teta
delto = width*0.05
const(:) = panneau(nel).const(:)
const2(:,:) = panneau(nel).const2(:,:)
const3(:,:) = panneau(nel).const3(:,:)


!if(ich.eq.1) read(iu_22(iboat))                                &
!              (npt(i,nel),i=1,nsol),                           &
!              (((cha(i,j,kk,nel),i=1,100),j=1,3),kk=1,nsol),   &
!              (((dcha(i,j,kk,nel),i=1,100),j=1,nsol),kk=1,9)   

! var. de concept. = epaisseur du bordage (k=1)
! ---------------------------------------------
!	 const(1)= d  -> const2(1,1)=d(d)/d(épaiss)			
!	 const(2)= k  -> const2(2,1)=d(k)/d(épaiss)			
!	 const(3)= hy -> const2(3,1)=d(hy)/d(épaiss)			
!	 const(4)= ry -> const2(4,1)=d(ry)/d(épaiss)			
!	 const(5)= hx -> const2(5,1)=d(hx)/d(épaiss)			
!	 const(6)= rx -> const2(6,1)=d(rx)/d(épaiss)			

! var. de concept. relative aux cadres (k=2,5) et aux raid.(k=6,9) 
! ---------------------------------------------------------------
! constante	    dérivée(k=2,5)   constante	 dérivée(k=6,9)
!    1 const(3)= oy -> const2(1,k)     const( 9)= ox -> const2(1,k)				
!    2 const(4)= hy -> const2(2,k)     const(10)= hx -> const2(1,k)				
!    3 const(5)= sy -> const2(3,k)     const(11)= sx -> const2(1,k)				
!    4 const(6)= ry -> const2(4,k)     const(12)= rx -> const2(1,k)				
!    5 const(7)= ty -> const2(5,k)     const(13)= tx -> const2(1,k)				
!    6 const(8)= ly -> const2(6,k)     const(14)= lx -> const2(1,k)				
!
!calcul des arguments des fonctions exponentielles
!-------------------------------------------------
!les valeurs absolues de leurs parties reelles et complexes sont
!entreposees successivement dans le vecteur arg.

!
eta1=eta(indMateriau(nel))
ityp1=itype(nel)

pi1=pi/180.d00   ! constant
lamb=lam(1)
eta2=(1.d00-eta1)/2.d00
h=const(1)+2.d00*(const(5)+const(11))
g=const(1)*eta1
z1=(h-g)/2.d00
a11=(h+g)/2.d00
b11=const(2)+const(12)
c=const(1)+const(9)
d=2.d00*const(2)+const(7)+const(13)
f=const(8)+const(14)

if((ityp1.eq.1).or.(ityp1.eq.2)) then
   x=const(2)+const(6)  ! cas normal
   w=const(1)+const(3)
else if((ityp1.eq.3).or.(ityp1.eq.4).or.(ityp1.eq.5)) then			
   x=const(6)  ! la contribution transversale du bordé est supprimée
   w=const(3)
else
   write(iu_11(iboat),*) 'in bo1: ityp1 is wrong =',ityp1
   write(iu_14(iboat),*) 'in bo1: itype is wrong =',ityp1			
   read(*,*)

   stop
endif

bc=b11*c-const(10)*const(10)
xw=x*w-const(4)*const(4)
cw=c*w-g*g
bw=b11*w+x*c
oxy=c+w
dxy=oxy-2.d00*g*eta1
rxy=b11+x
wc=const(10)*w+const(4)*c
gh=c*w-g*h
ss=const(5)+const(11)       ! sx+sy	
tt=const(7)+const(13)       ! tx+ty	


if(ityp1.eq.5) then													

   !cas d'une epontille											

   aetoil=(const(9)*const(12)-(const(10)*const(10)))*ss		
   cetoil=const(12)*const(3 )* const(9 )+const(9 )  *ss*tt	      &			
                   -const(3 )* const(10)*const(10)						
   detoil=0.d+00												
   eetoil=0.d+00												
   fetoil=(ss*(const(12)*const(3)  +   const(6)*const(9)	      &
            +2*const(10)*const(4)))+tt*const(9)*const(3)		
   jetoil=     const(6)*const(3)*const(9)+ss*tt*const(3)	      &
              -const(9)*const(4)*const(4)							
   ketoil= ss*(const(3 )*const(6)  -   const(4)*const(4))					
   betoil=0.d+00													
   getoil=0.d+00													
   hetoil=0.d+00													
   ietoil=0.d+00													
else																
   aetoil=z1*bc
   cetoil=(b11*w+d*z1)*c - (g*f+w*const(10))*const(10) - b11*g*h
   fetoil=d*gh + f*wc + const(4)*const(10)*2.d00*a11 + z1*bw
   !getoil=.d+00  (cfr version p : nxyyx)
   jetoil=w*(c*x+d*z1) - const(4)*(const(4)*c+g*f) - g*h*x
   ketoil=z1*xw

   if(panneau(nel).q.ge.(1.e5)) then
!     cas d'une plaque raidie  (rayon très grand > 10000 m)
      betoil=0.d+00
      detoil=0.d+00
      eetoil=0.d+00
      ietoil=0.d+00
   else
!     cas d'une coque raidie  (rayon <10000 m)
      betoil=2.d00*g*const(10)*z1/panneau(nel).q
      detoil=z1*cw/(panneau(nel).q*panneau(nel).q)
      eetoil=-(f*cw+2.d+00*z1*wc)/panneau(nel).q
      ietoil=2.d00*const(4)*g*z1/panneau(nel).q
   endif
endif															

!calcul des coefficients (equ) de l'équation diff. du 8ème ordre
!   (voir dans subr. darg)
!------------------------------------------------------------------

call assemb (lam,equ,aetoil,betoil,cetoil,detoil,eetoil,fetoil,ietoil,jetoil,ketoil)

if(impr.ne.0) then
   write(iu_11(iboat),4)  nel
   write(iu_11(iboat),*) 'a*  b*  c* ',aetoil,betoil,cetoil,detoil
   write(iu_11(iboat),*) 'd*  e*  f*,',detoil,eetoil,fetoil
   write(iu_11(iboat),*) 'i*  j*  k*,',ietoil,jetoil,ketoil
   write(iu_11(iboat),12) ijk,(equ(i),i=1,9)
endif

!resolution de l'equation differentielle du 8ème ordre (call polrt)
!discussion des resultats  (call discu)
!------------------------------------------------------------------

do i=1,9
   equ2(i)=equ(i)
enddo

if=1

call polrt (equ2,cof,8,root,root1,ier)


call discu (root,root1,ier,arg,if,m) 


if(if.eq.0) then
   write(*,*) 'Error panel:',nel
   write(*,*)"error dans la résolution de l'équ. diff.(subr. discu)"
   write(*,*)"error solving diff. equation (subr. discu)"
   write(*,*)
   write(*,*) 'Debug Info : epsa = ',panneau(nel).epsa,', epsr = ',panneau(nel).epsr
   write(*,*)"verifier"
   write(*,*)"les dimensions des raidisseurs et cadres"
   write(*,*)"peuvent être trop petites."
   write(*,*)"check"
   write(*,*)"the dimensions of the stiffeners and frames"
   write(*,*)"are maybe to small."
   write(iu_11(iboat),*)"error dans la résolution de l'équ. diff.(subr. discu)"
   write(iu_11(iboat),*)"error solving diff. equation (subr. discu)"
   write(iu_11(iboat),*)
   write(iu_11(iboat),*)"verifier"
   write(iu_11(iboat),*)"les dimensions des raidisseurs et cadres"
   write(iu_11(iboat),*)"peuvent être trop petites."
   write(iu_11(iboat),*)"check"
   write(iu_11(iboat),*)"the dimensions of the stiffeners and frames"
   write(iu_11(iboat),*)"are maybe to small."
   write(iu_14(iboat),*)"error dans la résolution de l''équ. diff.(subr.discu)"																		
   write(iu_14(iboat),*)"verifier les dimensions des raidisseurs et cadres peuvent être trop petites."
   write(iu_14(iboat),*)														

   write(*,*) 'stop'
   read(*,*)

   stop
endif

mm=2*m
ms=2*panneau(nel).mt
do i=1,mm
   argq(i)=arg(i)*panneau(nel).q
enddo


if(impr.eq.1) write(iu_11(iboat),42) (arg(i),i=1,mm)

!calcul des derivees des arg(i) par rapport aux var. de conception,
!soit darg(i,k) avec i=1,m et k=1,nvar(nel,iboat)                                           
!------------------------------------------------------------------
if(iopti.ge.1) then
   if((impr.ne.0).and.(nvar(nel,iboat).ne.0)) write(iu_11(iboat),644)
   it=1
   do ik=1,nvar(nel,iboat)
      kk=nxit(ik,nel,iboat)
      call dargsr(lam,dequ,panneau(nel).q,const,a11,b11,c,d,f,h,g,x,w,z1,bc,cw,bw,  &
                           xw,dxy,wc,oxy,rxy,gh,eta1,eta2,kk,da,					&
                           ss,tt,ityp1,const2)

      !call dargsr_review(lam,dequ,panneau(nel).q,const,eta1,k,da,ityp1,const2)

      do i=1,m
         call comple(equ,dequ,arg,darg,i,kk,disa(1),disa(33),it,alg(1))
      enddo

      if(impr.eq.1) then
         write(iu_11(iboat),'(a30,i2)') 'variable de conception nø ',kk 
         write(iu_11(iboat),'(a25,9e12.5)')'d(equ)/dxi  =',dequ 
         write(iu_11(iboat),'(a25,4e12.5)')'d(alpha)/dxi=',(darg(2*i-1,kk),i=1,m)
         write(iu_11(iboat),'(a25,4e12.5)')'d(béta )/dxi=',(darg(2*i,kk),i=1,m)
         it=it+1
      endif
   enddo
endif


!calcul des coefficients des diverses grandeurs utiles (call coe)  
!---------------------------------------------------------------             
!entrepose en coeff(i,j) les coefficients de exp(-a*y)*cos(b11*y),   
!exp(-a*y)*sin(b11*y), exp(-a(2*pi-y))*cos(b11(2*pi-y)), exp(-a(2*pi-y))*
!sin(b11(2*pi-y)),..., pour chaque racine complexe precedement definie
!et pour chaque effet a considerer, correspondant respectivement a  
!u,v,dw/dy,ny,nx,nyx,nxy,my,mx,myx,mxy,qy,qx,ry,rx,w et 10 fois    
!(xodx,zodx)  pour un maximum de 10 traverses

do i=1,4
   sinu(i)=0.d00
   cosi(i)=0.d00
   expt(i)=0.d00
enddo

do i=1,m
   expt(i)=expo(argq(2*i-1)*2*pi)
enddo

call coe(const,arg,coeff,lam,eta1,eta2,panneau(nel).mt,m,z1,x,w, &
         a11,b11,c,f,h,g,panneau(nel).q,nvar(nel,iboat),nxit(:,nel,iboat),dcoeff,   &
         darg,const2,const3,alg(1),ityp1)


!calcul des effets des quatre charges lineaires et de la charge
!exterieure
!--------------------------------------------------------------
!ils sont entreposes dans disc(i),disb(i),disa(i),disd(i), avec 16
!memoires pour chaque effet, et dans dish(i) avec 37 memoires pour
!chaque effet.

do 507 i=1,4
  k(i)=1
507  continue


goto(503,504,505),m-1
503 ip(1)=1
    ip(2)=2
    ip(3)=9
    ip(4)=10
    k(3)=2
    k(4)=2
    sinu(1)=dsin(argq(2)*2.d00*pi)
    sinu(2)=dsin(argq(4)*2.d00*pi)
    cosi(1)=dcos(argq(2)*2.d00*pi)
    cosi(2)=dcos(argq(4)*2.d00*pi)
    goto 506
504 ip(1)=1
    ip(2)=9
    ip(3)=17
    ip(4)=18
    k(2)=2
    k(3)=3
    k(4)=3
    sinu(3)=dsin(argq(6)*2.d00*pi)
    cosi(1)=dcos(argq(2)*2.d00*pi)
    cosi(2)=dcos(argq(4)*2.d00*pi)
    cosi(3)=dcos(argq(6)*2.d00*pi)
    goto 506
505 ip(1)=1
    ip(2)=9
    ip(3)=17
    ip(4)=25
    k(2)=2
    k(3)=3
    k(4)=4
    cosi(1)=dcos(argq(2)*2.d00*pi)
    cosi(2)=dcos(argq(4)*2.d00*pi)
    cosi(3)=dcos(argq(6)*2.d00*pi)
    cosi(4)=dcos(argq(8)*2.d00*pi)
506 continue

if((impr.eq.1).and.(iopti.eq.0)) write(iu_11(iboat),50)
if((impr.eq.1).and.(iopti.ge.1)) write(iu_11(iboat),51)
q2pi = 2.d00*pi*panneau(nel).q

if(impr.eq.1) write(iu_11(iboat),*) 'calcul relatif à disc'
if(impr.eq.1) write(iu_11(iboat),*) '______________________'

call linea(m,coeff,dcoeff,2,6,3,14,disc,hyp,k,panneau(nel).mt,ip,1,impr,    &
           q2pi,dpx,px,darg,nxit(:,nel,iboat),nvar(nel,iboat),ilinea(1),ilinea(5),iopti,    &
           sinu,cosi,expt)


if(iopti.ge.1)then
   call dpar(m,px,coeff,dcoeff,dpx,dvarc,1,nxit(:,nel,iboat),nvar(nel,iboat),panneau(nel).mt)
endif

if((impr.ne.0).and.(iopti.ge.1)) then
   do  il=1,nvar(nel,iboat)
       l=nxit(il,nel,iboat)
       write(iu_11(iboat),*) 'var. de concept. nø',l
       write(iu_11(iboat),47) (dpx(i,l),i=1,8)
47     format('dpx=',5x,8(e13.6,1x))
       write(iu_11(iboat),*) ' '
       do ii=1,13+2*panneau(nel).mt
          write(iu_11(iboat),123)ii,(dvarc(ii,l,i),i=1,16)
       enddo
 enddo
123    format('fct.',i2,3x,8(e13.6,1x)/,9x,8(e13.6,1x))
endif


if(impr.eq.1) write(iu_11(iboat),*) 'calcul relatif à disb'
if(impr.eq.1) write(iu_11(iboat),*) '______________________'

call linea(m,coeff,dcoeff,2,3,14,6,disb,hyp,k,panneau(nel).mt,ip,1,impr,  &
           q2pi,dpx,px,darg,nxit(:,nel,iboat),nvar(nel,iboat),ilinea(1),ilinea(5),iopti,  &
           sinu,cosi,expt)


if(iopti.ge.1)then
   call dpar(m,px,coeff,dcoeff,dpx,dvarb,1,nxit(:,nel,iboat),nvar(nel,iboat),panneau(nel).mt)
endif

if(impr.eq.1) write(iu_11(iboat),*) 'calcul relatif à disa'
if(impr.eq.1) write(iu_11(iboat),*) '______________________'
call linea(m,coeff,dcoeff,16,1,8,4,disa,hyp,k,panneau(nel).mt,ip,-1,impr,  &
           q2pi,dpx,px,darg,nxit(:,nel,iboat),nvar(nel,iboat),ilinea(1),ilinea(5),iopti,   &
           sinu,cosi,expt)


if(iopti.ge.1)then
   call dpar(m,px,coeff,dcoeff,dpx,dvara,-1,nxit(:,nel,iboat),nvar(nel,iboat),panneau(nel).mt)
endif

if(impr.eq.1) write(iu_11(iboat),*) 'calcul - relatif à disd'
if(impr.eq.1) write(iu_11(iboat),*) '______________________'
call linea(m,coeff,dcoeff,16,1,4,8,disd,hyp,k,panneau(nel).mt,ip,-1,impr,  &
           q2pi,dpx,px,darg,nxit(:,nel,iboat),nvar(nel,iboat),ilinea(1),ilinea(5),iopti,   &
           sinu,cosi,expt)


if(iopti.ge.1)then
   call dpar(m,px,coeff,dcoeff,dpx,dvard,-1,nxit(:,nel,iboat),nvar(nel,iboat),panneau(nel).mt)
endif

! ----------------------------------------------------------------------
!calcul de vø,uø,wø, vøø,uøø,wø, vøøø,uøøø,wøøø                     
!pour les cas de charge disa,disb,disc,disd   ( cfr subroutine  deriv)       
!                                                              
!complement au calcul des coefficients des diverses grandeurs      
!utiles  c-a-d  nx,nxy,mx,mxy,myx,qx,rx   (cfr subroutine manq )
! ----------------------------------------------------------------------
!
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
temp_(1:720) = disa(:)       !TODO car passe pas avec new compil => a remettre au propre
call manq(lam,eta1,const,temp_,16,panneau(nel).q,ityp1)		
disa(:) = temp_(1:720)	
temp_(1:720) = disb(:)
call manq(lam,eta1,const,temp_,16,panneau(nel).q,ityp1)
disb(:) = temp_(1:720)	
temp_(1:720) = disc(:)
call manq(lam,eta1,const,temp_,16,panneau(nel).q,ityp1)
disc(:) = temp_(1:720)	
temp_(1:720) = disd(:)
call manq(lam,eta1,const,temp_,16,panneau(nel).q,ityp1)			
disd(:) = temp_(1:720)	

!integration de la pression hydraulique et charges de bord.
!===========================================================
!(subroutines hydros, press, bord, mom )
!
do i=1,1710  ! mise a zero (initialisation de dish)
   do is=1,nsol
      dish(i,is)=0.d00
   enddo
enddo

if(iopti.ge.1) then   ! mise a zero (initialisation de dvarh)
   do 8427 is=1,nsol
   do 8427 ii=1,13+2*panneau(nel).mt
   do 8427 kk=1,nvar(nel,iboat)
      k1=nxit(kk,nel,iboat)
      do 8427 j=1,38
         dvarh(ii,k1,j,is)=0.d00
   8427   continue
endif

!subr press et hydros
bkse=1.d00
ze  =1.d-60
if(panneau(nel).kse.eq.1) bkse=-1.d00

if(ibat.eq.1) then  !ibat=0 si terme pair (-->pas utile pour charge sym)
   if(panneau(nel).kse.ne.0) then
      if(ivaria(nel).eq.1) then  ! subr press et dpress
         do is=1,nsol

            call press(arg,m,xi(is,nel),xf(is,nel),phil,disc,dish(1,is),  &
	                panneau(nel).q,ijk,ms,bkse)

         enddo
         if((iopti.ge.1).and.(nvar(nel,iboat).ge.1)) then
            do is=1,nsol

               call dpress(arg,darg,m,xi(is,nel),xf(is,nel),phil,panneau(nel).q,  &
                    disc,dvarc,dvarh(1,1,1,is),bkse,ijk,nxit(:,nel,iboat),nvar(nel,iboat),panneau(nel).mt,     &
					sinu,cosi,expt)

   	       enddo
         endif
      else    ! subr hydro et dhydro
       do is=1,nsol

            call hydro(arg,m,xi(is,nel),phil,disc,dish(1,is),  &
                       teta,panneau(nel).q,ijk,ms,bkse,sinu,cosi,expt)

       enddo
         if((iopti.ge.1).and.(nvar(nel,iboat).ge.1)) then
            do is=1,nsol

               call dhydro(arg,darg,m,xi(is,nel),phil,panneau(nel).q,     &
                  disc,dvarc,dvarh(1,1,1,is),bkse,ijk,teta,nxit(:,nel,iboat),nvar(nel,iboat),panneau(nel).mt,   &
                    sinu,cosi,expt)

            enddo
         endif
      endif  ! if(ivaria.eq.1)
   endif  ! if(kse.ne.0) 
endif  ! if(ibat.eq.1)

call annuld(dtemp,11286)   ! dtemp = dvarh(33,9,38)

!  boucle sur loc,locy et locz + mom (bateau et bus)
do 5326 is=1,nsol    ! boucle sur les is cas de charge

   ! subr loc,locy et locz   

   if(ich.ne.0) then                                               
      call locy(arg,m,1.0d+00,phil,disa,dtemp(1),dish(:,is),teta,panneau(nel).q,  &
                ijk,lamb,width,ms,cha(1:100,1:3,is,nel),npt(is,nel),by,sinu,cosi,expt)

      call locz(arg,m,-1.0d+00,phil,disc,dtemp(1),dish(:,is),teta,panneau(nel).q, &
                ijk,lamb,width,ms,cha(1:100,1:3,is,nel),npt(is,nel),bz,sinu,cosi,expt)


      call loc(arg,m,phil,disc,dtemp(1),dish(:,is),panneau(nel).q,      &
               ijk,lamb,width,ms,cha(1:100,1:3,is,nel),npt(is,nel),bkse,sinu,cosi,expt)


      if((iopti.ge.1).and.(nvar(nel,iboat).ge.1)) then
         ddtemp(1:100,1:9) = dcha(1:100,is,1:9,nel)
         call dlocy(arg,darg,m,+1.0d+00,phil,disa,dvara,dtemp(1),dvarh(:,:,:,is),teta,panneau(nel).q,  &
		            ijk,lamb,width,cha(1:100,1:3,is,nel),ddtemp,npt(is,nel),nxit(:,nel,iboat),nvar(nel,iboat),panneau(nel).mt,by,sinu,cosi,expt)
         dcha(1:100,is,1:9,nel) = ddtemp(1:100,1:9)

         ddtemp(1:100,1:9) = dcha(1:100,is,1:9,nel)
         call dlocz(arg,darg,m,-1.0d+00,phil,disc,dvarc,dtemp(1),dvarh(:,:,:,is),teta,panneau(nel).q,   &
		               ijk,lamb,width,cha(1:100,1:3,is,nel),ddtemp,npt(is,nel),nxit(:,nel,iboat),nvar(nel,iboat),panneau(nel).mt,bz,sinu,cosi,expt)
         dcha(1:100,is,1:9,nel) = ddtemp(1:100,1:9)
         
         call dloc(arg,darg,m,phil,disc,dvarc,dtemp(1),dvarh(:,:,:,is),panneau(nel).q,       &
		           ijk,lamb,width,cha(1:100,1:3,is,nel),npt(is,nel),bkse,nxit(:,nel,iboat),nvar(nel,iboat),panneau(nel).mt,sinu,cosi,expt)


      endif
   endif  

   !lecture des données relatives aux moments de bords
   if(ityp1.eq.5) goto 5326								
   if(imom.eq.0) then
      call annuld(abcd,8)
   else
      read(iu_21(iboat)) abcd
   endif

!----------------------------------------------------------------------------
!lecture des efforts nb et moments mb de bord(dans file busc.txt et busm.txt)
!! nb et mb  sont les mêmes pour tous les cas de charge !!!!
!----------------------------------------------------------------------------

   if(ibusc.eq.0) goto 32

!a) force nb (agissant dans le plan du bordé)
!   ---------
   if((nel.eq.1).and.(is.eq.1).and.(ijk.eq.1)) then
      open(iu_33(iboat),file=nom//'busc.txt',status='old',err=657)
	  read(iu_33(iboat),'(a1)') ibus ! skip ligne titre
	  goto 658
      657 write(*,*)'stop: nom du fichier (busc.txt) est incorrect'
      write(*,*)'    : error, (busc.txt) data file not valid'
      write(iu_14(iboat),*) 'stop: nom du fichier (busc.txt) est incorrect'	!sept06				!bug
      stop  
      658 continue
   endif

   if(is.eq.1) then
      if((nel.eq.1).and.(ijk.gt.1)) then
         rewind iu_33(iboat)
	     read(iu_33(iboat),'(a1)') ibus ! skip ligne titre
	  endif
	  read(iu_33(iboat),*) busc1,busc2,busc3,busc4,busc5,busc6,busc7,busc8
      if(ijk.eq.1) then
	     write(iu_11(iboat),'(a,i3,8(1x,e11.4),a)') 'nel=',nel,busc1,busc2,busc3,busc4,busc5,busc6,busc7,busc8,' forces(n/m)'
	  endif
   endif

!  charge en x**3
!----------------
   abcd(1)=abcd(1)+ busc1   ! les efforts de buscage nb 
   abcd(5)=abcd(5)+ busc5   ! s'ajoutent aux effets du moment d'ensemble (subr bateau)
!  charge en x**2
!----------------
   abcd(2)=abcd(2)+ busc2
   abcd(6)=abcd(6)+ busc6
! charge en x
   abcd(3)=abcd(3)+ busc3
   abcd(7)=abcd(7)+ busc7
! charge uniforme
   abcd(4)=abcd(4)+ busc4
   abcd(8)=abcd(8)+ busc8

!b) moments mb
!   ----------
   if((nel.eq.1).and.(is.eq.1).and.(ijk.eq.1)) then
      open(iu_20(iboat),file=nom//'busm.txt',status='old',err=659)
	  read(iu_20(iboat),'(a1)') ibus ! skip ligne titre
	  goto 660
      659 write(*,*)'stop: nom du fichier (busm.txt) est incorrect'
      write(*,*)'    : error, (busm.txt) data file not valid'
      write(iu_14(iboat),*) 'stop: nom du fichier (busm.txt) est incorrect'
      stop  
      660 continue
   endif

   if(is.eq.1) then
      if((nel.eq.1).and.(ijk.gt.1)) then
         rewind iu_20(iboat)
	     read(iu_20(iboat),'(a1)') ibus ! skip ligne titre
	  endif
	  read(iu_20(iboat),*) busm1,busm2,busm3,busm4,busm5,busm6,busm7,busm8
      if(ijk.eq.1) then
	     write(iu_11(iboat),'(a,i3,8(1x,e11.4),a)') 'nel=',nel,busm1,busm2,busm3,busm4,busm5,busm6,busm7,busm8,' moments(n.m/m)'
	  endif
   endif

   !transfer moment (mb en n.m/m) --> charge (q en n/m /m)
   ! avec  q= mb*(ijk*pi/l)  (n/m par mètre courant selon y)
   !       q est du même type que la charge unitaire  disc
   !       disc = 10000 sin(lamb x) selon -0z
   
   xmult = ijk*pi/width  ! avec mb développé sur 5% de l

   ! moment en x**3  : busm1 & busm5
   ! moment en x**2  : busm2 & busm6
   ! moment en x     : busm3 & busm7
   ! moment uniforme : busm4 & busm8

   !forces "q" concentrees entre x=0 et x=d=delto)
   !(on divise par 10000 car disb correspond à une charge unitaire de 10000)
   !(  avec disc = 10000 sin(lamb x) ) selon -oz
   
   e1=2.d00*dsin(ijk*pi*delto/width) /(ijk*pi*delto*10000.d00)  

   !forces "q" concentrees entre x=l-d et x=l)
   
   e2=2.d00*dsin(ijk*pi*(width-delto)/width)/(ijk*pi*delto*10000.d00)
   a2=xmult*(busm1*e1+busm5*e2)
   b2=xmult*(busm2*e1+busm6*e2)
   c9=xmult*(busm3*e1+busm7*e2)
   d2=xmult*(busm4*e1+busm8*e2)

   call mom(arg,m,a2,b2,c9,d2,phil,disc,dtemp,dish(1,is),panneau(nel).q,ms)
 
   if((iopti.ge.1).and.(nvar(nel,iboat).ge.1)) then
      call dmomsr(arg,darg,m,phil,disc,dvarc,dtemp,dvarh(1,1,1,is),panneau(nel).q, &
                  nxit(:,nel,iboat),nvar(nel,iboat),panneau(nel).mt,a2,b2,c9,d2,sinu,cosi,expt)
   endif

   32  continue
 
!---------------fin lecture forces de buscage -----------------------------------------
  
   !moment m1 + nb (forces concentrees entre x=0 et x=d=delto)
   !(on divise par 10000 car disb correspond à une charge unitaire de 10000)
   !(  avec disb = 10000 cos(lamb x) )selon ox
  
   e1=2.d00*dsin(ijk*pi*delto/width) /(ijk*pi*delto*10000.d00)  
  
   !moment m2 +nb (forces concentrees entre x=l-d et x=l)
   
   e2=2.d00*dsin(ijk*pi*(width-delto)/width)/(ijk*pi*delto*10000.d00)
   
   !write(6,*)' pour m1, e1=',e1,' pour m2, e2=',e2
   a2=abcd(1)*e1+abcd(5)*e2
   b2=abcd(2)*e1+abcd(6)*e2
   c9=abcd(3)*e1+abcd(7)*e2
   d2=abcd(4)*e1+abcd(8)*e2

   call mom(arg,m,a2,b2,c9,d2,phil,disb,dtemp,dish(1,is),panneau(nel).q,ms)
   
   if((iopti.ge.1).and.(nvar(nel,iboat).ge.1)) then
      call dmomsr(arg,darg,m,phil,disb,dvarb,dtemp,dvarh(1,1,1,is),panneau(nel).q,  &
	              nxit(:,nel,iboat),nvar(nel,iboat),panneau(nel).mt,a2,b2,c9,d2,sinu,cosi,expt)
   endif

5326 continue  ! ** fin de boucle sur les cas de charge
    
  
! ----------------------------------------------------------------------
!calcul de vø,uø,wø, vøø,uøø,wø, vøøø,uøøø,wøøø  ( cfr subroutine  deriv)                   
!pour les cas de charge                                       
!dish,  disla,dislb,dislc,disld   et   dismla,disma,dismc,dismd                                 
!  +                                                                
!complement au calcul des coefficients des diverses grandeurs      
!utiles  c-a-d  nx,nxy,mx,mxy,myx,qx,rx   (cfr subroutine manq )
! ----------------------------------------------------------------------
do is=1,nsol
   call deriv(3,m,01,dish(1,is),arg,30,panneau(nel).q)
   call deriv(3,m,30,dish(1,is),arg,33,panneau(nel).q)
   call deriv(3,m,33,dish(1,is),arg,36,panneau(nel).q)
   call manq(lam,eta1,const,dish(1,is),38,panneau(nel).q,ityp1)	
enddo    
         
if((ijk.ge.2).or.(impr.eq.0)) goto 315
write(iu_11(iboat),54)
do i=1,3 
   j1=(i-1)*16+1
   write(iu_11(iboat),55) (disc(j),disb(j),disa(j),disd(j),j=j1,(j1+4*m-1))
enddo     
          
if((panneau(nel).kse.eq.0).and.(ich.eq.0).and.(imom.eq.0)) goto 315
write(iu_11(iboat),58)
do is=1,nsol
   if(is.eq.1) then
      write(iu_11(iboat),*) ' cas de charge nø',is
	  call pimpr(dish(1,is),ms,m)
   endif   
enddo      
315 continue
           
           
do ij=1,2*mt_max*2*mt_max
   aux(ij)=0.d00
enddo      
           
do ij=1,2*mt_max*(8+nsolmax)
   zb(ij)=0.d00
   hyp(ij)=0.d00
   ali(ij)=0.d00
enddo       
            
!  =====================================================================
            
!  expression partielle des dérivées des disa, disb, ... et dish (voir linea)
!  --------------------------------------------------------------------------
!	dvara(33,9,16), dvarb, dvarc et dvard (pour disc, disa, disb et disd) 
!     avec 13+2*panneau(nel).mt variables (u v wø ny nyx my ry w wøø wøøø uø vø vøø +
!                  xo, zo),
!		        9 variables de conception,
!		  et   16 termes.
!  dvarh(33,9,38,is) avec 38 termes pour dish avec is le nø du cas de
!  charge (is=1,nsol)
        
!voir subr dpar et subr dpress ou dhydro (ci-avant)

if((impr.ne.0).and.(iopti.ne.0).and.(nvar(nel,iboat).ne.0)) then
   do is=1,nsol
      write(iu_11(iboat),*)' cas de charge nø',is
      write(iu_11(iboat),*)' ====================='
      do ivar=1,nvar(nel,iboat)
	     iv=nxit(ivar,nel,iboat)
      	 write(iu_11(iboat),*)' dérivée de dish , var. xi nø',iv
      	 write(iu_11(iboat),*)' ---------------------------------'
	     do ii=1,13+2*panneau(nel).mt
      	    write(iu_11(iboat),'(a10,i3,6e12.5)')' fonct nø',ii,(dvarh(ii,iv,i,is),i=1,6)
		    write(iu_11(iboat),'(8e12.5)') (dvarh(ii,iv,i,is),i=7,14)
		    write(iu_11(iboat),'(8e12.5)') (dvarh(ii,iv,i,is),i=15,22)
       	    write(iu_11(iboat),'(8e12.5)') (dvarh(ii,iv,i,is),i=23,30)
         enddo
      enddo
   enddo
endif

!***********************************************************************
!partie ii:
!-----------
!début des calculs relatifs aux traverses
!***********************************************************************
if(panneau(nel).mt.eq.0) goto 622

!calcul de la matrice du systeme de resolution des inconnues
!hyperstatiques    (effets des traverses)
!------------------------------------------------------------------
do i=1,2
   call inteq(disb,m,i,1,aux,nel)
   call inteq(disc,m,i,2,aux,nel)
enddo

!calcul des termes independants du systeme de resolution des       
!inconnues hyperstatiques (effets des traverses)
!-----------------------------------------------------------
!charge exterieure (subroutines hyp1 et hyp2)
!8 cas de charges aux bords du panneau (subroutine vector)

if((panneau(nel).kse.ne.0).or.(ich.ne.0).or.(imom.ne.0)) then
   call hyp1(dish(1,1),zb,1,nel)
   do is=2,nsol
      is1= 8+is
      call hyp1(dish(1,is),zb,is1,nel)
   enddo
endif

      do 64 kk=1,m
      do 64 i=1,panneau(nel).mt
      sm =argq(2*kk-1)*panneau(nel).abtr(i)*pi1
      sn =argq(2*kk)  *panneau(nel).abtr(i)*pi1
      sp =argq(2*kk-1)*(phil-panneau(nel).abtr(i))*pi1
      sq =argq(2*kk)  *(phil-panneau(nel).abtr(i))*pi1
      smm=argq(2*kk-1)*(360.d00-panneau(nel).abtr(i))*pi1
      snn=argq(2*kk)  *(360.d00-panneau(nel).abtr(i))*pi1
      spp=argq(2*kk-1)*(360.d00-(phil-panneau(nel).abtr(i)))*pi1
      sqq=argq(2*kk)  *(360.d00-(phil-panneau(nel).abtr(i)))*pi1
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

      if((panneau(nel).kse.eq.0).and.(ich.eq.0).and.(imom.eq.0)) goto 6802
      call hyp2(dish(1,1),panneau(nel).mt,zb,tm,tn,tp,tq,tmm,tnn,tpp,tqq,kh,i,1)

      do is=2,nsol
        is1= 8+is
        call hyp2(dish(1,is),panneau(nel).mt,zb,tm,tn,tp,tq,tmm,tnn,tpp,tqq,kh,i,is1)
      enddo
6802  continue

      call vecto(disc,tm,tn,i,kk,panneau(nel).mt,2,+1.0d00,tmm,tnn,zb,nsolmax)
      call vecto(disc,tp,tq,i,kk,panneau(nel).mt,3,+1.0d00,tpp,tqq,zb,nsolmax)
      call vecto(disd,tm,tn,i,kk,panneau(nel).mt,4,+1.0d00,tmm,tnn,zb,nsolmax)
      call vecto(disd,tp,tq,i,kk,panneau(nel).mt,5,-1.0d00,tpp,tqq,zb,nsolmax)
      call vecto(disa,tm,tn,i,kk,panneau(nel).mt,6,+1.0d00,tmm,tnn,zb,nsolmax)
      call vecto(disa,tp,tq,i,kk,panneau(nel).mt,7,-1.0d00,tpp,tqq,zb,nsolmax)
      call vecto(disb,tm,tn,i,kk,panneau(nel).mt,8,+1.0d00,tmm,tnn,zb,nsolmax)
      call vecto(disb,tp,tq,i,kk,panneau(nel).mt,9,+1.0d00,tpp,tqq,zb,nsolmax)
      call plimit(disc,disb,+tm,+tn,tmm,tnn,i,kk,panneau(nel).mt,01,03,ali)
      call plimit(disc,disb,+tp,+tq,tpp,tqq,i,kk,panneau(nel).mt,02,03,ali)
      call plimit(disc,disb,+tm,+tn,tmm,tnn,i,kk,panneau(nel).mt,03,02,ali)
      call plimit(disc,disb,+tp,+tq,tpp,tqq,i,kk,panneau(nel).mt,04,02,ali)
      call plimit(disc,disb,+tm,+tn,tmm,tnn,i,kk,panneau(nel).mt,05,24,ali)
      call plimit(disc,disb,+tp,+tq,tpp,tqq,i,kk,panneau(nel).mt,06,24,ali)
      call plimit(disc,disb,+tm,+tn,tmm,tnn,i,kk,panneau(nel).mt,07,25,ali)
      call plimit(disc,disb,+tp,+tq,tpp,tqq,i,kk,panneau(nel).mt,08,25,ali)
      call plimit(disc,disb,-tm,-tn,-tmm,-tnn,i,kk,panneau(nel).mt,09,27,ali)
      call plimit(disc,disb,+tp,+tq,+tpp,+tqq,i,kk,panneau(nel).mt,10,27,ali)
      call plimit(disc,disb,-tm,-tn,-tmm,-tnn,i,kk,panneau(nel).mt,11,26,ali)
      call plimit(disc,disb,+tp,+tq,+tpp,+tqq,i,kk,panneau(nel).mt,12,26,ali)
      call plimit(disc,disb,-tm,-tn,-tmm,-tnn,i,kk,panneau(nel).mt,13,01,ali)
      call plimit(disc,disb,+tp,+tq,+tpp,+tqq,i,kk,panneau(nel).mt,14,01,ali)
      call plimit(disc,disb,-tm,-tn,-tmm,-tnn,i,kk,panneau(nel).mt,15,29,ali)
      call plimit(disc,disb,+tp,+tq,+tpp,+tqq,i,kk,panneau(nel).mt,16,29,ali)
   64 continue

!resolution des effets hyperstatiques des traverses
!---------------------------------------------------
nbrequ=8+nsolmax  ! + 2*4 (8 efforts de bord unitaires nb et mb)

call resolu(ms,nbrequ,aux(1),zb(1))

!============
!après resolu, la matrice aux est la matrice inverse (a conserver)
!           et la matrice hyp contient les vecteurs solutions.
if(impr.ge.1) then
   write(iu_11(iboat),65)
   do 66 i=1,nbrequ
      ij=1+(i-1)*20
      jm=ij+ms-1
   66   write(iu_11(iboat),67) (hyp(j),j=ij,jm)
endif

  622 continue

      if((impr.ge.1).and.(panneau(nel).mt.ge.1)) then
        write(iu_11(iboat),*)' '
        write(iu_11(iboat),*)'vecteur ali (w,u,..) pour les mt cas de charges'
        do 4599 ii=1,16
 4599   write(iu_11(iboat),'(i2,3(8e14.5/))') ii,(ali(i+ii*20),i=1,ms)
 	endif

!fin du calcul des inconnues hyperstatiques des traverses (partie ii)

!********************************************************************************
!partie iii:
!-----------
!   calcul des sensibilités (dérivées des déplac. par rapport aux var. de concept.
!********************************************************************************
!calcul des dérivées des disa, disb, ... et dish aux points choisis
!------------------------------------------------------------------
!sensa(13,10,9) avec 13 variables w,u,v,wø,my,ny,nyx,ry,wøø,wøøø,uø,vø,vøø
!	   		     et 10 points de calcul des sensibilités,
!             idem avec sensb,sensc,sensd et sensh
!ypt(10)  coordonnées selon oy des pts de calcul des sensibilités 

if(iopti.ge.1) then
   if(panneau(nel).mt.ge.1) then
      call dhyps(phil,teta,panneau(nel).q,m,panneau(nel).mt,ms,aux(1),bid1,bid2,nel)
   endif
   call sens(phil,teta,panneau(nel).q,m,panneau(nel).mt,nel)
endif


!********************************************************************************
!partie iv:
!-----------
!calcul de la matrice "d'assemblage" pour le calcul des inc. hyper. de bord
!********************************************************************************


!calcul des deplacements et reactions  sur les bords (1er partie)  
!(charge exterieure) - inf1 (effet des traverses)                  
!-----------------------------------------------------------------

if(panneau(nel).mt.ne.0) then
   call pinf1 (sum,ali,zb,hyp,0,0,ms,mt_max,nsolmax)

   do is=2,nsol
      is1= is*20+140     ! is1 = donnée
      is2= (is-1)*16     ! is2 = solution
      call pinf1 (sum,ali,zb,hyp,is1,is2,ms,mt_max,nsolmax)
   enddo
                                                                       
endif

!calcul des deplacements et reactions sur les bords (2ème partie)
!(charge exterieure) - inf2 et inf3                          	
!------------------------------------------------------------------


if(dabs(teta).ne.180.d00) goto 444
      c19=-1.d00
      s1=0.d00
      c9=-dcos(phil*pi1)
      s2=-dsin(phil*pi1)
      goto 446
444   if(dabs(teta).ne.90.d00) goto 445
      ii=1.d00
      if(teta.eq.-90.d00) ii=-1
      c19=0.d00
      s1=ii
      c9=-ii*dsin(phil*pi1)
      s2=ii*dcos(phil*pi1)
      goto 446
445   c19=dcos(teta*pi1)
      s1=dsin(teta*pi1)
      c9=dcos((teta+phil)*pi1)
      s2=dsin((teta+phil)*pi1)
446   ph=phil*pi1
      ph2=ph*ph
      ph3=ph2*ph


if((panneau(nel).kse.ne.0).or.(ich.ne.0).or.(imom.ne.0)) then
   do is=1,nsol
      is1= (is-1)*16
      call pinf2(zb,dish(1,is),s1,s2,c19,c9,ph,ph2,ph3,is1,mt_max,nsolmax)
   enddo
 endif


!calcul des deplacements et reactions  sur les bords
! (8 charges unitaires de bords)
!--------------------------------------------------------------
do kk=1,2*mt_max*2*mt_max
   aux(kk)=0.d00
enddo

do kk=1,m
   sp =argq(2*kk-1)*phil*pi1
   sq =argq(2*kk)  *phil*pi1
   smm=argq(2*kk-1)*2*pi
   snn=argq(2*kk)  *2*pi
   spp=argq(2*kk-1)*(360.d00-phil)*pi1
   sqq=argq(2*kk)  *(360.d00-phil)*pi1
   sp =expo(sp)
   smm=expo(smm)
   spp=expo(spp)
   tm=1.d00
   tn=0.d00
   tp =sp *dcos(sq)
   tq =sp *dsin(sq)
   tmm=smm*dcos(snn)
   tnn=smm*dsin(snn)
   tpp=spp*dcos(sqq)
   tqq=spp*dsin(sqq)

   if((panneau(nel).kse.ne.0).or.(ich.ne.0).or.(imom.ne.0)) then
      do is=1,nsol
         is1= (is-1)*16
         call pinf3(zb,dish(1,is),tp,tq,tmm,tnn,tpp,tqq,kk,is1,mt_max,nsolmax)
      enddo
   endif

   call pedge(tm,tn,tp,tq,tmm,tnn,tpp,tqq,kk,01,03,disa,disb,disc,disd,+1.0d+00,-1.0d+00,aux,mt_max)
   call pedge(tm,tn,tp,tq,tmm,tnn,tpp,tqq,kk,03,02,disa,disb,disc,disd,+1.0d+00,-1.0d+00,aux,mt_max)
   call pedge(tm,tn,tp,tq,tmm,tnn,tpp,tqq,kk,05,24,disa,disb,disc,disd,+1.0d+00,-1.0d+00,aux,mt_max)
   call pedge(tm,tn,tp,tq,tmm,tnn,tpp,tqq,kk,07,25,disa,disb,disc,disd,+1.0d+00,-1.0d+00,aux,mt_max)
   call pedge(tm,tn,tp,tq,tmm,tnn,tpp,tqq,kk,09,27,disa,disb,disc,disd,-1.0d+00,+1.0d+00,aux,mt_max)
   call pedge(tm,tn,tp,tq,tmm,tnn,tpp,tqq,kk,11,26,disa,disb,disc,disd,-1.0d+00,+1.0d+00,aux,mt_max)
   call pedge(tm,tn,tp,tq,tmm,tnn,tpp,tqq,kk,13,01,disa,disb,disc,disd,-1.0d+00,+1.0d+00,aux,mt_max)
   call pedge(tm,tn,tp,tq,tmm,tnn,tpp,tqq,kk,15,29,disa,disb,disc,disd,-1.0d+00,+1.0d+00,aux,mt_max)

enddo

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

!
!contribution des traverses à la matrice de rigidité
!---------------------------------------------------
if(panneau(nel).mt.gt.0) then
   do 75 j=1,8
   do 75 i=1,16
      ij=i+(j-1)*20
      sum=0.d00
      do 76 kk=1,ms
         kh=kk+i*20
         jm=kk+j*20
         sum=sum+ali(kh)*hyp(jm)
      76 continue

      aux(ij)=aux(ij)+sum/10000.d00

   75 continue
endif

if(impr.eq.0) goto 777

write(iu_11(iboat),4002) nel
do 8 i=1,16
   write(iu_11(iboat),4001) (aux((ii-1)*20+i),ii=1,8),(zb(i+(ii-1)*16),ii=1,nsol)
8 continue

777 continue

!***********************************************************************
!partie v:
!-----------
!stokage des resultats intermediaires
!***********************************************************************
write(iu_scratch_1(iboat,nel)) disa,disb,disc,disd,argq,m,hyp,lamb,ijk

if((panneau(nel).kse.eq.0).and.(ich.eq.0).and.(imom.eq.0)) then
else
   write(iu_scratch_1(iboat,nel)) dish ! ((dish(l,ll),l=1,1710),ll=1,nsol)
endif

if(iopti.ge.1) then
   if (idebug.eq.1) write(6667,*) sensh,sensa, sensb, sensc, sensd,sensaa,sensbb,senscc,sensdd, &
                 defa,defb,defc,defd,defaa,defbb,defcc,defdd
   write(iu_scratch_2(iboat,nel)) sensh,sensa, sensb, sensc, sensd,sensaa,sensbb,senscc,sensdd, &
                 defa,defb,defc,defd,defaa,defbb,defcc,defdd
   if (idebug.eq.1) backspace(6667)	
   backspace(iu_scratch_2(iboat,nel))	
endif


!***********************************************************************
!partie vi:
!-----------
!rangement des coefficients des equations de continuite et d'equilibre  
!et resolution lors du passage pour le dernier panneau.
!***********************************************************************

nvsmb=nsol
call mdr(nel,phil,nvsmb,aux,zb)

return

!******************************************************************
!formats
!******************************************************************
4  format(1h1,43x,16h panneau numero ,i2,/45x,18(1h+) )
12 format(    14h terme numero ,i2/' du developpement de la pression  &
              hydrostatique en serie de fourier'/1x,72(1h+),//'       &
			  coefficients de l''equation caracteristique,'/'         &
			  ordonnes par puissances ','croissantes',//(6(2x,e15.8)))

42 format(/'arguments des fonctions exponentielles (alpha,béta)',/(4(2x,e14.7)))

50 format(' résolutions des 4 cas de charge de base (c b a d)',                   &
          ' dont le vecteur indépendant est 0 0 0 -5000'/' la matrice des',       &
          ' coeff. et le vect solution sont :'/)

51 format(' résolutions des 4 cas de charge de base (c b a d)',                   &
          ' dont le vecteur indépendant est 0 0 0 -5000'/' la matrice des',       &
		  ' coeff., le vect solution (px) et les dérivées (dpx)'/                 &
          'de ce vecteur par rapport aux var. de conception sont :'/)

54 format(1h1,' coefficients des fonctions exponentielles,'/                          &
              ' pour les 4 cas de charges lineaires de base',/1x,45(1h+),/            &
			  ' chaque colonne contient les coefficients de exp(a*y)*cos(b*y),'/      &
			  ' exp(a*y)*sin(b*y),exp(a(2*pi-y))*cos(b(..))et exp(a(..))*sin(b(...)), &
			  '/' ordonnes comme dans arg(i),successivement',/                        &                         
			  ' pour v,u,w,xodx,zodx (maximum 10 groupes xodx,zodx),'/                &
			  ' my,ny,ry,nyx,qy,wø,  vø,uø,wø,vøø,uøø,wøø,vøøø,uøøø,wøøø et ',        &
			  ' nx,nxy,mx,mxy,myx,qx,rx.'//t4,'charge (c)',t21,'charge (b)',          &
			  t38,'charge (a)',t55,'charge (d)',//)

55 format(16(4(2x,e15.8)/))
58 format(1h1,' coefficients des fonctions exponentielles, pour la pression          &
                hydrostatique'/1x,74(1h+)/' chaque groupe  contient successivement   &
				le terme independant, '/1x,'les coefficients de y, y**2 et y**3,     &
				le coefficient de cos(teta+y), sin(teta+y)' /' de exp(a*y)*cos(b*y), &
				exp(a*y)*sin(b*y),exp(a*(phil-y))*cos(b*(phil-y)),...'/1x,           &
				'de exp(a*(2pi-y)*cos(b*(2pi-y)),exp(..)*sin(..),..'/1x,             &
				'de exp(a(2pi-(phil-y)))*cos(...),exp(..)*sin(..)'/1x,               &
				'ordonnes comme dans arg(i)., successivement pour'/                  &
				' vo uo wo xodx zodx (10 groupes xodx zodx maximum),                 &
				'/' my ny ry nyx qy wø      vø,uø,wø,vøø,uøø,wøø,vøøø,               &
				uøøø,wøøø et  nx nxy mx mxy myx qx rx.'//)

65   format(/'effets hyperstatiques exerces par les traverses'/                &
            48(1h+)/' ils sont ordonnes comme suit :'/                         &
	   	     ' - les lignes: phi et f successivement pour chaque traverses '/  &
		     ' - les colonnes, charge exterieur, charge c sup, charge c inf,', &
		     ' idem avec les charges c, d et a'/                               &
		     '   avec de plus les effets pour les 4 cas de charges de bords ', &
		     '(a,0,0,0 // 0,b,0,0 // 0,0,c,0 // 0,0,0,d)'/)
67   format(/6(1x,e11.4))
644  format(/'dérivées des coef. de l''équ. diff. du 8éme ordre par rapport aux variables de conception'/85(1h-))
2003 format(/' cas de charge de bord type a : a*(q*phil)**3=1000.,', 'b=c=d=0'/t2,67(1h-)//)
2004 format(/' cas de charge de bord type b : a=0, b(q*phil)**2=1000.,',' c=d=0 '/t2,67(1h-)//)
2005 format(/' cas de charge de bord type c : a=0, b=0, c(q*phil)=1000., d=0'/t2,67(1h-)//)
2006 format(/' cas de charge de bord type d : a=0, b=0, c=0, d=1000.'/t2,62(1h-)//)
2007 format(/,t5,30(1h*)/t5,1h*,t10,'les forces de bords' ,t35,1h*/t5,1h*,t35,1h*/t5,30(1h*))
2008 format(/,t5,30(1h*)/t5,1h*,t10,'les moments de bords',t35,1h*/t5,1h*,t35,1h*/t5,30(1h*))
4001 format(/4(1x,e11.4)/4(1x,e11.4)/9(1x,e11.4))

 4002 format(1h1,'pour le calcul des effets hyperstatiques de bord du ',            &
                 'panneau ',i3, ' (ordre c d a b)'/                                 &
				 'voici par ligne la matrice des coeff. des inc. hyperstatiques,'/  &
				 ' puis les 9 termes indep. pour la p.h. et les 8 effets de bords : &
				 '/' w(y=0),w(y=phil),u(y=0),u(..),my,..,ny,..,nyx,..,ry,..,'/      &
				 ' v,..,dw/dy,...')



end

