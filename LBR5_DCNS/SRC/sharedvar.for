      module sharedvar
      integer*4 langue
      integer*4 mdp(15,4),mar(15,4),negalt
	integer*4 iweight,iprice								!fev2007
	integer*4 impr2,impr,sym,imod,ielt,isymx,isymy,inert	!sept2006
	integer*4 iana											!fev2007
	real*8 wmax,spoids1,imin,modmin,omet,omet2,yneut,xneut	!sept2006	!r&d14	!fev2007
      character*80 don1
      character*11 nom
c
c variables partagees (taille fixe) ...........................................
      integer*4 ifonct(2),nnsol(10)
      real*8 dis(5),fam(6),bm1(10),bm2(10),bm3(10),bm4(10),
     *	   sf1(10),sf3(10)									!fev2007
	real*8 w1,w2,w3,rho,fk(3)								!multi obj
	integer*4 imulti,iicout,iredress   					    !multi obj
      integer*4 ipareto, nsteppar, iparcout, iparpoids, iparinertia
      integer*4 refthick_input
c
c variables partagees (taille variable) .......................................
      integer*4 neto
      character*3, allocatable, save ::      modes(:)
      double precision, allocatable, save :: blocka(:,:)
      double precision, allocatable, save :: solt(:,:)
      double precision, allocatable, save :: sens1(:,:,:,:)
      double precision, allocatable, save :: sens2(:,:,:,:)
      double precision, allocatable, save :: sens3(:,:,:,:)
      double precision, allocatable, save :: sens4(:,:,:,:)
      double precision, allocatable, save :: a(:,:)
      double precision, allocatable, save :: b(:,:)
      double precision, allocatable, save :: dzsn(:,:,:)
      double precision, allocatable, save :: cha(:,:,:)
      double precision, allocatable, save :: dcha(:,:,:)
      integer, allocatable, save ::          npt(:)
      integer, allocatable, save ::          lm2(:)
      double precision, allocatable, save :: tetas(:)
      double precision, allocatable, save :: tetaq(:)
      double precision, allocatable, save :: delt(:)
      double precision, allocatable, save :: delt2(:)
      double precision, allocatable, save :: z(:,:)
      double precision, allocatable, save :: angle(:)
      integer, allocatable, save ::          noh(:,:)
      integer, allocatable, save ::          noeud(:,:)
      integer, allocatable, save ::          mcomp(:,:)
      integer, allocatable, save ::          nno(:,:)
      integer, allocatable, save ::          nsign(:)
      double precision, allocatable, save :: e(:)
      double precision, allocatable, save :: eta(:)
      double precision, allocatable, save :: sigy(:)
      double precision, allocatable, save :: coefk(:)		!r&d15
      double precision, allocatable, save :: spec(:)
      double precision, allocatable, save :: ploc(:)
      double precision, allocatable, save :: ypts(:,:)
      integer, allocatable, save ::          nvar(:)
      integer, allocatable, save ::          m1cont(:)
      integer, allocatable, save ::          m2cont(:)
      integer, allocatable, save ::          nxit(:,:)
      integer, allocatable, save ::          ipts(:)
      integer, allocatable, save ::          ipts2(:,:)
      integer, allocatable, save ::          ipts3(:,:)
      integer, allocatable, save ::          lcont(:,:)
      integer, allocatable, save ::          lcont4(:,:,:,:)
      integer, allocatable, save ::          nvarr(:)
      integer, allocatable, save ::          nxitr(:,:)
      integer, allocatable, save ::          isemr(:)
      integer, allocatable, save ::          isema(:)
      double precision, allocatable, save :: xicou(:)
      double precision, allocatable, save :: ximin(:)
      double precision, allocatable, save :: ximax(:)
      double precision, allocatable, save :: ximin_dcns(:)
      double precision, allocatable, save :: ximax_dcns(:)
      double precision, allocatable, save :: cjmax(:)
      double precision, allocatable, save :: cjmax3(:,:,:)
      double precision, allocatable, save :: ega(:)
      integer, allocatable, save ::          inv(:)
      integer, allocatable, save ::          inv3(:,:,:)
      integer, allocatable, save ::          mega(:,:)
      integer, allocatable, save ::          nvs(:)
      integer, allocatable, save ::          nxi2(:,:)
      integer, allocatable, save ::          negal(:)
      integer*2, allocatable, save ::        kli(:,:)
      double precision, allocatable, save :: zsn(:,:)
      double precision, allocatable, save :: qn(:)
      double precision, allocatable, save :: philn(:)
      double precision, allocatable, save :: abc(:)
      double precision, allocatable, save :: asol(:)
      double precision, allocatable, save :: bsol(:)
      double precision, allocatable, save :: csol(:)
      double precision, allocatable, save :: dsol(:)
      double precision, allocatable, save :: amom(:)
      double precision, allocatable, save :: bmom(:)
      double precision, allocatable, save :: cmom(:)
      double precision, allocatable, save :: dmom(:)
      double precision, allocatable, save :: part(:)
      double precision, allocatable, save :: abcd(:)
      double precision, allocatable, save :: coef(:,:,:)
      integer, allocatable, save ::          icha(:)
      integer, allocatable, save ::          itype(:)
      integer, allocatable, save ::          isect(:)
      double precision, allocatable, save :: tfa(:)
      double precision, allocatable, save :: tfr(:)
      integer, allocatable, save ::          m1tabl(:,:)
      integer, allocatable, save ::          izut(:)
      double precision, allocatable, save :: zut(:)
      double precision, allocatable, save :: corro(:,:) !corrosion		!aout2006
      double precision, allocatable, save :: corroel(:) !corrosion		!aout2006
c      double precision, allocatable, save :: dw(:)	  !restri poids		!septembre 2006	
	double precision, allocatable, save :: dixx(:)						!septembre 2006	

c variables ushull ............................................................
      double precision, allocatable, save :: dsud(:,:)
      double precision, allocatable, save :: dsub(:,:)
      double precision, allocatable, save :: dsub1(:,:)
      double precision, allocatable, save :: dsusu(:,:)
      double precision, allocatable, save :: dsusl(:,:)
      double precision, allocatable, save :: dh(:,:)
      double precision, allocatable, save :: dg(:,:)
      double precision, allocatable, save :: dad(:,:)
      double precision, allocatable, save :: dab(:,:)
      double precision, allocatable, save :: dab1(:,:)
      double precision, allocatable, save :: das(:,:)
      double precision, allocatable, save :: dult(:)
	integer, allocatable, save ::          id(:)			!dec2006
	integer, allocatable, save ::          ib1(:)			!dec2006
	integer, allocatable, save ::          ib(:)			!dec2006
	integer, allocatable, save ::          is9(:)			!dec2006
c
c variables bo1 ...............................................................
      real*8 coeff(32,41)
      real*8 dcoeff(32,33,9)
      real*8 ali(360)
      real*8 dvara(33,9,16)
      real*8 dvarb(33,9,16)
      real*8 dvarc(33,9,16)
      real*8 dvard(33,9,16)
      real*8 dvarh(33,9,38,10)
      real*8 alix(10,13)
      real*8 aliz(10,13)
      real*8 dalix(10,13,9)
      real*8 daliz(10,13,9)
      real*8 dhyp(20,18,9)
      real*8 dish(1710,10)
      real*8 disa(720)
      real*8 disb(720)
      real*8 disc(720)
      real*8 disd(720)
c
c variables optis .............................................................
      double precision, allocatable, save :: xiopt(:)		!extension neto !fev2007
      double precision, allocatable, save :: fiopt(:)		!extension neto	!fev2007
      double precision, allocatable, save :: cjopt(:)		!extension neto	!fev2007
      integer*4       , allocatable, save :: ic_rest(:)	!extension neto	!fev2007  dad
	double precision, allocatable, save :: ratio_rest(:) !extension neto	!fev2007  dad
      double precision, allocatable, save :: cjmopt(:)	!extension neto	!fev2007
      double precision, allocatable, save :: cijopt(:,:)	!oct06     
	double precision, allocatable, save :: dxopt(:,:)	!extension neto !fev2007
	double precision, allocatable, save :: z1opt(:)		!extension neto !fev2007
	double precision, allocatable, save :: z2opt(:)		!extension neto !fev2007
	double precision, allocatable, save :: z3opt(:)		!extension neto !fev2007
      double precision, allocatable, save :: sopt(:)		!extension neto !fev2007
	double precision, allocatable, save :: fimulti(:)	!multi obj
	double precision, allocatable, save :: derredress(:)!redressage !septembre 2007
c
c variables sensibcout ........................................................
      double precision, allocatable, save :: vlarg(:)
      double precision, allocatable, save :: vepaiss(:)
      double precision, allocatable, save :: vhac(:)
      double precision, allocatable, save :: veac(:)
      double precision, allocatable, save :: vlsc(:)
      double precision, allocatable, save :: vesc(:)
      double precision, allocatable, save :: deltacsc(:)
      double precision, allocatable, save :: vhar(:)
      double precision, allocatable, save :: vear(:)
      double precision, allocatable, save :: vlsr(:)
      double precision, allocatable, save :: vesr(:)
      double precision, allocatable, save :: deltar(:)
      double precision, allocatable, save :: entrsc(:)
      double precision, allocatable, save :: philsc(:)
      double precision, allocatable, save :: qsc(:)
      double precision, allocatable, save :: coutpan(:)
      double precision, allocatable, save :: costpan(:)
      double precision, allocatable, save :: coutmat(:)
      double precision, allocatable, save :: dcorsc(:)
      double precision, allocatable, save :: dcorsc2(:)
      double precision, allocatable, save :: dcorsc3(:)
c      double precision, allocatable, save :: dfctsc(:)	!janv2007
      double precision, allocatable, save :: vdiffc(:)
c

c	variables coord ........................................................
	real*8 aiyytot,aixxtot,aiuutot,aiyytot2,aixxtot2,aiuutot2				!sept2006
	real*8 yneutpart,aixxtotpart											!r&d13	!fev2007


c	variables inertia.......................................................
	real*8 yneutnet,yneutgro,inet,igro							!r&d14	!fev2007
	real*8 yneutplnet,yneutplgro,iplnet,iplgro					!r&d14	!fev2007
	double precision, allocatable, save :: dyneutnet(:,:)		!r&d14	!fev2007
	double precision, allocatable, save :: dyneutgro(:,:)		!r&d14	!fev2007
	double precision, allocatable, save :: dinet(:,:)			!r&d14	!fev2007
	double precision, allocatable, save :: digro(:,:)			!r&d14	!fev2007
	double precision, allocatable, save :: dyneutplnet(:,:)		!r&d14	!fev2007
	double precision, allocatable, save :: dyneutplgro(:,:)		!r&d14	!fev2007
	double precision, allocatable, save :: diplnet(:,:)			!r&d14	!fev2007
	double precision, allocatable, save :: diplgro(:,:)			!r&d14	!fev2007


c	variables bending
	real*8 signet(2),siggro(2)									!r&d14	!fev2007
	double precision, allocatable, save :: dsignet(:,:,:)		!r&d14	!fev2007
	double precision, allocatable, save :: dsiggro(:,:,:)		!r&d14	!fev2007


c	variables stifbend
	real*8 sigstif			!r&d14	!fev2007
	real*8 dsigstif(9)		!r&d14	!fev2007


c	variables shear.......................................................
	double precision, allocatable, save :: taunet(:,:)		!r&d14	!fev2007
	double precision, allocatable, save :: taugro(:,:)		!r&d14	!fev2007
	double precision, allocatable, save :: dtaunet(:,:,:)	!r&d14	!fev2007
	double precision, allocatable, save :: dtaugro(:,:,:)	!r&d14	!fev2007

c	variables buckdcn.....................................................
	real*8 dbuckpl_07,    dbuckpl_08						  !dcn07
	double precision, allocatable, save :: ddbuckpl_07(:,:),  !dcn07
     *                                       ddbuckpl_08(:,:)   !dcn07
	real*8 dbuckstif_07,dbuckstif_08     					  !dcn07
	double precision, allocatable, save :: ddbuckstif_07(:,:),!dcn07
     *                                       ddbuckstif_08(:,:) !dcn07
	real*8 inpart_07,inpart_08							      !dcn07
	double precision, allocatable, save :: dinpart_07(:,:),   !dcn07
     *                                       dinpart_08(:,:)    !dcn07
      

c ancien common dim1 ..........................................................
      integer*4 nmax,nemax,nvmax,m1max,m1tmax,m2max,m2tmax,mmmax,ngmax   ! dimension max des vecteurs
      integer*4 ismax,iptmax                                             ! dimension max des vecteurs
c
c ancien common opti ..........................................................
      integer*4 iopti,ntot,m1tot,m2tot
c
c ancien common opti2 .........................................................
      real*8 defa(13,4),defb(13,4),defc(13,4),defd(13,4)
      real*8 defaa(13,4),defbb(13,4),defcc(13,4),defdd(13,4)
      real*8 sensh(13,4,9,10)
      real*8 sensa(13,4,9),sensb(13,4,9),sensc(13,4,9),sensd(13,4,9)
      real*8 sensaa(13,4,9),sensbb(13,4,9),senscc(13,4,9),sensdd(13,4,9)
c
c ancien common opti3 .........................................................
      real*8 const2(6,9),const3(2,10),darg(8,9)
c
c ancien common grav ..........................................................
      real*8 ykg,xkgmin,xkgmax,xk,yk
      integer*4 igrav
c
c ancien common py ............................................................
      real*8 pi,sinu(4),cosi(4),expt(4)          ! bidon6 sert pour sin(4),cos(4) et exp(4)
      real*8 lam(8)
c
c ancien common hyp ...........................................................
      real*8 hyp(360)               ! bo1,sens,bo2 (ok pour 10 traverses et 10 cas de charges
c
c ancien common noeud .........................................................
      integer*4 ntn
c
c ancien common ent ...........................................................
      real*8 abtr(10),argq(8),const(74),hxtr(10),dxtr(10)      ! dans geom,bo1,loc,locy,bo2,mdr2
      real*8 wxtr(10),txtr(10),itype_tr(10)
	character*80 name_type_tr(10)

c
c ancien common ph ............................................................
      real*8 phmax                 ! equil (only), taille 1
c
c ancien common graf ..........................................................
      real*8 bidon9(20)             ! save (20 = nbre de dessins différents)
c
c ancien common ecr ..........................................................
      real*8 uecr(51)               ! on remplace u par uecr et mt par mtecr pour ne pas confondre
      integer*4 mtecr               ! bo2,ecr,ecri2,ecri3 (51= 31 + 2* mt(=10))
c
c ancien common str ..........................................................
      real*8 syd,syb,syb1,sysu,sysl,sud,sub,sub1,susu,susl
      real*8 ad,ab1,ab,as
c
c ancien common sans nom ......................................................
      real*8 bidonb(400+360)        ! bateau+system (ok avec 740),
                                    ! bo1,edge,inf3,mdr,mdr2,rang,rang2 (ok pour 10 traverses et 10 cas de charges)
c
c ancien common cout ..........................................................
      real*8 poids,spoids,fmat,fsou,fmdo,fmat11,fmat22,fmat33,
     *fsou11,fsou22
      real*8 fsou33,fmdo11,fmdo22,fmdo33,fmdo44,fmdo55,fmdo66,fmdo77
      real*8 rend_global,eqp
	
	real*8 dref,  drefx, drefy
	real*8 dref_b,dref_c,dref_l

	real*8 c1,c2,c3,dc1
	real*8 c_pb,c_tc,c_tl

	real*8 dw2,dw3
	real*8 dw_b,dw_c,dw_l

	real*8 p10,dp10
      real*8 p4,p5,dp4,dp5,p9x,p9y,dp9x,dp9y,ber,bet,p6,p7,c8,dc8,cout

      integer*4 icout,ialr,ialt

	integer*4,    allocatable, save :: itype_transv(:),itype_longit(:)
     *,                                  itype_transv_second(:)
     *,                                  itype_longit_second(:)
      character*80, allocatable, save :: name_type_transv(:),
     *                                   name_type_transv_second(:)
	character*80, allocatable, save :: name_type_longit(:),
     *                                   name_type_longit_second(:)

	double precision, allocatable, save :: rend_panel(:)

c
c ancien common ushull ........................................................
      real*8 depth,db,uhogm,usagm
      real*8 sysd,sysb1,sysb,syssup,syssdw
      integer*4 iult,irestr
      integer*4 nd,nb1,nb,ns
      integer*4 kd,kb1,kb,ksup,ksdw
c
c ancien common dede ..........................................................
      data mdp/5, 1, 1, 1,1, 1, 9 , 5, 3, 3, 1, 1, 0, 0, 0,
     *         7, 5, 5, 3,3, 3, 11, 9, 5, 5, 7, 9, 0, 0, 0,
     *         9, 7, 9, 5,5, 13,13,11, 7,11, 9,13, 0, 0, 0,
     *         11,9, 13,7,13,15,15,13,11,13,15,15, 0, 0, 0/,
     *     mar/6, 2, 2, 2,2, 2, 10, 6, 4, 4, 2, 2, 0, 0, 0,
     *         8, 6, 6, 4,4, 4, 12,10, 6, 6, 8,10, 0, 0, 0,
     *         10,8, 10,6,6, 14,14,12, 8,12,10,14, 0, 0, 0,
     *         12,10,14,8,14,16,16,14,12,14,16,16, 0, 0, 0/
c
c ancien common nom ...........................................................
      data nom/'fichdessin\'/
      end