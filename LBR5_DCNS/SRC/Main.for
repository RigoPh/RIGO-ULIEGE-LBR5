      subroutine main(code1)					!sécurité
      use sharedvar
c	use dfport
      implicit double precision (a-h,o-z)
      dimension atext(15)
      dimension nxi(9),al(1)      ! parce qu'on envoie qu'une partie de nxit a ents
      integer*2 dessin
	real*8 length							!r&d15
      character*80 res1,res2,res3,res4,res5,res6,res7	!r&d14	!fev2007 !dad
      character*80 texte,title,version
	character*1 at(84300)
	character*30 com			!sept2006  !dad
	dimension dw(9*nmax),sxm(iptmax+2*neto+11)	!restri poids	!sept 2006	
							!(ne pas confondre avec dw = hauteur de l'âme (web)
	dimension dprice(9*nmax)	!restri cout	!fev 2007
	dimension thicknet(neto),thickgro(neto)		!r&d14	!fev2007
	dimension dciacs(9*nmax)
	character*16 code1							!sécurité
c	integer cpt									!multi obj
	logical security_activated
	double precision txr_dcns(nmax)
	double precision tya_dcns(nmax)

c
c affichage du nom du fichier de donnees ......................................
      i=index(don1,' ')-1
      write(6,*)
      write(6970,*)
      if(langue.eq.1) then
	   write(6,100)'fichier de donnees: '//don1(1:i)
	   write(6970,100)'fichier de donnees: '//don1(1:i)
	endif
      if(langue.eq.2)then   
	   write(6,100)'data file         : '//don1(1:i)
	   write(6970,100)'data file         : '//don1(1:i)
	endif
      if(langue.eq.1)then
	   write(6,100)'nbe de panneaux   :',neto
	   write(6970,100)'nbe de panneaux   :',neto
	endif
      if(langue.eq.2)then
	   write(6,100)'pannel number     :',neto
	   write(6970,100)'pannel number     :',neto
	endif
c
c nom des fichiers resultats ..................................................
      res1='sol-'//don1
      res2='opt-'//don1
      res3='sol2-'//don1
	res4='bug-'//don1
	res5='sol3-'//don1		!r&d14	!fev2007
	res6='sol-red-'//don1		!dad
      res7='sol-deri-'//don1		!dad
      i=index(res2,' ')-1
      if(langue.eq.1)then
	   write(6,100)'fichier solution  : '//res2(1:i)
	   write(6970,100)'fichier solution  : '//res2(1:i)
      endif
      if(langue.eq.2)then
	   write(6,100)'solution file     : '//res2(1:i)
	   write(6970,100)'solution file     : '//res2(1:i)
      endif
c
c ouverture des fichiers ......................................................

!     open(6969,file='303.dat',form='formatted')           ! fichier debug
!     open(6868,file='nan.dat',form='formatted')           ! fichier debug


      open(2218,status='scratch',form='formatted')      ! lulu (fichier scatch qui n'etait pas defini)
      open(42,status='scratch',form='unformatted')      ! pour le dessin (subr ent,vision
      open(43,status='scratch',form='unformatted')      ! u,v,w, ..(subr. save et vision)
      open(45,status='scratch',form='unformatted')      ! pour le dessin (subr ent,vision
      open(46,status='scratch',form='unformatted')      ! pour le dessin (subr ent,copy,visionn
                                                        ! données courantes pour subr.visionn (dessin)
      open(55,file=don1,status='old',err=903)           ! fichier d'entree
c     open(57,file='atableur.txt',status='old',err=904) ! données coût
      open(66,file=res1)                                ! sortie listing (sortie standard)
      open(67,file=res3)                                ! sortie listing (recapitulatif des resultats)
      open(94,status='scratch',form='unformatted')      ! subr participation:forces de bord (abcd)
                                                        ! coeff. de participation
      open(97,status='scratch',form='unformatted')      ! subr bateau,bo1:forces de bord (abcd),
      open(98,status='scratch',form='unformatted')      ! subr. ent,bo1 (charge de type cha)
                                                        ! sauvetage pour itera=1 seulement
      open(99,status='scratch',form='unformatted')      ! subr. ent,bo1,bo2
c      open(200,status='scratch',form='formatted')       ! données pour conlin
      open(301,status='scratch',form='formatted')  ! données pour conlin		!extension neto	!fev2007
!     open(301,file='301.dat',form='formatted')  ! données pour conlin		!extension neto	!fev2007
      open(302,status='scratch',form='unformatted')! données fct objct		!extension neto	!fev2007
      open(303,status='scratch',form='unformatted')! subr. contraintes		!extension neto	!fev2007
      open(304,status='scratch',form='formatted')  ! valeurs extrêmes			!extension neto	!fev2007

      open(305,file='alstom.txt')   ! données pour subr "cost-alstom", mars 2003		!extension neto	!fev2007
	open(306,status='scratch',form='unformatted') !données pour bo2,février 2004	!fev2007

	open(29,file=res4)				!bug  29=année du crash de wall street
		
      open(93,status='scratch',form='unformatted')     ! subr pepite:redressage  !redressage !septembre 2007
      open(7777,status='scratch',form='unformatted')   ! subr pepite:redressage  !redressage !septembre 2007
      open(7778,file =res6)                            ! subr pepite:redressage  !redressage !septembre 2007
      open(7779,file =res7)                            ! subr pepite:redressage  !redressage !septembre 2007


c vérification des signatures gui et solveur
	security_activated= .false.
	!call security(code1,security_activated)					!sécurité
      if(security_activated) then
         write(*,*) 'security activated '
	else
         write(*,*) 'security not activated '
	endif


c lecture des donnees (unite 55) ..............................................
      read(55,4)atext
	write(66,4)atext		!juil07
	write(66,*)				!juil07
      write(67,4)atext		!juil07
	write(67,*)				!juil07

	read(55,*) iana					!r&d14	!=1:lbr-4 structural analysis	!fev2007
											!=2:beam structural analysis	!fev2007

      read(55,4)atext
      write(66,3)atext		!juil07
	write(67,3)atext		!juil07

	write(66,*) 
      write(66,*) 'noms des fichiers /file names:'
      write(66,*) ' input - donnees    :', don1
      write(66,*) ' output-resultat #1 :', res1
      write(66,*) '                 #3 :', res3
      write(66,*)

c ecriture de l'entete du fichier sol2- (unite 67) ............................
c      call entete(67)
c	call entete(66)							!nov2006

      read(55,*)impr,impr2,indaig,indrai,dessin,jlph2,ibusc,neto
      if(neto.eq.1)then                                                                                    !flexion d'axe vert.
       if(langue.eq.1)then
        write(6,100)' structure a un seul panneau ... sortie.'
        write(6970,100)' structure a un seul panneau ... sortie.'
       else
        write(6,100)' single panel structure ... aborting.'
        write(6970,100)' single panel structure ... aborting.'
       endif
      endif 

	if(langue==1)then										!nov2006
	 write(66,10) neto  ! french							!nov2006
	 write(* ,10) neto										!nov2006
	 write(6970 ,10) neto										!nov2006
	else													!nov2006
       write(66, 6) neto  ! english							!nov2006
       write(* , 6) neto  ! english							!nov2006
	write(6970 ,10) neto
	endif  													!nov2006
	                                                                                                             !flexion d'axe vert.
c      if(langue.eq.1)write(67,10)neto
      if(langue.eq.2)write(67,6) neto

c jlph2 = nbre de termes impair, avec son signe > ou <
c jlph  = nbre de termes total (pair et impair)
      jlph=2*iabs(jlph2)-1
      iprint=0
	read(55,*) imulti,rho,w1,w2,w3,fk(1),fk(2),fk(3)		!multi obj	!fev2007
      read(55,*) ipareto, nsteppar, iparcout, iparpoids, iparinertia

      read(55,*)iopti,iteram
      if(iopti.ne.1)iteram=99
      if(iopti.ge.1)then
       open(666,file=res2)                             ! output file for the optimization process opt-nom.*
       open(78,status='scratch',form='unformatted')    ! save list of struct constraint in ent to be re-used in contr
       write(666,  3)atext
       write(666,511)  ! variables de conception et restrictions geometriques
c       if(impr2.eq.-3) impr2=-2			!sept2006
       if(iopti.ge.3 ) iprint=1
	if(langue==1) write(66,'(t22,a)')'avec optimisation'		!nov2006				
        if(langue==2) write(66,'(t22,a)')'with optimisation'		!nov2006
	  write (66,400)											!nov2006
	  write(66,*)'                     (iteration nø 1)'		!nov2006
	  write(66,*)'                     (max=',iteram,')'		!nov2006
      else														!nov2006
        if(langue==1) write(66,'(t22,a)')'sans optimisation'		!nov2006
        if(langue==2) write(66,'(t22,a)')'without optimisation'	!nov2006
	  write (66,400)											!nov2006
      endif 

	if(langue==1) then
        write (66, 11) jlph2,ibusc
        if(impr  .ne.0) write(66,7)
        if(indaig.eq.0) write(66,9)
        if(indrai.eq.0) 
     *     write(66,'(/a)')' pas de resultats dans les raidisseurs'
        if(impr2 .eq.1) write(66,8)
	else
        write (66, 14) jlph2,ibusc
        if(impr  .ne.0) write(66,'(/a)')' with intermediate results'
        if(indaig.eq.0) 
     *     write(66,'(/a)')' results in the frames are not printed'
        if(indrai.eq.0) 
     *     write(66,'(/a)')' results in the stiffeners are not printed'
        if(impr2 .eq.1) write(66,*)' final results + equilibrium checks'
	endif

c lecture des couts, rendement, ...
      read(55,*) icout             ! icout=0 -> fct obj. poids ; icout =1 -> fct obj. coût
      read(55,*) iredress                                 !dad   (14.12.2007)

	write(666,*)'neto',neto              				!eugen (17.10.2007)
	write(666,*)'iana',iana              				!eugen (17.10.2007)
	write(666,*)'icout',icout							!eugen (11.10.2007)
	write(666,*)'iredress',iredress			  		    !dad   (14.12.2007)
	write(666,*)'iteram',iteram							!eugen (11.10.2007)
	write(666,*)'imulti',imulti							!eugen (11.10.2007)
	write(666,'(a,f9.4,f9.4,f9.4/)')'weights',w1,w2,w3	!eugen (12.10.2007)   



      read(55,*) rend_global,eqp   ! rendement, equivalent poids de la mdo (=k)
      read(55,*) refthick_input    ! reference thickness input
      read(55,*) dref,drefx,drefy  ! epaisseurs de référence
      read(55,*) dref_b,dref_c,dref_l 
	                             ! epaisseurs de référence 
								 !                (Bulbe, Composite, Laminé)
      read(55,*) c1,c2,c3,dc1      ! cout des matériaux
      read(55,*) c_pb,c_tc,c_tl    !(tole.Euro/kg). (Bulbe, Composite, Laminé)
      read(55,*) dw2,dw3           ! extra weight
      read(55,*) dw_b,dw_c,dw_l    ! extra weight   (Bulbe, Composite, Laminé)


      read(55,*) p10,dp10          ! mdo bordé
      read(55,*) p4,p5,dp4,dp5     ! mdo assembl. membr-bordé
      read(55,*) p9x,p9y,dp9x,dp9y ! mdo constr. membr.
      read(55,*) p6,p7,ber,bet     ! mdo intersect et goussets
      read(55,*) c8,dc8,ialr,ialt  ! coût soudure (energ + consom.
c lecture des parametres generaux width, dis, fam, ipoids, nsol
      read(55,*) width,length      ! 8-5-96	!r&d15
      read(55,*) (dis(i),i=1,5)    ! 8-5-96
      read(55,*) (fam(i),i=1,6)    ! 8-5-96
      fam(5)=0.
      fam(6)=0.
      read(55,*) ipoids

	if(langue==1) then ! francais											!nov2006
	  write(66,'(/a,f9.4,a)')' longueur de la structure =',width,' m'		!nov2006
	  write(66,12)(dis(i),i=1,5)											!nov2006
        write(66,13)(fam(i),i=1,6)											!nov2006
        write(66,*)' '														!nov2006
        if(ipoids.eq.1) then													!nov2006
          write(66,*)' on tient compte du poids propre des panneaux',			!nov2006
     *               ' (ipoids =1)'											!nov2006
        else																	!nov2006
          write(66,*)' on ne tient pas compte du poids propre des',			!nov2006
     *               ' panneaux (ipoids =0)'									!nov2006
        endif																	!nov2006
        write(66,'(1x,51(1h-)/)')												!nov2006
	else  ! anglais															!nov2006
	  write(66,'(/a,f9.4,a)')' length of the structure =',width,' m'		!nov2006
	  write(66,'(/2a)')' locations (x) of 5 sections where displ.',			!nov2006
     *                   ' and stress are given (in meters)'					!nov2006
	  write(66,'(t10,5(2x,f9.4))')(dis(i),i=1,5)							!nov2006
        write(66,13)(fam(i),i=1,6)											!nov2006
        write(66,*)' '														!nov2006
        if(ipoids.eq.1) then													!nov2006
          write(66,*)'deadweight of the structure is considered',				!nov2006
     *               ' (ipoids =1)'											!nov2006
        else																	!nov2006
          write(66,*)'deadweight of the structure is not considered',			!nov2006
     *               ' (ipoids =0)'											!nov2006
        endif																	!nov2006
        write(66,'(1x,46(1h-)/)')												!nov2006
	endif																	!nov2006

c lecture des sollicitations
c  nsolm = nbre de cas de sollicitations defini dans les données
c  nsol  = nbre de cas de sollicitations qui seront étudiés
c  nnsol = liste des numeros des nsol cas étudies.
      read(55,*) nsolm, nsol,(nnsol(i),i=1,nsol)
      if(langue==1) write(66,419) nsolm, nsol,(nnsol(i),i=1,nsol) 			!nov2006 
      if(langue==2) write(66,420) nsolm, nsol,(nnsol(i),i=1,nsol)				!nov2006
	do i=1,nsolm
       read(55,*)texte
      enddo
      if((nsol.lt.1).or.(nsol.gt.ismax))goto 905
c calcul du parametre ne (nbre d'inconnues hyperstatiques de la structure)
      ne=neto*8
c verification de la taille du vecteur al càd dzsn(ne,9*neto,nsol)<7200000
      imax=72*ismax*nmax
      if(imax.gt.7200000)then
        write(*,*) ' le nbre de panneau est trop élevé pour nsol=',nsol
	  write(6970,*)
     *' le nbre de panneau est trop élevé pour nsol=',nsol
	  write(66,*)' le nbre de panneau est trop élevé pour nsol=',nsol
        write(29,*)'le nbre de sollicitation est incorrect (max.= 10)'			!sept2006
	  pause'stop'
	  stop
      endif
c
c
c 8.0 depart d'une nouvelle iteration (optimisation via opti/conlin) ==========
c==============================================================================
      itera=0																				   !septembre 2006
	write(29,*)'iteration 0'		!13.12.05								!bug           !septembre 2006
	write(29,*)'<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<                        !septembre 2006
     *<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<'                                   !septembre 2006
	write(29,*)' '															!bug           !septembre 2006
	iter1= iteram+1	 !13.12.05 pour avoir analyse structurelle du dernier échantillonage   !septembre 2006

c  	if(imulti.eq.1) then			!multi obj
c	  call annuld(fk,3)				!multi obj
c	  if(w1.ne.0) then				!multi obj
c	    iicout=icout				!multi obj
c		cpt=1						!multi obj
c	  elseif(w2.ne.0) then			!multi obj
c	    iicout=0					!multi obj
c		cpt=2						!multi obj
c	  elseif(w3.ne.0) then			!multi obj
c	    iicout=-1					!multi obj
c		cpt=3						!multi obj
c	  endif							!multi obj
c	else							!multi obj
c	  iicout=-5						!multi obj
c	endif							!multi obj
	
  303 continue
     
c      if((imulti.eq.1).and.(itera.gt.iter1)) then		!multi obj
c	  if(cpt.eq.1) then								!multi obj
c	    if(w2.ne.0) then							!multi obj
c		  iicout=0									!multi obj
c		elseif(w3.ne.0) then						!multi obj
c		  iicout=-1									!multi obj
c		  cpt=2										!multi obj
c	    endif										!multi obj
c	  elseif(cpt.eq.2) then							!multi obj
c	    if(w3.ne.0) then							!multi obj
c		  iicout=-1									!multi obj
c		else										!multi obj
c	      cpt=3										!multi obj
c	      iicout=-2									!multi obj
c	    endif										!multi obj
c	  elseif(cpt.eq.3) then							!multi obj
c		iicout=-2									!multi obj
c	  elseif(cpt.eq.4) then							!multi obj
c		goto 304									!multi obj
c	  endif											!multi obj
c	  cpt=cpt+1										!multi obj
c	  itera=0										!multi obj
c        do  nel=1,neto								!multi obj
c          close(100+nel)								!multi obj
c          if(iopti.ge.1) close(400+nel)				!multi obj		
c          close(700+nel)								!multi obj
c        enddo										!multi obj
c	endif											!multi obj
	  
			
c      if((imulti.ne.1).and.(itera.gt.iter1)) goto 304						!multi obj	!septembre2006

	if(itera.gt.iter1) goto 304												!multi obj
																		
	if(itera.gt.0)then														!septembre2006
	write(29,*)'<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<			!septembre2006
     *<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<'					!septembre2006
	  write(29,*)'iteration ',itera								!bug		!septembre2006
	  write(29,*)'<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<		!septembre2006
     *<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<'					!septembre2006
	  write(29,*)' '											!bug		!septembre2006
      endif																	!septembre2006

      if(langue==1) write(67,10) neto											!septembre2006
      if(langue==2) write(67, 6) neto											!septembre2006

	if (impr2.eq.1) then		!+++31.01.06								!septembre2006
		if (itera.lt.iter1)then  !juillet04		!+++31.01.06 !13.12.05		!septembre2006										       !septembre 2006
	    write(66,418)itera              !juillet04							!septembre2006
	    write(67,418)itera              !juillet04							!septembre2006
		elseif (itera.eq.iter1) then				    !13.12.05			!septembre2006
	    write(66,418)iteram     !juil07			!13.12.05				!septembre2006
	    write(67,418)iteram     !juil07			!13.12.05				!septembre2006
		endif            !  analyse structurelle finale  !juillet04			!septembre2006
	
c	  if(itera.gt.1)then													!septembre2006
c	  if(impr2.eq.1) then													!septembre2006
c		write(66,418)itera													!septembre2006
	  else
	  	close(66)
 	  	close(67)
            open(66,file=res1)                                ! sortie listing (sortie standard)
            open(67,file=res3)                                ! sortie listing (recapitulatif des resultats)
          write(66,3) atext       !sept.2006
          write(67,3) atext       !sept.2006 
          if(itera.eq.iter1) then												!septembre2006
		write(66,418) iteram		!juil07			!13.12.05			!septembre2006
		write(67,418) iteram		!juil07			!13.12.05			!septembre2006			
		else																!septembre2006
          write(66,418)itera
          write(67,418)itera
          endif			!analyse structurelle finale
          write(66,10) neto
          write(66,'(t26,a)')'avec optimisation'
          write(66,'(t26,a,i3,a)')'(iter. max=',iteram,')'
          write(66,400)
          write(66,11) jlph2,ibusc
          if(impr.ne.0)write(66,7)
          if(indaig.eq.0)write(66,9)
          if(impr2.eq.1)write(66,8)
          write(66,12)(dis(i),i=1,5)
          write(66,*)
          if(ipoids.eq.1) then
            write(29,*)' on tient compte du poids propre des panneaux',		!septembre2006
     *                 ' (ipoids =1)'
          else
            write(66,*)' on ne tient pas compte du poids propre des',
     *                 ' panneaux (ipoids =0)'
            write(29,*)' on ne tient pas compte du poids propre des',			!septembre2006
     *                 ' panneaux (ipoids =0)'
          endif
          write(66,'(1x,51(1h-)/)')
          write(66,419) nsolm, nsol,(nnsol(i),i=1,nsol)
        endif
	if(icout.lt.2) then							!26.05.04					!septembre2006
        write(66,*)'le cout est calculé par un modèle simplifié '			!26.05.04
        write(67,*)'le cout est calculé par un modèle simplifié '			!26.05.04
	  write(29,*)'le cout est calculé par un modèle simplifié '		!bug	!septembre2006
        if(iopti.ge.1) then
	    write(666,*)'le cout est calculé par un modèle simplifié '		!26.05.04
	  endif
	else															!26.05.04
        write(66,*)'le cout est calculé par un modèle détaillé cost cat'	!26.05.04
        write(67,*)'le cout est calculé par un modèle détaillé cost cat'	!26.05.04
	  write(29,*)'le cout est calculé par un modèle détaillé cost cat'	!bug	!septembre2006
        if(iopti.ge.1) then
	    write(666,*)'le cout est calculé par modèle détaillé cost cat'	!26.05.04
	  endif
	endif															!26.05.04

c      if(langue==1) write(67,10) neto										!septembre2006
c      if(langue==2) write(67, 6) neto										!septembre2006
c      write(67,418)itera														!septembre2006
c
c 8.2 impression des parametres de couts ======================================
c==============================================================================
      if(iopti.ge.1)then
		if(imulti.eq.1)then							!10.10.2007 (eugen)
			write(666,429)	
		else	
			if(icout.eq.(-1)) then												!septembre2006
c				write(*  ,428)														!septembre2006											
				write(67 ,428)														!septembre2006
				write(666,428)														!septembre2006
			endif																!septembre2006	   
			if(icout.eq.0)then
				write(67 ,421)
				write(666,421)
			endif
			if(icout.ge.1)then				!juil07
				write(67 ,422)
				write(666,422)
			endif
		endif
      endif
	if(impr2.ge.0) then								!15.10.05
      write(67,423) rend_global,eqp,dref,drefx,drefy,c1,c2,c3,dc1,
     *sdw2,dw3
      write(67,424) p10,dp10,p4,dp4,p5,dp5
      write(67,425) p9x,p9y,dp9x,dp9y
      write(67,426) p6,p7,ber,bet
      write(67,427) c8,dc8,ialr,ialt
	endif
c
c 8.3 initialisation de ifonct ================================================
c==============================================================================
      ifonct(1)=1      ! ifonct(terme impair)=1 pour termes 1,3,7,..
      ifonct(2)=0      ! ifonct(terme pair  )=0 pour termes 2,4,6,...
                       ! si cas de charges symétriques selon x
                       !  ifonct(terme impair)=1 pour termes 1,3,7,..
                       !  ifonct(terme pair  )=0 pour termes 2,4,6,...
                       ! si cas de charges non symétriques selon x
                       !  ifonct =1 pour tous les termes.
c
c 9.0 lecture des donnees relatives a chaque panneau et calcul des ============
c     grandeurs caracteristiques   (subr. ent) ================================
c==============================================================================
      iff  =0        ! iff=1 si error dans ent
      ichag=0        ! icha  indice de charges variables selon ox
      pot  =0.
      ntot =0
      m1tot=0
      m2tot=0
	spoids1 = 0			!05.12.05					            !septembre2006
      call annuli(m1tabl,nmax*ismax)								!dec 2006

	call annuld(fmat11,1)	!material from plating				!nov 2006	
	call annuld(fmat22,1)	!material from long stiff.			!nov 2006
	call annuld(fmat33,1)	!material from trans. frame			!nov 2006
	call annuld(fmat,1)		!material (total)					!nov 2006
	call annuld(fsou11,1)	!consomables :stiff					!nov 2006
	call annuld(fsou22,1)	!consomables :frame					!nov 2006
	call annuld(fsou,1)		!consomables (total)				!nov 2006
	call annuld(fmdo11,1)	!labor cost for:stiff. on plate		!nov 2006
	call annuld(fmdo22,1)	!labor cost for:stiff buiding		!nov 2006
	call annuld(fmdo33,1)	!labor cost for:frames. on plate	!nov 2006
	call annuld(fmdo44,1)	!labor cost for:frames. building	!nov 2006
	call annuld(fmdo55,1)	!labor cost for:slot cutting		!nov 2006
	call annuld(fmdo66,1)	!labor cost for:stiff.bracket,tap	!nov 2006
      call annuld(fmdo77,1)	!labor cost for:plating building 	!nov 2006  
	call annuld(fmdo,1)		!labor cost (total)					!nov 2006
               
      if(langue.eq.1)then
	   write(6,100)'lecture des donnees (sr ent)'
	   write(6970,100)'lecture des donnees (sr ent)'
      endif
      if(langue.eq.2) then
   	   write(6,100)'reading data (sr ent)'
   	   write(6970,100)'reading data (sr ent)'
      endif
      
	do 2242 nel=1,neto

        m1cont(nel)=0
        call ents(nel,width,ibusc,dessin,pot,ipoids,itera,iprint,
     *            nvar(nel),nsolm,nsol,iff,nxit(1,nel),length,
     *            txr_dcns,tya_dcns)	!r&d15
        if(itera.eq.0) then
          open(100+nel,status='scratch',form='unformatted')
          open(700+nel,status='scratch',form='unformatted') ! pour sauver zsn dans bo1
          if(iopti.ge.1) then
          open(400+nel,status='scratch',form='unformatted')	!extension neto	!fev2007
          endif
        endif
        if(icha(nel).eq.1) ichag=1   ! icha indice de charge variable selon ox
 2242 continue
      if(iff.eq.1)  then
	  write(*,*) 'some errors in your data'
	  write(6970,*) 'some errors in your data'
	  write(29,*) 'subroutine main'												!bug
	  write(29,*) 'some errors in your data'									!bug
	  pause 'stop'
	  stop   
	endif 

      rewind(302)				  ! extension neto	!fev2007
	rewind(99)				  !r&d14	!fev2007
      if(iopti.ge.1)rewind(78)  ! fichier avec liste des restriction cjmax, lcont, inv
      pot=pot*width+spoids1				!05.12.05		!septembre2006
      if(langue==1) write(66,405)pot
      if(langue==1) write(67,405)pot		!05.12.05		!septembre2006

      if(langue==2) write(66,'(/2a,e11.4,2a/)')'total weight of the ',
     * 'structure = ',pot,' n (if ipa=1, reduced specific weight is ',
     * 'considered due to buoyancy force)'

c 10.0 transformations des donnees (subr. modif et coord) =====================
c =============================================================================
c     subr coord: restictions sur centre de gravité,
      if(langue.eq.1)then
	   write(6,100)'transformation des donnees (sr modif)'
	   write(6970,100)'transformation des donnees (sr modif)'
	endif
      if(langue.eq.2)then
	   write(6,100)'data transformation (sr modif)'
	   write(6970,100)'data transformation (sr modif)'
	endif
      call modif(nc,ncondi)       !flexion d'axe vert.
      img=0  ! compteur des restr. struct (img= img+2 si 2 rest. sur centre de gravité)
      if(langue.eq.1)then
	   write(6,100)'coordonnees des panneaux (sr coord)'
	   write(6970,100)'coordonnees des panneaux (sr coord)'
	endif
      if(langue.eq.2)then
	   write(6,100)'panel coordinates (sr coord)'
	   write(6970,100)'panel coordinates (sr coord)'
	endif

      call coord(impr,img,ncondi)!,isymx,isymy)sept06	!r&d14	!fev2007

      rewind(42)
      rewind(45)


c	restriction sur poids		!restri poids							!sept 2006
c	=====================												!sept 2006
																		!sept 2006
	read(55,4) atext													!sept 2006		
      read(55,*) iweight,wmax												!sept 2006	
																		!sept 2006
	if((iopti.ne.0).and.(iweight.ne.0)) then							!sept 2006
	  weight=0.															!sept 2006
	  call annuld(dw,ntot)												!sept 2006
	  ncont=0															!sept 2006
	  do nel=1,neto														!sept 2006
          nbrxi=nvar(nel)													!sept 2006
          call objpd1(nel,ntot,ncont,nxit(1,nel),nbrxi,weight,dw,			!sept 2006
     *		width,0,spec(nel),itype(nel),isect(nel))
     
c          call objpd1(nel,ntot,ncont,nxit(1,nel),nbrxi,obj,	!septembre 2006
c    *              fiopt,width,
c    *              1,spec(nel),itype(nel),isect(nel))	!septembre 2006		!février 2004
     
     		!sept 2006	
          ncont=ncont+nbrxi												!sept 2006
	  enddo																!sept 2006
	  write(66,'(//a/1x,22(1h*))')										!sept 2006
     *    ' restriction sur poids'										!sept 2006
	  write(666,'(//a/1x,22(1h*))')										!sept 2006
     *	' restriction sur poids'										!sept 2006
	  write(66,'(a,t17,e14.7,t33,a)') ' poids actuel = ',weight,'n'		!sept 2006
	  write(666,'(a,t17,e14.7,t33,a)') ' poids actuel = ',weight,'n'	!sept 2006
	  write(66,'(a,t17,e14.7,t33,a)') ' poids maximum = ',wmax,'n'		!sept 2006
	  write(666,'(a,t17,e14.7,t33,a)') ' poids maximum = ',wmax,'n'		!sept 2006
        com='poids < wmax'												!sept 2006
	  c=weight															!sept 2006
	  cm=wmax															!sept 2006
	  write(303) c,cm,com												!extension neto	!fev2007
	  write(303) (dw(i),i=1,ntot)										!extension neto	!fev2007
!	  write(6969,*) c,cm,com												!extension neto	!fev2007
!	  write(6969,*) (dw(i),i=1,ntot)										!extension neto	!fev2007

c	    do ii=1,ntot
c		open(666666, file = 'dw-main456-dyn.txt')
c		write (666666,*) dw(ii)
c		enddo
c		pause										!sept 2006
	  
	  img=img+1				! +1 au compteur des restrictions struc		!sept 2006
        if(iopti.gt.2) then												!sept 2006
		if(impr2.ne.-3) then											!sept 2006	
	    write(666,*) 													!sept 2006
	    write(666,*) 'c,cm,text=',c,cm,'  ',com							!sept 2006
          write(666,*) 'dc après write 303:'								!extension neto	!fev2007
          write(666,'(9(e10.3,1x))') (dw(i),i=1,ntot)						!sept 2006
		endif															!sept 2006		
	  endif																!sept 2006
	  rewind 302														!extension neto	!fev2007
	endif																!sept 2006

c	read(55,*)	!bypass restriction cout								!fev2007
c	read(55,*)															!fev2007


c	restriction sur cout		!restri cout							!fev2007
c	=====================												!fev2007
																		!fev2007
	read(55,4) atext													!fev2007
      read(55,*) iprice,pricemax,icost									!fev2007
																		!fev2007
	if((iopti.ne.0).and.(iprice.ne.0)) then								!fev2007
	  price=0.															!fev2007
	  call annuld(dprice,ntot)											!fev2007
	  initcout=icout													!fev2007
	  if(icost.eq.1) then												!fev2007
	    icout=icost														!fev2007
		ncont=0															!fev2007
	    do nel=1,neto													!fev2007
            nbrxi=nvar(nel)												!fev2007
            call objct1(nel,ntot,ncont,xiopt,nxit(1,nel),nbrxi,			!fev2007
     *				  price,dprice,width,iprint,spec(nel),				!fev2007
     *				  itype(nel),isect(nel))							!fev2007
            ncont=ncont+nbrxi												!fev2007
	    enddo															!fev2007
	  endif																!fev2007
	  if(icost.gt.1) then												!fev2007
	    icout=icost														!fev2007
		call sensibcout(dprice,price,ntot,width)						!fev2007
	  endif																!fev2007
	  icout=initcout													!fev2007
	  write(66,'(//a/1x,21(1h*))')										!fev2007
     *    ' restriction sur cout'											!fev2007
	  write(666,'(//a/1x,21(1h*))')										!fev2007
     *	' restriction sur cout'											!fev2007
	  write(66,'(a,t17,e14.7,t33,a)')									!fev2007
     *	' cout actuel = ',price,'euro ($ or ...)'						!fev2007
	  write(666,'(a,t17,e14.7,t33,a)')									!fev2007
     *	' cout actuel = ',price,'euro ($ or ...)'						!fev2007
	  write(66,'(a,t17,e14.7,t33,a)')									!fev2007
     *	' cout maximum = ',pricemax,'euro ($ or ...)'					!fev2007
	  write(666,'(a,t17,e14.7,t33,a)')									!fev2007
     *	' cout maximum = ',pricemax,'euro ($ or ...)'					!fev2007
        com='price < pricemax'											!fev2007
	  c=price															!fev2007
	  cm=pricemax														!fev2007
	  write(303) c,cm,com												!extension neto	!fev2007
	  write(303) (dprice(i),i=1,ntot)									!extension neto	!fev2007
!	  write(6969,*) c,cm,com												!extension neto	!fev2007
!	  write(6969,*) (dprice(i),i=1,ntot)									!extension neto	!fev2007
	  img=img+1									! +1 au compteur des restrictions struc		!fev2007
        if(iopti.gt.2) then												!fev2007
		if(impr2.ne.-3) then											!fev2007
	    write(666,*)													!fev2007
	    write(666,*) 'c,cm,text=',c,cm,'  ',com							!fev2007
          write(666,*) 'dc après write 303:'								!fev2007
          write(666,'(9(e10.3,1x))') (dprice(i),i=1,ntot)					!fev2007
		endif															!fev2007
	  endif																!fev2007
	  rewind 302														!extension neto	!fev2007
	endif																!fev2007


c 11.0 moments d'extremites   (subr. bateau) ==================================
c =============================================================================
      read(55,4)atext
      read(55,*) imom
      if (imom.eq.0) goto 2314
      write(66,5241)atext
      read (55,*) yred
      write(66,2313)yred

      ico=0
      if(itera.eq.0) then				!septembre2006
        if((dabs(1.0-yred).gt.0.001).or.(jlph.eq.1)) then
          if(langue.eq.1)then
            write(6,100)' ... application d''un facteur de reduction'
            write(6,100)'     aux moments de flexion d''extremites.'
            write(6,101)'     facteur de reduction: yred =',yred

            write(6970,100)' ... application d''un facteur de reduction'
            write(6970,100)'     aux moments de flexion d''extremites.'
            write(6970,101)'     facteur de reduction: yred =',yred

          else
            write(6,100)' ... applying a reduction factor at'
            write(6,100)'     the extremity bending moments.'
            write(6,101)'     reduction factor: yred =',yred

            write(6970,100)' ... applying a reduction factor at'
            write(6970,100)'     the extremity bending moments.'
            write(6970,101)'     reduction factor: yred =',yred

          endif
        endif
      endif

      is=0
      do 733 i=1,nsolm
        icc=0
        do k=1,nsol
          if (nnsol(k).eq.i) icc=1   ! icc=1 cas de charge retenu
        enddo
        if (icc.eq.0) then           ! icc=0 cas de charge pas retenu
          read(55,*)
          goto 733
        else
          is=is+1
        endif
        read (55,*) bm1(is),sf1(is),bm3(is),sf3(is)     !eugen	!fev2007
        if (neto.eq.1) then												!flexion d'axe vert.
		write(66,*)'this particular case (only one panel) is not		
     *treated!'
		write(29,*)'this particular case (only one panel) is not	  !bug	!septembre2006
     *treated!'																!septembre2006
          write(*,*)'this particular case (only one panel) is not		
     *treated!'															!flexion d'axe vert.
          write(6970,*)'this particular case (only one panel) is not		
     *treated!'															!flexion d'axe vert.
		stop															!flexion d'axe vert.
        endif																!flexion d'axe vert.
	  if (((bm1(is).ne.(0.)).and.(isymx.ne.0)).
     *      or.((bm3(is).ne.(0.)).and.(isymy.ne.0))) then			!eugen	!fev2007
          write(66,*)'it is not possible to consider a bending moment
     * around an axis of symmetry!'
          write(*,*)'it is not possible to consider a bending moment
     * around an axis of symmetry!'
          write(6970,*)'it is not possible to consider a bending moment
     * around an axis of symmetry!'
	    write(29,*)'it is not possible to consider a bending moment
     * around an axis of symmetry!'    
		stop
        endif
	  write (66,2315) nnsol(is),bm1(is),bm3(is),bm1(is),bm3(is)	        !flexion d'axe vert.	!eugen	!fev2007	  
	  bm1(is)=bm1(is)/yred											    !flexion d'axe vert.
	  bm2(is)=bm1(is)/yred											    !flexion d'axe vert.	!eugen	!fev2007
	  bm3(is)=bm3(is)/yred											    !flexion d'axe vert.
	  bm4(is)=bm3(is)/yred											    !flexion d'axe vert.	!eugen	!fev2007
  733 continue

	if(iana.eq.2) goto 2500												!r&d14	!fev2007

c     pour utiliser les termes pairs et impairs
      ifonct(1)=1
      ifonct(2)=1
      if(langue.eq.1)then
       write(6,100)'moments de flexion primaires (sr bateau)'
       write(6970,100)'moments de flexion primaires (sr bateau)'
      else
       write(6,100)'primary hull bending moments (sr bateau)'
       write(6970,100)'primary hull bending moments (sr bateau)'
      endif
      call bateau(nsol,xneut,yneut)!,isymx,isymy)
c sauvetage dans sbr bateau des forces de bord "abcd" sur file 97 (pour relecture dans bo1)

 2314 continue


c	moments d'extremite en tenant compte des coefficients de participation
c     ======================================================================
	
	do 25 nel=1,neto
	  if(itype(nel).eq.5) goto 25														!coefficient de participation
	  if(part(nel).ne.(1.0)) then
	    ipart=1
	    goto 21
	  endif
   25	continue

	do is=1,nsol
	  if((bm3(is).ne.(0.)).or.(bm4(is).ne.(0.))) then
	    write(66,*) 'the coefficients of participation are not taken
     * taken into account if a bending moment around a vertical axis is
     * considered'
          write(*,*) 'the coefficients of participation are not taken
     * taken into account if a bending moment around a vertical axis is
     * considered'
          write(6970,*) 'the coefficients of participation are not taken
     * taken into account if a bending moment around a vertical axis is
     * considered'
		write(29,*) 'the coefficients of participation are not taken					!bug
     * taken into account if a bending moment around a vertical axis is					!bug
     * considered'
	    pause
          goto 22
	  endif
	enddo 

   21	if((imom.ne.0).and.(ipart.eq.1)) then
       if(langue.eq.1)then
        write(6,100)'moments d''extremites (sr participation)'
        write(6970,100)'moments d''extremites (sr participation)'
       else
        write(6,100)'extremity moments (sr participation)'
        write(6970,100)'extremity moments (sr participation)'
       endif
	  write(66,5242)
	  call participation(nsol)
	endif																!coefficient de participation


c	rearrangement du fichier de travail 97
c     =======================================
   22	if(imom.ne.0) then
        rewind 97															!coefficient de participation
	
  	  do is=1,nsol
	    do 23 nel=1,neto
	      if(itype(nel).eq.5) goto 23
		  read(97) abcd
		  do i=1,8
		    coef(nel,i,is)=abcd(i)
	      enddo
   23	    continue
	  enddo
	 
	  rewind 97
	
	  do 24 nel=1,neto
	    if(itype(nel).eq.5) goto 24
	    do is=1,nsol
	      do i=1,8
	        abcd(i)=coef(nel,i,is)
	      enddo
	      write(97) abcd
	    enddo
  24	  continue
      endif																!coefficient de participation



c12.0 lecture des restrictions d'egalite (et impression)
c     =================================================
 2500 read(55,4,end=903)atext			!r&d14	!fev2007
      read(55,*,end=903)negalt
      do 1333 i=1,negalt
        read(55,*,end=903) (mega(i,k),k=1,4),ega(i)
 1333 continue

      if((iopti.ge.1).and.(itera.eq.0)) then				!sept2006
       if(langue.eq.1)then
        write(6,100)'restrictions d''egalite (sr sbega)'
        write(6970,100)'restrictions d''egalite (sr sbega)'
       else
        write(6,100)'equality restrictions (sr sbega)'
        write(6970,100)'equality restrictions (sr sbega)'
       endif
       call sbega(atext)!,negalt)		!sept06
      endif


c13.0 donnees pour le calcul de la resistance ultime de la poutre navire
c     ==================================================================
c     mise à zéro des sections du deck,fonds et sides
      if(langue.eq.1)then
       write(6,100)'resistance ultime (sr hull)'
       write(6970,100)'resistance ultime (sr hull)'
      else
       write(6,100)'ultimate strength (sr hull)'
       write(6970,100)'ultimate strength (sr hull)'
      endif
      call hull(itera)


c **********************************************************************
c **********************************************************************

	if(iana.eq.2) then													!r&d14	!fev2007

	  open(68,file=res5)			! listing resultats					!fev2007
	  if (itera.lt.iter1)then											!fev2007
	    write(68,'(t40,a,t50,i2/,t40,12(1h=))') 'iteration',itera		!fev2007
	  elseif (itera.eq.iter1) then										!fev2007
	    write(68,'(t40,a,t50,i2/,t40,12(1h=))') 'iteration',iteram		!fev2007
	  endif																!fev2007

	  write(68,'(//a)') '!!!structural analysis using beam theory!!!'	!fev2007

c	neutral axis and moment of inertia (gross, net and plate scantling)
c	===================================================================
	
	  write(68,'(///a/,34(1h-)/)')										!fev2007
     *  '/neutral axis and moment of inertia/'							!fev2007

c	net scantling
c	-------------
	  call inertia(yneutnet,dyneutnet,inet,dinet,nsol,0,0)				!fev2007

c	gross scantling
c	---------------
	  call inertia(yneutgro,dyneutgro,igro,digro,nsol,1,0)				!fev2007

	  write(68,'(t15,a,t33,a/,48(1h-))')								!fev2007
     *  'net scantling','gross scantling'									!fev2007
	  write(68,'(a,t18,f6.3,t37,f6.3)')'yneut [m]',yneutnet,yneutgro	!fev2007
	  write(68,'(a,t17,f8.3,t36,f8.3)')'ixx [m4]',inet,igro				!fev2007

c	plate scantling
c	---------------
c	  net scantling	  
	  call inertia(yneutplnet,dyneutplnet,iplnet,diplnet,nsol,0,1)		!fev2007

c	  gross scantling
        call inertia(yneutplgro,dyneutplgro,iplgro,diplgro,nsol,1,1)		!fev2007


c	structural analysis based on beam theory							
c	========================================
	  
c	hull girder shear stresses
c	-------------------------- 

	  do nel=1,neto
	    read(302) epsa,epsr,thicknet(nel)
	    thickgro(nel)=thicknet(nel)+corro(nel,1)
	  enddo
	  rewind 302

c	  net scantling
c	  -------------
c        et=timef()

        call shear(thicknet,yneutplnet,dyneutplnet,		!fev2007
     *			 iplnet,diplnet,taunet,dtaunet)			!fev2007
c	   et1=timef()				
	  
c	  gross scantling
c	  ---------------
c        et=timef()
        call shear(thickgro,yneutplgro,dyneutplgro,		!fev2007
     *			 iplgro,diplgro,taugro,dtaugro)			!fev2007
c	   et1=timef()				

	  
	  write(68,'(///a/,32(1h-))') '/shear stress and normal stress/'	!fev2007

	  do is=1,nsol														!fev2007
	    write(68,'(/a,t12,i2/,14(1h*))') '/load case/',is				!fev2007
		write(68,'(t42,a,t84,a/,106(1h-))')								!fev2007
     *    'net scantling','gross scantling'								!fev2007
	    write(68,'(a,t12,a,t33,a,t46,a,t60,a,t75,a,t88,a,t102,a)/')		!fev2007
     *    'panel','stress [n/mm2]','start','center',						!fev2007				
     *    'end','start','center','end'									!fev2007

		do nel=1,neto													!fev2007
		  if(itype(nel).ne.5) then										!fev2007

c	hull girder bending stresses
c	----------------------------

		    call imprtau(nel,is)											  						

c	  net scantling
c	  -------------
c	        et=timef()
	        call bending(nel,bm1(is),yneutnet,dyneutnet,inet,dinet,		!fev2007
     *					 signet,dsignet)								!fev2007
c	        et1=timef()

c	  gross scantling
c	  ---------------
c	        et=timef()
	        call bending(nel,bm1(is),yneutgro,dyneutgro,igro,digro,		!fev2007
     *					 siggro,dsiggro)								!fev2007
c	        et1=timef()

		    write(68,'(1x,i3,t15,a,t31,f8.3,t57,f8.3,					
     *	    t73,f8.3,t99,f8.3)') nel,'/sx/',(signet(1)/1.e+06),			
     *		(signet(2)/1.e+06),(siggro(1)/1.e+06),(siggro(2)/1.e+06)	!fev2007

c	stiffener bending stresses
c	--------------------------
			read(302) epsa,epsr,delta,hya,dya,wya,tya,					!fev2007	
     *		          hxr,dxr,wxr,txr,phil,q,epais,epsa2				!fev2007

			if(hxr.gt.0.010) then										!fev2007
			  call stifbend(nel,epsa,epsr,delta,hxr,dxr,wxr,txr,		!fev2007
     *						epsa2,is,nsol,sigstif,dsigstif)				!fev2007
		    else														!fev2007
			  sigstif=0.												!fev2007
			  read(99)													!fev2007
			endif														!fev2007

			write(68,'(1x,i3,t12,a,t44,f8.3)') nel,'/sx_stiff/',		
     *		(sigstif/1.e+06)											!fev2007
     
			
c	scantling optimization
c	======================

			if(iopti.ge.1) then											!fev2007

			  if(epsa2.ge.(0.00001)) then								!fev2007
			    backspace(302)											!fev2007
	            read(302) epsa,epsr,delta,hya,dya,wya,tya,		
     *		              hxr,dxr,wxr,txr,phil,q,epais,epsa2,
     *				      hya2,dya2,wya2,tya2							!fev2007
			  endif														!fev2007

	continue


c	iacs requirements
c	=================
                ratio=0.
			  do i=1,m1tabl(nel,is)										!fev2007
			    read(78) ic,iy,invv,cmax								!fev2007
!	            write(6969,*)
!	            write(6969,*)'ic=',ic
!				write(6969,*)'nel=',nel
!				write(6969,*)'is=',is
!				write(6969,*)'m1tabl=',m1tabl(nel,is)	
!	            write(6969,*)'invv=',invv
!				write(6969,*)'cmax=',cmax
!				write(6969,*)'coefk=',coefk(nel)
!	            write(6969,*)

				if(ic.eq.10) then										!fev2007
				  cm=invv*cmax/coefk(nel)								!fev2007	!r&d15
				  com='el.bord.flex'									!fev2007
				  call bendyield(nel,ciacs,dciacs)						!fev2007
	            elseif(ic.eq.14) then									!fev2007
c		          et=timef()											!fev2007
				  cm=invv*cmax											!fev2007
	              com='fl.bord.comp'									!fev2007
				  call compbuck(nel,e(nel),sigy(nel),coefk(nel),		!fev2007	!r&d15
     *						    epsa,epsr,delta,hxr,dxr,wxr,txr,		!fev2007
     *				            wya,q,epsa2,wya2,ciacs,dciacs)			!fev2007	!juil07
c		          et1=timef()											!fev2007
			    elseif(ic.eq.19) then									!fev2007
				  cm=invv*cmax/coefk(nel)								!fev2007	!r&d15
	              com='el.bord.cis'										!fev2007
				  call shearyield(nel,is,ciacs,dciacs)					!fev2007
				elseif(ic.eq.20) then									!fev2007
c		          et=timef()											!fev2007
				  cm=invv*cmax											!fev2007
	              com='fl.bord.cis'										!fev2007
				  call shearbuck(nel,e(nel),sigy(nel),q,epsa,epsr,		!fev2007	!juil07
     *							 delta,hxr,epsa2,is,ciacs,dciacs)		!fev2007
				  continue												!fev2007
c		          et1=timef()											!fev2007
				elseif(ic.eq.37) then									!fev2007
c		          et=timef()											!fev2007
				  cm=invv*cmax											!fev2007
	              com='fl.raid.'										!fev2007
				  call stifbuck(nel,e(nel),sigy(nel),coefk(nel),		!fev2007	!r&d15
     *							epsa,epsr,delta,hxr,dxr,wxr,txr,		!fev2007
     *							wya,q,epsa2,wya2,ciacs,dciacs)			!fev2007	!juil07
c		          et1=timef()											!fev2007
				elseif(ic.eq.38) then									!fev2007
c		          et=timef()											!fev2007
				  cm=invv*cmax/coefk(nel)								!fev2007	!r&d15
	              com='el.raid.'										!fev2007
				  call stifyield(nel,coefk(nel),ciacs,dciacs)			!fev2007	!r&d15
c		          et1=timef()											!fev2007
				  continue												!fev2007

				elseif(ic.eq.39) then									!fev2007
c		          et=timef()											!fev2007
				  cm=invv*cmax											!fev2007
	              com='fl.raid.'										!fev2007
				  call stifbuck_dcns(nel,e(nel),sigy(nel),coefk(nel),   !fev2007	!r&d15
     *							epsa,epsr,delta,hxr,dxr,wxr,txr,		!fev2007
     *							wya,q,epsa2,wya2,ciacs,dciacs)			!fev2007	!juil07
c		          et1=timef()											!fev2007


				endif													!fev2007
				write(303) ciacs,cm,com,ic,ratio                        !fev2007
				write(303) (dciacs(j),j=1,ntot)							!fev2007
!				write(6969,*) 'ciacs=',ciacs
!				write(6969,*) 'cm=',cm
!				write(6969,*) 'com=',com							    !fev2007
!				write(6969,*) (dciacs(j),j=1,ntot)	
!				write(6969,*) 'break_beam'	
										!fev2007
			  enddo														!fev2007
			endif														!fev2007
		  else															!fev2007
		    read(302)													!fev2007
			read(99)													!fev2007
		  endif															!fev2007
	    enddo															!fev2007
	    rewind 302														!fev2007
		rewind 99														!fev2007
	  enddo																!fev2007

	  rewind 78															!fev2007

	  goto 2501															!fev2007
	
	endif																!r&d14	!fev2007	

c **********************************************************************
c **********************************************************************

c14.0 mise en forme des equations de continuite et resolution (bo1)
c     ==================================================================
c     boucle externe sur les termes de la serie de fourier (boucle 2201)
c    14.1  boucle interne (bo1) sur les panneaux et sur les cas de charges,
c          structure bi-appyée           
c    14.2  boucle interne (co1/co4) pour les forces de bords
c    14.3  calcul des sensibilités (mdr2)
c           

	if(langue.eq.1)then
       write(6,100)'mise en forme des equations de continuite (sr bo1)'
       write(6,*)

       write(6970,100)
     *'mise en forme des equations de continuite (sr bo1)'
       write(6970,*)
      else
       write(6,100)'assembly of the rigidity matrix (sr bo1)'
       write(6,*)
 
       write(6970,100)'assembly of the rigidity matrix (sr bo1)'
       write(6970,*)
      endif
      do 2201 ijk=1,jlph  !jlmax ************
      if(mod(ijk,2).eq.1) then  
	  iterm=1              ! iterm = 1 si ijk est impair
        ibat =1              ! 'pour le terme ',ijk,'ibat=1'
	else       
        iterm=2              ! iterm = 2 si ijk est pair
	  ibat=0               ! (terme pair)
        if(jlph2.le.0) then  ! jlph negatif, seul les termes impairs sont considerés
		ifonct(iterm)=0    ! skip termes pairs 
          goto 2201	
	  endif
      if((imom.ne.0).or.(ichag.ne.0).or.(ibusc.ne.0)) then 
        !imom =0  flexion d'ensemble --> pas besoin de calculer les termes pairs
	  !ichag=0  charge selon x     --> pas besoin de calculer les termes pairs
	  !ibusc=0  force de buscage   --> pas besoin de calculer les termes pairs
        goto 555  ! il faut utiliser les termes pairs de la série de fourier
      endif
      ifonct(iterm)=0
	goto 2201  ! skip termes pairs         
	endif
 555  continue

      if(langue.eq.1) then
	   write(6,8000)'serie de fourier : terme no',ijk		!fev2007
	   write(6970,8000)'serie de fourier : terme no',ijk		!fev2007
      endif

      if(langue.eq.2) then
	   write(6,8000)'fourier series: term nr',ijk			!fev2007
	   write(6970,8000)'fourier series: term nr',ijk			!fev2007
      endif
 8000 format('+',a,1x,i2)

	  ifonct(iterm)=1 

        rewind 97
        rewind 98
        rewind 99


        call lamsr(lam,ijk,pi,width)

        do nel=1,neto
          ich=icha(nel)
          if(ipoids.eq.1) ich=1
          call bo1 (nel,width,ijk,ne,
     *              jlph,ibat,ich,imom,ibusc,
     *              nvar(nel),nxit(1,nel),ipts(nel),ypts(1,nel),nsol,
     *              nc)
        end do
      rewind 99

c14.3 module d'optimisation - calcul des sensibilites (subr. mdr2)
c     ------------------------------------------------------------
c      dzsn(ne,9*neto,10)sauvé dans allong
c      la taille max de dzsn pour neto = 100 est 7.2 mio, soit 57.6 mb

      if(iopti.ge.1)then
        call mdr2(ne,nsol,nc)
      endif

 2201 continue       ! fin de la boucle sur les termes de fourier (jlph)                                                   

      do nel=1,neto
         rewind(400+nel)	!extension neto	!fev2007
         rewind(700+nel)
	end do

c **********************************************************************
c **********************************************************************

c 15.0 determination des contraintes et deplacements (subr. bo2)
c     =========================================================

      if(langue==1) then
	   write(*,408)
         write(6970,408)
	endif
      if(langue==2) then 
	   write(*,409)
         write(6970,409)
	endif
      if((itera.eq.0).and.(iopti.ge.1))write(666,512)

c     img= compteur des restr. structurelles général (= 1 ou 2 si rest sur centre de gravité)
      do 2401 is=1,nsol
        if(langue.eq.1)then
	     write(6,8000)'cas de charge no',is
	     write(6970,8000)'cas de charge no',is
	  endif
        if(langue.eq.2)then
	     write(6,8000)'load case nr',is
	     write(6970,8000)'load case nr',is
	  endif
        if(langue==1 ) then ! french
           write(66,2402) is
           if(iopti.ge.1) write(666,2402)is
           write(67,2402) is
	  else  ! english
           write(66,2403) is
           if(iopti.ge.1) write(666,2403)is
           write(67,2403) is
	  endif
c     read (in file 78) the list of constraints saved in subr. ent 
      im=0
      if(iopti.ge.1) then
         do ip=1,neto
           do i=im+1,im+m1tabl(ip,is)
               read(78) lcont(1,i),lcont(2,i),inv(i),cjmax(i)
	      enddo
	   im=im+m1tabl(ip,is)
	   enddo
	endif

      im=0				!compteur des restr. structurelles pour le cas de charges en cours
	rewind(306)			!13.05.04	!extension neto	!fev2007
	nbuck=0				!dcn07
      do 2301 nel=1,neto
        ich=icha(nel)
        if(ipoids.eq.1) ich=1
        rewind (100+nel)
      call bo2(nel,jlph,indaig,indrai,ne,width,
     *         ich,imom,iprint,is,nsol,im,img,itera,
     *         i66,i67,m1tabl(1,is),sxm,nbuck)		!dcn07		
        rewind (100+nel)
        rewind (400+nel)	!extension neto	!fev2007
        rewind (700+nel)

c15.1 sauvetage des donnees graphiques
c     ---------------------------------
        if(dessin.eq.0)goto 2301

      call save(nel,indaig,indrai,itype(nel))        ! lulu : on n'utilisera plus le dessin fortran mais l'interface

 2301 continue
      img=img+im   ! compteur general des restr struct
      rewind 304	 !extension neto	!fev2007
	rewind 2218								!février 2004						

c15.2 impressions de s ultime panneaux (paik)
c     --------------------------------------
       if (impr2.ge.-2) then				!15.10.05							!septembre2006        
	  if(langue==2 ) then ! english
           write(67,'(/a/40(1h-))')' ultimate strength of panels (paik)'
	     write(67,'(2a)')'nel,  sig ult (n/mm2) > ? >',  !sult calculé via subr. result
     *        ' mean compressive axial stress, (nx)'       !nx   calculé dans subr contr
	  else  ! french
          write(67,'(/a/40(1h-))')' resist ultime panneau (paik)'
	    write(67,'(2a)')'nel,  sig ult (n/mm2) > ? >',
     *        ' contrainte moyenne axiale en compression (nx)'
	  endif
c
c    attention : a n'est pas le meme que a declare dans sharedvar !!!
c	  do inel=1,neto													
c	    write(67,'(i3,1x,f10.5,18x,f10.5)')inel, a(i66+inel-1)/1.e6,	
c     *                                            -a(i67+inel-1)/1.e6		
c        enddo

	  
c	  do inel=1,neto
c	  if(itype(inel).ne.5) then													
c	   write(67,'(i3,1x,f10.5,18x,f10.5)')inel, sxm(iptmax+inel)/1.e6,	
c     *                                      -sxm(iptmax+neto+inel)/1.e6		
c        endif
c	  enddo

	  do inel=1,neto
	  if(itype(inel).ne.5) then													
	   write(67,'(i3,1x,f10.5,18x,f10.5)')inel, sxm(iptmax+inel)/1.e6,	
     *                                      -sxm(iptmax+neto+inel)/1.e6		
        else	 
	   write(67,'(i3,1x,a9,18x,a9)')inel,'EPONTILLE','EPONTILLE'      !nov2006                              
	  endif
	  enddo
	 																
	endif					!15.10.05										

c15.3 analyse des résultats extrêmes
c     -------------------------------
      call analys(indaig,indrai)!,impr2)		! février 2004

      rewind 304		!extension neto	!fev2007
      rewind 99
	rewind 2218						! février 2004

 2401 continue


c **********************************************************************
c **********************************************************************

c16.0 module d'optimisation
c     =======================

 2501 if(iana.eq.2) rewind 99						!r&d14	!fev2007
	if(iopti.eq.0) goto 304						! goto §16.1)	!13.12.05	!multi obj
	if(itera.ge.iter1) goto 350					!multi obj
	if ((iopti.ge.1).and.(itera.eq.0))then		!13.12.05
	itera=1										!13.12.05
	endif										!13.12.05
      rewind 303		!extension neto	!fev2007

      
	call optis(itera,iteram,width,iprint,nsol,txr_dcns,tya_dcns)!,negalt)

c avec itera = numéro de l'itération en cours
c	 m1tot = nbre de restrictions structurelles (=m1) pour 1 cas de charge
c	 m2tot = nbre de restrictions géométriques  (=m2)

c       1 delta = nouvelle épaisseur du bordage
c       2 hya   = nouvelle hauteur de l'âme des tranversaux (aiguilles)
c       2 hya   = nouvelle épaisseur de l'âme des tranversaux (aiguilles)
c       4 wya   = nouvelle largeur des semelles des tranversaux (aiguilles)
c       5 epsa  = nouvelle entredistance entre tranversaux (aiguilles)
c       6 hxr   = nouvelle hauteur de l'âme des raidisseurs
c       6 hxr   = nouvelle épaisseur de l'âme des raidisseurs
c       8 wxr   = nouvelle largeur des semelles des raidisseurs
c       9 epsr  = nouvelle entredistance entre raidisseurs
	
	
      ico=0                                     !
      if(iopti.eq.1) goto 357                   ! iter. automatique
       write(*,*)
       write(*,*) ' voulez relancer une nouvelle iteration ?'
       write(*,*) ' oui=0 (défaut),  non=1'

       write(6970,*)
       write(6970,*) ' voulez relancer une nouvelle iteration ?'
       write(6970,*) ' oui=0 (défaut),  non=1'

       read(*,*) ico
 357  continue

c     to save "updated data" after optimization process

      call copy(itera,txr_dcns,tya_dcns)

  350 if(ico.ne.1) then
         rewind 42
         rewind 43
         rewind 45
         rewind 46
         rewind 78 
	   if(ibusc.ne.0) rewind 95
	   if(ibusc.ne.0) rewind 96
         rewind 97 
c        rewind 98 (pas nécessaire car sauvetage des données seulement pour itera=1)
  	   rewind 99
  	   rewind 301	!extension neto	!fev2007
  	   rewind 302	!extension neto	!fev2007
         rewind 303	!extension neto	!fev2007
         rewind 304	!extension neto	!fev2007
	   rewind 305	!extension neto	!fev2007
	   rewind 2218	!février 2004

         itera=itera+1
         rewind 55
!	   read(55,'(a)') version				!r&d14	!fev2007     saute titre
!	   read(55,*)   iana1				!r&d14	!fev2007     saute iana
!	   read(55,'(a)')	title			!dad	!14.12.2007  saute parametres redressage
!	   read(55,*)               !dad	!26.06.2007  saute parametres pareto
!	   read(55,*)               !dad	!26.06.2007  saute parametres rend_panel
!	   read(55,4) atext			!r&d14	!fev2007
         read(55,'(26(/))')
	   do i=1,nsolm       !nov2003
	     read(55,*) title      !nov2003
	   enddo              !nov2003
	   close (68)	!r&d14	!fev2007
         goto 303
c        vers une nouvelle itération (goto 303)
      endif

c
c 16.1 fin des iterations (optimisation), fermeture des fichiers ==============
c==============================================================================
  304 close(55)
      close(97)
      close(98)
      close(99)
      do  nel=1,neto
        close(100+nel)
        if(iopti.ge.1) close(400+nel)		!extension neto	!fev2007
        close(700+nel)
      enddo
      close (301)		!extension neto	!fev2007
      close (302)		!extension neto	!fev2007
      close (303)		!extension neto	!fev2007
      close (304)		!extension neto	!fev2007
      close (305)		!extension neto	!fev2007
      close (2218)
      if(iopti.ge.1) close (666)
      close (66)
      close (67)
	close (68)	!r&d14	!fev2007
      close (77)
	close (6969)
      return

c ======================================================================
  900 write(*,*) 'error à la lecture des restrictions d''égalité'
      write(6970,*) 'error à la lecture des restrictions d''égalité'
      stop

c
c formats .....................................................................
 100  format(1x,a,i8)
 101  format(1x,a,e15.6)
 200  format(a)
    3 format(1h1,1x,70(1h*)/,
     *       2x,1h*,4x,15a4,4x,1h*/,
     *       2x,1h*,4x,60(1h+),4x,1h*/,
     *       2x,1h*,68x,1h*/,
     *       2x,70(1h*)//)
    4 format(15a4)
    5 format(1h1)
    6 format(///
     * 16x,'analysis of a structure composed of ',i3,' panel(s)'/
     * 15x,49(1h+)/)
    7 format(/' avec resultats intermediaires')
    8 format(/' avec resultats finaux + verifications')
    9 format(/' pas de resultats dans les transversaux (cadres).')
   10 format(///
     *,16x,'analyse d''une structure a ',i3,' panneau(x)'/15x,45(1h+)/)
   11 format(///' unites en metres et newtons',//,' nombre de termes',
     * ' de la serie de fourier pour les charges ',i3//
     * ' efforts de bord nb et mb (buscage) --> file: busc.txt'/
     * '  ibusc =',i2,'  ( 0:sans nb et mb  et 1 : avec nb et mb)'/)
   12 format(/' coordonnees (x) des 5 sections ou les resultats sont',
     *        ' fournis (en metre)'/t10,5(2x,f9.4))
   13 format(/' coefficients de cisaillement des semelles',
     *          ' des transversaux',2(f6.3,2x)/
     *      42x,' des raidisseurs ',2(f6.3,2x)/
     *      42x,' des traverses   ',2(f6.3,2x)//)
   14 format(///' units: metre, newton and equivalent',
     *          ' water height (m) for lateral pressure'//
     * ' number of terms of the fourier series expansion',
     * ' for loads (jlph) =',i3//
     * ' with end forces nb & end moments mb defined in file: busc.txt'/
     * '  ibusc =',i2,'  ( 0:without nb & mb  et 1 : with nb & mb)'/)
!  301 format(17(/),a1)  ! saut de 20 lignes lors de la relecture du fichier de données	!r&d14	!fev2007
  301 format(27(/))  ! saut de 20 lignes lors de la relecture du fichier de données	!r&d14	!fev2007
  399 format(/'limitations'/12(1h*)/,
     *  i5,' = nbre max de panneau (n)'/
     *  i5,' = nbre max de variables de conception pour',
     *          ' la structure (nv)'/
     *  i5,' = nbre max  rest. struct. par panneau (m1)'/
     *  i5,' = nbre max  rest. géom. par panneau (m2)'/
     *  i5,' = nbre max  rest. (struct. + géom/) pour la ',
     *           'structure et par tous les cas de charges (mm) !!'/
     *     '        = somme(m1+m2) !!'/
     *  i5,' = nbre max  rest. égalité (pour la struct.) (ng)'/
     *  i5,' = nbre max de cas de charges utilisés (is)'/
     *  5x,'   (mais il peut y avoir jusqu à 20 cas de charges',
     *     ' dans le jeu de données dont 10 peuvent être utilisés)'/
     *  i5,' = nbre max de pts de calcul des sensibilités',
     *              ' par panneau (ipt)'/)
  400 format(21x,20(1h*))

  405 format(/' le poids total de la structure est = ',e14.7,' n.',/					!septembre2006
     *		'ce poids reprend la structure principale, secondaire,' !05.12.05		!septembre2006
     *		 'les traverses et la corrosion.'/						!05.12.05		!septembre2006
     *		 'avant itération d''optimisation.'/					!13.12.05		!septembre2006
     *        ' (!!1/2 structure si symétrique et structure déjaugée'	!05.12.05		!septembre2006
     *		   'si ipa= 1)'/)														!septembre2006
  406 format(/'assembling of the rigidity matrix (subr. bo1)'/43(1h=))
  407 format(/'mise en forme des equations de continuite et resolution',
     *        ' (subr. bo1)'/50(1h=))
  408 format(/'determination des contraintes et deplacements'/46(1h=))
  409 format(/'calculation of the displacements & stresses'/46(1h=))
  417 format(/'module optimisation (subr. mdr2)')
  418 format(//30x,'iteration nø',i2/30x,20(1h=)/)
  419 format( ' cas de charge considérés:'/1x,25(1h=)/
     *        ' - nbre de cas de charge disponibles = ',i2/
     *        ' - nbre de cas de charge analysés    = ',i2/
     *        ' - numeros des cas analysés = ',10i3//)
  420 format( ' considered load cases:'/1x,22(1h=)/
     *        ' - available  load cases = ',i2/
     *        ' - considered load cases = ',i2/
     *        ' - nbr of the considered cases = ',10i3//)
  421 format(/' la fonction objectif est le poids'/1x,35(1h-))
  422 format(/' la fonction objectif est le cout' /1x,35(1h-))
  428 format(/' la fonction objectif est inertie' /1x,32(1h-))	!obj inertie	!septembre2006
  429 format(/' la fonction objectif est multi' /1x,32(1h-))				!obj multi (10.10.2007 - eugen)
  423 format(3x,'rendement global du chantier         =',f8.3/
     *       3x,'equivalent poids de la main d''oeuvre =',f8.3/
     *       3x,'epaisseur de référence borde (dref)  =',f8.3,' m'/
     *       3x,'epaisseur de référence long. (drefx) =',f8.3,' m'/
     *       3x,'epaisseur de référence cadre (drefy) =',f8.3,' m'/
     *      3x,'coût du matériau au kg, borde (c1)   =',f8.3,' euro/kg'/
     *      3x,'coût du matériau au kg, long. (c2)   =',f8.3,' euro/kg'/
     *      3x,'coût du matériau au kg, cadre (c3)   =',f8.3,' euro/kg'/
     *       3x,'variation de c1 par mm de tôle (dc1) =',f8.3/
     *       3x,'poids compl.pour membr.longitud.(dw2)=',f8.3/
     *       3x,'poids compl.pour membr. transv. (dw3)=',f8.3/)
  424 format(3x,'mdo construction du bordé (p10)      =',f8.3,' h-h/m2'/
     *       3x,'variat. de p10 par mm de bordé (dp10)=',f8.3/
     *       3x,'mdo soudage long. sur bordé (p4)     =',f8.3,' h-h/m'/
     *       3x,'variat. de p4 par mm d''âme (dp4)     =',f8.3/
     *       3x,'mdo soudage transv. sur bordé (p5)   =',f8.3,' h-h/m'/
     *       3x,'variat. de p5 par mm d''âme (dp5)     =',f8.3/)
  425 format(3x,'mdo construct. membrures long..(p9x) =',f8.3,' h-h/m'/
     *       3x,'mdo construct. membrures cadre (p9y) =',f8.3,' h-h/m'/
     *       3x,'variat. de p9x par mm d''âme (dp9x)   =',f8.3/
     *       3x,'variat. de p9y par mm d''âme (dp9y)   =',f8.3/)
  426 format(3x,'mdo pour intersect. long/trans (p6)  =',f8.3,
     *                                                 ' h-h/unité'/
     *       3x,'mdo pour gousset pour long/trans (p7)=',f8.3,
     *                                                 ' h-h/unité'/
     *       3x,'fréq. relat.gousset sur long. (bétar)=',f8.3/
     *       3x,'fréq. relat.gousset sur trans.(bétat)=',f8.3/)
  427 format(3x,'coût energie+consommable(soudure)(c8)=',f8.3,' euro/m'/
     *       3x,'var. de c8 par mm de la tôle soudée  =',f8.3/
     *       3x,'longitudinaux reconstitués (alpha r) =',i4,' oui=0'/
     *       3x,'transversaux  reconstitués (alpha t) =',i4,' non=1'//)

  511 format('variables de conception et restrictions geometriques'/
     *        52(1h=))
  512 format(/'restrictions'/14(1h=))

 2313 format(/'le coefficient de réduction est ',f6.4)
 2315 format('cas de charge nø',i2' les moments d''extremites',
     *  ' (non réduits) sont:'/
     *'a gauche bmx1= ',e14.7,' n.m'/,'         bmy1= ',e14.7,' n.m'/,
     *'a droite bmx2= ',e14.7,' n.m'/,'         bmy2= ',e14.7,' n.m'/)                     !flexion d'axe vert.
 2402 format(//30(1h*)/' *** cas de charge nø',i2,' ***'/30(1h*)/)
 2403 format(//25(1h*)/' *** load case no',i2,'  ***'/25(1h*)/)
 5241 format(///' donnees relatives aux moments d''extremites'/
     *  t2,50(1h*)/15a4)
 5242 format(///' donnees relatives aux coefficients de participation'/
     *  t2,50(1h*)/15a4)
c
c erreur de lecture du fichier de donnees (unite 55) ..........................
  903 if(langue.eq.1)then
         write(6,*)' fichier d''entree corrompu ... sortie.'
         write(6970,*)' fichier d''entree corrompu ... sortie.'
	endif
      if(langue.eq.2)then
	   write(6,*)' data file corrupted ... aborting.'
	   write(6970,*)' data file corrupted ... aborting.'
	endif
      stop 1
c
c erreur de lecture du fichier atableur.txt (unite 57) ........................
  904 if(langue.eq.1)then
         write(6,*)' atableur.txt corrompu ... sortie.'
         write(6970,*)' atableur.txt corrompu ... sortie.'
	endif
      if(langue.eq.2)then
	   write(6,*)' atableur.txt corrupted ... aborting.'
	   write(6970,*)' atableur.txt corrupted ... aborting.'
	endif
      stop 2
c
c erreur sur le nbe de sollicitations .........................................
  905 if(langue.eq.1)then
       write(6,*)' nombre de sollicitations incorrect ... sortie.'
       write(6970,*)' nombre de sollicitations incorrect ... sortie.'
      endif
      if(langue.eq.2)then
       write(6,*)' wrong load case number ... aborting.'
       write(6970,*)' wrong load case number ... aborting.'
      endif
      stop 3
c
c erreur sur le nbe de sollicitations .........................................
  906 if(langue.eq.1)then
       write(6,*)' nombre de panneaux trop grand ... sortie.'
       write(6970,*)' nombre de panneaux trop grand ... sortie.'
      endif
      if(langue.eq.2)then
       write(6,*)' panel number too high ... aborting.'
       write(6970,*)' panel number too high ... aborting.'
      endif
      stop 4
      end

c
c------------------------------------------------------------------------------
      subroutine lamsr(lamv,ijk,pi,width)
      implicit real*8(a-h,o-z)
      real*8 lamv
      dimension lamv(8)
      lamv(1)=ijk*pi/width
      lamv(2)=lamv(1)*lamv(1)
      lamv(3)=lamv(2)*lamv(1)
      lamv(4)=lamv(2)*lamv(2)
      lamv(5)=lamv(3)*lamv(2)
      lamv(6)=lamv(3)*lamv(3)
      lamv(7)=lamv(4)*lamv(3)
      lamv(8)=lamv(4)*lamv(4)
      return                                                            
      end
