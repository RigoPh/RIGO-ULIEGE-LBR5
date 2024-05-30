subroutine read_panel_data(nel)

use param_section

implicit double precision (a-h,o-z)

!subroutine init_panel_data()
poids9=0.d00
dpoids(:)=0.d00
panneau(nel).aire  =0.d00
panneau(nel).aiy   =0.d00
panneau(nel).aix   =0.d00
panneau(nel).sredy =0.d00
panneau(nel).sredx =0.d00
panneau(nel).tork  =0.d00
argq(:)=0.d00
const(:)   =0.d00
const2(:,:)=0.d00
const3(:,:)=0.d00
darg(:,:)     =0.d00
panneau(:).inda=0 !TODO check si nécessaire
panneau(:).indr=0
! facteur de conversion en radians
pi1=pi/180.d00

! ====================================================================================================
! read panel geometrical properties (curvature radius, angle,height,width) 
!                                   (longitudinal and transverse stiffeners  distribution, spacing, ...)
!                                   (longitudinal girders                    position                  )
! read panel structural  properties (participation coefficient, young modulus) 
! read loads (localized, distributed (stepwise, linearly))
! ====================================================================================================


!call read_panel_type()
! ====================================================================================================
! lecture du type de panneau : coque ou plaque
! ====================================================================================================
read(iu_10(iboat),'(a9)',end=900) texte

! itype= 1  coque       coque  standard
! itype= 2  plaque      plaque standard
! itype= 3  coque1      coque  sans contribution transversale du bordé
! itype= 4  plaque1     plaque sans contribution transversale du bordé
! itype= 5  epontille   epontille élément poutre

if((index(texte,'plaque').ne.0).or.(index(texte,'PLAQUE').ne.0)) then
   panneau(nel).types='plaque'
   read(texte,'(t7,i1)',err=900) i
   if(i.eq.1) then
     itype(nel)=4
   else
     itype(nel)=2
   endif
elseif((index(texte,'coque').ne.0).or.(index(texte,'COQUE').ne.0)) then
   panneau(nel).types='coque'
   read(texte,'(t6,i1)',err=900) i
   if(i.eq.1) then
     itype(nel)=3
   else
     itype(nel)=1
   endif
elseif ((index(texte,'epontille').ne.0).or.(index(texte,'EPONTILLE').ne.0)) then
   panneau(nel).types='epontille'
   itype(nel)=5
endif


if((panneau(nel).types.eq.'plaque').or.(panneau(nel).types.eq.'PLAQUE')) then
   read(iu_10(iboat),*,end=900) panneau(nel).hight,panneau(nel).delta
   panneau(nel).q=10000000000.d00
   panneau(nel).phil=panneau(nel).hight/panneau(nel).q/pi1
elseif((panneau(nel).types.eq.'epontille').or.(panneau(nel).types.eq.'EPONTILLE')) then
   read(iu_10(iboat),*,end=900) panneau(nel).hight,panneau(nel).epais
   !carré, cercle ou double t
   read(iu_10(iboat),'(a7)',end=900) panneau(nel).section
   panneau(nel).q=10000000000.d00
   panneau(nel).phil=panneau(nel).hight/panneau(nel).q/pi1
   panneau(nel).delta=0.d00
else
   read(iu_10(iboat),*,end=900) panneau(nel).phil,panneau(nel).q,panneau(nel).delta
endif

!call read_panel_stiffeners()  !!! write à déplacer !
! ====================================================================================================
! donnees relatives au raidissage (aig. et raid)
! ----------------------------------------------
! entr = entredistance réelle entre raid.=(d)raid
! epsr = largeur collaborante moyenne compte tenu du nbre de raid. défini
!        par mode (type de répartition)
! epsa=(d)aig
! ====================================================================================================


if(itype(nel).ne.5) then
   read(iu_10(iboat),*,end=900) panneau(nel).epsa,panneau(nel).entr,panneau(nel).epsa2,panneau(nel).entr2,panneau(nel).corro(1)	
   read(iu_10(iboat),*,end=900) panneau(nel).hya,panneau(nel).dya,panneau(nel).wya,panneau(nel).tya,panneau(nel).corro(2),panneau(nel).itype_transv,panneau(nel).name_type_transv
   read(iu_10(iboat),*,end=900) panneau(nel).hxr,panneau(nel).dxr,panneau(nel).wxr,panneau(nel).txr,panneau(nel).corro(3),panneau(nel).itype_longit,panneau(nel).name_type_longit
   panneau(nel).epais=0.d00
   isect(nel)=0
   panneau(nel).wya_fatigue=panneau(nel).wya
   panneau(nel).tya_fatigue=panneau(nel).tya
   panneau(nel).wxr_fatigue=panneau(nel).wxr
   panneau(nel).txr_fatigue=panneau(nel).txr
else
   ! heff: hauteur sans hiloire    
   read(iu_10(iboat),*,end=900) panneau(nel).epsa,panneau(nel).heff
   
   if((panneau(nel).section.eq.'cercle').or.(panneau(nel).section.eq.'CERCLE'))then

      isect(nel)  =1
      read(iu_10(iboat),*,end=900) panneau(nel).hya,panneau(nel).corro(2) 

      panneau(nel).dya    =0.d00
      panneau(nel).wya    =0.d00
      panneau(nel).tya    =0.d00
      panneau(nel).corro(1)=0.d00
	  panneau(nel).corro(3)=0.d00

   elseif((panneau(nel).section.eq.'carre').or.(panneau(nel).section.eq.'CARRE'))then
      isect(nel)  =2
      read(iu_10(iboat),*,end=900) panneau(nel).hya,panneau(nel).corro(2)

      panneau(nel).dya    =0.d00
      panneau(nel).wya    =0.d00
      panneau(nel).tya    =0.d00
      panneau(nel).corro(1)=0.d00
      panneau(nel).corro(3)=0.d00

   elseif((panneau(nel).section.eq.'doublet').or.(panneau(nel).section.eq.'DOUBLET'))then
      isect(nel)  =3
      read(iu_10(iboat),*,end=900) panneau(nel).hya,panneau(nel).dya,panneau(nel).wya,panneau(nel).tya,panneau(nel).corro(2)

      panneau(nel).epais  =0.d00
      panneau(nel).corro(1)=0.d00
	  panneau(nel).corro(3)=0.d00 

   endif
   panneau(nel).hxr=1.0d-3
   panneau(nel).dxr=1.0d-3
   panneau(nel).wxr=1.0d-3
   panneau(nel).txr=1.0d-3
   panneau(nel).epsr=panneau(nel).hight/5     !epsr=1.000
   panneau(nel).entr=panneau(nel).hight/5     !entr=1.000
   panneau(nel).epsa2=0.d00
   panneau(nel).entr2=0.d00
endif

if(panneau(nel).epsa.le.(1.0d-5)) panneau(nel).epsa=1.d00
if(panneau(nel).entr.le.(1.0d-5)) panneau(nel).entr=panneau(nel).q*dabs(panneau(nel).phil)*pi1

if((panneau(nel).epsa2.ge.(1.0d-5)).or.&
   (panneau(nel).entr2.ge.(1.0d-5))) then

   if(panneau(nel).epsa2.le.(1.0d-5)) panneau(nel).epsa2=1.d00
   if(panneau(nel).entr2.le.(1.0d-5)) panneau(nel).entr2=panneau(nel).q*dabs(panneau(nel).phil)*pi1

   read(iu_10(iboat),*,end=900) panneau(nel).hya2,panneau(nel).dya2,panneau(nel).wya2,panneau(nel).tya2,panneau(nel).ksa2,panneau(nel).itype_transv_second,panneau(nel).name_type_transv_second
   read(iu_10(iboat),*,end=900) panneau(nel).hxr2,panneau(nel).dxr2,panneau(nel).wxr2,panneau(nel).txr2,panneau(nel).ksr2,panneau(nel).itype_longit_second,panneau(nel).name_type_longit_second

endif

if(itype(nel).ne.5) then
   if((iopti.eq.0).or.(itera.eq.0)) then	!TODO ca ne sert à rien je pense, à supprimer	
      read(iu_10(iboat),*,end=900) mode(nel)
	  panneau(nel).mode=mode(nel)
   else
      read(iu_10(iboat),*,end=900) ! mod_ ! bidon
	  mode(nel)=modes(nel)
	  panneau(nel).mode=mode(nel)			
   endif
else
   !mod_     =  'ee1'
   mode(nel)=  'ee1'
   panneau(nel).mode=mode(nel)

endif

if    ((mode(nel).eq.'ee1'.or.mode(nel).eq.'EE1').or.&
       (mode(nel).eq.'es4'.or.mode(nel).eq.'ES4')) then
                                  xmode(nel)=-1.d00
elseif((mode(nel).eq.'es1'.or.mode(nel).eq.'ES1').or.&
       (mode(nel).eq.'es3'.or.mode(nel).eq.'ES3').or.&
	   (mode(nel).eq.'ec1'.or.mode(nel).eq.'EC1').or.&
	   (mode(nel).eq.'el3'.or.mode(nel).eq.'EL1')) then
                                  xmode(nel)=-5.d-1
elseif(mode(nel).eq.'sl1'.or.mode(nel).eq.'sl2'.or.mode(nel).eq.'el2'.or.	&
		mode(nel).eq.'SL1'.or.mode(nel).eq.'SL2'.or.mode(nel).eq.'EL2') then
   xmode(nel)=+5.d-1
elseif(mode(nel).eq.'ll1'.or.mode(nel).eq.'LL1') then
   xmode(nel)=+1.d00
else
   xmode(nel)= 0.d00
endif
denom(nel) = (1.d00/panneau(nel).entr  + xmode(nel)/(dabs(panneau(nel).phil)*panneau(nel).q*pi1) )
bornestifflargeur(nel,iboat)=(dabs(panneau(nel).phil)*panneau(nel).q*pi1)
bornestiffmode(nel,iboat)=panneau(nel).mode

!if(denom(nel).le.0.0001) then	!<- VRAIE FORMULE ! Mais si on met ça, ça plante parfois dans RECIP sir EPSR n'est pas très très grand !?!
! TODO qd on a un panneau avec des petits raid. => prendre un EPSR très grand artificiellement
if(denom(nel).le.0.0) then
   write(iu_11(iboat),*)
   write(iu_11(iboat),*)'attention : error dans le panneau ',nel
   write(iu_11(iboat),*)'****************************************'
   write(iu_11(iboat),*)'le paramètre mode =',mode(nel),' est incompatible avec'
   write(iu_11(iboat),*)'    l''entredistance entre les raidisseurs=',panneau(nel).entr
   write(iu_11(iboat),*)'    et la largeur du panneau.'
   write(iu_11(iboat),*)'l''écartement entre les raidisseurs est '
   write(iu_11(iboat),*)'probablement supérieur à la largeur(l) du panneau.'
   write(iu_11(iboat),*)'    entr=',panneau(nel).entr,' et mode =',mode(nel)
   write(iu_11(iboat),*)'cela peut induire un epsr erroné (<0 ou infini)!!!'     
   write(iu_11(iboat),*)'il faut corriger cette situation!!'
   write(iu_14(iboat),*)													
   write(iu_14(iboat),*)'attention : error dans le panneau ',nel				
   write(iu_14(iboat),*)'****************************************'			
   write(iu_14(iboat),*)'le paramètre mode =',mode(nel),' est incompatible avec'	
   write(iu_14(iboat),*)'    l''entredistance entre les raidisseurs=',panneau(nel).entr		
   write(iu_14(iboat),*)'    et la largeur du panneau.'						
   write(iu_14(iboat),*)'l''écartement entre les raidisseurs est '			
   write(iu_14(iboat),*)'probablement supérieur à la largeur(l) du panneau.'	
   write(iu_14(iboat),*)'    entr=',panneau(nel).entr,' et mode =',mode(nel)					
   write(iu_14(iboat),*)'cela peut induire un epsr erroné (<0 ou infini)!!!'	
   write(iu_14(iboat),*)'il faut corriger cette situation!!'
	  	  
    mode(nel)='EE2'
	panneau(nel).mode='EE2'
   xmode(nel)= 0.d00
   panneau(nel).epsr= panneau(nel).entr
   denom(nel)= 1.d00/panneau(nel).entr
   
   write(iu_11(iboat),*)'correction automatique: on a'
   write(iu_11(iboat),*)'  mode  =', mode(nel)
   write(iu_11(iboat),*)' xmode  =',xmode(nel)
   write(iu_11(iboat),*)'  entr  =', panneau(nel).entr
   write(iu_11(iboat),*)'  epsr  =', panneau(nel).epsr
   write(iu_11(iboat),*)
endif

panneau(nel).epsr = 1.d00/denom(nel)


if (panneau(nel).entr/(dabs(panneau(nel).phil)*panneau(nel).q*pi1).gt.0.99)  then
   write(iu_11(iboat),*)
   write(iu_11(iboat),*)'attention : error dans le panneau ',nel
   write(iu_11(iboat),*)'****************************************'
   write(iu_11(iboat),*)'le paramètre mode =',mode(nel),' est incompatible avec'
   write(iu_11(iboat),*)'    l''entredistance entre les raidisseurs=',panneau(nel).entr
   write(iu_11(iboat),*)'    et la largeur du panneau.'
   write(iu_11(iboat),*)'l''écartement entre les raidisseurs est '
   write(iu_11(iboat),*)'probablement supérieur à la largeur(l) du panneau.'
   write(iu_11(iboat),*)'    entr=',panneau(nel).entr,' et mode =',mode(nel)
   write(iu_11(iboat),*)'cela peut induire un epsr erroné ou négatif!!!'
   write(iu_11(iboat),*)'epsr=',panneau(nel).epsr,'  !!!'
   write(iu_11(iboat),*)'il faut corriger cette situation!!'
   write(iu_11(iboat),*)
   write(iu_14(iboat),*)													
   write(iu_14(iboat),*)'attention : error dans le panneau ',nel			
   write(iu_14(iboat),*)'****************************************'			
   write(iu_14(iboat),*)'le paramètre mode =',mode(nel),' est incompatible avec'	
   write(iu_14(iboat),*)'    l''entredistance entre les raidisseurs=',panneau(nel).entr		
   write(iu_14(iboat),*)'    et la largeur du panneau.'					
   write(iu_14(iboat),*)'l''écartement entre les raidisseurs est '			
   write(iu_14(iboat),*)'probablement supérieur à la largeur(l) du panneau.'
   write(iu_14(iboat),*)'    entr=',panneau(nel).entr,' et mode =',mode(nel)					
   write(iu_14(iboat),*)'cela peut induire un epsr erroné ou négatif!!!'		
   write(iu_14(iboat),*)'epsr=',panneau(nel).epsr,'  !!!'								
   write(iu_14(iboat),*)'il faut corriger cette situation!!'					
   write(iu_14(iboat),*)															
	  
   mode (nel)='EE2'
   panneau(nel).mode='EE2'
   xmode(nel)=0.
   panneau(nel).epsr=panneau(nel).entr
   write(iu_11(iboat),*)'correction automatique: on a'
   write(iu_11(iboat),*)'  mode  =', mode(nel)
   write(iu_11(iboat),*)'  xmode =',xmode(nel)
   write(iu_11(iboat),*)'  entr  =', panneau(nel).entr
   write(iu_11(iboat),*)'  epsr  =', panneau(nel).epsr
   write(iu_11(iboat),*)
endif

if((iopti.eq.0).or.&
   (itera.eq.0)) then							
   modes(nel)=mode(nel)       
endif																		

if((panneau(nel).epsa2.ge.(1.0d-5)).or.&
   (panneau(nel).entr2.ge.(1.0d-5))) then
    if (panneau(nel).entr2.ne.(dabs(panneau(nel).phil)*panneau(nel).q*pi1)) then !TODO A revérifier (ajouté par Fred le 28/04/2009)
		if (((1.d00/panneau(nel).entr2) + xmode(nel)/(dabs(panneau(nel).phil)*panneau(nel).q*pi1)).lt.1e-6) then
			panneau(nel).epsr2 = dabs(panneau(nel).hight)
		else
			panneau(nel).epsr2=1.d00/ ( 1.d00/panneau(nel).entr2 + xmode(nel)/(dabs(panneau(nel).phil)*panneau(nel).q*pi1) )
		endif
	else
		panneau(nel).epsr2 = panneau(nel).entr2
	endif
else					
   panneau(nel).epsr2 = panneau(nel).entr2		
endif

if(itype(nel).eq.5) goto 648                     !pas de division par 0           
qdelta=panneau(nel).q/panneau(nel).delta
if(qdelta.gt.199.) goto 648
write(iu_11(iboat),649)qdelta

if(qdelta.lt.101.)then
   write(iu_11(iboat),'(/a,i3/15(1h=))')'panneau no ',nel
   write(* ,'(/a,i3/15(1h=))')'panneau no ',nel
   write(iu_14(iboat),'(/a,i3/15(1h=))')'panneau no ',nel							
   write(iu_11(iboat),*)' le rapport limite 1/100 est depasse.',' excecution stoppee. '
   write(iu_14(iboat),*)' le rapport limite 1/100 est depasse.',' excecution stoppee. '								
   write(*,*)' le rayon est ',panneau(nel).q,' et l''épaisseur est ',panneau(nel).delta
   write(*,*)
   write(*,*)' le rapport limite 1/100 est depasse.',' execution stoppee. '
   write(*,*) 'stop'
   read(*,*)													
   iff=1        ! stop après subr. ent
else
   write(iu_11(iboat),'(/a,i3/15(1h=))')'panneau no ',nel
   write(* ,'(/a,i3/15(1h=))')'panneau no ',nel
   write(iu_14(iboat),'(/a,i3/15(1h=))')'panneau no ',nel								
   write(iu_11(iboat),*)' le rapport limite est compris entre 1/100 et 1/200.'
   write(*,*) ' le rapport limite est compris entre 1/100 et 1/200.'
   write(iu_14(iboat),*)' le rapport limite est compris entre 1/100 et 1/200.'		
   write(iu_11(iboat),*)' attention :',' la validite des resultats n''est plus assuree.'
   write(iu_11(iboat),*)' *********  '
   write(iu_14(iboat),*)' attention :',' la validite des resultats n''est plus assuree.'	
   write(iu_14(iboat),*)' *********  '												
   write(*,*) ' attention :',' la validite des resultats n''est plus assuree.'
endif
648 continue

!call update_design_variables()
! ====================================================================================================
! transfer des nouvelles valeurs des variables de conception
! (itération nø 2 et suivante)
! ====================================================================================================

if((iopti.ge.1).and.(itera.gt.0)) then	

        do 123 i=1,nvar(nel,iboat)
          goto(124,125,120,126,127,128,121,129,130),nxit(i,nel,iboat)  
  124     panneau(nel).delta=xicou(i+ntot(iboat),iboat)
          goto 123
  125     panneau(nel).hya=  xicou(i+ntot(iboat),iboat)
          goto 123
  120     panneau(nel).dya=  xicou(i+ntot(iboat),iboat)
          goto 123
  126     panneau(nel).wya=  xicou(i+ntot(iboat),iboat)
          goto 123
  127     panneau(nel).epsa= xicou(i+ntot(iboat),iboat)
          goto 123
  128     panneau(nel).hxr=  xicou(i+ntot(iboat),iboat)
          goto 123
  121     panneau(nel).dxr=  xicou(i+ntot(iboat),iboat)
          goto 123
  129     panneau(nel).wxr=  xicou(i+ntot(iboat),iboat)
          goto 123
  130     panneau(nel).epsr= xicou(i+ntot(iboat),iboat)
          panneau(nel).entr=1.0/( 1.0/panneau(nel).epsr - xmode(nel)/(dabs(panneau(nel).phil)*panneau(nel).q*pi1) )
  123   continue

        panneau(nel).tya=tfa(nel)
        panneau(nel).txr=tfr(nel)
endif

!call read_panel_participation()
! ====================================================================================================
! impression du numero du panneau (titre)
! ====================================================================================================

if(langue.eq.1) then
   write(iu_11(iboat),4)nel,panneau(nel).types
   4    format(/' panneau no ',i3,' (c''est une "',a9,'")'/36(1h=))		
else
   write(iu_11(iboat),'(/a,i3,a,a9,a/36(1h=))')	'panel no ',nel,' (it is a "',panneau(nel).types,'")'
endif

! ====================================================================================================
! lecture du coefficient de participation et du code de position		
! ====================================================================================================

if(itype(nel).ne.5) then
   read(iu_10(iboat),*,end=900) panneau(nel).part,panneau(nel).code,panneau(nel).fami,panneau(nel).lot
endif

!!! Rendement par panneau !!!
if(itype(nel).ne.5) then
	read(iu_10(iboat),*,end=900) panneau(nel).rend
	if (panneau(nel).rend.eq.0) then
		if (langue.eq.1) then
			write(*,'(a,i3,a)') 'Attention ! Parametre rend du panneau',nel,' est nul !!!'
		elseif (langue.eq.2) then
			write(*,'(a,i3,a)') 'Attention! Parameter rend of the panel',nel,' is equal to 0!!!'
		endif
	endif	
endif

!call read_panel_vibration(nel)
! ====================================================================================================
! lecture des paramètres pour l'étude en vibration		
! ====================================================================================================

if(itype(nel).eq.2) then !uniquement les PANNEAUX
   read(iu_10(iboat),*,end=900) panneau(nel).elmt_vib,panneau(nel).long_vib,	&
								panneau(nel).cl_av_vib,panneau(nel).cl_ar_vib
endif

!call read_panel_properties()
! ====================================================================================================
! lecture des proprietes structurelles (module de young, limite elastique, coefficient de poisson, ...)
! ====================================================================================================

read(iu_10(iboat),*,end=900) indMateriau(nel)	!Spécifie quel matériau est utilisé pour ce panneau

if(langue==1) write(iu_11(iboat),420) indMateriau(nel)
if(langue==2) write(iu_11(iboat),421) indMateriau(nel)

if(itype(nel).eq.5)then
   sh=e(indMateriau(nel))/(2.d00*(1.d00+0.d00))
else
   sh=e(indMateriau(nel))/(2.d00*(1.d00+eta(indMateriau(nel))))
endif

read(iu_10(iboat),*,end=900) panneau(nel).mt,panneau(nel).ksr,panneau(nel).ksa,panneau(nel).kst,panneau(nel).kse,ipa(nel),ivaria(nel)
read(iu_10(iboat),*,end=900) panneau(nel).angle

!call read_panel_loads()
! ====================================================================================================
! charges xi,xf
! ====================================================================================================

 xi(:,nel)=0.d00
 xf(:,nel)=0.d00
xxi(:,nel)=0.d00
xxf(:,nel)=0.d00

is=0
do 195 i=1,nsolm
   icc=0
   do k=1,nsol
      if (nnsol(k).eq.i) icc=1  ! icc=1 cas de charge retenu
   enddo
   if (icc.eq.0) then           ! icc=0 cas de charge pas retenu
      read(iu_10(iboat),*,end=900) panneau(nel).xi_full(i),panneau(nel).xf_full(i) !on enregistre qd même la valeur pour pouvoir créer les fichiers up
      goto 195
   else
      is=is+1
   endif
   if(ivaria(nel).eq.1) goto 191
   read(iu_10(iboat),*,end=900) panneau(nel).xi_full(i),panneau(nel).xf_full(i)
   xi(is,nel) = panneau(nel).xi_full(i)
   xf(is,nel) = panneau(nel).xf_full(i)
   goto 192
   191   read(iu_10(iboat),*,end=900) panneau(nel).xi_full(i),panneau(nel).xf_full(i)
   xi(is,nel) = panneau(nel).xi_full(i)
   xf(is,nel) = panneau(nel).xf_full(i)
   192   panneau(nel).ang=panneau(nel).angle
   if(ivaria(nel).eq.0) then                     ! pression variant avec la prof.
      if(panneau(nel).types.ne.'coque') then     ! plaque et epontille             
         xf(is,nel)=xi(is,nel)+dabs(panneau(nel).hight)*dsin(panneau(nel).angle*pi1)
         if(i.eq.nsolm) ivaria(nel)=1
      else									     ! coque
         teta=90.d00-panneau(nel).angle          ! calculé pour subr hughes
         xf(is,nel)=xi(is,nel)+panneau(nel).q*(dsin((teta-panneau(nel).phil)*pi1)-dsin(teta*pi1)) 
     endif
   endif
   xxi(is,nel)=xi(is,nel)
   xxf(is,nel)=xf(is,nel)
195 continue

read(iu_10(iboat),*,end=900) panneau(nel).ploc

! ====================================================================================================
! panneau modifie car phil>0 n'est pas compatible avec hydros (isign=-1)
! ====================================================================================================

if(panneau(nel).phil.le.0.0001) goto 80

teta      =-panneau(nel).angle-90.d00
nsign(nel)=-1

do 196 i=1,nsol
  if(ivaria(nel).eq.0) goto 193
  xi(i,nel)=xf(i,nel)
  xf(i,nel)=xxi(i,nel)
  goto 194
  193   xi(i,nel)=xi(i,nel)+panneau(nel).q*(dsin((teta-panneau(nel).phil)*pi1)-dsin(teta*pi1))
        xf(i,nel)=xxi(i,nel)              ! calculé pour subr hughes
  194   continue
196 continue

if(panneau(nel).angle.lt.-0.00001) goto 751
if(panneau(nel).angle.gt.0.00001)  goto 752

panneau(nel).angle=180.d00
goto 753

751  panneau(nel).angle=panneau(nel).angle+180.d00
     goto 753
752  panneau(nel).angle=panneau(nel).angle-180.d00
753  continue
     if(panneau(nel).types.ne.'coque') goto 81
     teta=teta-panneau(nel).phil
     panneau(nel).angle=panneau(nel).angle+panneau(nel).phil
     goto 81
80   teta=-panneau(nel).angle+90.d00
     nsign(nel)=1
81   continue

!call check_panel_coord()
! ====================================================================================================
! test sur teta (-180. < teta < 180. )
! ====================================================================================================

if(dabs(teta).lt.180.d00) goto 721
if(teta.ge.0.d00)         goto 722
teta=teta+360.d00
goto 721

722  teta=teta-360.d00
721  continue

! ====================================================================================================
! mise en memoire pour la subroutine equil
! ====================================================================================================

panneau(nel).teta = teta !tetaq(nel)=teta

! ====================================================================================================
! mise en memoire pour le calcul des coordonnees.
! ====================================================================================================

if(panneau(nel).phil.le.0) then
  panneau(nel).tetas = -panneau(nel).ang+90.d00 !tetas(nel)=-ang+90.d00
else
  panneau(nel).tetas = -panneau(nel).ang-90.d00 !tetas(nel)=-ang-90.d00
endif

call print_panel_data(nel) ! à déplacer en dessous des lectures

! call read_panel_distributed_loads()
! ====================================================================================================
! lecture des charges variables selon ox
! ====================================================================================================

read(iu_10(iboat),*,end=900) panneau(nel).icha

if((panneau(nel).icha).eq.0) goto 210

is=0
do 215 i=1,nsolm
   icc=0
   do k=1,nsol
      if (nnsol(k).eq.i) icc=1       ! icc=1 cas de charge retenu
   enddo
   if (icc.eq.0) then                      ! icc=0 cas de charge pas retenu
      read(iu_10(iboat),*,end=900) title_cha_full(i,nel) ! titre
      read(iu_10(iboat),*,end=900) npt_full(i,nel)   ! npt
      do j=1,npt_full(i,nel)
         read(iu_10(iboat),*,end=900) (cha_full(j,ij,i,nel),ij=1,3)
      enddo
      goto 215
   else
      is=is+1
   endif

   read(iu_10(iboat),*) title_cha_full(i,nel)
   title_loadcase(is,nel) = title_cha_full(i,nel) ! ligne titre, ex: "cas de charge 3"
   read(iu_10(iboat),*,end=900) npt_full(i,nel)
   npt(is,nel)= npt_full(i,nel)
   write(iu_11(iboat),212) nnsol(is),npt(is,nel)

   if(npt(is,nel).gt.100) then
      write(*,*)  ' *** error in panel ',nel
      write(*,*)  ' charges variables selon ox (max=100 pas)'
      write(iu_14(iboat),*)  ' *** error in panel ',nel					
      write(iu_14(iboat),*)  ' charges variables selon ox (max=100 pas)'	
 	  write(iu_31(iboat),*)  ' charges variables selon ox (max=100 pas)'
      write(iu_31(iboat),*)  ' load distributed along ox (max=100 steps)'
      iff=1
	  write(*,*) 'error'
	  read(*,*)
   endif

   write(iu_11(iboat),213)

   do j=1,npt(is,nel)
      read(iu_10(iboat),*,end=900)  (cha_full(j,ij,i,nel),ij=1,3)
	  cha(j,:,is,nel) = cha_full(j,:,i,nel)
   enddo

   if(panneau(nel).kse.eq.0) then
      panneau(nel).kse=2
      xi(is,nel)=0.d00
      xf(is,nel)=0.d00
      call annuld(cha(1,2,is,nel),nsol)
      call annuld(cha(1,3,is,nel),nsol)

      write(iu_11(iboat),218) nel
	  write(iu_14(iboat),218) nel										

      write(iu_11(iboat),7)   nnsol(is),xi(is,nel),xf(is,nel)
      write(*,*) 'panneau n° ',nel
      write(*,*) ' kse=0 est incompatible avec icha=1'
      write(*,*) ' nous proposons de prendre kse=2 avec'
      write(*,*) '       xi=xf=0  et  cha2=cha3=0 '
      write(*,*)' si oui taper "0"'
      write(*,*)' si non taper "1" '
      write(*,*)'    (c.à.d stop + corriger vos données)'
      read (*,*) iff

	  write(iu_14(iboat),*) 'panneau n° ',nel											
      write(iu_14(iboat),*) ' kse=0 est incompatible avec icha=1'			
      write(iu_14(iboat),*) ' nous proposons de prendre kse=2 avec'		
      write(iu_14(iboat),*) '       xi=xf=0  et  cha2=cha3=0 '				
	  write(iu_14(iboat),*) '    (c.à.d stop + corriger vos données)'	
!	  iff=1        
	  endif
	  do j=1,npt(is,nel)
          write(iu_11(iboat),214) j,cha(j,1,is,nel),cha(j,2,is,nel),cha(j,3,is,nel)
        enddo
 215  continue

210  continue



!call read_following_panels()
! ====================================================================================================
! lecture des paneaux suivants
! ====================================================================================================

read(iu_10(iboat),*,end=900) (noh(nel,j),j=1,10)
panneau(nel).noh9(:)=noh(nel,:)

write(iu_11(iboat),*)
if(langue==1) then
   write(iu_11(iboat),*) 'liste des panneaux (noh) qui suivent celui-ci: '
else
   write(iu_11(iboat),*) 'list of the panels (noh) that follows this one:'
endif

write(iu_11(iboat),*)    '------------------------------------------------'
write(iu_11(iboat),'(10i3)') (noh(nel,j),j=1,10)


! call read_girders()
! ====================================================================================================
! lectures des traverses
! ====================================================================================================

if(panneau(nel).mt.eq.0) goto 41
do 2001 i=1,panneau(nel).mt
   read(iu_10(iboat),*,end=900) panneau(nel).hxtr(i),panneau(nel).dxtr(i),panneau(nel).wxtr(i),	&
								panneau(nel).txtr(i),panneau(nel).abtr(i)
   if ((panneau(nel).abtr(i)-dabs(panneau(nel).phil)*panneau(nel).q*pi1).gt.0.0001.or.panneau(nel).abtr(i).lt.0) then           
      write(*,*)                                                    
      write(*,*)'attention : error dans le panneau ',nel            
      write(*,*)'****************************************'          
      write(*,*)'une traverse n''est pas positionnée sur le panneau'
      write(*,*)'vérifiez vos données'                              
      
	    write(iu_14(iboat),*)															
	    write(iu_14(iboat),*)'attention : error dans le panneau ',nel					
	    write(iu_14(iboat),*)'****************************************'					
	    write(iu_14(iboat),*)'une traverse n''est pas positionnée sur le panneau'
	    write(iu_14(iboat),*)'vérifiez vos données'					
	    read(*,*)   
	    stop                                                  
   endif                                             
   panneau(nel).abtr2(i)=panneau(nel).abtr(i)
2001 continue
if(langue==1) write(iu_11(iboat),39)
if(langue==2) write(iu_11(iboat),909)


do i=1,panneau(nel).mt
   write(iu_11(iboat),441) i,panneau(nel).abtr(i),panneau(nel).txtr(i),	&
							panneau(nel).wxtr(i),panneau(nel).dxtr(i),	&
							panneau(nel).hxtr(i)
   panneau(nel).abtr(i)=panneau(nel).abtr(i)/(pi1*panneau(nel).q)
   if(panneau(nel).phil.gt.0.) panneau(nel).abtr(i)=panneau(nel).phil-panneau(nel).abtr(i)
enddo


41  continue

!   call read_fatigue_parameters()
if (itype(nel).ne.5) then

	!read(iu_10(iboat),*)			! Lecture de Fatigue_Stiffeners
	read(iu_10(iboat),*) panneau(nel).Kcor				!fat_new
	read(iu_10(iboat),*) panneau(nel).I_Fat_Stif
	if (panneau(nel).I_Fat_Stif .NE. 0) THEN			!fat_new
	read(iu_10(iboat),*) panneau(nel).nbr_Fat_Stif
	do i=1,panneau(nel).nbr_Fat_Stif
		read(iu_10(iboat),*) panneau(nel).ix_Stif(i),panneau(nel).iy_Stif(i),panneau(nel).ES_Stif(i),					&
			panneau(nel).KT_Stif(i),panneau(nel).lambda_stif(i),panneau(nel).teta_stif(i),panneau(nel).BetaIf_stif(i),	&
			panneau(nel).m_stif(i),panneau(nel).c_stif(i),panneau(nel).weib_fact_stif(i)
	enddo
	endif			!fat_new

	!read(iu_10(iboat),*)			! Lecture de Fatigue_Plate
	read(iu_10(iboat),*) panneau(nel).I_Fat_Plate
	if (panneau(nel).I_Fat_Plate .NE. 0) THEN			!fat_new
	read(iu_10(iboat),*) panneau(nel).nbr_Fat_Plate
	do i=1,panneau(nel).nbr_Fat_Plate
		read(iu_10(iboat),*) panneau(nel).ix_Plate(i),panneau(nel).iy_Plate(i), & !panneau(nel).ES_Plate(i),	&
			panneau(nel).KT_Plate(i),panneau(nel).lambda_Plate(i),panneau(nel).teta_Plate(i),				&
			panneau(nel).BetaIf_Plate(i),panneau(nel).m_Plate(i),panneau(nel).c_Plate(i),					&
			panneau(nel).weib_fact_Plate(i)
	enddo
	endif			!fat_new

	!read(iu_10(iboat),*)			! Lecture de Fatigue_Plate_Frame
	read(iu_10(iboat),*) panneau(nel).I_Fat_Plate_Frame
	if (panneau(nel).I_Fat_Plate_Frame .NE. 0) THEN			!fat_new
	read(iu_10(iboat),*) panneau(nel).nbr_Fat_Plate_Frame
	do i=1,panneau(nel).nbr_Fat_Plate_Frame
		read(iu_10(iboat),*) panneau(nel).ix_Plate_Frame(i),panneau(nel).iy_Plate_Frame(i),						&
			panneau(nel).KT_Plate_Frame(i),panneau(nel).lambda_Plate_Frame(i),	&
			panneau(nel).teta_Plate_Frame(i),panneau(nel).BetaIf_Plate_Frame(i),panneau(nel).m_Plate_Frame(i),	&
			panneau(nel).c_Plate_Frame(i),panneau(nel).weib_fact_Plate_Frame(i)
	enddo
	endif			!fat_new

	!read(iu_10(iboat),*)			! Lecture de Fatigue_Frame
	read(iu_10(iboat),*) panneau(nel).I_Fat_Frame
	if (panneau(nel).I_Fat_Frame .NE. 0) THEN			!fat_new
	read(iu_10(iboat),*) panneau(nel).nbr_Fat_Frame
	do i=1,panneau(nel).nbr_Fat_Frame
		read(iu_10(iboat),*) panneau(nel).ix_Frame(i),panneau(nel).iy_Frame(i),panneau(nel).ES_Frame(i),	&
			panneau(nel).KT_Frame(i),panneau(nel).lambda_Frame(i),panneau(nel).teta_Frame(i),				&
			panneau(nel).BetaIf_Frame(i),panneau(nel).m_Frame(i),panneau(nel).c_Frame(i),					&
			panneau(nel).weib_fact_Frame(i)
	enddo
	endif			!fat_new


!STIFFENERS--------------------------------------------------------------
!1								I_Fat_Stif: if 1 :fatigue assessment for stiffeners and not if 0 
!2								number of assessment points
!3  1  1  1.7  2.15  45  2.2  3  2400000000000  3		ix,iy,Es,Kt,LAMBDA,TETA,BetaIF,m,C,Weib_fact
!3  3  1  1.7  2.15  45  2.2  3  2400000000000  3 		ix,iy,Es,Kt,LAMBDA,TETA,BetaIF,m,C,Weib_fact
!Plates-----------------------------------------------------------------
!1								I_Fat_Plate: if 1 :fatigue assessment for plate and not if 0 
!2								number of assessment points
!1  1  1.7  2.15  45  2.2  3  2400000000000  3 			ix,iy,Kt,LAMBDA,TETA,BetaIF,m,C,Weib_fact
!1  3  1.7  2.15  45  2.2  3  2400000000000  3			ix,iy,Kt,LAMBDA,TETA,BetaIF,m,C,Weib_fact
!Intersection Plate/frame------------------------------------------------
!1								I_Fat_Plate: if 1 :fatigue assessment for plate and not if 0 
!2								number of assessment points!
!1  1  1.7  2.15  45  2.2  3  2400000000000  3 			ix,iy,Kt,LAMBDA,TETA,BetaIF,m,C,Weib_fact	 
!1  3  1.7  2.15  45  2.2  3  2400000000000  3			ix,iy,Kt,LAMBDA,TETA,BetaIF,m,C,Weib_fact
!Frames-----------------------------------------------------------------
!1								I_Fat_Plate: if 1 :fatigue assessment for plate and not if 0 
!2								number of assessment points
!3  1  1  1.7  2.15  45  2.2  3  2400000000000  3		ix,iy,Es,Kt,LAMBDA,TETA,BetaIF,m,C,Weib_fact
!3  1  1  1.7  2.15  45  2.2  3  2400000000000  3		ix,iy,Es,Kt,LAMBDA,TETA,BetaIF,m,C,Weib_fact
!Girders-----------------------------------------------------------------
!2								MT
!1								I_Fat_Plate: if 1 :fatigue assessment for plate and not if 0 
!0.013000  2.800000  						GIRDERS longitudinaux (m)! TXTR : épaisseur semelle girder (traverses)	!ABTR : position of the girder with reference to the departure node (in meter)
!2								number of assessment points
!1  1  1.7  2.15  45  2.2  3  2400000000000  3 			ix,Es,Kt,LAMBDA,TETA,BetaIF,m,C,Weib_fact	
!3  3  1.7  2.15  45  2.2  3  2400000000000  3			ix,Es,Kt,LAMBDA,TETA,BetaIF,m,C,Weib_fact
!1								I_Fat_Plate: if 1 :fatigue assessment for plate and not if 0 
!0.013000  2.800000  						GIRDERS longitudinaux (m)! TXTR : épaisseur semelle girder (traverses)	!ABTR : position of the girder with reference to the departure node (in meter)
!2								number of assessment points
!1  1  1.7  2.15  45  2.2  3  2400000000000  3 			ix,Es,Kt,LAMBDA,TETA,BetaIF,m,C,Weib_fact	
!3  3  1.7  2.15  45  2.2  3  2400000000000  3			ix,Es,Kt,LAMBDA,TETA,BetaIF,m,C,Weib_fact


!
!             read(iu_10(iboat),*) panneau(nel).i_fatigue_stiffened_panel(1), &
!			                      panneau(nel).i_fatigue_stiffened_panel(2), & 
!			                      panneau(nel).i_fatigue_stiffened_panel(3), & 
!			                      panneau(nel).i_fatigue_stiffened_panel(4) !fat_r(nel)
!
!             if(((panneau(nel).i_fatigue_stiffened_panel(1)).eq.1).or.   &
!                ((panneau(nel).i_fatigue_stiffened_panel(2)).eq.1).or.   &
!                ((panneau(nel).i_fatigue_stiffened_panel(3)).eq.1).or.   &
!                ((panneau(nel).i_fatigue_stiffened_panel(4)).eq.1)) then
!		        panneau(nel).nfatiguem=1
!             endif
!
!             read(iu_10(iboat),*) panneau(nel).i_fatigue_frame              !fat_a(nel)
!
!			 if((panneau(nel).i_fatigue_frame.eq.1)) then	!frame
!                panneau(nel).nfatiguem=1
!             endif
!
!			 read(iu_10(iboat),*) panneau(nel).i_fatigue_girder             !fat_g
!
!             if((panneau(nel).i_fatigue_girder.eq.1)) then		! girder
!                panneau(nel).nfatiguem=1      
!             endif

!			 read(iu_10(iboat),*) panneau(nel).number_points_considered
!
!			 if(((panneau(nel).nfatiguem).eq.1).and.((panneau(nel).number_points_considered).eq.0)) then
!                write(* ,'(a,i4,a)')'Calcul de fatigue demande pour le panneau',nel, &
!				                  '  alors qu on ne le calcule en aucun point'
!                write(*,*) 'stop'
!				stop
!
!			 endif
!
!			 do j=1,panneau(nel).number_points_considered
!			    read(iu_10(iboat),*) panneau(nel).ix_stiffened_pannel(j), &
!			                         panneau(nel).iy_stiffened_pannel(j)
!			 enddo
!
!             panneau(nel).number_points_considered=1
!!			 do j=1,panneau(nel).number_points_considered
!!			    panneau(nel).ix_stiffened_pannel(j)=3
!!			    panneau(nel).iy_stiffened_pannel(j)=1
!!			 enddo
!!
!             read(iu_10(iboat),*) panneau(nel).weib_fact								! parametre ayant des valeurs 1 à 4 qui permet le calcul du "weibull shape parameter"
!			   											                     	        ! 1:for deck longitudinals,2:for ship side,3:for bottom longitudinals,4:for longitudinal and transverse bulkheads
!             if((panneau(nel).mt).ne.0) then                                              ! pas de traverse(girder) dans le panneau
!                do mmt=1,panneau(nel).mt
!     	           read(iu_10(iboat),*) panneau(nel)%epstr(mmt)					        !girder breadth (m)
!	               read(iu_10(iboat),*) panneau(nel)%girder_length(mmt)			        !girder length  (m)
!                enddo
!             endif
!
!             read(iu_10(iboat),*) panneau(nel).ptyr,panneau(nel).esr,panneau(nel).lbr,panneau(nel).wbr,panneau(nel).tbr    !stiffener: type of profile, end type ,length of bracket(mm) , width of bracket(mm) , thickness of bracket(mm) 
!             read(iu_10(iboat),*) panneau(nel).ptya,panneau(nel).esa,panneau(nel).lba,panneau(nel).wba,panneau(nel).tba    !frame :	type of profile, end type ,length of bracket(mm) , width of bracket(mm) , thickness of bracket(mm) 
!
!             if((panneau(nel).mt).ne.0) then                                                          ! pas de traverse(girder) dans le panneau
!                do mmt=1,panneau(nel).mt
!                   read(iu_10(iboat),*) panneau(nel)%ptyg(mmt),panneau(nel)%esg(mmt),panneau(nel)%lbg(mmt),panneau(nel)%wbg(mmt),panneau(nel)%tbg(mmt)		!girder :	type of profile, end type ,length of bracket(mm) , width of bracket(mm) , thickness of bracket(mm) 
!                enddo
!             endif
!
!
!	         read(iu_10(iboat),*) panneau(nel).kt_hs_1r,panneau(nel).kt_hs_2r,panneau(nel).kt_hs_3r,panneau(nel).kt_hs_4r	!HOT SPOT FACTOR (STIFFENERS)
!	         read(iu_10(iboat),*) panneau(nel).kt_hs_1a,panneau(nel).kt_hs_2a,panneau(nel).kt_hs_3a,panneau(nel).kt_hs_4a	!HOT SPOT FACTOR (FRAME)
!
!             if((panneau(nel).mt).ne.0) then                                                          ! pas de traverse(girder) dans le panneau
!                do mmt=1,panneau(nel).mt
!	               read(iu_10(iboat),*) panneau(nel)%kt_hs_1g(mmt),panneau(nel)%kt_hs_2g(mmt),panneau(nel)%kt_hs_3g(mmt),panneau(nel)%kt_hs_4g(mmt)	!HOT SPOT FACTOR (GIRDER)			
!	            enddo
!             endif
!
!
!             read(iu_10(iboat),*) panneau(nel).betaif		                                !improvement fatigue life factor for grind technique, generaly to be taken equal to 2,2					
!             read(iu_10(iboat),*) panneau(nel).kcor                                        !corrosion factor							
!            !read(iu_10(iboat),*) panneau(nel).th                                          !h_weibull             		weibull shape parameter
!            !read(iu_10(iboat),*) panneau(nel).tn                                          !ntcyc				  	    number of load cycles during design life
!             read(iu_10(iboat),*) panneau(nel).tm                                          !m          	  	        slope of s-n curve; commonly m=3
!             read(iu_10(iboat),*) panneau(nel).tc                                          !c   (mpa^m)		        fatigue capacity
!!             read(iu_10(iboat),*) panneau(nel).lambda_weld                                 !coefficient depending on the weld configuration	new5
!             read(iu_10(iboat),*) panneau(nel).toe_angle                                   !mean weld toe angle     
!             read(iu_10(iboat),*) panneau(nel).lambda_weld                                 !coefficient depending on the weld configuration		new5


endif

m5cont(nel)=panneau(nel).nfatiguem ! Correction fred (indispensable)

!if(impr2.le.-1.and.IS_FATIGUE.gt.0) then	!fat_new
 !  write (* ,*)'impr2 doit etre plus grand que -1 pour faire un calcul fatigue !!!'	!fat_new
  ! write (*, *) 'stop - Press Enter to resum'	!fat_new
   !read (*,*)	!fat_new
  ! stop	!fat_new

!endif	!fat_new



!fat_new	
!DO I=1,4
!	if ((panneau(nel).i_fatigue_stiffened_panel(I).eq.1).AND.(panneau(nel).toe_angle(I).EQ.0))THEN			!NEW10
!		write(*,10) 'input value error: TETA(PANEL=',nel,'), Hot Spot',I,'=0' 			!NEW10
!		write(*,*) 'stop'										!NEW10
!		read(*,*)
!		stop												!NEW10
!	endif													!NEW10

!	if ((panneau(nel).i_fatigue_stiffened_panel(I).eq.1).AND.(panneau(nel).lambda_weld(I).EQ.0))THEN			!NEW10
!		write(*,11) 'input value error: LAMBDA(PANEL=',nel_fatigue,'), Hot Spot',I,'=0' 			!NEW10
!		write(*,*) 'stop'									!NEW10
!		read(*,*)										    !NEW10
!		stop												!NEW10
!	endif													!NEW10
!
!ENDDO
 

! ====================================================================================================
! check panel angles and computes edge force coefficients 
! ====================================================================================================
!call comp_edge_forces_coeff()
! ====================================================================================================
! phil=dabs(phil)
! ====================================================================================================

phil=dabs(panneau(nel).phil)

! ====================================================================================================
! les 4 cas de base pour les effets de bords a(y**3)+b(y**2)+c*y+d
! ====================================================================================================

qphil=panneau(nel).q*phil*pi1
ddd=1.0d03
ccc=ddd/qphil
bbb=ccc/qphil
aaa=bbb/qphil

delto=width*5.0d-2
if(ibusc.ne.0) then
   write(iu_11(iboat),6) delto
endif

! ====================================================================================================
! read design variables
! ====================================================================================================
!call read_design_variables()
! ====================================================================================================
! lecture de variables de conception , xi, ximax et ximin
! ====================================================================================================

    read(iu_10(iboat),*,end=900) title                               ! titre
    read(iu_10(iboat),*,end=900) nvar(nel,iboat)                     ! nbrxi
    
    
	if(nvar(nel,iboat).ne.0) read(iu_10(iboat),*,end=900) (nxit(i,nel,iboat),i=1,nvar(nel,iboat))	
    continue
    
	if(itera.eq.0)then		!TODO Vérifier si il faut laisser cette condition							
	   read (iu_10(iboat),*,end=900) (panneau(nel).dvmin(i),i=1,9)		
	   read (iu_10(iboat),*,end=900) (panneau(nel).dvmax(i),i=1,9)			
	   !if (nel.eq.1) write(*,*) 'LECTURE dvmax panneau 1'
	   	   bornestiffmin(nel,iboat)=panneau(nel).dvmin(9)
	       bornestiffmax(nel,iboat)=panneau(nel).dvmax(9)
	       bornestiff(nel,iboat)=panneau(nel).entr
	   do i=1,nvar(nel,iboat)														
	      ximin(ntot(iboat)+i,iboat)=panneau(nel).dvmin(nxit(i,nel,iboat))
	      ximax(ntot(iboat)+i,iboat)=panneau(nel).dvmax(nxit(i,nel,iboat))				
	   enddo
													
	else	
													
       ! itera>1 : on conserve les ximin et ximax modifiés dans subr. opti
       read (iu_10(iboat),*,end=900) temp_                           ! xi min
       read (iu_10(iboat),*,end=900) temp_                           ! xi max	
	   ! ce qui suit est oblig, sinon dans Geom les dvmin et dvmax peuvent être nuls
	   do i=1,nvar(nel,iboat)														
	      panneau(nel).dvmin(nxit(i,nel,iboat)) = ximin(ntot(iboat)+i,iboat)
	      panneau(nel).dvmax(nxit(i,nel,iboat)) = ximax(ntot(iboat)+i,iboat)
	      if (nxit(i,nel,iboat) .eq. 9) then
	      panneau(nel).dvmin(nxit(i,nel,iboat))=bornestiffmin(nel,iboat)
	      panneau(nel).dvmax(nxit(i,nel,iboat))=bornestiffmax(nel,iboat)
	      endif
	   enddo
	   if (panneau(nel).mode .EQ. 'EE2') then
	   bornestiff(nel,iboat)=panneau(nel).epsr
	   else 
	   bornestiff(nel,iboat)= panneau(nel).entr 
	   endif
	endif						
	
	if(iopti.eq.0) nvar(nel,iboat)=0

! ====================================================================================================
! vérification des variables de conception : ximin < xi < ximax
! et sauvetage des variables de conception dans le vecteur xicou(n)
! (lors de la première itération)
! ====================================================================================================
    if((itera.eq.0).and.(iopti.ge.1)) then

       write(iu_31(iboat),283)       nel,nvar(nel,iboat)
       write(iu_31(iboat),'(a,9i2)') 'soit les variables locales:',(nxit(i,nel,iboat),i=1,nvar(nel,iboat))
       do 131 i=ntot(iboat)+1,ntot(iboat)+nvar(nel,iboat)
          if(itype(nel).ne.5) then                                   !si plaques,coques        
             goto(134,135,141,136,137,138,142,139,140),nxit(i-ntot(iboat),nel,iboat)
  134        call borne(panneau(nel).delta,i,'= épaisseur bordage          ', nel,ximin(i,iboat),ximax(i,iboat),iff)
             xicou(i,iboat)=panneau(nel).delta
             goto 131
  135        call borne(panneau(nel).hya,i,  '= hauteur âme transversaux   ', nel,ximin(i,iboat),ximax(i,iboat),iff)
             xicou(i,iboat)=panneau(nel).hya
             goto 131
  141        call borne(panneau(nel).dya,i,  '= épaisseur âme transversaux ', nel,ximin(i,iboat),ximax(i,iboat),iff)
             xicou(i,iboat)=panneau(nel).dya
             goto 131
  136        call borne(panneau(nel).wya,i,  '= largeur semelle transvers. ', nel,ximin(i,iboat),ximax(i,iboat),iff)
             xicou(i,iboat)=panneau(nel).wya
             goto 131
  137        call borne(panneau(nel).epsa,i, '= larg. effect. transversaux ', nel,ximin(i,iboat),ximax(i,iboat),iff)
             xicou(i,iboat)=panneau(nel).epsa
             goto 131
  138        call borne(panneau(nel).hxr,i,  '= hauteur âme raidisseurs    ', nel,ximin(i,iboat),ximax(i,iboat),iff)
             xicou(i,iboat)=panneau(nel).hxr
             goto 131
  142        call borne(panneau(nel).dxr,i,  '= épaisseur âme raidisseurs  ', nel,ximin(i,iboat),ximax(i,iboat),iff)
             xicou(i,iboat)=panneau(nel).dxr
             goto 131
  139        call borne(panneau(nel).wxr,i,  '= largeur semelle raidisseurs', nel,ximin(i,iboat),ximax(i,iboat),iff)
             xicou(i,iboat)=panneau(nel).wxr
             goto 131
  140        continue  
             ! entredistance entre raidisseurs entr et epsr
             write(iu_31(iboat),'(a,i3,a)') 'var. nø',i,': entr et epsr'
             write(iu_31(iboat),'(a,e11.4,a,e11.4,a,e11.4)')                &
                                'pour entr, les limites min et max sont:',  &
                                 ximin(i,iboat),' < entr=',panneau(nel).entr,' < ',ximax(i,iboat)    
             write(iu_31(iboat),*) 'mode =',mode(nel),' et xmode =',xmode(nel)
             temp_= xmode(nel)/(dabs(panneau(nel).phil)*panneau(nel).q*pi1)
             denom1 = (1.0/ximax(i,iboat)  + temp_ )
             denom2 = (1.0/ximin(i,iboat)  + temp_ )
             if((denom1.lt.0.000).or.(denom2.le.0.000)) then
                write(iu_31(iboat),*)
                write(iu_31(iboat),*) 'attention : error dans le panneau ',nel
                write(iu_31(iboat),*) '****************************************'
                write(iu_31(iboat),*) 'le paramètre mode est incompatible avec les'
                write(iu_31(iboat),*) 'bornes de variation de l''entredistance entre'
                write(iu_31(iboat),*) 'les raidisseurs et la largeur du panneau.'
                write(iu_31(iboat),*) 'vous devez corriger cette situation !!'
                write(iu_31(iboat),*) 'essayer en modifiant le paramère mode'
                write(iu_31(iboat),*) ' - actuellement on a mode =',mode(nel)
                write(iu_31(iboat),*) ' - prenez plutôt     mode = EE2'
                write(*,*)
                write(*,*)'attention : error dans le panneau ',nel
                write(*,*)'****************************************'
                write(*,*)'le paramètre mode est incompatible avec les'
                write(*,*)'bornes de variation de l''entredistance entre'
                write(*,*)'les raidisseurs et la largeur du panneau.'
                write(*,*)'vous devez corriger cette situation !!'
                write(*,*)'essayer en modifiant le paramère mode'
                write(*,*)' - actuellement on a mode =',mode(nel)
                write(*,*)' - prenez plutôt     mode = EE2'
            
		        write(iu_14(iboat),*)														
	            write(iu_14(iboat),*) 'attention : error dans le panneau ',nel				
	            write(iu_14(iboat),*) '****************************************'				
                write(iu_14(iboat),*) 'le paramètre mode est incompatible avec les'				
	            write(iu_14(iboat),*) 'bornes de variation de l''entredistance entre'			
	            write(iu_14(iboat),*) 'les raidisseurs et la largeur du panneau.'			
	            write(iu_14(iboat),*) 'vous devez corriger cette situation !!'				
	            write(iu_14(iboat),*) 'essayer en modifiant le paramère mode'				
	            write(iu_14(iboat),*) ' - actuellement on a mode =',mode(nel)						
	            write(iu_14(iboat),*) ' - prenez plutôt     mode = EE2'						
	      
		        write(*,*) 'error'
				read(*,*)																	
		        iff=1

             endif
             if (denom1.eq.0.) then
                write(iu_31(iboat),*)
                write(iu_31(iboat),*) 'attention : error dans le panneau ',nel
                write(iu_31(iboat),*) '****************************************'
                write(iu_31(iboat),*) 'la borne maxi de epsr est incompatible avec'
                write(iu_31(iboat),*) 'le parametre mode et la largeur du panneau.'
                write(iu_31(iboat),*) 'vous devez corriger cette situation !!'
                write(iu_31(iboat),*) 'essayer en modifiant le paramère mode'
                write(iu_31(iboat),*)' - on avait       ximax =',ximax(i,iboat)
                write(*,*)  'attention : error dans le panneau ',nel
                write(*,*)  '****************************************'
                write(*,*)  ' - on avait       ximax =',ximax(i,iboat)
                ximax(i,iboat)=ximax(i,iboat)*0.99
                denom1 = (1.0/ximax(i,iboat)  + temp_ )
                write(iu_31(iboat),*)' - on change pour ximax =',ximax(i,iboat)
                write(*,*)  ' - on change pour ximax =',ximax(i,iboat)
          
		        write(iu_14(iboat),*)													
	            write(iu_14(iboat),*)'attention : error dans le panneau ',nel			
	            write(iu_14(iboat),*)'****************************************'				
                write(iu_14(iboat),*)'la borne maxi de epsr est incompatible avec'			
	            write(iu_14(iboat),*)'le parametre mode et la largeur du panneau.'				
	    
	 	     endif

             write(iu_31(iboat),*) 'pour epsr (vrai variable de conception), on a:'
              ximax(i,iboat)= 1.0/ denom1 
              ximin(i,iboat)= 1.0/ denom2 
             call borne(panneau(nel).epsr,i, '= larg. effect. raidisseurs  ', nel,ximin(i,iboat),ximax(i,iboat),iff)
             xicou(i,iboat)=bornestiff(nel,iboat)
          else
             goto(143,144,145,146,147),nxit(i-ntot(iboat),nel,iboat)
  143        call borne(panneau(nel).hya,i,  '= 1/2 hauteur âme, diam,côté ', nel,ximin(i,iboat),ximax(i,iboat),iff)
             xicou(i,iboat)=panneau(nel).hya
             goto 131
  144        call borne(panneau(nel).dya,i,  '= épaisseur âme épont.doublet', nel,ximin(i,iboat),ximax(i,iboat),iff)
             xicou(i,iboat)=panneau(nel).dya
             goto 131
  145        call borne(panneau(nel).wya,i,  '= largeur semel.épont.doublet', nel,ximin(i,iboat),ximax(i,iboat),iff)
             xicou(i,iboat)=panneau(nel).wya
             goto 131
  146        call borne(panneau(nel).epais,i,'=épaisseur paroi mince       ', nel,ximin(i,iboat),ximax(i,iboat),iff)
             xicou(i,iboat)=panneau(nel).epais
             goto 131
  147        call borne(panneau(nel).epsa,i, '= larg. effect. épontille    ', nel,ximin(i,iboat),ximax(i,iboat),iff)
             xicou(i,iboat)=panneau(nel).epsa
             goto 131
          endif
  131  continue
       if((ntot(iboat)+nvar(nel,iboat)).gt.m1tmax) then
          write(*,*)  'le nombre max de variables de conception est', &
                      ' dépasse :ntot>nvmax ',ntot(iboat),' > ',nvmax
          write(iu_31(iboat),*) 'le nombre max de variables de conception est',&
		                        'dépasse :ntot>nvmax ',ntot(iboat),' > ',nvmax
          write(iu_14(iboat),*) 'le nombre max de variables de conception est',&
		                        ' dépasse :ntot>nvmax ',ntot(iboat),' > ',nvmax
		
		  write(*,*) 'error'
		  read(*,*)
		  iff=1 !stop à la sortie de la subr ent
       endif
    endif

return

! === formats et gestion des erreurs =================================================================================================

  6  format(' efforts de bords dans le plan du panneau selon l''axe x'/,' a repartir sur une distance de ',f7.3,'  (m)'/)
  7  format(' cas nø',i2,' variation lineaire de la p.h. (xi a xf) :'/  &
      '    au bord de depart  (y=0)   , xi=',f7.3,' m.'/                 &
      '    au bord d''arrivee (y=phil) , xf=',f7.3,' m.'/                &
      '     nb: 1m = 1 m d''eau (colonne) = 9.81kn/m2')
 10  format (a30,i3,a11,i3,a2)				
 11  format (a32,i3,a11,i3,a2)				
  39  format(/' caracteristiques des traverses'/1x,30(1h+)/              &
      t20,'distance',t32,'epaisseur de',t45,'largeur de',t57,            &
      'epaisseur',t67,'hauteur'/t20,'par rapport',t32,'la semelle',      &
      t45,'la semelle',t57,'de l ame',t67,'de l ame'/t20,'au sommet '/   &
      t23,'(m)',t36,'(m)',t50,'(m)',t62,'(m)',t72,'(m)')


 212  format(/'cas nø',i2,': charges variables selon ox,',   &
      ' definies en ',i2,' intervalles'/                     &
      t11,'regulierement espaces (charge en kn par metre courant)')
 213  format(' intervalle nø     charge verticale (n/m)     pression (m)  en y=0  et en y=phil')
 214  format(t4,i2,t20,e14.7,' n/m',t45,e14.7,' m. ',e14.7,' m.')
 218  format(/'panneau nø',i3,': kse=0 est incompatible avec icha=1'/'!!! nous prenons kse=2 avec xi=xf=0  et  cha2=cha3=0 !!!!!!!'/)

 283  format(/'panneau nø',i3/15(1h-)/'nbre de variables de conception = ',i3)

 420  format('Materiau utilise est le numero : ',i4)
 421  format('Material used is the number: ',i4)
 441  format(' traverse n ',i2,6x,5(f8.5,4x))
 649  format(//' le rapport rayon/epaisseur est de ',e11.4,' m.'/)

 909  format(/' longitudinal girder dimensions & positions'/1x,30(1h+)/  &
      t20,'positions',t32,'flange',t45,'flange',t57,                     &
      ' web',t67,' web'/t20,'related to',t32,'thickness',                &
      t45,'width',t57,'thickness',t67,'height'/t20,'depart edge(y=0)'/   &
      t23,'(m)',t36,'(m)',t50,'(m)',t62,'(m)',t72,'(m)')

 900 write(* ,*)'erreur de lecture : "end of file"'
     write(iu_11(iboat),*)'erreur de lecture : "end of file"'
     write(iu_14(iboat),*)'erreur de lecture : "end of file"'
     write(*,*) 'stop'
     read(*,*)
     stop

end
