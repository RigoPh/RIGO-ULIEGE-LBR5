subroutine write_panel_data()



use param_section




! ====================================================================================================
!
! subroutine de sauvetage des données après chaque itération
! en vue d'une réutilisation ultérieure.
!
! ====================================================================================================

implicit double precision (a-h,o-z)
character*1  num

!pi=2.d00*acos(0.d00)
pi1=pi/180.d00

!!!rewind iu_10(iboat)
num=char(96+itera)
section_file=section_files(iboat)
open(iu_34(iboat),file='up-'//num//'-'//section_file)

write(iu_34(iboat),'(a80)') header1
write(iu_34(iboat),'(i2)')  iana			
write(iu_34(iboat),'(a80)') header2

write(iu_34(iboat),'(5(i2,1x),i3,1x,i2,1x,i3,t64,a)') impr,impr2,indaig,indrai,dessin,jlph2,ibusc,neto,'commentaire'

write(iu_34(iboat),'(2(i2,1x),3(f5.3,1x),2(f11.1,1x),f7.3,t64,a)') imulti,rho,w1,w2,w3,fk(1),fk(2),fk(3),'imulti,rho,w1,w2,w3'	!TODO RAJOUTER 3 termes

write(iu_34(iboat),'(i2,1x,i4,3(i2,1x),t64,a)')  ipareto, nsteppar, iparcout, iparpoids, iparinertia,	    &
                   'frontière Pareto: active (0 = non, 1 = oui); nombre des pas; coût (0 = non, 1 = oui); poids (0 = non, 1 = oui); inertia (0 = non, 1 = oui)'								

write(iu_34(iboat),'(4(i2,2x),2(f8.1,2x),i2,2x,f10.1,t64,a)') lcc2,lcc3,lcc4,lcc5,lightweight,deadweight,lccscenario,poidsLBR5_init,   &
                   'LIFE CYCLE COST: Mod.2-3-4-5 (0=non, 1=oui), Lightweight, Deadweight,Scenario (1:dwt cst, 2:displ cst), PoidsLBR5_init'


write(iu_34(iboat),'(i2,1x,i3,t64,a)') iopti,iteram,'iopti (1 oui, 0 non) et itemax = nbre d''itération'

write(iu_34(iboat),'(i2,  t64,a)') icout,   'icout'
write(iu_34(iboat),'(i2,  t64,a)') iredress,'iredress'   
write(iu_34(iboat),'(f8.4,t64,a)') rend_global,'rend_global'		
write(iu_34(iboat),'(i2,  t64,a)') refthick_input,'Reference thickness input (0 = LBR5, 1 = DCNS)'
	
write(iu_34(iboat),'(i2,  t64,a)')		           nbrMat,'nbrMateriau'	

do ind=1,nbrMat

   write(iu_34(iboat),'(a,1x,i2)') 'MATERIAU ',ind						
   write(iu_34(iboat),'(e11.4,1x,f6.3,1x,2(e11.4,1x),f4.2,2x,e11.4,t64,a)') e(ind),eta(ind),sigy(ind),sigyadm(ind),coefk(ind),spec(ind) , &
                                                                           'e, eta, sigy, sigyadm,coefk, spec'

   write(iu_34(iboat),'(3(f8.5,1x),t64,a)') dref(ind),drefx(ind),drefy(ind),'eo, eox, eoy  (épaisseur de référence en m)'
   write(iu_34(iboat),'(3(f8.5,1x),t64,a)') dref_b(ind),dref_c(ind),dref_l(ind),'Reference thickness for profiles (Bulb, Composite, Rolled)'
   write(iu_34(iboat),'(4(f8.3,1x),t64,a)') c1(ind),c2(ind),c3(ind),dc1(ind),'prix matér. : c1,c2,c3(tole,euro/kg), dc1(tole,variation par mm)'
   write(iu_34(iboat),'(3(f8.3,1x),t64,a)')  c_pb(ind),c_tc(ind),c_tl(ind),'Prix Matér. : C_PB.C_TC.C_TL(tole.Euro/kg). (Bulb, Composite, Rolled)'

   write(iu_34(iboat),'(2(f8.3,1x),t64,a,a)')  dw2(ind),dw3(ind),'extra weight: ','dw2(long,variation sur c2),dw3(trans,variation sur c3)'
   write(iu_34(iboat),'(3(f8.3,1x),t64,a,a)')  dw_b(ind),dw_c(ind),dw_l(ind),'Extra weight on profiles(%)  (Bulb, Composite, Rolled)'
   write(iu_34(iboat),'(f8.3,t64,a)') labour_cost(ind),'Labour Cost (Euros/h-h) = Ancien EQP*C1'
   write(iu_34(iboat),'(2(f8.3,1x),t64,a)') p10(ind),dp10(ind),'mdo bordé   : p10(h-h/m),  pc10(variation par mm) '
   write(iu_34(iboat),'(4(f8.3,1x),t64,a,a)') p4(ind),p5(ind),dp4(ind),dp5(ind),'mdo assembl.: ','p4x(long,h-h/m),p5y(trans,h-h/m),dp4x(long),dp5y(trans)'
   write(iu_34(iboat),'(2x,4(f8.3,1x),t64,a,a)') p9x(ind),p9y(ind),dp9x(ind),dp9y(ind),'mdo membr.  ',': p9x(long,h-h/m),p9y(trans,h-h/m),dp9x(long),dp9y(trans)'
   write(iu_34(iboat),'(4(f8.3,1x),t64,a,a)') p6(ind),p7(ind),ber(ind),bet(ind),'mdo joints  ',': p6(intersect),p7(gousset),béta-x(long.),béta-y(transv)'
   write(iu_34(iboat),'(2(f8.3,1x),2(i2,6x),t64,a,a)') c8(ind),dc8(ind),ialr(ind),ialt(ind),'consommables: c8(euro/m),','dc8(variation par mm),alpha-x(long.),alpha-y(transv)'

enddo

  

write(iu_34(iboat),'(f9.5,1x,f9.5,t64,a)') width,length, 'Longueur du tronçon (WIDTH), Longueur reglementaire du navire'
write(iu_34(iboat),'(5(f8.5,1x),t64,a)') (dis(i),i=1,5),'section de calcul (dis)'
write(iu_34(iboat),'(6(f6.3,1x),t64,a)')  (fam(i),i=1,6),'cisaillement des semelles (fam)'
write(iu_34(iboat),'(i2,t64,a)') ipoids,'ipoids ( 1 oui, 0 non)'

write(iu_34(iboat),12) nsolm, nsol,(nnsol(i),i=1,nsol)


do i=1,nsolm       
	 write (iu_34(iboat),'(a80)')            casename(i)
	 write (iu_34(iboat),'(i2)') is_loadcase_fatigue(i)
enddo                    

do i=1,nsolm
	write(iu_34(iboat),'(f8.4,t64,a)') tirant(i)
   !if(is_selected_loadcase_fatigue(i).eq.1) then !TODO mis en commentaire car is_selected_loadcase_fatigue défini à nsol (pas nsolm) => erreur
   !write(iu_34(iboat),'(f8.4,t64,a)') tirant(i)         
   !endif
enddo

write(iu_34(iboat),'(f8.4,t64,a)') depth    ,'depth of the ship'
write(iu_34(iboat),'(f8.4,t64,a)') part_full,'part of the ship s life in full load condition, given in tab 9 (bV rules, ptb,ch7,sec 4)	'
write(iu_34(iboat),'(  i4,t64,a)') method_fatigue,'damage calculation method. 1: dnV method ,2: bV method'
write(iu_34(iboat),'(f8.4,t64,a)') tfl      ,'increased design life (between 25 and 40).if no incrfease of design life, tfl=20'
write(iu_34(iboat),'(  i4,t64,a)') sens_finite_diff,'si sens_finite_diff :calcul des sensibilites par differences fines pour comparaison'
write(iu_34(iboat),'(  i4,t64,a)') sens_finite_diff,'si sens_finite_diff ' ! ajout de cette ligne par Olivier Yerrna afin qu'on puisse lire les fichiers up car le module fatigue ne le permettait plus.
!write(iu_34(iboat),*) (NNSOL_BV_TYPE(I),I=1,nsol_fatigue) !A corriger car marche pas				!fat_new

12  format(12(i3),5x,'load cases: nbr available; nbr selected, list of selected')

! ====================================================================================================
!
! lecture des donnees relatives a chaque panneau
!
! ====================================================================================================
ntot(iboat)=0

!rewind(iu_18(iboat))

do nel=1,neto
   call ent2(nel)
enddo

! ====================================================================================================
!
! transformations des donnees  
!
! ====================================================================================================

write(iu_34(iboat),'(5a29)') 'boundary conditions; y=0, y=h'
write(iu_34(iboat),'(i3,t64,a)')  ncondi,'nbre de conditions de bords'
if(ncondi.gt.0) then
do 910 i=1,ncondi
write(iu_34(iboat),'(i3,3x,i2,t64,a,i3,a,i3)')(nno9(i,j),j=1,2),'cond nø',i,',  panel ',nno9(i,1)
  910 continue
endif

write(iu_34(iboat),'(5a65)') 'gravity repere utilisateur + restriction sur le centre de gravite'
write(iu_34(iboat),'(f12.6,2x,f12.6,t64,a)') xk9,yk9,'(xk,yk) coord du centre du repere utilisateur'
write(iu_34(iboat),'(i2,2x,f9.6,2x,f9.6,t64,a)') igrav,xkgmin,xkgmax,'igrav(no:0;min:1;max:2;min et max:3),xkgmin,xkgmax'


! ====================================================================================================
!
! restriction sur inertie								
!
! ====================================================================================================

write(iu_34(iboat),'(5a18)') 'inertia constraint'
write(iu_34(iboat),'(i2,2x,e14.7,t64,a)') inert,imin,'inert,imin'


! ====================================================================================================
!
! restriction sur module sectionnel						
!
! ====================================================================================================

write(iu_34(iboat),'(5a26)') 'section modulus constraint'
write(iu_34(iboat),'(i2,2x,e14.7,2x,i2,t64,a)') imod,modmin,ielt,'imod,modmin,ipan'											!restri module

! ====================================================================================================
!
! restriction sur poids									  
!
! ====================================================================================================


write(iu_34(iboat),'(5a17)') 'weight constraint'
write(iu_34(iboat),'(i2,2x,e14.7,t64,a)') iweight,wmax,'iweight,wmax'	

	

! ====================================================================================================
!
! restriction sur cout									
!
! ====================================================================================================

write(iu_34(iboat),'(5a15)') 'cost constraint'				
write(iu_34(iboat),'(i2,2x,e14.7,2x,i2,t64,a)') iprice,pricemax,icost,'iprice,pricemax,icost'	


! ====================================================================================================
!
! moments d'extremites   (subr. bateau)
!
! ====================================================================================================


write(iu_34(iboat),'(5a6)') 'moment'
write(iu_34(iboat),'(i2,t64,a)')imom,'(= 0:pas de moment, = 1:avec moment)'

if (imom.ne.0) then
   write(iu_34(iboat),'(f8.5,t64,a)')yred,'coeff. de réduction 1.24 pour sig. et 1.04 pour w'
   do i=1,nsolm
      write(iu_34(iboat),'(4(1x,e14.7),t63,a)') bm11(i),sf11(i),bm31(i),sf31(i),'mx> 0 en hogging = pont tendu (n.m); my'
   enddo
!   do i=1,nsolm
!      write(iu_34(iboat),'(1(1x,e14.7),t63,a)') TEffortT(i)
!   enddo
endif
!   write(iu_34(iboat),'(2(1x,i2),4(1x,e14.7),t63,a)') decktri, castrim, longtri, longtot, largeurtri, vitessemax
!   do i=1,decktri
!   write(iu_34(iboat),'(1(1x,i2),t63,a)') ptri(i)
!   enddo
!   do i=1,castrim
!   write(iu_34(iboat),'(6(1x,e14.7),t63,a)') GMT(i)
!   enddo
!endif

! ====================================================================================================
!
! lecture des contraintes d'egalite (et impression)
!
! ====================================================================================================

write(iu_34(iboat),'(5a21)') 'equality restrictions'
write(iu_34(iboat),'(i4,t64,a,a)')negalt(iboat),'nbre de restrictions d''égalité'

do i=1,negalt(iboat)
   write(iu_34(iboat),'(i3,2x,i3,6x,i3,2x,i3,6x,f8.4)')	(mega(i,k,iboat),k=1,4),ega(i,iboat)		
enddo

! ====================================================================================================
!
! donnees pour le calcul de la resistance ultime de la poutre navire
!
! ====================================================================================================


write(iu_34(iboat),'(5a17)') 'ultimate strenght'
write(iu_34(iboat),'(i2,t64,a)')irestr,'(irestr: utilisation comme restriction ; 0 = non;1 = oui)'

if(irestr.eq.1) then
   write(iu_34(iboat),'(t2,e14.7,2x,e14.7,4x,a)')  uhogm,usagm,'(if irest=1 : uhogg>0 et usagg<0 (n.m)'
endif

write(iu_34(iboat),'(i2,t64,a,a)') iult,'(iult =0 no analysis, = 1 for paik/caldwell,   &
	                                                           = 2 for procol(smith),   &
															   = 3 for...)'

if(iult.eq.1) then
   write(iu_34(iboat),'(t4,2(f8.4,2x),t64,a)')depth,db,'hull depth & double bottom hight (m.)'
   write(iu_34(iboat),301)  nd, (id(i), i=1,nd)
   write(iu_34(iboat),302)  nb1,(ib1(i),i=1,nb1)
   write(iu_34(iboat),303)  nb, (ib(i), i=1,nb)
   write(iu_34(iboat),304)  ns, (is9(i), i=1,ns)
 
 301    format(4x,i2,(20i3/),' deck') 
 302    format(4x,i2,(20i3/),' inner bottom')
 303    format(4x,i2,(20i3/),' outer bottom')
 304    format(4x,i2,(20i3/),' both sides plates')


   write(iu_34(iboat),'(6x,i2,2x,e14.7,t64,a)') kd,  sysd, 'ref panel & sy for upper deck'
   write(iu_34(iboat),'(6x,i2,2x,e14.7,t64,a)') kb1, sysb1,'ref panel & sy for inner bottom'
   write(iu_34(iboat),'(6x,i2,2x,e14.7,t64,a)') kb,  sysb, 'ref panel & sy for outer bottom'
   write(iu_34(iboat),'(6x,i2,2x,e14.7,t64,a)')ksup,syssup,'ref panel & sy for upper part of the side plate'
   write(iu_34(iboat),'(6x,i2,2x,e14.7,t64,a)')ksdw,syssdw,'ref panel & sy for lower part of the side plate'
endif

close(iu_34(iboat))




return
end














! ====================================================================================================
!
! subroutine ent2
! ecriture des donnees relatives a chaque panneau.
!
! ====================================================================================================


subroutine ent2(nel)

use param_section
use PARAM_SECTION_VECTOR 

implicit double precision (a-h,o-z)

!character*20  name_type_tr1

! ====================================================================================================
!
! utilisation des nouvelles valeurs des variables de conception.
!
! ====================================================================================================


do 123 i=1,nvar(nel,iboat)
       goto(124,125,120,126,127,128,121,129,130),nxit(i,nel,iboat)
   124 panneau(nel).delta=xicou(i+ntot(iboat),iboat)
       goto 123
   125 panneau(nel).hya=xicou(i+ntot(iboat),iboat)
       goto 123
   120 panneau(nel).dya=xicou(i+ntot(iboat),iboat)
       goto 123
   126 panneau(nel).wya=xicou(i+ntot(iboat),iboat)
       goto 123
   127 panneau(nel).epsa=xicou(i+ntot(iboat),iboat)
       goto 123
   128 panneau(nel).hxr=xicou(i+ntot(iboat),iboat)
       goto 123
   121 panneau(nel).dxr=xicou(i+ntot(iboat),iboat)
       goto 123
   129 panneau(nel).wxr=xicou(i+ntot(iboat),iboat)
       goto 123
   130 panneau(nel).epsr=xicou(i+ntot(iboat),iboat)

       temp= ((1.0/panneau(nel).epsr) - (xmode(nel)/(dabs(panneau(nel).phil)*panneau(nel).q*pi1)) )
 	     if(temp.le.0.000) then
	        write(iu_31(iboat),*)
	        write(iu_31(iboat),*)'attention : error dans le panneau ',nel
	        write(iu_31(iboat),*)'  (subr. copy)'
	        write(iu_31(iboat),*)'****************************************'
            write(iu_31(iboat),*)'le paramètre mode est incompatible avec les bornes de variation de l''entredistance entre les raidisseurs et la largeur du panneau.'
	        write(iu_31(iboat),*)'vous devez corriger cette situation !!'
	        write(iu_31(iboat),*)'essayer en modifiant le paramère mode'
	        write(iu_31(iboat),*)' - actuellement on a mode =',mode(nel)
	        write(iu_31(iboat),*)' - prenez plutôt     mode = EE2'
	        write(*,*)
	        write(*,*)'attention : error dans le panneau ',nel
	        write(*,*)'  (subr. copy)'
	        write(*,*)'****************************************'
            write(*,*)'le paramètre mode est incompatible avec les bornes de variation de l''entredistance entre les raidisseurs et la largeur du panneau.'
	        write(*,*)'vous devez corriger cette situation !!'
	        write(*,*)'essayer en modifiant le paramère mode'
	        write(*,*)' - actuellement on a mode =',mode(nel)
	        write(*,*)' - prenez plutôt     mode = EE2'
	        write(iu_14(iboat),*)													
	        write(iu_14(iboat),*)'attention : error dans le panneau ',nel				
	        write(iu_14(iboat),*)'  (subr. copy)'					
	        write(iu_14(iboat),*)'****************************************'			
            write(iu_14(iboat),*)'le paramètre mode est incompatible avec les bornes de variation de l''entredistance entre les raidisseurs et la largeur du panneau.'		
	        write(iu_14(iboat),*)'vous devez corriger cette situation !!'		
	        write(iu_14(iboat),*)'essayer en modifiant le paramère mode'		
	        write(iu_14(iboat),*)' - actuellement on a mode =',mode(nel)					
	        write(iu_14(iboat),*)' - prenez plutôt     mode = EE2'					
            write(*,*) 'stop'
            read(*,*)
	        stop
	   endif
     panneau(nel).entr=1.0/ temp
     if(nel.eq.1)  write(iu_31(iboat),'(/a/1x,50(1h-)/a)') ' espacement effectif entre raidisseurs long.(lisses)',' (spacing to consider for the long. stiffeners)'

     write(iu_31(iboat),'(a,i3,a,f8.4,a)') ' panneau=',nel,': entredistance(raid.)=',panneau(nel).entr,' (m)'

123  continue

panneau(nel).tya=tfa(nel)
panneau(nel).txr=tfr(nel)


!!! SAUVETAGE DES NOUVELLES VALEURS !!! TODO à supprimer qd toutes les fonctions
! objectifs obj...2 seront supprimées et que sera utilisé partout ecriVar dans Optis

!call updatepanneau(nel)



! ====================================================================================================
!
! impression du types de panneau : coque ou plaque
!
! ====================================================================================================

write(iu_34(iboat),'(a9,t15,a,i3,a)') panneau(nel).types,'PANNEAU nø ',nel,'          ======================================================================================'	

if(panneau(nel).types.eq.'plaque') then

!---------------------
!  cas de la "plaque"
!---------------------

   write(iu_34(iboat),'(f12.5,2x,f8.5,t64,a)') panneau(nel).hight,panneau(nel).delta,'haut.,epaiss.(m)'
elseif((panneau(nel).types.eq.'epontille').or.(panneau(nel).types.eq.'EPONTILLE')) then								

!---------------------
!  cas de l'"épontille"												
!---------------------

   write(iu_34(iboat),'(f12.5,2x,f8.5,t64,a)') panneau(nel).hight,panneau(nel).epais,'haut.(m),epaiss.(m)'  
	 if((panneau(nel).section.eq.'cercle').or.(panneau(nel).section.eq.'CERCLE')) write(iu_34(iboat),'(a6)')  'cercle'        
	 if((panneau(nel).section.eq.'carre').or.(panneau(nel).section.eq.'CARRE')) write(iu_34(iboat),'(a5)')   'carre'						
	 if((panneau(nel).section.eq.'doublet').or.(panneau(nel).section.eq.'DOUBLET')) write(iu_34(iboat),'(a7)') 'doublet'				
else

!---------------------
!  cas de la "coque"
!---------------------

   write(iu_34(iboat),'(f10.5,1x,e14.7,1x,f8.5,t64,a)') panneau(nel).phil,panneau(nel).q,panneau(nel).delta,'angl.(deg), rayon et epaiss (m)'
endif

! ====================================================================================================
!
! impression relatives au raidissage (aig. et raid)
!
! ====================================================================================================

      
if ((panneau(nel).types.ne.'epontille').and.(panneau(nel).types.ne.'EPONTILLE')) then
   write(iu_34(iboat),'(5(f9.6,1x),t64,a)') panneau(nel).epsa,panneau(nel).entr,panneau(nel).epsa2,panneau(nel).entr2,panneau(nel).corro(1),                                         'Entr. aig. et raid, idem raid. compl. (m); ép. corrosion tôle'			
   write(iu_34(iboat),'(5(f9.6,1x),i2,1x,a,t64,a)') panneau(nel).hya,panneau(nel).dya,panneau(nel).wya,panneau(nel).tya,panneau(nel).corro(2),panneau(nel).itype_transv,panneau(nel).name_type_transv,       'Cadres Transversaux (m). Hauteur âme. Epaisseur âme. Largeur semelle. Epaisseur semelle; ép. corrosion cadre; type de profilé; nom profilé'			
   write(iu_34(iboat),'(5(f9.6,1x),i2,1x,a,t64,a)') panneau(nel).hxr,panneau(nel).dxr,panneau(nel).wxr,panneau(nel).txr,panneau(nel).corro(3),panneau(nel).itype_longit,panneau(nel).name_type_longit,       'Raidisseurs  Long.  (m). Hauteur âme. Epaisseur âme. Largeur semelle. Epaisseur semelle; ép. corrosion raidisseur; type de profilé; nom profilé'


   if((panneau(nel).epsa2.ge.(0.00001)).or.(panneau(nel).entr2.ge.(0.00001))) then
      write(iu_34(iboat),'(4(f9.6,1x),2x,i2,1x,i2,1x,a,t64,a)') panneau(nel).hya2,panneau(nel).dya2,panneau(nel).wya2,panneau(nel).tya2,panneau(nel).ksa2,panneau(nel).itype_transv_second,panneau(nel).name_type_transv_second,' Cadres transversaux compl. (m)  cadres transversaux compl. (m)'
      write(iu_34(iboat),'(4(f9.6,1x),2x,i2,1x,i2,1x,a,t64,a)') panneau(nel).hxr2,panneau(nel).dxr2,panneau(nel).wxr2,panneau(nel).txr2,panneau(nel).ksr2,panneau(nel).itype_longit_second,panneau(nel).name_type_longit_second,' Raidisseurs long.   compl. (m)  raidisseurs long. compl.   (m)'
   endif
      write(iu_34(iboat),'(a3,t64,a)') mode(nel),'code : répartition raid. long.'
else  
	
	 write(iu_34(iboat),'(2(f9.6),t64,a)') panneau(nel).epsa,panneau(nel).heff,'ecartement, hauteur sans hiloire'           
	 if ((panneau(nel).section.ne.'doublet').and.(panneau(nel).section.ne.'DOUBLET')) then                          
	    write(iu_34(iboat),'(2(f9.6,1x),i2,1x,a)') panneau(nel).hya,panneau(nel).corro(2)
		!,itype_transv(nel),name_type_transv(nel)                     
	 else                                                    
	    write(iu_34(iboat),'(5(f9.6,1x),i2,1x,a)') panneau(nel).hya,panneau(nel).dya,panneau(nel).wya,panneau(nel).tya,panneau(nel).corro(2)
		!,itype_transv(nel),name_type_transv(nel)         
	 endif                                                   

endif                                                     

! ====================================================================================================
!
! lecture du coefficient de participation		
!
! ====================================================================================================
if((panneau(nel).types.ne.'epontille').and.(panneau(nel).types.ne.'EPONTILLE')) then 
   write(iu_34(iboat),'(f12.7,2x,i3,2x,i3,2x,i3,t64,a)') panneau(nel).part,panneau(nel).code,panneau(nel).fami,panneau(nel).lot,'coefficient de participation. code de position. famille. lot'  
endif

! Rendement du panneau
if((panneau(nel).types.ne.'epontille').and.(panneau(nel).types.ne.'EPONTILLE')) then 
	write(iu_34(iboat),'(f4.2,t64,a)') panneau(nel).rend,'REND_PANEL'
endif

! ====================================================================================================
!
! Paramètres de vibration		
!
! ====================================================================================================
if(itype(nel).eq.2) then !uniquement les PANNEAUX
   write(iu_34(iboat),'(i2,2x,f12.7,i2,1x,i2,t64,a)') panneau(nel).elmt_vib,panneau(nel).long_vib,	&
								panneau(nel).cl_av_vib,panneau(nel).cl_ar_vib,'Vibration : N°Element; Long.Element CL avant;CL arriere (2=simply supported;6=clampsed)'
endif



! ====================================================================================================
!
! suite de la lecture des données (caract. mat., charge et angle)
!
! ====================================================================================================

write(iu_34(iboat),'(i2,t64,a)') indMateriau(nel),'MATERIAU choisi'
!write(iu_34(iboat),'(e14.7,1x,f6.3,1x,3(e14.7,1x),t70,a)') e(nel),eta9(nel),sigy(nel),coefk(nel),spec(nel),'e, poisson, sy, sadm, poids spec'
write(iu_34(iboat),'(i2,3x,4(i2,1x),4x,i2,3x,i2,t64,a)') panneau(nel).mt,panneau(nel).ksr,panneau(nel).ksa,panneau(nel).kst,panneau(nel).kse,ipa(nel),ivaria(nel),'mt,ksr,ksa,kst,kse,ipa,ivaria'
write(iu_34(iboat),'(f12.7,t64,a)') panneau(nel).angle,'angle (degré))'

do i=1,nsolm
		write(iu_34(iboat),'(f12.4,1x,f12.4,t64,a,i2,a)') panneau(nel).xi_full(i),panneau(nel).xf_full(i),'xi, xf  cas nø',i,' (ivaria=1)'
enddo

write(iu_34(iboat),'(f12.4,t64,a)') panneau(nel).ploc,'ploc = pression max locale (lisse et maille)'

! ====================================================================================================
!
! lecture des charges variables selon ox
!
! ====================================================================================================

write(iu_34(iboat),'(i2,t64,a,a)') panneau(nel).icha,'charge variable selon x',      ' (= 0 pas de surcharges, =1 avec surcharge)'

if(panneau(nel).icha.gt.0) then
   do i=1,nsolm
      write(iu_34(iboat),'(5a20)') title_cha_full(i,nel)
      write(iu_34(iboat),'(2x,i2,t64,a,a)') npt_full(i,nel),'nbre d''intervalles'
      do j=1,npt_full(i,nel)
         write(iu_34(iboat),'(2x,f12.4,4x,f10.4,1x,f10.4,t64,a)') (cha_full(j,ij,i,nel),ij=1,3),'charges : cha1 (n/m) et cha2,cha3 (m)'
      enddo
   enddo
endif

! ====================================================================================================
!
! lecture des noh
!
! ====================================================================================================
write(iu_34(iboat),'(10(i3,1x),t64,a)') panneau(nel).noh9(:),'liste des 10 panneaux qui suivent'

! ====================================================================================================
!
! lectures des traverses
!
! ====================================================================================================
if(panneau(nel).mt.ne.0) then

   do i=1,panneau(nel).mt
      write(iu_34(iboat),'(4(f8.6,1x),3x,f10.6,t64,a)')		&
			panneau(nel).hxtr(i),panneau(nel).dxtr(i),panneau(nel).wxtr(i),	&
			panneau(nel).txtr(i),panneau(nel).abtr2(i),						&
			' Raidisseurs longitudinaux (m)'
   enddo

endif


! ====================================================================================================
!
! lectures des parametres de fatigue
!
! ====================================================================================================

!fat_new:mise en commentaire tout ce qui est fatigue

!if((panneau(nel).types.ne.'epontille').and.(panneau(nel).types.ne.'EPONTILLE')) then 
!
!write(iu_34(iboat),'(4(i4,1x),t64,a)')   panneau(nel).i_fatigue_stiffened_panel(1), &
!			                     panneau(nel).i_fatigue_stiffened_panel(2), & 
!			                     panneau(nel).i_fatigue_stiffened_panel(3), & 
!			                     panneau(nel).i_fatigue_stiffened_panel(4), & !fat_r(nel)
 !                                'i_fatigue_stiffened_panel'
!write(iu_34(iboat),'(1(i4,1x),t64,a)')   panneau(nel).i_fatigue_frame,      & !fat_a(nel)
!                                 'i_fatigue_frame'  
!write(iu_34(iboat),'(1(i4,1x),t64,a)')   panneau(nel).i_fatigue_girder,     & !fat_g
!                                 'i_fatigue_girder'  
								 
!write(iu_34(iboat),'(i3,t64,a)') panneau(nel).number_points_considered,'number_points_considered'

!do j=1,panneau(nel).number_points_considered
!    write(iu_34(iboat),'(i3,1x,i3)') panneau(nel).ix_stiffened_pannel(j), &
!			                         panneau(nel).iy_stiffened_pannel(j)

!enddo
!write(iu_34(iboat),'(1(f8.4,1x),t64,a)') panneau(nel).weib_fact(j),'weib_fact'	



!if((panneau(nel).mt).ne.0) then                                              ! pas de traverse(girder) dans le panneau
!	do mmt=1,panneau(nel).mt
!		write(iu_34(iboat),'(f8.6)') panneau(nel)%epstr(mmt)					        !girder breadth (m)
!		write(iu_34(iboat),'(f8.6)') panneau(nel)%girder_length(mmt)			        !girder length  (m)
!	enddo
!endif			   											                     	        ! 1:for deck longitudinals,2:for ship side,3:for bottom longitudinals,4:for longitudinal and transverse bulkheads

!write(iu_34(iboat),'(5(f8.6,1x),t64,a)') panneau(nel).ptyr,   &
!                                 panneau(nel).esr,    &
!								 panneau(nel).lbr,    &
!								 panneau(nel).wbr,    &
!								 panneau(nel).tbr,	  &
!								 'stiffener: type of profile, end type ,length of bracket(mm) , width of bracket(mm) , thickness of bracket(mm)'


!write(iu_34(iboat),'(5(f8.6,1x),t64,a)') panneau(nel).ptya,   &
!                                 panneau(nel).esa,    &
!								 panneau(nel).lba,    &
!								 panneau(nel).wba,    &
!								 panneau(nel).tba,    &
!								 'frame    : type of profile, end type ,length of bracket(mm) , width of bracket(mm) , thickness of bracket(mm)'


!if((panneau(nel).mt).ne.0) then                                                          ! pas de traverse(girder) dans le panneau
 !  do mmt=1,panneau(nel).mt
  !    write(iu_34(iboat),'(5(f8.6,1x),t64,a)') panneau(nel)%ptyg(mmt),  &
   !                                    panneau(nel)%esg (mmt),  &
	!		   						   panneau(nel)%lbg (mmt),  &
	!								   panneau(nel)%wbg (mmt),  &
	!								   panneau(nel)%tbg (mmt),  &
	!								   'girder   : end type ,length of bracket, Width of bracket, thickness of bracket'

   !enddo
!endif

!write(iu_34(iboat),'(4(f8.6,1x),t64,a)') panneau(nel).kt_hs_1r, &
 !                                panneau(nel).kt_hs_2r, &
!								 panneau(nel).kt_hs_3r, &
!								 panneau(nel).kt_hs_4r, &
!								 'hot spot factor (stiffeners)'
			  	
!write(iu_34(iboat),'(4(f8.6,1x),t64,a)') panneau(nel).kt_hs_1a, &
 !                                panneau(nel).kt_hs_2a, &
!								 panneau(nel).kt_hs_3a, &
!								 panneau(nel).kt_hs_4a, &
!								 'hot spot factor (frame)'
			  	
!if((panneau(nel).mt).ne.0) then                                                ! pas de traverse(girder) dans le panneau
!
 !  do mmt=1,panneau(nel).mt
  !    write(iu_34(iboat),'(4(f8.6,1x),t64,a)') panneau(nel)%kt_hs_1g(mmt), &
   !                                            panneau(nel)%kt_hs_2g(mmt), &
	!							               panneau(nel)%kt_hs_3g(mmt), &
	!								           panneau(nel)%kt_hs_4g(mmt), &
	!								           'hot spot factor (girder)'
!   enddo
!endif


!write(iu_34(iboat),'(1(f8.4,1x),t64,a)')  panneau(nel).betaif,'improvement fatigue life factor for grind technique, generaly to be taken equal to 2.2'				
!write(iu_34(iboat),'(1(f8.4,1x),t64,a)')  panneau(nel).kcor,'corrosion factor, pour chaque détail' 					
!write(iu_34(iboat),'(1(f8.4,1x),t64,a)')  panneau(nel).tm,'slope of s-n curve; commonly m=3'           	
!write(iu_34(iboat),'(1(e16.8,1x),t64,a)') panneau(nel).tc,'   (mpa^m)		        fatigue capacity' 
!write(iu_34(iboat),'(4(f8.4,1x),t64,a)')  panneau(nel).lambda_weld,'coefficient depending on the weld configuration'
!write(iu_34(iboat),'(4(f8.4,1x),t64,a)')  panneau(nel).toe_angle,'mean weld toe angle , in degrees, without being taken less than 30°(30° for butt joint,45° for T joint or cruciform)'


if (panneau(nel).types.ne.'epontille') then

write(iu_34(iboat),'(i2)') 0
write(iu_34(iboat),'(i2)') 0
write(iu_34(iboat),'(i2)') 0
write(iu_34(iboat),'(i2)') 0
write(iu_34(iboat),'(i2)') 0

endif

!endif


! ====================================================================================================
!
! lecture de variables de conception , xi, ximax et ximin
!
! ====================================================================================================

write(iu_34(iboat),'(5a16)')              'design variables' 
write(iu_34(iboat),'(i2,t64,a)')nvar(nel,iboat),'nbre de var. de conception'



if(nvar(nel,iboat).ne.0) then								
!  ecriture de nxi(i) (nø des var. de conception)
   write(iu_34(iboat),501)(nxit(i,nel,iboat),i=1,nvar(nel,iboat))
501    format(9(i2,1x),t30,'nø des var. de conception')
endif		
write(iu_34(iboat),502)(panneau(nel).dvmin(i),i=1,9)			
write(iu_34(iboat),502)(panneau(nel).dvmax(i),i=1,9)			
502    format(9(f10.6,1x))						

ntot(iboat)=ntot(iboat)+nvar(nel,iboat)

! ====================================================================================================
!
! restrictions structurelles: 
!
! ====================================================================================================


write(iu_34(iboat),'(5a22)') 'structural constraints'
write(iu_34(iboat),'(i2,t64,2a)') panneau(nel).im1,'(0 without structural constraints',', 1 with structural constraints)'
if(panneau(nel).im1.gt.0) then
   
   write(iu_34(iboat),'(3x,i2,t64,a)') panneau(nel).ipts,'nbre de pts pour le calcul des sensibilités'
   write(iu_34(iboat),601) (ypt9(i,nel),i=1,panneau(nel).ipts)
601      format(10(f9.4,1x),'  pts choisis pour le calcul des sensibilités')

   do is=1,nsolm
      write(iu_34(iboat),'(a80)') title_loadcase(is,nel)
      write(iu_34(iboat),'(i2,t64,a)') panneau(nel).m1(is),'nbre de restrictions structurelles'

      if(panneau(nel).m1(is).gt.0) then

         do i=1,panneau(nel).m1(is)
		 ! TODO normalement, pas besoin de prendre les vector, mais marche pas sinon => doit y avoir un bug
            write(iu_34(iboat),'(2x,i3,t8,i3,3x,e14.7,2x,i2,2x,i2,2x,a)') i,panneau(nel).lcont4_init(is,i,1),panneau(nel).cjmax9_init(is,i),   &
			                                                              panneau(nel).inv3_init(is,i),panneau(nel).lcont4_init(is,i,2),       &
																		  '(#	constraint type		cjmax	inv   ipt)'	 

         enddo
      endif
    enddo
endif

if ((panneau(nel).types.ne.'epontille').and.(panneau(nel).types.ne.'EPONTILLE')) then
	write(iu_34(iboat),'(a)') 'FATIGUE CONSTRAINTS' 
	write(iu_34(iboat),'(f9.2)') panneau(nel).cjmax_fatigue
endif


! ====================================================================================================
!
! lecture des m2 restrictions géométriques (ibm2 = nø du set de rest
!             m2 =  nbre de restr. géom. du panneau nel
!
! ====================================================================================================

write(iu_34(iboat),'(5a23)') 'geometrical constraints'
write(iu_34(iboat),'(i2,t64,a)') panneau(nel).m2,'m2=nbre de restrictions geometriques'

if(panneau(nel).m2.eq.99) then       ! cas ou un set de restrictions est choisi
   write(iu_34(iboat),'(i3,t64,a)') panneau(nel).ibm2,'ibm2= nø du set de restr. géom.)'
   write(iu_34(iboat),'(i3,2x,i3,t64,a)') panneau(nel).isema,panneau(nel).isemr,'isema,isemr'
else if(panneau(nel).m2.ne.0) then ! cas ou les restriction sont définies individuellement
!   read (iu_10(iboat),*,end=900) (lm2(i),i=1,m2)
!   read (iu_10(iboat),*,end=900) isema(nel),isemr(nel)
   write(iu_34(iboat),503)(panneau(nel).lm2(i),i=1,panneau(nel).m2)
503    format(20(i3,1x),'nø des m2 restrictions géométriques')
   write(iu_34(iboat),'(i3,2x,i3,t64,a)') panneau(nel).isema,panneau(nel).isemr,'isema,isemr'
endif

! ====================================================================================================
!
! lecture des restrictions de sloshing
!
! ====================================================================================================

if (itype(nel).ne.5) then
	write(iu_34(iboat),'(a)') 'Sloshing constraints'
	write(iu_34(iboat),504) panneau(nel).nsloshm,(panneau(nel).nslosh(i),i=1,panneau(nel).nsloshm)
504  format(4(i3),t64,'nombre de restrictions + restriction selectionnees')
	if (panneau(nel).nsloshm.gt.0) write(iu_34(iboat),505) panneau(nel).press_slosh
505  format(3(f9.3),t64,'epaisseur min; inertie min; section min')
endif


return

900 write(*,*)'subroutine copy, erreur de lecture : "end of file"'
pause 'stop'      


stop
end
