subroutine open_section_output_files()
! renommage de open_section_file() et suppression de l'ouverture de iu_10
! on n'ouvre que des fichiers de sortie et de travail



use param_section
!use param_opti_local



! ====================================================================================================
! nom des fichiers resultats 
! ====================================================================================================

section_file=section_files(iboat)

iunit1='sol-' //trim(section_file)
iunit2='opt-' //trim(section_file)
iunit3='sol2-'//trim(section_file)
iunit4='bug-' //trim(section_file)
iunit5='sol3-'//trim(section_file)		
iunit6=section_file(1:len_trim(section_file)-4)//'1.txt'	
iunit7=section_file(1:len_trim(section_file)-4)//'2.txt'
iunit8=section_file(1:len_trim(section_file)-4)//'3.txt'	
write(iunit9 ,'(a,i1,a)')    '303_',iboat,'.dat' ! Fichier contient toutes les restrictions
!write(iunit10,'(a,i1,a)') 'alstom_',iboat,'.dat'
!write(iunit11,'(a,i1,a)') 'audrey_',iboat,'.dat'
write(iunit12,'(a,i1,a)') 'redressage_',iboat,'.dat'



! ouverture des fichiers 
!=============================

if (iboat.eq.1) then !Ouverture du fichier des résultats d'optimisation (Opt)
	open(iu_opti,file='opt-MULTI-STRUCTURES.txt')
endif

! fichier d'entree   deja ouvert et lu dans read_ship_input_file() Maud                                                 
! open(iu_10(iboat),file=section_file,status='old',err=903)  !open(iu_10(iboat),file=iunit6,status='old',err=903) 
! sortie listing (sortie standard)          
open(iu_11(iboat),file=iunit1)                
! sortie listing (recapitulatif des resultats)                     
open(iu_12(iboat),file=iunit3)                        
!bug  
open(iu_14(iboat),file=iunit4)				


!open(iu_22(iboat),STATUS='SCRATCH',FORM='UNFORMATTED')      
!open(iu_22(iboat),status='scratch',form='unformatted')      
! subr. ent,bo1,bo2                                                 
!open(iu_23(iboat),status='scratch',form='unformatted')      
! données pour conlin	    
!open(iu_23(iboat),file='conlin.dat')      
! données pour conlin	    

!open(iu_24(iboat),status='scratch',form='formatted')  
! données fct objct
!open(iu_25(iboat),status='scratch',form='unformatted')
! subr. contraintes			


!open(iu_26(iboat),file=iunit9)		
!open(iu_26(iboat),status='scratch',form='unformatted')
! valeurs extrêmes	
open(iu_27(iboat),status='scratch',form='formatted')  			


! données pour subr "cost-alstom" 
!open(iu_28(iboat),file=iunit10)   
! fichier scratch 
open(iu_30(iboat),status='scratch',form  ='formatted')      

! données pour  "redressage" 
open(iu_redress(iboat),file=iunit12)   


!55   -> iu_10(iboat)=10+(iboat-1)*100
!66   -> iu_11(iboat)=11+(iboat-1)*100
!67   -> iu_12(iboat)=12+(iboat-1)*100
!68   -> iu_13(iboat)=13+(iboat-1)*100
!29   -> iu_14(iboat)=14+(iboat-1)*100

!42   -> iu_15(iboat)=15+(iboat-1)*100
!43   -> iu_16(iboat)=16+(iboat-1)*100
!45   -> iu_17(iboat)=17+(iboat-1)*100
!46   -> iu_18(iboat)=18+(iboat-1)*100

!94   -> iu_19(iboat)=19+(iboat-1)*100
!95   -> iu_20(iboat)=20+(iboat-1)*100
!97   -> iu_21(iboat)=21+(iboat-1)*100
!98   -> iu_22(iboat)=22+(iboat-1)*100
!99   -> iu_23(iboat)=23+(iboat-1)*100

!301  -> iu_24(iboat)=24+(iboat-1)*100
!302  -> iu_25(iboat)=25+(iboat-1)*100
!303  -> iu_26(iboat)=26+(iboat-1)*100
!304  -> iu_27(iboat)=27+(iboat-1)*100
!305  -> iu_28(iboat)=28+(iboat-1)*100
!306  -> iu_29(iboat)=29+(iboat-1)*100

!2218 -> iu_30(iboat)=30+(iboat-1)*100
 
!666  -> iu_31(iboat)=31+(iboat-1)*100
! 78  -> iu_32(iboat)=32+(iboat-1)*100
! 96  -> iu_33(iboat)=33+(iboat-1)*100
! 77  -> iu_34(iboat)=34+(iboat-1)*100
! 57  -> iu_35(iboat)=35+(iboat-1)*100


!  1  -> iu_36(iboat)=36+(iboat-1)*100
!  2  -> iu_37(iboat)=37+(iboat-1)*100
!  3  -> iu_38(iboat)=38+(iboat-1)*100
!  7  -> iu_39(iboat)=39+(iboat-1)*100
!  8  -> iu_40(iboat)=40+(iboat-1)*100
! 10  -> iu_41(iboat)=41+(iboat-1)*100
! 44  -> iu_42(iboat)=42+(iboat-1)*100
! 88  -> iu_43(iboat)=43+(iboat-1)*100
 
         
!77 = up-'//num//'-'//filename
!78 = fichier avec liste des restriction


write(iunit13,'(a,i1,a)') 'iu_conlin',iboat,'.dat'
!write(iunit14,'(a,i1,a)') 'reduce_conlin',iboat,'.dat'

if (idebug_conlin.eq.1) then
   iu_conlin(iboat)=6698+iboat
   open(iu_conlin(iboat),file=iunit13,form='formatted')
   open(9997,file='red1.dat'   )
endif



!iu_conlin(1)=6699
!iu_conlin(2)=6700


return

! erreur de lecture du fichier de donnees (unite iu_10(iboat)) 
 903 if(langue.eq.1)write(*,*)'Fichier d''entree manquant !'                  
     if(langue.eq.2)write(*,*)'Input file missing!.'                       
     stop 1                                                                              

end
