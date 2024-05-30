      subroutine discu(root,root1,ier,arg,if,m)                         pdi00010
      implicit real*8(a-h,o-z)                                          pdi00020
      dimension root(8),root1(8),arg(8)                                 pdi00030
c***********************************************************************pdi00040
c                                                                       pdi00050
c     subroutine discu                                                  pdi00060
c     subroutine de discussion des resultats de polrt,suivant les       pdi00070
c     valeurs de ces resultats et des indices ier.                      pdi00080
c                                                                       pdi00090
c      modif: 17-3-94				création : thèse ph. rigo                                                                 
c***********************************************************************pdi00100
      if(ier.eq.1) write(66,*)' equation de degré 0 ???'                                          
      if(ier.eq.2) write(66,*)' equation de degré >35 ???'                                           
      if(ier.eq.3) write(66,19)                                         pdi00120
      if(ier.eq.4) write(66,*)' coéf de equ(9)=0 ???'                                          
      if((ier.lt.0).and.(ier.gt.4)) write(66,*)' ier non valide =',ier                                        
      
	if(ier.eq.1) write(29,*)' equation de degré 0 ???'							!bug                          
      if(ier.eq.2) write(29,*)' equation de degré >35 ???'						!bug                                     
      if(ier.eq.3) write(29,19)													!bug       
      if(ier.eq.4) write(29,*)' coéf de equ(9)=0 ???'								!bug 	                           
      if((ier.lt.0).and.(ier.gt.4)) write(29,*)' ier non valide =',ier			!bug 

	if(ier.eq.0) goto 17                                             ! pdi00110
      if=0                                                              pdi00130
      return                                                            pdi00140
      
   17 j=-1                                                              pdi00150
      do 20 i=1,8                                                       pdi00160
        if(root(i)) 21,22,20                                            pdi00170
   22   write(66,24) 
   	  write(29,24)	                                                   pdi00180
        if=0                                                            pdi00190
        return                                                          pdi00200
   21   j=j+2                                                           pdi00210
        arg(j)=root(i)                                                  pdi00220
        arg(j+1)=root1(i)                                               pdi00230
   20 continue                                                          pdi00240
   
      m=4                                                               

      do 26 i=1,3                                                       pdi00260
      j=i+1                                                             pdi00270
      do 26 im=j,4                                                      pdi00280
        if(dabs(arg(2*i-1)-arg(2*im-1))-1.e-08) 44,26,26                pdi00290
   44   if(dabs(arg(2*i)-arg(2*im))-1.e-08) 45,49,49                    pdi00300
   49   if(dabs(arg(2*i)+arg(2*im))-1.e-08) 28,26,26                    pdi00310
   45   write(66,46)                                                    pdi00320
        write(29,46)
	  if=0                                                            !pdi00330
        return                                                          pdi00340
   28   m=m-1                                                           pdi00350
        if(im-4) 29,48,29                                               pdi00360
   29   jm=im+1                                                         pdi00370
        do 27 imp=jm,4                                                  pdi00380
          arg(2*imp-3)=arg(2*imp-1)                                     pdi00390
          arg(2*imp-2)=arg(2*imp)                                           
   27   continue
        if(m-2) 47,47,48                                                pdi00410
   48   arg(7)=0.                                                       pdi00420
        arg(8)=0.                                                       pdi00430
   26 continue                                                          
   47 continue
   
      if(m-3) 30,31,30                                                  pdi00450
   31 do 33 i=1,2                                                       pdi00460
        if(dabs(arg(2*i))-1.e-08) 33,32,32                              pdi00470
   32   arg(7)=arg(2*i-1)                                                 
        arg(8)=arg(2*i)                                                   
        j=2*i-1                                                           
        do 34 im=j,6                                                      
   34   arg(im)=arg(im+2)                                                 
        arg(7)=0.                                                         
        arg(8)=0.                                                         
   33 continue 
   30 continue

c     prise des valeurs absolues des solutions (solutions de signe +)
      do 50 i=1,m                                                        
       arg(2*i-1)=dabs(arg(2*i-1) )                                         
       arg(2*i)  =dabs(arg(2*i)   )                           
   50 continue   
   
   19 format(' non convergence du processus d''iteration visant a'/     pdi00610
     *       'determiner les racines de l''equation caracteristique.')         
   24 format(' l''equation caracteristique a des racines imaginaires'/  pdi00630
     *       'pures. la methode n''est pas valable.')                          
   46 format(' l''equation caracteristique a des racines multiples.'/   pdi00650
     *       'la methode doit etre reformulee.')                               
      return                                                            pdi00670
      end                                                               pdi00680
