      subroutine rang2(nel,a1,b1,aux,zb)


      
      use param_section
      

      implicit double precision (a-h,o-z)                                         pra00020
      dimension a1(ne,8),b1(ne,nsol)              
      dimension aux(2*mt_max*2*mt_max),zb(2*mt_max*(8+nsolmax))

c***********************************************************************pra00050
c                                                                       
c     subroutine rang2, (assemblage automatique)                           
c     *****************
c
c     modifié : 18- 8-94 									créer: 18-8-94
c                4-12-95 (nsol cas de charge)
c                                                                                                                                            
c***********************************************************************pra00090
      ii=1                                                              pra00130
      if(nsign(nel).eq.1) ii=0                                          pra00140
      
      nd=noeud(nel,1)                                                   pra00160
      na=noeud(nel,2)                                                   pra00170
      i1=(nc+nd-1)*4+1                                                  pra00180
      i2=(nc+na-1)*4+1                                                  pra00190

c                                                                       pra00230
c rangement des coefficients relatifs aux equations d'equilibre et aux  pra00240
c conditions d'appuis eventuels                                         pra00250
c --------------------------------------------------------------------  pra00260
      m=1                                                               pra00270
      n=1                                                               pra00280
      if(nno(nd,2).ne.0) m=nno(nd,2)                                    pra00290
      if(nno(na,2).ne.0) n=nno(na,2)                                    pra00300

      do 100 j=1,8                                                      pra00310
      k=20*(j-1)                                                        pra00320
      do 100 i=1,4                                                      pra00330
      a1(i1+i-1,j)=-aux(mdp(m,i)+k+ii)                                     pra00340
      a1(i2+i-1,j)= aux(mar(n,i)+k-ii)                                     pra00350
  100 continue                                                          pra00360

      do 80 is=1,nsol                                                   pra00380
      kk=(is-1)*16
      do 80 i=1,4                                                       pra00380
      b1(i1+i-1,is)=b1(i1+i-1,is)-zb(mdp(m,i)+ii+kk)                            
      b1(i2+i-1,is)=b1(i2+i-1,is)+zb(mar(n,i)-ii+kk)                            
   80 continue                                                          pra00470
c                                                                       pra00480
c rangement des coefficients relatifs aux equations de compatibilite    pra00490
c ------------------------------------------------------------------    pra00500
      if (nc.eq.0) goto 300                                             pra00510
      do 200 j=1,nc                                                     pra00520
      m=(j-1)*4+1                                                       pra00530
      if (mcomp(j,1).eq.nel) goto 210                                   pra00540
      if (mcomp(j,2).eq.nel) goto 220                                   pra00550
      goto 200                                                          pra00560

c  le panneau arrive au noeud considéré (c'est son extrémité qui compte)
  210 do 201 i=1,8                                                      pra00570
      k=20*(i-1)                                                        pra00580
      a1(m,i  )=aux(2+k-ii)                                                pra00590
      a1(m+1,i)=aux(4+k-ii)                                                pra00600
      a1(m+2,i)=aux(14+k-ii)                                               pra00610
      a1(m+3,i)=aux(16+k-ii)                                               pra00620
  201 continue                                                          pra00630
      do 81 is=1,nsol                                                   pra00380
      kk=(is-1)*16
      b1(m  ,is)=b1(m  ,is)+zb( 2-ii+kk)                                   pra00640
      b1(m+1,is)=b1(m+1,is)+zb( 4-ii+kk)                                   pra00650
      b1(m+2,is)=b1(m+2,is)+zb(14-ii+kk)                                   pra00660
      b1(m+3,is)=b1(m+3,is)+zb(16-ii+kk)                                   pra00670
   81 continue
      goto  200                                                         pra00760

c  le panneau part du noeud considéré (c'est son noeud de départ qui compte)
  220 do 202 i=1,8                                                      pra00770
      k=20*(i-1)                                                        pra00780
      a1(m,i  )=-aux(1+k+ii)                                               pra00790
      a1(m+1,i)=-aux(3+k+ii)                                               pra00800
      a1(m+2,i)=-aux(13+k+ii)                                              pra00810
      a1(m+3,i)=-aux(15+k+ii)                                              pra00820
  202 continue                                                          pra00830
      do 82 is=1,nsol                                                   pra00380
      kk=(is-1)*16
      b1(m  ,is)=b1(m  ,is)-zb( 1+ii+kk)                                   pra00840
      b1(m+1,is)=b1(m+1,is)-zb( 3+ii+kk)                                   pra00850
      b1(m+2,is)=b1(m+2,is)-zb(13+ii+kk)                                   pra00860
      b1(m+3,is)=b1(m+3,is)-zb(15+ii+kk)                                   pra00870
   82 continue
  200 continue                                                          pra00960
  300 continue
      return                                                            pra00970
      end                                                               pra00980
