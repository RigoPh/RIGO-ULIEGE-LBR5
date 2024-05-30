subroutine pinf1(sum,ali,zb,hyp,kk,k,ms,mt_max,nsolmax)


implicit double precision(a-h,o-z)
dimension ali(2*mt_max*(8+nsolmax)),hyp(2*mt_max*(8+nsolmax)),zb(2*mt_max*(8+nsolmax))

!-----------------------------------------------------------------------
!
!    calcul des termes independants pour la determination des
!    inconnues de bords lors de l'assemblage des panneaux dans mdr.
!    et cela pour la charge exterieure.
!
!    inf1 est relatif a la presence des traverses.
!
!-----------------------------------------------------------------------
do i=1,16
   sum=0.
   ! ms = 2* nbre de traverses
   do j=1,ms 
      ij=j+i*20
      sum=sum+ali(ij)*hyp(j+kk)
   enddo
   zb(i+k)=-sum/10000.
enddo
return
end
