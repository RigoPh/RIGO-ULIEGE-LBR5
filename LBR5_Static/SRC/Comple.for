      SUBROUTINE COMPLE(EQU,DEQU,ARG,DARG,NBRXI,I,K,A,B,IT,XX)

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION A(8,4),B(8,4),
     *          EQU(9),DEQU(9),ARG(8),DARG(8,9)
	DIMENSION XX(1)

c     COMMON/ALLONG/AU1(4),AU2(4),AU3(4),AU4(4),AU5(4),AU6(4),AU7(4),
c    *              AU8(4),ARG2(8),ARG3(8),ARG4(8),ARG5(8)

      CALL COMPL(EQU,DEQU,ARG,DARG,NBRXI,I,K,A,B,IT,
     *  XX(1) ,XX(5), XX(9),XX(13),XX(17),XX(21),XX(25),XX(29), ! AU1  à AU8
     *  XX(33),XX(41),XX(49),XX(57))                            ! ARG2 à ARG5

	RETURN
	END

C***********************************************************************
C***********************************************************************
      SUBROUTINE COMPL(EQU,DEQU,ARG,DARG,NBRXI,I,K,A,B,IT,
     *             AU1,AU2,AU3,AU4,AU5,AU6,AU7,AU8,ARG2,ARG3,ARG4,ARG5)

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION A(8,4),B(8,4),
     *          EQU(9),DEQU(9),ARG(8),DARG(8,9)
      DIMENSION AU1(4),AU2(4),AU3(4),AU4(4),AU5(4),AU6(4),AU7(4),
     *          AU8(4),ARG2(8),ARG3(8),ARG4(8),ARG5(8)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       Modifié : 15-4-99							     Créer : 1994
C       --------			                             -------
C	SUBROUTINE COMPLEX : Calcul de la dérivée (DARG) des solutions (ARG) 
C     de l'équation du 8ème ordre par rapport au variables de conception (XI).
C     avec:
C     I = indice du alpha et béta (i=1,M) avec Mmin=2 et Mmax=4 
C     K = nø de la variable de conception (XI) par rapport à laquelle on dérive. 
C	
C     EQU  = Coéfficients de l'équation du 8ème ordre
C     ARG  = Solutions de l'équation du 8ème ordre (alpha + i béta)
C	       alpha(i) = ARG(2i-1) et béta(i) = ARG(2i), i=1,4
C     DEQU = Dérivées des coéfficients EQU (voir subr. DARG)
C     DARG = Dérivées des solutions de l'équation du 8ème ordre (alpha + i béta)
C	A,B  = Matrice de calcul de stokage (temporaire)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  A(1,i) = Solutions complexes de l'équation du 8ème ordre (partie réelle= alpha)
C  B(1,i) = Solutions complexes de l'équation du 8ème ordre (partie imaginaire=béta)
C           si les solutions sont: (A,B)=(alpha(i),béta(i))=(ARG(2i-1),ARG(2i))

	IF(IT.EQ.1) THEN
      DO 1 J=2*I-1,2*I
      ARG2(J)=ARG(J)*ARG(J)
      ARG3(J)=ARG2(J)*ARG(J)
      ARG4(J)=ARG2(J)*ARG2(J)
   1  ARG5(J)=ARG3(J)*ARG2(J)
      AU1(I)=2.*ARG(2*I-1)*ARG(2*I)                           !  2 A B
      AU2(I)=ARG2(2*I-1)-ARG2(2*I)                            !  A**2-B**2
      AU3(I)=-2.*AU1(I)*AU2(I)                                !  -4(A**3 B - A B**3)
      AU4(I)=ARG4(2*I-1)+ARG4(2*I)-6.*ARG2(2*I-1)*ARG2(2*I)   !  A**4+B**4-6 A**2 B**2
      AU5(I)=ARG3(2*I-1)-3.*ARG(2*I-1)*ARG2(2*I)              !  +(A**3 - 3 A B**2)
      AU6(I)=ARG3(2*I)-3.*ARG(2*I)*ARG2(2*I-1)                !  -(3 A**2 B - B**3)
      AU7(I)=ARG5(2*I-1)-10.*ARG3(2*I-1)*ARG2(2*I)            !
     *                  +5.*ARG(2*I-1)*ARG4(2*I)              !  A**5-10 A**3 B**2 + 5 A B**4
      AU8(I)=ARG5(2*I)-10.*ARG3(2*I)*ARG2(2*I-1)
     *                  +5.*ARG(2*I)*ARG4(2*I-1)              ! B**5-10 B**3 A**2 + 5 B A**4

	A(1,I)=ARG(2*I-1)
	B(1,I)=ARG(2*I)
	A(2,I)=AU2(I)
	B(2,I)=AU1(I)
	A(3,I)=AU5(I)
	B(3,I)=-AU6(I)
	A(4,I)=+AU4(I)
	B(4,I)=-AU3(I)
	A(5,I)=AU7(I)
	B(5,I)=AU8(I)
	A(6,I)=+AU5(I)*AU5(I)-AU6(I)*AU6(I)
	B(6,I)=-2.*AU5(I)*AU6(I)
	A(7,I)=+AU4(I)*AU5(I)-AU3(I)*AU6(I)
	B(7,I)=-AU4(I)*AU6(I)-AU3(I)*AU5(I)
	A(8,I)=+AU4(I)*AU4(I)-AU3(I)*AU3(I)
	B(8,I)=-2.*AU3(I)*AU4(I)
      ENDIF

c 	WRITE(66,*)'(Alpha,Béta)    ',A(1,I),B(1,I)
c 	WRITE(66,*)'(Alpha,Béta)**2 ',A(2,I),B(2,I)
c 	WRITE(66,*)'(Alpha,Béta)**3 ',A(3,I),B(3,I)
c 	WRITE(66,*)'(Alpha,Béta)**4 ',A(4,I),B(4,I)
c 	WRITE(66,*)'(Alpha,Béta)**5 ',A(5,I),B(5,I)
c 	WRITE(66,*)'(Alpha,Béta)**6 ',A(6,I),B(6,I)
c 	WRITE(66,*)'(Alpha,Béta)**7 ',A(7,I),B(7,I)
c 	WRITE(66,*)'(Alpha,Béta)**8 ',A(8,I),B(8,I)

c 	WRITE(66,*)' '
c 	WRITE(66,*)'vérification des solutions'
c	BOF1=EQU(9)*A(8,I)
c	BOF2=EQU(7)*A(6,I)
c	BOF3=EQU(5)*A(4,I)
c	BOF4=EQU(3)*A(2,I)
c 	WRITE(66,*)'EQU(9)*A(8,I)='
c 	WRITE(66,10)BOF1
c 	WRITE(66,*)'EQU(7)*A(6,I)='
c 	WRITE(66,10)BOF2
c 	WRITE(66,*)'EQU(5)*A(4,I)='
c 	WRITE(66,10)BOF3
c 	WRITE(66,*)'EQU(3)*A(2,I)='
c 	WRITE(66,10)BOF4
c 	WRITE(66,*)'EQU(1)       ='
c 	WRITE(66,10)EQU(1)
c	BOF=BOF1+BOF2+BOF3+BOF4
c 	WRITE(66,*)'terme 1 à 4   ='
c 	WRITE(66,10)BOF
c	BOF=BOF+EQU(1)
c 	WRITE(66,*)'terme 1 à 5   ='
c 	WRITE(66,10)BOF


c 	WRITE(66,*)' '
c 	WRITE(66,*)'calcul de DERIVA'
c	BOF1=DEQU(9)*A(8,I)
c	BOF2=DEQU(7)*A(6,I)
c	BOF3=DEQU(5)*A(4,I)
c	BOF4=DEQU(3)*A(2,I)
c 	WRITE(66,*)'DEQU(9)*A(8,I)='
c 	WRITE(66,10)BOF1
c 	WRITE(66,*)'DEQU(7)*A(6,I)='
c 	WRITE(66,10)BOF2
c 	WRITE(66,*)'DEQU(5)*A(4,I)='
c 	WRITE(66,10)BOF3
c 	WRITE(66,*)'DEQU(3)*A(2,I)='
c 	WRITE(66,10)BOF4
c 	WRITE(66,*)'DEQU(1)       ='
c 	WRITE(66,10)DEQU(1)
c	BOF=BOF1+BOF2+BOF3+BOF4
c 	WRITE(66,*)'terme 1 à 4   ='
c 	WRITE(66,10)BOF

c 	WRITE(66,*)' '
c 	WRITE(66,*)'calcul de TEMPA'
c	BOF1=8.*EQU(9)*A(7,I)
c	BOF2=6.*EQU(7)*A(5,I)
c	BOF3=4.*EQU(5)*A(3,I)
c	BOF4=2.*EQU(3)*A(1,I)
c 	WRITE(66,*)'8.*EQU(9)*A(7,I)='
c 	WRITE(66,10)BOF1
c 	WRITE(66,*)'6.*EQU(7)*A(5,I)='
c 	WRITE(66,10)BOF2
c 	WRITE(66,*)'4.*EQU(5)*A(3,I)='
c 	WRITE(66,10)BOF3
c 	WRITE(66,*)'2.*EQU(3)*A(1,I)='
c 	WRITE(66,10)BOF4
c	BOF=BOF1+BOF2+BOF3
c 	WRITE(66,*)'terme 1 à 3   ='
c 	WRITE(66,10)BOF
c 	WRITE(66,*)' '


 	DERIVA=DEQU(9)*A(8,I)+DEQU(7)*A(6,I)+DEQU(5)*A(4,I)
     *	                 +DEQU(3)*A(2,I)+DEQU(1)
 	DERIVB=DEQU(9)*B(8,I)+DEQU(7)*B(6,I)+DEQU(5)*B(4,I)
     *	                 +DEQU(3)*B(2,I)     
 	TEMPA=8.*EQU(9)*A(7,I)+6.*EQU(7)*A(5,I)
     *                      +4.*EQU(5)*A(3,I)+2.*EQU(3)*A(1,I)
 	TEMPB=8.*EQU(9)*B(7,I)+6.*EQU(7)*B(5,I)
     *                      +4.*EQU(5)*B(3,I)+2.*EQU(3)*B(1,I)

c 	WRITE(66,*)'DERIVA',DERIVA
c 	WRITE(66,*)'DERIVB',DERIVB
c 	WRITE(66,*)'TEMPA ',TEMPA
c 	WRITE(66,*)'TEMPB ',TEMPB
	TEMP=TEMPA*TEMPA+TEMPB*TEMPB
c 	WRITE(66,*)'TEMP ',TEMP

	DARG(2*I-1,K)=-(DERIVA*TEMPA+DERIVB*TEMPB)/TEMP
	DARG(2*I,K)=-(DERIVB*TEMPA-DERIVA*TEMPB)/TEMP

c 	WRITE(66,*)'DARG(2*I-1,K) ',DARG(2*I-1,K)
c     WRITE(66,*)'DARG(2*I,K) ',DARG(2*I,K)

  10  FORMAT(20X,D24.17)
      RETURN
      END
