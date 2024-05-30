      SUBROUTINE DISCU(ROOT,ROOT1,IER,ARG,IF,M)                         PDI00010
      IMPLICIT REAL*8(A-H,O-Z)                                          PDI00020
      DIMENSION ROOT(8),ROOT1(8),ARG(8)                                 PDI00030
C***********************************************************************PDI00040
C                                                                       PDI00050
C     SUBROUTINE DISCU                                                  PDI00060
C     SUBROUTINE DE DISCUSSION DES RESULTATS DE POLRT,SUIVANT LES       PDI00070
C     VALEURS DE CES RESULTATS ET DES INDICES IER.                      PDI00080
C                                                                       PDI00090
C      Modif: 17-3-94				Création : Thèse Ph. Rigo                                                                 
C***********************************************************************PDI00100
      IF(IER.EQ.1) WRITE(66,*)' Equation de degré 0 ???'                                          
      IF(IER.EQ.2) WRITE(66,*)' Equation de degré >35 ???'                                           
      IF(IER.EQ.3) WRITE(66,19)                                         PDI00120
      IF(IER.EQ.4) WRITE(66,*)' Coéf de EQU(9)=0 ???'                                          
      IF((IER.LT.0).AND.(IER.GT.4)) WRITE(66,*)' IER non valide =',IER                                        
 
      IF(IER.EQ.1) WRITE(29,*)' Equation de degré 0 ???'							!bug                          
      IF(IER.EQ.2) WRITE(29,*)' Equation de degré >35 ???'						!bug                                     
      IF(IER.EQ.3) WRITE(29,19)													!bug       
      IF(IER.EQ.4) WRITE(29,*)' Coéf de EQU(9)=0 ???'								!bug 	                           
      IF((IER.LT.0).AND.(IER.GT.4)) WRITE(29,*)' IER non valide =',IER			!bug 

      IF(IER.EQ.0) GOTO 17                                              PDI00110
      IF=0                                                              PDI00130
      RETURN                                                            PDI00140
      
   17 J=-1                                                              PDI00150
      DO 20 I=1,8                                                       PDI00160
        IF(ROOT(I)) 21,22,20                                            PDI00170
   22   WRITE(66,24)                                                    PDI00180
	  WRITE(29,24)																!bug
        IF=0                                                            PDI00190
        RETURN                                                          PDI00200
   21   J=J+2                                                           PDI00210
        ARG(J)=ROOT(I)                                                  PDI00220
        ARG(J+1)=ROOT1(I)                                               PDI00230
   20 CONTINUE                                                          PDI00240
   
      M=4                                                               

      DO 26 I=1,3                                                       PDI00260
      J=I+1                                                             PDI00270
      DO 26 IM=J,4                                                      PDI00280
        IF(DABS(ARG(2*I-1)-ARG(2*IM-1))-1.E-08) 44,26,26                PDI00290
   44   IF(DABS(ARG(2*I)-ARG(2*IM))-1.E-08) 45,49,49                    PDI00300
   49   IF(DABS(ARG(2*I)+ARG(2*IM))-1.E-08) 28,26,26                    PDI00310
   45   WRITE(66,46)                                                    PDI00320
	  WRITE(29,46)																!bug
        IF=0                                                            PDI00330
        RETURN                                                          PDI00340
   28   M=M-1                                                           PDI00350
        IF(IM-4) 29,48,29                                               PDI00360
   29   JM=IM+1                                                         PDI00370
        DO 27 IMP=JM,4                                                  PDI00380
          ARG(2*IMP-3)=ARG(2*IMP-1)                                     PDI00390
          ARG(2*IMP-2)=ARG(2*IMP)                                           
   27   CONTINUE
        IF(M-2) 47,47,48                                                PDI00410
   48   ARG(7)=0.                                                       PDI00420
        ARG(8)=0.                                                       PDI00430
   26 CONTINUE                                                          
   47 CONTINUE
   
      IF(M-3) 30,31,30                                                  PDI00450
   31 DO 33 I=1,2                                                       PDI00460
        IF(DABS(ARG(2*I))-1.E-08) 33,32,32                              PDI00470
   32   ARG(7)=ARG(2*I-1)                                                 
        ARG(8)=ARG(2*I)                                                   
        J=2*I-1                                                           
        DO 34 IM=J,6                                                      
   34   ARG(IM)=ARG(IM+2)                                                 
        ARG(7)=0.                                                         
        ARG(8)=0.                                                         
   33 CONTINUE 
   30 CONTINUE

c     Prise des valeurs absolues des solutions (solutions de signe +)
      DO 50 I=1,M                                                        
       ARG(2*I-1)=DABS(ARG(2*I-1) )                                         
       ARG(2*I)  =DABS(ARG(2*I)   )                           
c       ARG(2*I-1)=-ARG(2*I-1)                                            
c       IF(ARG(2*I)) 51,50,50                                             
c   51  ARG(2*I)=-ARG(2*I)    	                           
   50 CONTINUE   
   
c     M=3
c	ARG(3)=1.E-10
c	ARG(4)=DABS(ROOT1(5)) 
c	ARG(5)=1.E-10
c	ARG(6)=DABS(ROOT1(7)) 
c	ARG(7)=0. 
c	ARG(8)=0.
                                                       

   19 FORMAT(' NON CONVERGENCE DU PROCESSUS D''ITERATION VISANT A'/     PDI00610
     *       'DETERMINER LES RACINES DE L''EQUATION CARACTERISTIQUE.')         
   24 FORMAT(' L''EQUATION CARACTERISTIQUE A DES RACINES IMAGINAIRES'/  PDI00630
     *       'PURES. LA METHODE N''EST PAS VALABLE.')                          
   46 FORMAT(' L''EQUATION CARACTERISTIQUE A DES RACINES MULTIPLES.'/   PDI00650
     *       'LA METHODE DOIT ETRE REFORMULEE.')                               
      RETURN                                                            PDI00670
      END                                                               PDI00680
