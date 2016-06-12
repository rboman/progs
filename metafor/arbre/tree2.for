      PROGRAM TREE
C 
C    ls *.for > listpro.txt 
C
      CHARACTER*80 INFILE
      CHARACTER*200 NOM_PROC,TEMP
      CHARACTER*100000 SUBR
      INTEGER IOERR,i,j,k,LENSUBR(1000),nosub,nocall(1000)
      INTEGER dejaf(1000)
      INTEGER APPELS(1000,100),NPOS(100),remember(1000)
C--
C   SUBR() : noms des sous routines
C   LENSUBR() : taille des noms des sous routines
C   
C--
      DO i=1,1000
         dejaf(i)=0
      ENDDO
      i=0
      nosub=0
      ncall=0
C------------------------------------------------------------------
C               lecture des noms des routines --> SUBR
C                                             --> LENSUBR
C                                             --> nosub
C------------------------------------------------------------------
      OPEN(UNIT=1,FILE='listpro.txt',STATUS='OLD')
C
C
      WRITE(*,*)'lecture des noms des sous routines'
C
      nn=50
C  
 40   nosub=nosub+1
      READ(1,FMT='(A50)',IOSTAT=IOERR)TEMP
      SUBR((nosub-1)*nn+1:nosub*nn)=TEMP(1:nn)
      j=0
      IF(IOERR.EQ.0)THEN
 60      j=j+1
         IF(SUBR((nosub-1)*nn+j:(nosub-1)*nn+j+3).EQ.'.for') THEN
            LENSUBR(nosub)=j-1
c            WRITE(*,*)'..',SUBR((nosub-1)*nn+1:(nosub-1)*nn+j-1)
            GOTO 40
         ENDIF
         GOTO 60     
      ENDIF

      CLOSE(1)
      nosub=nosub-1
C
      WRITE(*,*)'...ok...'
C
c      GOTO 998
c      DO i=1,nosub
c         j=LENSUBR(i)
c         TEMP(1:j)=SUBR((i-1)*nn+1:(i-1)*nn+j)
c         TEMP(j+1:j+4)='.for'
c         WRITE(*,*)'--',TEMP(1:j+4),'--'
c         READ(*,*)
c      ENDDO
C      GOTO 999
c 998  CONTINUE
C------------------------------------------------------------------
C                         lecture des CALL --> APPELS()
C------------------------------------------------------------------   
C
      WRITE(*,*)'lecture des appels'
C
      DO i=1,nosub
C
         j=LENSUBR(i)
         TEMP(1:j)=SUBR((i-1)*nn+1:(i-1)*nn+j)
         TEMP(j+1:j+4)='.for'
         
c         WRITE(*,*)'Routine : ',TEMP(1:j+4),(i-1)*nn+1,(i-1)*nn+j 
c         WRITE(*,*)'--------- '
         nocall(i)=0
c         READ(*,*)
         OPEN(UNIT=1,FILE=TEMP(1:j+4),STATUS='OLD')
C        - lit la ligne suivante - 
         ligne = 0     
 10      READ(1,FMT='(A80)',IOSTAT=IOERR)INFILE
         ligne=ligne+1
         IF(IOERR.EQ.0)THEN
C           - test ligne de commentaires -
            IF((INFILE(1:1).EQ.'C').OR.(INFILE(1:1).EQ.'c')) GOTO 10
C           - Boucle sur la ligne -
            DO j=1,76
C              - Recherche un call -
               IF (INFILE(j:(j+3)).EQ.'CALL') THEN
                  IF(INFILE(j+5:j+7).EQ.'LIB') THEN
C                     WRITE(*,*)'LIBRARY'
                     GOTO 10
                  ENDIF
                  nocall(i)=nocall(i)+1
C                 WRITE(*,*)'trouve 1 CALL, numero',ncall
C                 jj est le pointeur dans INFILE
C                 k  est le pointeur dans NOM_PROC
                  jj=j+5
                  k=0
C                 - extraction du nom de la routine -
 30               IF(((ICHAR(INFILE(jj:jj)).GE.ICHAR('A')).AND.
     *                (ICHAR(INFILE(jj:jj)).LE.ICHAR('Z'))).OR.
     *               ((ICHAR(INFILE(jj:jj)).GE.ICHAR('0')).AND.
     *                (ICHAR(INFILE(jj:jj)).LE.ICHAR('9'))).OR.
     *               ((ICHAR(INFILE(jj:jj)).GE.ICHAR('a')).AND.
     *                (ICHAR(INFILE(jj:jj)).LE.ICHAR('z')))) THEN
c 30               IF(INFILE(jj:jj).NE.'(').AND.
c      *              ICHAR(INFILE(jj:jj)).NE.       
c                     WRITE(*,*)ICHAR(INFILE(jj:jj))
                     k=k+1
                     NOM_PROC(k:k)=INFILE(jj:jj)
                     jj=jj+1
                     GOTO 30
                  ENDIF
                  CALL TOUPPER(NOM_PROC,k,TEMP)
                  NOM_PROC(1:k)=TEMP(1:K)                  
c                  k=k-1
c                  WRITE(*,*)'..CALL ',NOM_PROC(1:k),' line ',ligne
C                 -recherche du n° de la sous routine-
                  APPELS(i,nocall(i))=0
c                  WRITE(*,*)'search...'
                  DO l=1,nosub
                     long=LENSUBR(l)
                     IF(long.EQ.k) THEN                   
                        CALL TOUPPER(SUBR((l-1)*nn+1:(l-1)*nn+long),
     *                               long,TEMP)
                        IF(NOM_PROC(1:k).EQ.TEMP(1:k)) THEN
                           APPELS(i,nocall(i))=l
C                           WRITE(*,*)'  routine numero :',l
                           Goto 100
                        ENDIF
                     ENDIF
                  ENDDO
 100              CONTINUE
               ENDIF
            ENDDO
         ELSE
            GOTO 20
         ENDIF
         GOTO 10
C
C        - on passe au fichier suivant -
 20      CONTINUE
c         WRITE(*,*)'<EOF>'
c         WRITE(*,*)'Nbre de CALL :',nocall(i)
c         READ(*,*)
         APPELS(i,nocall(i)+1)=-1
         CLOSE(1)
C
      ENDDO
      WRITE(*,*)'...ok...'

c      DO i=1,nosub
c         WRITE(*,*)nocall(i)
c         READ(*,*)
c      ENDDO

c       GOTO 999

C
C----------------------------------------------------------------
C  Creation de l'arbre a partir de MTPP01
C
C----------------------------------------------------------------
C
C     - recherche du numero de la premiere routine -
      DO l=1,nosub
         long=LENSUBR(l)
         IF(long.EQ.6) THEN                   
            CALL TOUPPER(SUBR((l-1)*nn+1:(l-1)*nn+long),
     *                   long,TEMP)
            IF(TEMP(1:6).EQ.'MTPP01') THEN
               nfirst=l
               Goto 101
            ENDIF
         ENDIF
      ENDDO
 101  CONTINUE
C
      WRITE(*,*)'Routine principale : MTPP01'
      WRITE(*,*)'numero',nfirst
      WRITE(*,*)
C
c
c
c      WRITE(*,*)nocall(nfirst)
c      DO i=1,nocall(nfirst)+1
c         WRITE(*,*)APPELS(nfirst,i)
c      ENDDO
c      READ(*,*)
c
c

      ncurrent=nfirst
      niv=2
      remember(1)=nfirst
      NPOS(2)=1
      DO i=1,200
          TEMP(i:i)='.'
      ENDDO

 200  CONTINUE
C        -- affichage -
         long=LENSUBR(ncurrent)
c         WRITE(*,*)'niv=',niv,' ncurrent=',ncurrent,
c     *             ' NPOS(niv)=',NPOS(niv),' rem=',remember(niv-1),
c     *             ' appel=',APPELS(ncurrent,NPOS(niv))
C
      ncu=ncurrent
      ni=niv-2
      INFILE(1:4*niv)=TEMP(1:4*niv)
      INFILE(4*niv+1:4*niv+long) =
     *      SUBR((ncu-1)*nn+1:(ncu-1)*nn+long)
c      WRITE(*,*)INFILE(1:4*niv+long)
      IF(APPELS(remember(niv-1),NPOS(niv)).NE.0) THEN
      WRITE(*,*)TEMP(1:4*ni),SUBR((ncu-1)*nn+1:(ncu-1)*nn+long),ni
c      WRITE(*,*)ni,'  ',SUBR((ncu-1)*nn+1:(ncu-1)*nn+long),dejaf(ncu)
      ELSE
      WRITE(*,*)TEMP(1:4*ni),'Routine inconne !'
      ENDIF
c      DO i=1,nosub
c         WRITE(*,*)i,dejaf(i)
c      ENDDO
c      READ(*,*)
C
c         IF(ncurrent.NE.0) THEN
c         WRITE(*,*)'vecteur appel'
c         DO i=1,nocall(ncurrent)+1
c            WRITE(*,*)'appel :',APPELS(ncurrent,i)
c         ENDDO
c         ENDIF
c         READ(*,*)
C

         IF(ncurrent.EQ.0) THEN
            NPOS(niv)=NPOS(niv)+1
            IF(APPELS(remember(niv-1),NPOS(niv)).EQ.-1) THEN
 215           IF(niv.EQ.2) GOTO 999
C              - on remonte a la routine appelante -
C              - on passe au suivant si possible -
               niv=niv-1
               NPOS(niv)=NPOS(niv)+1
C               ncurrent=APPELS(remember(niv-1),NPOS(niv)) 
               IF(NPOS(niv).GE.nocall(remember(niv-1))) THEN
                  GOTO 215 
               ELSE
                  ncurrent=APPELS(remember(niv-1),NPOS(niv))
               ENDIF        
            ELSE
               ncurrent=APPELS(remember(niv-1),NPOS(niv))
            ENDIF
            GOTO 200
         ENDIF

                     
C        - si la routine n appelle rien -
         IF ((APPELS(ncurrent,1).EQ.-1).OR.
     *        (dejaf(ncurrent).EQ.1)) THEN
C           - si on est a la derniere routine - 
            IF(NPOS(niv).EQ.nocall(remember(niv-1))) THEN
C              - si on est au niveau 2, c est fini ! -
 210           IF(niv.EQ.2) GOTO 999
C              - on remonte a la routine appelante -
C              - on passe au suivant si possible -
               niv=niv-1
               NPOS(niv)=NPOS(niv)+1
C               ncurrent=APPELS(remember(niv-1),NPOS(niv)) 
               IF(NPOS(niv).GE.nocall(remember(niv-1))) THEN
                  GOTO 210 
               ELSE
                  ncurrent=APPELS(remember(niv-1),NPOS(niv))
               ENDIF                  
            ELSE 
C              - on passe a la suivante -
               NPOS(niv)=NPOS(niv)+1
               ncurrent=APPELS(remember(niv-1),NPOS(niv)) 
            ENDIF
         ELSE
C              - on passe a la routine appelée -
               dejaf(ncurrent)=1
               remember(niv)=ncurrent
               ncurrent=APPELS(ncurrent,1)
               niv=niv+1
               NPOS(niv)=1
         ENDIF

      GOTO 200          
C
 999  CONTINUE
      END
C
C---------------------------------------------------
C  conversion d'une chaine en uppercase
C  rem : 0 < LONG < 50
C---------------------------------------------------
      SUBROUTINE TOUPPER(CHAINE,LONG,DESTI)
C
      CHARACTER*50 CHAINE,DESTI
      INTEGER LONG,i,INC
C
      INC=ICHAR('a')-ICHAR('A')
C
      DO i=1,LONG
         IF((ICHAR(CHAINE(i:i)).GE.ICHAR('a')).AND.
     *      (ICHAR(CHAINE(i:i)).LE.ICHAR('z'))) THEN
C
            DESTI(i:i)=CHAR(ICHAR(CHAINE(i:i))-INC)
C
         ELSE
            DESTI(i:i)=CHAINE(i:i)
         ENDIF
      ENDDO
C      WRITE(*,*)'--',CHAINE(1:long),'--> --',DESTI(1:long),'--',long

      RETURN
      END
