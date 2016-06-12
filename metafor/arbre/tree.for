      PROGRAM TREE
C=========================================================================
C                            TREE pour FORTRAN
C                            ^^^^^^^^^^^^^^^^^
C  .Description : Ecrit sur la sortie standard un arbre représentant
C                 la structure des appels (CALL) aux sous-routines.
C
C  .Utilisation : Exécuter       'ls *.for > listpro.txt'
C                 Lancer ensuite 'tree > arbre.txt'
C
C  .Derniere modif. : 31.10.97
C
C  .Vars : SUBMAX     : nbre max de sous routines à considérer.
C          SUBR       : longue chaine de caractères contenant le 
C                       nom des routines du programme.
C          nosub      : nombre de sous routines.
C          ncurrent   : numero de la routine courante.
C
C          SUBR()     : noms des sous routines.
C          LENSUBR()  : taille des noms des sous routines.
C          APPELS()   : contient, pour chaque sous routine, les numeros
C                       des sous routines appelées par celle-ci.
C          nocall()   : nombre d'appels pour chaque sous routine.
C          remember() : contient l'enchainement des appels dans l'arbre.
C          NPOS()     : contient la position des sous routines courantes.
C          dejaf()    : 0 -> la routine a deja été affichée précédemment.
C                       1 -> la routine n'a pas été affichée préc.
C
C=========================================================================
C 
C-------------------------------------------------------------------------
C                              Déclarations
C-------------------------------------------------------------------------
C
      INTEGER SUBMAX
      PARAMETER(SUBMAX=1000)
C
      CHARACTER*80 INFILE,PREUMS
      CHARACTER*200 NOM_PROC,TEMP
      CHARACTER*100000 SUBR
C
      INTEGER IOERR,i,j,k,LENSUBR(1000),nosub,nocall(1000)
      INTEGER dejaf(1000),APPELS(1000,100),NPOS(100),remember(1000)
C
C-------------------------------------------------------------------------
C                              Initialisations
C-------------------------------------------------------------------------
C
      DO i=1,1000
         dejaf(i) = 0
      ENDDO
      i=0
      nosub=0
      ncall=0
C
C-------------------------------------------------------------------------
C                              Choix des options 
C-------------------------------------------------------------------------
      PREUMS='MTPP01'
C
C -- Affichage des CALL --
C
      OPT3=0
C
C -- Arbre complet (1=oui) --
C
      OPT1=0
C
C -- Affichage des routines inconnues (1=oui) -
C
      OPT2=1
C     
C------------------------------------------------------------------------
C               lecture des noms des routines --> SUBR
C                                             --> LENSUBR
C                                             --> nosub
C------------------------------------------------------------------------
C
      OPEN(UNIT=1,FILE='listpro.txt',STATUS='OLD')
C
c      WRITE(*,*)'lecture des noms des sous routines'
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
            GOTO 40
         ENDIF
         GOTO 60     
      ENDIF

      CLOSE(1)
      nosub=nosub-1
C
c      WRITE(*,*)'...ok...'
C
C------------------------------------------------------------------
C                    Lecture des CALL --> APPELS()
C------------------------------------------------------------------   
C
c      WRITE(*,*)'lecture des appels'
C
      DO i=1,nosub
C
         j=LENSUBR(i)
         TEMP(1:j)=SUBR((i-1)*nn+1:(i-1)*nn+j)
         TEMP(j+1:j+4)='.for'
C
         IF(OPT3.EQ.1)THEN         
            WRITE(*,*)'Routine : ',TEMP(1:j+4),(i-1)*nn+1,(i-1)*nn+j 
            WRITE(*,*)'--------- '
         ENDIF
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
C
C                         - Boucle sur la ligne -
C
            DO j=1,76
C
C                    - Recherche un call -
C
               IF (INFILE(j:(j+3)).EQ.'CALL') THEN
                  IF(INFILE(j+5:j+7).EQ.'LIB') THEN
C                     WRITE(*,*)'LIBRARY'
                     GOTO 10
                  ENDIF
                  nocall(i)=nocall(i)+1
C                 jj est le pointeur dans INFILE
C                 k  est le pointeur dans NOM_PROC
                  jj = j+5
                  k  = 0

C                 - Extraction du nom de la routine -

 30               IF(((ICHAR(INFILE(jj:jj)).GE.ICHAR('A')).AND.
     *                (ICHAR(INFILE(jj:jj)).LE.ICHAR('Z'))).OR.
     *               ((ICHAR(INFILE(jj:jj)).GE.ICHAR('0')).AND.
     *                (ICHAR(INFILE(jj:jj)).LE.ICHAR('9'))).OR.
     *               ((ICHAR(INFILE(jj:jj)).GE.ICHAR('a')).AND.
     *                (ICHAR(INFILE(jj:jj)).LE.ICHAR('z')))) THEN
                     k=k+1
                     NOM_PROC(k:k)=INFILE(jj:jj)
                     jj=jj+1
                     GOTO 30
                  ENDIF
                  CALL TOUPPER(NOM_PROC,k,TEMP)
                  NOM_PROC(1:k)=TEMP(1:K)                  
                  IF(OPT3.EQ.1)THEN
                     WRITE(*,*)'..CALL ',NOM_PROC(1:k),' line ',ligne
                  ENDIF

C                 - Recherche du n° de la sous routine -

                  APPELS(i,nocall(i))=0
                  DO l=1,nosub
                     long=LENSUBR(l)
                     IF(long.EQ.k) THEN                   
                        CALL TOUPPER(SUBR((l-1)*nn+1:(l-1)*nn+long),
     *                               long,TEMP)
                        IF(NOM_PROC(1:k).EQ.TEMP(1:k)) THEN
                           APPELS(i,nocall(i))=l
                           GOTO 100
                        ENDIF
                     ENDIF
                  ENDDO
 100              CONTINUE
C
               ENDIF
C                       - Fin de recherche du CALL -
C                     - On passe au caractère suivant -
            ENDDO
         ELSE
            GOTO 20
         ENDIF
C                     - On passe a la ligne suivante -
         GOTO 10
C
 20      CONTINUE

C        - Ajoute un '-1' pour terminer la liste -
C        - Pas '0' car cela signifie 'fichier inconnu' -
         APPELS(i,nocall(i)+1)=-1
         CLOSE(1)
C
C        - On passe au fichier suivant -
C
      ENDDO
C
C     - Tous les fichiers ont été traités -
C     
c      WRITE(*,*)'...ok...'
C
C-------------------------------------------------------------------------
C            Creation de l'arbre a partir de PREUMS
C-------------------------------------------------------------------------
C
C     - Recherche de la longueur de PREUMS -
C
      jj=1
      longp=0
 400  IF(((ICHAR(PREUMS(jj:jj)).GE.ICHAR('A')).AND.
     *    (ICHAR(PREUMS(jj:jj)).LE.ICHAR('Z'))).OR.
     *   ((ICHAR(PREUMS(jj:jj)).GE.ICHAR('0')).AND.
     *    (ICHAR(PREUMS(jj:jj)).LE.ICHAR('9'))).OR.
     *   ((ICHAR(PREUMS(jj:jj)).GE.ICHAR('a')).AND.
     *    (ICHAR(PREUMS(jj:jj)).LE.ICHAR('z')))) THEN
         longp=longp+1
         jj=jj+1
         GOTO 400
      ENDIF
C
C     - Recherche du numero de la premiere routine -
C
      DO l=1,nosub
         long=LENSUBR(l)
         IF(long.EQ.longp) THEN                   
            CALL TOUPPER(SUBR((l-1)*nn+1:(l-1)*nn+long),
     *                   long,TEMP)
            IF(TEMP(1:longp).EQ.PREUMS(1:longp)) THEN
               nfirst=l
               Goto 101
            ENDIF
         ENDIF
      ENDDO
 101  CONTINUE
C
      WRITE(*,*)'---------------------------------------------'
      WRITE(*,*)'               Tree METAFOR '
      WRITE(*,*)'---------------------------------------------'
      WRITE(*,*)
C
      ncurrent=nfirst
      niv=2
      remember(1)=nfirst
      NPOS(2)=1
      DO i=1,200
          TEMP(i:i)='_'
      ENDDO

 200  CONTINUE
C
         long=LENSUBR(ncurrent)
C
c         WRITE(*,*)'niv=',niv,' ncurrent=',ncurrent,
c     *             ' NPOS(niv)=',NPOS(niv),' rem=',remember(niv-1),
c     *             ' appel=',APPELS(ncurrent,NPOS(niv))
C
      ncu=ncurrent
      ni=niv-2
C                        - Affichage -

      IF((APPELS(remember(niv-1),NPOS(niv)).NE.0).OR.
     *   niv.EQ.2) THEN
         WRITE(*,*)TEMP(1:4*ni),SUBR((ncu-1)*nn+1:(ncu-1)*nn+long)
      ELSE
         IF(OPT2.EQ.1) THEN
            WRITE(*,*)TEMP(1:4*ni),'??????'
         ENDIF
      ENDIF
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
 215           IF(niv.EQ.3) GOTO 999
C              - on remonte a la routine appelante -
C              - on passe au suivant si possible -
               niv=niv-1
               NPOS(niv)=NPOS(niv)+1
C               ncurrent=APPELS(remember(niv-1),NPOS(niv)) 
               IF(NPOS(niv).GT.nocall(remember(niv-1))) THEN
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
               IF(OPT1.EQ.0) THEN
                  dejaf(ncurrent)=1
               ENDIF
C           - si on est a la derniere routine - 
            IF(NPOS(niv).EQ.nocall(remember(niv-1))) THEN
C              - si on est au niveau 2, c est fini ! -
 210           IF(niv.EQ.3) GOTO 999
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
C              - on marque la routine -
               IF(OPT1.EQ.0) THEN
                  dejaf(ncurrent)=1
                  IF(APPELS(ncurrent,1).NE.-1) THEN
                     SUBR((ncu-1)*nn+long+1:(ncu-1)*nn+long+3)='...'
                     LENSUBR(ncu)=LENSUBR(ncu)+3
                  ENDIF
               ENDIF
C              - on passe a la routine appelée -

               remember(niv)=ncurrent
               ncurrent=APPELS(ncurrent,1)
               niv=niv+1
               NPOS(niv)=1
         ENDIF
      GOTO 200          
C
C-----------------------------------------------------------------------
C     Routines qui n'ont rien à foutre là !
C-----------------------------------------------------------------------
C
 999  WRITE(*,*)
      WRITE(*,*)'Ces routines n ont rien à foutre là :'
      DO i=1,nosub
         IF(dejaf(i).EQ.0)THEN
            long=LENSUBR(i)
            WRITE(*,*)SUBR((i-1)*nn+1:(i-1)*nn+long)
         ENDIF
      ENDDO

      CONTINUE
      END
C
C
C
C========================================================================
C  Conversion d'une chaine en uppercase
C  rem : 0 < LONG < 50
C========================================================================
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
C
      RETURN
      END
C------------------------------------------------------------------------
