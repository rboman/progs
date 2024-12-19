PROGRAM TemporarilyDisableOutput
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT
    IMPLICIT NONE
    PRINT *, "Demarrage..."

    ! Étape 2 : Réouvrir l'unité 6 pour rediriger la sortie vers "NUL"
    OPEN(UNIT=OUTPUT_UNIT, FILE='NUL')
  
    ! Test : Exécuter une routine silencieuse
    PRINT *, "Cette sortie est redirigee vers NUL (print)" ! ne fonctionne pas
    write(OUTPUT_UNIT,*) "Cette sortie est redirigee vers NUL (write)" ! fonctionne

    ! Étape 3 : Réaffecter l'unité 6 à la sortie standard
    CLOSE(UNIT=OUTPUT_UNIT)
    OPEN(UNIT=OUTPUT_UNIT, FILE='CON')
  
    ! Test : Vérifier que la sortie est réactivée
    PRINT *, "La sortie ecran est reactivee !"
  
  END PROGRAM TemporarilyDisableOutput
  