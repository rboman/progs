# 05 — Code Fortran

> Review du code original Fortran (src_f/).

---

## FORT-01 🔴 `use OMP_LIB` sans guard dans `mod_time.f90`

**Fichier** : [src_f/mod_time.f90](../src_f/mod_time.f90)

```fortran
module mod_time
    use OMP_LIB     ← sans directive conditionnelle !
    implicit none
    ...
end module mod_time
```

Le module `OMP_LIB` est inclus inconditionnellement. Sur un système sans OpenMP, cette instruction causera une **erreur de compilation** car le module n'existe pas.

Dans le corps du module, les appels OpenMP sont correctement protégés par la **sentinelle OpenMP** (`!$`) :
```fortran
!$  this%start_time = omp_get_wtime()
```
Mais le `use OMP_LIB` lui-même n'est pas protégé.

**Correction** : Utiliser un préprocesseur conditionnel ou la directive OpenMP sur le `use` :
```fortran
! Option 1 : uniquement si OpenMP est actif (extension GCC)
#ifdef _OPENMP
    use OMP_LIB
#endif
```

Le CMakeLists.txt du code Fortran n'utilise OpenMP que si disponible, donc en pratique ce bug ne se manifeste que si on compile sans OpenMP sur un système où le module n'est pas fourni par défaut. Sur certains systèmes, `OMP_LIB` est un module stub même sans `-fopenmp`.

---

## FORT-02 🟠 Fichier monolithique de 1800+ lignes

**Fichier** : [src_f/SPH_module.f90](../src_f/SPH_module.f90)

Tout le code Fortran principal (types de données, kernels, sorter, solver, I/O) est dans un seul fichier de plus de 1800 lignes. C'est difficile à naviguer et à maintenir. Le README le mentionne d'ailleurs dans les TODO :
> "fortran: faire plusieurs modules"

Le refactoring naturel serait :
- `mod_particle.f90` : types `fixed_particle`, `mobile_particle`
- `mod_sorter.f90` : type `particle_sort`
- `mod_manager.f90` : type `particle_manager`
- `mod_kernels.f90` : fonctions kernel
- `mod_eqstate.f90` : équations d'état

---

## FORT-03 🟠 Le module utilise `cpu_time` au lieu de `omp_get_wtime` pour les timers

**Fichier** : [src_f/mod_time.f90](../src_f/mod_time.f90)

```fortran
subroutine s_timer_start(this)
    !$  if (.true.) then
    !$      this%start_time = omp_get_wtime()   ← wall-clock time
    !$  else
            call cpu_time(this%start_time)        ← CPU time
    !$  end if
end subroutine
```

La sémantique des sentinelles `!$` implique que la branche `omp_get_wtime()` est utilisée **seulement si OpenMP est actif**. Sinon, `cpu_time` est utilisée.

`cpu_time` retourne le temps CPU *cumulé sur tous les threads*, ce qui fausse les mesures de performance d'un code parallèle (le temps total peut sembler plus grand que le temps de mur). Pour une mesure cohérente, `omp_get_wtime()` devrait être utilisé inconditionnellement si disponible.

---

## FORT-04 🟠 Tableau `vec_gradW` de taille fixe 150 aussi en Fortran

**Fichier** : [src_f/SPH_module.f90](../src_f/SPH_module.f90)

```fortran
real(DP), dimension(1:3, 1:150) :: vec_gradW
real(DP), dimension(1:3, 1:150) :: vec_gradW_mod
```

Même problème qu'en C++ (BUG-02). La limite de 150 voisins est hardcodée. Le Fortran permet les tableaux allouables, ce qui serait préférable :
```fortran
real(DP), dimension(:,:), allocatable :: vec_gradW
```

---

## FORT-05 🟠 Timers Fortran globaux (non encapsulés)

**Fichier** : [src_f/mod_time.f90](../src_f/mod_time.f90)

```fortran
type(s_timer) :: timer_total      
type(s_timer) :: timer_copy_vars     
type(s_timer) :: timer_initialisation
type(s_timer) :: timer_save
! ...
```

Les timers sont des **variables globales** du module, identique au problème de `g_timers` en C++. Une refactorisation en tableau ou en type structuré serait plus propre.

---

## FORT-06 🟡 `list_addElement` : réallocation par copie manuelle

**Fichier** : [src_f/SPH_module.f90](../src_f/SPH_module.f90)

```fortran
elseif(this%nbr == this%max_nbr) then
    allocate(temp_lst(1:this%max_nbr+this%incr))
    temp_lst(1:this%max_nbr) = this%lst(1:this%max_nbr)
    call move_alloc(temp_lst, this%lst)     ! [RB] c'est une fct fortran
    this%max_nbr = this%max_nbr + this%incr
end if
```

La gestion de liste dynamique est implémentée manuellement avec une stratégie d'incrément fixe (`incr = 35`). Un incrément fixe conduit à O(n²) allocations au lieu de O(n log n) avec une stratégie de doublement. Pour un grand nombre de voisins, c'est sous-optimal. En pratique (max ~150 voisins), le coût est négligeable.

---

## FORT-07 🟡 `particle_manager` : pas de gestion d'erreur sur les `read`

**Fichier** : [src_f/SPH_module.f90](../src_f/SPH_module.f90)

Les lectures depuis les fichiers d'entrée (`input.fp`, `input.mp`, `input.prm`) ne vérifient pas les codes de retour `iostat`. Un fichier corrompu ou incomplet provoquera une **erreur fatale** non gérée de Fortran au lieu d'un message d'erreur clair.

---

## FORT-08 🟡 `SPH_simulation.f90` : mesure du temps avec `system_clock` ET les timers maison

**Fichier** : [src_f/SPH_simulation.f90](../src_f/SPH_simulation.f90)

```fortran
call system_clock(t1, clock_rate, clock_max)
call timer_total%start()

call manager%initialisation()
call manager%solver()

call timer_total%stop()
call system_clock(t2, clock_rate, clock_max)

print *,'Elapsed real time = ', DBLE(t2-t1)/clock_rate
call time_print_stats()
```

Le temps total est mesuré **deux fois** : avec `system_clock` ET avec `timer_total`. L'une des deux mesures est redondante. De plus, `system_clock` peut revenir à zéro (`clock_max`) pour les simulations très longues.

---

## FORT-09 🟡 Précision du type déclaration

```fortran
real*8 :: start_time = 0.0d0
```

La syntaxe `real*8` est non standard Fortran (extension vendor). Le code utilise déjà `real(DP)` dans d'autres endroits. `real(REAL64)` du module `iso_fortran_env` serait la meilleure pratique.

---

## Résumé

Le code Fortran est essentiellement la transcription OOP (classes Fortran 2003) d'un algorithme bien défini. Sa principale faiblesse est structurelle (monolithique) et son manque de robustesse dans la gestion des erreurs. La logique physique elle-même est correcte et bien commentée.
