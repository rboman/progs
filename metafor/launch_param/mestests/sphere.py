# -*- coding: utf-8 -*-
# $Id:  $

#################################################
#   Thick-walled sphere under internal pressure #
#===============================================#
# - Elasto-viscoplastic material                #
# - Mixed, kinematic or isotropic hardening     #
# - Perzyna viscoplasticity                     #
#################################################.

#0. Entète                                # Elle est obligatoire et toujours la même !
#=====================================================================================

from __future__ import print_function
from __future__ import division
from builtins import range
from past.utils import old_div
from wrap import *                        #Importation des modules
import math

_metafor = None  #Analyse Metafor vide

#Interfaçage avec le logiciel Metafor 
#=====================================================================================
def getMetafor(_parameters={}): 
    global _metafor #Le mot clé "global" fait référence à la variable _metafor définie à la ligne 17.
                    #Sinon la variable _metafor est une variable locale à la fonction getMetafor(). 
    if _metafor == None: 
        _metafor = buildDomain(_parameters)     #Création d'une analyse Metafor si _metafor n'existe pas.
    return _metafor

# Construction du dictionnaire contenant les paramètres
#=====================================================================================
def getParameters(_parameters={}):

    parameters= {} #Dictionnaire vide
    #Un dictionaire est un conteneur associatif. Il définit un lien unique entre une clé de type string ('R1') et une valeur de type quelconque associée à cette clé (100).
    #Dès lors, l'accès à un valeur stockée dans le dictionnaire se fait au moyen de la clé et de l'opérateur [ ] : parameters['R1'].

    #1. Définition des paramètres
    #=============================
    
    #1.1 Géometrie                       #Paramètres de géométrie (; si plusieurs commandes sur la même ligne)
    #-------------
    parameters['R1'] = 100.                            #(mm)
    parameters['R2'] = 400.                

            
    #1.2 Maillage                        #Paramètres du maillage
    #-------------------------------------------------------------
    parameters['Nr']=20                     #Nombre de divisions radiales 
    parameters['No']=20                    #Nombre de divisions circonférentielles  

    #1.3 Discrétisation temporelle        
    #-------------------------------------------------------------------------

    parameters['DeltatMax'] = 0.1                  #Pas de temps maximum  (sec) 
    parameters['DeltatInit'] = 0.1                 #Pas de temps initial  (sec)  
    parameters['Archi']   = 1                      #Paramètre du nombre d'archivage 

    parameters['MaxNumberMechanicalIterations'] =10                         #Nombre d'itérations mécaniques par pas de temps
    parameters['ResidualMethodComputation'] =Method4ResidualComputation()   #Choix de la méthode pour adimensionnaliser le résidu de NR
    parameters['ResidualTolerance'] =1.0E-6                                 #Choix de la tolérance sur le résidu adimensionnel de NR  
    
    
    #1.4 Matériau                        #Paramètres matériau
    #----------------------------------------------------------------------
    parameters['Density']  = 8.190E-9                  #Densité (T/mm3) non obligatoire ici, car c'est une étude quasi-statique.
    parameters['Young'] = 200500.                      #MPa
    parameters['Nu'] = 0.3                              
    parameters['SigmaY_0'] = 200.                      #MPa
    parameters['SigmaY_Infty'] = 500.                  #MPa  
    parameters['Hard'] = 19000.                         #MPa
    parameters['Delta'] = 0.5
    parameters['Eta_Kin'] = 0.                         #Paramètre de recouvrance dynamique
    parameters['Visco_K'] = 0.                     #Paramètre de viscosité plastique du modèle de Persyna
    
    #CHOIX DU MATERIAU
    parameters['MaterialBehaviour'] = "ELASTOPLASTIC" #(ELASTOPLASTIC or ELASTOVISCOPLASTIC)
    parameters['HardeningModel'] = "NOHARDENING"        #(NOHARDENING, ISOTROPIC, KINEMATIC, MIXED)
    parameters['IsotropicHardening'] = "LINEAR"       #(LINEAR or NONLINEAR)
    parameters['KinematicHardening'] = "NONLINEAR"       #(LINEAR or NONLINEAR)
    
    #1.5 Chargement                   #Paramètres de la mise en charge
    #---------------------------------------------------------------------------------
    
    parameters['Umax'] = 0.8                   #Déplacement radial imposé maximum (mm)
    parameters['TChaU'] = 1.0                        #Durée de mise en charge (sec) 
    parameters['NCycle'] = 1                        #Nombre de cycles de charge-décharge

    parameters.update(_parameters)  #Mise à jour des paramètres 

    #On ajoute au dictionnaire "parameters" les clés et leurs valeurs associées définies dans "_parameters".
    #C'est une opération d'inclusion : 
    #Si une clé du dictionnaire "_parameters" n'existe pas dans le dictionnaire "parameters", la clé est créée et la valeur associée est copiée dedans. 
    #Si une clé du dictionnaire "_parameters" existe déjà dans le dictionnaire "parameters", la valeur associée est copiée dans la clé commune. 
    return parameters  


# Construction du domaine 
#=====================================================================================
def buildDomain(_parameters={}):

    #Ouverture du domaine 
    #===============================================================
    metafor = Metafor() #Création d'une analyse Metafor
    parameters  = getParameters(_parameters) #Récupération des paramètres de l'analysis
    domain = metafor.getDomain() #Accéder au domaine
    
       
    #2. Définition de la géométrie
    #===============================================================

    geometry = domain.getGeometry() #Accéder à la géométrie
    geometry.setDimAxisymmetric()   #Modélisation axisymétrique

    #2.1 Points
    #---------------------------------------------------------------
    pointset = geometry.getPointSet()  #Accéder aux points
    #Définition des sommets du secteur annulaire
    P1 = pointset.define( 1,  parameters['R1'],  0.,  0.)
    P2 = pointset.define( 2,  parameters['R2'],  0.,  0.)
    P3 = pointset.define( 3,  parameters['R2']*math.cos(math.pi/4.0),  parameters['R2']*math.sin(math.pi/4.0),   0.)
    P4 = pointset.define( 4,  0.,  parameters['R2'],   0.)
    P5 = pointset.define( 5,  0.,  parameters['R1'],   0.)
    P6 = pointset.define( 6,  parameters['R1']*math.cos(math.pi/4.0),  parameters['R1']*math.sin(math.pi/4.0),   0.)
    
    #Axe de révolution
    Axis1 = pointset.define( 7,  0.,  0.,   0.)
    Axis2 = pointset.define( 8,  0.,  0.,   1.)
    
    #2.2 Courbes
    #--------------------------------------------------------------
    curveset = geometry.getCurveSet()  #Accéder aux courbes
    #Définition des côtés du secteur annulaire 
    C1 = curveset.add( Line( 1, P1, P2) )
    C2 = curveset.add( Arc( 2, P2, P3, P4) )
    C3 = curveset.add( Line( 3, P4, P5) )
    C4 = curveset.add( Arc( 4, P5, P6, P1) )

    #2.3 Contours
    #-----------------------------------------------------------------
    wireset = geometry.getWireSet()  #Accéder aux contours
    W1 = wireset.add(Wire(1, [curveset(1), curveset(2), curveset(3), curveset(4)])) #Définir le contour 1 composé des lignes 1 à 4
                                                                               #!SENS ANTIHORLOGIQUE!
    #2.4 Surface
    #----------------------------------------------------------------
    surfaceset = geometry.getSurfaceSet()  #Accéder aux surfaces
    #Définir un plan d'origine point 1 et de normale définie par le produit vectoriel entre le vecteur 
    #reliant le point 7 et le point 2 et celui reliant le point 7 et le point 4.
    Surf1 = surfaceset.add(Plane(1, Axis1, P2, P4))
    
    #2.5 Face à mailler
    #--------------------------------------------------------------------------
    sideset = geometry.getSideSet()  #Accéder aux faces
    #Définir la face 1 (partie maillée plus tard) composée de l'intérieur du contour 1 et de la surface plane 1
    S1 = sideset.add(Side(1,[wireset(1)]))
    S1.setSurface(Surf1)                                      
    
    #3. Maillage
    #============
    
    #3.1 Définition du nombre de mailles sur chaque courbe  (2 côtés en vis a vis ont le même nombre de mailles)
    #-----------------------------------------------------
    SimpleMesher1D(C1).execute(parameters['Nr'])                               #Nr mailles sur la courbe 1
    SimpleMesher1D(C2).execute(parameters['No'])                               #No mailles sur la courbe 2
    SimpleMesher1D(C3).execute(parameters['Nr'])                               #Nr mailles sur la courbe 3
    SimpleMesher1D(C4).execute(parameters['No'])                               #No mailles sur la courbe 4

    #3.2 Définition de la face à mailler
    #------------------------------------
    TransfiniteMesher2D(S1).execute2( (1,2,3,4) )  #Indiquer quelles lignes correspondent aux côtés du maillage de la face 1  

    #4. Lois de comportement
    #========================
    
    #4.1 Définition du matériau et #4.2 Définition des lois d'écrouissage
    #--------------------------------------------------------------------
    
    materset = domain.getMaterialSet()  #Accéder aux comportements du matériau
    lawset = domain.getMaterialLawSet()  #Accéder aux lois matérielles
    
    if (parameters['MaterialBehaviour'] == "ELASTOPLASTIC"): 
        if(parameters['HardeningModel'] == "NOHARDENING"):
            material1 = materset.define (1, EvpIsoHHypoMaterial)    #Créer un matériau élasto-plastique numéro 1 à écrouissage isotrope
            material1.put(MASS_DENSITY,    parameters['Density'])   #Masse volumique
            material1.put(ELASTIC_MODULUS, parameters['Young'])     #Module de Young
            material1.put(POISSON_RATIO,   parameters['Nu'])        #Coefficient de Poisson
            material1.put(YIELD_NUM,1)                #Numéro de la loi d'écrouissage isotrope  1
            
            lawset1 = lawset.define(1, LinearIsotropicHardening)  #Ecrouissage isotrope linéaire 1
            lawset1.put(IH_SIGEL,   parameters['SigmaY_0'])
            lawset1.put(IH_H,       0.0)              #Sans Ecrouissage : Hard = 0.0
            
            print("Elasto-plastic material with no hardening")
        elif(parameters['HardeningModel'] == "ISOTROPIC"):
            material1 = materset.define (1, EvpIsoHHypoMaterial)    #Créer un matériau élasto-plastique numéro 1 à écrouissage isotrope
            material1.put(MASS_DENSITY,    parameters['Density'])   #Masse volumique
            material1.put(ELASTIC_MODULUS, parameters['Young'])     #Module de Young
            material1.put(POISSON_RATIO,   parameters['Nu'])        #Coefficient de Poisson
            material1.put(YIELD_NUM,1)                #Numéro de la loi d'écrouissage isotrope  1
            
            if(parameters['IsotropicHardening'] == "LINEAR"): 
                lawset1 = lawset.define(1, LinearIsotropicHardening)                    #Ecrouissage isotrope linéaire 1
                lawset1.put(IH_SIGEL,   parameters['SigmaY_0'])
                lawset1.put(IH_H,       parameters['Hard'])
                
                print("Elasto-plastic material with a linear isotropic hardening")
            elif(parameters['IsotropicHardening'] == "NONLINEAR"): 
                lawset1 = lawset.define(1, SaturatedIsotropicHardening)                 #Ecrouissage isotrope non-linéaire de Voce 1
                lawset1.put(IH_SIGEL,parameters['SigmaY_0'])
                lawset1.put(IH_Q, parameters['SigmaY_Infty']-parameters['SigmaY_0'])
                lawset1.put(IH_KSI,old_div(parameters['Hard'],(parameters['SigmaY_Infty']-parameters['SigmaY_0'])))
                
                print("Elasto-plastic material with a non-linear isotropic hardening")
            else:
                raise Exception('Unknown Isotropic Hardening =', IsotropicHardening)
        elif(parameters['HardeningModel'] == "KINEMATIC"):
            material1 = materset.define (1, EvpMixtHHypoMaterial)   #Créer un matériau élasto-plastique numéro 1 à écrouissage mixte
            material1.put(MASS_DENSITY,    parameters['Density'])   #Masse volumique
            material1.put(ELASTIC_MODULUS, parameters['Young'])     #Module de Young
            material1.put(POISSON_RATIO,   parameters['Nu'])        #Coefficient de Poisson
            material1.put(YIELD_NUM,1)                #Numéro de la loi d'écrouissage isotrope  (1)
            material1.put(KH_NB,1)                    #Nombre d'écrouissages cinématiques
            material1.put(KH_NUM1,2)                  #Numéro de la loi d'écrouissage cinématique (2)
            
            lawset1 = lawset.define(1, LinearIsotropicHardening)  #Ecrouissage isotrope linéaire 1
            lawset1.put(IH_SIGEL,   parameters['SigmaY_0'])
            lawset1.put(IH_H,       0.0)              #Sans Ecrouissage Isotrope : Hard_Iso = 0.0
            if(parameters['KinematicHardening'] == "LINEAR"): 
                lawset2 = lawset.define(2, DruckerPragerKinematicHardening)     #Ecrouissage cinématique linéaire de Drucker Prager  2                                                        
                lawset2.put(KH_H, parameters['Hard'])
                
                print("Elasto-plastic material with a linear kinematic hardening")
            elif(parameters['KinematicHardening'] == "NONLINEAR"): 
                lawset2 = lawset.define(2, ArmstrongFrederickKinematicHardening)     #Ecrouissage cinématique non-linéaire d'Armstrong Frédérick 2                                                          
                lawset2.put(KH_H, parameters['Hard'])
                lawset2.put(KH_B, parameters['Eta_Kin'])
                
                print("Elasto-plastic material with a non-linear kinematic hardening")
            else:
                raise Exception('Unknown Kinematric Hardening =', KinematicHardening)
        elif parameters['HardeningModel'] == "MIXED" :
            material1 = materset.define (1, EvpMixtHHypoMaterial)   #Créer un matériau élasto-viscoplastique numéro 1 à écrouissage mixte
            material1.put(MASS_DENSITY,    parameters['Density'])   #Masse volumique
            material1.put(ELASTIC_MODULUS, parameters['Young'])     #Module de Young
            material1.put(POISSON_RATIO,   parameters['Nu'])        #Coefficient de Poisson
            material1.put(YIELD_NUM,1)                #Numéro de la loi d'écrouissage isotrope  (1)
            material1.put(KH_NB,1)                    #Nombre d'écrouissages cinématiques
            material1.put(KH_NUM1,2)                  #Numéro de la loi d'écrouissage cinématique (2)
            
            #Définition des coefficients de pondération des écrouissages isotropes et cinématiques
            
            Hard_Iso = parameters['Hard']*(1.0-parameters['Delta'])                   
            Hard_Kin = parameters['Hard']*parameters['Delta']   
            
            if(parameters['IsotropicHardening'] == "LINEAR"): 
                lawset1 = lawset.define(1, LinearIsotropicHardening)                    #Ecrouissage isotrope linéaire 1
                lawset1.put(IH_SIGEL,   parameters['SigmaY_0'])
                lawset1.put(IH_H,       Hard_Iso)
                
                if(parameters['KinematicHardening'] == "LINEAR"): 
                    lawset2 = lawset.define(2, DruckerPragerKinematicHardening)     #Ecrouissage cinématique linéaire de Drucker Prager  2                                                        
                    lawset2.put(KH_H, Hard_Kin)
                    
                    print("Elasto-plastic material with a mixte hardening : linear isotropic hardening and linear kinematic hardening")
                elif(parameters['KinematicHardening'] == "NONLINEAR"): 
                    lawset2 = lawset.define(2, ArmstrongFrederickKinematicHardening)     #Ecrouissage cinématique non-linéaire d'Armstrong Frédérick 2                                                          
                    lawset2.put(KH_H, Hard_Kin)
                    lawset2.put(KH_B, parameters['Eta_Kin'])
                    
                    print("Elasto-plastic material with a mixte hardening : linear isotropic hardening and non-linear kinematic hardening")
                else:
                    raise Exception('Unknown Kinematric Hardening =', KinematicHardening)
            elif(parameters['IsotropicHardening'] == "NONLINEAR"):
                lawset1 = lawset.define(1, SaturatedIsotropicHardening)                 #Ecrouissage isotrope non-linéaire de Voce 1
                lawset1.put(IH_SIGEL,   parameters['SigmaY_0'])
                lawset1.put(IH_Q, parameters['SigmaY_Infty']-parameters['SigmaY_0'])
                lawset1.put(IH_KSI,old_div(Hard_Iso,(parameters['SigmaY_Infty']-parameters['SigmaY_0'])))
                if(parameters['KinematicHardening'] == "LINEAR"): 
                
                    lawset2 = lawset.define(2, DruckerPragerKinematicHardening)     #Ecrouissage cinématique linéaire de Drucker Prager  2                                                        
                    lawset2.put(KH_H, Hard_Kin)
                    
                    print("Elasto-plastic material with a mixte hardening : non-linear isotropic hardening and linear kinematic hardening")
                elif(parameters['KinematicHardening'] == "NONLINEAR"): 
                
                    lawset2 = lawset.define(2, ArmstrongFrederickKinematicHardening)     #Ecrouissage cinématique non-linéaire d'Armstrong Frédérick 2                                                          
                    lawset2.put(KH_H, Hard_Kin)
                    lawset2.put(KH_B, parameters['Eta_Kin'])
                    
                    print("Elasto-plastic material with a mixte hardening : non-linear isotropic hardening and non-linear kinematic hardening")
                else:
                    raise Exception('Unknown Kinematric Hardening =', KinematicHardening)
            else:
                raise Exception('Unknown Isotropic Hardening =', IsotropicHardening)
        else: 
            raise Exception('Unknown Hardening Model =', HardeningModel)
    elif(parameters['MaterialBehaviour'] == "ELASTOVISCOPLASTIC"): 
        lawset3 = lawset.define (3, PerzynaYieldStress)                         #Loi de viscosité plastique de Perzyna 3
        lawset3.put(PERZYNA_K, parameters['Visco_K'])
        lawset3.put(PERZYNA_M, 1.)
        lawset3.put(PERZYNA_N, 0.)
        lawset3.put(IH_NUM, 1)                                   #Numéro de l'écrouissage isotrope (1)
        if(parameters['HardeningModel'] == "NOHARDENING"):
            material1 = materset.define (1, EvpIsoHHypoMaterial)    #Créer un matériau élasto-viscoplastique numéro 1 à écrouissage isotrope
            material1.put(MASS_DENSITY,    parameters['Density'])   #Masse volumique
            material1.put(ELASTIC_MODULUS, parameters['Young'])     #Module de Young
            material1.put(POISSON_RATIO,   parameters['Nu'])        #Coefficient de Poisson
            material1.put(YIELD_NUM,3)                #Numéro de la loi de viscosité plastique (3)
            
            lawset1 = lawset.define(1, LinearIsotropicHardening)  #Ecrouissage isotrope linéaire 1
            lawset1.put(IH_SIGEL,   parameters['SigmaY_0'])
            lawset1.put(IH_H,       0.0)              #Sans Ecrouissage : Hard = 0.0
            
            print("Elasto-viscoplastic material with no hardening")
        elif(parameters['HardeningModel'] == "ISOTROPIC"):
            material1 = materset.define (1, EvpIsoHHypoMaterial)    #Créer un matériau élasto-viscoplastique numéro 1 à écrouissage isotrope
            material1.put(MASS_DENSITY,    parameters['Density'])   #Masse volumique
            material1.put(ELASTIC_MODULUS, parameters['Young'])     #Module de Young
            material1.put(POISSON_RATIO,   parameters['Nu'])        #Coefficient de Poisson
            material1.put(YIELD_NUM,3)                #Numéro de la loi de viscosité plastique (3)
            
            if(parameters['IsotropicHardening'] == "LINEAR"): 
                lawset1 = lawset.define(1, LinearIsotropicHardening)                    #Ecrouissage isotrope linéaire 1
                lawset1.put(IH_SIGEL,   parameters['SigmaY_0'])
                lawset1.put(IH_H,       parameters['Hard'])
                
                print("Elasto-viscoplastic material with a linear isotropic hardening")
                
            elif(parameters['IsotropicHardening'] == "NONLINEAR"): 
                lawset1 = lawset.define(1, SaturatedIsotropicHardening)                 #Ecrouissage isotrope non-linéaire de Voce 1
                lawset1.put(IH_SIGEL,parameters['SigmaY_0'])
                lawset1.put(IH_Q, parameters['SigmaY_Infty']-parameters['SigmaY_0'])
                lawset1.put(IH_KSI,old_div(parameters['Hard'],(parameters['SigmaY_Infty']-parameters['SigmaY_0'])))
                
                print("Elasto-viscoplastic material with a non-linear isotropic hardening")
                
            else:
                raise Exception('Unknown Isotropic Hardening =', IsotropicHardening)
        elif(parameters['HardeningModel'] == "KINEMATIC"):
            material1 = materset.define (1, EvpMixtHHypoMaterial)   #Créer un matériau élasto-viscoplastique numéro 1 à écrouissage mixte
            material1.put(MASS_DENSITY,    parameters['Density'])   #Masse volumique
            material1.put(ELASTIC_MODULUS, parameters['Young'])     #Module de Young
            material1.put(POISSON_RATIO,   parameters['Nu'])        #Coefficient de Poisson
            material1.put(YIELD_NUM,3)                #Numéro de la loi de viscosité plastique (3)
            material1.put(KH_NB,1)                    #Nombre d'écrouissages cinématiques
            material1.put(KH_NUM1,2)                  #Numéro de la loi d'écrouissage cinématique (2)
            
            lawset1 = lawset.define(1, LinearIsotropicHardening)  #Ecrouissage isotrope linéaire 1
            lawset1.put(IH_SIGEL,   parameters['SigmaY_0'])
            lawset1.put(IH_H,       0.0)              #Sans Ecrouissage Isotrope : Hard_Iso = 0.0
            
            if(parameters['KinematicHardening'] == "LINEAR"): 
                lawset2 = lawset.define(2, DruckerPragerKinematicHardening)     #Ecrouissage cinématique linéaire de Drucker Prager  2                                                        
                lawset2.put(KH_H, parameters['Hard'])
                
                print("Elasto-viscoplastic material with a linear kinematic hardening")
                
            elif(parameters['KinematicHardening'] == "NONLINEAR"): 
                lawset2 = lawset.define(2, ArmstrongFrederickKinematicHardening)     #Ecrouissage cinématique non-linéaire d'Armstrong Frédérick 2                                                          
                lawset2.put(KH_H, parameters['Hard'])
                lawset2.put(KH_B, parameters['Eta_Kin'])
                
                print("Elasto-viscoplastic material with a non-linear kinematic hardening")
                
            else:
                raise Exception('Unknown Kinematric Hardening =', KinematicHardening)
        elif parameters['HardeningModel'] == "MIXED" :
            material1 = materset.define (1, EvpMixtHHypoMaterial)   #Créer un matériau élasto-viscoplastique numéro 1 à écrouissage mixte
            material1.put(MASS_DENSITY,    parameters['Density'])   #Masse volumique
            material1.put(ELASTIC_MODULUS, parameters['Young'])     #Module de Young
            material1.put(POISSON_RATIO,   parameters['Nu'])        #Coefficient de Poisson
            material1.put(YIELD_NUM,3)                #Numéro de la loi de viscosité plastique (3)
            material1.put(KH_NB,1)                    #Nombre d'écrouissages cinématiques
            material1.put(KH_NUM1,2)                  #Numéro de la loi d'écrouissage cinématique (2)
            
            #Définition des coefficients de pondération des écrouissages isotropes et cinématiques
            
            Hard_Iso = parameters['Hard']*(1.0-parameters['Delta'])                   
            Hard_Kin = parameters['Hard']*parameters['Delta']   
            
            if(parameters['IsotropicHardening'] == "LINEAR"): 
                lawset1 = lawset.define(1, LinearIsotropicHardening)                    #Ecrouissage isotrope linéaire 1
                lawset1.put(IH_SIGEL,   parameters['SigmaY_0'])
                lawset1.put(IH_H,       Hard_Iso)
                
                if(parameters['KinematicHardening'] == "LINEAR"): 
                    lawset2 = lawset.define(2, DruckerPragerKinematicHardening)     #Ecrouissage cinématique linéaire de Drucker Prager  2                                                        
                    lawset2.put(KH_H, Hard_Kin)
                    
                    print("Elasto-viscoplastic material with a mixte hardening : linear isotropic hardening and linear kinematic hardening")
                    
                elif(parameters['KinematicHardening'] == "NONLINEAR"): 
                    lawset2 = lawset.define(2, ArmstrongFrederickKinematicHardening)     #Ecrouissage cinématique non-linéaire d'Armstrong Frédérick 2                                                          
                    lawset2.put(KH_H, Hard_Kin)
                    lawset2.put(KH_B, parameters['Eta_Kin'])
                    
                    print("Elasto-viscoplastic material with a mixte hardening : linear isotropic hardening and non-linear kinematic hardening")
                    
                else:
                    raise Exception('Unknown Kinematric Hardening =', KinematicHardening)
                
            elif(parameters['IsotropicHardening'] == "NONLINEAR"):
                lawset1 = lawset.define(1, SaturatedIsotropicHardening)                 #Ecrouissage isotrope non-linéaire de Voce 1
                lawset1.put(IH_SIGEL,   parameters['SigmaY_0'])
                lawset1.put(IH_Q, parameters['SigmaY_Infty']-parameters['SigmaY_0'])
                lawset1.put(IH_KSI,old_div(Hard_Iso,(parameters['SigmaY_Infty']-parameters['SigmaY_0'])))
                
                if(parameters['KinematicHardening'] == "LINEAR"): 
                    lawset2 = lawset.define(2, DruckerPragerKinematicHardening)     #Ecrouissage cinématique linéaire de Drucker Prager  2                                                        
                    lawset2.put(KH_H, Hard_Kin)
                    
                    print("Elasto-viscoplastic material with a mixte hardening : non-linear isotropic hardening and linear kinematic hardening")
                    
                elif(parameters['KinematicHardening'] == "NONLINEAR"): 
                    lawset2 = lawset.define(2, ArmstrongFrederickKinematicHardening)     #Ecrouissage cinématique non-linéaire d'Armstrong Frédérick 2                                                          
                    lawset2.put(KH_H, Hard_Kin)
                    lawset2.put(KH_B, parameters['Eta_Kin'])
                    
                    print("Elasto-viscoplastic material with a mixte hardening : non-linear isotropic hardening and non-linear kinematic hardening")
                    
                else:
                    raise Exception('Unknown Kinematric Hardening =', KinematicHardening)
            
            else:
                raise Exception('Unknown Isotropic Hardening =', IsotropicHardening)
        else: 
            raise Exception('Unknown Hardening Model =', HardeningModel)
    else : 
        raise Exception('Unknown Material Behaviour =', MaterialBehavour)

    #5. Définition des éléments finis volumiques
    #==============================================================================
    
    #5.1 Définition des propriétés des éléments finis volumiques
    #------------------------------------------------------------------------------
    prp1 = ElementProperties(Volume2DElement)                     #Créer une propriété prp1 pour les éléments finis volumiques
                                                                  #et définir le type d'éléments finis
                                                                  
    prp1.put(MATERIAL, 1)                                           #Numéro du matériau associé aux éléments finis
    prp1.put(STIFFMETHOD, STIFF_ANALYTIC)                           #Méthode de calcul de la matrice de raideur tangente 
    prp1.put(CAUCHYMECHVOLINTMETH, VES_CMVIM_SRIPR)                 #Sous intégration sélective avec report de pression
    
    #5.2 Génération des éléments finis volumiques sur le maillage
    #-------------------------------------------------------------------------------
    interactionset = domain.getInteractionSet()  #Accéder à la gestion des interactions
    
    app1 = FieldApplicator(1)                                       #Création d'un générateur (numéro 1) d'éléments finis volumiques
    app1.push(sideset(1))                                           #Support = face 1
    app1.addProperty(prp1)                                          #Assigner la propriété prp1 aux futurs éléments volumiques de la face 1
    interactionset.add(app1)                                        #Ajouter le générateur 1 à la liste des interactions
    
    #6. Fixations
    #==============================================================================
    loadingset=domain.getLoadingSet()  #Accéder à la gestion des Dofs imposés
    
    #On bloque le déplacement normal sur les plans de symétrie horizontale et verticale de la sphère : 
    fix1 = loadingset.define(C1,Field1D(TY,RE),0.)
    fix2 = loadingset.define(C3,Field1D(TX,RE),0.)
    
    #7. Chargement (par un déplacement imposé)
    #===============================================================================
    
    fctU = PieceWiseLinearFunction()    #Fonction d'amplitude unitaire décrivant l'évolution temporelle de la mise en charge
    fctU.setData(0., 0. )                 
    
    #Définition d'une fonction en dents de scie sur NCycle
    for i in range (parameters['NCycle']):               #i in [0 1 ... NCycle-2 NCycle-1]          
        fctU.setData((4*i+1)*parameters['TChaU'],  1.)
        fctU.setData((4*i+3)*parameters['TChaU'],  -1.)
        fctU.setData((4*i+4)*parameters['TChaU'], 0.) 

    #On impose un déplacement radial sur la face interne de la sphere : u_r(t)=Umax*fctU :
    loadingset.defineRad(C4, Field3D(TXTYTZ,RE), Axis1, Axis2, parameters['Umax'], fctU)
    
    #8. Schéma d'intégration temporelle
    #==================================
    
    #8.1 Gestion des pas de temps
    #-----------------------------
    tsm = metafor.getTimeStepManager()                              #Accéder à la gestion des pas de temps
    
    tsm.setInitialTime(0.0,   parameters['DeltatInit'])                #Commencer en t=0 avec un pas de temps initial = DeltatInit
    for i in range (parameters['NCycle']*4):                                         #i in [0 1 ... 4*NCycle-2 4*NCycle-1]
       tsm.setNextTime((i+1)*parameters['TChaU'], parameters['Archi'], parameters['DeltatMax'])                          
    #Aller jusqu'à t=(i+1)*TChaU (séquences principales d'un cycle de charge et décharge) en archivant "Archi" résultats sur cette séquence et 
    #avec un pas de temps maximal de DeltatMax. Si Archi = 1, on archive les instants aux extrêmités de chaque intervalle de temps [ti ti+1].
    #Par contre si Archi = 2, on archive les instants aux extrêmités de chaque intervalle de temps [ti ti+1] et on force le passage de l'algorithme à l'instant 
    #(ti+1-ti)/2 pour archiver les résultats à cet instant.
        
    #8.2 Gestion des itérations mécaniques
    #----------------------------------------------
    mim = metafor.getMechanicalIterationManager()   #Accéder à la gestion des itérations mécaniques
    
    mim.setMaxNbOfIterations(parameters['MaxNumberMechanicalIterations'])              #Nombre d'itérations mécaniques par pas de temps
    mim.setResidualTolerance(parameters['ResidualTolerance'])                          #Précision sur le résidu du NR de 1.E-6
    mim.setResidualComputationMethod(parameters['ResidualMethodComputation'])          #Méthode pour adimensionnaliser le résidu
    
        
    #8.3 Choix du schéma d'intégration temporelle
    #---------------------------------------------
    metafor.setIntegerData(MDE_NDYN, 0)  #Analyse quasi-statique (facultatif car valeur par défaut)
    
    #9. Archivage des courbes sur le disque dur
    #==========================================
    
    valuesmanager = metafor.getValuesManager()          #Accéder à la gestion des courbes
    
    valuesmanager.add(1, MiscValueExtractor(metafor,EXT_T),'time')
    #Archiver le temps en courbe numéro 1 et nommer time le fichier d'archivage du temps
    valuesmanager.add(2, IFNodalValueExtractor(P1, IF_CRITERION),'SigmaVMInt' )
    #Archiver la contrainte équivalente de Von Mises extrapolée au noeud inférieur gauche en courbe numéro 2
    #et nommer SigmaVMInt le fichier d'archivage de la grandeur aux différents instants
    valuesmanager.add(3, IFNodalValueExtractor(P2, IF_CRITERION),'SigmaVMExt' )
    #Archiver la contrainte équivalente de Von Mises extrapolée au noeud inférieur droit en courbe numéro 3
    #et nommer SigmaVMInt le fichier d'archivage de la grandeur aux différents instants
    valuesmanager.add(4, DbNodalValueExtractor(P1, Field1D(TX,RE)),'u' )
    #Archiver le déplacement radial au noeud inférieur gauche en courbe numéro 4
    #et nommer u le fichier d'archivage de la grandeur aux différents instants    
    valuesmanager.add(5, DbNodalValueExtractor(C3, Field1D(TX,GF1)), SumOperator(),'R' )
    #Archiver la force de réaction à le plan de coupe verticale de la sphere en courbe numéro 5
    #et nommer R le fichier d'archivage de la grandeur aux différents instants
    valuesmanager.add(6, IFNodalValueExtractor(P1, IF_EPL),'EPLInt' )
    #Archiver la déformation plastique équivalente extrapolée au noeud inférieur gauche en courbe numéro 6
    #et nommer EPLInt le fichier d'archivage de la grandeur aux différents instants
    valuesmanager.add(7, IFNodalValueExtractor(P2, IF_EPL),'EPLExt' )
    #Archiver la déformation plastique équivalente extrapolée au noeud inférieur droit en courbe numéro 7
    #et nommer EPLExt le fichier d'archivage de la grandeur aux différents instants


                                                        
    #10. Visualisation en temps réel des courbes            (FACULTATIF mais utile)
    #===========================================
    # On met les commandes de visualisation entre try et except pour gérer automatiquement les cas 
    # où le logiciel Metafor n'a pas été compilé avec la visualisation et ne possède donc pas l'objet VizWin.
    
    try:
        #Demander d'ouvrir une fenêtre VizWin avec deux graphes : 
        #La courbe 1 nommé SigmaVMInt affiche les valeurs archivées pour la courbe 2 (SigmaVM au noeud inférieur gauche) 
        #en fonction de celles archivées pour la courbe 1 (temps); 
        #La courbe 2 nommé SigmaVMExt affiche les valeurs archivées pour la courbe 3 (SigmaVM au noeud inférieur droit)
        #en fonction de celles pour la courbe 1 (temps); 
        
        dataCurve1 = VectorDataCurve(1, valuesmanager.getDataVector(1), valuesmanager.getDataVector(2),'SigmaVMInt')
        dataCurve2 = VectorDataCurve(2, valuesmanager.getDataVector(1), valuesmanager.getDataVector(3),'SigmaVMExt')
        dataCurveSet1 = DataCurveSet()                     
        dataCurveSet1.add(dataCurve1)                     
        dataCurveSet1.add(dataCurve2)                      
        winc1 = VizWin()                                   
        winc1.add(dataCurveSet1)                           
        metafor.addObserver(winc1)                        
    
        #Demander d'ouvrir une nouvelle fenêtre VizWin avec un graphe : 
        #La courbe 1 nommé R affiche les valeurs archivées pour la courbe 5 (Force de réaction totale) 
        #en fonction de celles archivées pour la courbe 1 (temps); 
        
        dataCurve3 = VectorDataCurve(3, valuesmanager.getDataVector(1), valuesmanager.getDataVector(5),'R')
        dataCurveSet2 = DataCurveSet()                     
        dataCurveSet2.add(dataCurve3)                       
        winc2 = VizWin()                                   
        winc2.add(dataCurveSet2)
        metafor.addObserver(winc2)
        
        #Demander d'ouvrir une nouvelle fenêtre VizWin avec un graphe : 
        #La courbe 1 nommé u affiche les valeurs archivées pour la courbe 4 (Déplacement radial imposé au noeud inférieur gauche) 
        #en fonction de celles archivées pour la courbe 1 (temps);     
        
        dataCurve4 = VectorDataCurve(4, valuesmanager.getDataVector(1), valuesmanager.getDataVector(4),'u')
        dataCurveSet3 = DataCurveSet()                     
        dataCurveSet3.add(dataCurve4)                       
        winc3 = VizWin()                                  
        winc3.add(dataCurveSet3)
        metafor.addObserver(winc3)
        
        #Demander d'ouvrir une nouvelle fenêtre VizWin avec deux graphes : 
        #La courbe 1 nommé EPLInt affiche les valeurs archivées pour la courbe 6 (EPL au noeud inférieur gauche) 
        #en fonction de celles archivées pour la courbe 1 (temps); 
        #La courbe 2 nommé EPLExt affiche les valeurs archivées pour la courbe 7 (EPL au noeud inférieur droit)
        #en fonction de celles pour la courbe 1 (temps); 
        
        dataCurve5 = VectorDataCurve(5, valuesmanager.getDataVector(1), valuesmanager.getDataVector(6),'EPLInt' )
        dataCurve6 = VectorDataCurve(6, valuesmanager.getDataVector(1), valuesmanager.getDataVector(7),'EPLExt' )
        dataCurveSet4 = DataCurveSet()                     
        dataCurveSet4.add(dataCurve5)                      
        dataCurveSet4.add(dataCurve6)                      
        winc4 = VizWin()                                   
        winc4.add(dataCurveSet4)                           
        metafor.addObserver(winc4)                          
        
    except NameError:
        pass
    
    #L'objet TestSuiteChecker de Metafor écrit par défaut un certain nombre de grandeurs caractéristiques du déroulement du calcul, à la fin du calcul. 
    #Il est possible d'ajouter des valeurs spécifiques extraites du problème (mesures caractéristiques du problème). 
    #La personnalisation du TestSuiteChecker au problème se fait à l'aide des fonctions suivantes :     
    testSuite = metafor.getTestSuiteChecker()
    testSuite.checkExtractor(2)                         #Affiche à l'écran la valeur finale de la courbe archivée 2 (SigmaVMInt)
    testSuite.checkExtractor(5)                         #Affiche à l'écran la valeur finale de la courbe archivée 5 (Force de réaction totale)                                
    
    return metafor