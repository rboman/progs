# generer le vcproj:
#     qmake -tp vc mtLicense.pro
#
# Changer a la main dans Visual : 
#    dupliquer le projet pour en faire un x64 si nécessaire
#    [All]   : Ignore Import Library : "No" => "Yes"
#    Vérifier les dépendences de Metafor, MetaforGUI et _mtViz
#    Powergrep: ..\..\..\..\local64\qt-4.3.2 => $(MYLIBS)\qt-4.3.2
#    Powergrep: E:\local64\qt-4.3.2 => $(MYLIBS)\qt-4.3.2
#

TEMPLATE    = lib
LANGUAGE    = C++

META_DIR = ../../../oo_meta
OOFE_DIR = ../../../oofelie

DEFINES += HAVE_CONFIG_H MTLICENSE_EXPORTS

VPATH = $$META_DIR/mtLicense

INCLUDEPATH += ..

HEADERS += LicClient.h

SOURCES += LicClient.cpp

FORMS = 

INCLUDEPATH += $$META_DIR/mtGlobal

CONFIG  += dll 
CONFIG  += qt 
CONFIG  += debug_and_release 
CONFIG  += thread exceptions rtti stl warn_on 

QT += network 
QT -= gui

!debug_and_release|build_pass {
    CONFIG(debug, debug|release) {
        TARGET =  mtLicense_d
        DLLDESTDIR = $$META_DIR
    } else {
        TARGET =  mtLicense
        DLLDESTDIR = $$META_DIR
    }
}

MOC_DIR = ./mocs
UI_DIR  = ./uics




