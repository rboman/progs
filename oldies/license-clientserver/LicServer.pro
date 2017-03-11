#
# generer le vcproj:
#     qmake -tp vc LicServer.pro
#
# Changer a la main dans Visual : 
#    dupliquer le projet pour en faire un x64 si nécessaire
#    [All]   : Ignore Import Library : "No" => "Yes"
#    Vérifier les dépendences de Metafor, MetaforGUI et _mtViz
#    Powergrep: ..\..\..\..\local64\qt-4.3.2 => $(MYLIBS)\qt-4.3.2
#    Powergrep: E:\local64\qt-4.3.2 => $(MYLIBS)\qt-4.3.2
#

TEMPLATE    = app
LANGUAGE    = C++

META_DIR = ../../../oo_meta
OOFE_DIR = ../../../oofelie

DEFINES += HAVE_CONFIG_H

VPATH = $$META_DIR/LicServer

INCLUDEPATH += ..

SOURCES += main.cpp  

HEADERS += LicServer.h

SOURCES += LicServer.cpp \
           main.cpp

FORMS = 

INCLUDEPATH += $$META_DIR/mtGlobal

CONFIG  += qt 
CONFIG  += debug_and_release 
CONFIG  += thread exceptions rtti stl warn_on 
CONFIG  += console

QT += network 

!debug_and_release|build_pass {
    CONFIG(debug, debug|release) {
        TARGET =  LicServer_d
        DESTDIR = $$META_DIR
    } else {
        TARGET =  LicServer
        DESTDIR = $$META_DIR
    }
}

MOC_DIR = ./mocs
UI_DIR  = ./uics




