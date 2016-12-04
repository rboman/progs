TEMPLATE	= app
CONFIG		= qt warn_off release
HEADERS		= src/m_baconexporter.h \
		  src/m_exporter.h \
		  src/m_matlabexporter.h \
		  src/m_oofelieexporter.h \
		  src/t_arc.h \
		  src/t_baconexporter.h \
		  src/t_curve.h \
		  src/t_exporter.h \
		  src/t_line.h \
		  src/t_maille.h \
		  src/t_matlabexporter.h \
		  src/t_matrix.h \
		  src/t_matrixbuilder.h \
		  src/t_mesh.h \
		  src/t_mesher.h \
		  src/t_mparam.h \
		  src/t_oofelieexporter.h \
		  src/t_param.h \
		  src/t_point.h \
		  src/t_ppoint.h \
		  src/t_ioobject.h \
          mywidgeti.h
SOURCES		= src/gendon.cpp \
		  src/genmai.cpp \
		  src/m_baconexporter.cpp \
		  src/m_exporter.cpp \
		  src/m_matlabexporter.cpp \
		  src/m_oofelieexporter.cpp \
		  main.cpp \
		  src/t_arc.cpp \
		  src/t_baconexporter.cpp \
		  src/t_exporter.cpp \
		  src/t_line.cpp \
		  src/t_matlabexporter.cpp \
		  src/t_matrix.cpp \
		  src/t_matrixbuilder.cpp \
		  src/t_mesh.cpp \
		  src/t_mesher.cpp \
		  src/t_mparam.cpp \
		  src/t_oofelieexporter.cpp \
		  src/t_param.cpp \
		  src/t_point.cpp \
		  src/t_ppoint.cpp \
		  src/t_ioobject.cpp \
          mywidgeti.cpp
INTERFACES	= mywidget.ui
TARGET		= genmai
DEFINES+=QT_DLL
CONFIG+=thread
INCLUDEPATH += src

win32:CXXFLAGS += -GX




