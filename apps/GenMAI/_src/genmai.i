%module genmai
%{

// utils --
#include "Point.h"
#include "IntNumber.h"
#include "PolarPoint.h"
#include "Arc.h"
#include "Line.h"
#include "Element.h"


// params --
//#include "Parameters.h"
#include "MeshParameters.h"
#include "ToolParameters.h"


// builders --
#include "Mesh.h"
#include "MeshBuilder.h"
#include "Tool.h"
#include "ToolBuilder.h"

// export --
#include "NodeRenumberer.h"
#include "OofelieMeshExporter.h"
#include "BaconMeshExporter.h"
#include "MatlabMeshExporter.h"

#include "ToolExporter.h"
#include "OofelieToolExporter.h"
#include "BaconDatToolExporter.h"
#include "BaconToolExporter.h"
#include "MatlabToolExporter.h"

%}

%ignore *::operator<<;
%ignore operator=;
%ignore Point::operator*(double, const Point &);
%ignore Point::atan2;
%ignore Point::cosin;

%rename(output) print; // Rename all `print' functions to `output'
//%rename(__add__) operator+;
//%rename(__sub__) Complex::operator-; 
//%rename(__neg__) Complex::operator-();  // Unary - 

// utils 

%include "IntNumber.h"
%include "PtNumber.h"
%include "Point.h"
%include "PolarPoint.h"
%include "Curve.h"
%include "Arc.h"
%include "Line.h"
%include "Element.h"

// -- gestion des "std::string"
/*
%typemap(python,in)  
        const std::string,    std::string,  
        const string,         string,  
        const std::string &,  std::string &,   
        const string &,       string & 
        { 
                $1 = new std::string; 
                $1->assign (PyString_AsString($input),PyString_Size($input));
        } 

//%typemap(python,in) std::string & {
//  if (PyString_Check ($source))
//  {
//    $target = new std::string((char *)PyString_AsString($source));
//  }
//  else
//  {
//    PyErr_SetString (PyExc_TypeError, "not a String");
//    return NULL;
//  }
//}


%typemap(python,out)  
        const std::string,   std::string,  
        const string,        string 
        { 
                $result = PyString_FromStringAndSize($1.c_str(),$1.size());
        } 
%typemap(python,out)  
        const std::string &, std::string &,  
        const string &,      string & 
        {  		
           ERREUR // Le retour d'une commande ne doit pas etre par reference (voir dans scxx.i) 
        } 

//%typemap(python,out) string & {
//  $target = PyString_FromString((const char *)$source->c_str()); 
//}


%typemap(python,freearg)  
        const std::string,   std::string ,  
        const string,        string,  
        const std::string &, std::string &,  
        const string &,      string &
        { 
                delete $1;
        } 

//%typemap (python,freearg) string & {
//  if (&EMPTY_STRING != $source)
//  {
//    delete $source;
//  }
//}
*/  

%include "std_string.i"

// params

%include "Parameters.h"
%include "MeshParameters.h"

%include "ToolParameters.h"

// builders

%include "TargetObject.h"
%include "Mesh.h"
%include "Tool.h"

%include "Builder.h"
%include "MeshBuilder.h"
%include "ToolBuilder.h"

// export

%include "NodeRenumberer.h"
%include "Exporter.h"               // for "save"
%include "MeshExporter.h"           // for "save"
%include "OofelieMeshExporter.h"
%include "BaconMeshExporter.h"
%include "MatlabMeshExporter.h"

%include "ToolExporter.h"
%include "OofelieToolExporter.h"
%include "BaconDatToolExporter.h"
%include "BaconToolExporter.h"
%include "MatlabToolExporter.h"
