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
