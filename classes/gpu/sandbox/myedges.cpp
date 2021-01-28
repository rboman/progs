#include <cstdio>
#include <iostream>
#ifdef _MSC_VER
#include <gmsh.h_cwrap>
#else
#include <gmsh.h>
#endif

// [DOC from gmsh.h]
//   A geometrical entity in the Gmsh API is represented by two integers: its
//   dimension (dim = 0, 1, 2 or 3) and its tag (its unique, strictly positive
//   identifier). When dealing with multiple geometrical entities of possibly
//   different dimensions, the entities are packed as a vector of (dim, tag)
//   integer pairs.
//
//   typedef std::vector<std::pair<int, int> > vectorpair;

// [RB] note that entities of different dimensions can share the same tag
//      (although the pair (dim,tag) should be unique)

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        std::cout << "Usage: " << argv[0] << " file.msh [options]" << std::endl;
        return 0;
    }

    // [DOC from gmsh.h]
    //   Initialize Gmsh. This must be called before any call to the other functions in
    //   the API. If `argc' and `argv' (or just `argv' in Python or Julia) are
    //   provided, they will be handled in the same way as the command line arguments
    //   in the Gmsh app. If `readConfigFiles' is set, read system Gmsh configuration
    //   files (gmshrc and gmsh-options).
    //
    //   GMSH_API void initialize(int argc = 0, char ** argv = 0,
    //                            const bool readConfigFiles = true);
    gmsh::initialize(argc, argv);

    // print messages on the terminal
    gmsh::option::setNumber("General.Terminal", 1);

    gmsh::open(argv[1]); // reads the msh file

    // explore the mesh: what type of 2D elements do we have?

    // [DOC from gmsh.h]
    //   Get the types of elements in the entity of dimension `dim' and tag `tag'.
    //   If `tag' < 0, get the types for all entities of dimension `dim'. If `dim'
    //   and `tag' are negative, get all the types in the mesh.
    //
    //   GMSH_API void getElementTypes(std::vector<int> & elementTypes,
    //                                 const int dim = -1,
    //                                 const int tag = -1);
    std::vector<int> eleTypes;
    gmsh::model::mesh::getElementTypes(eleTypes, 2);
    if (eleTypes.size() != 1)
    {
        gmsh::logger::write("Hybrid meshes not handled in this example!",
                            "error");
        // [RB] I guess that "getElementEdgeNodes" is not implemented for hybrid meshes.
        return 1;
    }

    // get all the properties (name, order, etc) related to this type of element

    // [DOC from gmsh.h]
    //   Get the properties of an element of type `elementType': its name
    //   (`elementName'), dimension (`dim'), order (`order'), number of nodes
    //   (`numNodes'), coordinates of the nodes in the reference element
    //   (`nodeCoord' vector, of length `dim' times `numNodes') and number of
    //   primary (first order) nodes (`numPrimaryNodes').
    //
    //   GMSH_API void getElementProperties(const int elementType,
    //                                      std::string & elementName,
    //                                      int & dim,
    //                                      int & order,
    //                                      int & numNodes,
    //                                      std::vector<double> & nodeCoord,
    //                                      int & numPrimaryNodes);
    int eleType2D = eleTypes[0];
    std::string name;
    int dim, order, numNodes, numPrimaryNodes;
    std::vector<double> paramCoord;
    gmsh::model::mesh::getElementProperties(eleType2D, name, dim, order,
                                            numNodes, paramCoord, numPrimaryNodes);
    gmsh::logger::write("2D elements are of type '" + name + "' (type = " +
                        std::to_string(eleType2D) + ") ");

    // iterate over all surfaces, get the 2D elements and create new 1D elements
    // for all edges

    // [DOC from gmsh.h]
    //   Get all the entities in the current model. If `dim' is >= 0, return only the
    //   entities of the specified dimension (e.g. points if `dim' == 0). The
    //   entities are returned as a vector of (dim, tag) integer pairs.
    //
    //   GMSH_API void getEntities(gmsh::vectorpair & dimTags,
    //                             const int dim = -1);
    std::vector<std::pair<int, int>> entities;
    gmsh::model::getEntities(entities, 2);
    std::cout << "There are " << entities.size() << " entitie(s) of dim 2\n";

    for (std::size_t i = 0; i < entities.size(); i++)
    {
        // [DOC from gmsh.h]
        //   Get the elements of type `elementType' classified on the entity of tag
        //   `tag'. If `tag' < 0, get the elements for all entities. `elementTags' is a
        //   vector containing the tags (unique, strictly positive identifiers) of the
        //   elements of the corresponding type. `nodeTags' is a vector of length equal
        //   to the number of elements of the given type times the number N of nodes
        //   for this type of element, that contains the node tags of all the elements
        //   of the given type, concatenated: [e1n1, e1n2, ..., e1nN, e2n1, ...]. If
        //   `numTasks' > 1, only compute and return the part of the data indexed by
        //   `task'.
        //   GMSH_API void getElementsByType(const int elementType,
        //                                   std::vector<std::size_t> & elementTags,
        //                                   std::vector<std::size_t> & nodeTags,
        //                                   const int tag = -1,
        //                                   const std::size_t task = 0,
        //                                   const std::size_t numTasks = 1);
        int s = entities[i].second;
        std::cout << "processing entity with tag #" << s << '\n';
        std::vector<std::size_t> elementTags, nodeTags;
        gmsh::model::mesh::getElementsByType(eleType2D, elementTags, nodeTags, s);
        gmsh::logger::write("- " + std::to_string(elementTags.size()) +
                            " elements in surface " + std::to_string(s));

        // get the nodes on the edges of the 2D elements

        // [DOC from gmsh.h]
        //   Get the nodes on the edges of all elements of type `elementType'
        //   classified on the entity of tag `tag'. `nodeTags' contains the node tags
        //   of the edges for all the elements: [e1a1n1, e1a1n2, e1a2n1, ...]. Data is
        //   returned by element, with elements in the same order as in `getElements'
        //   and `getElementsByType'. If `primary' is set, only the primary (begin/end)
        //   nodes of the edges are returned. If `tag' < 0, get the edge nodes for all
        //   entities. If `numTasks' > 1, only compute and return the part of the data
        //   indexed by `task'.
        //   GMSH_API void getElementEdgeNodes(const int elementType,
        //                                     std::vector<std::size_t> & nodeTags,
        //                                     const int tag = -1,
        //                                     const bool primary = false,
        //                                     const std::size_t task = 0,
        //                                     const std::size_t numTasks = 1);
        std::vector<std::size_t> nodes;
        gmsh::model::mesh::getElementEdgeNodes(eleType2D, nodes, s);

        // create a new discrete entity of dimension 1

        // [DOC from gmsh.h]
        //   Add a discrete model entity (defined by a mesh) of dimension `dim' in the
        //   current model. Return the tag of the new discrete entity, equal to `tag' if
        //   `tag' is positive, or a new tag if `tag' < 0. `boundary' specifies the tags
        //   of the entities on the boundary of the discrete entity, if any. Specifying
        //   `boundary' allows Gmsh to construct the topology of the overall model.
        //   GMSH_API int addDiscreteEntity(const int dim,
        //                                  const int tag = -1,
        //                                  const std::vector<int> & boundary = std::vector<int>());
        int c = gmsh::model::addDiscreteEntity(1);
        std::cout << "creating new discrete entity of dimension 1 (new tag=" << c << ")\n";

        // and add new 1D elements to it, for all edges

        // [DOC from gmsh.h]
        //   Return an element type given its family name `familyName' ("point",
        //   "line", "triangle", "quadrangle", "tetrahedron", "pyramid", "prism",
        //   "hexahedron") and polynomial order `order'. If `serendip' is true, return
        //   the corresponding serendip element type (element without interior nodes).
        //   GMSH_API int getElementType(const std::string & familyName,
        //                               const int order,
        //                               const bool serendip = false);
        int eleType1D = gmsh::model::mesh::getElementType("line", order);

        // [DOC from gmsh.h]
        //   Add elements of type `elementType' classified on the entity of tag `tag'.
        //   `elementTags' contains the tags (unique, strictly positive identifiers) of
        //   the elements of the corresponding type. `nodeTags' is a vector of length
        //   equal to the number of elements times the number N of nodes per element,
        //   that contains the node tags of all the elements, concatenated: [e1n1,
        //   e1n2, ..., e1nN, e2n1, ...]. If the `elementTag' vector is empty, new tags
        //   are automatically assigned to the elements.
        //   GMSH_API void addElementsByType(const int tag,
        //                                   const int elementType,
        //                                   const std::vector<std::size_t> & elementTags,
        //                                   const std::vector<std::size_t> & nodeTags);
        gmsh::model::mesh::addElementsByType(c, eleType1D, {}, nodes);

        // this will create two 1D elements for each edge; to create unique elements
        // it would be useful to call getElementEdgeNodes() with the extra `primary'
        // argument set to 'true' (to only get start/end nodes even in the
        // high-order case, i.e. consider topological edges), then sort them and
        // make them unique.

        // this could be enriched with additional info: each topological edge could
        // be associated with the tag of its parent element; in the sorting process
        // (eliminating duplicates) a second tag can be associated for internal
        // edges, allowing to keep track of neighbors
    }

    std::cout << "iterate over all 1D elements and get integration information\n";

    //gmsh::write("edges.msh");

    // iterate over all 1D elements and get integration information
    gmsh::model::mesh::getElementTypes(eleTypes, 1);

    // [DOC from gmsh.h]
    //   Get the numerical quadrature information for the given element type
    //   `elementType' and integration rule `integrationType' (e.g. "Gauss4" for a
    //   Gauss quadrature suited for integrating 4th order polynomials).
    //   `integrationPoints' contains the u, v, w coordinates of the G integration
    //   points in the reference element: [g1u, g1v, g1w, ..., gGu, gGv, gGw].
    //   `integrationWeigths' contains the associated weights: [g1q, ..., gGq].
    //   GMSH_API void getIntegrationPoints(const int elementType,
    //                                      const std::string & integrationType,
    //                                      std::vector<double> & integrationPoints,
    //                                      std::vector<double> & integrationWeights);
    int eleType1D = eleTypes[0];
    std::vector<double> uvw, q;
    gmsh::model::mesh::getIntegrationPoints(eleType1D, "Gauss3", uvw, q);

    // [DOC from gmsh.h]
    //   Get the basis functions of the element of type `elementType' at the
    //   integration points `integrationPoints' (given as concatenated triplets of
    //   coordinates in the reference element [g1u, g1v, g1w, ..., gGu, gGv, gGw]),
    //   for the function space `functionSpaceType' (e.g. "Lagrange" or
    //   "GradLagrange" for Lagrange basis functions or their gradient, in the u,
    //   v, w coordinates of the reference element). `numComponents' returns the
    //   number C of components of a basis function. `basisFunctions' returns the
    //   value of the N basis functions at the integration points, i.e. [g1f1,
    //   g1f2, ..., g1fN, g2f1, ...] when C == 1 or [g1f1u, g1f1v, g1f1w, g1f2u,
    //   ..., g1fNw, g2f1u, ...] when C == 3.
    //   GMSH_API void getBasisFunctions(const int elementType,
    //                                   const std::vector<double> & integrationPoints,
    //                                   const std::string & functionSpaceType,
    //                                   int & numComponents,
    //                                   std::vector<double> & basisFunctions);
    std::vector<double> bf;
    int numComp;
    gmsh::model::mesh::getBasisFunctions(eleType1D, uvw, "Lagrange", numComp, bf);
    gmsh::model::getEntities(entities, 1);
    for (std::size_t i = 0; i < entities.size(); i++)
    {
        int c = entities[i].second;
        std::vector<std::size_t> elementTags, nodeTags;
        gmsh::model::mesh::getElementsByType(eleType1D, elementTags, nodeTags, c);
        gmsh::logger::write("- " + std::to_string(elementTags.size()) +
                            " elements on curve " + std::to_string(c));

        // [DOC from gmsh.h]
        //   Get the Jacobians of all the elements of type `elementType' classified on
        //   the entity of tag `tag', at the G integration points `integrationPoints'
        //   given as concatenated triplets of coordinates in the reference element
        //   [g1u, g1v, g1w, ..., gGu, gGv, gGw]. Data is returned by element, with
        //   elements in the same order as in `getElements' and `getElementsByType'.
        //   `jacobians' contains for each element the 9 entries of the 3x3 Jacobian
        //   matrix at each integration point. The matrix is returned by column:
        //   [e1g1Jxu, e1g1Jyu, e1g1Jzu, e1g1Jxv, ..., e1g1Jzw, e1g2Jxu, ..., e1gGJzw,
        //   e2g1Jxu, ...], with Jxu=dx/du, Jyu=dy/du, etc. `determinants' contains for
        //   each element the determinant of the Jacobian matrix at each integration
        //   point: [e1g1, e1g2, ... e1gG, e2g1, ...]. `points' contains for each
        //   element the x, y, z coordinates of the integration points. If `tag' < 0,
        //   get the Jacobian data for all entities. If `numTasks' > 1, only compute
        //   and return the part of the data indexed by `task'.
        //   GMSH_API void getJacobians(const int elementType,
        //                              const std::vector<double> & integrationPoints,
        //                              std::vector<double> & jacobians,
        //                              std::vector<double> & determinants,
        //                              std::vector<double> & points,
        //                              const int tag = -1,
        //                              const std::size_t task = 0,
        //                              const std::size_t numTasks = 1);
        std::vector<double> jac, det, pts;
        gmsh::model::mesh::getJacobians(eleType1D, uvw, jac, det, pts, c);
    }

    // gmsh::fltk::run();

    gmsh::finalize();
    return 0;
}
