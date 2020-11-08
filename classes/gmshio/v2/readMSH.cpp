// readMSH.cpp - "readMSH" function and related routines

#include "gmshio.h"

/*
    $MeshFormat // same as MSH version 2
        version(ASCII double; currently 4.0) file-type(ASCII int; 1 for binary mode)
        data-size(ASCII int; currently unused)
        <int with value one; only in binary mode, to detect endianness>
    $EndMeshFormat
*/

void readFormatHeader(FILE *fp, double &version)
{
    char string[256] = "";
    fgets(string, sizeof(string), fp);
    int format, size;
    if (sscanf(string, "%lf %d %d", &version, &format, &size) != 3)
        errorAndQuit("bad format", fp);

    if (version < 2.0 || version > 4.0)
    {
        printf("Error: Unknown mesh file version (%g)\n", version);
        fclose(fp);
        return;
    }
    if (format != 0)
        errorAndQuit("cannot read mesh in binary format", fp);
}

/*
    $Entities
        numPoints(unsigned long) numCurves(unsigned long)
            numSurfaces(unsigned long) numVolumes(unsigned long)
        // points
        tag(int) boxMinX(double) boxMinY(double) boxMinZ(double)
            boxMaxX(double) boxMaxY(double) boxMaxZ(double)
            numPhysicals(unsigned long) physicalTag(int) ...
        ...
        // curves
        tag(int) boxMinX(double) boxMinY(double) boxMinZ(double)
            boxMaxX(double) boxMaxY(double) boxMaxZ(double)
            numPhysicals(unsigned long) physicalTag(int) ...
            numBoundingPoints(unsigned long) tagPoint(int) ...
        ...
        // surfaces
        tag(int) boxMinX(double) boxMinY(double) boxMinZ(double)
            boxMaxX(double) boxMaxY(double) boxMaxZ(double)
            numPhysicals(unsigned long) physicalTag(int) ...
            numBoundingCurves(unsigned long) tagCurve(int) ...
        ...
        // volumes
        tag(int) boxMinX(double) boxMinY(double) boxMinZ(double)
            boxMaxX(double) boxMaxY(double) boxMaxZ(double)
            numPhysicals(unsigned long) physicalTag(int) ...
            numBoundngSurfaces(unsigned long) tagSurfaces(int) ...
        ...
    $EndEntities
*/

void readEntities(FILE *fp, std::vector<std::map<int, std::vector<int>>> &ent2phy)
{
    char string[256] = "";
    fgets(string, sizeof(string), fp);

    unsigned long int numPoints, numCurves, numSurfaces, numVolumes;
    if (sscanf(string, "%lu %lu %lu %lu", &numPoints, &numCurves, &numSurfaces, &numVolumes) != 4)
        errorAndQuit("bad format [1]", fp);

    std::vector<unsigned long int> nums(4);
    nums[0] = numPoints;
    nums[1] = numCurves;
    nums[2] = numSurfaces;
    nums[3] = numVolumes;

    // TODO: check all types!

    for (int k = 0; k < 4; ++k)
    {
        // ----read entities of dimension k
        for (unsigned long int i = 0; i < nums[k]; ++i)
        {
            int tag; //, physicalTag;
            unsigned long int numPhysicals;
            double boxMinX, boxMinY, boxMinZ, boxMaxX, boxMaxY, boxMaxZ;
            if (fscanf(fp, "%d %lf %lf %lf %lf %lf %lf %lu", &tag, &boxMinX, &boxMinY, &boxMinZ,
                       &boxMaxX, &boxMaxY, &boxMaxZ, &numPhysicals) != 8)
                errorAndQuit("bad format [2]", fp);
            //printf("k=%d,i=%d\n",k,i);
            for (unsigned long int j = 0; j < numPhysicals; ++j)
            {
                int phy;
                if (fscanf(fp, "%d", &phy) != 1)
                    errorAndQuit("bad format [3]", fp);
                ent2phy[k][tag].push_back(phy); // physical phy contains entity tag
            }
            fgets(string, sizeof(string), fp); // skip remaining chars of current line
        }

        // print info about physical groups and entities
        for (auto it = ent2phy[k].begin(); it != ent2phy[k].end(); ++it)
        {
            printf("Entity %d (%dD) belongs to %lu physical groups:", it->first, k, (unsigned long int)it->second.size());
            for (int i = 0; i < it->second.size(); ++i)
                printf(" %d", it->second[i]);
            printf("\n");
        }
    }
}

/*
    $PhysicalNames // same as MSH version 2
        numPhysicalNames(ASCII int)
        dimension(ASCII int) tag(ASCII int) name(127 characters max)
        ...
    $EndPhysicalNames
*/

void readPhyNames(FILE *fp, double version)
{
    char string[256] = "";
    fgets(string, sizeof(string), fp);

    int numNames;
    if (sscanf(string, "%d", &numNames) != 1)
        errorAndQuit("while reading Physical Names (numNames)", fp);

    for (int i = 0; i < numNames; i++)
    {
        int dim = -1, num;
        if (version > 2.0)
        {
            if (fscanf(fp, "%d", &dim) != 1)
                errorAndQuit("while reading Physical Names (dim)", fp);
        }
        if (fscanf(fp, "%d", &num) != 1)
            errorAndQuit("while reading Physical Names (num)", fp);

        fgets(string, sizeof(string), fp);
        // use the name here!
    }
}

/*
    ** version 2.2

        $Nodes
            number-of-nodes
            node-number x-coord y-coord z-coord
            ...
        $EndNodes

    ** version 4

        $Nodes
            numEntityBlocks(unsigned long) numNodes(unsigned long)
            tagEntity(int) dimEntity(int) parametric(int; see below) numNodes(unsigned long)
                tag(int) x(double) y(double) z(double)
                <u(double; if parametric and on curve or surface)>
                <v(double; if parametric and on surface)>
                ...
            ...
        $EndNodes
*/

void readNodes(FILE *fp, std::vector<Node *> &nodes,
               std::map<int, Node *> &nodeMap, double version)
{
    char string[256] = "";
    fgets(string, sizeof(string), fp);

    if (version < 4.0) // version 2
    {
        int nbNodes;
        if (sscanf(string, "%d", &nbNodes) != 1)
            errorAndQuit("while reading nodes (nbNodes)", fp);

        printf("Reading %d nodes\n", nbNodes);
        for (int i = 0; i < nbNodes; i++)
        {
            Node *n = new Node;
            fscanf(fp, "%d %lf %lf %lf", &n->num, &n->x, &n->y, &n->z);
            nodes.push_back(n);
            nodeMap[n->num] = n;
        }
    }
    else // version 4
    {
        unsigned long int numEntityBlocks, numNodes;
        if (sscanf(string, "%lu %lu", &numEntityBlocks, &numNodes) != 2)
            errorAndQuit("while reading nodes (numEntityBlocks)", fp);

        printf("Reading %lu blocks of nodes (total = %lu nodes)\n", numEntityBlocks, numNodes);

        for (unsigned long int k = 0; k < numEntityBlocks; k++)
        {
            int tagEntity, dimEntity, parametric;
            unsigned long int numNodesB;
            if (fscanf(fp, "%d %d %d %lu", &tagEntity, &dimEntity, &parametric, &numNodesB) != 4)
                errorAndQuit("while reading node blocks", fp);

            printf("Reading block of %d nodes\n", numNodesB);
            for (unsigned long int i = 0; i < numNodesB; i++)
            {
                Node *n = new Node;
                fscanf(fp, "%d %lf %lf %lf", &n->num, &n->x, &n->y, &n->z);
                nodes.push_back(n);
                nodeMap[n->num] = n;
                //printf("\t%d node%d %lf %lf %lf\n", i, n->num, n->x, n->y, n->z);

                // ignore parametric data
                if (parametric)
                {
                    double dummy;
                    for(int i=0; i<dimEntity; ++i)
                        fscanf(fp, "%lf", &dummy);
                    //fgets(string, sizeof(string), fp); // skip remaining chars of line
                }
            }
        }
        //printf("nodeMap.size()=%lu\n", (unsigned long)nodeMap.size());
    }
}

/*
    ** version 2.2

        $Elements
            number-of-elements
            elm-number elm-type number-of-tags < tag > ... node-number-list
            ...
        $EndElements

    ** version 4

        $Elements
            numEntityBlocks(unsigned long) numElements(unsigned long)
            tagEntity(int) dimEntity(int) typeEle(int; see below) numElements(unsigned long)
                tag(int) numVert(int) ...
                ...
            ...
        $EndElements  
*/

void readElements(FILE *fp, std::vector<Element *> &elements,
                  std::map<int, Node *> &nodeMap, std::vector<std::map<int, std::vector<int>>> &ent2phy,
                  double version)
{
    char string[256] = "";
    fgets(string, sizeof(string), fp);

    if (version < 4.0) // version 2
    {
        int nbElements;
        if (sscanf(string, "%d", &nbElements) != 1)
            errorAndQuit("while reading nodes (nbElements)", fp);

        printf("Reading %d elements\n", nbElements);

        for (int i = 0; i < nbElements; i++)
        {
            Element *e = new Element;
            int numTags;
            fscanf(fp, "%d %d %d", &e->num, &e->type, &numTags);
            for (int j = 0; j < numTags; j++)
            {
                int val;
                fscanf(fp, "%d", &val);
                if (j == 0)
                    e->region = val;
            }
            int nbNodes = 0;
            if (e->type == 1)
                nbNodes = 2; // line
            else if (e->type == 2)
                nbNodes = 3; // triangle
            else if (e->type == 3)
                nbNodes = 4; // quad
            else if (e->type == 4)
                nbNodes = 4; // tetra
            else if (e->type == 5)
                nbNodes = 8; // hexa
            else if (e->type == 15)
                nbNodes = 1; // point
            else
            {
                printf("Error: unknown element type %d\n", e->type);
                fclose(fp);
                exit(1);
            }
            for (int j = 0; j < nbNodes; j++)
            {
                int numNode;
                fscanf(fp, "%d", &numNode);
                e->nodes.push_back(nodeMap[numNode]);
            }
            elements.push_back(e);
        }
    }
    else // version 4
    {
        unsigned long int numEntityBlocks, numElements;
        if (sscanf(string, "%lu %lu", &numEntityBlocks, &numElements) != 2)
            errorAndQuit("while reading elements (numEntityBlocks)", fp);

        printf("Reading %d blocks of elems (total = %d elems)\n", numEntityBlocks, numElements);

        for (unsigned long int k = 0; k < numEntityBlocks; k++)
        {
            int tagEntity, dimEntity, typeEle;
            unsigned long int numElementsB;
            if (fscanf(fp, "%d %d %d %lu", &tagEntity, &dimEntity, &typeEle, &numElementsB) != 4)
                errorAndQuit("while reading elem blocks", fp);

            // tag?
            int numtag=-1;
            auto it = ent2phy[dimEntity].find(tagEntity);
            if (it != ent2phy[dimEntity].end())
                numtag = it->second[0]; // find first corresponding physical tag (what if more than 1?)
            else
                // if no physical tags, we use the tag as provided (which the tag of the entity)
                numtag = tagEntity;

            printf("Reading block of %lu %dD elms (type=%d, tag=%d)", numElementsB, dimEntity, typeEle, tagEntity);
            
            printf(" => physical group %dD %d)\n", dimEntity, numtag);

            for (unsigned long int i = 0; i < numElementsB; i++)
            {
                Element *e = new Element;
                //int numTags;
                fscanf(fp, "%d", &e->num);
                e->type = typeEle;
                e->region = numtag; // we should first check if tag is there
                // TODO what if more that 1 phygroup?? => duplicate elem? NO=> how to write results?
                int nbNodes = 0;
                if (e->type == 1)
                    nbNodes = 2; // line
                else if (e->type == 2)
                    nbNodes = 3; // triangle
                else if (e->type == 3)
                    nbNodes = 4; // quad
                else if (e->type == 4)
                    nbNodes = 4; // tetra
                else if (e->type == 5)
                    nbNodes = 8; // hexa
                else if (e->type == 15)
                    nbNodes = 1; // point
                else
                {
                    printf("Error: unknown element type %d\n", e->type);
                    fclose(fp);
                    exit(1);
                }
                for (int j = 0; j < nbNodes; j++)
                {
                    int numNode;
                    fscanf(fp, "%d", &numNode);
                    e->nodes.push_back(nodeMap[numNode]);
                }
                elements.push_back(e);
            }
        }
    }
}

void readMSH(const char *fileName,
             std::vector<Node *> &nodes,
             std::vector<Element *> &elements)
{
    FILE *fp = fopen(fileName, "r");
    if (!fp)
    {
        printf("Error: cannot open file %s\n", fileName);
        return;
    }

    char string[256] = "";
    double version = 0.0;
    std::map<int, Node *> nodeMap;
    std::vector<std::map<int, std::vector<int>>> ent2phy(4);

    while (1)
    {
        // look for a block (line beginning with "$")
        do
        {
            fgets(string, sizeof(string), fp);
            if (feof(fp))
                break;
        } while (string[0] != '$');
        if (feof(fp))
            break;

        // process block
        if (!strncmp(&string[1], "MeshFormat", 10))
            readFormatHeader(fp, version);
        else if (!strncmp(&string[1], "Entities", 8))
            readEntities(fp, ent2phy);
        else if (!strncmp(&string[1], "PhysicalNames", 13))
            readPhyNames(fp, version);
        else if (!strncmp(&string[1], "Nodes", 5))
            readNodes(fp, nodes, nodeMap, version);
        else if (!strncmp(&string[1], "Elements", 8))
            readElements(fp, elements, nodeMap, ent2phy, version);

        // look for the closing "$" command
        do
        {
            fgets(string, sizeof(string), fp);
            if (feof(fp))
                errorAndQuit("Premature end of file", fp);
        } while (string[0] != '$');
    }

    fclose(fp);
}
