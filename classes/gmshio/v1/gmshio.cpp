#include <cstring>
#include <cstdio>
#include <cmath>
#include <vector>
#include <map>

struct Node {
  int num;
  double x, y, z;
};

struct Element {
  int num, type, region;
  std::vector<Node*> nodes;
};

void readMSH(const char *fileName, std::vector<Node*> &nodes,
             std::vector<Element*> &elements)
{
  FILE *fp = fopen(fileName, "r");
  if(!fp){
    printf("Error: cannot open file %s\n", fileName);
    return;
  }

  char string[256] = "";
  double version = 2.2;
  std::map<int, Node*> nodeMap;

  while (1) {
    do {
      fgets(string, sizeof(string), fp);
      if (feof(fp)) break ;
    } while (string[0] != '$') ;

    if (feof(fp))  break ;

    // read format header
    if(!strncmp(&string[1], "MeshFormat", 10)) {
      fgets(string, sizeof(string), fp) ;
      int format, size;
      if(sscanf(string, "%lf %d %d", &version, &format, &size) != 3){
        fclose(fp);
        return;
      }
      if(version < 2.0 || version >= 3.0){
	printf("Error: Unknown mesh file version (%g)\n", version);
        fclose(fp);
	return;
      }
      if(format){
	printf("Error: cannot read mesh in binary format");
        fclose(fp);
        return;
      }
    }

    // physical names
    else if(!strncmp(&string[1], "PhysicalNames", 13)) {
      fgets(string, sizeof(string), fp) ;
      int numNames;
      if(sscanf(string, "%d", &numNames) != 1){
        fclose(fp);
        return ;
      }
      for(int i = 0; i < numNames; i++) {
        int dim = -1, num;
        if(version > 2.0){
          if(fscanf(fp, "%d", &dim) != 1){
            fclose(fp);
            return ;
          }
        }
        if(fscanf(fp, "%d", &num) != 1){
          fclose(fp);
          return ;
        }
        fgets(string, sizeof(string), fp) ;
        // use the name here!
      }
    }

    // read nodes
    else if (!strncmp(&string[1], "Nodes", 5)) {
      fgets(string, sizeof(string), fp) ;
      int nbNodes;
      if(sscanf(string, "%d", &nbNodes) != 1){
        fclose(fp);
        return;
      }
      printf("Reading %d nodes\n", nbNodes);
      for (int i = 0 ; i < nbNodes ; i++) {
        Node *n = new Node;
        fscanf(fp, "%d %lf %lf %lf", &n->num, &n->x, &n->y, &n->z) ;
        nodes.push_back(n);
        nodeMap[n->num] = n;
      }
    }

    // read elements
    else if (!strncmp(&string[1], "Elements", 8)) {
      fgets(string, sizeof(string), fp) ;
      int nbElements;
      if(sscanf(string, "%d", &nbElements) != 1){
        fclose(fp);
        return;
      }
      printf("Reading %d elements\n", nbElements);
      for (int i = 0 ; i < nbElements ; i++) {
        Element *e = new Element;
        int numTags;
        fscanf(fp, "%d %d %d", &e->num, &e->type, &numTags);
        for(int j = 0; j < numTags; j++){
          int val;
          fscanf(fp, "%d", &val);
          if(j == 0)
            e->region = val;
        }
        int nbNodes = 0;
        if(e->type == 1) nbNodes = 2; // line
        else if(e->type == 2) nbNodes = 3; // triangle
        else if(e->type == 3) nbNodes = 4; // quad
        else if(e->type == 4) nbNodes = 4; // tetra
        else if(e->type == 5) nbNodes = 8; // hexa
        else if(e->type == 15) nbNodes = 1; // point
        else{
          printf("Error: unknown element type %d\n", e->type);
          fclose(fp);
          return;
        }
        for(int j = 0; j < nbNodes; j++){
          int numNode;
          fscanf(fp, "%d", &numNode) ;
          e->nodes.push_back(nodeMap[numNode]);
	}
        elements.push_back(e);
      }
    }

    do {
      fgets(string, sizeof(string), fp) ;
      if (feof(fp)){
        printf("Error: Premature end of file\n");
        fclose(fp);
        return;
      }
    } while (string[0] != '$') ;

  }

  fclose(fp);
}

void writeMSH(char *fileName, double time, int step,
              std::map<Node*, std::vector<double> > &solution)
{
  FILE *fp = fopen(fileName, "w");
  if(!fp){
    printf("Error: cannot open file %s\n", fileName);
    return;
  }

  if(solution.empty()){
    printf("Error: empty solution\n");
    fclose(fp);
    return;
  }

  std::map<Node*, std::vector<double> >::iterator it = solution.begin();

  int nbComp = it->second.size();

  fprintf(fp, "$MeshFormat\n2.2 0 8\n$EndMeshFormat\n");
  fprintf(fp, "$NodeData\n");
  fprintf(fp, "%d\n", 1);
  fprintf(fp, "\"my solution\"\n");
  fprintf(fp, "%d\n", 1);
  fprintf(fp, "%g\n", time);
  fprintf(fp, "%d\n", 3);
  fprintf(fp, "%d\n", step);
  fprintf(fp, "%d\n", nbComp);
  fprintf(fp, "%lu\n", (unsigned long)solution.size());
  for(it = solution.begin(); it != solution.end(); it++){
    fprintf(fp, "%d", it->first->num);
    for(int i = 0; i < nbComp; i++)
      fprintf(fp, " %g", it->second[i]);
    fprintf(fp, "\n");
  }
  fprintf(fp, "$EndNodeData\n");
  fclose(fp);
}

int main(int argc, char **argv)
{
  if(argc < 2){
    printf("Usage: %s file.msh\n", argv[0]);
    return 1;
  }

  std::vector<Node*> nodes;
  std::vector<Element*> elements;
  readMSH(argv[1], nodes, elements);

  printf("Read %lu nodes and %lu elements\n", (unsigned long)nodes.size(), (unsigned long)elements.size());
  std::map<Node*, std::vector<double> > solution;

  for(int step = 0; step < 10; step++){
    double sx = step, sz = step/2.;
    for(unsigned int i = 0; i < nodes.size(); i++){
      Node *n = nodes[i];
      double v = sqrt((n->x - sx)*(n->x - sx) + n->y * n->y + (n->z - sz) * (n->z - sz));
      std::vector<double> val(1, v);
      solution[n] = val;
    }
    char name[256];
    sprintf(name, "sol_%04d.pos", step);
    writeMSH(name, 0.25 * step, step, solution);
  }

  return 0;
}
