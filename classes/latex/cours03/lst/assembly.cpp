gmm::row_matrix< gmm::wsvector<double> > Tmp(nnodes,nnodes);
for(int i=0; i<elems.size(); ++i) {
    Element *e = elems[i];
    gmm::dense_matrix<double> Ke(8,8);
    buildK(e, Ke);
    for(int ii=0; ii<e->nodes.size(); ++ii) {
        Node *nodi = e->nodes[ii];
        for(int jj=0; jj< e->nodes.size(); ++jj) {
            Node *nodj = e->nodes[jj];
            Tmp(nodi->row,nodj->row) +=  Ke(ii,jj);
        }
    }
}
gmm::csr_matrix<double> K;    
gmm::copy(Tmp, K);           // copy Tmp into a read-only CSR