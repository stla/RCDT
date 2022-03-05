// -*- mode: C++; c-indent-level: 2; c-basic-offset: 2; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"
// [[Rcpp::depends(RcppArmadillo)]]


#include "CDT.h"

typedef CDT::V2d<double> Vertex;
typedef CDT::Edge Edge;
typedef CDT::Triangulation<double> Triangulation;


// CDT::TriangleVec triangulate(const arma::mat & points){
//   Triangulation cdt(CDT::VertexInsertionOrder::AsProvided);
//   size_t npoints = points.n_rows;
//   std::vector<Vertex> vertices(npoints);
//   for (size_t i = 0; i < npoints; ++i) {
//     const arma::rowvec row_i = points.row(i);
//     vertices[i] = Vertex::make(row_i(0), row_i(1));
//   }
//   cdt.insertVertices(vertices);
//   cdt.eraseSuperTriangle();
//   return cdt.triangles;
// }

// [[Rcpp::export]]
arma::umat Rcpp_delaunay(const arma::mat & points){
  Triangulation cdt(CDT::VertexInsertionOrder::AsProvided);
  // insert vertices
  const size_t npoints = points.n_rows;
  std::vector<Vertex> vertices(npoints);
  for (size_t i = 0; i < npoints; ++i) {
    const arma::rowvec row_i = points.row(i);
    vertices[i] = Vertex::make(row_i(0), row_i(1));
  }
  cdt.insertVertices(vertices);
  cdt.eraseSuperTriangle();
  // output
  const CDT::TriangleVec triangles = cdt.triangles;
  const size_t ntriangles = triangles.size();
  arma::umat out(ntriangles, 3);
  for(size_t i = 0; i < ntriangles; ++i){
    const CDT::VerticesArr3 trgl = triangles[i].vertices;
    out(i, 0) = trgl[0];
    out(i, 1) = trgl[1];
    out(i, 2) = trgl[2];
  }
  return out + 1;
}

// void* operator new (size_t size, const unsigned & v1, const unsigned & v2) {
//   /* Do something, then return a pointer to at least 'size' bytes. */
//   return ::operator new(size);
// }

// [[Rcpp::export]]
Rcpp::List Rcpp_constrained_delaunay(
    const arma::mat & points, const arma::umat & edges
){
  Triangulation cdt(CDT::VertexInsertionOrder::AsProvided);
  // insert vertices
  const size_t npoints = points.n_rows;
  std::vector<Vertex> vertices(npoints);
  for (size_t i = 0; i < npoints; ++i) {
    const arma::rowvec row_i = points.row(i);
    vertices[i] = Vertex::make(row_i(0), row_i(1));
  }
  cdt.insertVertices(vertices);
  // insert edges
  const size_t nedges = edges.n_rows;
  // for(size_t i = 0; i < nedges; ++i){
  //   const arma::urowvec edge = edges.row(i);
  //   cdt.insert_constraint(vertices[edge[0]], vertices[edge[1]]);
  // }
  std::vector<Edge> Edges;
  Edges.reserve(nedges);
  //Sampleclass *qs = new Edge();
  for (size_t i = 0; i < nedges; ++i) {
    const arma::urowvec row_i = edges.row(i) - 1;
    Edges.push_back(Edge(row_i(0), row_i(1)));
    // Edge *edge = new (row_i(0), row_i(1)) Edge;
    // //edge = Edge(row_i(0), row_i(1));//(row_i(0), row_i(1)));
    // Edges[i] = *edge;
  }
  cdt.insertEdges(Edges);
  cdt.eraseOuterTrianglesAndHoles();
  //// output
  // triangles
  const CDT::TriangleVec triangles = cdt.triangles;
  arma::umat out_triangles(triangles.size(), 3);
  for(size_t i = 0; i < triangles.size(); ++i){
    const CDT::VerticesArr3 trgl = triangles[i].vertices;
    out_triangles(i, 0) = trgl[0];
    out_triangles(i, 1) = trgl[1];
    out_triangles(i, 2) = trgl[2];
  }
  // border edges
  CDT::EdgeUSet borderEdges = cdt.fixedEdges;
  arma::umat out_edges(borderEdges.size(), 2);
  std::unordered_set<Edge> :: iterator itedge;
  size_t i = 0;
  for(itedge = borderEdges.begin(); itedge != borderEdges.end(); itedge++){
    const Edge edge = *itedge;
    out_edges(i, 0) = CDT::edge_get_v1(edge);
    out_edges(i, 1) = CDT::edge_get_v2(edge);
    i++;
  }
  // all edges
  CDT::EdgeUSet allEdges = CDT::extractEdgesFromTriangles(triangles);
  arma::umat out_alledges(allEdges.size(), 2);
  std::unordered_set<Edge> :: iterator it;
  i = 0;
  for(it = allEdges.begin(); it != allEdges.end(); it++){
    const Edge edge = *it;
    out_alledges(i, 0) = CDT::edge_get_v1(edge);
    out_alledges(i, 1) = CDT::edge_get_v2(edge);
    i++;
  }
  //
  return Rcpp::List::create(Rcpp::Named("triangles") = out_triangles + 1,
                            Rcpp::Named("borderEdges") = out_edges + 1,
                            Rcpp::Named("allEdges") = out_alledges + 1);
}
