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
  size_t npoints = points.n_rows;
  std::vector<Vertex> vertices(npoints);
  for (size_t i = 0; i < npoints; ++i) {
    const arma::rowvec row_i = points.row(i);
    vertices[i] = Vertex::make(row_i(0), row_i(1));
  }
  cdt.insertVertices(vertices);
  cdt.eraseSuperTriangle();
  const CDT::TriangleVec triangles = cdt.triangles;
  arma::umat out(triangles.size(), 3);
  for(size_t i = 0; i < triangles.size(); ++i){
    const CDT::VerticesArr3 trgl = triangles[i].vertices;
    out(i, 0) = trgl[0];
    out(i, 1) = trgl[1];
    out(i, 2) = trgl[2];
  }
  return out;
}

// void* operator new (size_t size, const unsigned & v1, const unsigned & v2) {
//   /* Do something, then return a pointer to at least 'size' bytes. */
//   return ::operator new(size);
// }

// [[Rcpp::export]]
arma::umat Rcpp_constrained_delaunay(
    const arma::mat & points, const arma::umat & edges
){
  Triangulation cdt(CDT::VertexInsertionOrder::AsProvided);
  size_t npoints = points.n_rows;
  std::vector<Vertex> vertices(npoints);
  for (size_t i = 0; i < npoints; ++i) {
    const arma::rowvec row_i = points.row(i);
    vertices[i] = Vertex::make(row_i(0), row_i(1));
  }
  size_t nedges = edges.n_rows;
  // for(size_t i = 0; i < nedges; ++i){
  //   const arma::urowvec edge = edges.row(i);
  //   cdt.insert_constraint(vertices[edge[0]], vertices[edge[1]]);
  // }
  std::vector<Edge> Edges;
  //Sampleclass *qs = new Edge();
  for (size_t i = 0; i < nedges; ++i) {
    const arma::urowvec row_i = edges.row(i);
    Edges.push_back(Edge(row_i(0), row_i(1)));
    // Edge *edge = new (row_i(0), row_i(1)) Edge;
    // //edge = Edge(row_i(0), row_i(1));//(row_i(0), row_i(1)));
    // Edges[i] = *edge;
  }
  cdt.insertVertices(vertices);
  cdt.insertEdges(Edges);
  cdt.eraseOuterTrianglesAndHoles();
  const CDT::TriangleVec triangles = cdt.triangles;
  arma::umat out(triangles.size(), 3);
  for(size_t i = 0; i < triangles.size(); ++i){
    const CDT::VerticesArr3 trgl = triangles[i].vertices;
    out(i, 0) = trgl[0];
    out(i, 1) = trgl[1];
    out(i, 2) = trgl[2];
  }
  return out;
}
