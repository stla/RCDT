// -*- mode: C++; c-indent-level: 2; c-basic-offset: 2; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"
// [[Rcpp::depends(RcppArmadillo)]]
#define NDEBUG

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
arma::umat Rcpp_delaunay(const arma::mat & tpoints){
  Triangulation cdt(CDT::VertexInsertionOrder::AsProvided);
  // insert vertices
  const size_t npoints = tpoints.n_cols;
  std::vector<Vertex> vertices(npoints);
  for (size_t i = 0; i < npoints; ++i) {
    const arma::vec col_i = tpoints.col(i);
    vertices[i] = Vertex::make(col_i(0), col_i(1));
  }
  cdt.insertVertices(vertices);
  cdt.eraseSuperTriangle();
  //// output
  // triangles
  const CDT::TriangleVec triangles = cdt.triangles;
  const size_t ntriangles = triangles.size();
  arma::umat out_triangles(3, ntriangles);
  for(size_t i = 0; i < ntriangles; ++i){
    const CDT::VerticesArr3 trgl = triangles[i].vertices;
    out_triangles.col(i) = {trgl[0] + 1, trgl[1] + 1, trgl[2] + 1};
  }
  // all edges
  // CDT::EdgeUSet allEdges = CDT::extractEdgesFromTriangles(triangles);
  // arma::umat out_alledges(allEdges.size(), 2);
  // std::unordered_set<Edge> :: iterator it;
  // size_t i = 0;
  // for(it = allEdges.begin(); it != allEdges.end(); it++){
  //   const Edge edge = *it;
  //   out_alledges(i, 0) = CDT::edge_get_v1(edge);
  //   out_alledges(i, 1) = CDT::edge_get_v2(edge);
  //   i++;
  // }
  //
  return out_triangles;
}

// void* operator new (size_t size, const unsigned & v1, const unsigned & v2) {
//   /* Do something, then return a pointer to at least 'size' bytes. */
//   return ::operator new(size);
// }

// [[Rcpp::export]]
Rcpp::List Rcpp_constrained_delaunay(
    const arma::mat & tpoints, const arma::umat & tedges
){
  CDT::Triangulation<double> cdt(
      CDT::VertexInsertionOrder::AsProvided,
      CDT::IntersectingConstraintEdges::Resolve,
      0.0
  );
  // insert vertices
  const size_t npoints = tpoints.n_cols;
  std::vector<Vertex> vertices(npoints);
  for (size_t i = 0; i < npoints; ++i) {
    const arma::vec col_i = tpoints.col(i);
    vertices[i] = Vertex::make(col_i(0), col_i(1));
  }
  cdt.insertVertices(vertices);
  // insert edges
  const size_t nedges = tedges.n_cols;
  std::vector<Edge> Edges;
  Edges.reserve(nedges);
  for (size_t i = 0; i < nedges; ++i) {
    const arma::uvec col_i = tedges.col(i);
    Edges.push_back(Edge(col_i(0) - 1, col_i(1) -1));
  }
  cdt.insertEdges(Edges);
  cdt.eraseOuterTrianglesAndHoles();
  //// output
  // vertices
  const std::vector<Vertex> cdt_vertices = cdt.vertices;
  const size_t nvertices = cdt_vertices.size();
  arma::mat out_vertices(2, nvertices);
  for(size_t k = 0; k < nvertices; ++k){
    const Vertex v = cdt_vertices[k];
    out_vertices.col(k) = {v.x, v.y};
  }
  // triangles
  const CDT::TriangleVec triangles = cdt.triangles;
  arma::umat out_triangles(3, triangles.size());
  for(size_t i = 0; i < triangles.size(); ++i){
    const CDT::VerticesArr3 trgl = triangles[i].vertices;
    out_triangles.col(i) = {trgl[0] + 1, trgl[1] + 1, trgl[2] + 1};
  }
  // border edges
  CDT::EdgeUSet borderEdges = cdt.fixedEdges;
  arma::umat out_edges(2, borderEdges.size());
  std::unordered_set<Edge> :: iterator itedge;
  size_t i = 0;
  for(itedge = borderEdges.begin(); itedge != borderEdges.end(); itedge++){
    const Edge edge = *itedge;
    out_edges.col(i) = {CDT::edge_get_v1(edge) + 1, CDT::edge_get_v2(edge) + 1};
    i++;
  }
  // all edges
  // CDT::EdgeUSet allEdges = CDT::extractEdgesFromTriangles(triangles);
  // arma::umat out_alledges(allEdges.size(), 2);
  // std::unordered_set<Edge> :: iterator it;
  // i = 0;
  // for(it = allEdges.begin(); it != allEdges.end(); it++){
  //   const Edge edge = *it;
  //   out_alledges(i, 0) = CDT::edge_get_v1(edge);
  //   out_alledges(i, 1) = CDT::edge_get_v2(edge);
  //   i++;
  // }
  //
  return Rcpp::List::create(
    Rcpp::Named("vertices") = out_vertices,
    Rcpp::Named("triangles") = out_triangles,
    Rcpp::Named("borderEdges") = out_edges
  );
}
