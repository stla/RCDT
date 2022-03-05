// -*- mode: C++; c-indent-level: 2; c-basic-offset: 2; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"
// [[Rcpp::depends(RcppArmadillo)]]

#include "CDT.h"

typedef CDT::V2d<double> Vertex;
typedef CDT::Triangulation<double> Triangulation;


CDT::TriangleVec triangulate(const arma::mat & points){
  Triangulation cdt(CDT::VertexInsertionOrder::AsProvided);
  size_t npoints = points.n_rows;
  std::vector<Vertex> vertices(npoints);
  for (size_t i = 0; i < npoints; ++i) {
    const arma::rowvec row_i = points.row(i);
    vertices[i] = Vertex::make(row_i(0), row_i(1));
  }
  cdt.insertVertices(vertices);
  cdt.eraseSuperTriangle();
  return cdt.triangles;
}

// [[Rcpp::export]]
arma::umat getTriangles(const arma::mat & points){
  const CDT::TriangleVec triangles = triangulate(points);
  arma::umat out(triangles.size(), 3);
  for(size_t i = 0; i < triangles.size(); ++i){
    const CDT::VerticesArr3 trgl = triangles[i].vertices;
    out(i, 0) = trgl[0];
    out(i, 1) = trgl[1];
    out(i, 2) = trgl[2];
  }
  return out;
}
