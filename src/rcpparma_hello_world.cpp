// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]

#include "CDT.h"
typedef CDT::V2d<double> Vertex;
typedef CDT::Triangulation<double> Triangulation;


CDT::TriangleVec triangulate(const arma::mat & points){
  Triangulation cdt(CDT::VertexInsertionOrder::AsProvided); 
  std::vector<Vertex> vertices(points.n_rows);
  for (size_t i = 0; i < points.n_rows; ++i) {
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
  
// simple example of creating two matrices and
// returning the result of an operatioon on them
//
// via the exports attribute we tell Rcpp to make this function
// available from R
//
// [[Rcpp::export]]
arma::mat rcpparma_hello_world() {
    arma::mat m1 = arma::eye<arma::mat>(3, 3);
    arma::mat m2 = arma::eye<arma::mat>(3, 3);
	                     
    return m1 + 3 * (m1 + m2);
}


// another simple example: outer product of a vector, 
// returning a matrix
//
// [[Rcpp::export]]
arma::mat rcpparma_outerproduct(const arma::colvec & x) {
    arma::mat m = x * x.t();
    return m;
}

// and the inner product returns a scalar
//
// [[Rcpp::export]]
double rcpparma_innerproduct(const arma::colvec & x) {
    double v = arma::as_scalar(x.t() * x);
    return v;
}


// and we can use Rcpp::List to return both at the same time
//
// [[Rcpp::export]]
Rcpp::List rcpparma_bothproducts(const arma::colvec & x) {
    arma::mat op = x * x.t();
    double    ip = arma::as_scalar(x.t() * x);
    return Rcpp::List::create(Rcpp::Named("outer")=op,
                              Rcpp::Named("inner")=ip);
}
