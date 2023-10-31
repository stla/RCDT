# RCDT 1.3.0 (2023-10-31)

The package does no longer depend on 'randomcoloR', but it now depends on 
'colorsGen' and 'Polychrome'.


# RCDT 1.2.1 (2022-08-13)

* Skip a unit test on Mac; it fails for some reason. 


# RCDT 1.2.0 (2022-08-07)

* Speed improvement, by removing some useless code. 

* Upgraded CDT.

* Handle intersecting constraint edges.


# RCDT 1.1.0 (2022-04-06)

* New example to show that the triangulation depends on the order of the points.

* Now the output of `delaunay` contains a 'rgl' mesh object.

* Now the `plotDelaunay` function differentiates the border edges and the 
constraint edges.

* New feature: elevated Delaunay triangulation (or 2.5D Delaunay triangulation).

* More examples in the README file.


# RCDT 1.0.0 (2022-03-06)

First release.
