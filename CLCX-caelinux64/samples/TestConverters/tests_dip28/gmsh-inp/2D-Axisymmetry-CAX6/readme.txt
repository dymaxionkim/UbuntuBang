Load geo file into GMSH
Mesh it, apply second order
Export as INP with no options
selected.
In Launcher, use GMSH INP 
converter with 2 CAX key.
You will optain .._OUT.inp
file (mesh Type=CAX6 and groups)
Directions of normales should be 
positive to avoid gen3dim issue
when calculation.
