
.onAttach <- function(libname, pkgname){
  options( java.parameters = "-Xmx4G" )
  packageStartupMessage("Java memory set to 4 gigabytes, if further changes are required use 'options(java.parameters = '-Xmx4g')' and change the value. This is required for bartMachine, restarting R may be required if analysis has taken place with rJava prior")
}
