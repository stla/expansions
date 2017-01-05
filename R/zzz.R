.onLoad <- function(libname, pkgname) {
  library.dynam("ExpansionsR", pkgname, libname, now=TRUE)
  .C("HsStart")
  invisible()
}

.onUnLoad <- function(libpath) {
  library.dynam.unload("ExpansionsR", libpath)
  invisible()
}
