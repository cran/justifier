is_spec <- function(x) {
  return(("justifierSpec" %in% class(x)) || length(names(x)) > 1);
}

is_ref <- function(x) {
  return(("justifierRef" %in% class(x)) || ((length(names(x)) == 0) && (is.character(x))));
}

is_nested <- function(x) {
  return(is.list(x) && any(unlist(lapply(x, is.list))));
}
