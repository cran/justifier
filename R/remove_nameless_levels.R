remove_nameless_levels <- function(x,
                                   newNames = "id",
                                   errorOnMissingNewName = TRUE) {
  if (!is.list(x)) {
    ### If x is not a list, just return it
    return(x);
  } else {
    if (!is.null(names(x))) {
      ### If x has names, pass all lists in x on and collect and return the results
      indicesOfLists <-
        which(lapply(x,
                     is.list));
      if (length(indicesOfLists) == 0) {
        ### If all elements of x are atomic, since x has names, we can just
        ### return x; nothing more to do here.
        return(x);
      } else {
        x[indicesOfLists] <-
          lapply(x[indicesOfLists],
                 remove_nameless_levels,
                 newNames = newNames,
                 errorOnMissingNewName = errorOnMissingNewName);
        return(x);
      }
    } else {
      ### If x does not have names, but is a list, check whether its elements
      ### all have at least one field with a name in newNames, and collect
      ### those, prioritizing based on the order provided in newNames; then
      ### name each element for its id
    }
  }
}
