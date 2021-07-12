which_child_specs <- function(x,
                              childNames = c('source',
                                             'assertion',
                                             'justification',
                                             'decision',
                                             'justifier')) {
  if (is.null(names(x))) {
    return(FALSE);
  } else if (any(names(x) %in% childNames)) {
    return(which(names(x) %in% childNames));
  } else {
    return(FALSE);
  }
}
