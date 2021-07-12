has_child_specs <- function(x,
                            childNames = c('source',
                                           'assertion',
                                           'justification',
                                           'decision',
                                           'justifier')) {
  if (is.null(names(x))) {
    return(FALSE);
  } else if (any(names(x) %in% childNames)) {
    return(TRUE);
  } else {
    return(FALSE);
  }
}
