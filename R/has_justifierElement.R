has_justifierElement <- function(x,
                                 what) {
  return(
    !(is.null(x[[what]]) || all(is.na(x[[what]])) || all((length(x[[what]]) == 0)))
  );
}
