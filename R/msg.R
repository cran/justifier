msg <- function(...,
                silent = justifier::opts$get("silent")) {
  if (!silent) {
    cat0(...);
  }
  return(
    invisible(
      paste0(
        ...
      )
    )
  );
}
