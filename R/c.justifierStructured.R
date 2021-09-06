#' @method c justifierStructured
#' @export
#' @rdname constructingJustifications
c.justifierStructured <- function(...) {

  ### Get arguments in a list
  res <- list(...);

  elementType <- unlist(lapply(res, whichJustifier));

  if (length(unique(elementType)) != 1) {
    stop("All elements to concatenate must be of the same type! ",
         "So either all decisions, or all justifications, or all ",
         "assertions, or all sources - you passed elements of types ",
         vecTxtQ(elementType), ", respectively.");
  }

  elementType <- unique(elementType);

  res <- res[which(unlist(lapply(res, length)) > 0)];

  ### If any of the arguments does itself have multiple elements,
  ### we need to place the single elements in lists.
  if (any(unlist(lapply(res, inherits, "multipleJustifierElements")))) {
    res <-
      lapply(res,
             function(x) {
               return(ifelseObj(inherits(x, "singleJustifierElement"),
                                structure(list(x),
                                          class = c(elementType,
                                                    "singleJustifierElement",
                                                    "justifierElement",
                                                    "justifier")),
                                x));
             });
    ### ... And then remove one level of lists
    res <- unlist(res,
                  recursive = FALSE);
  }

  ### Set names to identifiers
  ### We can't do this until the parser can handle it.
  # names(res) <-
  #   unlist(lapply(res,
  #                 function(x) {
  #                   return(x$id);
  #                 }));

  class(res) <-
    c(elementType,
      "justifierStructured",
      "multipleJustifierElements",
      "justifierElement",
      "justifier"
    );

  return(res);

}
