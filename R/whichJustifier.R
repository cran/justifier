whichJustifier <- function(x) {
  if (inherits(x, 'justifier')) {
    UseMethod("whichJustifier");
  } else {
    stop("You can only check what justifier objects are, but the object ",
         "you passed ('", deparse(substitute(x)),
         "') has class(es) ", vecTxtQ(class(x)), ".");
  }
}

#' @export
#' @method whichJustifier singleJustifierElement
whichJustifier.singleJustifierElement <- function(x) {
  res <- intersect(class(x), specificJustifierClasses);
  if (length(res) == 0) {
    return(NULL);
  } else {
    return(res);
  }
}

#' @export
#' @method whichJustifier multipleJustifierElements
whichJustifier.multipleJustifierElements <- function(x) {
  res <-
    unlist(
      lapply(
        x,
        whichJustifier
      )
    );
  uniqueRes <- unique(res);
  if (length(uniqueRes) == 1) {
    return(uniqueRes);
  } else {
    stop("You passed different justifier elements: ",
         vecTxtQ(uniqueRes), ".");
  }
}

#' @export
#' @method whichJustifier justifierIdRef
whichJustifier.justifierIdRef <- function(x) {
  res <- intersect(class(x), specificJustifierClasses);
  if (length(res) == 0) {
    return(NULL);
  } else {
    return(res);
  }
}

#' @export
#' @method whichJustifier justifierStructured
whichJustifier.justifierStructured <- function(x) {
  res <- intersect(class(x), specificJustifierClasses);
  if (length(res) == 0) {
    return(NULL);
  } else {
    return(res);
  }
}
