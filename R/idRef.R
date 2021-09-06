#' Create a reference to one or more justifier objects
#'
#' @param x The identifier(s)
#' @param what Optionally, what `x` is (`decision`, `justification`,
#' `assertion`, or `source`).
#' @param silent Whether to be silent or chatty.
#'
#' @return The justifier id reference object.
#' @rdname referencingById
#' @export
#'
#' @examples exampleSource <-
#'   justifier::source("This is a book about R.");
#'
#' exampleAssertion <- justifier::assert(
#'   "R is a functional language",
#'   source = justifier::idRef(exampleSource)
#' );
#'
#' ### Get and show the reference
#' (sourceId <- exampleAssertion$source);
#'
#' sourceId <- as.character(sourceId);
#'
#' ### Manually assign an identifier
#' justifier::idRef(sourceId);
#'
#' ### Repeat while specifying what we're passing
#' justifier::idRef(sourceId, what="source");
idRef <- function(x,
                  what = NULL,
                  silent = justifier::opts$get("silent")) {

  if (inherits(x, 'justifier') ||
      is.character(x)) {
    UseMethod("idRef");
  } else {
    stop("As `x`, you have to pass one or more identifiers (i.e. of the ",
         "justifier entities you want to refer to) as character vector, or ",
         "one or more justifier objects. You passed an object ",
         "of class(es) ", vecTxtQ(class(x)), ".");
  }

}

#' @export
#' @rdname referencingById
#' @method idRef singleJustifierElement
idRef.singleJustifierElement <- function(x,
                                         what = NULL,
                                         silent = justifier::opts$get("silent")) {

  res <- x$id;

  specificClass <- whichJustifier(x);

  class(res) <-
    c("justifier", "justifierIdRef", specificClass, class(res));

  return(res);

}

#' @export
#' @rdname referencingById
#' @method idRef multipleJustifierElements
idRef.multipleJustifierElements <- function(x,
                                            what = NULL,
                                            silent = justifier::opts$get("silent")) {

  res <- get_ids_from_structured_justifierElements(x);

  specificClass <- whichJustifier(x);

  class(res) <-
    c("justifier", "justifierIdRef", specificClass, class(res));

  return(res);
}

#' @export
#' @rdname referencingById
#' @method idRef justifierIdRef
idRef.justifierIdRef <- function(x,
                                 what = NULL,
                                 silent = justifier::opts$get("silent")) {
  return(x);
}

#' @export
#' @rdname referencingById
#' @method idRef character
idRef.character <- function(x,
                            what = NULL,
                            silent = justifier::opts$get("silent")) {

  if (!is.null(what)) {
    specificClass <-
      paste0(
        "justifier",
        tools::toTitleCase(what)
      );
    class(x) <- c(specificClass, class(x));
  }

  class(x) <- c("justifier", "justifierIdRef", class(x));

  return(x);

}

#' @export
#' @rdname referencingById
#' @method idRef justifierStructured
idRef.justifierStructured <- function(x,
                                      what = NULL,
                                      silent = justifier::opts$get("silent")) {

  res <- get_ids_from_structured_justifierElements(x);

  if (is.null(res)) {
    return(NULL);
  }

  specificClass <- whichJustifier(x);

  class(res) <-
    c("justifier", "justifierIdRef", specificClass, class(res));

  return(res);

}

specificJustifierClasses <-
  c("justifierSource",
    "justifierAssertion",
    "justifierJustification",
    "justifierDecision",
    "justifierJustifier");
