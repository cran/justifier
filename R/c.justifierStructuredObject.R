#' Concatenate two or more structured justifier objects
#'
#' @param ... Structured justifier objects
#'
#' @return Invisibly, the concatenated list
#' @export
#'
#' @method c justifierStructuredObject
#' @export
#' @rdname structuredJustifications
c.justifierStructuredObject <- function(...) {

  ### Get arguments in a list
  dots <- list(...);

  ### https://stackoverflow.com/questions/55019441/deparse-substitute-with-three-dots-arguments
  # args <- lapply(
  #   as.list(match.call()[-1L]),
  #   substitute
  # );
  # args <- unlist(lapply(args, deparse));

  dots <- dots[!is.null(dots)];

  dots <- dots[!unlist(lapply(dots, function(x) return(identical(list(), unclass(x)))))];

  # args <- args[!is.null(dots)];

  if (!(all(unlist(lapply(dots, inherits, "justifierStructuredObject"))))) {
    passedClasses <- unlist(lapply(lapply(dots, class), vecTxtQ));
    stop("I can only concatenate objects with class ",
         "'justifierStructuredObject' (i.e. a complete structured justifier ",
         "object containing sources, assertions, justifications, decisions, ",
         " and additional justifier metadata or configuration information), ",
         "but you passed at least one object with (an) unknown class(es). ",
         paste0("argument ", 1:length(passedClasses),
                " has class(es) ", passedClasses,
                collapse = "; "), ".");
  }

  res <-
    list(
      sources =
        unlist(
          do.call(c, lapply(dots, function(x) return(x$sources))),
          recursive = FALSE
        ),
      assertions =
        unlist(
          do.call(c, lapply(dots, function(x) return(x$assertions))),
           recursive = FALSE
        ),
      justifications =
        unlist(
          do.call(c, lapply(dots, function(x) return(x$justifications))),
          recursive = FALSE
        ),
      decisions =
        unlist(
          do.call(c, lapply(dots, function(x) return(x$decisions))),
          recursive = FALSE
        ),
      justifier =
        unlist(
          do.call(c, lapply(dots, function(x) return(x$justifier))),
          recursive = FALSE
        )
    );

  ### Just in case
  if (is.null(res$sources)) {
    res$sources <- structure(
      list(),
      class = c("justifier", "justifierStructured", "justifierSource", "list")
    );
  } else {
    class(res$sources) <- c("justifier", "justifierStructured", "justifierSource", "list");
  }
  if (is.null(res$assertions)) {
    res$assertions <- structure(
      list(),
      class = c("justifier", "justifierStructured", "justifierAssertion", "list")
    );
  } else {
    class(res$assertions) <- c("justifier", "justifierStructured", "justifierAssertion", "list");
  }
  if (is.null(res$justifications)) {
    res$justifications <- structure(
      list(),
      class = c("justifier", "justifierStructured", "justifierJustification", "list")
    );
  } else {
    class(res$justifications) <- c("justifier", "justifierStructured", "justifierJustification", "list");
  }
  if (is.null(res$decisions)) {
    res$decisions <- structure(
      list(),
      class = c("justifier", "justifierStructured", "justifierDecision", "list")
    );
  } else {
    class(res$decisions) <- c("justifier", "justifierStructured", "justifierDecision", "list");
  }
  if (is.null(res$justifier)) {
    res$justifier <- structure(
      list(),
      class = c("justifier", "justifierStructured", "justifierJustifier", "list")
    );
  } else {
    class(res$justifier) <- c("justifier", "justifierStructured", "justifierJustifier", "list");
  }

  class(res) <-
    c("justifier", "justifierStructuredObject", "list");

  return(invisible(res));

}
