#' Import a structured justifier object from JSON
#'
#' @param x Either a path to an existing file, or a character vector with the
#' JSON to import.
#'
#' @return The justifier object.
#' @export
#'
#' @examples ### Programmatically create a justification with two assertions
#' ### but without sources; flatten it; and show the json
#' justifier::justify(
#'   "Icecream will make me feel less fit",
#'   assertion = c(
#'     justifier::assert('Icecream is rich in energy'),
#'     justifier::assert('Consuming high-energy foods makes me feel less fit')
#'   ),
#'   weight = -.5
#' ) |>
#'   justifier::flatten() -> originalObject;
#'
#' originalObject |>
#'   justifier::export_to_json() ->
#'   exportedJSON;
#'
#' ### And import it again
#' importedFromJSON <-
#'   justifier::import_from_json(
#'     exportedJSON
#'   );
import_from_json <- function(x) {

  if (!requireNamespace('jsonlite', quietly=TRUE)) {
    stop("You need to have 'jsonlite' installed to convert to JSON!");
  }

  if ((is.character(x)) && (length(x) == 1) && (file.exists(x))) {
    res <- jsonlite::read_json(path = x);
  } else if ((is.character(x))) {
    res <- jsonlite::fromJSON(x);
  } else {
    stop("As `x`, you have to pass either a character vector containing ",
         "the JSON, or the path to an existing file to import.");
  }

  ### Sources

  if (is.null(res$sources) || (length(res$sources) == 0)) {
    res$sources <- structure(
      list(),
      class = c("justifier", "justifierStructured", "justifierSource", "list")
    );
  } else {
    res$sources <-
      lapply(res$sources,
             `class<-`,
             c("justifierSource",
               "singleJustifierElement",
               "justifierElement",
               "justifier"));
    class(res$sources) <- c("justifier", "justifierStructured", "justifierSource", "list");
  }

  ### Assertions

  if (is.null(res$assertions) || (length(res$assertions) == 0)) {
    res$assertions <- structure(
      list(),
      class = c("justifier", "justifierStructured", "justifierAssertion", "list")
    );
  } else {
    res$assertions <-
      lapply(
        res$assertions,
        replace_empty_lists_with_null,
        select = "source"
      );
    res$assertions <-
      lapply(
        res$assertions,
        set_class_selectively,
        select = "source",
        class = c("justifier", "justifierIdRef", "justifierSource", "character")
      );
    res$assertions <-
      lapply(res$assertions,
             `class<-`,
             c("justifierAssertion", "singleJustifierElement", "justifierElement", "justifier"));
    class(res$assertions) <- c("justifier", "justifierStructured", "justifierAssertion", "list");
  }

  ### Justifications

  if (is.null(res$justifications) || (length(res$justifications) == 0)) {
    res$justifications <- structure(
      list(),
      class = c("justifier", "justifierStructured", "justifierJustification", "list")
    );
  } else {
    res$justifications <-
      lapply(
        res$justifications,
        replace_empty_lists_with_null,
        select = "assertion"
      );
    res$justifications <-
      lapply(
        res$justifications,
        set_class_selectively,
        select = "assertion",
        class = c("justifier", "justifierIdRef", "justifierAssertion", "character")
      );
    res$justifications <-
      lapply(res$justifications,
             `class<-`,
             c("justifierJustification", "singleJustifierElement", "justifierElement", "justifier"));
    class(res$justifications) <- c("justifier", "justifierStructured", "justifierJustification", "list");
  }

  ### Decisions

  if (is.null(res$decisions) || (length(res$decisions) == 0)) {
    res$decisions <- structure(
      list(),
      class = c("justifier", "justifierStructured", "justifierDecision", "list")
    );
  } else {
    res$decisions <-
      lapply(
        res$decisions,
        replace_empty_lists_with_null,
        select = "justification"
      );
    res$decisions <-
      lapply(
        res$decisions,
        set_class_selectively,
        select = "justification",
        class = c("justifier", "justifierIdRef", "justifierJustification", "character")
      );
    res$decisions <-
      lapply(res$decisions,
             `class<-`,
             c("justifierDecision",
               "singleJustifierElement",
               "justifierElement",
               "justifier"));
    class(res$decisions) <- c("justifier", "justifierStructured", "justifierDecision", "list");
  }

  ### Justifier config

  if (is.null(res$justifier)) {
    res$justifier <- structure(
      list(),
      class = c("justifier", "justifierStructured", "justifierJustifier", "list")
    );
  } else {
    res$justifier <-
      lapply(res$justifier,
             `class<-`,
             c("justifierJustifier", "singleJustifierElement", "justifierElement", "justifier"));
    class(res$justifier) <- c("justifier", "justifierStructured", "justifierJustifier", "list");
  }

  class(res) <-
    c("justifier", "justifierStructuredObject", "list");

  return(res);

}

replace_empty_lists_with_null <- function(x,
                                          select) {

  if (is.null(x) || (length(x) == 0)) {
    return(x);
  }

  if (is.list(x[[select]]) &&
      (length(x[[select]]) == 0)) {
    ### This removes `select` from x
    x[[select]] <- NULL;
    ### Add it again
    x <-
      c(x,
        stats::setNames(list(NULL), nm = select)
      );
  }

  return(x);

}

set_class_selectively <- function(x,
                                  select,
                                  class) {

  if (is.null(x) || (length(x) == 0)) {
    return(x);
  }

  if (is.character(x[[select]]) &&
      (length(x[[select]]) > 0)) {
    class(x[[select]]) <- class;
  }

  return(x);

}
