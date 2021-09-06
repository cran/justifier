#' Flatten a justifier tree
#'
#' Flattening takes all justifications, assertions, and sources from their
#' parents and returns a structured justifier object containing these
#' elements in separate lists, with each occurrence replaced with a reference
#' to the corresponding identifier.
#'
#' @param x The justifier object or objects.
#' @param ... Additional arguments are passed to the methods.
#' @param recursionLevel The depth of the recursion
#' @param silent Whether to be silent or chatty
#'
#' @return A flattened justifier object.
#' @rdname flatteningJustifierElements
#' @export
#'
#' @examples ### Programmatically create a justification with two assertions
#' ### but without sources
#' justifierJustification <-
#'   justifier::justify(
#'     "Icecream will make me feel less fit",
#'     assertion = c(
#'       justifier::assert('Icecream is rich in energy'),
#'       justifier::assert('Consuming high-energy foods makes me feel less fit')
#'     ),
#'     weight = -.5
#'   );
#'
#' ### Flatten it into a structures justifier object
#' structuredJustification <-
#'   justifier::flatten(
#'     justifierJustification
#'   );
#'
#' ### Check it
#' str(structuredJustification, 1);
flatten <- function(x,
                    ...,
                    recursionLevel = 0,
                    silent = justifier::opts$get("silent")) {
  if (inherits(x, 'justifierElement')) {
    UseMethod("flatten");
  } else {
    stop("You can only flatten justifier objects, but the object ",
         "you passed ('", deparse(substitute(x)),
         "') has class(es) ", vecTxtQ(class(x)), ".");
  }
}

#' @export
#' @rdname flatteningJustifierElements
#' @method flatten multipleJustifierElements
flatten.multipleJustifierElements <- function(x,
                                              ...,
                                              recursionLevel = 0,
                                              silent = justifier::opts$get("silent")) {

  msg(spc(recursionLevel),
      "Flattening multiple justifier objects; processing them one by one.\n",
      silent = silent);

  res <-
    do.call(
      c,
      lapply(
        x,
        flatten,
        recursionLevel = recursionLevel + 1,
        silent = silent)
    );

  class(res) <-
    c("justifier", "justifierStructuredObject", "list");

  return(res);

}

#' @export
#' @rdname flatteningJustifierElements
#' @method flatten singleJustifierElement
flatten.singleJustifierElement <- function(x,
                                           ...,
                                           recursionLevel = 0,
                                           silent = justifier::opts$get("silent")) {

  res <- emptyStructuredJustifierObject;

  ###---------------------------------------------------------------------------
  ### Source
  ###---------------------------------------------------------------------------

  if (inherits(x, "justifierSource")) {

    msg(spc(recursionLevel),
        "Flattening a single source into a structured justifier object.\n",
        silent = silent);

    res$sources <- list(x);

    names(res[["sources"]]) <-
      get_ids_from_structured_justifierElements(
        res[["sources"]]
      );

    class(res$sources) <-
      c("justifier", "justifierStructured", "justifierSource", "list");

    msg(spc(recursionLevel),
        "Returning a single structured source with identifier ",
        names(res[["sources"]]), ".\n",
        silent = silent);

    return(res);

  } else if (inherits(x, "justifierAssertion")) {

    singularName <- "assertion";

  } else if (inherits(x, "justifierJustification")) {

    singularName <- "justification";

  } else if (inherits(x, "justifierDecision")) {

    singularName <- "decision";

  }

  plural <- c(source = "sources",
              assertion = "assertions",
              justification = "justifications",
              decision = "decisions");

  pluralName <- plural[singularName];

  childNameVector <- c(assertion = "source",
                       justification = "assertion",
                       decision = "justification");

  childName <- childNameVector[singularName];

  childPlural <- plural[childName];

  ###---------------------------------------------------------------------------
  ### Process
  ###---------------------------------------------------------------------------

  msg(spc(recursionLevel),
      "Flattening a single ", singularName,
      " into a structured justifier object.\n",
      silent = silent);

  if (!has_justifierElement(x, childName)) {

    msg(spc(recursionLevel),
        "No ", childPlural, " specified.\n",
        silent = silent);

    oldClass <- class(res[[pluralName]]);

    ### No children to process
    res[[pluralName]] <- list(x);

    class(res[[pluralName]]) <- oldClass;

    names(res[[pluralName]]) <-
      get_ids_from_structured_justifierElements(
        res[[pluralName]]
      );

    msg(spc(recursionLevel),
        "Returning a single structured childless ", singularName,
        " with identifier ",
        names(res[[pluralName]]), ".\n",
        silent = silent);

  } else {

    if (inherits(x[[childName]], "singleJustifierElement")) {
      msg(spc(recursionLevel),
          "One ", childName, " specified. Recursing to structure it.\n",
          silent = silent);
    } else if (inherits(x[[childName]], "multipleJustifierElements")) {
      msg(spc(recursionLevel),
          length(x[[childName]]),
          " ", childPlural, " specified. Recursing to structure them.\n",
          silent = silent);
    } else {
      stop(spc(recursionLevel),
           "Should not happen.");
    }

    structuredChildren <-
      flatten(
        x[[childName]],
        recursionLevel = recursionLevel + 1,
        silent = silent
      );

    res <-
      c(res,
        structuredChildren);

    x[[childName]] <-
      justifier::idRef(
        structuredChildren[[childPlural]]
      );

    structuredTarget <-
      selective_flattening(
        x,
        what = singularName,
        recursionLevel = recursionLevel + 1,
        silent=silent);

    names(structuredTarget[[pluralName]]) <-
      get_ids_from_structured_justifierElements(
        structuredTarget[[pluralName]]
      );

    res <- c(
      res,
      structuredTarget
    );

    msg(spc(recursionLevel),
        "Returning a single structured ", singularName,
        " with identifier ",
        names(structuredTarget[[pluralName]]), ".\n",
        silent = silent);

  }

  class(res) <-
    c("justifier", "justifierStructuredObject", "list");

  return(res);

}

emptyStructuredJustifierObject <-
  structure(
    list(
      sources = structure(list(),
                          class=c("justifier", "justifierStructured", "justifierSource", "list")),
      assertions = structure(list(),
                             class=c("justifier", "justifierStructured", "justifierAssertion", "list")),
      justifications = structure(list(),
                                 class=c("justifier", "justifierStructured", "justifierJustification", "list")),
      decisions = structure(list(),
                            class=c("justifier", "justifierStructured", "justifierDecision", "list")),
      justifier = structure(list(),
                            class=c("justifier", "justifierStructured", "justifierJustifier", "list"))
    ),
    class = c("justifier", "justifierStructuredObject", "list")
  );
