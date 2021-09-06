#' Programmatically constructing justifier elements
#'
#' These functions can be used to programmatically construct justifications.
#'
#' @param label A human-readable label for the `decision`, `justification`,
#' `assertion`, or `source`. Labels are brief summaries of the core of the
#' decision, justification, assertion, or source. More details, background
#' information, context, and other comments can be placed in the description.
#' @param description A human-readable description. This can be used to
#' elaborate on the label. Note that the label should be reader-friendly and
#' self-contained; but because they also have to be as short as possible,
#' descriptions can be used to provide definitions, context, background
#' information, or add any other metadata or comments.
#' @param type Types are used when working with a framework. Frameworks define
#' type identifiers, consisting of letters, digits, and underscores. By
#' specifying these identifiers the type of a decision, justification,
#' assertion, or source. Source types can be, for example, types of documents
#' or other data providers, such as "empirical evidence', 'expert consensus',
#' 'personal opinion', or 'that one meeting that we had in May'. Assertion
#' types can be, for example, data types or types of facts, such as 'number',
#' 'prevalence', 'causal relationship', or 'contact information'.
#' Justification types can be, for example, types of reasoning or logical
#' expressions, such as 'deduction', 'induction', or 'intersection'. Decision
#' types are the most framework-specific, heavily depend on the specific
#' context of the decision, and are used by frameworks to organise the
#' decisions in a project. Examples of decision types are the decision to
#' recruit a certain number of participants in a scientific study; the decision
#' to target a certain belief in a behavior change intervention; the decision
#' to merge two codes in a qualitative study; the decision to hire a staff
#' member; or the decision to make a certain purchase.
#' @param xdoi For `source`s, XDOI identifier (a DOI, or, if that does not
#' exist, ISBN or other unique identifier of the source).
#' @param source In assertions, the source (or sources) that the assertion
#' is based on can be specified using `srce()`.
#' @param alternatives The alternatives that were considered in a decision.
#' @param assertion In justifications, the assertion (or assertions) that
#' the justification is based on can be specified using `asrt()`.
#' @param justification In decisions, the justification (or justifications)
#' that the decision is based on can be specified using `jstf()`.
#' @param id The identifier (randomly generated if omitted).
#' @param ... Additional fields and values to store in the element.
#'
#' @rdname constructingJustifications
#' @aliases dcsn jstf asrt srce decide decision assert justify source
#' @return The generated object.
#'
#' @examples ### Programmatically create a partial justification object
#' justifierAssertion <-
#'   justifier::assert(
#'     "This is an assertion",
#'     source = c(
#'       justifier::source('This is a first source'),
#'       justifier::source('This is a second source')));
#'
#' ### Programmatically create a justification with two assertions
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
#' ### Show it
#' justifierJustification;
#'
#' ### Programmatically create a simple decision
#' simpleDecision <-
#'   justifier::decide(
#'     "decision",
#'     justification = justifier::jstf(
#'       "justification",
#'       assertion = justifierAssertion
#'     )
#'   );
#'
#' ### Programmatically create a justification object for a full decision
#' fullJustifierObject <-
#'   justifier::decide(
#'     "I decide to go get an icecream",
#'     justification = c(
#'       justifier::justify(
#'         "Having an icecream now would make me happy",
#'         assertion = c(
#'           justifier::assert(
#'             "Decreasing hunger increases happiness",
#'             source = justifier::source(
#'               "My past experiences"
#'             )
#'           ),
#'           justifier::assert(
#'             "I feel hungry",
#'             source = justifier::source(
#'               "Bodily sensations"
#'             )
#'           )
#'         ),
#'         weight = 1
#'       ),
#'       justifierJustification,
#'       justifier::justify(
#'         "I can afford to buy an icecream.",
#'         assertion = c(
#'           justifier::assert(
#'             "My bank account balance is over 300 euro.",
#'             source = justifier::source(
#'               "My bank app"
#'             )
#'           ),
#'           justifier::assert(
#'             "I need to keep at least 100 euro in my bank account.",
#'             source = justifier::source(
#'               "Parental advice"
#'             )
#'           )
#'         ),
#'         weight = .3
#'       )
#'     )
#'   );
#'
#' ### Show the full object
#' fullJustifierObject;
#'
#' ### Combine both into a list of decisions
#' twoDecisions <-
#'   c(simpleDecision,
#'     fullJustifierObject);
#'
#' ### Show the combination
#' twoDecisions;
#'
#' @export source
#' @export srce
source <-
  srce <- function(label,
                   description = NULL,
                   type = NULL,
                   id = NULL,
                   xdoi = NULL,
                   ...) {
  return(justifierObjectConstructor(justifierType = "S",
                                    id=id,
                                    label = label,
                                    description = description %||% "",
                                    xdoi = xdoi %||% "",
                                    type = type %||% "",
                                    ...));
}

#' @export assert
#' @export asrt
#' @rdname constructingJustifications
assert <-
  asrt <-
          function(label,
                   description = "",
                   type = NULL,
                   id = NULL,
                   source = NULL,
                   ...) {
  return(justifierObjectConstructor(justifierType = "A",
                                    id=id,
                                    label = label,
                                    description = description %||% "",
                                    type = type %||% "",
                                    source = source %||% NULL,
                                    ...));
}

#' @export justify
#' @export jstf
#' @rdname constructingJustifications
justify <-
   jstf <- function(label,
                    description = "",
                    type = NULL,
                    id = NULL,
                    assertion = NULL,
                    ...) {
  return(justifierObjectConstructor(justifierType = "J",
                                    id=id,
                                    label = label,
                                    description = description %||% "",
                                    type = type %||% "",
                                    assertion = assertion %||% NULL,
                                    ...));
}

#' @export dcsn
#' @export decision
#' @export decide
#' @rdname constructingJustifications
decide <-
  decision <-
  dcsn <- function(label,
                   description = NULL,
                   type = NULL,
                   id = NULL,
                   alternatives = NULL,
                   justification = NULL,
                   ...) {
  return(justifierObjectConstructor(justifierType = "D",
                                    id=id,
                                    label = label,
                                    description = description %||% "",
                                    type = type %||% "",
                                    justification = justification %||% "",
                                    ...));
}

#' @export
#' @method print singleJustifierElement
print.singleJustifierElement <- function(x, ...) {

  if (is.null(x) || (length(x) == 0)) {
    cat("You passed an empty justifier element.");
    return(invisible(x));
  }

  cat0("Justifier element of type '",
       class(x)[1], "' and with id '",
       x$id,
       "'.\n");
  plot(x);
  return(invisible(x));
}

#' @export
#' @method print multipleJustifierElements
print.multipleJustifierElements <- function(x, ...) {

  if (is.null(x) || (length(x) == 0)) {
    cat("You passed an empty list of justifier elements.");
  }

  cat0("A list of ", length(x), " justifier elements of ",
       "type ", class(x[[1]])[1], " and with identifiers ",
       vecTxtQ(unlist(lapply(x, function(y) return(y$id)))));
  plot(x);
  return(invisible(x));
}

#' @export
#' @method plot singleJustifierElement
plot.singleJustifierElement <- function(x, ...) {

  if (is.null(x) || (length(x) == 0)) {
    return(invisible(x));
  }

  tree <-
    create_justifierTree(
      x
    );

  graph <- create_justifierGraph(
    tree[[1]]
  );

  print(graph);

  return(invisible(x));

}

#' @export
#' @method plot multipleJustifierElements
plot.multipleJustifierElements <- function(x, ...) {

  for (i in seq_along(x)) {
    plot(x[[i]]);
  }

  return(invisible(x));

}

###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------

justifierObjectConstructor <-
  function(justifierType,
           id = NULL,
           ...) {

    justifierType <- toupper(justifierType);
    if (length(justifierType) > 1) {
      stop("I can only generate one justifier object!");
    }
    if (!any(c("D", "J", "A", "S") == justifierType)) {
      stop("I can only generate an object of type D(ecision), ",
           "J(ustification), A(ssertion), or S(ource)!");
    }

    if (is.null(id)) {
      id <- generate_id(type=justifierType)
    }

    res <- c(list(id = id),
             list(...));

    justifierClasses <-
      list(D = "justifierDecision",
           J = "justifierJustification",
           A = "justifierAssertion",
           S = "justifierSource");

    class(res) <-
      unname(
        c(justifierClasses[justifierType],
          "singleJustifierElement",
          "justifierElement",
          "justifier")
      );

    return(res);

  }

###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
