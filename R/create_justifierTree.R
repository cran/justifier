create_justifierTree <- function(x,
                                 silent = TRUE) {

  ### Call 'buildListWithChildren' from the right starting point; then
  ### that function will recurse to structure data.tree's 'explicit list'
  ### properly

  targetElement <-
    names(parentChildRelationships)[
      which(names(parentChildRelationships) %in% names(x))[1]
    ];

  if (is.na(targetElement)) {
    msg("Identified no known target element among the names of `x`, so ",
        "assuming it has no children. (The names of `x` are: ",
        vecTxtQ(names(x)), ").",
        silent = silent);
    x$children <- list("");
    names(x$children) <- NA;
  } else {
    msg("Identified ", targetElement, " as top-level target element to ",
        "process; proceeding to recursively build tree from that level.\n",
        silent = silent);

    x <-
      buildExplicitDataTreeList(
        x,
        targetElement = targetElement,
        childElement = parentChildRelationships[targetElement],
        silent = silent
      );
  }

  if ("id" %in% names(x)) {
    x <- list(x);
    names(x) <- x[[1]]$id;
  }

  res <-
    lapply(names(x),
           function(decisionId) {
             if (all(is.na(names(x[[decisionId]]$children))) &&
                 (nchar(x[[decisionId]]$children[[1]]) == 0)) {
               msg("No children available, building a tree with only a root.\n",
                   silent = silent);
               x[[decisionId]]$children <- NULL;
             } else {
               msg("Returning a tree with ", length(x[[decisionId]]$children),
                   " children.\n",
                   silent = silent);
             }
             res <-
               data.tree::FromListExplicit(explicitList = x[[decisionId]],
                                           nameName="id",
                                           childrenName="children",
                                           nodeName=decisionId);
             return(res);
           });

  names(res) <- names(x);

  class(res) <- c("justifierTree",
                  class(res));

  return(res);

}

parentChildRelationships = c(
  decision = 'justification',
  justification = 'assertion',
  assertion = 'source',
  source = NULL
)

justifierClasses = c(
  decision = 'justifierDecisionList',
  justification = 'justifierJustificationList',
  assertion = 'justifierAssertionList',
  source = 'justifierSourceList'
)

buildExplicitDataTreeList <- function(x,
                                      targetElement,
                                      childElement = NULL,
                                      silent=TRUE) {

  if (!silent) {
    cat0("Starting to build explicit data tree list for target element '",
         targetElement, "' and ");
    if (is.null(childElement)) {
      cat0("no child element specified.\n");
    } else {
      cat0("child element '", childElement, "'.\n");
    }
  }

  ### If this is a vector (e.g. a source with just an id), return it.
  if (is.atomic(x)) {
    if (!silent) {
      cat0("An atomic object was passed, returning as-is!");
    }
    return(x);
  }

  ### If this is a list without the indicated children, return it unclassed.
  if (!(targetElement %in% names(x))) {
    if (!silent) {
      cat0("The passed object did not contain anything with the target ",
           "element name, unclassing and returning!");
    }
    return(unclass(x));
  }

  ### Data Tree can create a tree of an 'explicit list', which
  ### basically wants the children to be in an element called
  ### 'children'.
  if (!is.null(x$children)) {
    x$children_old <- x$children;
  }

  x$children <- x[[targetElement]];

  if (all(unlist(lapply(x$children, is.list)))) {
    ### Only in this case, `lapply` through the lists; otherwise, we
    ### have only one child without the 'intermediate list', so introduce
    ### that
    x$children <-
      lapply(x$children,
             function(child) {
               # class(child) <- justifierClasses[targetElement];
               # child$justifierType <- justifierClasses[targetElement];
               return(child);
             });
  } else {
    ### Add the 'intermediate list'
    x$children <- list(x$children);
    # class(x$children) <- justifierClasses[targetElement];
    # x$children$justifierType <- justifierClasses[targetElement];
  }

  x[targetElement] <- NULL;

  if (!is.null(childElement)) {
    x$children <-
      lapply(
        x$children,
        buildExplicitDataTreeList,
        targetElement = childElement,
        childElement = parentChildRelationships[childElement]
      );
  }

  names(x$children) <-
    unlist(lapply(x$children, function(y) {
      if (is.atomic(y)) {
        return(y['id']);
      } else {
        return(y$id);
      }
    }));

  return(unclass(x));
}
