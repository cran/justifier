#' Parsing justifications
#'
#' This function is normally called by [load_justifications()]; however,
#' sometimes it may be desirable to parse justifications embedded in more
#' complex objects, for example as provided by [yum::load_and_simplify()].
#' Therefore, this function can also be called directly.
#'
#' While there is some flexibility in how justifications can be specified,
#' they are most easily processed further if they all follow the same
#' conventions. This function ensures this. The convention is as follows:
#' - all specifications are provided in four 'flat' lists, named after the
#'   types of elements they contain;
#' - all elements have  a unique identifier
#' - all references to other elements are indeed only references to the other
#'   elements' id's in these 'flat lists'
#'
#' @param x An object resulting from a call to [yum::load_and_simplify()].
#'
#' @return The parsed `justifier` object
#'
#' @examples ### Specify an example text
#' exampleFile <-
#'   system.file("extdata",
#'               "simple-example.jmd",
#'               package="justifier");
#'
#' ### Show contents
#' cat(readLines(exampleFile), sep="\n");
#'
#' ### Load it with yum::load_and_simplify()
#' loadedMinutes <- yum::load_and_simplify(exampleFile);
#'
#' ### Show contents
#' names(loadedMinutes);
#'
#' ### Parse 'manually'
#' parsedJustifications <- justifier::parse_justifications(loadedMinutes);
#'
#' ### Show contents
#' names(parsedJustifications);
#'
#' @export
parse_justifications <- function(x) {

  res <- list(raw = x);

  ### Process all justifications and create four organised lists,
  ### where id's are used as names
  justNames <- tolower(names(x));
  res$structured <-
    list(sources = to_specList(x[which(justNames == 'source')],
                               types="sources",
                               type="source"),
         assertions = to_specList(x[which(justNames == 'assertion')],
                                  types="assertions",
                                  type="assertion"),
         justifications = to_specList(x[which(justNames == 'justification')],
                                      types="justifications",
                                      type="justification"),
         decisions = to_specList(x[which(justNames == 'decision')],
                                 types="decisions",
                                 type="decision"));

  ### Set names
  res$structured <- lapply(res$structured, function(elementList) {
    elementIds <-
      lapply(elementList, function(singleElement) {
        return(ifelse(is.null(singleElement$id),
                      "noID",
                      singleElement$id));
      });
    ### Set ids for elements without an id (shouldn't happen;
    ### may want to throws an error instead)
    if (any(elementIds=="noID")) {
      warning("Some elements don't have an ID!");
    }
    elementIds[elementIds=="noID"] <-
      paste0("id_",
             1:length(elementIds[elementIds=="noID"]));
    names(elementList) <-
      elementIds;
    return(elementList);
  });

  ### Check all decisions for justifications that were specified there,
  ### instead of simply being references that were specified as
  ### 'root' elements
  for (i in seq_along(res$structured$decisions)) {
    if (!is.null(res$structured$decisions[[i]]$justification)) {
      ### Flatten this justification, in case it is a list with a redundant level
      res$structured$decisions[[i]]$justification <-
        to_specList(res$structured$decisions[[i]]$justification,
                    types="justifications", type="justification");
      ### Process the elements one by one
      for (j in seq_along(res$structured$decisions[[i]]$justification)) {
        ### Check whether it is a specification (or one or more references)
        if (is_spec(res$structured$decisions[[i]]$justification[[j]])) {
          ### Check whether it already exists in the root
          if ((length(names(res$structured$justifications)) > 0) &&
              res$structured$decisions[[i]]$justification[[j]]$id %in%
                names(res$structured$justifications)) {
            ### If so, merge them and store the result in the root
            res$structured$justifications[[res$structured$decisions[[i]]$justification[[j]]$id]] <-
              merge_specs(res$structured$justifications[[res$structured$decisions[[i]]$justification[[j]]$id]],
                          res$structured$decisions[[i]]$justification[[j]]);
          } else {
            ### If not, copy this to the root
            if (is.null(res$structured$decisions[[i]]$justification[[j]]$id)) {
              stop("Error: no id for:\n\n",
                   paste0(utils::capture.output(print(res$structured$decisions[[i]]$justification[[j]])),
                          collapse="\n"));
            }
            res$structured$justifications[[res$structured$decisions[[i]]$justification[[j]]$id]] <-
              res$structured$decisions[[i]]$justification[[j]];
          }
        }
      }
      ### Once they are all copied and/or merged, replace them with references
      if (is.list(res$structured$decisions[[i]]$justification)) {
        if (!is.null(names(res$structured$decisions[[i]]$justification)) &&
            (length(names(res$structured$decisions[[i]]$justification)) == 1) &&
            (names(res$structured$decisions[[i]]$justification) == "id")) {
          res$structured$decisions[[i]]$justification <-
            structure(res$structured$decisions[[i]]$justification$id,
                      class="justifierRef");
        } else {
          res$structured$decisions[[i]]$justification <-
            structure(unname(purrr::map_chr(res$structured$decisions[[i]]$justification, "id")),
                      class="justifierRef");
        }
      }
    }
  }

  ### Repeat the exact same procedure for justifications and assertions.
  ### Tihs could be done more efficiently, but becomes much harder to read and
  ### maintain. So, I'll defer to Hadley Wickham: He who gives up code safety
  ### for code speed deserves neither :-)
  ### https://twitter.com/hadleywickham/status/504368538874703872
  for (i in seq_along(res$structured$justifications)) {
    if (!is.null(res$structured$justifications[[i]]$assertion)) {
      ### Flatten this justification, in case it is a list with a redundant level
      res$structured$justifications[[i]]$assertion <-
        to_specList(res$structured$justifications[[i]]$assertion,
                    types="assertions", type="assertion");
      ### Process the elements one by one
      for (j in seq_along(res$structured$justifications[[i]]$assertion)) {
        ### Check whether it is a specification (or one or more references)
        if (is_spec(res$structured$justifications[[i]]$assertion[[j]])) {
          ### Check whether it already exists in the root
          if ((length(names(res$structured$assertions)) > 0) &&
                res$structured$justifications[[i]]$assertion[[j]]$id %in%
              names(res$structured$assertions)) {
            ### If so, merge them and store the result in the root
            res$structured$assertions[[res$structured$justifications[[i]]$assertion[[j]]$id]] <-
              merge_specs(res$structured$assertions[[res$structured$justifications[[i]]$assertion[[j]]$id]],
                          res$structured$justifications[[i]]$assertion[[j]]);
          } else {
            ### If not, copy this to the root
            res$structured$assertions[[res$structured$justifications[[i]]$assertion[[j]]$id]] <-
              res$structured$justifications[[i]]$assertion[[j]];
          }
        }
      }
      ### Once they are all copied and/or merged, replace them with references
      if (is.list(res$structured$justifications[[i]]$assertion)) {
        if (!is.null(names(res$structured$justifications[[i]]$assertion)) &&
            (length(names(res$structured$justifications[[i]]$assertion)) == 1) &&
            (names(res$structured$justifications[[i]]$assertion) == "id")) {
          res$structured$justifications[[i]]$assertion <-
            structure(res$structured$justifications[[i]]$assertion$id,
                      class="justifierRef");
        } else {
          res$structured$justifications[[i]]$assertion <-
            structure(unname(purrr::map_chr(res$structured$justifications[[i]]$assertion, "id")),
                      class="justifierRef");
        }
      }
    }
    ###
    ### Justifications can also contain *other* justifications. They're the only
    ### 'recursive type' in that sense.
    ###
    if (!is.null(res$structured$justifications[[i]]$justification)) {
      ### Flatten this justification, in case it is a list with a redundant level
      res$structured$justifications[[i]]$justification <-
        to_specList(res$structured$justifications[[i]]$justification,
                    types="justifications", type="justification");
      ### Process the elements one by one
      for (j in seq_along(res$structured$justifications[[i]]$justification)) {
        ### Check whether it is a specification (or one or more references)
        if (is_spec(res$structured$justifications[[i]]$justification[[j]])) {
          ### Check whether it already exists in the root
          if ((length(names(res$structured$justifications)) > 0) &&
                res$structured$justifications[[i]]$justification[[j]]$id %in%
              names(res$structured$justifications)) {
            ### If so, merge them and store the result in the root
            res$structured$justifications[[res$structured$justifications[[i]]$justification[[j]]$id]] <-
              merge_specs(res$structured$justifications[[res$structured$justifications[[i]]$justification[[j]]$id]],
                          res$structured$justifications[[i]]$justification[[j]]);
          } else {
            ### If not, copy this to the root
            res$structured$justifications[[res$structured$justifications[[i]]$justification[[j]]$id]] <-
              res$structured$justifications[[i]]$justification[[j]];
          }
        }
      }
      ### Once they are all copied and/or merged, replace them with references
      if (is.list(res$structured$justifications[[i]]$justification)) {
        if (!is.null(names(res$structured$justifications[[i]]$justification)) &&
            (length(names(res$structured$justifications[[i]]$justification)) == 1) &&
            (names(res$structured$justifications[[i]]$justification) == "id")) {
          res$structured$justifications[[i]]$justification <-
            structure(res$structured$justifications[[i]]$justification$id,
                      class="justifierRef");
        } else {
          res$structured$justifications[[i]]$justification <-
            structure(unname(purrr::map_chr(res$structured$justifications[[i]]$justification, "id")),
                      class="justifierRef");
        }
      }
    }

  }

  ### Then again for the assertions and sources.
  for (i in seq_along(res$structured$assertions)) {
    if (!is.null(res$structured$assertions[[i]]$source)) {
      ### Flatten this justification, in case it is a list with a redundant level
      res$structured$assertions[[i]]$source <-
        to_specList(res$structured$assertions[[i]]$source,
                    types="sources", type="source");
      ### Process the elements one by one
      for (j in seq_along(res$structured$assertions[[i]]$source)) {
        ### Check whether it is a specification (or one or more references)
        if (is_spec(res$structured$assertions[[i]]$source[[j]])) {
          ### Check whether it already exists in the root
          if ((length(names(res$structured$sources)) > 0) &&
              res$structured$assertions[[i]]$source[[j]]$id %in%
                names(res$structured$sources)) {
            ### If so, merge them and store the result in the root
            res$structured$sources[[res$structured$assertions[[i]]$source[[j]]$id]] <-
              merge_specs(res$structured$sources[[res$structured$assertions[[i]]$source[[j]]$id]],
                          res$structured$assertions[[i]]$source[[j]]);
          } else {
            ### If not, copy this to the root
            res$structured$sources[[res$structured$assertions[[i]]$source[[j]]$id]] <-
              res$structured$assertions[[i]]$source[[j]];
          }
        }
      }
      ### Once they are all copied and/or merged, replace them with references
      if (is.list(res$structured$assertions[[i]]$source)) {
        if (!is.null(names(res$structured$assertions[[i]]$source)) &&
            (length(names(res$structured$assertions[[i]]$source)) == 1) &&
            (names(res$structured$assertions[[i]]$source) == "id")) {
          res$structured$assertions[[i]]$source <-
            structure(res$structured$assertions[[i]]$source$id,
                      class="justifierRef");
        } else {
          res$structured$assertions[[i]]$source <-
            structure(unname(purrr::map_chr(res$structured$assertions[[i]]$source, "id")),
                      class="justifierRef");
        }
      }
    }
  }

  res$supplemented <- res$structured;

  ### Now all elements have been copied to the root. Now, we replace
  ### all references with these completed (merged) elements

  ### First for the sources in the assertions
  for (i in seq_along(res$supplemented$assertions)) {
    if (!is.null(res$supplemented$assertions[[i]]$source)) {
      sourceIds <-
        res$supplemented$assertions[[i]]$source;
      res$supplemented$assertions[[i]]$source <-
        lapply(sourceIds,
               function(j) {
                 return(res$supplemented$sources[[j]]);
               });
      names(res$supplemented$assertions[[i]]$source) <-
        sourceIds;
    }
  }

  ### Then for the assertions in the justifications
  for (i in seq_along(res$supplemented$justifications)) {
    if (!is.null(res$supplemented$justifications[[i]]$assertion)) {
      assertionIds <-
        res$supplemented$justifications[[i]]$assertion;
      res$supplemented$justifications[[i]]$assertion <-
        lapply(assertionIds,
               function(j) {
                 return(res$supplemented$assertions[[j]]);
               });
      names(res$supplemented$justifications[[i]]$assertion) <-
        assertionIds;
    }
  }

  ### Then for the justifications in the justifications
  for (i in seq_along(res$supplemented$justifications)) {
    if (!is.null(res$supplemented$justifications[[i]]$justification)) {
      justificationIds <-
        res$supplemented$justifications[[i]]$justification;
      res$supplemented$justifications[[i]]$justification <-
        lapply(justificationIds,
               function(j) {
                 return(res$supplemented$justifications[[j]]);
               });
      names(res$supplemented$justifications[[i]]$justification) <-
        justificationIds;
    }
  }

  ### Then for the justifications in the decisions
  for (i in seq_along(res$supplemented$decisions)) {
    if (!is.null(res$supplemented$decisions[[i]]$justification)) {
      justificationIds <-
        res$supplemented$decisions[[i]]$justification;
      res$supplemented$decisions[[i]]$justification <-
        lapply(justificationIds,
               function(j) {
                 return(res$supplemented$justifications[[j]]);
               });
      names(res$supplemented$decisions[[i]]$justification) <-
        justificationIds;
    }
  }

  ### Now create one decision tree for each decision
  res$decisionTrees <-
    lapply(res$supplemented$decisions,
           function(d) {
             d$children <- d$justification;
             d$justification <- NULL;
             d$children <-
               lapply(d$children,
                      function(j) {
                        j$children <- j$assertion;
                        j$assertion <- NULL;
                        j$children <-
                          lapply(j$children,
                                 function(a) {
                                   a$children <- a$source;
                                   a$source <- NULL;
                                   return(a);
                                 });
                        return(j);
                      });
             return(d);
           });

  res$decisionTrees <-
    lapply(names(res$decisionTrees),
           function(decisionId) {
             res <-
               data.tree::FromListExplicit(explicitList = res$decisionTrees[[decisionId]],
                                           nameName="id",
                                           childrenName="children",
                                           nodeName=decisionId);
             return(res);
           });

  names(res$decisionTrees) <-
    names(res$supplemented$decisions);

  res$decisionGraphs <-
    lapply(names(res$decisionTrees),
           function(dTreeName) {
             dTree <-
               res$decisionTrees[[dTreeName]];
             tryCatch({
               dTree$Do(function(node) {
                 lbl <-
                   ifelse(is.null(node$label),
                                  node$name,
                                  node$label)
                 lbl <-
                   justifier::sanitize_for_DiagrammeR(lbl);
                 lbl <-
                   paste0(strwrap(lbl, 40), collapse="\n");
                 data.tree::SetNodeStyle(node,
                                         label = lbl);
               });
               dTreeGraph <-
                 data.tree::ToDiagrammeRGraph(dTree);
               dTreeGraph <-
                 justifier::apply_graph_theme(dTreeGraph,
                                              c("layout", "dot", "graph"),
                                              c("rankdir", "LR", "graph"),
                                              c("outputorder", "nodesfirst", "graph"),
                                              c("fixedsize", "false", "node"),
                                              c("shape", "box", "node"),
                                              c("style", "rounded,filled", "node"),
                                              c("color", "#000000", "node"),
                                              c("margin", "0.2,0.6", "node"),
                                              c("color", "#888888", "edge"),
                                              c("dir", "none", "edge"),
                                              c("fillcolor", "#FFFFFF", "node"));
             }, error = function(e) {
               warning("Error issued when converting '",
                       dTreeName, "' decision tree to a decision graph: ",
                       e$message, "\n\nClass and content:\n\n",
                       paste0(utils::capture.output(print(class(dTree))),
                              collapse="\n"),
                       "\n",
                       paste0(utils::capture.output(print(dTree)),
                              collapse="\n"));
             });
             if (is.null(dTreeGraph))
               dTreeGraph <- NA;
             return(dTreeGraph);
           });

  names(res$decisionGraphs) <-
    names(res$supplemented$decisions);

  class(res) <- 'justifications';
  return(res);

}
