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
#' @param justifierFields Which fields to copy from `justifier` metadata to
#' the elements within the specified scope.
#' @param fromFile The file from which the `justifier` specifications were read.
#' @param path The path holding these `justifier` specifications (not necessary
#' if `fromFile` is provided).
#' @param storeDecisionGraphSvg Whether to also produce (and return) the SVG
#' for the decision graph.
#' @param silent Whether to be chatty or quiet.
#' @param ... Additional arguments are passed on to [graphics::plot()] for the
#' `print` method or to [DiagrammeR::render_graph()] for the `plot` method.
#'
#' @return The parsed `justifier` object.
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
#' @rdname parsing_justifications
#' @export
parse_justifications <- function(x,
                                 justifierFields="^date$|^framework$",
                                 fromFile=NULL,
                                 path=NULL,
                                 storeDecisionGraphSvg = FALSE,
                                 silent=TRUE) {

  res <- list(raw = x);

  ### The below commands have now been placed into this function
  # res$structured <-
  #   structure_list(x);

  ### Process all specifications and create five organised lists,
  ### where id's are used as names
  justNames <- tolower(names(x));
  msg("\nStarting first processing sweep to process a series of specifications with names ",
      vecTxtQ(justNames), ".\n", silent = silent);

  res$structured <-
    list(sources = to_specList(x[which(justNames == 'source')],
                               types="sources",
                               type="source",
                               silent=silent),
         assertions = to_specList(x[which(justNames == 'assertion')],
                                  types="assertions",
                                  type="assertion",
                                  silent=silent),
         justifications = to_specList(x[which(justNames == 'justification')],
                                      types="justifications",
                                      type="justification",
                                      silent=silent),
         decisions = to_specList(x[which(justNames == 'decision')],
                                 types="decisions",
                                 type="decision",
                                 silent=silent),
         justifier = to_specList(x[which(justNames == 'justifier')],
                                 types="justifiers",
                                 type="justifier",
                                 idsRequired=FALSE,
                                 silent=silent));

  res$logging <- list(n_firstSweep = unlist(lapply(res$structured,
                                                   length)));

  if (!silent) {
    cat0("\nFinished first processing sweep; extracted ",
         sum(unlist(res$logging$n_firstSweep)), " specifications overall.");
  }

  ### Set fromFile
  if (is.null(fromFile)) {
    fromFile <- "none";
  }
  res$structured <- lapply(res$structured, function(elementList) {
    elementList <-
      lapply(elementList, function(singleElement) {
        singleElement$fromFile <- fromFile;
        return(singleElement);
      });
    return(elementList);
  });

  if (!silent) {
    cat0("\nAdded `fromFile = '", fromFile, "'` to all these specifications.");
  }

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

  if (!silent) {
    cat0("\n\nStarting to process specified decisions to extract justifications.");
  }

  ### Check all decisions for justifications that were specified there,
  ### instead of simply being references that were specified as
  ### 'root' elements
  for (i in seq_along(res$structured$decisions)) {
    if (!silent) {
      cat0("\n\nStarting to process decision ", i, ", with name '",
           names(res$structured$decisions)[i], "' and ",
           ifelse(is.null(res$structured$decisions[[i]]$id),
                  "no id set.",
                  paste0("id '", res$structured$decisions[[i]]$id, "'.")));
    }

    if (!is.null(res$structured$decisions[[i]]$justification)) {
      ### Flatten this justification, in case it is a list with a redundant level
      res$structured$decisions[[i]]$justification <-
        to_specList(res$structured$decisions[[i]]$justification,
                    types="justifications", type="justification",
                    silent=silent);
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
            ### Also add the 'fromFile' fields.
            res$structured$justifications[[res$structured$decisions[[i]]$justification[[j]]$id]]$fromFile <-
              res$structured$decisions[[i]]$fromFile;
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
    } else {
      if (!silent) {
        cat0("\nThis decision does not contain any justifications.");
      }
    }
  }

  if (!silent) {
    cat0("\n\nFinished processing decisions; starting to process justifications to extract assertions and other justifications.");
  }

  ### Repeat the exact same procedure for justifications and assertions.
  ### This could be done more efficiently, but becomes much harder to read and
  ### maintain. So, I'll defer to Hadley Wickham: He who gives up code safety
  ### for code speed deserves neither :-)
  ### https://twitter.com/hadleywickham/status/504368538874703872
  for (i in seq_along(res$structured$justifications)) {
    if (!silent) {
      cat0("\n\nStarting to process justification ", i, ", with name '",
           names(res$structured$justifications)[i], "' and ",
           ifelse(is.null(res$structured$justifications[[i]]$id),
                  "no id set.",
                  paste0("id '", res$structured$justifications[[i]]$id, "'.")));
    }

    if (!is.null(res$structured$justifications[[i]]$assertion)) {
      ### Flatten this justification, in case it is a list with a redundant level
      res$structured$justifications[[i]]$assertion <-
        to_specList(res$structured$justifications[[i]]$assertion,
                    types="assertions", type="assertion",
                    silent=silent);
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
            ### Also add the 'fromFile' fields.
            res$structured$assertions[[res$structured$justifications[[i]]$assertion[[j]]$id]]$fromFile <-
              res$structured$justifications[[i]]$fromFile;
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
    } else {
      if (!silent) {
        cat0("\nThis justification does not contain any assertions.");
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
                    types="justifications", type="justification",
                    silent=silent);
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
            ### Also add the 'fromFile' fields.
            res$structured$justifications[[res$structured$justifications[[i]]$justification[[j]]$id]]$fromFile <-
              res$structured$justifications[[i]]$fromFile;
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
    } else {
      if (!silent) {
        cat0("\nThis justification does not contain any justifications.");
      }
    }

  }

  if (!silent) {
    cat0("\n\nFinished processing justifications; starting to process assertions to extract sources.");
  }

  ### Then again for the assertions and sources.
  for (i in seq_along(res$structured$assertions)) {
    if (!silent) {
      cat0("\n\nStarting to process assertion ", i, ", with name '",
           names(res$structured$assertions)[i], "' and ",
           ifelse(is.null(res$structured$assertions[[i]]$id),
                  "no id set.",
                  paste0("id '", res$structured$assertions[[i]]$id, "'.")));
    }

    if (!is.null(res$structured$assertions[[i]]$source)) {
      ### Flatten this justification, in case it is a list with a redundant level
      res$structured$assertions[[i]]$source <-
        to_specList(res$structured$assertions[[i]]$source,
                    types="sources", type="source",
                    silent=silent);
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
            ### Also add the 'fromFile' fields.
            res$structured$sources[[res$structured$assertions[[i]]$source[[j]]$id]]$fromFile <-
              res$structured$assertions[[i]]$fromFile;
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
    } else {
      if (!silent) {
        cat0("\nThis assertions does not contain any sources.");
      }
    }
  }

  if (!silent) {
    cat0("\n\nFinished processing assertions. All specifications are now extracted.\n\n");
  }

  ###---------------------------------------------------------------------------
  ### Add directory to each element based on fromFile
  ###---------------------------------------------------------------------------

  for (i in names(res$structured)) {
    if (length(res$structured[[i]]) > 0) {
      for (j in seq_along(res$structured[[i]])) {
        if (!is.null(res$structured[[i]][[j]]$fromFile)) {
          res$structured[[i]][[j]]$fromDir <-
            dirname(res$structured[[i]][[j]]$fromFile);
        } else {
          stop("'fromFile' is not set for '",
               i, "' element with identifier ',
               j, '! Only the fields ",
               vecTxtQ(names(res$structured[[i]][[j]])),
               " are available!");
        }
      }
    }
  }

  ###---------------------------------------------------------------------------
  ### Extract any references to framework specifications
  ###---------------------------------------------------------------------------

  res$frameworks <- list(specs = list(),
                         loaded = list());

  if (length(res$structured$justifier) > 0) {
    for (currentJustifierSpec in res$structured$justifier) {
      if (!is.null(currentJustifierSpec$framework)) {
        res$frameworks$specs <- c(res$frameworks$specs,
                                  list(c(framework = currentJustifierSpec$framework,
                                         scope = currentJustifierSpec$scope,
                                         fromFile = currentJustifierSpec$fromFile)));
      }
    }
  }

  frameworksToLoad <-
    purrr::map_chr(res$frameworks$specs, 'framework');

  if (is.null(path) && dir.exists(dirname(fromFile))) {
    path <- dirname(fromFile);
  }

  if (!dir.exists(path)) {
    stop("Path specified in 'path' ('", path, "') does not exist!");
  }

  for (i in frameworksToLoad) {
    if (!is.null(path)) {
      fileToLoad <-
        file.path(path, i);
    } else {
      fileToLoad <- i;
    }
    if (file.exists(fileToLoad)) {
      res$frameworks$loaded <- c(res$frameworks$loaded,
                                 yum::load_yaml_fragments(fileToLoad));
      names(res$frameworks$loaded)[length(res$frameworks$loaded)] <-
        i;
    }
  }

  ###---------------------------------------------------------------------------
  ### Extract any direct framework specifications
  ###---------------------------------------------------------------------------

  if (length(res$structured$justifier) > 0) {
    for (currentJustifierSpec in res$structured$justifier) {
      if (!is.null(currentJustifierSpec$id)) {
        res$frameworks$loaded <- c(res$frameworks$loaded,
                                   list(list(justifier = currentJustifierSpec)));
      }
    }
  }

  ###---------------------------------------------------------------------------
  ### Parse framework specifications
  ###---------------------------------------------------------------------------

  if (length(res$frameworks$loaded) > 0) {
    if (!silent) {
      cat0("\nStarting to process frameworks.\n\n");
    }
    res$frameworks$parsed <-
      lapply(res$frameworks$loaded,
             parse_framework);
    frameworkIds <-
      unlist(lapply(res$frameworks$parsed,
                    function(x) {
                      return(x$id);
                    }));
    names(res$frameworks$parsed) <-
      ifelse(nchar(frameworkIds) > 0,
             frameworkIds,
             names(res$frameworks$loaded));

  } else {
    if (!silent) {
      cat0("\nNo frameworks specified.\n\n");
    }
  }

  ###---------------------------------------------------------------------------
  ### Copy justifier metadata to every element
  ###---------------------------------------------------------------------------

  if (length(res$structured$justifier) > 0) {

    # ### Get scopes
    # justifierScopes <-
    #   purrr::map_chr(res$structured$justifier, 'scope');
    #
    # ### Set to local if not set
    # justifierScopes <-
    #   ifelse(unlist(lapply(justifierScopes, is.null)),
    #          "local",
    #          justifierScopes);

    ### Corrected the above old code; couldn't deal with non-specified scopes
    justifierScopes <-
      unlist(lapply(res$structured$justifier,
                     function(x) {
                       if ("scope" %in% names(x)) {
                         return(x$scope);
                       } else {
                         return("local");
                       }
                     }));

    ### Convert all to lower case - one never knows
    justifierScopes <- tolower(justifierScopes);

    ### First process all universal specs - apply to all other elements
    for (currentJustifierSpec in res$structured$justifier[justifierScopes=="universal"]) {
      objectWithFieldsToCopy <-
        currentJustifierSpec[setdiff(names(currentJustifierSpec), 'scope')];
      ### Merge with sources
      res$structured$sources <- lapply(res$structured$sources,
                                       merge_specs,
                                       y=objectWithFieldsToCopy,
                                       ySpecs=justifierFields);
      ### Merge with assertions
      res$structured$assertions <- lapply(res$structured$assertions,
                                          merge_specs,
                                          y=objectWithFieldsToCopy,
                                          ySpecs=justifierFields);
      ### Merge with justifications
      res$structured$justifications <- lapply(res$structured$justifications,
                                              merge_specs,
                                              y=objectWithFieldsToCopy,
                                              ySpecs=justifierFields);
      ### Merge with decisions
      res$structured$decisions <- lapply(res$structured$decisions,
                                         merge_specs,
                                         y=objectWithFieldsToCopy,
                                         ySpecs=justifierFields);
    }

    ### Then process all global specs - apply to all other elements in the same directory
    for (currentJustifierSpec in res$structured$justifier[justifierScopes=="global"]) {
      objectWithFieldsToCopy <-
        currentJustifierSpec[setdiff(names(currentJustifierSpec), 'scope')];
      if (length(res$structured$sources) > 0) {
        sourcesToProcess <- grepl(currentJustifierSpec$fromDir,
                                  purrr::map_chr(res$structured$sources, 'fromDir'),
                                  fixed=TRUE);
      } else {
        sourcesToProcess <- FALSE;
      }
      if (length(res$structured$assertions) > 0) {
        assertionsToProcess <- grepl(currentJustifierSpec$fromDir,
                                     purrr::map_chr(res$structured$assertions, 'fromDir'),
                                     fixed=TRUE);
      } else {
        assertionsToProcess <- FALSE;
      }
      justificationsToProcess <- grepl(currentJustifierSpec$fromDir,
                                       purrr::map_chr(res$structured$justifications, 'fromDir'),
                                       fixed=TRUE);
      decisionsToProcess <- grepl(currentJustifierSpec$fromDir,
                                  purrr::map_chr(res$structured$decisions, 'fromDir'),
                                  fixed=TRUE);
      ### Merge with sources
      res$structured$sources[sourcesToProcess] <-
          lapply(res$structured$sources[sourcesToProcess],
                 merge_specs,
                 y=objectWithFieldsToCopy,
                 ySpecs=justifierFields);
      ### Merge with assertions
      res$structured$assertions[assertionsToProcess] <-
        lapply(res$structured$assertions[assertionsToProcess],
               merge_specs,
               y=objectWithFieldsToCopy,
               ySpecs=justifierFields);
      ### Merge with justifications
      res$structured$justifications[justificationsToProcess] <-
        lapply(res$structured$justifications[justificationsToProcess],
               merge_specs,
               y=objectWithFieldsToCopy,
               ySpecs=justifierFields);
      ### Merge with decisions
      res$structured$decisions[decisionsToProcess] <-
        lapply(res$structured$decisions[decisionsToProcess],
               merge_specs,
               y=objectWithFieldsToCopy,
               ySpecs=justifierFields);
    }

    ### Then process all local specs - apply to all other elements in the same file
    for (currentJustifierSpec in res$structured$justifier[justifierScopes=="local"]) {
      objectWithFieldsToCopy <-
        currentJustifierSpec[setdiff(names(currentJustifierSpec), 'scope')];
      sourcesToProcess <- purrr::map_chr(res$structured$sources, 'fromFile') == currentJustifierSpec$fromFile;
      assertionsToProcess <- purrr::map_chr(res$structured$assertions, 'fromFile') == currentJustifierSpec$fromFile;
      justificationsToProcess <- purrr::map_chr(res$structured$justifications, 'fromFile') == currentJustifierSpec$fromFile;
      decisionsToProcess <- purrr::map_chr(res$structured$decisions, 'fromFile') == currentJustifierSpec$fromFile;

      ### Merge with sources
      res$structured$sources[sourcesToProcess] <-
        lapply(res$structured$sources[sourcesToProcess],
               merge_specs,
               y=objectWithFieldsToCopy,
               ySpecs=justifierFields);
      ### Merge with assertions
      res$structured$assertions[assertionsToProcess] <-
        lapply(res$structured$assertions[assertionsToProcess],
               merge_specs,
               y=objectWithFieldsToCopy,
               ySpecs=justifierFields);
      ### Merge with justifications
      res$structured$justifications[justificationsToProcess] <-
        lapply(res$structured$justifications[justificationsToProcess],
               merge_specs,
               y=objectWithFieldsToCopy,
               ySpecs=justifierFields);
      ### Merge with decisions
      res$structured$decisions[decisionsToProcess] <-
        lapply(res$structured$decisions[decisionsToProcess],
               merge_specs,
               y=objectWithFieldsToCopy,
               ySpecs=justifierFields);
    }

  }


  ###---------------------------------------------------------------------------
  ### Process framework specifications
  ###---------------------------------------------------------------------------

  res$fwApplications <- list();

  for (currentFramework in names(res$frameworks$parsed)) {

    res$fwApplications[[currentFramework]] <- list(verifications = data.frame(),
                                                   fieldScoring = data.frame());

    if (!silent) {
      cat0("\n\n\nStarting to evaluate framework specifications for framework '",
           res$frameworks$parsed[[currentFramework]]$label, "' (id: '",
           res$frameworks$parsed[[currentFramework]]$id, "').\n");
    }

    for (currentElement in names(res$frameworks$parsed[[currentFramework]]$conditions)) {

      if (!silent) {
        cat0("\n\nStarting to process conditions for '", currentElement, "' elements.");
      }

      for (currentType in names(res$frameworks$parsed[[currentFramework]]$conditions[[currentElement]])) {

        if (!silent) {
          cat0("\n  Starting to process condition for elements of type '", currentType, "'.");
        }

        for (currentCondition in names(res$frameworks$parsed[[currentFramework]]$conditions[[currentElement]][[currentType]])) {

          if (!silent) {
            cat0("\n    Starting to process condition '", currentCondition, "'.");
          }

          THIS_CONDITION <-
            res$frameworks$parsed[[currentFramework]]$conditions[[currentElement]][[currentType]][[currentCondition]];

          specsToProcessLogical <-
            unlist(lapply(res$structured[[paste0(currentElement, "s")]],
                          function(spec) {
                            if (currentType == ".*") {
                              return(TRUE);
                            } else if (is.null(spec$type)) {
                              return(FALSE);
                            } else {
                              return(grepl(currentType,
                                           spec$type));
                            }
                          }));

          specsToProcess <- which(specsToProcessLogical);

          if (length(specsToProcess) > 0) {
            if (!silent) {
              cat0("\n      Found ", length(specsToProcess), " '", currentElement, "' elements with type '",
                   currentType, "' (at indices ",
                   vecTxtQ(specsToProcess), ").");
            }

            for (i in specsToProcess) {

              THIS_SPECIFICATION <- res$structured[[paste0(currentElement, "s")]][[i]];
              if (!silent) {
                cat0("\n      Processing specification '", THIS_SPECIFICATION$id, "'.");
              }

              ###---------------------------------------------------------------
              ### Set scores based on specified fields
              ###---------------------------------------------------------------

              if (!is.null(THIS_CONDITION$field) &&
                  !is.null(THIS_CONDITION$scores) &&
                  !is.null(THIS_CONDITION$values)) {

                if (!is.null(THIS_SPECIFICATION[[THIS_CONDITION$field]])) {
                  if (tolower(THIS_SPECIFICATION[[THIS_CONDITION$field]]) %in% tolower(THIS_CONDITION$values)) {
                    scoreIndex <-
                      which(tolower(THIS_CONDITION$values) == tolower(THIS_SPECIFICATION[[THIS_CONDITION$field]]));
                    scoreToSet <-
                      THIS_CONDITION$scores[scoreIndex];
                  } else {
                    scoreToSet <- NA;
                  }
                  if (is.null(res$structured[[paste0(currentElement, "s")]][[i]]$scores)) {
                    res$structured[[paste0(currentElement, "s")]][[i]]$scores <- list();
                  }
                  res$structured[[paste0(currentElement, "s")]][[i]]$scores[[THIS_CONDITION$field]] <-
                    scoreToSet;
                  res$fwApplications[[currentFramework]]$fieldScoring <-
                    rbind(res$fwApplications[[currentFramework]]$fieldScoring,
                          data.frame(element = currentElement,
                                     id = THIS_SPECIFICATION$id,
                                     type = currentType,
                                     condition = currentCondition,
                                     field = THIS_CONDITION$field,
                                     value = THIS_SPECIFICATION[[THIS_CONDITION$field]],
                                     score = scoreToSet,
                                     stringsAsFactors = FALSE));
                }
              }

              ###---------------------------------------------------------------
              ### Run verifications
              ###---------------------------------------------------------------

              if (!is.null(THIS_CONDITION$verifications)) {

                for (currentVerificationIndex in seq_along(THIS_CONDITION$verifications)) {

                  if (!silent) {
                    cat0("\n        Testing verification: ",
                         THIS_CONDITION$verifications[currentVerificationIndex]);
                  }

                  tmpRes <- tryCatch({
                    if (eval(parse(text=THIS_CONDITION$verifications[currentVerificationIndex]))) {
                      tmpRes <- "OK";
                    } else {
                      tmpRes <- "FAILED!";
                    }
                  }, error = function(e) {
                    return(paste0("FAILED with error: ", e$message));
                  }, warning = function (w) {
                    return(paste0("FAILED with warning: ", w$message));
                  });

                  if (tmpRes == "FAILED!") {

                    if (!is.null(THIS_CONDITION$verificationMsgs) &&
                        (currentVerificationIndex <= length(currentVerificationIndex))) {
                      tmpRes <- THIS_CONDITION$verificationMsgs[currentVerificationIndex];
                    }
                  }

                  ### Replace placeholders; first find them
                  placeHolderMatches <-
                    gregexpr("\\[\\[([^][]+)\\]\\]", tmpRes);

                  if (!(unlist((placeHolderMatches)) == -1)) {
                    placeHolderContents <-
                      unlist(regmatches(tmpRes, placeHolderMatches));
                    placeHolderObjectNames <-
                      gsub("\\[\\[([^][]+)\\]\\]", "\\1", placeHolderContents);
                    retrievedObjects <-
                      unlist(lapply(placeHolderObjectNames,
                                    function(x) {
                                      res <- eval(parse(text=x));
                                      if (is.null(res) || is.na(res)) {
                                        return("Unspecified");
                                      } else {
                                        return(vecTxtQ(res));
                                      }
                                    }));
                    regmatches(tmpRes, placeHolderMatches) <- retrievedObjects;
                  }

                  res$fwApplications[[currentFramework]]$verifications <-
                    rbind(res$fwApplications[[currentFramework]]$verifications,
                          data.frame(element = currentElement,
                                     id = THIS_SPECIFICATION$id,
                                     type = currentType,
                                     condition = currentCondition,
                                     verification = THIS_CONDITION$verifications[currentVerificationIndex],
                                     result = tmpRes,
                                     stringsAsFactors = FALSE));

                  if (!silent) {
                    cat0("\n        Verification result: ",
                         utils::tail(res$fwApplications[[currentFramework]]$verifications$result, 1));
                  }

                }
              }

              ### ----------------------------------------------------
            } ### Done with processing this condition for this element


          } else {
            if (!silent) {
              cat0("\n  Found no '", currentElement, "' elements with type '",
                   currentCondition, "'.");
            }
          }

        } ### Done with this condition
      } ### Done with this type
    } ### Done with this element
  }

  ###---------------------------------------------------------------------------
  ### Add framework information to the elements
  ###---------------------------------------------------------------------------

  ### Add the sequences from the framework




  ###---------------------------------------------------------------------------
  ### Replace all references with the objects themselves.
  ###---------------------------------------------------------------------------

  ### Now all elements have been copied to the root. Now, we replace
  ### all references with these completed (merged) elements

  res$supplemented <- res$structured;

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

  ###---------------------------------------------------------------------------
  ### Decision tree creation
  ###---------------------------------------------------------------------------

  if (!silent) {
    cat0("\n\nStarting decision tree creation.\n");
  }

  ### Now create one decision tree for each decision
  # res$decisionTrees <-
  #   lapply(res$supplemented$decisions,
  #          function(d) {
  #            d$children <- d$justification;
  #            d$justification <- NULL;
  #            d$children <-
  #              lapply(d$children,
  #                     function(j) {
  #                       j$children <- j$assertion;
  #                       j$assertion <- NULL;
  #                       j$children <-
  #                         lapply(j$children,
  #                                function(a) {
  #                                  a$children <- a$source;
  #                                  a$source <- NULL;
  #                                  return(a);
  #                                });
  #                       return(j);
  #                     });
  #            return(d);
  #          });
  #
  # res$decisionTrees <-
  #   lapply(names(res$decisionTrees),
  #          function(decisionId) {
  #            res <-
  #              data.tree::FromListExplicit(explicitList = res$decisionTrees[[decisionId]],
  #                                          nameName="id",
  #                                          childrenName="children",
  #                                          nodeName=decisionId);
  #            return(res);
  #          });

  res$decisionTrees <-
    lapply(
      res$supplemented$decisions,
      create_justifierTree,
      silent = silent
    );

  ### This may not be a good idea
  res$decisionTrees <- lapply(
    seq_along(res$decisionTrees),
    function(x) {
      if (length(x) == 1) {
        return(res$decisionTrees[[x]][[1]]);
      } else {
        stop("I just created justifier trees, but one of the results had ",
             "a length of more than one element!");
      }
    }
  );

  names(res$decisionTrees) <-
    names(res$supplemented$decisions);

  if (!silent) {
    cat0("Created decision trees.\n");
  }

  ###---------------------------------------------------------------------------
  ### Aggregate scores at decision level
  ###---------------------------------------------------------------------------

  if (!silent) {
    cat0("\nAggregating scores for each decision.\n");
  }

  ### Process separately for every framework
  for (currentFramework in names(res$frameworks$parsed)) {

    if (!silent) {
      cat0("\n  Processing framework '", currentFramework, "'.");
    }

    ### Identify the fields with scores in this framework
    fieldScoreTypes <-
      sort(unique(res$fwApplications[[currentFramework]]$fieldScoring$field));

    ### Process every decision separately
    for (currentDecision in names(res$decisionTrees)) {

      if (!silent) {
        cat0("\n    Processing decision '", currentDecision, "'.");
      }

      if (is.null(res$decisionTrees[[currentDecision]]$scores)) {
        res$decisionTrees[[currentDecision]]$scores <- list();
      }

      ### Collect scores per field
      for (currentField in fieldScoreTypes) {
        res$decisionTrees[[currentDecision]]$scores[[currentField]] <- list();

        if (!silent) {
          cat0("\n      Processing field '", currentField, "'.");
        }

        ### Collect
        res$decisionTrees[[currentDecision]]$scores[[currentField]]$vector <-
          unlist(res$decisionTrees[[currentDecision]]$Get(function(node) {
            return(node$scores[[currentField]]);
          },
          traversal = "post-order",
          filterFun = function(node) {
            return(!(is.null(node$scores) || is.null(node$scores[[currentField]])));
        }));

        res$decisionTrees[[currentDecision]]$scores[[currentField]]$number <-
          length(res$decisionTrees[[currentDecision]]$scores[[currentField]]$vector);

        if (!silent) {
          cat0("\n        Extracted ",
               res$decisionTrees[[currentDecision]]$scores[[currentField]]$number,
               " scores.");
        }

        if (res$decisionTrees[[currentDecision]]$scores[[currentField]]$number > 0) {

          res$decisionTrees[[currentDecision]]$scores[[currentField]]$sum <-
            sum(res$decisionTrees[[currentDecision]]$scores[[currentField]]$vector);

          res$decisionTrees[[currentDecision]]$scores[[currentField]]$mean <-
            res$decisionTrees[[currentDecision]]$scores[[currentField]]$sum /
            res$decisionTrees[[currentDecision]]$scores[[currentField]]$number;

        } else {
          res$decisionTrees[[currentDecision]]$scores[[currentField]]$sum <- NA;
          res$decisionTrees[[currentDecision]]$scores[[currentField]]$mean <- NA;
        }
      }
    }
  }

  ###---------------------------------------------------------------------------
  ### Add framework with decision scores
  ###---------------------------------------------------------------------------

  if (!silent) {
    cat0("\nAdding framework with decision scores.\n");
  }


  ### Process separately for every framework
  for (currentFramework in names(res$frameworks$parsed)) {

    res$fwApplications[[currentFramework]]$scoreDf <- data.frame();

    ### Identify the fields with scores in this framework
    fieldScoreTypes <-
      sort(unique(res$fwApplications[[currentFramework]]$fieldScoring$field));

    ### Process every decision separately
    for (currentDecision in names(res$decisionTrees)) {

      ### Collect scores per field
      for (currentField in fieldScoreTypes) {

        res$fwApplications[[currentFramework]]$scoreDf <-
          rbind(res$fwApplications[[currentFramework]]$scoreDf,
                data.frame(decision = currentDecision,
                           field = currentField,
                           score = res$decisionTrees[[currentDecision]]$scores[[currentField]]$mean));
      }
    }
  }

  ###---------------------------------------------------------------------------
  ### Build decision graphs
  ###---------------------------------------------------------------------------

  if (!silent) {
    cat0("\n\nStarting decision graph creation.");
  }

  res$decisionGraphs <-
    lapply(
      names(res$decisionTrees),
      function(dTreeName) {
        return(create_justifierGraph(res$decisionTrees[[dTreeName]]));
      }
    );

  names(res$decisionGraphs) <-
    names(res$decisionTrees);

  if (!silent) {
    cat0("\nCreated decision graphs.");
  }

  if (storeDecisionGraphSvg) {

    res$decisionGraphsSvg <-
      lapply(res$decisionGraphs,
             function(graph) {
               dot_code <- DiagrammeR::generate_dot(graph);
               graphSvg <-
                 DiagrammeRsvg::export_svg(DiagrammeR::grViz(dot_code));
               graphSvg <-
                 sub(".*\n<svg ", "<svg ", graphSvg);
               graphSvg <- gsub('<svg width=\"[0-9]+pt\" height=\"[0-9]+pt\"\n viewBox=',
                                '<svg viewBox=',
                                graphSvg);
               return(graphSvg);
             })
    names(res$decisionGraphsSvg) <-
      names(res$supplemented$decisions);

    if (!silent) {
      cat0("\nBecause `storeDecisionGraphSvg` was set to `TRUE`, ",
           "I stored the decision graphs as svg.");
    }

  } else {
    if (!silent) {
      cat0("\nBecause `storeDecisionGraphSvg` was set to `FALSE`, ",
           "I did not store the decision graphs as svg.");
    }
  }

  ###---------------------------------------------------------------------------
  ### Show verification results
  ###---------------------------------------------------------------------------

  for (currentFramework in names(res$frameworks$parsed)) {
    if (all(res$fwApplications[[currentFramework]]$verifications$result == "OK")) {
      cat0("\n\nAll specifications of decisions, justifications, assertions, and sources ",
           "successfully passed all verifications for framework '",
           res$fwApplications[[currentFramework]]$label, "'.");
    } else {
      cat0("\n\nNot all specifications of decisions, justifications, assertions, and sources ",
           "successfully passed all verifications for framework '",
           res$fwApplications[[currentFramework]]$label, "':\n");
      for (i in which(res$fwApplications[[currentFramework]]$verifications$result != "OK")) {
        cat0("\n- In the ",
             res$fwApplications[[currentFramework]]$verifications[i, c('element')],
             " with identifier `",
             res$fwApplications[[currentFramework]]$verifications[i, c('id')],
             "`, when applying a verification in framework condition `",
             res$fwApplications[[currentFramework]]$verifications[i, c('condition')],
             "`, it failed with message: \"",
             res$fwApplications[[currentFramework]]$verifications[i, c('result')],
             "\".\n");
      }
    }
  }

  class(res) <- 'justifications';
  return(res);

}

#' @method print justifierDecisionGraph
#' @export
#' @rdname parsing_justifications
print.justifierDecisionGraph <- function(x, ...) {
  print(graphics::plot(x, ...));
}

#' @method plot justifierDecisionGraph
#' @export
#' @rdname parsing_justifications
plot.justifierDecisionGraph <- function(x, ...) {
  return(DiagrammeR::render_graph(x, ...));
}
