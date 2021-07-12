#' Producing a list of specifications
#'
#' This function is for internal use, but has been exported in
#' case it's useful for people working 'manually' with
#' lists of justifications.
#'
#' @param x The list to parse.
#' @param types The class to assign to the specification
#' list (the `justifierSpecList` object to return).
#' @param type The class to assign to each specification
#' (in addition to `justifierSpec`).
#' @param idsRequired Whether to require identifiers.
#' @param silent Whether to be chatty or silent.
#'
#' @return A list of classes `c("justifierSpecList", types)` where
#' each element is a specification of class
#' `c("justifierSpec", type)`.
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
#' ### Show classes
#' class(loadedMinutes["assertion"]);
#'
#' ### Convert to specification list
#' res <- to_specList(loadedMinutes["assertion"],
#'                    type="assertion",
#'                    types="assertions");
#' ### Show classes
#' class(res);
#'
#' ### Show original and parsed objects
#' loadedMinutes["assertion"];
#' res;
#'
#' @export
to_specList <- function(x,
                        types,
                        type,
                        idsRequired=TRUE,
                        silent=TRUE) {
  if ("justifierSpecList" %in% class(x)) {
    if (!silent) {
      cat0("\nIn converting to a specification list, I found that the class of provided object is `justifierSpecList`: immediately returning it as is.");
    }
    return(x);
  } else if ("justifierSpec" %in% class(x)) {
    if (!silent) {
      cat0("\nIn converting to a specification list, I found that the class of provided object is `justifierSpec`: wrapping it in a `justifierSpecList` and immediately returning it.");
    }
    return(structure(list(x),
                     names=x$id,
                     class=c("justifierSpecList", types)));
  } else if (!is.list(x) || (length(x) == 0)) {
    ### No justifier element, just return it
    if (!silent) {
      cat0("\nIn converting to a specification list, I found that the provided object is not a list or has length 0. Immediately returning it as is.");
    }
    return(x);
    ### That also means that after this point we only have lists with nonzero lengths
  }

  xRefs <-
    unlist(lapply(x, is_ref));
  xSpecs <-
    unlist(lapply(x, is_spec));
  xNesteds <-
    unlist(lapply(x, is_nested));

  if (!silent) {
    cat0("\nProcessing a specification of type '",
         type, "' (list type '", types, "') where identifiers ",
         ifelse(idsRequired, "are", "aren't"), " required.");
    cat0("\nThis specification contains ", length(x),
         ifelse(is.null(names(x)), " unnamed elements.",
                paste0(" elements with names ", vecTxtQ(names(x)), ".")));
    cat0(ifelse(all(xRefs),
                "\nAll of these elements are references.",
                ifelse(all(!xRefs),
                       "\nNone of these elements are references.",
                       paste0("Elements ", vecTxt(which(xRefs)), " are references."))));
    cat0(ifelse(all(xSpecs),
                "\nAll of these elements are specifications.",
                ifelse(all(!xSpecs),
                       "\nNone of these elements are specifications.",
                       paste0("Elements ", vecTxt(which(xSpecs)), " are specifications."))));
    cat0(ifelse(all(xNesteds),
                "\nAll of these elements are recursive (i.e. contain other specifications or objects).",
                ifelse(all(!xNesteds),
                       "\nNone of these elements are recursive (i.e. contain other specifications or objects).",
                       paste0("Elements ", vecTxt(which(xNesteds)), " are recursive (i.e. contain other specifications or objects)."))));
  }

  ### First process any child specifications there may be
  # if (has_child_specs(x)) {
  #   childSpecIndices <- which_child_specs(x);
  #   childSpecType <- names(x)[childSpecIndices];
  #   childSpecTypes <- paste0(childSpecType, 's');
  #   x[which_child_specs(x)] <-
  #     mapply(to_specList,
  #            x[childSpecIndices],
  #            type = childSpecType,
  #            types = childSpecTypes,
  #            SIMPLIFY=FALSE);
  # }

  if (is.null(names(x)) || ((length(unique(names(x))) == 1) && (all(unique(names(x)) == type)))) {
    ### In this case, one or more elements were specified. The absence of names
    ### suggests that these are specifications, not references. Because the
    ### `load_and_simplify` function in `yum` provides names as well (but
    ### always uses the name of the element), by checking whether all elements
    ### have the name of the `type` argument, we can catch that case.

    if (all(xSpecs)) {
      ### They're all specifications; check whether they all have an id
      ids <-
        purrr::map_chr(x, function(element) {
          if (is.null(element$id)) {
            return("");
          } else {
            return(element$id);
          }
        });
      if (all(nchar(ids) > 0)) {
        if (!silent) {
          cat0("\nAll elements are specifications, and they all have a valid id (",
               vecTxtQ(ids), "). Returning it as is, with class `justifierSpec` and within a list with class `justifierSpecList`.\n");
        }
        return(structure(lapply(x,
                                function(spec) {
                                  return(structure(spec,
                                                   class=c("justifierSpec", type)));
                                }),
                         names=ids,
                         class=c("justifierSpecList", types)));
      } else {
        if (!silent) {
          cat0("\nAll elements are specifications, but they don't all have valid identifiers (",
               vecTxtQ(ids), "). Adding identifiers if needed, then returning it with class `justifierSpec` and within a list with class `justifierSpecList`.\n");
        }
        if (idsRequired) {
          emptyIds <- which(nchar(ids)==0);
          ids[emptyIds] <-
            paste0("id_", seq(1, sum(nchar(ids)==0)));
          warning("Some elements did not have identifiers set! I set those ",
                  "identifiers to ", vecTxtQ(ids[emptyIds]), ".");
        }
        return(structure(lapply(seq_along(x),
                                function(specIndex) {
                                  x[[specIndex]]$id <- unname(ids[specIndex]);
                                  return(structure(x[[specIndex]],
                                                   class=c("justifierSpec", type)));
                                }),
                         names=ids,
                         class=c("justifierSpecList", types)));
      }
    } else if (all(xRefs)) {
      ### One or more elements are references. We have to figure out which ones
      ### are the references, convert those to specifications, and return
      ### the result.
      message("This functionality has not been developed yet! (All references)");
      cat0("Length: ", length(x));
      cat0("\nNames: ", vecTxtQ(names(x)));
      #cat0("\nObjects: ");
      #print(x);
      cat0("\n--- Terminating processing this element.\n");
    } else if (all(xNesteds)) {
      message("This functionality has not been developed yet! (All nested)");
      cat0("Length: ", length(x));
      cat0("\nNames: ", vecTxtQ(names(x)));
      #cat0("\nObjects: ");
      #print(x);
      cat0("\n--- Terminating processing this element.\n");
    } else {
      ### This is an odd object; throw an error.
      stop("You provided an object I cannot parse, sorry! It looks like:\n\n",
           paste0(utils::capture.output(print(x)), collapse="\n"));
    }
  } else {
    ### Names are set, so this is a specification of a single element; or it
    ### is a list of specifications that has already been processed. In the latter
    ### case, the class 'justifierSpec' has been set.
    if (all(unlist(lapply(x, is_spec)))) {
      if (!silent) {
        cat0("\nBecause all elements are specifications, simply returning them within a list with class `justifierSpecList`.\n");
      }
      ### All specifications; return without doing anything
      return(structure(x,
                       class=c("justifierSpecList", types)));
    } else {
      ### So they're not all specifications; this should be a single specification
      ### then. However, if only 'id' is set, that can be shorthand. So first check
      ### whether 'id' is the only name; if so, 'unvectorize' if needed, structure
      ### as specification(s), and return.
      if ((length(unique(names(x)))==1) && (unique(names(x)) == "id")) {
        if (!silent) {
          cat0("\nThis specification contains multiple named fields; returning it as is, with class `justifierSpec`, within a list with class `justifierSpecList`.\n");
        }
        ids <- x$id;
        return(structure(lapply(ids,
                                function(id) {
                                  return(structure(list(id = id),
                                                   class=c("justifierSpec", type)));
                                }),
                         names=ids,
                         class=c("justifierSpecList", types)));
      } else {
        ### More than one name, so it should be a single specification.
        ### Structure it as specification and return it.
        if (!silent) {
          cat0("\nThis specification contains multiple named filds; returning it as is, with class `justifierSpec`, within a list with class `justifierSpecList`.\n");
        }
        return(structure(list(structure(x,
                                        class=c("justifierSpec", type))),
                         names=x$id,
                         class=c("justifierSpecList", types)));
      }
    }
  }




  # if (!is.null(names(x)) && length(names(x)==1) && (names(x) == "id")) {
  #   ### This is only a reference to one or more elements
  #   if (length(x$id) == 1) {
  #     ### Single reference
  #
  #   }
  #
  #
  # if (is.null(x$id)) {
  #     return(x);
  #   } else {
  #     return(structure(list(x),
  #                      names=x$id));
  #   }
  # } else if (is.null(names(x))) {
  #   ### Check whether all list elements have an identifier set
  #   elementIds <-
  #     unlist(lapply(x,
  #                   function(subX) ifelse(is.null(subX$id), "", subX$id)));
  #   names(x) <- elementIds;
  #   return(x);



  #   print("=================");
  #   print(elementIds);
  #   print(x);
  #   print("-----------------");
  #   if (all(nchar(elementIds) > 0)) {
  #     res <- x;
  #     # elementIds <-
  #     #   purrr::map_chr(unlist(x, recursive=FALSE),
  #     #                  "id");
  #     # print(elementIds);
  #     # if (length(x) > 1) {
  #     #   res <- structure(x,
  #     #                    names = elementIds);
  #     # } else {
  #     #   res <- structure(unlist(x, recursive=FALSE),
  #     #                    names = elementIds);
  #     # }
  #     # res <- unlist(x, recursive=FALSE);
  #     # if ((length(x) == 1) && (length(res) != 1)) {
  #     #   res <- list(res);
  #     # }
  #     # print("=================");
  #     # print(x);
  #     # print("-----------------");
  #     # print(res);
  #     # print(elementIds);
  #     # res <- structure(res,
  #     #                  names = elementIds);
  #     return(res);
  #   } else {
  #     warning("While trying to clean up the provided justifier specifications, ",
  #             "I encountered a list where the mandatory identifier ('id') seems ",
  #             "to have been omitted from one or more specifications. I'm trying to ",
  #             "flatten this object:\n\n",
  #             paste0(capture.output(str(x)), collapse="\n"), "\n");
  #     return(x);
  #   }
  # } else {
  #   warning("Provided element has an unknown structure!");
  #   return(x);
  # }
}


