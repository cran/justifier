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
                        type) {
  if ("justifierSpecList" %in% class(x)) {
    return(x);
  } else if ("justifierSpec" %in% class(x)) {
    return(structure(list(x),
                     names=x$id,
                     class=c("justifierSpecList", types)));
  } else if (!is.list(x) || (length(x) == 0)) {
    ### No justifier element, just return it
    return(x);
    ### That also means that after this point we only have lists with nonzero lengths
  } else if (is.null(names(x)) ||
             (length(unique(names(x))) == 1) && (unique(names(x)) == type)) {
    ### In this case, one or more elements were specified. The absence of names
    ### suggests that these are specifications, not references. Because the
    ### `load_and_simplify` function in `yum` provides names as well (but
    ### always uses the name of the element), by checking whether all elements
    ### have the name of the `type` argument, we can catch that case.
    if (all(unlist(lapply(x, is_spec)))) {
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
        return(structure(lapply(x,
                                function(spec) {
                                  return(structure(spec,
                                                   class=c("justifierSpec", type)));
                                }),
                         names=ids,
                         class=c("justifierSpecList", types)));
      } else {
        emptyIds <- which(nchar(ids)==0);
        ids[emptyIds] <-
          paste0("id_", seq(1, sum(nchar(ids)==0)));
        warning("Some elements did not have identifiers set! I set those ",
                "identifiers to ", ufs::vecTxtQ(ids[emptyIds]), ".");
        return(structure(lapply(seq_along(x),
                                function(specIndex) {
                                  x[[specIndex]]$id <- unname(ids[specIndex]);
                                  return(structure(x[[specIndex]],
                                                   class=c("justifierSpec", type)));
                                }),
                         names=ids,
                         class=c("justifierSpecList", types)));
      }
    } else if (all(unlist(lapply(x, is_ref)))) {
      ### One or more elements are references. We have to figure out which ones
      ### are the references, convert those to specifications, and return
      ### the result.
    } else {
      ### This is an odd object; throw an error.
      stop("You provided an object I cannot parse, sorry!");
    }
  } else {
    ### Names are set, so this is a specification of a single element; or it
    ### is a list of specifications that has already been processed. In the latter
    ### case, the class 'justifierSpec' has been set.
    if (all(unlist(lapply(x, is_spec)))) {
      ### All specifications; return without doing anything
      return(structure(x,
                       class=c("justifierSpecList", types)));
    } else {
      ### So they're not all specifications; this should be a single specification
      ### then. However, if only 'id' is set, that can be shorthand. So first check
      ### whether 'id' is the only name; if so, 'unvectorize' if needed, structure
      ### as specification(s), and return.
      if ((length(unique(names(x)))==1) && (unique(names(x)) == "id")) {
        ids <- x$id;
        return(structure(lapply(ids,
                                function(id) {
                                  return(structure(list(id = id),
                                                   class=c("justifierSpec", type)));
                                }),
                         names=ids,
                         class=c("justifierSpecList", types)));
      } else {
        ### More than one namee, so it should be a single specification.
        ### Structure it as specification and return it.
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


