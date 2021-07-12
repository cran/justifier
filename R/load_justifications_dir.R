#' @rdname load_justifications
#' @export
load_justifications_dir <- function(path,
                                    recursive = TRUE,
                                    extension = "jmd",
                                    regex = NULL,
                                    justificationContainer = c("justifier",
                                                               "justification",
                                                               "decision",
                                                               "assertion",
                                                               "source"),
                                    delimiterRegEx = "^---$",
                                    ignoreOddDelimiters = FALSE,
                                    encoding="UTF-8",
                                    silent=TRUE) {

  if (!dir.exists(path)) {
    stop("Directory '",
         path,
         "' does not exist!");
  }

  if (is.null(regex)) {
    regex <- paste0("^(.*)\\.", extension, "$");
  }

  ###--------------------------------------------------------------------------
  ### Load the YAML fragments containing the DCT specifications
  ###--------------------------------------------------------------------------

  # justifications <-
  #   yum::load_and_simplify_dir(path=path,
  #                              recursive=recursive,
  #                              fileRegexes = regex,
  #                              select=paste0(justificationContainer,
  #                                            collapse="|"),
  #                              delimiterRegEx = delimiterRegEx,
  #                              ignoreOddDelimiters = ignoreOddDelimiters,
  #                              encoding = encoding,
  #                              silent=silent);

  ### Load manually
  justifications <-
    yum::load_yaml_dir(path=path,
                       recursive=recursive,
                       fileRegexes=regex,
                       select=paste0(justificationContainer,
                                     collapse="|"),
                       delimiterRegEx = delimiterRegEx,
                       ignoreOddDelimiters = ignoreOddDelimiters,
                       encoding = encoding,
                       silent=silent);

  ### Store filename in every list
  justifications <- lapply(seq_along(justifications),
                            function(i) {
                              ### This is a file; it has several YAML fragments
                              res <- lapply(seq_along(justifications[[i]]),
                                            function(j, elementName = names(justifications[[i]][[j]])) {
                                              if (FALSE) { #elementName == 'justified') {
                                                res <- justifications[[i]][[j]];
                                                if (is.list(res)) {
                                                  res[['fromFile']] <-
                                                    names(justifications)[i];
                                                } else {
                                                  res['fromFile'] <-
                                                    names(justifications)[i];
                                                }
                                              } else {
                                                ### We're now within a YAML fragment;
                                                ### Each has several specified objects, each
                                                ### containing one or more specifications, so
                                                ### we have to go two levels deeper
                                                res <- lapply(seq_along(justifications[[i]][[j]]),
                                                              function(k) {
                                                                ### We're now within a specified object;
                                                                ### If it has names, add the filename and
                                                                ### continue; otherwise, recursive one level
                                                                ### deeper and then add the filename.
                                                                if (is.null(names(justifications[[i]][[j]][[k]]))) {
                                                                  res <- lapply(seq_along(justifications[[i]][[j]][[k]]),
                                                                                function(l) {
                                                                                  ### Deepest possible level;
                                                                                  ### add the filename and continue
                                                                                  res <- justifications[[i]][[j]][[k]][[l]];
                                                                                  if (is.list(res)) {
                                                                                    res[['fromFile']] <-
                                                                                      names(justifications)[i];
                                                                                  } else {
                                                                                    res['fromFile'] <-
                                                                                      names(justifications)[i];
                                                                                  }
                                                                                  return(res);
                                                                                });
                                                                } else {
                                                                  res <- justifications[[i]][[j]][[k]];
                                                                  if (is.list(res)) {
                                                                    res[['fromFile']] <-
                                                                      names(justifications)[i];
                                                                  } else {
                                                                    res['fromFile'] <-
                                                                      names(justifications)[i];
                                                                  }
                                                                }
                                                                return(res);
                                                              });
                                              }
                                              names(res) <- names(justifications[[i]][[j]]);
                                              return(res);
                                            });
                              names(res) <- names(justifications[[i]]);
                              class(res) <- c("yumFromFile", "list");
                              return(res);
                            });

  class(justifications) <- c("yumFromDir", "list");

  ### Remove names like we do in yum::load_and_simplify_dir
  names(justifications) <- NULL;

  ### Simplify
  justifications <-
    yum::simplify_by_flattening(justifications,
                                simplify = ".*");

  ### Remove empty elements
  if (is.null(justifications)) {
    justifications <- list();
  }

  class(justifications) <-
    c("simplifiedYum", "list");

  ###--------------------------------------------------------------------------
  ### Parse DCT specifications and return result
  ###--------------------------------------------------------------------------

  res <-
    parse_justifications(justifications,
                         path=path,
                         silent=silent);

  return(res);

}
