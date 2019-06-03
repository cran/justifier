#' @rdname load_justifications
#' @export
load_justifications_dir <- function(path,
                                    recursive = TRUE,
                                    extension = "jmd",
                                    regex,
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

  if (missing(regex)) {
    regex <- paste0("^(.*)\\.", extension, "$");
  }

  ###--------------------------------------------------------------------------
  ### Load the YAML fragments containing the DCT specifications
  ###--------------------------------------------------------------------------

  justifications <-
    yum::load_and_simplify_dir(path=path,
                               recursive=recursive,
                               fileRegexes = regex,
                               select=paste0(justificationContainer,
                                             collapse="|"),
                               delimiterRegEx = delimiterRegEx,
                               ignoreOddDelimiters = ignoreOddDelimiters,
                               encoding = encoding,
                               silent=silent);

  ###--------------------------------------------------------------------------
  ### Parse DCT specifications and return result
  ###--------------------------------------------------------------------------

  res <-
    parse_justifications(justifications);

  return(res);

}
