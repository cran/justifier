#' Export justification as YAML
#'
#' @param x The justification, either loaded from one or more files or
#' programmatically constructed. This can be one or more decisions,
#' justifications, assertions, or sources.
#' @param file If specified, the file to export the justification to.
#' @param encoding The encoding to use when writing the file.
#' @param append Whether to append to the file, or replace its contents.
#' @param preventOverwriting Whether to prevent overwriting an existing file.
#' @param silent Whether to be silent or chatty.
#'
#' @return The generated YAML, invisibly, unless file is NULL.
#' @export
#'
#' @examples ### Programmatically create a simple justification object
#' justifierObject <-
#'   justifier::asrt(
#'     "assertion",
#'     source = c(
#'       justifier::srce('source1'),
#'       justifier::srce('source2')));
#'
#' ### Export to YAML
#' justifierYAML <-
#'   justifier::export_justification(
#'     justifierObject,
#'     file=NULL);
#'
#' ### Show YAML
#' cat(justifierYAML, sep="\n");
export_justification <- function(x,
                                 file = NULL,
                                 encoding = "UTF-8",
                                 append = TRUE,
                                 preventOverwriting = TRUE,
                                 silent = justifier::opts$get('silent')) {

  if ("justifierSource" %in% class(x)) {
    justifierType <- "source";
  } else if ("justifierAssertion" %in% class(x)) {
    justifierType <- "assertion";
  } else if ("justifierJustification" %in% class(x)) {
    justifierType <- "justification";
  } else if ("justifierDecision" %in% class(x)) {
    justifierType <- "decision";
  } else {
    stop("The object you passed with argument `x` does not have a valid class!");
  }

  x <- yaml::as.yaml(x);
  x <- gsub("\n", "\n  ", x);
  x <- paste0("---\n",
              justifierType,
              ":\n  ",
              x,
              "\n---\n");

  if (is.null(file)) {
    class(x) <- c("export_justification", class(x));
    return(x);
  } else {
    if (!dir.exists(dirname(file))) {
      stop("The directory specified where the output file '",
           basename(file), "' is supposed to be written ('",
           dirname(file),
           "') does not exist.");
    }
    if (file.exists(file) && append) {
        con <- file(description=file,
                    open="a",
                    encoding=encoding);
        writeLines(text=c("\n\n",
                          x),
                   con=con);
        close(con);
    } else if (file.exists(file) && preventOverwriting) {
      if (!silent) {
        message("File '",
                file, "' exists, and `preventOverwriting` was `TRUE`, so I did not ",
                "write the justification to disk.");
      }
    } else {
      con <- file(description=file,
                  open="w",
                  encoding=encoding);
      writeLines(text=x,
                 con=con);
      close(con);
    }
    if (!silent) {
      message("I just wrote a justification to file '",
              file,
              "'.");
    }
    invisible(x);
  }


}

#' @export
#' @method print export_justification
print.export_justification <- function(x,
                                       ...) {
  cat(x);
  return(invisible(x));
}
