#' Save your workspace
#'
#' @param file If specified, the file to export the justification to.
#' @param encoding The encoding to use when writing the file.
#' @param append Whether to append to the file, or replace its contents.
#' @param preventOverwriting Whether to prevent overwriting an existing file.
#' @param silent Whether to be silent or chatty.
#'
#' @return The result of a call to [export_justification()].
#' @export
#'
#' @examples clean_workspace(force = TRUE, silent=FALSE);
#' log_decision("First we start using `justifier`.",
#'              silent=FALSE);
#' log_decision(paste0("Then we start documenting our ",
#'                     "decisions and justifications."),
#'              silent=FALSE);
#' log_decision("Then we start learning from ourselves.",
#'              silent=FALSE);
#' save_workspace();
save_workspace <- function(file = NULL,
                           encoding = "UTF-8",
                           append = FALSE,
                           preventOverwriting = TRUE,
                           silent=justifier::opts$get('silent')) {

  res <-
    getOption(justifier::opts$get('workspace'),
              NULL);

  if (is.null(res)) {
    cat("Your workspace is empty - not saving anything!");
    return(invisible(NULL));
  }

  return(export_justification(res,
                              file = file,
                              encoding = encoding,
                              append = append,
                              preventOverwriting = preventOverwriting,
                              silent = silent));

}
