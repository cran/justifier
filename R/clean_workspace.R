#' Clean your workspace
#'
#' @param force Whether to force cleaning the workspace
#' @param silent Whether to be chatty or silent.
#'
#' @export
#'
#' @examples ### Without `force=TRUE`, presents a query to the user in
#' ### interactive mode:
#' clean_workspace(silent=FALSE);
#'
#' ### Set `force=TRUE` to force clean the workspace
#' clean_workspace(force = TRUE, silent=FALSE);
clean_workspace <- function(force = FALSE,
                            silent=justifier::opts$get('silent')) {

  if (!force) {
    if (interactive()) {
      reply <-
        readline("Cleaning your workspace is irreversible. Are you sure? [Yes/No] ");
      if (tolower(reply) == "yes") {
        force <- TRUE;
      }
    }
  }

  if (force) {
    do.call(options,
            stats::setNames(list(NULL),
                            justifier::opts$get('workspace')));
    if (!silent) {
      message("Your justifier workspace has been cleaned and is now empty.");
    }
  } else {
    if (!silent) {
      message("You did not specify `force=TRUE` or did answer `yes` when I asked ",
              "you whether you were sure. Did NOT clear your workspace.");
    }
  }

  return(invisible(NULL));
}
