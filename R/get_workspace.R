#' Get your justifier workspace identifier
#'
#' This is used to be able to log decisions programmatically.
#'
#' @param silent Whether to be suppress messages.
#'
#' @return Invisibly, the workspace identifier.
#' @export
#'
#' @examples justifier::get_workspace_id();
get_workspace <- function(silent=justifier::opts$get('silent')) {
  id <- justifier::opts$get('workspace_id');

  if (!silent) {
    message("Your justifier workspace, currently set to workspace identifier '",
            id, "', contains .\n");
  }

  return(invisible(id));
}
