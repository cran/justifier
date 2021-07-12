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
get_workspace_id <- function(silent=justifier::opts$get('silent')) {
  id <- justifier::opts$get('workspace_id');
  if (!silent) {
    message("Your justifier workspace is currently set to workspace identifier '",
            id, "'.\n");
    return(invisible(id));
  } else {
    return(id);
  }
}
