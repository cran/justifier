#' Set your justifier workspace identifier
#'
#' This is used to be able to log decisions programmatically.
#'
#' @param id The workspace identifier
#' @param silent Whether to be suppress messages.
#'
#' @return Invisibly, the passed `id`.
#' @export
#'
#' @examples set_workspace_id("my_workspace");
set_workspace_id <- function(id,
                             silent=justifier::opts$get('silent')) {
  if (is.null(id) | is.na(id) | (length(id) != 1)) {
    stop("Your justifier workspace identifier has to be one or ",
         "more characters long, but you passed: ", id);
  }
  if (!grepl("^[a-zA-Z0-9_]*$", id)) {
    stop("Your justifier workspace identifier may only contain ",
         "latin letters, arabic numbers, and underscores. You passed: ", id);
  }
  justifier::opts$set(workspace_id = id);
  if (!silent) {
    message("Just set your workspace identifier to '",
            id, "'.\n");
  }
  return(invisible(id));
}
