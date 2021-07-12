#' Options for the justifier package
#'
#' The `justifier::opts` object contains three functions to set, get, and reset
#' options used by the escalc package. Use `justifier::opts$set` to set options,
#' `justifier::opts$get` to get options, or `justifier::opts$reset` to reset specific or
#' all options to their default values.
#'
#' If you use justifier to programmatically document your decisions in an
#' R file, there is one option that you common use: `workspace_id` and
#' `workspace_option_name`
#'
#' It is normally not necessary to get or set `justifier` options.
#'
#' The following arguments can be passed:
#'
#' \describe{
#'   \item{...}{For `justifier::opts$set`, the dots can be used to specify the options
#'   to set, in the format `option = value`, for example,
#'   `EFFECTSIZE_POINTESTIMATE_NAME_IN_DF = "\n"`. For
#'   `justifier::opts$reset`, a list of options to be reset can be passed.}
#'   \item{option}{For `justifier::opts$set`, the name of the option to set.}
#'   \item{default}{For `justifier::opts$get`, the default value to return if the
#'   option has not been manually specified.}
#' }
#'
#' The following options can be set:
#'
#' \describe{
#'
#'   \item{}{The name of the column
#'   with the effect size values.}
#'
#'   \item{}{The name of the column
#'   with the effect size variance.}
#'
#'   \item{}{The name of the column
#'   with the missing values.}
#'
#' }
#'
#' @aliases opts set get reset
#'
#' @usage opts
#'
#' @examples ### Get the default 'silent' setting
#' justifier::opts$get('silent');
#'
#' ### Set to FALSE
#' justifier::opts$set(silent = FALSE);
#'
#' ### Check that it worked
#' justifier::opts$get('silent');
#'
#' ### Reset this option to its default value
#' justifier::opts$reset('silent');
#'
#' ### Check that the reset worked, too
#' justifier::opts$get('silent');
#'
#' @export opts
opts <- list();

opts$set <- function(...) {
  dots <- list(...);
  dotNames <- names(dots);
  names(dots) <-
    paste0("justifier.", dotNames);
  if (all(dotNames %in% names(opts$defaults))) {
    do.call(options,
            dots);
  } else {
    stop("Option '", option, "' is not a valid (i.e. existing) option for justifier!");
  }
}

opts$get <- function(option, default=FALSE) {
  optionName <- as.character(substitute(option));
  if ((!is.null(opts$defaults)) && !optionName %in% names(opts$defaults)) {
    stop("Option '", optionName, "' is not a valid (i.e. existing) option for justifier!");
  } else {
    return(getOption(paste0("justifier.", optionName),
                     opts$defaults[[optionName]]));
  }
}

opts$reset <- function(...) {
  optionNames <-
    unlist(lapply(as.list(substitute(...())),
                  as.character));
  if (length(optionNames) == 0) {
    do.call(opts$set,
            opts$defaults);
  } else {
    prefixedOptionNames <-
      paste0("justifier.", optionNames);
    if (all(optionNames %in% names(opts$defaults))) {
      do.call(opts$set,
              opts$defaults[optionNames]);
    } else {
      invalidOptions <-
        !(optionNames %in% names(opts$defaults));
      stop("Option(s) ", vecTxtQ(optionNames[invalidOptions]),
           "' is/are not a valid (i.e. existing) option for justifier!");
    }
  }
}

opts$defaults <-
  list(

    ### Default regex replacements when sanitizing for DiagrammeR
    regExReplacements = list(c("\\\"", "`"),
                             c("\\'", "`"),
                             c("\\\\", "/"),
                             c("[^a-zA-Z0-9;)(,._-`/]", " ")),

    weight_fieldName = "weight",

    ### scales::show_col(viridis::viridis(2, begin=.1, end=.8, alpha=.9))
    negWeight_color = "#482576E6",
    posWeight_color = "#9AD93CE6",

    node_color = "#000000FF",
    edge_color = "#000000FF",

    penwidth = 2,

    ### For working with workspaces
    workspace_id = "wsid",

    ### Use triple colon because when constructing this object,
    ### it's not exported yet, or something like that?
    workspace = paste0("WORKSPACE_", justifier:::opts$get("workspace_id")),

    ### Whether to be chatty or silent
    silent = TRUE

  )

