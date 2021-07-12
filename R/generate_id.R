#' Generate unique identifier(s)
#'
#' Convenience function to generate a unique identifiers for
#' sources, assertions, justifications, and decisions.
#'
#' @param prefix An identifier prefix.
#' @param type The type of the justifier object; `D`, `J`, `A` or `S`.
#' @param stopOnIllegalChars Whether to [base::stop()] or produce a [base::warning()]
#' when encountering illegal characters (i.e. anything other than a letter or
#' underscore).
#'
#' @return A character vector containing the identifier(s).
#' @examples generate_id(type = "S", 'sourceExample');
#' generate_id(type = "A", 'assertionExample');
#' @name generate_id
#' @rdname generate_id
#' @export
generate_id <- function(type,
                        prefix = paste(sample(letters, 4), collapse=""),
                        stopOnIllegalChars = FALSE) {

  if (length(prefix) > 1) {
    stop("Use `generate_ids` to generate multiple ids at once.");
  }

  type <- toupper(type);
  if (length(type) > 1) {
    stop("I can only generate an identifier of one type!");
  }
  if (!any(c("D", "J", "A", "S") == type)) {
    stop("I can only generate an identifier of type D(ecision), ",
         "J(ustifier), A(ssertion), or S(ource)!");
  }

  if ((grepl("[^a-zA-Z_]+",
             prefix))) {
    if (stopOnIllegalChars) {
      stop("The specified prefix contains illegal characters, and argument ",
           "`stopOnIllegalChars` is set to TRUE, so I'm stopping.");
    } else {
      warning("The specified prefix contains illegal characters, and argument ",
              "`stopOnIllegalChars` is set to FALSE, so I'm removing them.");
      prefix <-
        gsub("[^a-zA-Z_]+",
             "",
             prefix);
    }
  };

  #timeNrString <- gsub('\\.', '', format(Sys.time(), "%y%m%d%H%M%OS2"));
  timeNrString <- as.character(round(as.numeric(Sys.time()) * 100, 0));

  res <- numericToBase30(as.numeric(timeNrString));
  return(paste0(prefix, "_", type, res));
}
