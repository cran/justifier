#' Export a justifier specification to JSON
#'
#' @param x The justifier specification.
#' @param file Optionally, a file to save the JSON to.
#' @param wrap_in_html Whether to wrap the JSON in an HTML element.
#' @param ... Any additional arguments are ignored.
#'
#' @return If a file is specified to write, to, `x` will be returned invisibly
#' to allow building a pipe chain; if `file=NULL`, the resulting JSON
#' will be returned as a character vector.
#' @export
#' @rdname export_to_json
#'
#' @examples ### Programmatically create a justification with two assertions
#' ### but without sources; flatten it; and show the json
#' justifier::justify(
#'   "Icecream will make me feel less fit",
#'   assertion = c(
#'     justifier::assert('Icecream is rich in energy'),
#'     justifier::assert('Consuming high-energy foods makes me feel less fit')
#'   ),
#'   weight = -.5
#' ) |>
#'   justifier::flatten() |>
#'   justifier::export_to_json();
export_to_json <- function(x,
                           file = NULL,
                           wrap_in_html = FALSE) {

  if (!requireNamespace('jsonlite', quietly=TRUE)) {
    stop("You need to have 'jsonlite' installed to convert to JSON!");
  }

  if (inherits(x, "justifierStructuredObject")) {
    UseMethod("export_to_json");
  } else {
    stop("As `x`, you have to pass a structured justifier object.");
  }

}

#' @export
#' @rdname export_to_json
#' @method export_to_json justifierStructuredObject
export_to_json.justifierStructuredObject <- function(x,
                                                     file = NULL,
                                                     wrap_in_html = FALSE) {

  xToWrite <- x;

  if (is.null(file)) {
    res <-
      jsonlite::toJSON(
        xToWrite,
        pretty = TRUE,
        force = TRUE
      );

    if (wrap_in_html) {

      ### Escape single quotes
      res <-
        gsub("'",
             "&#39;",
             res
        );

      ### Generate id slug
      slug <- paste0("justifier-data-", justifier::randomSlug());

      res <-
        paste0(
          '<div id="',
          slug,
          '" data-justifier=\'',
          res,
          '\' class="justifier hidden" style="display:none"></div>'
        );

    } else {

      class(res) <- c("justifier_json", class(res));

    }

    return(
      res
    );

  } else {
    jsonlite::write_json(
      xToWrite,
      file,
      pretty = TRUE,
      force = TRUE
    );
    return(invisible(xToWrite));
  }
}

#' @rdname export_to_json
#' @export
#' @method print justifier_json
print.justifier_json <- function(x, ...) {
  cat(x, sep="\n");
  return(invisible(x));
}
