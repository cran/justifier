#' Import a structured justifier object from JSON
#'
#' @param x Either a path to an existing file, or a character vector with the
#' JSON to import.
#'
#' @return The justifier object.
#' @export
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
#'   justifier::flatten() -> originalObject;
#'
#' originalObject |>
#'   justifier::export_to_json() ->
#'   exportedJSON;
#'
#' ### And import it again
#' importedFromJSON <-
#'   justifier::import_from_json(
#'     exportedJSON
#'   );
import_from_json <- function(x) {

  if (!requireNamespace('jsonlite', quietly=TRUE)) {
    stop("You need to have 'jsonlite' installed to convert to JSON!");
  }

  if ((is.character(x)) && (length(x) == 1) && (file.exists(x))) {
    res <- jsonlite::read_json(path = x);
  } else if ((is.character(x))) {
    res <- jsonlite::fromJSON(x);
  } else {
    stop("As `x`, you have to pass either a character vector containing ",
         "the JSON, or the path to an existing file to import.");
  }

  res <-
    set_justifier_classes_to_structured_object(
      res
    );

  return(res);

}
