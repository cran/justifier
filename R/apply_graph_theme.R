#' Apply multiple DiagrammeR global graph attributes
#'
#' @param graph The [DiagrammeR::DiagrammeR] graph to apply the attributes to.
#' @param ... One or more character vectors of length three, where the first element is
#' the attribute, the second the value, and the third, the attribute type (`graph`,
#' `node`, or `edge`).
#'
#' @return The [DiagrammeR::DiagrammeR] graph.
#' @examples exampleJustifier <- '
#' ---
#' assertion:
#'   -
#'     id: assertion_id
#'     label: "An assertion"
#' decision:
#'   -
#'     id: decision_id
#'     label: "A decision"
#'     justification:
#'       -
#'         id: justification_id
#'         label: "A justification"
#'         assertion:
#'           -
#'             id: assertion_id
#'             description: "A description of an assertion"
#'             source:
#'               -
#'                 id: source1_id
#'                 label: "First source"
#'               -
#'                 id: source2_id
#'                 label: "second source"
#' ---
#' ';
#' justifications <-
#'   load_justifications(text=exampleJustifier);
#' miniGraph_original <-
#'   justifications$decisionGraphs[[1]];
#' miniGraph <-
#'   apply_graph_theme(miniGraph_original,
#'                     c("color", "#0000AA", "node"),
#'                     c("shape", "triangle", "node"),
#'                     c("fontcolor", "#FF0000", "node"));
#' ### This line should be run when executing this example as test, because
#' ### rendering a DiagrammeR graph takes quite long
#' \dontrun{
#' DiagrammeR::render_graph(miniGraph);
#' }
#' @export
apply_graph_theme <- function(graph,
                              ...) {
  for (currentSetting in list(...)) {
    if ((length(currentSetting) != 3) && is.character(currentSetting)) {
      stop("Only provide character vectors of length 3 in the dots (...) argument!");
    } else {
      graph <-
        DiagrammeR::add_global_graph_attrs(graph,
                                           currentSetting[1],
                                           currentSetting[2],
                                           currentSetting[3]);
    }
  }
  return(graph);
}
