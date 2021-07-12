create_justifierGraph <- function(
  dTree,
  weight_fieldName = justifier::opts$get("weight_fieldName")
) {

  negWeight_color <- justifier::opts$get("negWeight_color");
  posWeight_color <- justifier::opts$get("posWeight_color");
  node_color <- justifier::opts$get("node_color");
  edge_color <- justifier::opts$get("edge_color");
  penwidth <- justifier::opts$get("penwidth");

  if (!("Node" %in% class(dTree))) {
    stop("You must pass a justifier tree: you passed an object ",
         "of class(es) ", vecTxtQ(class(dTree)), "!");
  }

  tryCatch({

    dTree$Do(function(node) {

      lbl <-
        ifelse(is.null(node$label),
               node$name,
               node$label);

      lbl <- justifier::sanitize_for_DiagrammeR(lbl);

      lbl <- paste0(strwrap(lbl, 40), collapse = "\n");

      if (!is.null(node$weight)) {
        data.tree::SetNodeStyle(
          node,
          color = ifelse(
            node$weight < 0,
            negWeight_color,
            posWeight_color
          ),
          label = lbl);
        data.tree::SetEdgeStyle(
          node,
          color = ifelse(
            node$weight < 0,
            negWeight_color,
            posWeight_color
          ));
      } else {
        data.tree::SetEdgeStyle(
          node,
          color = edge_color
        );
        data.tree::SetNodeStyle(
          node,
          color = edge_color,
          label = lbl
        );
      }


    });

    dTreeGraph <-
      data.tree::ToDiagrammeRGraph(dTree);

  },
  error = function(e) {
    warning(
      "Error issued when converting decision tree to a decision graph: ",
      e$message,
      "\n\nClass and content:\n\n",
      paste0(utils::capture.output(print(class(
        dTree
      ))),
      collapse = "\n"),
      "\n",
      paste0(utils::capture.output(print(dTree)),
             collapse = "\n")
    )

  });

  if (is.null(dTreeGraph)) {
    dTreeGraph <- NA;
  } else {
    dTreeGraph <-
      justifier::apply_graph_theme(
        dTreeGraph,
        c("layout",      "dot",            "graph"),
        c("rankdir",     "LR",             "graph"),
        c("outputorder", "edgesfirst",     "graph"),

        c("fontname",    "Arial",          "node"),
        c("fillcolor",   "#FFFFFF",        "node"),
        c("fixedsize",   "false",          "node"),
        c("shape",       "box",            "node"),
        c("style",       "rounded,filled", "node"),
        c("color",       node_color,       "node"),
        c("margin",      "0.2,0.2",        "node"),

        c("fontname",    "Arial",          "edge"),
        c("color",       edge_color,       "edge"),
        c("penwidth",    penwidth,         "edge"),
        c("headclip",    "false",          "edge"),
        c("tailclip",    "false",          "edge"),
        c("dir",         "none",           "edge")

      )
  }

  class(dTreeGraph) <-
    c("justifierDecisionGraph", class(dTreeGraph));

  return(dTreeGraph);

}
