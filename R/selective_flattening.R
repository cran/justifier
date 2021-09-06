selective_flattening <- function(x,
                                 what,
                                 recursionLevel = 0,
                                 silent = justifier::opts$get("silent")) {

  msg(spc(recursionLevel),
      "Selectively flattening an object called '",
      deparse(substitute(x)), "' only selecting ",
      what, " specifications.\n",
      silent = silent);

  justifierClass <-
    paste0(
      "justifier",
      tools::toTitleCase(what)
    );

  justifierPlural <-
    paste0(tolower(what), "s");

  if ((inherits(x, justifierClass)) && (inherits(x, "singleJustifierElement"))) {

    msg(spc(recursionLevel),
        "Single justifier element present with identifier '",
        x$id, "'.\n",
        silent = silent);

    res <- emptyStructuredJustifierObject;

    classToUse <- class(res[[justifierPlural]]);

    res[[justifierPlural]] <- list(x);

    class(res[[justifierPlural]]) <- classToUse;

    names(res[[justifierPlural]]) <-
      get_ids_from_structured_justifierElements(
        res[[justifierPlural]]
      );

    class(res) <- c("justifier", "justifierStructuredObject", "list");

    return(res);

  } else if ((inherits(x, justifierClass)) && (inherits(x, "multipleJustifierElements"))) {

    msg(spc(recursionLevel),
        length(x), " elements present, calling myself recursively to structure them.\n",
        silent = silent);

    res <-
      do.call(
        c,
        lapply(
          x,
          selective_flattening,
          what = what,
          recursionLevel = recursionLevel + 1,
          silent = silent
        )
      );

    res <- unlist(res, recursive = FALSE);

    class(res) <- c("justifier", "justifierStructuredObject", "list");

    return(res);

  } else {

    stop("You passed an object that isn't a singleJustifierElement of ",
         "class ", justifierClass, " or a list of ",
         "multipleJustifierElements. Instead, it has class(es) ",
         vecTxtQ(class(x)), "...");

  }

}
