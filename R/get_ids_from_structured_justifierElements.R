get_ids_from_structured_justifierElements <- function(x) {

  if ("id" %in% names(x)) {

    res <- x['id'];

  } else {

    res <-
      lapply(x,
             function(current) {
               return(current$id);
             });

  }

  res <- unname(unlist(res));

  return(res);

}
