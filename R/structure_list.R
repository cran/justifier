# recurse_everything_to_the_root <- function(x) {
#   ### Start with the first level
#   res <- structure_list(x);
#   ### Now process each object that requires further processing
#   for (i in seq_along(res)) {
#     tmpRes <- structure_list(res[[i]]);
#     if (all(unlist(lapply(tmpRes, length)) == 0)) {
#       ### No sub-elements - we don't have to restructure anything or call
#       ### ourselves any more, and can return the object as we received it
#       return(res);
#     } else {
#       ### One or more of the elements has a sub-element; call ourselves
#       ### on it
#
#
#     }
#   }
# }

# if (has_child_specs(x))
#



structure_list <- function(x,
                           idPrefix = "id_",
                           firstNrForMissingId = 1,
                           warnOnMissingId = TRUE,
                           stopOnMissingId = FALSE,
                           childNames = c('source',
                                          'assertion',
                                          'justification',
                                          'decision',
                                          'justifier')) {
#
#   inChildNames <- names(x) %in% childNames;
#   res <- list
#
#   ### If x is simply a reference or a specification without children
#   if (is_ref(x) || (is_spec(x) && !is_nested(x) && !has_child_specs(x)) {
#     return(x);
#   }
#
#   if (!has_child_specs(x)) {
#
#   }
#
#   ### Create object to put in what we find.
#   res <- list(sources = list(),
#               assertions = list(),
#               justification = list(),
#               decisions = list(),
#               justifier = list()
#               misc = list());
#
#   xNames <- names(x);
#   xLists <- unlist(lapply(x, is.list));
#
#   if (length(xNames) == 0) {
#     for (i in seq_along(x)) {
#       if (xLists[i]) {
#
#       }
#     }
#   }
#
#
#






  ### Process all specifications and create five organised lists,
  ### where id's are used as names
  justNames <- tolower(names(x));
  res$structured <-
    list(sources = to_specList(x[which(justNames == 'source')],
                               types="sources",
                               type="source"),
         assertions = to_specList(x[which(justNames == 'assertion')],
                                  types="assertions",
                                  type="assertion"),
         justifications = to_specList(x[which(justNames == 'justification')],
                                      types="justifications",
                                      type="justification"),
         decisions = to_specList(x[which(justNames == 'decision')],
                                 types="decisions",
                                 type="decision"),
         justifier = to_specList(x[which(justNames == 'justifier')],
                                 types="justifiers",
                                 type="justifier"));

  ### Set names
  res$structured <- lapply(res$structured, function(elementList) {
    elementIds <-
      lapply(elementList, function(singleElement) {
        return(ifelse(is.null(singleElement$id),
                      "noID",
                      singleElement$id));
      });
    ### Set ids for elements without an id (shouldn't happen;
    ### may want to throws an error instead)
    if (any(elementIds=="noID")) {
      if (stopOnMissingId) {
        stop("Some elements don't have an ID and `stopOnMissingId` is set to TRUE, so here you go!");
      } else if (warnOnMissingId) {
        warning("Some elements don't have an ID and `warnOnMissingId` is set to TRUE, so here you go!");
      }
    }
    elementIds[elementIds=="noID"] <-
      paste0(idPrefix,
             firstNrForMissingId + 1:length(elementIds[elementIds=="noID"]) - 1);
    names(elementList) <-
      elementIds;
    return(elementList);
  });

  return(res$structured);

}
