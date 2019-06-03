merge_specs <- function(x,
                        y,
                        conflictNames = NULL) {

  xFields <- names(x);
  yFields <-names(y);
  allFields <- union(xFields, yFields);
  dupFields <- intersect(xFields, yFields);
  xOnly <- xFields[!(xFields %in% dupFields)];
  yOnly <- yFields[!(yFields %in% dupFields)];
  identicalFields <-
    structure(unlist(lapply(dupFields,
                            function(i) identical(x[i], y[i]))),
              names=dupFields);
  if (all(identicalFields)) {
    ### In this case, all duplicated fields are identical.
    ### That means we can simply add all fields that are in
    ### y but not in x to x, and return the result; we don't
    ### have to do field-level-merging
    return(c(x,
             y[yOnly]));
  } else {
    z <- lapply(dupFields[!identicalFields],
                function(fieldName) {
                  return(structure(c(x[fieldName],
                                     y[fieldName]),
                                   names=conflictNames));
                });
    names(z) <- dupFields[!identicalFields];
    return(c(x[xOnly],
             x[dupFields[identicalFields]],
             z,
             y[yOnly]));
  }
}
