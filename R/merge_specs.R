merge_specs <- function(x,
                        y,
                        xSpecs = ".*",
                        ySpecs = ".*",
                        conflictNames = NULL) {

  xFields <- grep(pattern=xSpecs,
                  x=names(x),
                  value=TRUE);
  yFields <- grep(pattern=ySpecs,
                  x=names(y),
                  value=TRUE);
  allFields <- union(xFields, yFields);
  dupFields <- intersect(xFields, yFields);
  xOnly <- xFields[!(xFields %in% dupFields)];
  yOnly <- yFields[!(yFields %in% dupFields)];

  identicalFields <-
    unlist(lapply(dupFields,
                  function(i) identical(x[i], y[i])));
  if (length(identicalFields) > 0) {
    names(identicalFields) <-
      dupFields;
  }

  if (all(identicalFields)) {
    ### In this case, all duplicated fields are identical.
    ### That means we can simply add all fields that are in
    ### y but not in x to x, and return the result; we don't
    ### have to do field-level-merging
    return(c(x[xFields],
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
