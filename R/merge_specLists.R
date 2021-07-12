#' Merging to justifier specification lists
#'
#' @param x,y The two justifier specification lists
#'
#' @return A merged justifier specification list.
#' @export
#'
#' @examples ### Add example
merge_specLists <- function(x, y) {
  if (!(("justifierSpecList" %in% class(x)) &&
        ("justifierSpecList" %in% class(y)))) {
    stop("Both x and y must have be justifier specification lists, ",
         "as indicated by their having class 'justifierSpecList'. However, ",
         "x has class(es) ", vecTxtQ(class(x)), " and y has class(es) ",
         vecTxtQ(class(y)), ".");
  }

  ### All elements in x and y
  xNames <- names(x);
  yNames <- names(y);

  ### Elements in both x and y
  overlap <- intersect(xNames, yNames);

  ### Elements in only x and only y
  xOnly <- setdiff(xNames, yNames);
  yOnly <- setdiff(yNames, xNames);

  ### Combine x with all elements in y not in x
  res <- c(x,
           y[yOnly]);

  ### Merge elements in both
  for (i in overlap) {
    res[[i]] <-
      merge_specs(x[[i]],
                  y[[i]]);
  }

  ### Return the result
  return(res);

}
