parse_framework <- function(x) {
  if (!unique(names(x)) == "justifier") {
    stop("This framework specification contains illegal elements. Framework ",
         "specifications must be specified in valid YAML and within an ",
         "element called 'justifier'. However, this framework also contains ",
         "elements called ", vecTxtQ(setdiff(names(x), 'justifier')), ".");
  }

  ### If we're good, set up this framework
  res <- list(id = character(),
              label = character(),
              description = character(),
              contextSpecifications = list(),
              conditionSpecifications = list(),
              contexts = data.frame(),
              conditions = list());

  ### Process every element and extract data contexts and conditions
  for (i in length(x)) {
    if (!is.null(x[[i]]$id)) {
      res$id <- c(res$id, x[[i]]$id);
    }
    if (!is.null(x[[i]]$label)) {
      res$label <- c(res$label, x[[i]]$label);
    }
    if (!is.null(x[[i]]$description)) {
      res$description <- c(res$description, x[[i]]$description);
    }
    if (!is.null(x[[i]]$context)) {
      res$contextSpecifications <- c(res$contextSpecifications, x[[i]]$context);
    }
    if (!is.null(x[[i]]$condition)) {
      res$conditionSpecifications <- c(res$conditionSpecifications, x[[i]]$condition);
    }
  }

  ### Process context specifications into a dataframe. To prevent loading
  ### dplyr for its bind_rows, we'll do this manually (base R's 'rbind'
  ### complains when not all dataframes have the same columns)
  for (i in seq_along(res$contextSpecifications)) {
    if (!is.null(res$contextSpecifications[[i]]$id)) {
      res$contexts[i, 'id'] <- res$contextSpecifications[[i]]$id;
    }
    if (!is.null(res$contextSpecifications[[i]]$label)) {
      res$contexts[i, 'label'] <- res$contextSpecifications[[i]]$label;
    }
    if (!is.null(res$contextSpecifications[[i]]$description)) {
      res$contexts[i, 'description'] <- res$contextSpecifications[[i]]$description;
    }
    if (!is.null(res$contextSpecifications[[i]]$sequence)) {
      res$contexts[i, 'sequence'] <- res$contextSpecifications[[i]]$sequence;
    }
    if (!is.null(res$contextSpecifications[[i]]$parentId)) {
      res$contexts[i, 'parentId'] <- res$contextSpecifications[[i]]$parentId;
    }
  }

  ### Process condition specifications (organise by element, then by type);
  res$conditions <- list(decision = list(),
                         justification = list(),
                         assertion = list(),
                         source = list());

  ### For convenience
  validElements <- c("decision",
                     "justification",
                     "assertion",
                     "source");

  for (i in seq_along(res$conditionSpecifications)) {

    ### Only process if the element and type are specified; otherwise skip
    if (!is.null(res$conditionSpecifications[[i]]$element)) {

      if (is.null(res$conditionSpecifications[[i]]$type)) {
        res$conditionSpecifications[[i]]$type <- ".*";
      }

      ### Set vars for convenience
      currentElement <- res$conditionSpecifications[[i]]$element;
      currentType <- res$conditionSpecifications[[i]]$type;

      if (currentElement %in% validElements) {
        if (is.null(res$conditions[[currentElement]][[currentType]])) {
          res$conditions[[currentElement]][[currentType]] <- list();
        }

        ### Get identifier, and if need be, create an empty object
        if (!is.null(res$conditionSpecifications[[i]]$id)) {
          currentId <- res$conditionSpecifications[[i]]$id;
          if (is.null(res$conditions[[currentElement]][[currentType]][[currentId]])) {
            res$conditions[[currentElement]][[currentType]][[currentId]] <-
              list();
          }
        } else {
          stop("Encountered a framework specification where a condition did not ",
               "have an identifier (id) set!");
        }

        ### Fill the object for this identifier. Note that we overwrite previous info.

        ### label
        if (!is.null(res$conditionSpecifications[[i]]$label)) {
          res$conditions[[currentElement]][[currentType]][[currentId]]$label <-
            res$conditionSpecifications[[i]]$label;
        }
        ### description
        if (!is.null(res$conditionSpecifications[[i]]$description)) {
          res$conditions[[currentElement]][[currentType]][[currentId]]$description <-
            res$conditionSpecifications[[i]]$description;
        }
        ### context
        if (!is.null(res$conditionSpecifications[[i]]$contextId)) {
          res$conditions[[currentElement]][[currentType]][[currentId]]$contextId <-
            res$conditionSpecifications[[i]]$contextId;
        }
        ### values
        if (!is.null(res$conditionSpecifications[[i]]$values)) {
          res$conditions[[currentElement]][[currentType]][[currentId]]$values <-
            res$conditionSpecifications[[i]]$values;
        }
        ### scores
        if (!is.null(res$conditionSpecifications[[i]]$scores)) {
          res$conditions[[currentElement]][[currentType]][[currentId]]$scores <-
            res$conditionSpecifications[[i]]$scores;
        }
        ### verifications
        if (!is.null(res$conditionSpecifications[[i]]$verifications)) {
          res$conditions[[currentElement]][[currentType]][[currentId]]$verifications <-
            res$conditionSpecifications[[i]]$verifications;
        }
        ### Verification messages
        if (!is.null(res$conditionSpecifications[[i]]$verificationMsgs)) {
          res$conditions[[currentElement]][[currentType]][[currentId]]$verificationMsgs <-
            rep_len(res$conditionSpecifications[[i]]$verificationMsgs,
                    length(res$conditions[[currentElement]][[currentType]][[currentId]]$verifications));
        }
        ### fields
        if (!is.null(res$conditionSpecifications[[i]]$field)) {
          res$conditions[[currentElement]][[currentType]][[currentId]]$field <-
            res$conditionSpecifications[[i]]$field;
        }

      } else {
        stop("Encountered a framework specification where a condition had an ",
             "element other than one of the valid elements (",
             vecTxtQ(validElements), ") was specified, specifically, ",
             vecTxtQ(currentElement), "!");
      }
    }
  }


  class(res) <- c("justifierFramework", "list");

  return(res);

}
