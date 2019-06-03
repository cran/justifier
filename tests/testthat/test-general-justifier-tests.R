context("general justifier tests")

###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------

testthat::test_that("reading a file with justifications works", {

  examplePath <- file.path(system.file(package="justifier"), 'extdata');

  res <- justifier::load_justifications(file.path(examplePath,
                                                  "example-minutes.jmd"));

  testthat::expect_equal(length(res$raw), 2);

});

###-----------------------------------------------------------------------------

testthat::test_that("reading a directory with justifications works", {

  examplePath <- file.path(system.file(package="justifier"), 'extdata');

  res <- load_justifications_dir(examplePath);

  testthat::expect_equal(length(res), 5);

});

###-----------------------------------------------------------------------------

testthat::test_that("parsing justifications works", {

  examplePath <- file.path(system.file(package="justifier"), 'extdata');

  res <- load_justifications(file.path(examplePath,
                                       "pp19.1-target-behavior-selection.jmd"));

  testthat::expect_equal(res$supplemented$decisions$
                           decision_to_select_behavior_1$
                           justification$justification_05$
                           assertion$assertion_nocturnal_2$
                           source$source_Lange$xdoi,
                         "doi:10.1111/j.1749-6632.2009.05300.x");

  testthat::expect_equal(res$supplemented$assertions$
                           assertion_sleep_memory_1$
                           source$source_Diekelmann$xdoi,
                         "doi:10.1038/nrn2762");

  testthat::expect_equal(res$supplemented$sources$
                           source_Diekelmann$
                           comment,
                         "test of a comment");

});

###-----------------------------------------------------------------------------

testthat::test_that("parsing simplified, just extracted justifications from yum works", {

  examplePath <- file.path(system.file(package="justifier"), 'extdata');

  res1 <- yum::load_and_simplify(file.path(examplePath,
                                           "pp19.1-target-behavior-selection.jmd"));

  res2 <- justifier::parse_justifications(res1);

  testthat::expect_equal(res2$supplemented$decisions$
                           decision_to_select_behavior_1$
                           justification$justification_05$
                           assertion$assertion_nocturnal_2$
                           source$source_Lange$xdoi,
                         "doi:10.1111/j.1749-6632.2009.05300.x");

});

###-----------------------------------------------------------------------------

testthat::test_that("the intervention development justification from the vignette is parsed correctly", {

  exampleFile <-
    system.file("doc",
                "justifier-in-intervention-development.Rmd",
                package="justifier");

  ### Sometimes the vignettes aren't rendered properly
  if (file.exists(exampleFile)) {
    res1 <- yum::load_and_simplify(exampleFile);

    res2 <- justifier::parse_justifications(res1);

    testthat::expect_equal(res2$supplemented$decisions$target_behavior_selection$type,
                           "selection_target_behavior");
  }

});

###-----------------------------------------------------------------------------

testthat::test_that("odd objects provided to to_specList throw an error", {

  testthat::expect_error(justifier::to_specList(list(1:4)));

});

#remotes::install_gitlab("r-packages/justifier", upgrade = "never", build_opts = c());

