## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  ---
#  justifier:
#    -
#      scope: global   # Can also be local or universal
#      framework: justifier-example-study-framework-specification.jmd
#    -
#      scope: local
#      date: 2019-03-06
#  ---

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  ---
#  decision:
#  
#    ### Type and decision can be the same, but it is also possible that there
#    ### are multiple decisions of the same type, for example relating to different
#    ### aspects of sample size planning or selection of operationalisations.
#    type: research_question
#    id: research_question
#  
#    ### The value is the decision that is taken (the chosen alternative). For decisions
#    ### with a type, this will be checked against the allowed values specified in the
#    ### justifier framework, if such a framework is available.
#    value: "What are the most important determinants of getting one's ecstasy tested?"
#  
#    ### The label and description are human-readable free-text fields.
#    label: "The answer to this research question is required to develop and effective interventions to promote ecstasy pill testing."
#    description: "To minimize the likelihood of incidents and accidental intoxication, testing is a required step."
#  
#    justification:
#      -
#        id: without_testing_users_may_ingest_contaminants
#        label: "Some XTC pills are contaminated, and one needs to test them to be aware of the pill contents."
#        assertion:
#          -
#            id: some_pills_are_contaminated
#            source: vogels_2009
#            label: "Not all produced XTC pills contain only MDMA as active ingredient."
#            evidence_type: "empirical study"
#      -
#        id: proper_dosing_requires_knowing_dose
#        label: "Proper dosing (~ 1-1.5 mg of MDMA per kg of body weight) becomes hard if the dose in a pill is unknown."
#        assertion:
#          id: dosing_requires_pill_content
#          label: "Determining the dose of MDMA one uses requires knowing both one's body weight and the pill dose."
#          source:
#            id: brunt_2012
#            label: "Brunt, Koeter, Niesink & van den Brink (2012)"
#            evidence_type: "empirical study"
#  ---

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  ---
#  decision:
#  
#    ### Type and decision can be the same, but it is also possible that there
#    ### are multiple decisions of the same type, for example relating to different
#    ### aspects of sample size planning or selection of operationalisations.
#    type: global_study_method
#    id: global_study_method
#  
#    ### The value is the decision that is taken (the chosen alternative). For decisions
#    ### with a type, this will be checked against the allowed values specified in the
#    ### justifier framework, if such a framework is available.
#    value: quantitative
#  
#    ### The label and description are human-readable free-text fields.
#    label: "We will conduct a quantitative study."
#    description: "Here, a decision can be explained more in detail (e.g. describing how the justifications relate to each other)."
#  
#    justification:
#      -
#        id: enough_known_about_testing_determinants
#        label: "We have enough information available to develop a questionnaire that is likely to measure the most important determinants and sub-determinants of XTC testing."
#        assertion:
#          -
#            id: testing_xtc_is_reasoned
#            source: phd_peters_2008
#            label: "Previous research indicates that getting one's ecstasy tested (or not) is largely a reasoned behavior"
#          -
#            id: there_is_qualitative_research_about_testing
#            source: phd_peters_2008
#            label: "There exists qualitative research about why people get their ecstasy tested"
#      -
#        id: limited_time
#        label: "We have to finish this study before the end of 2019."
#        assertion:
#          -
#            id: project_deadline
#            label: "The deadline for this project is december 2019."
#            source:
#              id: project_proposal
#              label: "The original proposal for this project as funded by our funder, see the document."
#              year: 2017
#  ---

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  ---
#  decision:
#    id: study_sample_size
#    type: study_sample_size
#  
#    value: 400
#  
#    label: "We aim to recruit around 400 participants."
#  
#    justification:
#      -
#        id: aipe_for_correlations
#        label: "To estimate a correlation accurately, you need ~ 400 participants."
#        assertion:
#          -
#            id: nice_round_number
#            label: "We want to recruit a nice round number of participants."
#            source:
#              id: nice_round_numbers_are_nice
#              year: 219
#              label: "In our meeting of 2019-07-05, we agreed that all team members like nice round numbers."
#              evidence_type: "team opinion"
#          -
#            id: exact_aipe_for_our_study
#            label: "Table 1 shows that 383 participants are sufficient even for correlations as low as .05."
#            source:
#              id: moinester_gottfried_2014
#              year: 2014
#              label: "Moinester, M., & Gottfried, R. (2014). Sample size estimation for correlations with pre-specified confidence interval. The Quantitative Methods of Psychology, 10(2), 124–130."
#              evidence_type: "theory"
#  ---

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  ---
#  decision:
#    id: preregistration
#    type: preregistration
#  
#    value: false
#  
#    label: "We will not preregister this study."
#  
#    justification:
#      -
#        id: low_risk_of_bias
#        label: "Because we do not test a specific hypothesis but simply want to know how relevant the different determinants are in this population, the need to preregister is less pressing."
#        assertion:
#          -
#            id: prereg_to_decrease_bias
#            label: "Preregistration_decreases_bias"
#            source:
#              id: example_source
#              label: "Not online now, but a study making this point would be nice here"
#              evidence_type: "empirical study"
#      -
#        id: no_time_for_prereg
#        label: "We have no time to complete a preregistration form."
#        assertion:
#          id: project_deadline
#  ---

## ----reading-justifications---------------------------------------------------
examplePath <- file.path(system.file(package="justifier"), 'extdata');

res <-
  justifier::load_justifications(file=file.path(examplePath,
                                                "study-example.jmd"),
                                 silent=TRUE);

## ----decision-scores----------------------------------------------------------
knitr::kable(res$fwApplications$`justifier-example-study-framework-specification`$scoreDf);

## ----decision-graph-1, fig.width=9--------------------------------------------
plot(res$decisionGraphs$research_question);

## ----decision-graph-2, fig.width=9--------------------------------------------
plot(res$decisionGraphs$study_sample_size);

