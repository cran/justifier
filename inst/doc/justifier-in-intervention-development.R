## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  ---
#  decision:
#    id: target_behavior_selection
#    type: selection_target_behavior
#  
#    label: "The target behavior is getting ecstasy tested"
#  
#    description: "In the Netherlands, it is possible to deliver an ecstasy pill to a testing centre. These testing centres, usually operated by the prevention department of substance use disorder organisations, a type of health organisation in the Netherlands, then test the pills. The client receives a code and can call the testing service a week later to find out which active ingredients were found in the pill, and in which dose (in milligrams)."
#  
#    date: 2019-09-03
#  
#    justification:
#      id: importance_of_knowing_pill_contents
#      label: "Knowing pill contents is important"
#      description: "Risk of ecstasy use increases as a higher dose is consumed (and adulterants can be toxic)."
#      assertion:
#        id: adulterants_can_be_toxic
#        label: "Adulterated MDMA can contain toxic substances."
#        source:
#          id: expert_interview_1
#          label: "Expert interview in document 'exp_interview_1.docx'."
#          type: "external expert opinion"
#  ---

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  ---
#  source:
#    id: west_development_2019
#    label: "West, R., Godinho, C. A., Bohlen, L. C., Carey, R. N., Hastings, J., Lefevre, C. E., & Michie, S. (2019). Development of a formal system for representing behaviour-change theories. Nature human behaviour, 3(5), 526. doi:10.1038/s41562-019-0561-2"
#    doi: "10.1038/s41562-019-0561-2"
#  ---

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  ---
#  decision:
#    id: sub_behavior_selection
#    type: selection_sub_behavior
#  
#    label: "The sub-behavior 'delivering pill to testing centre' is selected."
#  
#    description: "This is a necessary sub-behavior; without, the target behavior can impossibly be completed."
#  
#    date: 2019-09-03
#  
#    justification:
#      id: justification_only_pills_delivered_to_testing_centres_are_tested
#      label: "Only ecstasy pills delivered to a testing centre are tested."
#      description: "The procedures for testing ecstasy pills prescribe that only pills that are delivered to testing centres are tested."
#      assertion:
#        id: only_pills_delivered_to_testing_centres_are_tested
#        label: "The procedures for testing ecstasy pills prescribe that only pills that are delivered to testing centres are tested."
#        source:
#          id: drug_testing_conditions
#          label: "Conditions for getting drugs tested"
#          url: "https://www.drugs-test.nl/voorwaarden"
#  ---

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  ---
#  decision:
#    id: determinants_forgo_habit
#    type: selection_determinant
#  
#    label: "The determinant 'habit' is not selected."
#  
#    description: "We will not spend resources on targeting 'habit'"
#  
#    date: 2019-09-03
#  
#    justification:
#      id: justification_habit_not_important_for_testing
#      label: "Habit is not relevant."
#      description: "Getting ecstasy tested is an infrequently performed behavior. This means the requirements for developing automatic behavior are not in place."
#      assertion:
#        id: assertion_habit_not_important_for_testing
#        label: "Getting ecstasy tested is an infrequently performed behavior. This means the requirements for developing automatic behavior are not in place."
#        source:
#          id: planning_group_meeting_20190816
#          label: "Minutes of the planning group meeting at the 16th of August 2019."
#  ---

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  ---
#  decision:
#    id: subdeterminants_select_memory
#    type: selection_subdeterminant
#  
#    label: "We will target the expectation that with a higher dose, people remember less."
#  
#    description: "It seems people prefer to remember more of their ecstasy use sessions, while at the same time indicating that if they use a higher dose, they will remember less. This contradiction can be leveraged in an intervention to promote deliberately dosing at a lower intensity."
#  
#    date: 2019-09-03
#  
#    justification:
#      id: justification_subdeterminants_select_memory
#      label: "People simultaneously want to remember more, but realise they remember less if they use a higher dose."
#      description: "This inconsistency can be used in an intervention."
#      assertion:
#        - id: assertion_subdeterminant_high_dose_less_memories
#          label: "People expect to remember less afting using a high dose of ecstasy."
#          source:
#            id: crutzen_using_2017
#            label: "Crutzen, R., Peters, G.-J. Y., & Noijen, J. (2017) Using Confidence Interval-Based Estimation of Relevance to Select Social-Cognitive Determinants for Behavior Change Interventions. Frontiers in Public Health, 5, 165. doi:10.3389/fpubh.2017.00165"
#            doi: "10.3389/fpubh.2017.00165"
#        - id: assertion_subdeterminant_more_memories_preferred
#          label: "People prefer to remember more after having used ecstasy."
#          source:
#            id: partypanel_report_15.1
#            label: "Party Panel report for Party Panel 15.1"
#            spec: "Figure 23, CIBER plot for evaluations"
#            url: "https://partypanel.nl/resultResources/15.1/report.html"
#  ---

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  ---
#  decision:
#    id: bcps_select_persuasivecomm
#    type: selection_bcp
#  
#    label: "We will use persuasive communication combined with self-reevaluation."
#  
#    description: "The idea is to appeal to people's desire to experience the optimal ecstasy trip. Using this premise, we will prompt reflection on the effects of higher doses on one's memory (and other undesirable effects)."
#  
#    date: 2019-09-03
#  
#    justification:
#      - id: justification_bcps_select_persuasivecomm
#        label: "Persuasive communication and self-reevaluation are both BCPs that can target attitudinal beliefs."
#        assertion:
#          - id: assertion_persuasivecomm_can_target_attitude
#            label: "Persuasive communication can target attitudinal beliefs."
#            source:
#              id: kok_taxonomy_2016
#              label: "Kok, G., Gottlieb, N. H., Peters, G.-J. Y., Mullen, P. D., Parcel, G. S, Ruiter, R. A., Fernández, M. E., Markham, C, Bartholomew, L. K. (2016) A taxonomy of behaviour change methods: an Intervention Mapping approach. Health Psychology Review, 10, 297-312. doi:10.1080/17437199.2015.1077155"
#              doi: "10.1080/17437199.2015.1077155"
#          - id: assertion_selfReeval_can_target_attitude
#            label: "Self-reevaluation can be used to target attitudinal beliefs."
#            source: kok_taxonomy_2016
#  
#  ---

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  ---
#  decision:
#    id: application_factsheet
#    type: selection_application
#  
#    label: "We will create a factsheet."
#  
#    description: "The intervention source is considered an authoritative source in the target population, and generally dispenses trusted harm reduction information."
#  
#    date: 2019-09-03
#  
#    justification:
#      - id: justification_application_factsheet
#        label: "The intervention source's rapport with the target population means that a factsheet fits well with the target population's expectations, and they are likely to take the information seriously."
#        assertion:
#          - id: assertion_source_organisation_considered_reliable
#            label: "The organisation that will be the intervention's source is considered reliable by the target population."
#            source:
#              - id: year_report
#                label: "The year report of the organisation that will be the intervention's source"
#  ---

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  ---
#  decision:
#    id: conditions_persuasivecomm
#    type: selection_application
#  
#    label: "Messages need to be relevant and not too discrepant from the beliefs of the individual; can be stimulated by surprise and repetition. Will include arguments."
#  
#    date: 2019-09-03
#  
#    justification:
#      - id: justification_conditions_persuasivecomm
#        label: "The parameters for effectiveness for persuasive communication, as listed in the IM list of methods, are 'Messages need to be relevant and not too discrepant from the beliefs of the individual; can be stimulated by surprise and repetition. Will include arguments.'."
#        assertion:
#          - id: assertion_parameters_persuasivecomm
#            label: "The parameters for effectiveness for persuasive communication are that messages need to be relevant and not too discrepant from the beliefs of the individual; and persuasive communication can be stimulated by surprise and repetition. The message should include one or more arguments."
#            source: kok_taxonomy_2016
#  ---

## ----processing---------------------------------------------------------------

processedJustifications <-
  justifier::load_justifications(here::here("vignettes",
                                            "justifier-in-intervention-development.Rmd"),
                                 silent=TRUE);


## ----decision-tree-example----------------------------------------------------
processedJustifications$decisionTrees$bcps_select_persuasivecomm

## ----decision-graph-example, results="asis"-----------------------------------

cat("\n\n<div style='width:800px'>",
    processedJustifications$decisionGraphsSvg$bcps_select_persuasivecomm,
    "</div>\n\n<div style='width:800px'>",
    processedJustifications$decisionGraphsSvg$subdeterminants_select_memory,
    "</div>\n\n", sep="");


