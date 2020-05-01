library(tidyverse)
library(lme4)

#Step 1: Functions for different probability models of acceptance ------

generalizations_evaluated_together <- function(data) {
  #Equations to be solved
  #ungrammatical under both generalizations
  #prob_yes_for_disharmonic =
  #      1 - (prob_no_if_ungrammatical * prob_no_if_ungrammatical)
  #log(prob_no_if_ungrammatical) = log(1-prob_yes_for_disharmonic) / 2
  prob_no_if_ungrammatical <-
    exp(log(1 - data$Mean_Yes[data$TYPE == "Disharmony"]) / 2)

  #grammatical under the voicing generalization
  #prob_yes_for_onlyVoice =
  #      1 - (prob_no_if_gramm_by_voice * prob_no_if_ungrammatical)
  #log(prob_no_if_gramm_by_voice) + log(prob_no_if_ungrammatical) =
  #      log(1 - prob_yes_for_onlyVoice)
  prob_no_if_gramm_by_voice <-
    exp(log(1 - data$Mean_Yes[data$TYPE == "OnlyVoicing"]) -
          log(prob_no_if_ungrammatical))

  #grammatical under the continuancy generalization
  #prob_yes_for_onlyCont =
  #      1 - (prob_no_if_ungrammatical * prob_no_if_gramm_by_cont)
  #log(prob_no_if_gramm_by_cont) + log(prob_no_if_ungrammatical) =
  #      log(1 - prob_yes_for_onlyCont)
  prob_no_if_gramm_by_cont <-
    exp(log(1 - data$Mean_Yes[data$TYPE == "OnlyCont"]) -
          log(prob_no_if_ungrammatical))

  #Get the yes probabilities
  prob_yes_if_ungrammatical <- 1 - prob_no_if_ungrammatical
  prob_yes_if_gramm_by_voice <- 1 - prob_no_if_gramm_by_voice
  prob_yes_if_gramm_by_cont <- 1 - prob_no_if_gramm_by_cont

  #Generate probabilities from above model
  #ungrammatical under both generalizations
  prob_yes_for_disharmonic <- 1 - (prob_no_if_ungrammatical *
                                     prob_no_if_ungrammatical)
  #grammatical under the voicing generalization
  prob_yes_for_onlyVoice <- 1 - (prob_no_if_ungrammatical *
                                   prob_no_if_gramm_by_voice)
  #grammatical under the continuancy generalization
  prob_yes_for_onlyCont <- 1 - (prob_no_if_ungrammatical *
                                  prob_no_if_gramm_by_cont)
  #grammatical under both simple generalizations
  prob_yes_for_newStim <- 1 - (prob_no_if_gramm_by_voice *
                                 prob_no_if_gramm_by_cont)

  #Return the probabilities that a participant says a word is
  #acceptable, depending on the nature of the word
  data.frame(prob_yes_for_disharmonic=prob_yes_for_disharmonic,
             prob_yes_for_onlyVoice=prob_yes_for_onlyVoice,
             prob_yes_for_onlyCont=prob_yes_for_onlyCont,
             prob_yes_for_newStim=prob_yes_for_newStim)
}

generalizations_evaluated_one_at_a_time <- function(data) {
  #Equations to be solved
  #ungrammatical under both generalizations
  #prob_yes_for_disharmonic =
  #      ((1 - prob_no_if_ungrammatical) +
  #       (1 - prob_no_if_ungrammatical)) / 2
  prob_no_if_ungrammatical <-
    (1 - data$Mean_Yes[data$TYPE == "Disharmony"])

  #grammatical under the voicing generalization
  #prob_yes_for_onlyVoice =
  #      ((1 - prob_no_if_gramm_by_voice) +
  #       (1 - prob_no_if_ungrammatical)) / 2
  #2 * prob_yes_for_onlyVoice =
  #      2 - prob_no_if_gramm_by_voice - prob_no_if_ungrammatical
  #prob_no_if_gramm_by_voice =
  #      2 - 2 * prob_yes_for_onlyVoice - prob_no_if_ungrammatical
  prob_no_if_gramm_by_voice <- 2 -
    2 * data$Mean_Yes[data$TYPE == "OnlyVoicing"] -
    prob_no_if_ungrammatical

  #grammatical under the continuancy generalization
  #prob_yes_for_onlyCont =
  #      ((1 - prob_no_if_gramm_by_cont) +
  #       (1 - prob_no_if_ungrammatical)) / 2
  #2 * prob_yes_for_onlyCont =
  #      2 - prob_no_if_gramm_by_cont - prob_no_if_ungrammatical
  #prob_no_if_gramm_by_cont =
  #      2 - 2 * prob_yes_for_onlyCont - prob_no_if_ungrammatical
  prob_no_if_gramm_by_cont <- 2 -
    2 * data$Mean_Yes[data$TYPE == "OnlyCont"] -
    prob_no_if_ungrammatical

  #Get the yes probabilities
  prob_yes_if_ungrammatical <- 1 - prob_no_if_ungrammatical
  prob_yes_if_gramm_by_voice <- 1 - prob_no_if_gramm_by_voice
  prob_yes_if_gramm_by_cont <- 1 - prob_no_if_gramm_by_cont

  #Generating probabilities from above model
  #ungrammatical under both generalizations
  prob_yes_for_disharmonic <- ((1 - prob_no_if_ungrammatical) +
                                 (1 - prob_no_if_ungrammatical)) / 2
  #grammatical under the voicing generalization
  prob_yes_for_onlyVoice <- ((1 - prob_no_if_gramm_by_voice) +
                               (1 - prob_no_if_ungrammatical)) / 2
  #grammatical under the continuancy generalization
  prob_yes_for_onlyCont <- ((1 - prob_no_if_gramm_by_cont) +
                              (1 - prob_no_if_ungrammatical)) / 2
  #grammatical under both simple generalizations
  prob_yes_for_newStim <- ((1 - prob_no_if_gramm_by_voice) +
                             (1 - prob_no_if_gramm_by_cont)) / 2

  #Return the probabilities that a participant says a word is
  #acceptable, depending on the nature of the word
  data.frame(prob_yes_for_disharmonic=prob_yes_for_disharmonic,
             prob_yes_for_onlyVoice=prob_yes_for_onlyVoice,
             prob_yes_for_onlyCont=prob_yes_for_onlyCont,
             prob_yes_for_newStim=prob_yes_for_newStim)
}

#Step 2: Fit probabilities based on models to observed data ------------

exp2_data_summarized <-
  data.frame(TYPE = c("Disharmony", "OnlyVoicing", "OnlyCont",
                      "NewStims", "OldStims"),
             Mean_Yes = c(0.532, 0.57, 0.614, 0.653, 0.898),
             SE_Yes = c(0.0253, 0.0269, 0.025, 0.0281, 0.0140))

exp3_data_summarized <-
  data.frame(TYPE = c("Disharmony", "OnlyVoicing", "OnlyCont",
                      "NewCStims", "NewWStims", "OldStims"),
             Mean_Yes = c(0.467, 0.563, 0.58, 0.616, 0.824, 0.867),
             SE_Yes = c(0.0336, 0.0307, 0.0266, 0.0302, 0.0239, 0.022))

exp2_evaluated_together_predictions <-
  generalizations_evaluated_together(exp2_data_summarized)

exp2_evaluated_one_at_a_time_predictions <-
  generalizations_evaluated_one_at_a_time(exp2_data_summarized)

exp3_evaluated_together_predictions <-
  generalizations_evaluated_together(exp3_data_summarized)

exp3_evaluated_one_at_a_time_predictions <-
  generalizations_evaluated_one_at_a_time(exp3_data_summarized)

#Step 3: Simulate data and fit logisitic regression to results ---------

#Function for simulating judgment data
simulate_judgments <- function(num_participants,
                               num_items_per_type,
                               probability_model_predictions,
                               conditions) {
  TYPE = rep(conditions, num_items_per_type)
  judgments = data.frame(participant = rep(c(1:num_participants),
                                           each = num_items_per_type *
                                             length(conditions)),
                         TYPE = rep(TYPE, each = num_participants))

  #Simulate acceptability responses
  judgments <- judgments %>%
    #Ensure each row gets a different random number
    rowwise() %>%
    mutate(Random_Number = runif(1)) %>%
    ungroup() %>%
    mutate(response = case_when(
      TYPE == conditions[1] ~ Random_Number <=
        probability_model_predictions$prob_yes_for_disharmonic,
      TYPE == conditions[2] ~ Random_Number <=
        probability_model_predictions$prob_yes_for_onlyVoice,
      TYPE == conditions[3] ~ Random_Number <=
        probability_model_predictions$prob_yes_for_onlyCont,
      TYPE == conditions[4] ~ Random_Number <=
        probability_model_predictions$prob_yes_for_newStim,
      TRUE ~ NA)) %>%
    mutate(response = as.numeric(response))

  judgments
}

#Function for running simulations
run_simulations <- function(num_simulations,
                            seed,
                            num_participants,
                            num_items_per_type,
                            probability_model_predictions,
                            conditions) {
  AllBetaValuesIfNullTrue=NULL
  for(i in 1:num_simulations) {
    if(!is.null(seed)) {
      set.seed(seed * i + i)
    }
    message("Running simulation number ", i)
    sim_data <- simulate_judgments(num_participants,
                                   num_items_per_type,
                                   probability_model_predictions,
                                   conditions) %>%
      #Add coding for model
      mutate(Voicing = case_when(
        TYPE == "NewStims" | TYPE == "NewCStims" |
          TYPE == "OnlyVoicing" ~ 1,
        TRUE ~ 0),
        Stopping = case_when(
          TYPE == "NewStims" | TYPE == "NewCStims" |
            TYPE == "OnlyCont" ~ 1,
          TRUE ~ 0),
        Voicing = factor(Voicing),
        Stopping = factor(Stopping))

    message("Fitting model for simulation number ", i)
    model <- glmer(data = sim_data,
                   response ~ Voicing * Stopping + (1|participant),
                   family = binomial())

    #Get coefficients from model
    AllBetaValuesIfNullTrue <- rbind(AllBetaValuesIfNullTrue,
                                     coef(model)$participant[1, ])
  }
  AllBetaValuesIfNullTrue
}

#Run two different experiment 2 simulations
#Note that seed can be set to NULL or some other number for different
#simulations
exp2_evaluated_together_simulated_betas_1 <-
  run_simulations(num_simulations = 1000,
                  seed = 1234,
                  num_participants = 63,
                  num_items_per_type = 12,
                  probability_model_predictions =
                    exp2_evaluated_together_predictions,
                  conditions = c("Disharmony", "OnlyVoicing",
                                 "OnlyCont", "NewStims"))

exp2_evaluated_one_at_a_time_simulated_betas_1 <-
  run_simulations(num_simulations = 1000,
                  seed = 1234,
                  num_participants = 63,
                  num_items_per_type = 12,
                  probability_model_predictions =
                    exp2_evaluated_one_at_a_time_predictions,
                  conditions = c("Disharmony", "OnlyVoicing",
                                 "OnlyCont", "NewStims"))

exp2_evaluated_together_simulated_betas_2 <-
  run_simulations(num_simulations = 1000,
                  seed = 5678,
                  num_participants = 63,
                  num_items_per_type = 12,
                  probability_model_predictions =
                    exp2_evaluated_together_predictions,
                  conditions = c("Disharmony", "OnlyVoicing",
                                 "OnlyCont", "NewStims"))

exp2_evaluated_one_at_a_time_simulated_betas_2 <-
  run_simulations(num_simulations = 1000,
                  seed = 5678,
                  num_participants = 63,
                  num_items_per_type = 12,
                  probability_model_predictions =
                    exp2_evaluated_one_at_a_time_predictions,
                  conditions = c("Disharmony", "OnlyVoicing",
                                 "OnlyCont", "NewStims"))

#Run two different experiment 3 simulations
#Note that seed can be set to NULL or some other number for different
#simulations
exp3_evaluated_together_simulated_betas_1 <-
  run_simulations(num_simulations = 1000,
                  seed = 1234,
                  num_participants = 51,
                  num_items_per_type = 10,
                  probability_model_predictions =
                    exp3_evaluated_together_predictions,
                  conditions = c("Disharmony", "OnlyVoicing",
                                 "OnlyCont", "NewCStims"))

exp3_evaluated_one_at_a_time_simulated_betas_1 <-
  run_simulations(num_simulations = 1000,
                  seed = 1234,
                  num_participants = 51,
                  num_items_per_type = 10,
                  probability_model_predictions =
                    exp3_evaluated_one_at_a_time_predictions,
                  conditions = c("Disharmony", "OnlyVoicing",
                                 "OnlyCont", "NewCStims"))

exp3_evaluated_together_simulated_betas_2 <-
  run_simulations(num_simulations = 1000,
                  seed = 5678,
                  num_participants = 51,
                  num_items_per_type = 10,
                  probability_model_predictions =
                    exp3_evaluated_together_predictions,
                  conditions = c("Disharmony", "OnlyVoicing",
                                 "OnlyCont", "NewCStims"))

exp3_evaluated_one_at_a_time_simulated_betas_2 <-
  run_simulations(num_simulations = 10000,
                  seed = 5678,
                  num_participants = 51,
                  num_items_per_type = 10,
                  probability_model_predictions =
                    exp3_evaluated_one_at_a_time_predictions,
                  conditions = c("Disharmony", "OnlyVoicing",
                                 "OnlyCont", "NewCStims"))

#Step 4: Get interaction coefficient from actual data ------------------

#These are the interaction terms from models of the actual data
exp2_interaction_term <- 0.0147592
exp3_interaction_term <- -0.2890619

#Step 5: Actual interaction term further away from simulated mean? -----

get_prop_further <- function(sim_betas,
                             actual_interaction,
                             alternative = "two.sided") {
  #What proportion of interaction terms for the data based on the null
  #models are further away than the actual experimental results?
  if(alternative == "one.sided") {
    prop <- sum(sim_betas[, c("Voicing1:Stopping1")] >
                  actual_interaction) / NROW(sim_betas)
  }
  #What proportion of interaction terms from the simulated data based on
  #the null probability models are further away from the mean simulated
  #interaction term than the interaction term from the actual
  #experimental results?
  else if(alternative == "two.sided") {
    centered_sim_interaction_terms <-
      scale(sim_betas[, c("Voicing1:Stopping1")],
            scale = FALSE) %>%
      as.vector()
    centered_actual_interaction_term <- actual_interaction -
      mean(sim_betas[, c("Voicing1:Stopping1")])
    prop <- sum(abs(centered_sim_interaction_terms) >
                  abs(centered_actual_interaction_term)) /
      NROW(sim_betas)
  } else {
    stop("alternative argument must be 'one.sided' or 'two.sided'")
  }
  prop
}

exp2_sim_results <-
  data.frame(
    ProbabilityModel =
      c(rep("All generalizations evaluated together", 2),
        rep("One generalization evaluated at a time", 2)),
    Simulation = rep(c("First", "Second"), 2),
    PropSimulatedInteractionsFurtherAway =
      c(get_prop_further(exp2_evaluated_together_simulated_betas_1,
                         exp2_interaction_term),
        get_prop_further(exp2_evaluated_together_simulated_betas_2,
                         exp2_interaction_term),
        get_prop_further(exp2_evaluated_one_at_a_time_simulated_betas_1,
                         exp2_interaction_term),
        get_prop_further(exp2_evaluated_one_at_a_time_simulated_betas_2,
                         exp2_interaction_term)))

exp3_sim_results <-
  data.frame(
    ProbabilityModel =
      c(rep("All generalizations evaluated together", 2),
        rep("One generalization evaluated at a time", 2)),
    Simulation = rep(c("First", "Second"), 2),
    PropSimulatedInteractionsFurtherAway =
      c(get_prop_further(exp3_evaluated_together_simulated_betas_1,
                         exp3_interaction_term),
        get_prop_further(exp3_evaluated_together_simulated_betas_2,
                         exp3_interaction_term),
        get_prop_further(exp3_evaluated_one_at_a_time_simulated_betas_1,
                         exp3_interaction_term),
        get_prop_further(exp3_evaluated_one_at_a_time_simulated_betas_2,
                         exp3_interaction_term)))

#Table 5 in paper
print(exp2_sim_results)
#Table 8 in paper
print(exp3_sim_results)