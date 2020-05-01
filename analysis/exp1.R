library(here)
source(here("R/simplicity_bias.R"))

exp1_data <- load_data(here("data/exp1")) %>%
  ## Remove non-learners
  filter(!(originalParticipantNumber %in% c(12,32))) %>%
  ## Select only the test trials
  filter(GENERAL_TYPE == "Test" | GENERAL_TYPE == "Controls") %>%
  ## Count yes responses
  mutate(YesCount = ifelse(rating.response == "Yes", 1, 0)) %>%
  ## Reset baseline and use stimuli names from paper
  mutate(TYPE = as_factor(TYPE) %>%
           fct_relevel("NoPatternControls", "VoicingControls",
                       "StopControls", "NewTestStimuli",
                       "OldTestStimuli") %>%
           fct_recode("Disharmony" = "NoPatternControls",
                      "OnlyVoicing" = "VoicingControls",
                      "OnlyContinuancy" = "StopControls",
                      "NewStims" = "NewTestStimuli",
                      "OldStims" = "OldTestStimuli"))

exp1_data_by_part <- exp1_data %>%
  group_by(participant, TYPE) %>%
  summarize(Mean_Yes = mean(YesCount, na.rm = TRUE),
            SE_Yes = sd(YesCount, na.rm = TRUE)/sqrt(length(YesCount)),
            Mean_RT = mean(rating.rt, na.rm = TRUE),
            SE_RT = sd(rating.rt, na.rm = TRUE)/sqrt(length(rating.rt))) %>%
  ungroup()

# Figure 2 in the paper
fig_2 <- exp1_data_by_part %>%
  group_by(TYPE) %>%
  summarize(SE_Yes = sd(Mean_Yes)/sqrt(length(Mean_Yes)),
            Mean_Yes = mean(Mean_Yes),
            SE_RT = sd(Mean_RT)/sqrt(length(Mean_RT)),
            Mean_RT = mean(Mean_RT)) %>%
  ungroup() %>%
  ggplot(aes(x = TYPE, y = Mean_Yes)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Mean_Yes - SE_Yes,
                    ymax = Mean_Yes + SE_Yes,
                    width = 0.25)) +
  ylab("proportion of 'yes' responses") +
  xlab("") +
  coord_cartesian(ylim = c(0.4, 0.95))
#print(fig_2)

exp1_data_for_models <- exp1_data %>%
  inner_join(data.frame(
    TYPE = factor(
      c("NewStims", "Disharmony", "OldStims",
        "OnlyContinuancy", "OnlyVoicing"),
      levels = c("Disharmony", "OnlyVoicing", "OnlyContinuancy",
                 "NewStims", "OldStims")),
    Voicing = c(1, 0, 1, 0, 1),
    Continuancy = c(1, 0, 1, 1, 0)),
    by = "TYPE") %>%
  filter(TYPE != "OldStims")

exp1_m0 <- exp1_data_for_models %>%
  glmer(YesCount ~ Voicing + Continuancy + (1 | participant) + (1 | STIMULUS),
        data = .,
        family = binomial())
exp1_m1 <- exp1_data_for_models %>%
  glmer(YesCount ~ Voicing * Continuancy + (1 | participant) + (1 | STIMULUS),
        data = .,
        family = binomial())

# Table 2 in paper
#knitr::kable(broom::tidy(exp1_m1))
# Table 3 in paper
#knitr::kable(anova(exp1_m0,exp1_m1))