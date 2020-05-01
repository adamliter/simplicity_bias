library(here)
source(here("R/simplicity_bias.R"))

exp3_data <- load_data(here("data/exp3")) %>%
  ## There were no non-learners
  ## Select only the test trials
  filter(GENERAL_TYPE == "Test" | GENERAL_TYPE == "Controls") %>%
  ## Count yes responses
  mutate(YesCount = ifelse(rating.response == "Yes", 1, 0)) %>%
  ## Reset baseline and use stimuli names from paper
  mutate(TYPE = as_factor(TYPE) %>%
           fct_relevel("NoPatternControls", "VoicingControls",
                       "StopControls", "NewConsTestStimuli",
                       "NewWordTestStimuli", "OldTestStimuli") %>%
           fct_recode("Disharmony" = "NoPatternControls",
                      "OnlyVoicing" = "VoicingControls",
                      "OnlyContinuancy" = "StopControls",
                      "NewCStims" = "NewConsTestStimuli",
                      "NewWStims" = "NewWordTestStimuli",
                      "OldStims" = "OldTestStimuli"))

exp3_data_by_part <- exp3_data %>%
  mutate(rating.rt = as.numeric(rating.rt)) %>%
  group_by(participant, TYPE) %>%
  summarize(Mean_Yes = mean(YesCount, na.rm = TRUE),
            SE_Yes = sd(YesCount, na.rm = TRUE)/sqrt(length(YesCount)),
            Mean_RT = mean(rating.rt, na.rm = TRUE),
            SE_RT = sd(rating.rt, na.rm = TRUE)/sqrt(length(rating.rt))) %>%
  ungroup()

# Figure 6 in the paper
fig_6 <- exp3_data_by_part %>%
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
#print(fig_6)

exp3_data_for_models <- exp3_data %>%
  inner_join(data.frame(
    TYPE = factor(
      c("NewCStims", "Disharmony", "OldStims",
        "OnlyContinuancy", "OnlyVoicing"),
      levels = c("Disharmony", "OnlyVoicing", "OnlyContinuancy",
                 "NewCStims", "OldStims")),
    Voicing = c(1, 0, 1, 0, 1),
    Continuancy = c(1, 0, 1, 1, 0)),
    by = "TYPE") %>%
  filter(TYPE != "OldStims")

exp3_m0 <- exp3_data_for_models %>%
  glmer(YesCount ~ Voicing + Continuancy + (1 | participant) + (1 | STIMULUS),
        data = .,
        family = binomial())
exp3_m1 <- exp3_data_for_models %>%
  glmer(YesCount ~ Voicing * Continuancy + (1 | participant) + (1 | STIMULUS),
        data = .,
        family = binomial())

# Table 6 in paper
#knitr::kable(broom::tidy(exp3_m0))
# This comparison is discussed in note 12 of the paper
#knitr::kable(anova(exp3_m0, exp3_m1))
#knitr::kable(broom::tidy(exp3_m1))

exp3_newstims_model <- exp3_data %>%
  filter(TYPE %in% c("NewCStims", "NewWStims")) %>%
  glmer(YesCount ~ TYPE + (1 | participant) + (1 | STIMULUS),
        data = .,
        family = binomial())

# Table 7 in paper
knitr::kable(broom::tidy(exp3_newstims_model))

# Figure 7 in paper
exp3_voice_cont_tradeoff = exp3_data_by_part %>%
  filter(TYPE %in% c("OnlyVoicing", "OnlyContinuancy", "Disharmony")) %>%
  select(participant, Mean_Yes, TYPE) %>%
  pivot_wider(names_from = "TYPE", values_from = "Mean_Yes")

exp3_tradeoff_m0 <- lm(data = exp3_voice_cont_tradeoff,
                       OnlyContinuancy ~ OnlyVoicing)

fig_7 <- exp3_voice_cont_tradeoff %>%
  ggplot(aes(OnlyVoicing, OnlyContinuancy)) +
  geom_jitter(width = 0.025) +
  stat_smooth(method = "lm") +
  xlab("proportion of 'Yes' responses to OnlyVoicing") +
  ylab("proportion of 'Yes' responses to OnlyContinuancy") +
  geom_label(aes(label = paste0(
    "β=",
    round(summary(exp3_tradeoff_m0)$coefficients[2, 1], 3),
    ", p<0.00001"),
    x = 0.9,
    y = 1.1))
#print(fig_7)