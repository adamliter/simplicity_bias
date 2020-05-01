library(here)
source(here("R/simplicity_bias.R"))

exp2_data <- load_data(here("data/exp2")) %>%
  ## Remove non-learners
  filter(!(originalParticipantNumber %in% c(8, 18, 26, 27, 41, 49, 50,
                                            51, 59, 68, 69, 75,
                                            89, 92)),
         !(originalParticipantNumber == 46 & date == "2015_Nov_04_1725")) %>%
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

exp2_data_by_part <- exp2_data %>%
  mutate(rating.rt = as.numeric(rating.rt)) %>%
  group_by(participant, TYPE) %>%
  summarize(Mean_Yes = mean(YesCount, na.rm = TRUE),
            SE_Yes = sd(YesCount, na.rm = TRUE)/sqrt(length(YesCount)),
            Mean_RT = mean(rating.rt, na.rm = TRUE),
            SE_RT = sd(rating.rt, na.rm = TRUE)/sqrt(length(rating.rt))) %>%
  ungroup()

# Figure 3 in the paper
fig_3 <- exp2_data_by_part %>%
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
#print(fig_3)

exp2_data_for_models <- exp2_data %>%
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

exp2_m0 <- exp2_data_for_models %>%
  glmer(YesCount ~ Voicing + Continuancy + (1 | participant) + (1 | STIMULUS),
        data = .,
        family = binomial())
exp2_m1 <- exp2_data_for_models %>%
  glmer(YesCount ~ Voicing * Continuancy + (1 | participant) + (1 | STIMULUS),
        data = .,
        family = binomial())

# Table 4 in paper
#knitr::kable(broom::tidy(exp2_m0))
# This comparison is discussed in note 12 of the paper
#knitr::kable(anova(exp2_m0, exp2_m1))
#knitr::kable(broom::tidy(exp2_m1))

# Figure 4 in paper
exp2_cum_yes <- exp2_data_by_part %>%
  select(participant, TYPE, Mean_Yes) %>%
  pivot_wider(names_from = TYPE,
              values_from = Mean_Yes) %>%
  mutate(VoicePlusCont = Disharmony + (OnlyVoicing - Disharmony) + (OnlyContinuancy - Disharmony)) %>%
  pivot_longer( Disharmony:VoicePlusCont,
                names_to = "TYPE",
                values_to = "Mean_Yes")

exp2_cum_yes_plot <- NULL
exp2_participants <- unique(exp2_cum_yes$participant)
counter <- 1
for(p in exp2_participants) {
  if(p > 1){
    tmp <- exp2_cum_yes %>%
      filter(participant <= p)

    tmp <- tmp %>%
      group_by(TYPE) %>%
      summarize(Mean_Yes = mean(Mean_Yes)) %>%
      mutate(NumofSub = counter)

    counter <- counter + 1
    exp2_cum_yes_plot <- rbind(exp2_cum_yes_plot,tmp)
  }
}
fig_4 <- exp2_cum_yes_plot %>%
  ggplot(aes(NumofSub, Mean_Yes, group = TYPE, color = TYPE))+
    geom_point() +
  geom_smooth(method = "loess") +
  xlab("number of participants")+
  ylab("proportion of 'yes' responses")
#print(fig_4)

# Figure 5 in paper
exp2_voice_cont_tradeoff = exp2_data_by_part %>%
  filter(TYPE %in% c("OnlyVoicing", "OnlyContinuancy", "Disharmony")) %>%
  select(participant, Mean_Yes, TYPE) %>%
  pivot_wider(names_from = "TYPE", values_from = "Mean_Yes")

exp2_tradeoff_m0 <- lm(data = exp2_voice_cont_tradeoff,
                       OnlyContinuancy ~ OnlyVoicing)

fig_5 <- exp2_voice_cont_tradeoff %>%
  ggplot(aes(OnlyVoicing, OnlyContinuancy)) +
  geom_jitter(width = 0.025) +
  stat_smooth(method = "lm") +
  xlab("proportion of 'Yes' responses to OnlyVoicing") +
  ylab("proportion of 'Yes' responses to OnlyContinuancy") +
  geom_label(aes(label = paste0(
    "β=",
    round(summary(exp2_tradeoff_m0)$coefficients[2, 1], 3),
    ", p<0.00001"),
                 x = 0.9,
                 y = 1.1))
#print(fig_5)