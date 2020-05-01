library(tidyverse)
library(lme4)

load_data <- function(directory) {
  data <- list.files(directory, pattern = "*.csv") %>%
    imap(~ read_csv(file.path(directory, .x)) %>%
           select(SOUND:date) %>%
           rename(originalParticipantNumber = participant) %>%
           mutate(participant = .y)) %>%
    reduce(rbind)
}