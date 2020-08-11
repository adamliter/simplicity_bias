# Simplicity bias

[![DOI](https://zenodo.org/badge/doi/10.1017/S0952675720000093.svg)][doi]

## Overview

This repository contains the materials for the experiments in Durvasula
& Liter (2020) as well as the data files and analysis scripts. If you
make use of the materials in this repository, please cite our paper:

- Durvasula, Karthik & Adam Liter. 2020. There is a simplicity bias when
  generalising from ambiguous data. Phonology 37(2). 177–213. doi:
  [10.1017/S0952675720000093][doi].

Here is a BibTeX entry:

``` text
@article{durvasula2020,
  Author = {Durvasula, Karthik and Liter, Adam},
  Doi = {10.1017/S0952675720000093},
  Journal = {Phonology},
  Number = {2},
  Pages = {177--213},
  Title = {There Is a Simplicity Bias When Generalising from Ambiguous Data},
  Volume = {37},
  Year = {2020}}
```

## Cloning this repository

This repository uses [git LFS][git-lfs] to track the binary `.wav` files
that are included as part of this repository. If you'd like to clone the
repository and actually have the `.wav` files downloaded to your
computer, you'll need to install git LFS.

Once git LFS has been installed, you'll actually need to run one of its
commands called `install`:

```sh
git lfs install
```

This adds some stuff to your global config file (`~/.gitconfig`), which
allows git LFS to intercept actions performed on binary files. (If run
inside of a git repository, this command also installs a pre-push [git
hook][git-hooks] in that repository.)

Then, to clone this repository, run the following command:

```sh
git clone https://gitlab.com/ka-research/simplicity_bias.git
```

If you do not install git LFS, cloning this repository will still work,
but you'll just have placeholder text files in place of where the binary
`.wav` files should be.

## Experiment materials

The experiments reported in our paper were run using PsychoPy (Peirce
[2007][peirce2007]; [2009][peirce2009]). This repository contains the
necessary files for recreating our experiments, should you wish to do
so.

Experiments 1 and 2 were designed and run with version
[`1.82.00`][1-82-00] of PsychoPy. Experiment 3 was designed and run with
version [`1.85.4`][1-85-4]. We make no guarantees that the experiments
will work with other versions of PsychoPy.

The recordings used in our experiments were kindly recorded by [Mina
Hirzel][mina], and she has graciously agreed that we could publicly
share the recordings.

All of the materials are contained in the `materials` directory. The
PsychoPy files rely on relative paths to find the stimuli lists and the
recordings, so if you try to rerun these experiments and wish to modify
the directory structure of the `materials` folder, you will need to
modify the relative paths inside of the `.psyexp` files.

There is a `.psyexp` file for each experiment. When the experiment is
run, it prompts the participant for some demographic information, in
addition to a participant number. Each participant hears a different
randomly generated list of training stimuli and test stimuli, based on
their participant number. The stimuli files that we generated—we did not
use all of the ones that we generated—are included in the directories
`materials/vsh_exp1_stimuli`, `materials/vsh_exp2_stimuli`, and
`materials/vsh_exp3_stimuli`. We've also included the R scripts used to
generate these files in the `R` directory.

## Data

### Experiment 1

For each participant in Experiment 1, there is a CSV file file with
their data in the directory `data/exp1`. In the paper, we report testing
25 native-English-speaking participants; we also tested several
non-native speakers of English because of the manner in which the data
was collected (as extra credit for a class). The data files for these
participants—participants 2, 3, 4, 6, 7, 11, 14, 16, 17, 26, and 27—are
not included in this public repository because they were never going to
be included in the analysis. Furthermore, participant 28 did not
complete the test part of the experiment, so their data is not included
in this public repository, and the data from participants 13 and 21 was
lost due to technical difficulties.

We did, however, decide to exclude 2 of the 25 native speakers of
English from the analysis due to non-learning (see the paper for more
details), and so we've included the data for these 2
participants—participants 12 and 32—are included in this public
repository.

### Experiment 2

For each participant in Experiment 2, there is a CSV file with their
data in the directory `data/exp2`. In the paper, we report testing 78
native-English-speaking participants; we also tested several non-native
speakers of English because of the manner in which the data was
collected (as extra credit for a class). The data files for these
participants—participants 13, 14, 16, 28, 33, 37, 38, 40, 56, 60, 61,
70, 72, 76, 81, 86, 88, 90, and 94—are not included in this public
repository because they were never going to be included in the
analysis. Furthermore, participant 24 accidentally quit the experiment
halfway through and then started over; because they did half of the
experiment twice, their data was not analyzed and so is not included in
this public repository. Moreover, participant 74's data was lost due to
technical difficulties.

Additionally, we excluded 15 of the 78 participants due to non-learning
(see the paper for more details). The data for these participants is
included in this public repository. The non-learners were participants
8, 18, 26, 27, 41, (the second) 46, 49, 50, 51, 59, 68, 69, 75, 89, and
92. (Participant numbers 44, 45, 46, 47, and 48 were accidentally used
twice, so it was the second participant 46—the one who completed the
experiment on November 4—that was a non-learner.)

### Experiment 3

For each participant in Experiment 3, there is a CSV file with their
data in the directory `data/exp3`. In the paper, we report testing 51
native-English-speaking participants; we also tested several non-native
speakers of English because of the manner in which the data was
collected (as extra credit for a class). The data files for these
participants—participants 5, 6, 7, 8, 9, 10, 11, 16, 18, 19, 21, 27, 30,
31, 34, 40, 50, 52, 54, 55, 57, 100, 101, 103, 104, 105, 121, and
122—are not included in this public repository because they were never
going to be included in the analysis.

The participant numbers 1–57 were used in the first several testing
sessions, although the participant who should have been participant 42
changed their participant number to 1, so there are two participants
with the original participant number of "1".

In a subsequent testing session, the participant numbers 100–105 were
used. The data for participant 102 was lost due to technical
difficulties. In yet another subsequent testing session, the participant
numbers 120–137 were used. The data for participant 125 was lost due to
difficulties. The participant numbers 58–99 and 106–119 were never used.

## Analysis scripts

There is an analysis script for each of the Experiments, 1, 2,
and 3. These scripts reproduce the figures, tables, and analyses
reported in our paper.

Moreover, we also report several Monte Carlo simulations (see paper for
details). These simulations can be recreated exactly with the code in
`analysis/mc_simulations.R`. They are recreated exactly due to a random
seed being set; if you'd like to conduct different simulations, change
lines 226, 236, 246, 256, 269, 279, 289, and 299 of the code.


<!-- links -->
[doi]: https://dx.doi.org/10.1017/S0952675720000093
[lingbuzz]: https://ling.auf.net/lingbuzz/005120
[git-lfs]: https://git-lfs.github.com/
[git-hooks]: https://git-scm.com/book/en/v2/Customizing-Git-Git-Hooks
[peirce2007]: https://dx.doi.org/10.1016/j.jneumeth.2006.11.017
[peirce2009]: https://dx.doi.org/10.3389/neuro.11.010.2008
[1-82-00]: https://github.com/psychopy/psychopy/releases/tag/1.82.00
[1-85-4]: https://github.com/psychopy/psychopy/releases/tag/1.85.4
[mina]: https://minahirzel.com/
