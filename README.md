# Simplicity bias

## Overview

This repository contains the materials for the experiments in Durvasula
& Liter (2020) as well as the data files and analysis scripts. If you
make use of the materials in this repository, please cite Durvasula &
Liter (2020).

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
<!-- links -->
[git-lfs]: https://git-lfs.github.com/
[git-hooks]: https://git-scm.com/book/en/v2/Customizing-Git-Git-Hooks
[peirce2007]: https://dx.doi.org/10.1016/j.jneumeth.2006.11.017
[peirce2009]: https://dx.doi.org/10.3389/neuro.11.010.2008
[1-82-00]: https://github.com/psychopy/psychopy/releases/tag/1.82.00
[1-85-4]: https://github.com/psychopy/psychopy/releases/tag/1.85.4
[mina]: https://minahirzel.com/
