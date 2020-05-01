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

<!-- links -->
[git-lfs]: https://git-lfs.github.com/
[git-hooks]: https://git-scm.com/book/en/v2/Customizing-Git-Git-Hooks
