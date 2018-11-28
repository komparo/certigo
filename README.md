
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Build
Status](https://api.travis-ci.org/komparo/certigo.svg)](https://travis-ci.org/komparo/certigo)
![Lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)

# Certigo

A simple but powerful workflow manager.

  - Reproducible: Remembers what scripts, environments and seeds were
    used to produce an output. One little change triggers a rerun
  - Incremental: Won’t rerun if the input did not change, even if some
    earlier steps in the workflow were rerun
  - Output validation: Checks whether the output is correct using
    standard or custom formats
  - Modular: Build a complex workflow from self-contained modules. Each
    module is tested separately.
  - Easy development and debugging: Copies over a command that
    immediately enters the environment and script
  - Resumable: Remembers the jobs that were started previously, and the
    workflow can thus be resumed at any point
