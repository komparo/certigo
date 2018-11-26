
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Build
Status](https://api.travis-ci.org/komparo/certigo.svg)](https://travis-ci.org/komparo/certigo)
![Lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)

# Certigo

A simple but powerful workflow manager.

  - Reproducible: Remembers what scripts and environments were used to
    produce an output. One little change triggers a rerun
  - Incremental: Won’t rerun if the input did not change, even if some
    earlier steps in the workflow were rerun
  - Output validation: Check whether the output is correct using
    standard or custom formats
  - Modular: Build a complex workflow from self-contained modules. Each
    module is self-contained and can be tested independently
  - Easy debugging: Copies over a command that will immediately enter
    the script
