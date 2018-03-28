# Fact checking in specific domain

## Problem formulation

Given sentence that features information about Actor taking part in Movie, output True / False and indicate which movie actor was in.

## Existing solutions

- based on unsupervised text matching https://arxiv.org/abs/1802.07398 / http://www.fakenewschallenge.org/#fnc-i-results / http://ranger.uta.edu/~cli/pubs/2015/claimbuster-cj15-hassan.pdf
- knowledge based https://arxiv.org/pdf/1708.07239.pdf
- rule based (probably, not much publications specific to fact checking) / IR kind approaches

## Possible datasets

Possible datasets are identified and prepared in [1-fact_check_ideation.ipynb](1-fact_check_ideation.ipynb).

## Challenges

- noisy data (always an excuse)
- lots of possible ways of phrasing same things
- phrases taken out of context are harder to analyze (pronouns, text about Elizabeth Taylor referencing to her as Liz, etc)

## Metrics

We are supposed to get accuracy / recall / F1.

Further work in [1-fact_check_ideation.ipynb](1-fact_check_ideation.ipynb)