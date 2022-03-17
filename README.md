# Individualized treatment effect was predicted best by modeling baseline risk in interaction with treatment assignment

## Summary
**Objective**: To compare different risk-based methods for optimal prediction
of individualized treatment effects. **Study Design and Setting**: We simulated
RCT data using diverse assumptions for the average treatment effect, a baseline
prognostic index of risk (PI), the shape of its interaction with treatment
(none, linear, quadratic or non-monotonic), and the magnitude of
treatment-related harms (none or constant independent of the PI). We predicted
absolute benefit using: models with a constant relative treatment effect;
stratification in quarters of the PI; models including a linear interaction of
treatment with the PI; models including an interaction of treatment with a
restricted cubic spline (RCS) transformation of the PI; an adaptive approach
using Akaike’s Information Criterion. We evaluated predictive performance using
root mean squared error and measures of discrimination and calibration for
benefit. **Results**: The linear-interaction model and the RCS-interaction
displayed robust performance across many simulation scenarios. The RCS-model
was optimal when quadratic or non-monotonic deviations from a constant
treatment effect were stronger, and when sample size was larger. The adaptive
approach required larger sample sizes. Illustrations in the GUSTO-I trial
confirmed these findings. **Conclusion**: An interaction between baseline risk
and treatment assignment should be considered to improve treatment effect
predictions.

## Replication

To download the code of the simulation study and explore our results run:
```bash
git clone https://github.com/rekkasa/arekkas_HteSimulation_XXXX_2021.git
cd arekkas_HteSimulation_XXXX_2021
```

You can also replicate the study by running:
```bash
make clean
make submission/manuscript.pdf
```
This will start the entire simulation study from scratch and may take a while to complete.
R-package `renv` is used to recreate our R environment, to allow for reproducible research.
To adjust the settings of the simulation edit `code/SimulationScript.R`. You can increase 
or reduce the resources allocated to the task or alter the settings of the simulations
altogether.
