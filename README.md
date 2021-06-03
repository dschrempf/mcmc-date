Estimate node ages of a phylogeny by approximating the phylogenetic likelihood
with a multivariate normal distribution.

Modules containing definitions specific to the analysis:

-   **[Definitions](src/Definitions.hs):** File names, state space, prior distribution, and the
    likelihood function of the MCMC sampler, as well as the proposals and the
    monitors.

-   **[Calibrations](src/Calibrations.hs) and [Constraints](src/Constraints.hs):** Calibrations of node ages and node order
    constraints.

Other modules:

-   **[Tools](src/Tools.hs):** Miscellaneous tools to prepare the data.

-   **[Main](src/Main.hs):** Functions to prepare the data, run and continue the
    Metropolis-Hasting-Green algorithm.

If you try to understand what is going on, or if you want to change analysis
settings, your starting point should be 'Definitions'.

