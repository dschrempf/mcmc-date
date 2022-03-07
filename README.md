Estimate node ages of a phylogeny by approximating the phylogenetic likelihood
with a multivariate normal distribution.

Modules containing definitions specific to the analysis:

Modules:

-   **[Definitions](app/Definitions.hs):** Proposals and monitors, configuration.

-   **[Hamiltonian](app/Hamiltonian.hs):** Hamiltonian proposal.

-   **[Main](app/Main.hs):** Functions to prepare the data, run and continue the
    Metropolis-Hasting-Green algorithm.

-   **[Options](app/Options.hs):** Handle command line options.

-   **[Probability](app/Probability.hs):** Prior and likelihood functions.

-   **[State](app/State.hs):** State space. If you try to understand what is going on, or if you
    want to change analysis settings, your starting point should be `State`.

-   **[Tools](app/Tools.hs):** Miscellaneous tools.

