# A geological timescale for bacterial evolution and oxygen adaptation -- an mcmc-date tutorial

Read the tutorial [here](tutorial_goe.md) or download the [pdf version](tutorial_goe.pdf)

## File list

- `tutorial_goe.md` -- markdown version tutorial
- `createpdf.sh` -- a bash script to create a pdf version from the markdown file `tutorial_goe.md`
- `tutorial_goe.pdf` -- pdf version tutorial
- `data/1007_mito_plastid.tree` -- rooted species tree of Bacteria, inferred using 65 marker genes from 1007 genomes
- `data/1007_mito_plastid.tree.unrooted` -- unrooted species tree of Bacteria
- `data/65genes_bac_and_organelles.phylip` -- 65-gene concatenate including genes from the mitochondrial and plastid genomes
- `data/braces.json` -- bracin mitochondria and chloroplasts
- `data/Fossils.csv` -- fossil and geochemical calibrations
- `data/XGBoost.csv` -- fossil calibrations and aerobicity information based on the XGBoost classifier

The source of the data files is the publication [Davín et al. A geological timescale for bacterial evolution and oxygen adaptation (2024)](https://doi.org/10.6084/m9.figshare.23899299)
