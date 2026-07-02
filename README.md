# Two-Mode-Relational-Similarity

Short paper showing a new and efficient way to compute generalized similarities in two-mode networks, based on previous work by Kovacs (2010) using the ideas of duality and the projection of two-mode into one-mode networks introduced by Breiger (1974). 

Kovács, B. (2010). A generalized model of relational similarity. Social Networks, 32(3), 197-211.

Breiger, R. L. (1974). The duality of persons and groups. Social forces, 53(2), 181-190.

## Replication

This repository is set up with `renv` to ensure reproducibility. 

To run the analysis:
1. Open `Two-Mode-Relational-Similarity.Rproj` in RStudio/Positron.
2. Run `renv::restore()` to install all required dependencies.
3. Render `analysis.qmd` to generate the HTML report and the plots (saved in the `Plots/` directory).
