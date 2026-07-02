# Two-Mode Relational Similarities

This repository contains the complete replication materials—data, code, manuscripts, and presentation slides—for the paper **"Two-mode relational similarities"** by Omar Lizardo.

This repository reproduces the paper introducing two-mode extensions of Kovacs generalized relational similarity approach, applying it to Davis Southern Women data and the impressionist artist network.

---

## 📂 Repository Structure

The project directory is structured as follows:

```text
├── analysis.qmd                    # Main reproducible Quarto notebook (primary entry point)
├── Functions/                      # Custom mathematical models and helper scripts
├── Plots/                          # Directory where all manuscript figures are saved
├── Tabs/                           # Directory where all reproduced tables are saved
├── main.tex                        # Main LaTeX manuscript
├── cas-refs.bib                    # Bibliography file
└── renv.lock                       # renv lockfile for exact package versions
```

---

## 🛠️ Prerequisites & Installation

To run the reproducibility workflow, you will need **R** and the **Quarto** CLI (pre-installed in Positron and RStudio).

### 1. Required R Packages
Ensure you have the required R packages installed. You can install them by running the following command in your R console:

```R
renv::restore()
```

This project uses `renv`. Use `renv::restore()` to install dependencies.

---

## 🚀 How to Reproduce the Findings

1. **Clone the Repository**: Clone this repository to your local machine using git or download it as a ZIP file.
2. **Open the Project**: Open the `.Rproj` or `.R` file in your editor (e.g., RStudio or Positron). This ensures paths are resolved correctly relative to the project root.
3. **Install Dependencies**: Ensure the packages listed above are installed.
4. **Run the Computational Pipeline**:
   * **Using the Command Line (Quarto CLI)**:
     ```bash
     quarto render analysis.qmd
     ```
   * **Using R**:
     ```R
     quarto::quarto_render("analysis.qmd")
     ```
   * Or run interactively in your IDE. This step ensures that all tables and figures are updated directly from the code, guaranteeing that the numbers in the paper are exactly what the code computes.

---

## 📝 License & Citation

If you use the materials or code in this repository, please cite the paper:

```bibtex
@article{lizardo2023two,
  title={Two-mode relational similarities},
  author={Lizardo, Omar},
  journal={Social Networks},
  volume={76},
  pages={34--41},
  year={2023},
  publisher={Elsevier}
}
```
