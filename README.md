# ept-hydropower-lakes

R code and data analyzing EPT (Ephemeroptera, Plecoptera, Trichoptera) benthic macroinvertebrate community responses to lake hydropower regulation in Norway.

> **Paper Title:** *Hydropower regulation reshapes littoral invertebrate communities: functional traits reveal impacts beyond richness and abundance*\
> **Authors:** *Krogstie, Hanne Bjørnås; Solvåg, Christianne Dalsbotten; Ratikainen, Irja Ida; Kjærstad, Gaute; Finstad, Anders Gravbrøt*\
> **Journal/Preprint:** *[Journal Name or BioRxiv/EcoEvoRxiv link]* (Year)\
> **DOI:** *10.1007/s00027-026-01367-3*

## Getting Started

### Prerequisites
The analysis was built using **R (version 4.x)**. The core analysis relies on the following R packages:
* `tidyverse` (Data manipulation and plotting)
* `vegan` (Community ecology analysis and ordinations)
* `lme4` or `brms` (Statistical modeling - *adjust as needed*)

### Installation & Replication
1. **Clone or download** this repository:
   ```bash
   git clone https://github.com[your-username]/ept-hydropower-norway.git
   ```
2. Open `ept-hydropower-norway.Rproj` in RStudio. This ensures all file paths work seamlessly via relative paths.
3. Open and run `notebooks/main_analysis.Rmd` (or run scripts chronologically from the `R/` directory).

## Data Availability
* **`data/raw_biomonitoring_data.csv`**: Contains benthic macroinvertebrate counts across the studied Norwegian lakes.
* **`data/hydropower_metrics.csv`**: Environmental and regulation variables (e.g., water level fluctuations, flushing rates).

## License & Citation

-   **Code:** The R scripts and workflows in this repository are licensed under the [MIT License](LICENSE).
-   **Data:** The occurrences dataset contained in the `data/` directory are licensed under a [Creative Commons Attribution 4.0 International License (CC-BY 4.0)](https://creativecommons.org).

### How to Cite

If you use the code or data from this repository, please cite our publication: \> *[Insert your paper citation / Zenodo DOI here once generated]*

## Contact

For questions regarding the data or code, please contact **[Your Name]** at `your.email@institution.no`.
