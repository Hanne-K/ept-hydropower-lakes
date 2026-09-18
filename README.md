# ept-hydropower-lakes

R code and data analyzing EPT (Ephemeroptera, Plecoptera, Trichoptera) benthic macroinvertebrate community responses to lake hydropower regulation in Norway.

> **Paper Title:** *Hydropower regulation reshapes littoral invertebrate communities: functional traits reveal impacts beyond richness and abundance*\
> **Authors:** *Krogstie, Hanne Bjørnås; Solvåg, Christianne Dalsbotten; Ratikainen, Irja Ida; Kjærstad, Gaute; Finstad, Anders Gravbrøt*\
> **Journal/Preprint:** *Accepted to Aquatic Sciences* (2026)\
> **DOI:** *10.1007/s00027-026-01367-3*

## Getting Started

### Prerequisites
The analysis was built using **R (version 4.2.2)**. The core analysis relies on the R packages found in the script code/1_setup.R

### Installation & Replication
1. **Clone or download** this repository:
   ```bash
   git clone https://github.com[your-username]/ept-hydropower-norway.git
   ```
2. Open `ept-hydropower-norway.Rproj` in RStudio. This ensures all file paths work seamlessly via relative paths.
3. Open and run `notebooks/main.qmd` (or run scripts chronologically from the `R/` directory).

## Data Availability
* **`data/source_data/armour/InvertTraitsTable_v1.txt`**: Contains trait data sourced from the  U.S. Geological Survey Data Series 187.
* **`data/source_data/hydropower_reservoirs_NVE`**: Contains data and metadata on Norwegian reservoirs, sourced from The Norwegian Water Resources and Energy Directorate (NVE).
* **`data/source_data/locomotion/locomotion_EPT_freshwaterecologyinfo.csv`**: Contains trait data sourced from www.freshwaterecology.info .
* **`data/source_data/occurrences/occurrences_df_v2.rda`**: Contains species occurrence data for benthic macroinvertebrates of the orders Ephemeroptera, Plecoptera, and Trichoptera from the studied Norwegian lakes.
* **`data/source_data/occurrences/occurrences_highertaxa_df_v2.rda`**: Contains order-level occurrence data for the orders Ephemeroptera, Plecoptera, and Trichoptera from the studied Norwegian lakes.
* **`data/source_data/temperature/temperature_data.rda`**: Contains water temperature data from the studied Norwegian lakes.
* **`data/source_data/waterlevels/GPS_waterlevels.rda`**: Contains water level data from the studied Norwegian control lakes.
* **`data/source_data/waterlevels/Sildre_NVE`**: Contains water level data from the studied Norwegian regulated lakes Gjøljavatnet and Stor-Drakstsjøen, sourced from The Norwegian Water Resources and Energy Directorate (NVE).

## License & Citation

-   **Code:** The R scripts and workflows in this repository are licensed under the [MIT License](LICENSE).

-   **Data:** The occurrences dataset contained in the `data/occurrences` directory, the water temperature dataset contained in the `data/temperature` directory, and the water level dataset contained in the `data/waterlevels.rda` file are licensed under a [Creative Commons Attribution 4.0 International License (CC-BY 4.0)](https://creativecommons.org).

-   **Data:** The trait dataset contained in the `data/locomotion` directory was obtained from www.freshwaterecology.info, the taxa and autecology database for freshwater organisms, version 8.0 (accessed October 8, 2025). The use of these data is subject to the terms and conditions of freshwaterecology.info. The database should be cited as Schmidt-Kloiber & Hering (2015), with additional citations to the original sources specified by freshwaterecology.info for the relevant taxonomic groups and ecological parameters.

-   **Data:** The trait dataset contained in the `data/armour/InvertTraitsTable_v1.txt` file were obtained from Vieira et al. (2006), A database of lotic invertebrate traits for North America, U.S. Geological Survey Data Series 187 (Vieira et al. 2006). The data are in the U.S. public domain. The original source and authors are acknowledged and cited here.

-   **Data:** The water level datasets contained in the `data/waterlevels/Sildre_NVE`directory, were obtained from Sildre, a web portal by The Norwegian Water Resources and Energy Directorate (NVE), on February 5, 2025. The data are licensed under the Norwegian Licence for Open Government Data (NLOD), compatible with CC BY 3.0 Norway. Source: https://sildre.nve.no/

-   **Data:** The dataset on Norwegian reservoirs contained in the `data/hydropower_reservoirs_NVE` directory, were obtained from The Norwegian Water Resources and Energy Directorate (NVE), on June 3, 2023. The data are licensed under the Norwegian Licence for Open Government Data (NLOD), compatible with CC BY 3.0 Norway. Source: https://nedlasting.nve.no/gis/

**References**

Schmidt-Kloiber A, Hering D (2015) www.freshwaterecology.info - an online tool that unifies, standardises and codifies more than 20,000 European freshwater organisms and their ecological preferences. Ecol Indic. https://doi.org/10.1016/j.ecolind.2015.02.007

Vieira, N. K. M., Poff, N. L., Carlisle, D. M., Moulton, S. R., II, Koski, M. L., & Kondratieff, B. C. (2006). A database of lotic invertebrate traits for North America. U.S. Geological Survey Data Series 187. https://pubs.usgs.gov/ds/ds187/



### How to Cite

If you use the code or data from this repository, please cite our publication: \> *[DOI 10.1007/s00027-026-01367-3 / Zenodo DOI here once generated]*

## Contact

For questions regarding the data or code, please contact **Hanne B. Krogstie** at `hanne.krogstie@hotmail.com`.
