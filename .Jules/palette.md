## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-07-08 - Dynamically generated alt text for ggplot2 in loops
**Learning:** When generating multiple ggplot2 visualizations within a loop in R or Quarto, a static alt text creates an inaccessible experience for screen reader users as they cannot distinguish between the different plots. Furthermore, looping over a dataframe column without unique() causes redundant processing.
**Action:** Always dynamically generate the alt text within the labs(alt = ...) function (e.g., using paste()) to provide context-specific descriptions, and always use unique() on the loop iterator to prevent redundant generation of identical plots.
