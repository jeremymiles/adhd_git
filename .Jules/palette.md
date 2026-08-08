## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).

## 2024-05-24 - Dynamic Alt Text in Plot Loops
**Learning:** When generating multiple plots in a loop within Quarto documents, static alt text leads to ambiguous screen reader experiences as all images sound identical despite showing different data subsets.
**Action:** Dynamically generate the alt text in labs(alt = ...) (e.g., using paste()) to reflect the specific iteration's data, ensuring each plot is uniquely identifiable.
