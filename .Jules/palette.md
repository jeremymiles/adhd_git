## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-30 - Dynamic Alt Text for Plots in Loops
**Learning:** Generating plots in a loop without dynamic alt text results in identical descriptions for different data views, reducing accessibility. Additionally, iterating over non-unique dataframe columns causes redundant processing.
**Action:** Always use unique() on the loop iterator to prevent duplicate plot generation and dynamically generate alt text within labs(alt = ...) using paste() to provide context-specific descriptions for screen readers.
