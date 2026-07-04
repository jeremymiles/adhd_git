## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-07-04 - Dynamic alt text for iterated visualisations
**Learning:** When generating multiple data visualizations within a loop, static alt text causes screen readers to announce the same description for different plots, depriving users of crucial context.
**Action:** Always dynamically generate alt text within the loop (e.g., using paste() in the labs() function) to ensure each visualization has a unique, descriptive label reflecting its specific data subset.
