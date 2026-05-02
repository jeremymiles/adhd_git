## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-15 - Dynamic Alt Text in Generated Visualizations
**Learning:** When generating multiple visualizations in a loop (like `ggplot2` in R), static alt text makes all plots sound identical to screen readers, destroying context.
**Action:** Always dynamically generate `alt` text using iteration variables (e.g., `labs(alt = paste("Scatter plot showing sensitivity vs specificity for", name_1))`) to ensure each generated plot is uniquely identifiable for screen reader users.
