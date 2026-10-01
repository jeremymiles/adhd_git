## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-15 - Dynamic Alt Text in Generated Visualizations
**Learning:** When dynamically generating multiple plots (e.g., within loops in R/Quarto), static `alt` text creates redundant and confusing experiences for screen reader users as every plot sounds identical, removing critical context.
**Action:** Always use dynamic string generation (e.g., `paste()`) within the `labs(alt = ...)` function to create unique, descriptive alt text for each iteratively generated visualization.
