## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-09-14 - Dynamic alt text for loop-generated visualizations
**Learning:** When generating multiple data visualizations (like `ggplot2` plots) within a loop in R or Quarto, static alt text causes screen readers to announce the same description for every plot, confusing users and breaking accessibility.
**Action:** Always dynamically generate `alt` text within the `labs(alt = ...)` function (e.g., using `paste()` with the loop variable) to ensure each plot has a unique, descriptive label for screen readers.
