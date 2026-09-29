## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).

## 2024-05-15 - Accessibility in R/Quarto visualizations
**Learning:** Screen readers cannot interpret data visualizations in Quarto reports automatically. Adding `alt` text to `ggplot2` layers via `labs(alt = ...)` is crucial for web accessibility (WCAG 1.1.1 Non-text Content) of generated HTML reports.
**Action:** Always add descriptive `alt` text explaining the chart's purpose, variables, and key takeaways when creating or modifying `ggplot2` visualizations.
