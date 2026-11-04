## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-14 - Provide alt text for data visualizations in R/Quarto
**Learning:** Generating plots in R/Quarto using ggplot2 without explicit `alt` text reduces accessibility for screen-reader users, as they cannot interpret the visual data representations.
**Action:** Always include an `alt` argument within the `labs()` function when creating ggplot2 visualizations (e.g., `+ labs(alt = "Description of plot")`) to ensure meaningful descriptions are available to assistive technologies.
