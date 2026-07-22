## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-07-22 - Dynamic Alt Text in Loops
**Learning:** Generating multiple visualizations in a loop without dynamic alt text results in screen readers reading the same description or no description for each plot, confusing visually impaired users.
**Action:** Always dynamically generate alt text within the labs(alt = ...) function using string concatenation (e.g., paste()) when creating plots in a loop to ensure screen readers provide accurate, context-specific descriptions for each visualization.
