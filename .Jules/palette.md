## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-09-18 - Ensure screen reader accessibility for ggplot2 visualizations in loops
**Learning:** Data visualizations generated via ggplot2 inside loops lack contextual screen reader accessibility by default, resulting in missing or identical alt text for dynamically generated plots.
**Action:** Always dynamically generate the `alt` text within the `labs(alt = ...)` function (e.g., using `paste()`) when generating multiple plots in a loop to provide unique, context-aware descriptions for screen reader users.
