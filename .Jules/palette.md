## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-15 - Add alt text to ggplot2 visualizations
**Learning:** Data visualizations generated with ggplot2 in R/Quarto do not automatically have accessible text for screen readers. This leaves visually impaired users without context of what the plot is showing.
**Action:** Always add descriptive `alt` text to ggplot2 charts using `labs(alt = "...")`. If a `labs()` call already exists, add the `alt` argument inside it. Otherwise, append `+ labs(alt = "...")` to the ggplot declaration.
