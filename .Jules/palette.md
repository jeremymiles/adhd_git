## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-20 - Add alt text to ggplot2 visualizations
**Learning:** Data visualizations built with ggplot2 and generated in Quarto lack alternative text by default, making them inaccessible to screen readers.
**Action:** Always add descriptive `alt` text to ggplot2 visualizations using the `labs(alt = "...")` argument to ensure accessibility for all users.
