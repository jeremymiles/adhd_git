## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-15 - Add alt text to all data visualizations
**Learning:** Data visualizations like ggplot2 without alt text are completely inaccessible to screen reader users, who will just hear "image" or the file name.
**Action:** Always add descriptive `alt` text to ggplot visualizations using `labs(alt = "Description")`. Ensure descriptions convey the core message of the visualization.
