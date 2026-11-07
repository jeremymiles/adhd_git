## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).

## 2024-05-24 - Provide alt text for all complex data visualizations
**Learning:** Data visualizations without `alt` text are completely inaccessible to screen reader users, preventing them from understanding the core insights and data relationships presented. Complex plots (faceted, bubble charts, boxplots) require descriptive summaries.
**Action:** Always include an `alt` argument in `labs()` for ggplot2 charts to summarize the visualization's purpose and key data relationships, ensuring screen reader users can access the information.