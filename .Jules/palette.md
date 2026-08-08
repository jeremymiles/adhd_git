## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-18 - Dynamic alternative text for programmatically generated plots
**Learning:** When using loops to generate multiple data visualizations (like `ggplot2` outputs in Quarto/RMarkdown), using a single static `alt` string causes screen readers to announce identical, uninformative descriptions for every distinct chart.
**Action:** Always construct dynamic `alt` text strings (e.g., using `paste()` in R) within visualization loops to ensure context-specific alternative text is provided for each iteration's unique data or category.
