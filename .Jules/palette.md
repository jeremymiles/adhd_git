## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-14 - Use accessible descriptive text for R plots
**Learning:** Generating multiple `ggplot2` visualizations without `alt` text makes data inaccessible to screen readers, especially when generated dynamically in loops or faceted grids.
**Action:** Always add descriptive `alt` text to R plots using `labs(alt = ...)` and ensure dynamically generated plots use dynamic variables (e.g., `paste()`) to provide specific context.
