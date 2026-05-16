## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-14 - Dynamically generate alt text for visualizations in loops
**Learning:** When using loops to render multiple data visualizations (e.g., iterating through `d_ss_long$name` to create ggplot2 charts), setting a static `alt` text in `labs(alt = ...)` causes all generated plots to share the same description. This reduces accessibility for screen reader users who cannot distinguish the specific context or data of each individual chart.
**Action:** Always dynamically generate the `alt` text within the loop using functions like `paste()` (e.g., `alt = paste("Scatter plot for", name_1)`) to ensure each visualization accurately describes its unique data segment.
