## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).

## 2024-08-08 - Dynamic Alt Text for Iterative Visualizations
**Learning:** When generating multiple plots dynamically within a loop, static alt text fails to provide necessary context to screen readers, making the visualizations indistinguishable from one another.
**Action:** Always dynamically generate `alt` text within `labs(alt = ...)` (e.g., using `paste()`) when creating plots in a loop to ensure screen reader users receive specific, contextual descriptions for each visualization.
