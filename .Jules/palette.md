## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-14 - Use dynamic alt text for data visualizations in loops
**Learning:** When generating plots (like `ggplot2`) within a loop, using a single static `alt` description for all generated images reduces accessibility because screen reader users cannot distinguish between the different charts.
**Action:** Always generate dynamic `alt` text using string concatenation (e.g., `paste()`) to reflect the specific iteration's data in the `labs(alt = ...)` function.
