## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-14 - Add alternative text to ggplot visualizations
**Learning:** Data visualizations built with ggplot do not automatically include alternative text, rendering them invisible to screen readers and causing accessibility failures for visually impaired users.
**Action:** Always provide descriptive `alt` text to ggplot visualizations by utilizing `labs(alt = "description")`. When `labs()` is already present, add `alt` to it instead of appending another `labs()` call. For plots generated in loops, construct dynamic alt text using paste() or similar functions.
