## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-15 - Improve accessibility of ggplot2 visualizations with alt text
**Learning:** Plots generated without alt text are inaccessible to visually impaired users relying on screen readers. Including descriptive text via `labs(alt = "...")` directly embedded into `ggplot2` calls greatly enhances accessibility.
**Action:** Always add descriptive `alt` text to data visualizations by using or appending to the `labs()` function inside `ggplot2` declarations.
