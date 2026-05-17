## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-15 - Add alt text to ggplot2 visualizations
**Learning:** `ggplot2` visualizations rendered in R Markdown or Quarto documents lack alternative text by default, which creates significant accessibility barriers for users relying on screen readers.
**Action:** Always add descriptive alternative text using the `labs(alt = "...")` function directly within `ggplot2` calls to ensure screen reader compatibility.
