## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-20 - Adding alt text to ggplot2
**Learning:** Data visualizations generated using R packages like ggplot2 can be inaccessible to users relying on screen readers because they lack dynamic descriptions of what is being displayed.
**Action:** Always add descriptive alternative text to ggplot2 charts using the labs(alt = "...") function, and ensure the alt text provides meaningful context about the variables, comparisons, or groupings being visualized.
