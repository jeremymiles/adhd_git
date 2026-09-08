## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-15 - Dynamic Alt Text in Plots
**Learning:** Hardcoded single text strings for alt text in dynamically generated plots (like those created in a loop in R using ggplot) causes multiple distinct plots to share identical descriptions, misleading screen reader users about the individual contents.
**Action:** When generating multiple visualizations in a loop, always dynamically generate the `alt` text within the `labs(alt = ...)` function (e.g., using `paste()`) to provide accurate, specific descriptions for each plot.
