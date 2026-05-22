## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-14 - Dynamic alt text for loop-generated ggplot2 plots
**Learning:** Hardcoded alt text in loops causes identical descriptions for different plots, which is confusing for screen readers.
**Action:** Always dynamically generate the alt text within the `labs(alt = ...)` function (e.g., using `paste()`) when creating multiple ggplot2 visualizations in a loop.
