## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-10-07 - Ensure alternative text is available on ggplot2 visualizations
**Learning:** R data visualizations built with ggplot2 frequently lack alternative text descriptions by default, making them completely inaccessible to users relying on screen readers.
**Action:** Always utilize the `alt` argument within the `labs()` function to provide meaningful alternative text descriptions for all static plots, ensuring the information conveyed by the visualization is accessible to all users.
