## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-06-10 - Dynamic Alt Text for Screen Reader Accessibility
**Learning:** When generating multiple plots within a loop, static alt text causes screen readers to announce identical descriptions for different visualizations, removing context for visually impaired users.
**Action:** Always dynamically generate alt text within labs(alt = ...) using string concatenation (e.g., paste()) to ensure each visualization has a unique, descriptive label reflecting its specific data.
