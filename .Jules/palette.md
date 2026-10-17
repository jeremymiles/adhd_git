## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).

## 2024-10-17 - Add alt text to data visualizations
**Learning:** Many data visualizations built with ggplot2 lack alt text by default, which makes them inaccessible to screen readers.
**Action:** Always add an `alt` parameter to the `labs()` function in ggplot2 calls (e.g., `labs(alt = "Description")`) to ensure visualizations can be interpreted by screen readers.
