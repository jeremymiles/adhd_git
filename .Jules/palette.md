## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-14 - Add dynamic alt text to ggplot2 visualizations
**Learning:** By default, ggplot2 data visualizations lack descriptive text for users who rely on screen readers. This degrades accessibility for a key part of our data analysis.
**Action:** Always include alt text for visualizations by using the `labs(alt = ...)` parameter inside ggplot calls. For plots generated dynamically in a loop, generate the alt text dynamically as well using `paste()`.
