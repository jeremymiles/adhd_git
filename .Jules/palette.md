## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-09-24 - Screen Reader Accessible ggplot2 Visualizations
**Learning:** Data visualizations generated with ggplot2 often lack textual descriptions, rendering them completely opaque to screen reader users and those unable to view images.
**Action:** Always add descriptive `alt` text to ggplot2 visualizations using the `labs(alt = "...")` parameter. For dynamically generated plots within loops, construct dynamic alt text (e.g., using `paste()`) to ensure the descriptions accurately reflect the changing context.
