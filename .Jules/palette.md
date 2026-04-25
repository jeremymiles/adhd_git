## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-14 - Use dynamic alt text for ggplot2 generated in loops
**Learning:** When generating multiple `ggplot2` plots inside a loop, static `alt` text in `labs(alt = ...)` fails to describe the specific data shown in each individual plot, degrading the experience for screen reader users.
**Action:** Use string concatenation functions like `paste()` to dynamically generate the `alt` text in `labs(alt = ...)` to reflect the specific iteration's data, ensuring each plot has descriptive and unique alternative text.
