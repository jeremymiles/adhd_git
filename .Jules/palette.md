## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-15 - Add dynamic alternative text to plots in loops
**Learning:** Generating plots in a loop without dynamic alternative text (alt text) results in multiple images that screen readers either cannot describe or describe identically, severely hindering accessibility for visually impaired users analyzing the data.
**Action:** Always include dynamically generated `alt` text using `labs(alt = paste(...))` within `ggplot2::ggplot()` calls inside loops to accurately describe each specific plot's contents.
