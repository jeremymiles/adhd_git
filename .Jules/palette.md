## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-15 - Dynamic alternative text for generated plots
**Learning:** Hardcoded single static alternative texts for plots dynamically generated inside loops using `ggplot2` in RMarkdown are unhelpful and inaccessible for screen readers. They cannot convey the distinction between variations of the same chart type.
**Action:** When creating `ggplot2` visualizations inside a loop, dynamically generate the `alt` parameter using string operations (e.g., `paste()`) incorporating loop variables to reflect the specific iteration's data precisely.
