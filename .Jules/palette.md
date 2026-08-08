## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-14 - Dynamically generate alt text for iterative data visualizations
**Learning:** Hardcoding a static `alt` text attribute when generating multiple plots inside a loop results in screen readers reading the exact same description for different charts, causing confusion and missing context.
**Action:** When creating multiple plots iteratively (e.g., in a `for` loop), construct dynamic `alt` text using string concatenation functions (e.g., `paste()`) incorporating loop variables to provide distinct, accurate descriptions for each generated plot.
