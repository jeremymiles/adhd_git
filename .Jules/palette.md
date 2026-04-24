## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-15 - Add dynamic alt text to ggplot2 charts generated in loops
**Learning:** When generating multiple plots within a loop (e.g., using `ggplot2`), setting a static `alt` text in `labs()` results in identical descriptions for different charts, reducing accessibility for screen reader users. Furthermore, iterating over all elements of a vector instead of unique elements leads to redundant plot generation and performance issues.
**Action:** Always use dynamic `alt` text in loop-generated plots (e.g., using `paste()` with the loop variable) and ensure loops iterate over unique values (e.g., using `unique()`) to improve both accessibility and performance.
