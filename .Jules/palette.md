## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-14 - Use dynamic alt text and unique iterators for plots in loops
**Learning:** When generating multiple plots in a loop, static alt text limits accessibility as screen readers can't distinguish between the plots. Additionally, iterating directly over dataframe columns (e.g., `d_ss_long$name`) when generating plots can lead to severe performance issues due to redundant rendering.
**Action:** Always generate dynamic alt text using functions like `paste()` to reflect the specific data in each iteration. Always use `unique()` when iterating over dataframe columns to prevent redundant processing.
