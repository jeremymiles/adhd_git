## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-14 - Ensure ggplot2 accessibility with alt text
**Learning:** Screen readers cannot interpret the data in static ggplot2 images unless explicit `alt` text is provided.
**Action:** When creating ggplot2 visualizations, add an `alt` attribute describing the plot's structure and insights by either adding a new `labs(alt = "...")` layer or appending the `alt` argument to an existing `labs()` layer.
