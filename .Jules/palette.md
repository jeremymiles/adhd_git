## 2024-05-14 - Use accessible color palettes for data visualizations
**Learning:** Hardcoded basic colors (like "red", "green", "yellow", "chocolate4") in data visualizations like ggplot2 can be extremely difficult to distinguish for users with color vision deficiencies, reducing accessibility.
**Action:** Always replace basic hardcoded colors in plots with accessible, colorblind-friendly palettes such as Okabe-Ito (e.g., `#E69F00`, `#56B4E9`, `#009E73`, `#F0E442`, `#0072B2`, `#D55E00`, `#CC79A7`).
## 2024-05-15 - Dynamic Alt Text in Looped Visualizations
**Learning:** Generating multiple `ggplot2` visualizations within a loop without dynamically updating the `alt` text creates duplicate, ambiguous descriptions for screen reader users, confusing the context of what each chart distinctively shows.
**Action:** Always dynamically generate the `alt` text within the `labs(alt = ...)` function (e.g., using `paste()`) when creating plots in a loop, ensuring each output has a unique, descriptive alternative text.
