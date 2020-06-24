#  PlacePulse_PSP_RemoteSensing
 Comparison of remote sensing, PSPNet, and green screen scores in place pulse dataset

**Author:** [Andrew Larkin](https://www.linkedin.com/in/andrew-larkin-525ba3b5/) <br>
**Affiliation:** [Oregon State University, College of Public Health and Human Sciences](https://health.oregonstate.edu/) <br>
**Principal Investigator:** [Perry Hystad](https://health.oregonstate.edu/people/perry-hystad) <br>

**Summary** <br>
This is a comparison of different methodologies for measuring environmental composition and its relationship to participant perceptions in the MIT Place Pulse 2.0 dataset.  Measurement methodologies include:

- **Remote sensing measurements** - examples include NDVI, impervious surface area, air pollution
- **PSPNet labels** - labels include trees, roads, buildings, and paths
- **Green nature** - percent green in an image directly attributable to nature

**Repository Structure** <br>
Files are divided into three folders, with each folder corresponding to a unique stage of model development.

- **[variable estimates](https://github.com/larkinandy/LUR-NO2-Model/tree/master/variable%20estimates)** - scripts in this folder were used to derive NO2 estiamtes at air monitor locations using parallel processing. <br>
- **[upwind estimates](https://github.com/larkinandy/LUR-NO2-Model/tree/master/upwind%20estimates)** - scripts in this folder were used to estimate length of road upwind from air monitor locations.
- **[statistical analysis](https://github.com/larkinandy/LUR-NO2-Model/tree/master/statistical%20analysis)** - scripts in this folder were used to perform lasso variable selection, model evaluation, and sensitivity analysis. <br>

**External Links**
- **MIT Place Pulse (archived)** - 
- **PSPNet with ADEK20 Weights** - https://arxiv.org/pdf/1608.05442.pdf
