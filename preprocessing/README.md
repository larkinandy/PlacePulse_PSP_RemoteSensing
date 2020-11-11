## Data Preprocessing
 Calculating built environment composition, downloading NDVI from the Google Earth Engine, and performing QA steps

 **files** <br>
- **[ImgFeatures.py](./ImgFeatures.py)** - custom class which calculates join count and green screen estimates
- **[pp_constants.py](./pp_constants.py)** - security-sensitive constants, such as filepaths to data directories
- **[PP_GreenScreen.ipynb](./PP_GreenScreen.ipynb)** - Jupyter notebook used to impelement ImgFeatures class and calculate joing count and green screen statistics

**External Links**
- **Join Count Statistics** - https://www.youtube.com/watch?v=BdsdYEbUkj4
- **Green Screen Github Repo** - https://github.com/larkinandy/GSV_NDVI_Comparison
