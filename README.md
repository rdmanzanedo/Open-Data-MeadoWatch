# Open-Data-MeadoWatch Project

This is the repository for the analyses and data of the data paper of the MeadoWatch project: LINK TO THE DATA PAPER TO BE ADDED HERE

THIS REPOSITORY WILL REMAIN UNCHANGED FOR CONSISTENCY WITH THE ANALYSES IN THE ABOVEMENTIONED LINK. AN UP-TO-DATE VERSION OF THE MEADOWATCH DATABASE IS AVAILABLE IN THE OFFICIAL REPOSITORY HERE: LINK TO BE ADDED.

MeadoWatch director and contact: Prof. Dr. Janneke Hille Ris Lambers. Institute of Integrative Biology, D-USYS, ETH Zürich
MeadoWatch coordinators:
Dr. Meera Sethi. Biology Department, University of Washington, Seattle
Prof. Dr. Elli Theobald. Biology Department, University of Washington, Seattle
Manuscript analyses and lead:
Dr. Rubén D. Manzanedo. Institute of Integrative Biology, D-USYS, ETH Zürich
PhD Candidate Aji John. Biology Department, University of Washington, Seattle
Berry Brosi. Biology Department, University of Washington, Seattle
Joshua Jenkins. Biology Department, University of Washington, Seattle
Emilia Lia. Biology Department, University of Washington, Seattle
Annie Schiffer. Biology Department, University of Washington, Seattle
Jordana Sevigny. Biology Department, University of Washington, Seattle
Anna Wilson. Biology Department, University of Washington, Seattle
Yonit Yogev. National Park Service. USA.


Permanent website of MeadoWatch and further info: http://www.meadowatch.org/
https://www.youtube.com/channel/UCGBFTKxf8FIWswMDxBavpuQ/featured


PUBLICATIONS LINKED TO THE MEADOWATCH DATABASE
- Hille Ris Lambers, J., Cannistra, A. F., John, A., Lia, E., Manzanedo, R. D., Sethi, M., Sevigny, J., Theobald, E. J., Waugh, J. K. (2021) Climate change impacts on natural icons: do phenological shifts threaten the relationship between peak wildflowers and visitor satisfaction? Climate Change Ecology, 100008.

- Breckheimer, I. K., Theobald, E. J., Cristea, N. C., Wilson, A. K., Lundquist, J. D., Rochefort, R. M., & HilleRisLambers, J. (2020). Crowd‐sourced data reveal social–ecological mismatches in phenology driven by climate. Frontiers in Ecology and the Environment, 18(2), 76-82.

- John, A., Ong, J., Theobald, E. J., Olden, J. D., Tan, A., & HilleRisLambers, J. (2020). Detecting Montane Flowering Phenology with CubeSat Imagery. Remote Sensing, 12(18), 2894.

- Wilson, A., Bacher, K., Breckheimer, I., Lundquist, J., Rochefort, R., Theobald, E., ... & HilleRisLambers, J. (2017). Monitoring wildflower phenology using traditional science, citizen science, and crowd sourcing. Park Sci, 33, 17-26.

# Repository content
RAW DATA:
- MW_PhenoDat_2013_2019.csv -- Phenological records. Information on date, transect id, observer, and phenological state per flowering species.

- MW_SiteInfo_2013_2019.csv -- Latitude and longitude locatoin (WGS84), elevation and forest type for each MW site

- MW_Phenocurves.csv -- 

- MW_ground_temperatures.csv --


R SCRIPTS:
- Part 1 - Location temporal replication and species comp.R - Analyses and plotting for figure 1. 
- Part 2 - species composition.R - Analyses for Figure 2b.
- Part 3 - temporal resolution.R - Analyses for Figure 3.
- Part 4 - comparison scientists citizens.R - Functions and selecting for Sci-cit comparison.
- Part 5 - figure accuracies sci-cit.R - Analyses and plotting for figure 5.

FIGURES (SVG and PNG):
- All in folder 'Exported figures'. Exported figures from R Studio has been finalized in Inkscape. Basic versions of all figures can be done with the accompanying code,

MANUSCRIPT
- First draft_data_paper_MW.docx - Manuscript versions (currently in google docs: https://docs.google.com/document/d/101CBT2GUFirWMFm_pHwYSdolv_xV4OsDa3H95RNpcmU/edit?usp=sharing)
