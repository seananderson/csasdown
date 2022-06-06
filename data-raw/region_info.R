region_info <- tibble::tribble(
  ~Region, ~RegionFr, ~Email, ~Address, ~AddressFr,
  "Central and Arctic Region", "R\u00E9gion du Centre et de l'Arctique", "xcna-csa-cas@dfo-mpo.gc.ca", "501 University Cres.\\\\\\\\Winnipeg, MB, R3T 2N6", "501 University Cres.\\\\\\\\Winnipeg (MB) R3T 2N6",
  "Gulf Region", "R\u00E9gion du Golfe", "DFO.GLFCSA-CASGOLFE.MPO@dfo-mpo.gc.ca", "343 Universit\u00E9 Ave.\\\\\\\\Moncton, NB, E1C 9B6", "343 Universit\u00E9 Ave.\\\\\\\\Moncton (N.-B.) E1C 9B6",
  "Maritimes Region", "R\u00E9gion des Maritimes", "XMARMRAP@dfo-mpo.gc.ca", "1 Challenger Dr.\\\\\\\\Dartmouth, NS, B2Y 4A2", "1 Challenger Dr.\\\\\\\\Dartmouth (N.-\u00C9.) B2Y 4A2",
  "National Capital Region", "R\u00E9gion de la capitale nationale", "csas-sccs@dfo-mpo.gc.ca", "200 Kent St.\\\\\\\\Ottawa, ON, K1A 0E6", "200 Kent St.\\\\\\\\Ottawa (ON) K1A 0E6",
  "Newfoundland and Labrador Region", "R\u00E9gion de Terre-Neuve et Labrador", "DFONLCentreforScienceAdvice@dfo-mpo.gc.ca", "P.O. Box 5667\\\\\\\\St. John's, NL, A1C 5X1", "P.O. Box 5667\\\\\\\\St. John's (T.-N.-L.) A1C 5X1",
  "Pacific Region", "R\u00E9gion du Pacifique", "csap@dfo-mpo.gc.ca", "3190 Hammond Bay Rd.\\\\\\\\Nanaimo, BC, V9T 6N7", "3190, chemin Hammond Bay\\\\\\\\Nanaimo (C.-B.) V9T 6N7",
  "Quebec Region", "R\u00E9gion du Qu\u00E9bec", "bras@dfo-mpo.gc.ca", "850 route de la Mer, P.O. Box 1000\\\\\\\\Mont-Joli, QC, G5H 3Z4", "850 route de la Mer, P.O. Box 1000\\\\\\\\Mont-Joli (QC) G5H 3Z4"
)
usethis::use_data(region_info, overwrite = TRUE)
