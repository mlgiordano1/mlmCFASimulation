library(MplusAutomation)
genData <- "
! DO NOT EDIT IN TEXT. EDIT IN PRIMARY SIMULATION CODE r FILE. 
TITLE:	M3 Sim

montecarlo:
	names are y1-y9 ;
	nobservations = 3000;
	ncsizes = 1;
	csizes = 100 (30); !format #OfClustesr (SizeOfClusters)
	seed = 120170510;
	nrep = 2;
    repsave = ALL;
	save = C:\\Users\\mgiordan\\git\\mlmcfasimulation\\temp\\tempdata_*.dat;


ANALYSIS:	
	TYPE IS twolevel;
    !ESTIMATOR=BAYES;
    !estimator = WLS;
	!distribution = skewnormal;


MODEL POPULATION:

	%Within%
  ! Loadings
  L1 by y1@1;
  L1 by y2@1;
  L1 by y3@1;
  L1 by y5@.3;
  L2 by y4@1;
  L2 by y5@1;
  L2 by y6@1;
  L2 by y8@.3;
  L3 by y7@1;
  L3 by y8@1;
  L3 by y9@1;
  L3 by y6@.3;

	y1-y9*1;
	L1*1;
  L2*1;
  L3*1;

	%Between%
  L4 by y1@1;
  L4 by y2@1;
  L4 by y3@1;
  L4 by y5@1;
  L4 by y4@1;
  L4 by y5@1;
  L4 by y6@1;
  L4 by y8@1;
  L4 by y7@1;
  L4 by y8@1;
  L4 by y9@1;
  L4 by y6@1;  

	L4*.4;
	y1-y9@1;

MODEL:
	
	%Within%
  ! Loadings
  L1 by y1@1;
  L1 by y2@1;
  L1 by y3@1;
  L1 by y5@.3;
  L2 by y4@1;
  L2 by y5@1;
  L2 by y6@1;
  L2 by y8@.3;
  L3 by y7@1;
  L3 by y8@1;
  L3 by y9@1;
  L3 by y6@.3;

	y1-y9*1;
	L1*1;
  L2*1;
  L3*1;

	%Between%
  L4 by y1@1;
  L4 by y2@1;
  L4 by y3@1;
  L4 by y5@1;
  L4 by y4@1;
  L4 by y5@1;
  L4 by y6@1;
  L4 by y8@1;
  L4 by y7@1;
  L4 by y8@1;
  L4 by y9@1;
  L4 by y6@1;  

	L4*.4;
	y1-y9@1;

output:
	tech8 tech9;
"

# Write data generation input file, and run it
writeLines(genData, 
          "C:\\Users\\mgiordan\\git\\mlmcfasimulation\\temp\\genData.inp")
runModels(directory = "C:\\Users\\mgiordan\\git\\mlmcfasimulation\\temp")