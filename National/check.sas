proc univariate data = library.yrbs1991;
  var weight;
run;

proc freq data = library.yrbs1991;
  tables psu / nopercent nocum;
run;

proc surveyfreq data = library.yrbs1991;
  weight weight;
  cluster psu;
  strata stratum;
  tables q23;
run;

