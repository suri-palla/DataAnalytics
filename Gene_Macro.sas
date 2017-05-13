/* Macro - To extract the childid, motherid and fatheris from fullID */
%macro extract(dataset);
data &dataset;
set &dataset;
childid = input(substr(long_fullID,2,4),8.0);
motherid = input(substr(long_fullID,7,4),8.0);
fatherid = input(substr(long_fullID,12,4),8.0);
run;
%mend extract;

/* Macro - To set a flag for each file */
%macro addflag(dataset,flag);
data &dataset;
set &dataset;
&flag = 1;
run;
%mend addflag;
