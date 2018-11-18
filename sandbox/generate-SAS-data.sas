libname dat "~/research/SoftWare/heaven/sandbox/";
proc import datafile="~/research/SoftWare/heaven/sandbox/lpr.csv"
out= dat.lpr dbms=csv replace; 
run; 
proc import datafile="~/research/SoftWare/heaven/sandbox/lmdb.csv"
out= dat.lmdb dbms=csv replace; 
run; 
