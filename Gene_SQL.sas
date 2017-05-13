/* master mother - merge all mother files */
proc sql;
create table mother as
select 
X.*,
D.childid as ep_childid,D.fatherid as ep_fatherid,
E.childid as lp_childid,E.fatherid as lp_fatherid,
F.childid as pn_childid,F.fatherid as pn_fatherid,
COALESCE(flag_g_age_10,0) as flag_g_age_10,
COALESCE(flag_g_age_18,0) as flag_g_age_18,
COALESCE(flag_g_guth_f1,0) as flag_g_guth_f1,
COALESCE(D.flag_ep,0) as flag_ep,
COALESCE(E.flag_lp,0) as flag_lp,
COALESCE(F.flag_postnatal,0) as flag_postnatal,
D.long_fullID || "," || E.long_fullID || "," || F.long_fullID as fullstudyID
from (
    select motherid from m_g10
    union
    select motherid from m_g18
    union
    select motherid from m_gg
	union
	select motherid from m_ep
    union
    select motherid from m_lp
	union
    select motherid from m_pn
) as X
    left outer join m_g10 as A on A.motherid = X.motherid
    left outer join m_g18 as B on B.motherid = X.motherid
    left outer join m_gg as C on C.motherid = X.motherid
	left outer join m_ep as D on D.motherid = X.motherid
    left outer join m_lp as E on E.motherid = X.motherid
    left outer join m_pn as F on F.motherid = X.motherid
;
quit;


/* master child - merge all child files */

proc sql;
create table child as
select X.*,
A.motherid as cb_motherid,A.fatherid as cb_fatherid,
B.motherid as g_motherid,B.fatherid as g_fatherid,
COALESCE(flag_cord_f2,0) as flag_cord_f2,
COALESCE(flag_guth_f2,0) as flag_guth_f2,
A.long_fullID || "," || B.long_fullID as fullstudyID
from (
    select childid from c_cb
    union
    select childid from c_g
    
) as X
    left outer join c_cb as A on A.childid = X.childid
    left outer join c_g as B on B.childid = X.childid
    ;
quit;

/* AIM 1 */

proc sql;
create table aim1 as
select distinct
coalesce(m.motherid,c.cb_motherid,c.g_motherid) as motherid,
c.childid,
m.fullstudyID ||","||c.fullstudyID as fullstudyID,
coalesce(flag_g_guth_F1,0) as flag_g_guth_F1,
coalesce(flag_g_age_10,0) as flag_g_age_10,
coalesce(flag_g_age_18,0) as flag_g_age_18,
coalesce(flag_EP,0) as flag_EP,
coalesce(flag_LP,0) as flag_LP,
coalesce(flag_postnatal,0) as flag_postnatal,
coalesce(flag_cord_F2,0) as flag_cord_F2,
coalesce(flag_guth_F2,0) as flag_guth_F2
from 
mother m full join child c
on
m.motherid = c.cb_motherid
or
m.motherid = c.g_motherid
;
quit;


/* master father - merge all father files */
proc sql;
create table father as 
select 
X.*,
D.childid as fp_childid,D.motherid as fp_motherid,
COALESCE(flag_b_guth_f1,0) as flag_b_guth_f1,
COALESCE(flag_b_age10,0) as flag_b_age10,
COALESCE(flag_b_age18,0) as flag_b_age18,
COALESCE(flag_Father_Preg,0) as flag_Father_Preg,
D.long_fullID as fullstudyID
from (
    select fatherid from f_b10
    union
    select fatherid from f_b18
    union
    select fatherid from f_bg
	union
	select fatherid from f_fp
) as X
    left outer join f_b10 as A on A.fatherid = X.fatherid
    left outer join f_b18 as B on B.fatherid = X.fatherid
    left outer join f_bg as C on C.fatherid = X.fatherid
	left outer join f_fp as D on D.fatherid = X.fatherid
    
;
quit;



/* AIM 2 */

proc sql;
create table aim2 as
select 
coalesce(f.fatherid,c.cb_fatherid,c.g_fatherid) as fatherid,
coalesce(c.cb_motherid,c.g_motherid,f.fp_motherid) as motherid,
coalesce(c.childid,f.fp_childid) as childid,
/* f.fp_childid,f.fp_motherid,
   c.childid,c.cb_motherid,c.g_motherid, */
f.fullstudyID ||","||c.fullstudyID as fullstudyID,
COALESCE(flag_b_guth_f1,0) as flag_b_guth_f1,
COALESCE(flag_b_age10,0) as flag_b_age10,
COALESCE(flag_b_age18,0) as flag_b_age18,
COALESCE(flag_Father_Preg,0) as flag_Father_Preg,
coalesce(flag_cord_F2,0) as flag_cord_F2,
coalesce(flag_guth_F2,0) as flag_guth_F2
from 
father f full join child c
on
f.fatherid = c.cb_fatherid
or
f.fatherid = c.g_fatherid

;
quit;



/* AIM 3 */

proc sql;
create table aim3 as
select distinct
coalesce(t.motherid,cb_motherid,g_motherid,fp_motherid) as motherid,
t.childid,f.fatherid,
COALESCE(flag_g_age_10,0) as flag_g_age_10,
COALESCE(flag_g_age_18,0) as flag_g_age_18,
COALESCE(flag_g_guth_f1,0) as flag_g_guth_f1,
COALESCE(flag_ep,0) as flag_ep,
COALESCE(flag_lp,0) as flag_lp,
COALESCE(flag_postnatal,0) as flag_postnatal,
COALESCE(flag_b_guth_f1,0) as flag_b_guth_f1,
COALESCE(flag_b_age10,0) as flag_b_age10,
COALESCE(flag_b_age18,0) as flag_b_age18,
COALESCE(flag_Father_Preg,0) as flag_Father_Preg,
coalesce(flag_cord_F2,0) as flag_cord_F2,
coalesce(flag_guth_F2,0) as flag_guth_F2
from
(select * from mother m join child c
 on
 m.motherid = c.cb_motherid
 or
 m.motherid = c.g_motherid) t 
 join father f
 on 
t.motherid = f.fp_motherid
;
quit;


proc export data=aim1
    outfile='U:\Adv sas\Final Exam\FinalExamData\aim1.csv'
    dbms=csv
    replace;
run;

proc export data=aim2
    outfile='U:\Adv sas\Final Exam\FinalExamData\aim2.csv'
    dbms=csv
    replace;
run;

proc export data=aim3
    outfile='U:\Adv sas\Final Exam\FinalExamData\aim3.csv'
    dbms=csv
    replace;
run;
