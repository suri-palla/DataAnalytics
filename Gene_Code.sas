* libname exam 'U:\Adv sas\Final Exam\FinalExamData';

/* Mother Files */
filename m_g10 'U:\Adv sas\Final Exam\FinalExamData\mother\g_Age10_F1.txt';
filename m_g18 'U:\Adv sas\Final Exam\FinalExamData\mother\g_Age18_F1.txt';
filename m_gg 'U:\Adv sas\Final Exam\FinalExamData\mother\g_Guthreis_F1.txt';
filename m_ep 'U:\Adv sas\Final Exam\FinalExamData\mother\Early_Pregnancy.txt';
filename m_lp 'U:\Adv sas\Final Exam\FinalExamData\mother\Late_Pregnancy.txt';
filename m_pn 'U:\Adv sas\Final Exam\FinalExamData\mother\Postnatal.txt';


/*********************************************/
/************ MOTHER *******************/
/* reading the input files and creating flags */

data m_g10;
infile m_g10 firstobs=2;
input motherid;
run;

%addflag(m_g10,flag_g_age_10);

data m_g18;
infile m_g18 firstobs=2;
input motherid;
run;

%addflag(m_g18,flag_g_age_18);

data m_gg;
infile m_gg firstobs=2;
input motherid;
run;

%addflag(m_gg,flag_g_guth_f1);

data m_ep;
infile m_ep dlm='09'x truncover firstobs=2;
input long_fullID $ 1-27;
run;

%extract(m_ep);
%addflag(m_ep,flag_ep);

data m_lp;
infile m_lp truncover firstobs=2;
input long_fullID $ 1-27;
run;

%extract(m_lp);
%addflag(m_lp,flag_lp);

data m_pn;
infile m_pn truncover firstobs=2;
input long_fullID $ 1-27;
run;

%extract(m_pn);
%addflag(m_pn,flag_postnatal);

/*********************************************/


/* Child Files */
filename c_cb 'U:\Adv sas\Final Exam\FinalExamData\child\Cordblood_F2.txt';
filename c_g 'U:\Adv sas\Final Exam\FinalExamData\child\Guthries_F2.txt';

/************ CHILD *******************/
/* reading the input files and creating flags */

data c_cb;
infile c_cb truncover firstobs=2;
input long_fullID $ 1-17;
run;

%extract(c_cb);
%addflag(c_cb,flag_cord_f2);


data c_g;
infile c_g dlm='09'x truncover firstobs=2;
input long_fullID $ 1-17;
run;

%extract(c_g);
%addflag(c_g,flag_guth_f2);

/*********************************************/


/* Father Files */
filename f_b10 'U:\Adv sas\Final Exam\FinalExamData\father\b_Age10_F1.txt';
filename f_b18 'U:\Adv sas\Final Exam\FinalExamData\father\b_Age18_F1.txt';
filename f_bg 'U:\Adv sas\Final Exam\FinalExamData\father\b_Guthreis_F1.txt';
filename f_fp 'U:\Adv sas\Final Exam\FinalExamData\father\Fathers_Pregnancy.txt';

/************ FATHER *******************/
/* reading the input files and creating flags */
data f_b10;
infile f_b10 firstobs=2;
input fatherid;
run;

%addflag(f_b10,flag_b_age10);

data f_b18;
infile f_b18 firstobs=2;
input fatherid;
run;

%addflag(f_b18,flag_b_age18);

data f_bg;
infile f_bg firstobs=2;
input fatherid;
run;

%addflag(f_bg,flag_b_guth_f1);

data f_fp;
infile f_fp dlm='09'x truncover firstobs=2;
input long_fullID $ 1-17;
run;

%extract(f_fp);
%addflag(f_fp,flag_Father_Preg);


