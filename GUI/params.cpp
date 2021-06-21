#include <qstring.h>
#include "params.h"

Params::Params()
{
	PARAM_SET params[] = {
{"CONCEPT",0,0,3,
 "Concept",
 "The concepts are: \n\
  0 = only one CAR\n\
  1 = bispecific case, both CARs on each cell\n\
  2 = a mixture of cells with one or the other CAR\n\
  3 = a mixture of cells with one or the other CAR and cells with both CARs."},

{"NTCELLS",1000,0,0,
 "Number of T cells",
 "Initial number of T cells to simulate"},

{"NTUM_PER_TCELL",100,0,0,
 "Tumour cells/T cell",
 "Number of tumour cells per T cell"},

{"NDAYS", 25.0, 0, 0,
"Number of days",
"Length of the simulation (days)."},

{"SEED1", 1234, 0, 0,
"First RNG seed",
"The random number generator is seeded by a pair of integers.  Changing the seed generates a different Monte Carlo realization."},

{"SEED2", 5678, 0, 0,
"Second RNG seed",
"The random number generator is seeded by a pair of integers.  Changing the seed generates a different Monte Carlo realization."},

{"NCPU", 4, 1, 8,
"Number of CPUs",
"Number of CPUs to use for the simulation. (Currently there is no parallelisation.)"},

{"CAR1_MEDIAN", 1.0, 0.1, 10.0,
"CAR1 expression median parameter",
"CAR1 expression has a lognormal distribution, described by the median and shape parameters. \n\
The shape value must be greater than 1, and values close to 1 give distributions that are close to normal."},

{"CAR1_SHAPE", 2.0, 1.01, 3.0,
"CAR1 expression shape parameter",
"CAR1 expression has a lognormal distribution, described by the median and shape parameters.\n\
The shape value must be greater than 1, and values close to 1 give distributions that are close to normal."},

{"CAR2_MEDIAN", 1.0, 0.1, 10.0,
"CAR2 expression median parameter",
"CAR2 expression has a lognormal distribution, described by the median and shape parameters. \n\
The shape value must be greater than 1, and values close to 1 give distributions that are close to normal."},

{"CAR2_SHAPE", 2.0, 1.01, 3.0,
"CAR2 expression shape parameter",
"CAR2 expression has a lognormal distribution, described by the median and shape parameters.\n\
The shape value must be greater than 1, and values close to 1 give distributions that are close to normal."},

{"POTENCY_MEDIAN", 1.0, 0.1, 10,
"CAR potency median parameter",
"CAR potency has a lognormal distribution, described by the median and shape parameters.\n\
Kill signal strength is given by Pkill = Q*potency*(tumour susceptibility), where Q = TCR signal strength."},

{"POTENCY_SHAPE", 1.5, 1.01, 3.0,
"CAR potency shape parameter",
"CAR potency has a lognormal distribution, described by the median and shape parameters.\n\
Kill signal strength is given by Pkill = Q*potency*(tumour susceptibility), where Q = TCR signal strength."},

{"EXHAUSTION_MEAN", 1, 0, 0,
"Initial exhaustion mean",
"Initial exhaustion is a base value times a normally distributed random variable.  This is the mean of the r.v."},

{"EXHAUSTION_STD", 0.2, 0, 0,
"Initial exhaustion std",
"Initial exhaustion is a base value times a normally distributed random variable.  This is the standard deviation of the r.v."},

{"EXHAUSTION_BASE", 10, 0, 0,
"Initial exhaustion base",
"Initial exhaustion is this base value times a normally distributed random variable."},

{"EXHAUSTION_RATE", 0.01, 0, 0,
"Exhaustion rate",
"When a T cell encounters a tumour cell with TCR signal strength Q, the exhaustion increases by Q*rate \n\
 where Q = CARlevel*targetExpression."},

{"EXHAUSTION_THRESHOLD", 100, 0, 0,
"Exhaustion threshold",
"A T cell dies when its exhaustion exceeds this threshold."},

 {"DEATH_PROB_MAX", 0.01, 0, 0,
 "Max death probability",
 "After 2 days, a T cell has a probability of spontaneous apoptosis, the probability is a function of the cell's total stimulation level. \n\
  If the stimulation level is S, and the threshold for division is T, the death probability = (1 - S/T)*(max death probability)."},

{"DOUBLE_EXP_PROB", 0.5, 0, 0,
"Double CAR prob",
"In concept 3, probability that a cell expresses both CARs."},

{"Q_FACTOR", 1.0,0,0,
"Stimulation factor",
"The contribution to a cell's total stimulation, when TCR signal strength is Q, is equal to (stimulation factor)*Q."},

{"ACTIVATION_THRESHOLD", 0.5,0,0,
"T cell activation threshold",
"If the total stimulation level has not reached this threshold an undivided cell cannot kill."},

{"DIVIDE_THRESHOLD", 10,0,0,
"T cell division threshold",
"A cell undergoes division when its total stimulation reaches this level."},

{"STIM_HALFLIFE", 24,0,0,
"TCR stimulation halflife (hours)",
"A T cell's total stimulation decays with this halflife."},


{"TARGET_EXP1_MEDIAN", 1, 0.1, 10,
"Target1 expression median parameter",
"It has a lognormal distribution, described by the median and shape parameters.\n\
(...)"},

{"TARGET_EXP1_SHAPE", 1.5, 1.01, 3.0,
"Target1 expression shape parameter",
"It has a lognormal distribution, described by the median and shape parameters."},

{"TARGET_EXP2_MEDIAN", 1, 0.1, 10,
"Target2 expression median parameter",
"Target expression has a lognormal distribution, described by the median and shape parameters.\n\
[days]"},

{"TARGET_EXP2_SHAPE", 1.5, 1.01, 3.0,
"Target2 expression shape parameter",
"Target expression has a lognormal distribution, described by the median and shape parameters."},

{"SUSCEPT_MEDIAN", 1, 0.1, 10,
"Tumour susceptibility median parameter",
"It has a lognormal distribution, described by the median and shape parameters.\n\
(...)"},

{"SUSCEPT_SHAPE", 1.15, 1.01, 3.0,
"Tumour susceptibility shape parameter",
"It has a lognormal distribution, described by the median and shape parameters."},

{"PROB_MEET_RATE", 0.000001,0,0,
"Tcell-tumour prob meet rate",
"The probability that a given T cell will meet a tumour cell in a time step (6h) is equal to this value times the number of tumour cells."},

{"USE_KILL_PROB", 0,0,0,
"Use kill probability?",
 "The kill signal strength is computed as Pkill = Q*(T cell potency)*(tumour cell susceptibility) \n\
  when using kill probability, the probability of death = Pkill/Pkill_max \n\
  otherwise, death occurs if Pkill > kill signal threshold."},

{"PKILL_MAX", 2.5,0,0,
"Kill prob scaling factor",
"The kill signal strength is computed as Pkill = Q*(T cell potency)*(tumour cell susceptibility) \n\
 when using kill probability, the probability of death = Pkill/Pkill_max \n\
 otherwise, death occurs if Pkill > kill signal threshold."},

{"KILL_THRESHOLD", 40,0,0,
"Kill signal threshold",
 "The kill signal strength is computed as Pkill = Q*(T cell potency)*(tumour cell susceptibility) \n\
  when using kill probability, the probability of death = Pkill/Pkill_max \n\
  otherwise, death occurs if Pkill > kill signal threshold."},

{"TUMOUR_DIVIDE_PROB", 0.0025,0,0,
"Tumour cell divide prob",
"A tumour cell has the specified probability of dividing in a time step (6 hours)."},


/*
{"SPECIAL_CASE", 0, 0, 0,
"Special case simulation",
"Select one of the hard-coded special cases (> 0)"},

{"SPECIAL_CASE_FILE", 0, 0, 0,
"",
"Input file required by the selected hard-coded special case"},

{"INPUT_FILE", 0, 0, 0,
"fixed.inpdata",
"The auxiliary input file contains data that (almost!) never changes"},
*/

// Time-series plots
    {"nTC",                 1, 0,1,"","No. of T Cells"},
    {"nTumC",               1, 0,1,"","No of tumour cells"},
    {"nIDs",                1, 0,1,"","No of IDs"},
// Profile plots


};
    nParams = sizeof(params)/sizeof(PARAM_SET);
    workingParameterList = new PARAM_SET[nParams];
    for (int i=0; i<nParams; i++) {
        workingParameterList[i] = params[i];
    }
}


PARAM_SET Params::get_param(int k)
{
	return workingParameterList[k];
}

void Params::set_value(int k, double v)
{
	workingParameterList[k].value = v;
}

void Params::set_label(int k, QString str)
{
	workingParameterList[k].label = str;
}
