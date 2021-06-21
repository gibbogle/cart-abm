//ABM
#include <qstring.h>
#include "graphs.h"
#include "log.h"

LOG_USE();

Graphs::Graphs()
{
    GRAPH_SET tsGraphSet[] = {

        {"nTC",
        "Live T Cells",
        "",  "No. of T cells",
        1, true, 0, 1, 0, TS_TYPE},

        {"nTumC",
        "Live Tumour Cells",
        "",  "No. of Tumour cells",
        2, true, 0, 1, 0, TS_TYPE},

        {"nIDs",
        "Live T-cell Lineages",
        "",  "No. of Live T-cell lineages",
        3, true, 0, 1, 0, TS_TYPE},


/*
{"nDC",
"Antigen Presenting Cells",
"",  "No. of DCs",
2, true, 0, 1, 0, TS_TYPE},

{"ntot_LN",
"Total LN Population",
"",  "No. of cells",
3, true, 0, 1, 0, TS_TYPE},

{"ncogseed",
"Seed Cognate Cells",
"",  "No. of cells",
4, true, 0, 1, 0, TS_TYPE},

{"ncog_LN",
"Cognate T Cells (LN)",
"",  "No. of cells",
5, true, 0, 1, 0, TS_TYPE},

{"ncog_PER",
"Activated Cells (PER)",
"",  "No. of cells",
6, true, 0, 1, 0, TS_TYPE},

{"nbnd",
"Bound Cognate Cells",
"",  "No. of cells",
8, false, 0, 1, 0, TS_TYPE},

{"nexits",
"No. of Exit Portals",
"",  "No. of portals",
10, false, 0, 1, 0, TS_TYPE},

{"nteffgen0",
"Efferent Unactivated",
"",  "No. of cells",
11, false, 0, 1, 0, TS_TYPE},

{"nteffgen",
"Efferent Activated",
"",  "No. of cells",
12, true, 0, 1, 0, TS_TYPE},

{"act",
"Total DC Antigen",
"",  "Total DC antigen",
13, true, 0, .001, 0, TS_TYPE},

{"stim_LN",
"Average Stimulation (LN)",
"",  "Average stimulation of cognate cells in the LN",
14, true, 0, .001, 1.0, TS_TYPE},

{"stim_PER",
"Average Stimulation (PER)",
"",  "Average stimulation of cognate cells in the periphery",
15, false, 0, .001, 1.0, TS_TYPE},

{"stimrate_LN",
"Stimulation Rate (LN)",
"",  "Stimulation Rate (LN)",
16, false, 0, .001, 1.0, TS_TYPE},

{"DCcontact_time",
"1st DC Contact (min)",
"",  "1st DC Contact (min)",
17, false, 0, .001, 0, TS_TYPE},

{"DCtravel_time",
"Inter-DC Travel (min)",
"",  "Inter-DC Travel (min)",
18, false, 0, .001, 0, TS_TYPE},

{"DCbind_time",
"DC Bind Time (min)",
"",  "DC Bind Time (min)",
19, false, 0, .001, 0, TS_TYPE},

{"Bound_fraction",
"Bound Fraction",
"",  "Bound Fraction",
20, true, 0, .001, 1.0, TS_TYPE},

{"nDC_SOI",
"Cells in DC SOI",
"",  "Cells in DC SOI",
21, false, 0, .001, 0, TS_TYPE},

{"noDC_contact",
"Fraction no DC Contact",
"",  "Fraction no DC Contact",
22, false, 0, .001, 0, TS_TYPE},

{"noDC_contacttime",
"Time of no DC Contact",
"",  "Time of no DC Contact",
23, false, 0, 1, 0, TS_TYPE},

{"totDC_contacttime_LN",
"Total DC Contact Time (LN)",
"",  "Total DC Contact Time (LN)",
24, false, 0, 1, 0, TS_TYPE},

{"totDC_contacttime_PER",
"Total DC Contact Time (PER)",
"",  "Total DC Contact Time (PER)",
25, false, 0, 1, 0, TS_TYPE},


// Profile plots

{"CD69",
"CD69 Profile",
"",  "CD69 Profile",
PROFILE_CD69, false, 0, 1, 1.0, PROF_TYPE},

{"S1PR1",
"S1PR1 Profile",
"",  "S1PR1 Profile",
PROFILE_S1PR1, true, 0, 1, 1.0, PROF_TYPE},

{"CFSE",
"CFSE Profile (log)",
"",  "CFSE Profile (log)",
PROFILE_CFSE, false, 0, 1, 1.0, PROF_TYPE},

{"Stimulation",
"Stimulation Profile (LN)",
"",  "Stimulation Profile (LN)",
PROFILE_STIM, true, 0, 1, 1.0, PROF_TYPE},

{"Stimulation rate",
"Stim Rate Profile (LN)",
"",  "Stim Rate Profile (LN)",
PROFILE_STIMRATE, false, 0, 1, 1.0, PROF_TYPE},

{"Avidity LN",
"Avidity Profile (LN)",
"",  "Avidity Profile (LN)",
PROFILE_AVIDITY_LN, true, 0, 1, 1.0, PROF_TYPE},

{"Avidity PER",
"Avidity Profile (PER)",
"",  "Avidity Profile (PER)",
PROFILE_AVIDITY_PER, false, 0, 1, 1.0, PROF_TYPE},

{"Generation LN",
"Generation Profile (LN)",
"",  "Generation Profile (LN)",
PROFILE_GENERATION_LN, false, 0, 1, 1.0, PROF_TYPE},

{"DC contact time (min)",
"First DC Contact Time Profile",
"",  "First DC Contact Time Profile",
PROFILE_FIRSTDCCONTACTTIME, false, 0, 1, 1.0, PROF_TYPE},

{"DC bind time (min)",
"DC Bind Time Profile",
"",  "DC Bind Time Profile",
PROFILE_DCBINDTIME, false, 0, 1, 1.0, PROF_TYPE}
*/
};
    // Note: tsGraphs[] is constant = tsGraphSet[]
    // .active is either true or false, depending on checkBox_selectgraph
    // Note: the "ts" prefix is misleading because the list can include profile plots.

    n_tsGraphs = sizeof(tsGraphSet)/sizeof(GRAPH_SET);
    tsGraphs = new GRAPH_SET[n_tsGraphs];
    for (int i=0; i<n_tsGraphs; i++) {
        tsGraphs[i] = tsGraphSet[i];
    }
    graphList = new GRAPH_SET[maxGraphs];
    nGraphs = maxGraphs;
}


GRAPH_SET Graphs::get_graph(int k)
{
	return graphList[k];
}

int Graphs::get_dataIndex(int k)
{
	return graphList[k].dataIndex;
}

QString Graphs::get_tag(int k)
{
	return graphList[k].tag;
}

QString Graphs::get_title(int k)
{
//    QString title;
//    if (!graphList[k].active)
//        title = "";
//    else
//        title = graphList[k].title;
    return graphList[k].title;
}

QString Graphs::get_yAxisTitle(int k)
{
//    QString title;
//    if (!graphList[k].active)
//        title = "";
//    else
//        title = graphList[k].yAxisTitle;
    return graphList[k].yAxisTitle;
}

double Graphs::get_maxValue(int k) {
	return graphList[k].maxValue;
}

double Graphs::get_scaling(int k) {
	return graphList[k].scaling;
}

double Graphs::get_yscale(int k) {
    return graphList[k].yscale;
}

double Graphs::get_xscale(double xmax) {
    int n = 1;
    for (;;) {
        if (xmax <= n) break;
        n++;
    }
    return double(n);
}

void Graphs::set_activity(int k, bool activity)
{
    graphList[k].active = activity;
}

bool Graphs::isActive(int k)
{
	return graphList[k].active;
}

/*
bool Graphs::isTimeseries(int k)
{
    return graphList[k].ts;
}

bool Graphs::isProfile(int k)
{
    return !graphList[k].ts;
}
*/

bool Graphs::isTimeseries(int k)
{
    return (graphList[k].type == TS_TYPE);
}

bool Graphs::isProfile(int k)
{
    return (graphList[k].type == PROF_TYPE);
}

bool Graphs::isDistribution(int k)
{
    return (graphList[k].type == DIST_TYPE);
}

void Graphs::set_maxValue(int k, double v)
{
	graphList[k].maxValue = v;
}

void Graphs::makeGraphList(int non_ts)
{
    int k = maxGraphs;
    int nts = 0;
    char msg[128];
//    sprintf(msg,"n_tsGraphs: %d",n_tsGraphs);
//    LOG_MSG(msg);
    for (int i=0; i<n_tsGraphs; i++) {
//        sprintf(msg,"tsGraphs: %d dataIndex: %d type: %d active: %d",i,tsGraphs[i].dataIndex,tsGraphs[i].type,tsGraphs[i].active);
//        LOG_QMSG(tsGraphs[i].tag);
//        LOG_MSG(msg);
        if (tsGraphs[i].active) {
            k--;
            graphList[k] = tsGraphs[i];
//            sprintf(msg,"active: tsGraphs index: %d  graphList index: %d",i,k);
//            LOG_MSG(msg);
            nts++;
            if (nts == maxGraphs - non_ts) break;
        }
    }
    int ndummy = maxGraphs - nts - non_ts;
    for (k=0; k<ndummy; k++) {
        graphList[k].tag = "dummy";
        graphList[k].active = false;
        graphList[k].type = TS_TYPE;
        graphList[k].scaling = 1;
    }
    for (k=ndummy; k<ndummy + non_ts; k++) {
        graphList[k].tag = "non_ts";
        graphList[k].active = true;
        graphList[k].type = DIST_TYPE;  //????
        graphList[k].scaling = 1;
    }
    nGraphs = maxGraphs;

//    sprintf(msg,"nGraphs: %d  non_ts: %d  nts: %d",nGraphs,non_ts,nts);
//    LOG_MSG(msg);
}

/*
void Graphs::makeGraphList-old()
{
    char msg[128];
    int k = maxGraphs;
    int nts = 0;
    for (int i=0; i<n_tsGraphs; i++) {
        if (tsGraphs[i].active) {
            k--;
            graphList[k] = tsGraphs[i];
            nts++;
            if (nts == maxGraphs) break;
        }
    }
    int ndummy = maxGraphs - nts;
    for (k=0; k<ndummy; k++) {
        graphList[k].active = false;
        graphList[k].ts = true;
        graphList[k].tag = "dummy";
        graphList[k].scaling = 1;
        graphList[k].title = "";
        graphList[k].yAxisTitle = "";
    }
    nGraphs = maxGraphs;
//    sprintf(msg,"nGraphs: %d",nGraphs);
//    LOG_MSG(msg);
//    for (k=0; k<nGraphs; k++) {
//        LOG_QMSG(graphList[k].tag);
//        sprintf(msg,"k: %d  active: %d  ts: %d",k,graphList[k].active,graphList[k].ts);
//        LOG_MSG(msg);
//    }
}
*/
