#ifndef GRAPHS_H
#define GRAPHS_H

#include "profile.h"

#define maxGraphs 4     // 16 This is the number of graphs that can be displayed (tiled)

#define TS_TYPE 0
#define PROF_TYPE 1
#define DIST_TYPE 2

struct graph_set {
	QString tag;
	QString title;
	QString yAxisTitle;
    QString description;
	int dataIndex;		// this must be consistent with the ordering of summary_data[]
	bool active;		// false for dummy graphs
	double maxValue;
	double scaling;		// multiplying factor for scaling of summary_data
    double yscale;      // if != 0, this is the yscale for the plot
    int type;           // 0 = time-series, 1 = profile, 2 = distribution
//    bool ts;
};

typedef graph_set GRAPH_SET;

class Graphs
{
	GRAPH_SET *graphList;

public:

	Graphs();
	~Graphs();
    GRAPH_SET *tsGraphs;
    GRAPH_SET get_graph(int);
    int n_tsGraphs;
    int nGraphs;
	int get_dataIndex(int);
	QString get_tag(int);
	QString get_title(int);
	QString get_yAxisTitle(int);
	double get_maxValue(int);
	double get_scaling(int);
    double get_yscale(int);
    double get_xscale(double xmax);
    void set_activity(int, bool);
    bool isActive(int);
    bool isTimeseries(int);
    bool isProfile(int);
    bool isDistribution(int);
    void set_maxValue(int, double);
    void makeGraphList(int);
//    void makeGraphList();

};

#endif
