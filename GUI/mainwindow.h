/****************************************************************************

****************************************************************************/

#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <string>
#include <fstream>
#include <QTcpServer>
#include <QTcpSocket>

#include <QButtonGroup>
#include "QImage"

using namespace std;

#include "ui_ABM_GUI.h"
#include <qwt_plot_curve.h>
#include <qwt_plot_marker.h>
#include <qwt_symbol.h>
#include "params.h"
#include "misc.h"
#include "plot.h"
//#include "myvtk.h"
#include "result_set.h"
#include "log.h"
#include "qmycheckbox.h"
//#include "SimpleView2DUI.h"
//#include "SimpleView3DUI.h"
//#include "qvideooutput.h"

QT_BEGIN_NAMESPACE
class QAction;
class QMenu;
class QPlainTextEdit;
class QMdiArea;
class QTcpServer;
class QTcpSocket;
QT_END_NAMESPACE

#define MAX_DATA 64
#define VTK_SOURCE 0
#define QWT_SOURCE 1

class SliderPlus 
{
	QString name;
	int pindex;
	int windex;
	double vmin;
	double vmax;
	double dv;
	int n;

public:

	SliderPlus(QString, double, double, int,int, int);
	~SliderPlus();
	int val_to_int(double);
	double int_to_val(int);
	QString val_to_str(double);
	double str_to_val(QString);
	int pIndex();
	int wIndex();
	int nTicks();
};

struct video_str {
    int recording;
    bool started;
};

class MainWindow : public QMainWindow, private Ui::MainWindow
{
    Q_OBJECT

public:
    MainWindow(QWidget *parent = 0);

	char msg[2048];

protected:
    void closeEvent(QCloseEvent *event);

private slots:
//    void on_rbut_ACTIVATION_MODE_0_toggled(bool);
//    void on_cbox_record_toggled(bool);
//	void on_action_show_gradient2D_triggered();
//	void on_action_show_gradient3D_triggered();
//    void on_action_FACS_triggered();
//    void on_line_SPECIAL_CASE_textEdited(QString);
//    void on_checkBox_FACS_PLOT_toggled(bool checked);
//    void on_checkBox_EFFECTOR_FUNCTION_toggled(bool checked);
    void newFile();
    void open();
    void about();
    void documentWasModified();

    bool save();
    bool saveAs();
	void readInputFile();
//	void loadResultFile();
    void goToInputs();
    void goToOutputs();
//    void goToVTK();
//    void goToFACS();
    void runServer();
    void pauseServer();
    void stopServer();
	void changeParam();
	void redrawDistPlot();
	void showMore(QString);
	void updateSliderBox();
    void radioButtonChanged(QAbstractButton *);

	double getMaximum(RESULT_SET *, double *);
	void addGraph();
	void removeGraph();
	void removeAllGraphs();
//	void playVTK();
//	void setVTKSpeed();
	void saveSnapshot();
	void setSavePosStart();

public slots:
	void preConnection();
	void outputData(QString);
    void postConnection();
//	void timer_update();
	void errorPopup(QString);
    void displayScene();
	void showSummary();
    void UpdateGraphs(int *data);
    void redimensionCellArrays(int nbond_size);
//    bool getVideoFileInfo(int *nframes, QString *itemFormat, QString *itemCodec, QString *videoFileName);
    void processGroupBoxClick(QString);
signals:

private:
    void createActions();
	void createLists();
    void initDistPlots();
    void setupParamList();
	void loadParams();
	void reloadParams();

//	void enableInVitro();
//	void disableInVitro();
    void writeout();
	void execute_para();
	void read_cell_positions();
	void close_sockets();
	void compareOutputs();
	void clearAllGraphs();
	void initializeGraphs(RESULT_SET *);
	void drawGraphs();
	QString selectResultSet();
    int selectGraphCase();
    void setupGraphSelector();
    void setGraphsActive();
    void showGradient2D();
	void showGradient3D();

    void showmdiAreaSize();
    double erf(double z);
    double pnorm(double x1, double x2, double mu, double sig);
    double plognorm(double x1, double x2, double mu, double sig);
    void create_lognorm_dist(double p1, double p2,int n, double *x, double *prob);
    void create_hill_function(int hill_N, double hill_C, int n, double *x, double *hill);
    int dist_limit(double *p, int n);
	QString parse_rbutton(QString wtag, int *rbutton_case);

	PARAM_SET get_param(int);

    void checkMemory(QString loc);

    void createMenus();
    void createToolBars();
    void createStatusBar();
    void readSettings();
    void writeSettings();
    bool maybeSave();
    void loadFile(const QString &fileName);
//    bool saveFile(const QString &fileName);
    void setCurrentFile(const QString &fileName);
    QString strippedName(const QString &fullFileName);

    QPlainTextEdit *textEdit;
    QString curFile;

    QMenu *fileMenu;
    QMenu *editMenu;
    QMenu *helpMenu;
    QToolBar *fileToolBar;
    QToolBar *editToolBar;
    QAction *newAct;
    QAction *openAct;
    QAction *saveAct;
    QAction *saveAsAct;
    QAction *exitAct;
    QAction *cutAct;
    QAction *copyAct;
    QAction *pasteAct;
    QAction *aboutAct;
    QAction *aboutQtAct;

	QList<QLineEdit *> lineEdit_list;
	QList<QSpinBox *> spin_list;
	QList<QComboBox *> combo_list;
	QList<QCheckBox *> checkbox_list;
	QList<QRadioButton *> radiobutton_list;
	QList<QSlider *> slider_list;
	QList<QLabel *> label_list;
	QList<SliderPlus *> sliderplus_list;
	QList<QWidget *> sliderParam;
	QList<RESULT_SET *> result_list;

    QwtPlot *distplot_list[7];
    QwtPlotCurve *curve_list[7];

	QList<QWidget *> widget_list;

//    QwtPlot *qpFACS;
//    QwtPlotCurve *curveFACS;

	int nDistPts;
	int nTicks;
	int nParams;
	int nSliders;
	int nWidgets;
	int nLabels;
	int *param_to_sliderIndex;
	bool paramSaved;
	bool paused;
	bool posdata;
	bool DCmotion;
	bool done;
	bool first;
	bool started;
	int currentDescription;
	QString defaultInputFile;
	QString inputFile;
//	QString stopfile;
//	QString pausefile;
	QString cellfile;
//	QString dll_path;
	QTextBrowser *box_outputData;
	SocketHandler *sthread0;
	SocketHandler *sthread1;
	QTimer *timer;

	int step;
	int ntimes;
    int framenum;
	int savepos_start;
	int ncpu;
	double hours;
	double hour;
	int progress;
	int nGraphs;		// act, ntot_LN, ncog_PER, ...
	int nGraphCases;

	RESULT_SET *newR;

	Plot *graph_dummy;	// placeholder

    Plot *pGraph[MAX_DATA];
    QMyCheckBox **cbox_ts;
//    QCheckBox **cbox_ts;

	QString graphCaseName[Plot::ncmax];
	RESULT_SET *graphResultSet[Plot::ncmax];
	static const bool show_outputdata = false;
	static const bool use_CPORT1 = false;

	static const int CPORT0 = 5000;
	static const int CPORT1 = 5001;
	static const bool USE_RANGES = false;

//	MyVTK *vtk;
	ExecThread *exthread;

//    QVideoOutput   * videoVTK;
//    QVideoOutput   * videoFACS;

};

class MyDoubleValidator : public QDoubleValidator
{
public:
	MyDoubleValidator( double bottom, double top, int decimals, QObject* parent = 0)
		: QDoubleValidator( bottom, top, decimals, parent)
	{}

	QValidator::State validate ( QString &input, int &pos ) const
	{
		if ( input.isEmpty() || input == "." ) {
			return Intermediate;
		}
		bool ok;
		double entered = input.toDouble(&ok);
		if (!ok) return Invalid;
		if (entered < bottom())
			return Intermediate;
		if ( QDoubleValidator::validate( input, pos ) != Acceptable ) {
			return Invalid;
		}
		return Acceptable;
	}
};

static const double DELTA_T = 6;    // hours

#endif
