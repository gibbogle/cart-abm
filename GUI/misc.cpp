#include <string>
#include <fstream>
#ifdef _WIN32
#include <windows.h>
#endif
#include <QTcpServer>
#include <QTcpSocket>
#include <QtGui>
#include <QTcpServer>
#include <QMessageBox>

#include "misc.h"
#include "log.h"
#include "transfer.h"

#include "libcart.h"

LOG_USE();
char msg[2048];

class SleeperThread : public QThread
{
public:
    static void msleep(unsigned long msecs)
    {
        QThread::msleep(msecs);
    }
};

//-----------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------
SocketHandler::SocketHandler(int newport, QObject *parent)
	: QThread(parent)
{
    exiting = false;
    port = newport;
	sprintf(msg,"SocketHandler: port: %d",port);
	LOG_MSG(msg);
}

//-----------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------
SocketHandler::~SocketHandler() // make sure the worker object is destroyed
{
    exiting = true;
    wait();
}

//-----------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------
void SocketHandler::stop()
{
	LOG_MSG("SocketHandler::stop: set stopped");
	stopped = true;
}

//-----------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------
void SocketHandler::run()
{
	sprintf(msg,"run: port: %d", port);
	LOG_MSG(msg);
	quint16 qport = port;
	QString addressStr = "127.0.0.1";
	QHostAddress hostAddress;
	hostAddress.setAddress(addressStr);
    tcpServer = new QTcpServer(this);
	stopped = false;
	connect(tcpServer, SIGNAL(newConnection()), this, SLOT(processor()), Qt::DirectConnection);
    if (!tcpServer->listen(hostAddress,qport)) {
		sprintf(msg,"Unable to start the server: port: %d", port);
		LOG_MSG(msg);
        return;
    }
	sprintf(msg,"Listening on port: %d",tcpServer->serverPort());
	LOG_MSG(msg);
	LOG_MSG("serverAddress:");
	LOG_QMSG((tcpServer->serverAddress()).toString());
	bool timedOut = false;
	tcpServer->waitForNewConnection(-1,&timedOut);
	exec();
}

//-----------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------
void SocketHandler::processor()
{
//	LOG_MSG("In processor");
    socket = tcpServer->nextPendingConnection();
    sprintf(msg,"got server connection: %p",socket);
	LOG_MSG(msg);
    emit sh_connected();
	QString qdata;
	QByteArray ba;
	ba.resize(1024);
	while (true) {
		if (stopped) {
			LOG_MSG("Stopped!");
			break;
		}
		socket->waitForReadyRead(100);
		int nb = socket->bytesAvailable();
		if (nb > 0) {
			ba = socket->readLine(1024);
			qdata = QString(ba);
			QStringList s = qdata.split("^",QString::SkipEmptyParts);
			for (int k=0; k<s.length(); k++) {
				emit sh_output(s[k]); // Emit signal to update GUI
				if (port == CPORT0) {
					LOG_QMSG(s[k]);
				}
			}
			if (quitMessage(qdata)) {
				sprintf(msg,"Closing connection: port: %d", port);
				LOG_MSG(msg);
		        break;
			} else {
//				LOG_MSG("No bytes yet");
			}
		}
	}
	socket->close();
	tcpServer->close();
	if (port == CPORT0) {
		emit sh_disconnected();		// Is it right that both threads emit this?
	}
}

//-----------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------
ExecThread::ExecThread(QString infile)
{
	inputFile = infile;
}

//-----------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------
void ExecThread::run()
{
    int res = 0;
    int nt, ntum, nIDs;
	const char *infile, *outfile;
	QString infile_path, outfile_path;
    int len_infile, len_outfile, k;
    int data[4];

    LOG_MSG("Invoking DLL...");
    infile_path = inputFile;
	QString casename = QFileInfo(inputFile).baseName();
	len_infile = infile_path.length();
	std::string std_infile = infile_path.toStdString();
	infile = std_infile.c_str();
	outfile_path = casename.append(".res");
	len_outfile = outfile_path.length();
	std::string std_outfile = outfile_path.toStdString();
	outfile = std_outfile.c_str();

	paused = false;
	execute(&ncpu,const_cast<char *>(infile),&len_infile,const_cast<char *>(outfile),&len_outfile);
//	get_dimensions(&NX,&NY,&NZ);
//	mutex1.lock();
//	get_summary(summaryData);
//    getProfiles();
//    mutex1.unlock();
//	emit summary();		// Emit signal to update summary plots

    sprintf(msg,"run: nsteps: %d", nsteps);
    LOG_MSG(msg);
    get_initial_values(data);
    mutex1.lock();
    emit update(data);

    for (int i=1; i<= nsteps; i++) {
		bool updated = false;
		if (paused && !updated) {
			updated = true;
		}
		while(paused || leftb) {
			Sleep(100);
		}
		if (stopped) break;
        simulate_step(&nt,&ntum,&nIDs,&res);
        Sleep(5);
        sprintf(msg,"simulate: i: %d nt: %d ntum: %d nIDs: %d",i,nt,ntum,nIDs);
        LOG_MSG(msg);
        data[0] = i;
        data[1] = nt;
        data[2] = ntum;
        data[3] = nIDs;
        emit update(data);
        mutex1.lock();
        if (res == 1) break;
		if (stopped) break;
	}
    Sleep(100);
    terminate_run(&res);
	return;
}

/*
//-----------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------
void ExecThread::snapshot()
{
    int nTC_size, nDC_size, nbond_size;

    mutex2.lock();
//    get_scene_dimensions(&nTC_size,&nDC_size,&nbond_size);
//    sprintf(msg,"nTC_size, nDC_size, nbond_size: %d %d %d",nTC_size, nDC_size, nbond_size);
//    LOG_MSG(msg);
    get_scene(&nTC_list,TC_list,&nDC_list,DC_list,&nbond_list,bond_list);
    if (nTC_list > MAX_TC) {
		LOG_MSG("Error: MAX_TC exceeded");
		exit(1);
	}
	if (nDC_list > MAX_DC) {
		LOG_MSG("Error: MAX_DC exceeded");
		exit(1);
	}
	if (nbond_list > MAX_BOND) {
		LOG_MSG("Error: MAX_BOND exceeded");
		exit(1);
	}
//    mutex2.unlock();

    emit display(); // Emit signal to update VTK display
}

//-----------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------
void ExecThread::getProfiles()
{
    int k;

    k = PROFILE_CD69;
    get_profile_cd69(profile_x[k],profile_y[k],&profile_n[k]);
    k = PROFILE_S1PR1;
    get_profile_s1pr1(profile_x[k],profile_y[k],&profile_n[k]);
    k = PROFILE_CFSE;
    get_profile_cfse(profile_x[k],profile_y[k],&profile_n[k]);
    k = PROFILE_STIM;
    get_profile_stim(profile_x[k],profile_y[k],&profile_n[k]);
    k = PROFILE_STIMRATE;
    get_profile_stimrate(profile_x[k],profile_y[k],&profile_n[k]);
    k = PROFILE_AVIDITY_LN;
    get_profile_avidity_ln(profile_x[k],profile_y[k],&profile_n[k]);
    k = PROFILE_AVIDITY_PER;
    get_profile_avidity_per(profile_x[k],profile_y[k],&profile_n[k]);
    k = PROFILE_GENERATION_LN;
    get_profile_generation_ln(profile_x[k],profile_y[k],&profile_n[k]);
    k = PROFILE_FIRSTDCCONTACTTIME;
    get_profile_firstdccontacttime(profile_x[k],profile_y[k],&profile_n[k]);
    k = PROFILE_DCBINDTIME;
    get_profile_dcbindtime(profile_x[k],profile_y[k],&profile_n[k]);
}

////-----------------------------------------------------------------------------------------
////-----------------------------------------------------------------------------------------
//void ExecThread::getFACS()
//{
//    get_nfacs(&nFACS_cells);
//    if (!FACS_data || nFACS_cells*nFACS_vars > nFACS_dim) {
//        if (FACS_data) free(FACS_data);
//        nFACS_dim = 3*nFACS_cells*nFACS_vars;
//        FACS_data = (double *)malloc(nFACS_dim*sizeof(double));
//    }
//    get_facs(FACS_data);
//}
*/
//-----------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------
void ExecThread::stop()
{
	stopped = true;
}

	//-----------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------
void ExecThread::pause()
{
	paused = true;
}

//-----------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------
void ExecThread::unpause()
{
	paused = false;
}

//-----------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------
bool quitMessage(QString msg)
{
	if (msg.contains("__EXIT__",Qt::CaseSensitive))
		return true;
	else
		return false;
}
