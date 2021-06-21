#ifndef MISC_H
#define MISC_H

#include <QThread>
#include <QTcpServer>
#include <QMutex>

#include "libcart.h"

class SocketHandler : public QThread
 {
    Q_OBJECT

public:
    SocketHandler(int newport, QObject *parent = 0);
    ~SocketHandler();
    void run();
	void stop();
	void end();

	QTcpServer *tcpServer;
	int port;
	bool exiting;
	bool stopped;
	quint16 blockSize;
	QTcpSocket *socket;
	char msg[2048];
	static const int CPORT0 = 5000;
	static const int CPORT1 = 5001;
private slots:
	 void processor();
signals:
	 void sh_connected();
	 void sh_disconnected();
	 void sh_output(QString);
	 void sh_error(QString);
//     void facs_update();
};

class ExecThread: public QThread
{
	Q_OBJECT
	QString inputFile;
public:
	ExecThread(QString);
	void run();
//    void snapshot();
//    void getProfiles();
//    void getFACS();
    void pause();
	void unpause();
	void stop();
	int ncpu;
	int nsteps;
	bool paused;
	bool stopped;
    int summary_interval;
    QMutex mutex1;
signals:
    void display();
	void summary();
    void update(int *);
//    void action_VTK();
    void redimension(int);
//    void facs_update();
};

bool quitMessage(QString);

#endif
