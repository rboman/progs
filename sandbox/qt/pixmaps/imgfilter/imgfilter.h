//   Copyright 2019 Romain Boman
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//       http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.

#include <QWidget>
#include <QColor>
//#include <QPen>
//#include <QPainter>
#include <iostream>
//#include <vector>
#include <QHBoxLayout>
#include <QMainWindow>
#include <QImage>
#include <QLabel>
#include <QString>
#include <QAction>
#include <QMenuBar>
#include <QMenu>
#include <QToolBar>
#include "config.h"


class MainWindow : public QMainWindow
{
    Q_OBJECT;

    QImage *img1;
    QImage *img2;

    QLabel *lbl1;
    QLabel *lbl2;

public:
    MainWindow(QWidget* parent = 0) : QMainWindow(parent), img1(nullptr), img2(nullptr)
    {
        this->setWindowTitle("Filters");
        this->initUI();
    }    


    void initUI()
    {
        QWidget *centralw = new QWidget();        
        QHBoxLayout *hbox = new QHBoxLayout(centralw);

        QString srcDir = CMAKE_SOURCE_DIR;
        std::cout << srcDir.toStdString() << '\n';

        QString imgfile = srcDir +"/../snipercat.jpg"; 
        this->img1 = new QImage(imgfile);
        this->img2 = new QImage(imgfile);

        this->lbl1 = new QLabel(centralw);
        this->lbl1->setPixmap(QPixmap::fromImage(*this->img1));
        hbox->addWidget(this->lbl1);
        this->lbl2 = new QLabel(centralw);
        this->lbl2->setPixmap(QPixmap::fromImage(*this->img2));
        hbox->addWidget(this->lbl2);
        centralw->setLayout(hbox);
        this->setCentralWidget(centralw);

        // action(s)
        QString iconfile = srcDir +"/../../exit.png";
        QAction *exitAct = new QAction(QIcon(iconfile), "Exit", this);

        //exitAct->setShortcut("Ctrl+Q");
        exitAct->setStatusTip("Exit application");
        connect(exitAct, &QAction::triggered, this, &QMainWindow::close);

        QAction *resetAct = new QAction("Reset", this);
        connect(resetAct, &QAction::triggered, this, &MainWindow::reset_image);

        QAction *filter1Act = new QAction("Filter 1 - grid", this);
        connect(filter1Act, &QAction::triggered, this, &MainWindow::filter1);
        QAction *filter2Act = new QAction("Filter 2 - mirror Y", this);
        connect(filter2Act, &QAction::triggered, this, &MainWindow::filter2);
        QAction *filter3Act = new QAction("Filter 3 - chg colour", this);
        connect(filter3Act, &QAction::triggered, this, &MainWindow::filter3);
        QAction *filter4Act = new QAction("Filter 4 - blur", this);
        connect(filter4Act, &QAction::triggered, this, &MainWindow::filter4);

        // menu
        QMenuBar *menubar = this->menuBar();

        QMenu *fileMenu = menubar->addMenu("&File");
        fileMenu->addAction(exitAct);

        QMenu *filterMenu = menubar->addMenu("Filters");
        filterMenu->addAction(resetAct);
        filterMenu->addAction(filter1Act);
        filterMenu->addAction(filter2Act);
        filterMenu->addAction(filter3Act);
        filterMenu->addAction(filter4Act);

        // status bar
        this->statusBar();


        // toolbar
        QToolBar *toolbar = this->addToolBar("Exit");
        toolbar->addAction(exitAct);

        this->setGeometry(300, 300, 350, 250);
        this->setWindowTitle("snipercat");
        this->show();
    }
private:
    void update_image()
    {
        this->lbl2->setPixmap(QPixmap::fromImage(*this->img2));
    }

public slots:
    void reset_image()
    {
        std::cout << "reset\n";
        *this->img2 = this->img1->copy();
        this->update_image();
    }

    void filter1()
    {
        std::cout << "filter1 - add grid\n";
        *this->img2 = this->img1->copy();

        for(int i=0; i<this->img1->width(); i+=2)
            for(int j=0; j<this->img1->height(); j+=2)
                this->img2->setPixelColor(i, j, QColor(255,0,0));
        this->update_image();
    }
    void filter2()
    {
        std::cout << "filter2 - mirror Y\n";
        *this->img2 = this->img1->mirrored();
        this->update_image();
    }


    void filter3()
    {
        std::cout << "filter3 - chg colour\n";
        *this->img2 = this->img1->copy();
        for(int i=0; i<this->img1->width(); i+=1)
            for(int j=0; j<this->img1->height(); j+=1)
            {
                QColor col = this->img1->pixelColor(i,j);
                //newcol = col.darker()
                //newcol = col.lighter()
                //mean = (col.red() + col.green() + col.blue())/3
                int mean = col.value();
                QColor newcol(mean, mean, mean);
                this->img2->setPixelColor(i, j, newcol);
            }
        this->update_image();
    }

    void filter4()
    {
        std::cout << "filter4 - blur\n";
        *this->img2 = this->img1->copy();

        const int iK = 5;
        const int jK = 5;        
        double K[iK][jK];

        for(int i=0; i<iK; ++i) 
            for(int j=0; j<iK; ++j) 
                K[i][j]=1.0;
        int ni = (iK-1)/2;
        int nj = (jK-1)/2;

        for(int i=0; i<this->img1->width(); i+=1)
            for(int j=0; j<this->img1->height(); j+=1)
            {
                double r=0.0;
                double g=0.0;
                double b=0.0;
                double np=0.0;
                for(int k=-ni; k<ni+1; ++k)
                    for(int l=-nj; l<nj+1; ++l) 
                        if(this->img1->valid(i+k,j+l))
                        {
                            QColor col = this->img1->pixelColor(i+k,j+l);
                            double w = K[k+ni][l+nj];
                            r+= col.redF()*w;
                            g+= col.greenF()*w;
                            b+= col.blueF()*w;
                            np+=w;
                        }
                r/=np;
                g/=np;
                b/=np;
                QColor newcol; newcol.setRgbF(r,g,b);
                this->img2->setPixelColor(i, j, newcol);               
            }

        this->update_image();
    }

/*        
:
        for i in xrange(0,self.img1.width()):
            for j in xrange(0,self.img1.height()):
                r=0.0
                g=0.0
                b=0.0
                np=0.0
                for k in xrange(-ni,ni+1):
                    for l in xrange(-nj,nj+1): 
                        if(self.img1.valid(i+k,j+l)):
                            col = self.img1.pixelColor(i+k,j+l)
                            w = K[k+ni,l+nj]
                            r+= col.redF()*w
                            g+= col.greenF()*w
                            b+= col.blueF()*w
                            np+=w
                r/=np
                g/=np
                b/=np
                newcol = QColor()
                newcol.setRgbF(r,g,b)
                self.img2.setPixelColor(i, j, newcol)
        self.update_image()
*/

};








/*
class Fract : public QWidget
{
    Q_OBJECT;

    double x1;
    double y1;
    double x2;
    double y2;
    int nb_coul;

public:
    Fract(QWidget* parent = 0) : QWidget(parent)
    {
        x1 = -2.0;
        y1 =  1.2;
        x2 =  1.5;
        y2 = -1.2;
        nb_coul = 50;

        this->setWindowTitle("Mandelbrot (Qt/C++)");
        this->resize(640, 480);
    }

public slots: 
    virtual void paintEvent(QPaintEvent *event)
    {
        std::cout << "paintEvent: please wait...\n";
        QPainter *painter = new QPainter(this);

        // setup palette
        std::vector<QColor> colours;
        for(int n=0; n<nb_coul; ++n)
            colours.push_back(QColor((255*n)/nb_coul, 0, 0));
        colours.push_back(QColor(0, 0, 0));

        double a1 = x2-x1;
        double a2 = y2-y1;
        int sl = this->width();
        int sh = this->height();


        for(int xe=0; xe<sl; ++xe)
            for(int ye=0; ye<sh; ++ye)
            {
                double xc = (a1*xe)/sl+x1;
                double yc = (a2*ye)/sh+y1;

                int n = 0;
                double xn = 0.0;
                double yn = 0.0;

                while (n<nb_coul && yn*yn+xn*xn<4.0)
                {
                    double xn2 = xn*xn-yn*yn+xc;
                    double yn2 = 2*xn*yn+yc;
                    xn=xn2; yn=yn2;
                    n++;
                }
                QPen pen;
                pen.setColor(colours[n]);
                painter->setPen(pen);
                painter->drawPoint(xe, ye);

            }
            std::cout << "done.\n";
        painter->end();   // avoids "qpainter : cannot destroy paint device that is being painted" 
    }
};
*/

