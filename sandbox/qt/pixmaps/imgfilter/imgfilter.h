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
#include <iostream>
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
    MainWindow(QWidget *parent = 0)
        : QMainWindow(parent), img1(nullptr), img2(nullptr)
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

        QString imgfile = srcDir + "/../snipercat.jpg";
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
        QString iconfile = srcDir + "/../../exit.png";
        QAction *exitAct = new QAction(QIcon(iconfile), "Exit", this);

        // exitAct->setShortcut("Ctrl+Q");
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

        for (int i = 0; i < this->img1->width(); i += 2)
            for (int j = 0; j < this->img1->height(); j += 2)
                this->img2->setPixelColor(i, j, QColor(255, 0, 0));
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
        for (int i = 0; i < this->img1->width(); i += 1)
            for (int j = 0; j < this->img1->height(); j += 1)
            {
                QColor col = this->img1->pixelColor(i, j);
                // newcol = col.darker()
                // newcol = col.lighter()
                // mean = (col.red() + col.green() + col.blue())/3
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

        for (int i = 0; i < iK; ++i)
            for (int j = 0; j < iK; ++j)
                K[i][j] = 1.0;
        int ni = (iK - 1) / 2;
        int nj = (jK - 1) / 2;

        for (int i = 0; i < this->img1->width(); i += 1)
            for (int j = 0; j < this->img1->height(); j += 1)
            {
                double r = 0.0;
                double g = 0.0;
                double b = 0.0;
                double np = 0.0;
                for (int k = -ni; k < ni + 1; ++k)
                    for (int l = -nj; l < nj + 1; ++l)
                        if (this->img1->valid(i + k, j + l))
                        {
                            QColor col = this->img1->pixelColor(i + k, j + l);
                            double w = K[k + ni][l + nj];
                            r += col.redF() * w;
                            g += col.greenF() * w;
                            b += col.blueF() * w;
                            np += w;
                        }
                r /= np;
                g /= np;
                b /= np;
                QColor newcol;
                newcol.setRgbF(r, g, b);
                this->img2->setPixelColor(i, j, newcol);
            }

        this->update_image();
    }
};
