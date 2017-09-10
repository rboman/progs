//   Copyright 2017 Romain Boman
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

#include <QPainter>
#include <iostream>
#include <vector>

class Barres : public QWidget
{
    Q_OBJECT;

    static constexpr int nframes = 50;

    double pi;     
    
    int ox;
    int oy;


    double zoom;

    double a1; 
    double a2; 
    double a3; 
    double xb; 
    double ya; 
    double L; 
    double e; 
    double dp;

    int myTimerId;
    int frame;
public:
    Barres(QWidget* parent = 0);
protected: 
    virtual void paintEvent(QPaintEvent *event);
    virtual void timerEvent(QTimerEvent *event);
    virtual void showEvent(QShowEvent *event);
    virtual void hideEvent(QHideEvent *event);
};


