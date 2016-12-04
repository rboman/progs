
#include <QWidget>
#include <QColor>
#include <QPen>
#include <QPainter>

#include <iostream>
#include <vector>

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

        this->setWindowTitle("Mandelbrot (qt/c++)");
        this->resize(640, 480);
    }

public slots: 
    virtual void paintEvent( QPaintEvent * event )
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
    }
};
