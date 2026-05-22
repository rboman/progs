#ifndef PLOT2DWIDGET_H
#define PLOT2DWIDGET_H

#include <QWidget>
#include <QColor>
#include <QRect>
#include <QString>
#include <vector>
#include <utility>
#include "core.h"

class QPaintEvent;
class QMouseEvent;
class QPainter;

class Plot2DWidget : public QWidget {
    Q_OBJECT
public:
    explicit Plot2DWidget(QWidget* parent = nullptr);

    void add(const Curve& curve, const QColor& color = QColor());
    void autofit();

    QString plot_title;
    QString label_x_axis;
    QString label_y_axis;

protected:
    void paintEvent(QPaintEvent* event) override;
    void mousePressEvent(QMouseEvent* event) override;
    void mouseMoveEvent(QMouseEvent* event) override;

private:
    int margin_left;
    int margin_right;
    int margin_top;
    int margin_bottom;

    double xmin;
    double xmax;
    double ymin;
    double ymax;

    int gridX;
    int gridY;

    std::vector<Curve> curves;
    QRect plot_rect;
    int starttx;
    int startty;

    QColor color_bg;
    QColor color_frame;
    QColor color_grid;
    QColor color_labels;
    QColor color_title;
    QColor color_curve;
    int curve_width;

    void setupGUI();
    std::pair<double, double> computegrid(double xmin, double xmax, int gridCount);
    void draw_frame(QPainter& painter, const QRect& rect);
    void draw_grid(QPainter& painter, const QRect& rect);
    void draw_curves(QPainter& painter);
    void draw_labels_and_title(QPainter& painter, const QRect& rect);

    std::pair<int, int> ax2win(double ax, double ay) const;
    std::pair<double, double> win2ax(double wx, double wy) const;
    int _to_pixel(double value) const;
};

#endif // PLOT2DWIDGET_H
