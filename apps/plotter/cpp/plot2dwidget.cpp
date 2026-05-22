#include "plot2dwidget.h"
#include <QPainter>
#include <QPen>
#include <QPolygon>
#include <QMouseEvent>
#include <QPaintEvent>
#include <QFont>
#include <QLine>
#include <cmath>
#include <algorithm>

Plot2DWidget::Plot2DWidget(QWidget* parent) : QWidget(parent) {
    margin_left = 70;
    margin_right = 30;
    margin_top = 45;
    margin_bottom = 55;

    xmin = -6.1123;
    xmax = 12.6;
    ymin = -1.2;
    ymax = 2.2;

    gridX = 10;
    gridY = 5;

    starttx = 0;
    startty = 0;

    // Modern light theme colors matching PyQt version
    color_bg = QColor("#F8F9FA");
    color_frame = QColor("#CCCCCC");
    color_grid = QColor("#E0E0E0");
    color_labels = QColor("#555555");
    color_title = QColor("#111111");
    color_curve = QColor("#2563EB");
    curve_width = 3;

    plot_title = "Tracé de la fonction f(x)";
    label_x_axis = "Axe X";
    label_y_axis = "Axe Y";

    setupGUI();
}

void Plot2DWidget::setupGUI() {
    setWindowTitle("Plot2DWidget");
    resize(800, 450); // 16:9 ratio
}

void Plot2DWidget::add(const Curve& curve, const QColor& color) {
    Curve c = curve;
    if (color.isValid()) {
        c.color = color;
    }
    curves.push_back(c);
    update();
}

void Plot2DWidget::autofit() {
    if (curves.empty()) {
        return;
    }

    std::vector<double> all_x;
    std::vector<double> all_y;
    for (const auto& c : curves) {
        for (const auto& pt : c.pts) {
            if (std::isfinite(pt.x) && std::isfinite(pt.y)) {
                all_x.push_back(pt.x);
                all_y.push_back(pt.y);
            }
        }
    }

    if (all_x.empty() || all_y.empty()) {
        return;
    }

    double min_x = *std::min_element(all_x.begin(), all_x.end());
    double max_x = *std::max_element(all_x.begin(), all_x.end());
    double min_y = *std::min_element(all_y.begin(), all_y.end());
    double max_y = *std::max_element(all_y.begin(), all_y.end());

    double dx = max_x - min_x;
    double dy = max_y - min_y;

    if (dx <= 0.0 || !std::isfinite(dx)) {
        dx = 2.0;
        min_x -= 1.0;
        max_x += 1.0;
    }

    if (dy <= 0.0 || !std::isfinite(dy)) {
        dy = 2.0;
        min_y -= 1.0;
        max_y += 1.0;
    }

    double margin_x = dx * 0.1;
    double margin_y = dy * 0.1;

    xmin = min_x - margin_x;
    xmax = max_x + margin_x;
    ymin = min_y - margin_y;
    ymax = max_y + margin_y;

    if (!std::isfinite(xmin) || !std::isfinite(xmax) ||
        !std::isfinite(ymin) || !std::isfinite(ymax)) {
        xmin = -10.0;
        xmax = 10.0;
        ymin = -10.0;
        ymax = 10.0;
    }

    update();
}

std::pair<double, double> Plot2DWidget::computegrid(double xmin_val, double xmax_val, int gridCount) {
    double dx = xmax_val - xmin_val;
    double dgX = dx / gridCount;

    double expo = std::floor(std::log10(dgX));
    double factor = std::pow(10.0, expo);
    double dgX3 = dgX / factor;

    int bdXi = 1;
    if (dgX3 < 1.5) {
        bdXi = 1;
    } else if (dgX3 < 3.5) {
        bdXi = 2;
    } else if (dgX3 < 7.5) {
        bdXi = 5;
    } else {
        bdXi = 10;
    }

    double bdXf = bdXi * factor;

    double xminf = xmin_val / factor;
    double xmini = std::floor(xminf);

    while (true) {
        // Safe integer mod check
        long long xmini_int = static_cast<long long>(std::round(xmini));
        if (xmini_int % bdXi == 0 && xmini >= xminf) {
            break;
        }
        xmini += 1.0;
    }
    xminf = xmini * factor;

    return { xminf, bdXf };
}

void Plot2DWidget::paintEvent(QPaintEvent* event) {
    Q_UNUSED(event);

    plot_rect = QRect(
        margin_left,
        margin_top,
        width() - margin_left - margin_right,
        height() - margin_top - margin_bottom
    );

    if (!plot_rect.isValid()) {
        return;
    }

    QPainter painter(this);
    painter.setRenderHint(QPainter::Antialiasing, true);
    painter.fillRect(rect(), color_bg);

    draw_frame(painter, plot_rect);
    draw_grid(painter, plot_rect);
    draw_curves(painter);
    draw_labels_and_title(painter, plot_rect);
}

void Plot2DWidget::draw_frame(QPainter& painter, const QRect& r) {
    QPen pen(color_frame, 1, Qt::SolidLine);
    painter.setPen(pen);
    painter.drawRect(r);
}

void Plot2DWidget::draw_grid(QPainter& painter, const QRect& r) {
    auto grid_x_data = computegrid(xmin, xmax, gridX);
    double xminf = grid_x_data.first;
    double bdXf = grid_x_data.second;

    auto grid_y_data = computegrid(ymin, ymax, gridY);
    double yminf = grid_y_data.first;
    double bdYf = grid_y_data.second;

    QFont font = painter.font();
    font.setPointSize(9);
    painter.setFont(font);

    // X axis ticks
    double x = xminf;
    while (x < xmax) {
        double x1 = r.left() + (x - xmin) / (xmax - xmin) * r.width();
        int y1 = r.bottom();
        double x2 = x1;
        int y2 = r.top();

        QPen pen(color_grid, 1, Qt::DashLine);
        painter.setPen(pen);
        QLine line(_to_pixel(x1), y1, _to_pixel(x2), y2);
        painter.drawLine(line);

        QPen label_pen(color_labels, 1, Qt::SolidLine);
        painter.setPen(label_pen);
        QString text = QString::number(x, 'g', 4);
        painter.drawText(_to_pixel(x1 - 50), r.bottom() + 5, 100, 20, Qt::AlignHCenter | Qt::AlignTop, text);

        x += bdXf;
    }

    // Y axis ticks
    double y = yminf;
    while (y < ymax) {
        int x1 = r.left();
        double y1 = r.bottom() - (y - ymin) / (ymax - ymin) * r.height();
        int x2 = r.right();
        double y2 = y1;

        QPen pen(color_grid, 1, Qt::DashLine);
        painter.setPen(pen);
        QLine line(x1, _to_pixel(y1), x2, _to_pixel(y2));
        painter.drawLine(line);

        QPen label_pen(color_labels, 1, Qt::SolidLine);
        painter.setPen(label_pen);
        QString text = QString::number(y, 'g', 4);
        painter.drawText(r.left() - 55, _to_pixel(y1 - 10), 50, 20, Qt::AlignRight | Qt::AlignVCenter, text);

        y += bdYf;
    }
}

void Plot2DWidget::draw_curves(QPainter& painter) {
    painter.save();
    painter.setClipRect(plot_rect);

    for (const auto& c : curves) {
        QPolygon poly;
        for (const auto& pt : c.pts) {
            auto [wx, wy] = ax2win(pt.x, pt.y);
            poly << QPoint(wx, wy);
        }

        QPen pen;
        if (c.color.isValid()) {
            pen.setColor(c.color);
        } else {
            pen.setColor(color_curve);
        }
        pen.setWidth(curve_width);
        painter.setPen(pen);
        painter.drawPolyline(poly);
    }

    painter.restore();
}

void Plot2DWidget::draw_labels_and_title(QPainter& painter, const QRect& r) {
    // 1. Draw Title
    QPen title_pen(color_title, 1, Qt::SolidLine);
    painter.setPen(title_pen);
    QFont title_font = painter.font();
    title_font.setPointSize(12);
    title_font.setBold(true);
    painter.setFont(title_font);
    painter.drawText(0, 10, width(), 30, Qt::AlignHCenter | Qt::AlignTop, plot_title);

    // 2. Draw X-axis label
    QPen legend_pen(color_labels, 1, Qt::SolidLine);
    painter.setPen(legend_pen);
    QFont legend_font = painter.font();
    legend_font.setPointSize(10);
    legend_font.setBold(false);
    painter.setFont(legend_font);
    painter.drawText(r.left(), r.bottom() + 28, r.width(), 20, Qt::AlignHCenter | Qt::AlignTop, label_x_axis);

    // 3. Draw Y-axis label (rotated -90 deg)
    painter.save();
    painter.translate(15, r.center().y());
    painter.rotate(-90);
    painter.drawText(-100, -10, 200, 20, Qt::AlignCenter, label_y_axis);
    painter.restore();
}

std::pair<int, int> Plot2DWidget::ax2win(double ax, double ay) const {
    double wx = plot_rect.left() + (ax - xmin) / (xmax - xmin) * plot_rect.width();
    double wy = plot_rect.bottom() - (ay - ymin) / (ymax - ymin) * plot_rect.height();
    return { _to_pixel(wx), _to_pixel(wy) };
}

std::pair<double, double> Plot2DWidget::win2ax(double wx, double wy) const {
    double ax = xmin + (wx - plot_rect.left()) * (xmax - xmin) / plot_rect.width();
    double ay = ymin - (wy - plot_rect.bottom()) * (ymax - ymin) / plot_rect.height();
    return { ax, ay };
}

int Plot2DWidget::_to_pixel(double value) const {
    return static_cast<int>(std::round(value));
}

void Plot2DWidget::mousePressEvent(QMouseEvent* event) {
    if (event->button() == Qt::LeftButton || event->button() == Qt::RightButton) {
        starttx = event->pos().x();
        startty = event->pos().y();
    }
}

void Plot2DWidget::mouseMoveEvent(QMouseEvent* event) {
    if (event->buttons() & Qt::LeftButton) {
        auto [x1, y1] = win2ax(starttx, startty);
        auto [x2, y2] = win2ax(event->pos().x(), event->pos().y());
        double dx = x1 - x2;
        double dy = y1 - y2;
        xmin += dx;
        ymin += dy;
        xmax += dx;
        ymax += dy;
        starttx = event->pos().x();
        startty = event->pos().y();
        update();
    } else if (event->buttons() & Qt::RightButton) {
        double dx = static_cast<double>(event->pos().x() - starttx);
        double dy = static_cast<double>(event->pos().y() - startty);
        double dz = std::sqrt(dx * dx + dy * dy);
        if (dz > 400.0) {
            dz = 400.0;
        }
        if (dx < 0) {
            dz = -dz;
        }
        double zoom = 1.0 + dz / 400.0;
        if (zoom < 0.0) {
            zoom = 1.0;
        }
        double cx = (xmin + xmax) / 2.0;
        double cy = (ymin + ymax) / 2.0;
        xmin = cx - (cx - xmin) * zoom;
        ymin = cy - (cy - ymin) * zoom;
        xmax = cx - (cx - xmax) * zoom;
        ymax = cy - (cy - ymax) * zoom;

        starttx = event->pos().x();
        startty = event->pos().y();
        update();
    }
}
