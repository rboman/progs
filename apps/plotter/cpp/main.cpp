#include <QApplication>
#include <cmath>
#include "core.h"
#include "plot2dwidget.h"

// First math function: sin(2x) + cos(4x)
double fct(double x) {
    return std::sin(2.0 * x) + std::cos(4.0 * x);
}

// Second math function: 1.5 * sin(x)
double fct2(double x) {
    return std::sin(x) * 1.5;
}

// Helper to fill a Curve with function f over a range and add it to the widget
void plot_curve(Plot2DWidget& widget, std::function<double(double)> f, std::pair<double, double> rng, int n, const QColor& color) {
    Curve c;
    c.fill(f, rng, n);
    widget.add(c, color);
}

int main(int argc, char* argv[]) {
    QApplication app(argc, argv);

    Plot2DWidget win;
    win.plot_title = "Tracé multi-fonctions";

    // Tracé de la première fonction en bleu indigo moderne
    plot_curve(win, fct, {-1.5, 10.0}, 150, QColor("#2563EB"));

    // Tracé de la deuxième fonction en orange vif
    plot_curve(win, fct2, {-1.5, 10.0}, 150, QColor("#EA580C"));

    // Ajustement automatique du zoom au démarrage pour rendre visibles toutes les courbes
    win.autofit();

    win.show();

    return app.exec();
}
