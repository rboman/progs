
#include <QtWidgets/QApplication>
#include <QtWidgets/QDirModel>
#include <QtWidgets/QListView>

int
main(int argc, char *argv[])
{
    QApplication app(argc, argv);

    QDirModel model;
    // model.setNameFilters(QStringList("*.cpp"));
    model.setFilter(QDir::Files);
    QListView view;
    view.setModel(&model);
    view.setRootIndex(model.index(QDir::currentPath()));
    view.show();

    return app.exec();
}
