#include <QtGui>
#include <QtWidgets>

class MainWindow : public QMainWindow
{
public:
    MainWindow();

private:
    void create_actions_();
    void create_menus_();
    void about_();
    void dummy_();

    QMenuBar *menu_bar_;
    QMenu *file_menu_;
    QMenu *help_menu_;
    QAction *action_about_;
    QAction *action_dummy_;
};

MainWindow::MainWindow()
{
    resize(800, 600);

    create_actions_();
    create_menus_();
}

void
MainWindow::create_actions_()
{
    action_about_ = new QAction(tr("About"), this);
    action_dummy_ = new QAction(tr("Dummy"), this);
    connect(action_about_, &QAction::triggered, this, &MainWindow::about_);
    connect(action_dummy_, &QAction::triggered, this, &MainWindow::dummy_);
}

void
MainWindow::create_menus_()
{
    menu_bar_ = new QMenuBar(this);

    file_menu_ = new QMenu(tr("&File"));
    file_menu_->addAction(action_dummy_);
    menu_bar_->addAction(file_menu_->menuAction());

    help_menu_ = new QMenu(tr("&Help"));
    help_menu_->addAction(action_about_);

    menu_bar_->addAction(help_menu_->menuAction());

    menu_bar_->setNativeMenuBar(true);
}

void
MainWindow::about_()
{
    QMessageBox::about(this, tr("About"), tr("FooBar"));
}

void
MainWindow::dummy_()
{
    QMessageBox::about(this, tr("Dummy"), tr("Dummy"));
}

int
main(int argc, char **argv)
{
    QApplication app(argc, argv);

    MainWindow main_window;
    main_window.show();

    return app.exec();
}
