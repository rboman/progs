#include <QWidget>
#include <QVBoxLayout>
#include <QPushButton>
#include <QTextEdit>
#include <QStringList>
#include <QProcess>

#include <iostream>
#include <vector>

// reads the output of a process containing escaped colours.
// & displays the output into a QTextEdit widget.

class GUI : public QWidget
{
    Q_OBJECT;
    QTextEdit *textedit;

public:
    GUI(QWidget *parent = 0) : QWidget(parent)
    {
        QVBoxLayout *vbox = new QVBoxLayout(this);
        textedit = new QTextEdit();
        vbox->addWidget(textedit);

        QPushButton *button = new QPushButton("Run process");
        vbox->addWidget(button);
        this->setLayout(vbox);
        this->setGeometry(300, 300, 350, 300);
        this->setWindowTitle("GUI");
        this->show();

        connect(button, SIGNAL(pressed()), this, SLOT(on_button_pressed()));
    }

public slots:
    void on_button_pressed()
    {
        std::cout << "running process...\n";
        textedit->append("running process...");

        QProcess process(this);
        process.start("python", QStringList()
                                    << "-c"
                                    << "print 'this is \033[31mred\033[0m.'");
        if (!process.waitForStarted())
            textedit->append("=> process didn't start :(");

        if (!process.waitForFinished())
            textedit->append("=> process failure :(");

        QByteArray result = process.readAllStandardOutput();
        std::cout << result.toStdString().c_str();

        QString s(result.toStdString().c_str());
        s.replace("\033[31m", "<font color=red>");
        s.replace("\033[0m", "</font>");
        textedit->append("output: " + s);
    }
};
