#ifndef SPH_QTVTKHOOK_H
#define SPH_QTVTKHOOK_H

#include "sph.h"
#include "sphDisplayHook.h"
#include <QApplication>


namespace sph
{
class DisplayWindow;

/// this class manages the Qt application.

class SPH_API QtVTKHook : public DisplayHook
{
    QApplication *app;
    DisplayWindow *window;
    Model &model;

public:
    QtVTKHook(int &argc, char **argv, Model &model);
    virtual ~QtVTKHook();
    virtual void interact() override;
    virtual void update_data() override;

    virtual void loop() override;

    static void standalone_VTK_demo(); // for testing
};

}; // namespace sph

#endif // SPH_QTVTKHOOK_H
