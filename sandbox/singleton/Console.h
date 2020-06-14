#ifndef CONSOLE_H
#define CONSOLE_H

class Console
{
public:
    virtual ~Console() {}
    virtual void setColor(int color);
    virtual void restoreColor();
};

#endif // CONSOLE_H
