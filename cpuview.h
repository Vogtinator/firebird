#ifndef CPUVIEW_H
#define CPUVIEW_H

#include <QLabel>
#include <QTimer>

class CPUView : public QWidget
{
    Q_OBJECT
public:
    explicit CPUView(QWidget *parent = 0);

protected:
    virtual void showEvent(QShowEvent *ev) override;
    virtual void hideEvent(QHideEvent *ev) override;

public slots:
    void refreshRegisters();
    void autoRefresh(bool enabled);

private:
    QLabel *labels[16];
    QTimer refresh_timer;
};

#endif // CPUVIEW_H
