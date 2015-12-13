#include "cpuview.h"

#include <QGridLayout>

#include "core/cpu.h"

CPUView::CPUView(QWidget *parent)
    : QWidget(parent)
{
    auto *layout = new QGridLayout(this);

    static QString regnames[] = {
        QStringLiteral("R0"),
        QStringLiteral("R1"),
        QStringLiteral("R2"),
        QStringLiteral("R3"),
        QStringLiteral("R4"),
        QStringLiteral("R5"),
        QStringLiteral("R6"),
        QStringLiteral("R7"),
        QStringLiteral("R8"),
        QStringLiteral("R9"),
        QStringLiteral("R10"),
        QStringLiteral("R11"),
        QStringLiteral("R12"),
        QStringLiteral("SP"),
        QStringLiteral("LR"),
        QStringLiteral("PC") };

    for(unsigned int i = 0; i < 8; ++i)
    {
        layout->addWidget(new QLabel(regnames[i]), i, 0, 1, 1);
        layout->addWidget(labels[i] = new QLabel(), i, 1, 1, 1);
        layout->addWidget(new QLabel(regnames[i + 8]), i, 2, 1, 1);
        layout->addWidget(labels[i + 8] = new QLabel(), i, 3, 1, 1);
    }

    refresh_timer.setInterval(100);

    connect(&refresh_timer, &QTimer::timeout, this, &CPUView::refreshRegisters);
}

void CPUView::showEvent(QShowEvent *ev)
{
    autoRefresh(true);
    QWidget::showEvent(ev);
}

void CPUView::hideEvent(QHideEvent *ev)
{
    autoRefresh(false);
    QWidget::hideEvent(ev);
}

void CPUView::refreshRegisters()
{
    for(unsigned int i = 0; i < 16; ++i)
        labels[i]->setText(QStringLiteral("0x%1").arg(arm.reg[i], 8, 16, QLatin1Char('0')));
}

void CPUView::autoRefresh(bool enabled)
{
    if(enabled)
        refresh_timer.start();
    else
        refresh_timer.stop();
}
