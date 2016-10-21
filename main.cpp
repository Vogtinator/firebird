#include <QApplication>
#include <QTranslator>
#include <QtQml>

#ifndef MOBILE_UI
#include "mainwindow.h"
#else
#include <QQuickView>
#endif

#include "qtframebuffer.h"
#include "qmlbridge.h"

int main(int argc, char **argv)
{
    QApplication app(argc, argv);

    QTranslator appTranslator;
    appTranslator.load(QLocale::system().name(), QStringLiteral(":/i18n/i18n/"));
    app.installTranslator(&appTranslator);

    QCoreApplication::setOrganizationName(QStringLiteral("ndless"));
    QCoreApplication::setApplicationName(QStringLiteral("firebird"));
    app.setAttribute(Qt::AA_UseHighDpiPixmaps);

    // Register QMLBridge for Keypad<->Emu communication
    qmlRegisterSingletonType<QMLBridge>("Firebird.Emu", 1, 0, "Emu", qmlBridgeFactory);
    // Register QtFramebuffer for QML display
    qmlRegisterType<QMLFramebuffer>("Firebird.Emu", 1, 0, "EmuScreen");

    #ifndef MOBILE_UI
        MainWindow mw;
        main_window = &mw;
        mw.show();
    #else
        QQuickView mobile_ui;
        mobile_ui.engine()->addImportPath(QStringLiteral("qrc:/qml/qml/"));
        mobile_ui.setSource(QUrl(QStringLiteral("qrc:/qml/qml/MobileUI.qml")));
        mobile_ui.setResizeMode(QQuickView::SizeRootObjectToView);
        mobile_ui.show();
    #endif

    return app.exec();
}
