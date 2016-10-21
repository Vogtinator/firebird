import QtQuick 2.0
import QtQuick.Window 2.2
import QtQuick.Dialogs 1.2
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.0
import Firebird.UIComponents 1.0

Window {
    id: window
    minimumHeight: 350
    minimumWidth: 540

    color: fakeDialog.contentItem.color

    Dialog {
        id: fakeDialog
        visible: false
    }

    ConfigPages {
        anchors {
            bottom: actionRow.top
            right: parent.right
            left: parent.left
            top: parent.top
            margins: 5
        }
        model: ConfigPagesModel {
        }
    }

    RowLayout {
        id: actionRow

        anchors {
            left: parent.left
            bottom: parent.bottom
            right: parent.right
            margins: 5
            topMargin: 0
        }

        Label {
            Layout.fillWidth: true
            text: qsTr("Changes are saved automatically")
            font.italic: true
            color: "grey"
        }

        Button {
            text: qsTr("Ok")
            onClicked: window.visible = false
        }
    }
}
