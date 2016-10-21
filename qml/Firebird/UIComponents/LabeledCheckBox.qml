import QtQuick 2.0
import QtQuick.Controls 1.0
import QtQuick.Layouts 1.0

RowLayout {
    property alias checked: checkBox.checked
    property alias text: label.text
    spacing: 0

    CheckBox {
        id: checkBox
    }

    MouseArea {
        anchors.fill: parent
        onClicked: checkBox.checked = !checkBox.checked
    }

    Label {
        id: label
        anchors {
            left: checkBox.right
            leftMargin: -5
        }

        //Disabled for now, might be mistaken for a disabled control
        //color: checkBox.checked ? "" : "grey"
        text: ""
    }
}