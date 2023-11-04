//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls

// All Analysis forms must be built with the From QML item
Form
{
    AvailableVariablesList { name: "allVariablesList" ; visible:false}

    VariablesForm
    {
        preferredHeight: jaspTheme.smallDefaultVariablesFormHeight

        AvailableVariablesList { name: "selectVariablesList" ; title: "Available variables";
        source: 		[{ name: "allVariablesList", use: "type=ordinal|nominal|nominalText"}]
        }

        AssignedVariablesList { name: "multipleResponseVariables"; title: qsTr("Multiple response variables"); width:form.width/2.1; suggestedColumns: ["nominal","nominalText"]
        }
    }

    TextField { name: "responseValue"; label: qsTr("All response values are code as: "); value: "1" ;  fieldWidth:50; Layout.columnSpan: 2 }

    InputListView
    {
        id                  : nameResponseGroups
        name				: "nameResponseGroups"
        title				: qsTr("Group naming")
        optionKey			: "name"
        defaultValues		: [qsTr("Group 1"), qsTr("Group 2")]
        placeHolder			: qsTr("New Group")
        minRows				: 2
        preferredWidth		: (2 * form.width) / 5
        preferredHeight: jaspTheme.smallDefaultVariablesFormHeight

    }

    AssignedVariablesList
    {
        Layout.fillWidth				: true
        Layout.leftMargin				: 40
        preferredHeight                 : jaspTheme.smallDefaultVariablesFormHeight
        title							: qsTr("Variables in multiple responses")
        name							: "assignGroupVariables"
        source							: ["multipleResponseVariables"]
        addAvailableVariablesToAssigned	: true
        draggable						: false
        rowComponentTitle				: qsTr("Group")
        rowComponent: DropDown
        {
            name: "group"
            source: ["nameResponseGroups"]
        }
    }

    Group
    {
        title: qsTr("Multiple response tables")
        columns:			2
        Layout.columnSpan:	2

        CheckBox { name: "validOfResponse";			label: qsTr("Valid");	             checked: true	}
        CheckBox { name: "missingOfResponse";		label: qsTr("Missing");	             checked: true	}
        CheckBox { name: "percentOfResponses" ;		label: qsTr("Percent of responses"); checked: true	}
        CheckBox { name: "percentOfCases";			label: qsTr("Percent of cases");	 checked: false	}
        CheckBox { name: "total";			        label: qsTr("Total");	             checked: true	}
    }


    // TODO: Cross tables
    // assigned: Shun

}
