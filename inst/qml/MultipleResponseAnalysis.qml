// Copyright (C) 2013-2026 University of Amsterdam
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
// # Maintainer: Shun Wang <shuonwang@gmail.com>

import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls

Form
{
    id: form

    info: qsTr("Multiple Response Analysis analyses survey questions where respondents may " +
               "select more than one answer (\"check all that apply\"). Variables are first " +
               "collected into the 'Multiple response variables' box, then distributed over one " +
               "or more named Multiple Response Sets. Each set can be coded either as " +
               "**Dichotomous** (one binary variable per answer option, e.g. 0 = not selected, " +
               "1 = selected) or as **Category** (one variable per response slot holding the " +
               "chosen code). Outputs include frequency tables, crosstabulations, and charts.")

    columns: 1

    // Hidden master list: holds every column of the dataset so that the visible
    // available-list can filter it down to nominal/ordinal columns only.
    AvailableVariablesList { name: "allVariablesList"; visible: false }

    VariablesForm
    {
        preferredHeight: jaspTheme.smallDefaultVariablesFormHeight

        AvailableVariablesList
        {
            name:   "selectVariablesList"
            title:  qsTr("Available variables")
            source: [{ name: "allVariablesList", allowedColumns: ["nominal", "ordinal"] }]
        }

        AssignedVariablesList
        {
            name:           "multipleResponseVariables"
            title:          qsTr("Multiple response variables")
            allowedColumns: ["nominal", "ordinal"]
            info:           qsTr("All variables that take part in any multiple response set. " +
                                 "Assign each of them to a set in the 'Variables in multiple responses' box below.")
        }

        AssignedVariablesList
        {
            name:           "crosstabGroupVar"
            title:          qsTr("Crosstabs grouping variable")
            singleVariable: true
            allowedColumns: ["nominal", "ordinal"]
            info:           qsTr("Optional single categorical variable used as the column variable " +
                                 "of the crosstabulation when 'Grouping variable' is selected.")
        }
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Step 2 — coding of the response variables
    // ─────────────────────────────────────────────────────────────────────────
    RadioButtonGroup
    {
        name:    "setCoding"
        title:   qsTr("Variable coding")
        columns: 2
        info:    qsTr("How the selected response was recorded in the data.")

        RadioButton
        {
            value:   "dichotomous"
            label:   qsTr("Dichotomous")
            checked: true
            info:    qsTr("Each variable represents one answer option and is binary. " +
                          "Specify below which value (or value label) means 'selected'.")
        }

        RadioButton
        {
            value: "category"
            label: qsTr("Category")
            info:  qsTr("Each variable holds the code of one chosen answer. All unique non-missing " +
                        "values found across the variables of a set are treated as valid response categories.")
        }
    }

    TextField
    {
        name:            "responseValue"
        label:           qsTr("All response values are coded as:")
        value:           "1"
        fieldWidth:      60
        info:            qsTr("The value that indicates a selected response, used for Dichotomous coding. " +
                              "Matching is tolerant: the entered text is compared against both the underlying " +
                              "value and the value label of the column, ignoring surrounding whitespace. " +
                              "So for a variable with value labels you may type either '1' or 'Yes'.")
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Step 3 — define an arbitrary number of named response sets
    // ─────────────────────────────────────────────────────────────────────────
    InputListView
    {
        id:               nameResponseGroups
        name:             "nameResponseGroups"
        title:            qsTr("Multiple response set naming")
        optionKey:        "name"
        defaultValues:    [qsTr("Set 1"), qsTr("Set 2")]
        placeHolder:      qsTr("New set")
        minRows:          1
        preferredWidth:   (2 * form.width) / 5
        preferredHeight:  jaspTheme.smallDefaultVariablesFormHeight
        rowComponentTitle: qsTr("Label")
        info:             qsTr("Create one row per multiple response set. The text in the row is the set " +
                               "name (used as a key); the optional field on the right is a descriptive " +
                               "label shown in tables and charts. Rows can be added and removed freely, " +
                               "so any number of sets can be defined.")

        rowComponent: TextField
        {
            name:       "label"
            value:      ""
            fieldWidth: 110
        }
    }

    AssignedVariablesList
    {
        Layout.fillWidth:                true
        Layout.leftMargin:               40
        preferredHeight:                 jaspTheme.smallDefaultVariablesFormHeight
        title:                           qsTr("Variables in multiple responses")
        name:                            "assignGroupVariables"
        source:                          ["multipleResponseVariables"]
        addAvailableVariablesToAssigned: true
        draggable:                       false
        rowComponentTitle:               qsTr("Set")
        info:                            qsTr("Every variable placed in 'Multiple response variables' appears here. " +
                                              "Use the drop-down to say which multiple response set it belongs to. " +
                                              "Variables left on a set that no longer exists are ignored.")

        rowComponent: DropDown
        {
            name:   "group"
            source: ["nameResponseGroups"]
        }
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Missing values
    // ─────────────────────────────────────────────────────────────────────────
    RadioButtonGroup
    {
        name:  "missingValues"
        title: qsTr("Missing Values")
        info:  qsTr("Controls how missing data are handled when counting responses.")

        RadioButton
        {
            value:   "excludeCasewise"
            label:   qsTr("Exclude cases within each set (pairwise)")
            checked: true
            info:    qsTr("A case is dropped from a set only when every variable of that set is missing.")
        }

        RadioButton
        {
            value: "excludeListwise"
            label: qsTr("Exclude cases listwise across all variables")
            info:  qsTr("A case is dropped from all output as soon as any analysed variable is missing.")
        }
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Frequencies
    // ─────────────────────────────────────────────────────────────────────────
    Section
    {
        title:    qsTr("Frequencies")
        expanded: true
        info:     qsTr("Frequency table per multiple response set.")

        CheckBox
        {
            name:    "frequencies"
            label:   qsTr("Frequency tables")
            checked: true
            columns: 2
            info:    qsTr("Show a frequency table for every defined set. Counts (N) are always displayed.")

            CheckBox
            {
                name:    "freqResponsePct"
                label:   qsTr("% of responses")
                checked: true
                info:    qsTr("Share of each category in the total number of responses.")
            }

            CheckBox
            {
                name:    "freqCasePct"
                label:   qsTr("% of cases")
                checked: true
                info:    qsTr("Share of respondents (cases) that selected the category. " +
                              "These percentages can add up to more than 100%.")
            }

            CheckBox
            {
                name:    "freqTotal"
                label:   qsTr("Total")
                checked: true
                info:    qsTr("Append a Total row summarising responses and cases. " +
                              "Uncheck to omit all row/column totals from the frequency and crosstabulation tables.")
            }
        }
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Crosstabs
    // ─────────────────────────────────────────────────────────────────────────
    Section
    {
        title: qsTr("Crosstabs")
        info:  qsTr("Cross-tabulate the responses of one set against another set or against a categorical variable.")

        CheckBox
        {
            name:    "crosstabs"
            id:      crosstabs
            label:   qsTr("Crosstabulation")
            columns: 1
            info:    qsTr("Build a crosstabulation. Choose the set that forms the rows and what forms the columns.")

            DropDown
            {
                name:   "crosstabRowSet"
                label:  qsTr("Row set")
                source: ["nameResponseGroups"]
                info:   qsTr("The multiple response set whose categories form the rows.")
            }

            RadioButtonGroup
            {
                name:  "crosstabColumnType"
                title: qsTr("Column variable")
                info:  qsTr("What forms the columns of the table.")

                RadioButton
                {
                    value:   "set"
                    label:   qsTr("Another response set")
                    checked: true

                    DropDown
                    {
                        name:   "crosstabColSet"
                        label:  qsTr("Column set")
                        source: ["nameResponseGroups"]
                        info:   qsTr("The multiple response set whose categories form the columns.")
                    }
                }

                RadioButton
                {
                    value: "variable"
                    label: qsTr("Grouping variable")
                    info:  qsTr("Use the levels of the 'Crosstabs grouping variable' assigned at the top of the form.")
                }
            }

            Group
            {
                title:   qsTr("Table Statistics")
                columns: 2

                CheckBox
                {
                    name:    "crosstabRowPct"
                    label:   qsTr("Row percentages")
                    checked: true
                    info:    qsTr("Cell count as a percentage of its row total.")
                }

                CheckBox
                {
                    name:  "crosstabColPct"
                    label: qsTr("Column percentages")
                    info:  qsTr("Cell count as a percentage of its column total.")
                }

                CheckBox
                {
                    name:  "crosstabTotalPct"
                    label: qsTr("Total percentages")
                    info:  qsTr("Cell count as a percentage of the grand total of responses.")
                }

                CheckBox
                {
                    name:  "chiSquare"
                    label: qsTr("Chi-square test")
                    info:  qsTr("Pearson chi-square test on the cell counts. Multiple response data may " +
                                "violate the independence assumption, so interpret with caution.")
                }
            }
        }
    }

    Section
    {
        title: qsTr("Charts")
        info:  qsTr("Charts of the response distribution of every defined set, drawn with the JASP theme and palette.")

        Group
        {
            columns: 2
            info:    qsTr("Pick the chart types to draw. Nothing is drawn until variables have been assigned to a set.")

            CheckBox { name: "barChart";        label: qsTr("Bar chart");        info: qsTr("Vertical bars of the selected metric per response category.") }
            CheckBox { name: "horizontalBarChart"; label: qsTr("Horizontal bar chart"); info: qsTr("Same as the bar chart but with horizontal bars, useful for long category labels.") }
            CheckBox { name: "stackedBarChart"; label: qsTr("Stacked bar chart"); info: qsTr("Stacked bars of the crosstabulation of this set against the crosstab column variable.") }
            CheckBox { name: "pieChart";        label: qsTr("Pie chart");        info: qsTr("Pie chart of response frequencies.") }
            CheckBox { name: "donutChart";      label: qsTr("Donut chart");      info: qsTr("Ring chart of response frequencies.") }
            CheckBox { name: "lineChart";       label: qsTr("Line chart");       info: qsTr("Line connecting the category values, suitable for ordered options.") }
            CheckBox { name: "paretoChart"; id: paretoChart; label: qsTr("Pareto chart"); info: qsTr("Bars sorted from most to least frequent with a cumulative percentage line on a secondary axis.") }
        }

        Group
        {
            title:   qsTr("Pareto Options")
            visible: paretoChart.checked
            columns: 1

            CheckBox
            {
                name:    "paretoCumulativeLine"
                label:   qsTr("Show cumulative line")
                checked: true
                info:    qsTr("Draw the cumulative percentage line and its secondary axis.")
            }

            CheckBox
            {
                name:  "paretoReferenceLine"
                label: qsTr("Show 80% reference line")
                info:  qsTr("Add a horizontal dashed line at 80% cumulative percentage.")
            }
        }

        ColorPalette { info: qsTr("Colour palette used for all charts.") }

        RadioButtonGroup
        {
            name:  "chartMetric"
            title: qsTr("Chart values")
            info:  qsTr("Metric shown on the value axis, or as slice size for pie and donut charts.")

            RadioButton { value: "frequency";   label: qsTr("Frequencies (counts)"); checked: true; info: qsTr("Raw response counts.") }
            RadioButton { value: "responsePct"; label: qsTr("% of responses");                       info: qsTr("Counts divided by the total number of responses.") }
            RadioButton { value: "casePct";     label: qsTr("% of cases");                           info: qsTr("Counts divided by the number of respondents.") }
        }
    }
}
