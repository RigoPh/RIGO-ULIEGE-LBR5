Attribute VB_Name = "modComments"
' =========================================
'   THIS MODULE CONTAINS COMMENTS REGARDING
'                   THE CODE
' =========================================

' ======================================================================
'001.   THE LOAD CASES COLLECTIONS (colLoadCase)
'
'    The load cases collections are structured as follows:
'        -   one load case collection is created in the HEADER class;
'            this collection stores only the load cases names and
'            the bending moments values.
'        - one load case collection is created for each panel;
'            this collection stores data about one panel charges and
'            structural constraints, but doesn 't include load cases
'            names nor bending moments.
' ======================================================================
