VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cAVPROGeneralData"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit

' **************************************
'   cAVPROGeneralData
'
'   en conformité avec
'   le schéma AVPROLBR5.xsd du 17/10/2003
'
'   NBD/PRINCIPIA MARINE-17/10/2003
' **************************************

Private m_Length As Double
Public m_LoadCases As New Collection

Private Const LOADCASESTRINGKEY As String = "LoadCase"


' *****************************
' ***** PUBLIC PROPERTIES *****
' *****************************

Public Property Get length() As Double
    length = m_Length
End Property

Public Property Let length(ByVal Value As Double)
    m_Length = Value
End Property




' *****************************
' ****** PUBLIC METHODS *******
' *****************************


'Public Property Get LoadCases() As Collection
'LoadCases = m_LoadCases
'End Property

Public Function NoOfLoadCases() As Integer
    NoOfLoadCases = m_LoadCases.Count
End Function

Public Function AddLoadCase(oLoadCase As cAVPROLoadCase) As Boolean
On Error GoTo ErrHandler
    m_LoadCases.Add oLoadCase, LOADCASESTRINGKEY & oLoadCase.index
    AddLoadCase = True
    Exit Function
ErrHandler:
    AddLoadCase = False
End Function

Public Function RemoveLoadCase(iLoadCaseIndex As Integer) As Boolean
On Error GoTo ErrHandler
    Dim i As Integer
    For i = 1 To m_LoadCases.Count
        If m_LoadCases(i).index = iLoadCaseIndex Then
            m_LoadCases.Remove i
            RemoveLoadCase = True
            Exit For
        End If
    Next i
    Exit Function
ErrHandler:
    RemoveLoadCase = False
End Function

Public Sub writeToXML(file As Integer, Optional tabulation As Integer = 0)
    Dim i As Integer
    Dim spaces As Integer
    spaces = tabulation * 3

'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "<GENERAL_DATA>"
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "   <LENGTH>" & m_Length & "</LENGTH>"
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "   <LOAD_CASES>"
    For i = 1 To m_LoadCases.Count
        m_LoadCases.Item(i).writeToXML file, tabulation + 2
    Next i
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "   </LOAD_CASES>"
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "</GENERAL_DATA>"
End Sub

Public Sub readXML(NoOfFile As Integer)
    Dim strLine                 As String
    Dim lineHeader              As String
    Dim attributeStartPosition  As Long
    Dim attributeEndPosition    As Long
    Dim elementName             As String
    Dim oLoadCase               As cAVPROLoadCase
         
    Do While Not EOF(NoOfFile)
        ' LECTURE DE L'ELEMENT SUIVANT
        Line Input #NoOfFile, strLine
        strLine = cleanLine(strLine)
        lineHeader = readLineHeader(strLine)
        Select Case lineHeader
            Case "LENGTH"
                m_Length = Val_(readValue(strLine))
            Case "LOAD_CASES"
                Do
                    Line Input #NoOfFile, strLine
                    strLine = cleanLine(strLine)
                    lineHeader = readLineHeader(strLine)
                    Select Case lineHeader
                        Case "LOAD_CASE"
                            Set oLoadCase = New cAVPROLoadCase
                            elementName = readAttribute(strLine, 1) ' Lecture de l'index
                            oLoadCase.index = elementName
                            oLoadCase.readXML NoOfFile
                            AddLoadCase oLoadCase
                            Set oLoadCase = Nothing
                        Case "/LOAD_CASES"
                            Exit Do
                        Case Else
                            MsgBox "Error : cAVPROGeneralData.readXML : file bad format !", vbCritical + vbOKOnly
                            Exit Do
                    End Select
                Loop
                If m_LoadCases.Count = 0 Then MsgBox "Error : cAVPROGeneralData.readXML : the file contains no load case !", vbCritical + vbOKOnly
                
            Case "LENGTH/"
                MsgBox "Error : cAVPROGeneralData.readXML : the file contains no length !", vbCritical + vbOKOnly
            Case "/GENERAL_DATA"
                'MsgBox "XML file processed with no error.", vbInformation
                Exit Do
            Case "!--"
                ' ligne de commentaires
            Case Else
                MsgBox "Error : cAVPROGeneralData.readXML : file bad format !", vbCritical + vbOKOnly
                Exit Do
        End Select
    Loop
        
End Sub


' ***************************************
' *********** PRIVATE METHODS ***********
' ***************************************

Private Function readAttribute(strLine As String, noOfAttribute As Integer) As String
    ' RENVOIE L ATTRIBUT NUMERO noOfAttribute
    Dim i As Integer
    Dim strAttribute As String
    Dim attributeStartPosition As Long
    Dim attributeEndPosition As Long

    attributeEndPosition = 0
    For i = 1 To noOfAttribute
        attributeStartPosition = InStr(attributeEndPosition + 1, strLine, Chr$(34)) + 1
        If attributeStartPosition < attributeEndPosition Then
            readAttribute = ""
            Exit Function
        End If
        attributeEndPosition = InStr(attributeStartPosition, strLine, Chr$(34))
    Next i
    If attributeEndPosition <> 0 And attributeStartPosition > 1 Then
'FIXIT: Replace 'Mid' function with 'Mid$' function                                        FixIT90210ae-R9757-R1B8ZE
        readAttribute = Mid(strLine, attributeStartPosition, attributeEndPosition - attributeStartPosition)
    Else
        readAttribute = ""
    End If

End Function

Private Function readLineHeader(ByVal strLine As String) As String
    Dim sLine As String
    
'FIXIT: Replace 'Left' function with 'Left$' function                                      FixIT90210ae-R9757-R1B8ZE
    sLine = Left(strLine, InStr(1, strLine, ">"))
    
    If InStr(1, sLine, Chr$(32)) <> 0 Then
'FIXIT: Replace 'Mid' function with 'Mid$' function                                        FixIT90210ae-R9757-R1B8ZE
'FIXIT: Replace 'Left' function with 'Left$' function                                      FixIT90210ae-R9757-R1B8ZE
        readLineHeader = Mid(Left(sLine, InStr(1, sLine, Chr$(32)) - 1), 2)
    Else
'FIXIT: Replace 'Mid' function with 'Mid$' function                                        FixIT90210ae-R9757-R1B8ZE
        readLineHeader = Mid(sLine, 2, Len(sLine) - 2)
    End If
End Function

Private Function readValue(strLine As String) As String
    Dim startPosition As Long
    Dim endPosition As Long

    startPosition = InStr(1, strLine, ">") + 1
    endPosition = InStr(startPosition, strLine, "<")
    
    If endPosition <> 0 Then
'FIXIT: Replace 'Mid' function with 'Mid$' function                                        FixIT90210ae-R9757-R1B8ZE
        readValue = Mid(strLine, startPosition, endPosition - startPosition)
    Else
        readValue = ""
    End If
    
End Function

Private Function cleanLine(strLine As String) As String
    Dim char As String
    Dim i As Integer
    
    For i = 1 To Len(strLine)
'FIXIT: Replace 'Mid' function with 'Mid$' function                                        FixIT90210ae-R9757-R1B8ZE
        char = Mid(strLine, i, 1)
        If Asc(char) = 9 Then char = "  "
        cleanLine = cleanLine & char
    Next i
'FIXIT: Replace 'Trim' function with 'Trim$' function                                      FixIT90210ae-R9757-R1B8ZE
    cleanLine = Trim(cleanLine)
End Function



