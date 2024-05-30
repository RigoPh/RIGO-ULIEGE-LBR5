VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cAVPROProject"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit

' **************************************
'   cAVPROProject
'
'   en conformité avec
'   le schéma AVPROLBR5.xsd du 17/10/2003
'
'   NBD/PRINCIPIA MARINE-17/10/2003
' **************************************

Private m_Name As String
Private m_GeneralData As New cAVPROGeneralData
Public m_Panels As New Collection

Private Const PANELSTRINGKEY As String = "PANEL"

' *****************************
' ***** PUBLIC PROPERTIES *****
' *****************************

Public Property Get Name() As String
    Name = m_Name
End Property

Public Property Let Name(ByVal Value As String)
    m_Name = Value
End Property

Public Property Get GeneralData() As cAVPROGeneralData
    Set GeneralData = m_GeneralData
End Property

Public Property Let GeneralData(ByVal Value As cAVPROGeneralData)
    Set m_GeneralData = Value
End Property


' *****************************
' ****** PUBLIC METHODS *******
' *****************************

Public Function NoOfPanels() As Integer
    NoOfPanels = m_Panels.Count
End Function

Public Function AddPanel(oPanel As cAVPROPanel) As Boolean
On Error GoTo ErrHandler
    m_Panels.Add oPanel, PANELSTRINGKEY & oPanel.index
    AddPanel = True
    Exit Function
ErrHandler:
    AddPanel = False
End Function

Public Function RemovePanel(iPanelIndex As Integer) As Boolean
On Error GoTo ErrHandler
    Dim i As Integer
    For i = 1 To m_Panels.Count
        If m_Panels(i).index = iPanelIndex Then
            m_Panels.Remove i
            RemovePanel = True
            Exit For
        End If
    Next i
    Exit Function
ErrHandler:
    RemovePanel = False
End Function

Public Sub writeToXML(file As Integer, Optional tabulation As Integer = 0)
    Dim i As Integer
    Dim spaces As Integer
    spaces = tabulation * 3

'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "<?xml version=" & Chr$(34) & "1.0" & Chr$(34) & " encoding=" & Chr$(34) & "UTF-8" & Chr$(34) & "?>"
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "<PROJECT Name=" & Chr$(34) & m_Name & Chr$(34) & " xmlns:xsi=" & Chr$(34) & "http://www.w3.org/2001/XMLSchema-instance" & Chr$(34) & " xsi:noNamespaceSchemaLocation=" & Chr$(34) & App.Path & "\Schemas\AVPROLBR5.xsd" & Chr$(34) & ">"
    m_GeneralData.writeToXML file, tabulation + 1
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "   <PANELS NoOfPanels=" & Chr$(34) & m_Panels.Count & Chr$(34) & ">"
    For i = 1 To m_Panels.Count
        m_Panels.Item(i).writeToXML file, tabulation + 2
    Next i
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "   </PANELS>"
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "</PROJECT>"
End Sub

Public Sub readXML(NoOfFile As Integer)
    Dim strLine                 As String
    Dim lineHeader              As String
    Dim attributeStartPosition  As Long
    Dim attributeEndPosition    As Long
    Dim elementName             As String
    Dim oPanel                  As cAVPROPanel
    Dim sSurfaceName            As String
         
    'LECTURE DU HEADER
    Do While Not EOF(NoOfFile)
        Line Input #NoOfFile, strLine
        strLine = cleanLine(strLine)
        lineHeader = readLineHeader(strLine)
'FIXIT: Replace 'Mid' function with 'Mid$' function                                        FixIT90210ae-R9757-R1B8ZE
        Select Case Mid(lineHeader, 1, 1)
            Case "?"
            Case "!"
            Case Else
                If lineHeader = "PROJECT" Then
                    elementName = readAttribute(strLine, 1) ' Lecture du nom
                    m_Name = elementName
        
                        'LECTURE DU PROJET
                        Do While Not EOF(NoOfFile)
                            ' LECTURE DE L'ELEMENT SUIVANT
                            Line Input #NoOfFile, strLine
                            strLine = cleanLine(strLine)
                            lineHeader = readLineHeader(strLine)
                            Select Case lineHeader
                                Case "GENERAL_DATA"
                                    m_GeneralData.readXML NoOfFile
                                Case "PANELS"
                                    Do
                                        Line Input #NoOfFile, strLine
                                        strLine = cleanLine(strLine)
                                        lineHeader = readLineHeader(strLine)
                                        Select Case lineHeader
                                            Case "PANEL"
                                                Set oPanel = New cAVPROPanel
                                                elementName = readAttribute(strLine, 1) ' Lecture de l'index
                                                oPanel.index = elementName
                                                elementName = readAttribute(strLine, 2) ' Lecture de la surface
                                                sSurfaceName = elementName
                                                oPanel.Surface = oPanel.GetSurfaceIndex(sSurfaceName)
                                                oPanel.readXML NoOfFile
                                                AddPanel oPanel
                                                Set oPanel = Nothing
                                            Case "/PANELS"
                                                Exit Do
                                            Case Else
                                                MsgBox "XML file bad format !", vbCritical, "AVPRO"
                                                Exit Do
                                            End Select
                                    Loop

                                Case "PANELS/"
                                    MsgBox "Error : cAVPROProject.readXML : the file contains no panel !", vbCritical + vbOKOnly
                                Case "/PROJECT"
                                    'MsgBox "XML file processed with no error.", vbInformation, "AVPRO"
                                    Exit Do
                                Case "!--"
                                    ' ligne de commentaires
                                Case Else
                                    MsgBox "XML file bad format !", vbCritical, "AVPRO"
                                    Exit Do
                            End Select
                        Loop
                    
                    Exit Do
                Else
                    MsgBox "Error : cAVPROProject.readXML : file bad format !", vbCritical + vbOKOnly
                End If
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


