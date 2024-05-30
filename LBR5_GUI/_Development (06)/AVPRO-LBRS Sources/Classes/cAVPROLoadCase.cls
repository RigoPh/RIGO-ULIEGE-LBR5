VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cAVPROLoadCase"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit

' **************************************
'   cAVPROLoadCase
'
'   en conformité avec
'   le schéma AVPROLBR5.xsd du 17/10/2003
'
'   NBD/PRINCIPIA MARINE-17/10/2003
' **************************************

Private m_Index         As Integer
Private m_Name          As String
Private m_AftBendingMoment As Double
Private m_ForeBendingMoment As Double


' *****************************
' ***** PUBLIC PROPERTIES *****
' *****************************

Public Property Let index(ByVal value As Integer)
    m_Index = value
End Property

Public Property Get index() As Integer
    index = m_Index
End Property

Public Property Get Name() As String
    Name = m_Name
End Property

Public Property Let Name(ByVal value As String)
    m_Name = value
End Property

Public Property Get AftBendingMoment() As Double
    AftBendingMoment = m_AftBendingMoment
End Property

Public Property Let AftBendingMoment(ByVal value As Double)
    m_AftBendingMoment = value
End Property

Public Property Get ForeBendingMoment() As Double
    ForeBendingMoment = m_ForeBendingMoment
End Property

Public Property Let ForeBendingMoment(ByVal value As Double)
    m_ForeBendingMoment = value
End Property

Public Sub writeToXML(file As Integer, Optional tabulation As Integer = 0)
    Dim i As Integer
    Dim spaces As Integer
    spaces = tabulation * 3

'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "<LOAD_CASE Index=" & Chr$(34) & m_Index & Chr$(34) & ">"
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "   <NAME>" & m_Name & "</NAME>"
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "   <AFT_BENDING_MOMENT>" & m_AftBendingMoment & "</AFT_BENDING_MOMENT>"
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "   <FORE_BENDING_MOMENT>" & m_ForeBendingMoment & "</FORE_BENDING_MOMENT>"
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "</LOAD_CASE>"
End Sub

Public Sub readXML(NoOfFile As Integer)
    Dim strLine                 As String
    Dim lineHeader              As String
    Dim attributeStartPosition  As Long
    Dim attributeEndPosition    As Long
    Dim elementName             As String
         
    Do While Not EOF(NoOfFile)
        ' LECTURE DE L'ELEMENT SUIVANT
        Line Input #NoOfFile, strLine
        strLine = cleanLine(strLine)
        lineHeader = readLineHeader(strLine)
        Select Case lineHeader
            Case "/NAME"
            Case "NAME"
                m_Name = readValue(strLine)
            Case "/AFT_BENDING_MOMENT"
            Case "AFT_BENDING_MOMENT"
                m_AftBendingMoment = Val_(readValue(strLine))
            Case "/FORE_BENDING_MOMENT"
            Case "FORE_BENDING_MOMENT"
                m_ForeBendingMoment = Val_(readValue(strLine))
            Case "NAME/", "AFT_BENDING_MOMENT/", "FORE_BENDING_MOMENT/"
                MsgBox "Error : cAVPROLoadCase.readXML : file bad format !", vbCritical + vbOKOnly
            Case "/LOAD_CASE"
                Exit Do
            Case "!--"
                ' ligne de commentaires
            Case Else
                MsgBox "Error : cAVPROLoadCase.readXML : file bad format !", vbCritical + vbOKOnly
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
    sLine = left(strLine, InStr(1, strLine, ">"))
    
    If InStr(1, sLine, Chr$(32)) <> 0 Then
'FIXIT: Replace 'Mid' function with 'Mid$' function                                        FixIT90210ae-R9757-R1B8ZE
'FIXIT: Replace 'Left' function with 'Left$' function                                      FixIT90210ae-R9757-R1B8ZE
        readLineHeader = Mid(left(sLine, InStr(1, sLine, Chr$(32)) - 1), 2)
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


