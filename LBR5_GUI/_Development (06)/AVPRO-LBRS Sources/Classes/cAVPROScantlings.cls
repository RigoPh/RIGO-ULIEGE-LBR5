VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cAVPROScantlings"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit

' **************************************
'   cAVPROScantlings
'
'   en conformité avec
'   le schéma AVPROLBR5.xsd du 17/10/2003
'
'   NBD/PRINCIPIA MARINE-17/10/2003
' **************************************

Private m_HWeb As Double
Private m_TWeb As Double
Private m_HFla As Double
Private m_TFla As Double


' *****************************
' ***** PUBLIC PROPERTIES *****
' *****************************

Public Property Let HWeb(ByVal value As Double)
    m_HWeb = value
End Property

Public Property Get HWeb() As Double
    HWeb = m_HWeb
End Property

Public Property Let TWeb(ByVal value As Double)
    m_TWeb = value
End Property

Public Property Get TWeb() As Double
    TWeb = m_TWeb
End Property

Public Property Let HFla(ByVal value As Double)
    m_HFla = value
End Property

Public Property Get HFla() As Double
    HFla = m_HFla
End Property

Public Property Let TFla(ByVal value As Double)
    m_TFla = value
End Property

Public Property Get TFla() As Double
    TFla = m_TFla
End Property


' *****************************
' ****** PUBLIC METHODS *******
' *****************************

Public Sub writeToXML(file As Integer, Optional tabulation As Integer = 0)
    Dim i As Integer
    Dim spaces As Integer
    spaces = tabulation * 3

'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "<WEB_HEIGHT>" & Format(m_HWeb, "0.0000") & "</WEB_HEIGHT>"
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "<WEB_THICKNESS>" & Format(m_TWeb, "0.0000") & "</WEB_THICKNESS>"
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "<FLANGE_HEIGHT>" & Format(m_HFla, "0.0000") & "</FLANGE_HEIGHT>"
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "<FLANGE_THICKNESS>" & Format(m_TFla, "0.0000") & "</FLANGE_THICKNESS>"
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
            Case "WEB_HEIGHT"
                m_HWeb = Val_(readValue(strLine))
            Case "WEB_THICKNESS"
                m_TWeb = Val_(readValue(strLine))
            Case "FLANGE_HEIGHT"
                m_HFla = Val_(readValue(strLine))
            Case "FLANGE_THICKNESS"
                m_TFla = Val_(readValue(strLine))
            Case "WEB_HEIGHT/", "WEB_THICKNESS/", "FLANGE_HEIGHT/", "FLANGE_THICKNESS/"
                MsgBox "Error : cAVPROScantlings.readXML : file bad format !", vbCritical + vbOKOnly
            Case "/SCANTLING"
                Exit Do
            Case "!--"
                ' ligne de commentaires
            Case Else
                MsgBox "Error : cAVPROScantlings.readXML : file bad format !", vbCritical + vbOKOnly
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



