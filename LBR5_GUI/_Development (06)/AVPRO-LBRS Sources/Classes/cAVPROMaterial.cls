VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cAVPROMaterial"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit

' **************************************
'   cAVPROMaterial
'
'   en conformité avec
'   le schéma AVPROLBR5.xsd du 17/10/2003
'
'   NBD/PRINCIPIA MARINE-17/10/2003
' **************************************

Private m_Name As String
Private m_E    As Double
Private m_ETA  As Double
Private m_SIGY As Double
Private m_SIGM As Double
Private m_SPEC As Double


' *****************************
' ***** PUBLIC PROPERTIES *****
' *****************************

Public Property Get Name() As String
    Name = m_Name
End Property

Public Property Let Name(ByVal value As String)
    m_Name = value
End Property

Public Property Let YoungModulus(ByVal value As Double)
    m_E = value
End Property

Public Property Get YoungModulus() As Double
    YoungModulus = m_E
End Property

Public Property Let Poisson(ByVal value As Double)
    m_ETA = value
End Property

Public Property Get Poisson() As Double
    Poisson = m_ETA
End Property

Public Property Let SigAdm(ByVal value As Double)
    m_SIGY = value
End Property

Public Property Get SigAdm() As Double
    SigAdm = m_SIGY
End Property

Public Property Let SigMax(ByVal value As Double)
    m_SIGM = value
End Property

Public Property Get SigMax() As Double
    SigMax = m_SIGM
End Property

Public Property Let Density(ByVal value As Double)
    m_SPEC = value
End Property

Public Property Get Density() As Double
    Density = m_SPEC
End Property


' *****************************
' ****** PUBLIC METHODS *******
' *****************************

Public Sub writeToXML(file As Integer, Optional tabulation As Integer = 0)
    Dim i As Integer
    Dim spaces As Integer
    spaces = tabulation * 3

'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "<MATERIAL Name=" & Chr$(34) & m_Name & Chr$(34) & ">"
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "   <E>" & m_E & "</E>"
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "   <ETA>" & m_ETA & "</ETA>"
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "   <SIGY>" & m_SIGY & "</SIGY>"
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "   <SIGM>" & m_SIGM & "</SIGM>"
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "   <SPEC>" & m_SPEC & "</SPEC>"
'FIXIT: Print method has no Visual Basic .NET equivalent and will not be upgraded.         FixIT90210ae-R7593-R67265
    Print #file, Spc(spaces); "</MATERIAL>"
End Sub

Public Sub readXML(NoOfFile As Integer)
    Dim strLine                 As String
    Dim lineHeader              As String
    Dim attributeStartPosition  As Long
    Dim attributeEndPosition    As Long
         
    Do While Not EOF(NoOfFile)
        ' LECTURE DE L'ELEMENT SUIVANT
        Line Input #NoOfFile, strLine
        strLine = cleanLine(strLine)
        lineHeader = readLineHeader(strLine)
        Select Case lineHeader
            Case "E"
                m_E = Val_(readValue(strLine))
            Case "ETA"
                m_ETA = Val_(readValue(strLine))
            Case "SIGY"
                m_SIGY = Val_(readValue(strLine))
            Case "SIGM"
                m_SIGM = Val_(readValue(strLine))
            Case "SPEC"
                m_SPEC = Val_(readValue(strLine))
            Case "E/", "ETA/", "SIGY/", "SIGM/", "SPEC/"
                MsgBox "Error : cAVPROMaterial.readXML : file bad format !", vbCritical + vbOKOnly
            Case "/MATERIAL"
                Exit Do
            Case "!--"
                ' ligne de commentaires
            Case Else
                MsgBox "Error : cAVPROMaterial.readXML : file bad format !", vbCritical + vbOKOnly
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


