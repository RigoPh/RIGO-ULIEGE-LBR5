Attribute VB_Name = "modEnumSections"
Option Explicit

Private Const fntName = 0
Private Const fntSize = 1
Private Const fntBold = 2
Private Const fntItalic = 3
Private Const fntUnderline = 4

Function EnumSections(Filepath As String, _
                      sSection As String) As Collection
    
    If Len(Filepath) = 0 Or Dir(Filepath) = "" Then
        Exit Function
    End If
    
    If sSection = "" Then sSection = "PropertySheet"
    
On Error GoTo Err_EnumSections
    
    'Dim s As Collection
    Dim Col As Collection
    Dim hFile As Integer
    Dim line As String
    Dim Section As String
    Dim Attr As String
    Dim Value As String
    Dim Prop As Collection
    Dim i As Integer
    
    hFile = FreeFile
    Open Filepath For Input As #hFile
    
    Line Input #hFile, line
    line = Trim(line)
    Do While Not EOF(hFile)
        If IsSection(line) Then
            Section = StripBrackets(line)
            If Section = sSection Then
                Set Col = New Collection
                Do While Not EOF(hFile)
                    Line Input #hFile, line
                    line = Trim(line)
                    If IsSection(line) Then
                        Exit Do
                    ElseIf Len(line) > 0 And Not IsComment(line) Then
                        If IsAttribute(line) Then
                            GetAttribute line, Attr, Value
                            If Len(Value) > 0 And Len(Attr) > 0 Then
                                Col.Add Value, Attr
                            End If
'                        Else
'                            Set prop = GetProperty(line)
'                            col.Add prop
                        End If
                    End If
                Loop
                's.Add col, Section
                'Set col = Nothing
            End If
        Else
            Line Input #hFile, line
        End If
    Loop
    Set EnumSections = Col
Err_EnumSections:
    
    Close #hFile
    
End Function

Private Function IsSection(ByVal s As String) As Boolean
    s = Trim(s)
    IsSection = Left(s, 1) = "[" And Right(s, 1) = "]"
End Function

Private Function IsComment(ByVal s As String) As Boolean
    s = LTrim(s)
    IsComment = Left(s, 1) = ";"
End Function

Private Function IsAttribute(ByVal s As String) As Boolean
    IsAttribute = InStr(s, "=") > 0
End Function

Private Sub GetAttribute(ByVal Chunk As String, _
                         Attr As String, _
                         Value As String, _
                         Optional Delim As String = "=")
    Dim i As Integer
    i = InStr(Chunk, Delim)
    If i = 0 Then
        Attr = ""
        Value = " "
        Exit Sub
    End If
    Attr = Trim(Mid(Chunk, 1, i - 1))
    Value = Trim(Mid(Chunk, i + 1))
End Sub

'Private Function GetProperty(s As String) As Collection
'    Dim TempArr As Variant
'    Dim i As Integer
'    Dim Attr As String
'    Dim Value As String
'    Dim c As Collection
'
'    Set c = New Collection
'    TempArr = Split(s, ";")
'    For i = LBound(TempArr) To UBound(TempArr)
'        GetAttribute TempArr(i), Attr, Value, ":"
'        c.Add Value, Attr
'    Next
'    Set GetProperty = c
'End Function

Private Function StripBrackets(ByVal s As String)
    s = Trim(s)
    StripBrackets = Mid(s, 2, Len(s) - 2)
End Function

Public Function FontFromStr(ByVal FontName As String) As StdFont
    Dim arr As Variant
    Dim Font As New StdFont
    arr = Split(FontName, ";")
    On Error Resume Next
    With Font
        .Name = arr(fntName)
        .Size = Val(arr(fntSize))
        .Bold = CBool(arr(fntBold))
        .Italic = CBool(arr(fntItalic))
        .Underline = CBool(arr(fntUnderline))
    End With
    Set FontFromStr = Font
    Set Font = Nothing
End Function

Public Function StrFromFont(ByVal Font) As String
    Dim sTemp As String
    sTemp = ""
    With Font
        sTemp = sTemp & Font.Name & ";"
        sTemp = sTemp & Font.Size & ";"
        sTemp = sTemp & CInt(Font.Bold) & ";"
        sTemp = sTemp & CInt(Font.Italic) & ";"
        sTemp = sTemp & CInt(Font.Underline)
    End With
    StrFromFont = sTemp
End Function
