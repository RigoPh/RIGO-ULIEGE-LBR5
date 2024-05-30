Attribute VB_Name = "modStrings"
Option Explicit
Public ArrayCopy() As Variant
Public ArrayPaste() As Variant
'Public Clipboard As String

' ************************************
' Data file string manipulation module
' ************************************

Public Function GetCommonAppPath() As String
    Dim buff As String * 100
    SHGetFolderPath 0, CSIDL_VALUES.CSIDL_COMMON_APPDATA, -1, &H0, buff
    GetCommonAppPath = TrimNull(buff)
End Function

Public Function TrimNull(startstr As String) As String

   TrimNull = Left$(startstr, lstrlenW(StrPtr(startstr)))
   
End Function
' Cleans blanks, tabs and "-"
Public Function CleanLines(ByVal s As String) As String
    On Error GoTo CleanLinesErr
    Dim i As Integer
    For i = 1 To Len(s)
        'If left(s, 1) = " " Or left(s, 1) = Chr$(9) Or left(s, 1) = "-" Then
        If Left(s, 1) = " " Or Left(s, 1) = Chr$(9) Then
            s = Mid(s, 2, Len(s) - 1)
        Else
            CleanLines = s
            Exit Function
        End If
    Next i
    CleanLines = s
    Exit Function
CleanLinesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modStrings: Sub CleanLines")
End Function

' reads only tab separated values
Public Function GetValuesTAB(index As Integer, stringinput As String, v As Variant)
    On Error GoTo GetValuesTABErr
    Dim i As Integer
    Dim FirstOcc As Integer
    Dim FirstOccTab As Integer
    ReDim v(1 To index)
    Dim s As String
    For i = 1 To index
        stringinput = LTrim(stringinput)
        FirstOccTab = InStr(1, stringinput, Chr$(9)) - 1
        If FirstOccTab = -1 Then FirstOccTab = Len(stringinput)
        v(i) = Left(stringinput, FirstOccTab)
        stringinput = right(stringinput, Len(stringinput) - FirstOccTab)
        s = v(i)
        RTrim (s)
        v(i) = s
    Next i
    Exit Function
GetValuesTABErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modStrings: Function GetValuesTAB")
End Function

' Cleans blanks at right
'Public Function CleanBlanksRight(ByRef s As String)
'    On Error GoTo  CleanBlanksRightErr
'    Dim i As Integer
'    For i = Len(s) To 1 Step -1
'        If right(s, 1) = " " Or right(s, 1) = Chr$(9) Then
'            s = left(s, Len(s) - 1)
'        Else
'            Exit Function
'        End If
'    Next i
'    Exit Function
'CleanBlanksRightErr:
'    Call RaiseError(MyUnhandledError,Err.Description & "." & vbCrLf &  "modStrings: Function CleanBlanksRight")
'End Function

' Cleans blanks and tabs at left
'Public Function CleanBlanks(stringinput As String)
'    On Error GoTo  CleanBlanksErr
'    Dim i As Integer
'    For i = 1 To Len(stringinput)
'        If left(stringinput, 1) = " " Or left(stringinput, 1) = Chr$(9) Then
'            stringinput = Mid(stringinput, 2, Len(stringinput) - 1)
'        Else
'            Exit Function
'        End If
'    Next i
'    Exit Function
'CleanBlanksErr:
'    Call RaiseError(MyUnhandledError,Err.Description & "." & vbCrLf &  "modStrings: Sub CleanBLanks")
'End Function

Public Function GetValues(ByVal index As Integer, ByRef s As String, v As Variant)
    On Error GoTo GetValuesErr
    Dim i As Integer
    Dim occ As Integer
    If index = 0 Then Exit Function
    ReDim v(1 To index)
    s = CleanString(s)
    For i = 1 To index
    s = LTrim(s)
    If Left(s, 1) = "/" Then 'Label
        occ = InStr(2, s, "/") - 2
        v(i) = Mid(s, 2, occ)
        s = Mid(s, occ + 3, Len(s) - occ)
        GoTo NextI
    End If
    occ = InStr(1, s, " ") - 1
    If occ = -1 Then occ = Len(s)
    v(i) = Left(s, occ)
    s = Mid(s, occ + 1, Len(s) - occ)
NextI:
    Next i
    Exit Function
GetValuesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modStrings: Function GetValues.")  '& vbCrLf & "Last String: " & s)
End Function

Public Function ReadCorrosionThickness() 'temp
    On Error GoTo ReadCorrosionThicknessErr
    Dim fso As New FileSystemObject, fil As file, ts As TextStream
    Dim sFile As String, sLine As String, ParentFolderName As String
    Dim i As Integer, v() As Variant
    sFile = Project.Item(ActiveProject).sFileName
    
    ParentFolderName = fso.GetParentFolderName(sFile)
    If fso.FileExists(ParentFolderName & "\Atableur.txt") Then
        Set fil = fso.GetFile(ParentFolderName & "\Atableur.txt")
        Set ts = fil.OpenAsTextStream(ForReading)
    Else
        MsgBox "Corrosion thickness data file not found.", vbCritical + vbOKOnly
        Exit Function
    End If
    For i = 1 To 5
        ts.SkipLine
    Next i
    For i = 1 To Project.Item(ActiveProject).colPanel.Count
        sLine = ReadLn(ts)
        GetValues 4, sLine, v
        Project.Item(ActiveProject).colPanel.Item(i).cScantlings.CorrosionThickness = Val_(v(2))
        Project.Item(ActiveProject).colPanel.Item(i).cScantlings.cPrimaryFrames.CorrosionThickness = Val_(v(3))
        Project.Item(ActiveProject).colPanel.Item(i).cScantlings.cPrimaryStiffeners.CorrosionThickness = Val_(v(4))
    Next i
    MsgBox "Corrosion thickness data file succesfully read.", vbInformation + vbOKOnly
    Exit Function
ReadCorrosionThicknessErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modStrings: Function ReadCorrosionThickness")
End Function

' Reads values separates by blanks and tabs
'Public Function getValues(Index As Integer, stringinput As String, v As Variant)
'    On Error GoTo  GetValuesErr
'    Dim i As Integer
'    Dim FirstOcc As Integer
'    Dim FirstOccTab As Integer
'    ReDim v(1 To Index)
'    For i = 1 To Index
'1:
'        stringinput = CleanString(stringinput)
'        If left(stringinput, 1) = vbTab Then
'            stringinput = right(stringinput, Len(stringinput) - 1)
'            GoTo 1
'        End If
'
'        If left(stringinput, 1) = "/" Then 'Label (only one label per row)
'            stringinput = Mid(stringinput, 2, Len(stringinput) - 1)
'            FirstOcc = InStr(1, stringinput, "/") - 1
'            stringinput = Replace(stringinput, "/", "")
'            GoTo LabelMode
'        End If
'
'        FirstOcc = InStr(1, stringinput, " ") - 1
'        FirstOccTab = InStr(1, stringinput, vbTab) - 1
'        If FirstOcc = -1 Then
'            If FirstOccTab = -1 Then
'                FirstOcc = Len(stringinput)
'            Else
'                FirstOcc = FirstOccTab
'            End If
'        Else
'            If FirstOcc > FirstOccTab Then
'                If FirstOccTab > -1 Then
'                    FirstOcc = FirstOccTab
'                End If
'            End If
'        End If
'LabelMode:
'        v(i) = left(stringinput, FirstOcc)
'        stringinput = right(stringinput, Len(stringinput) - FirstOcc)
'    Next i
'    Exit Function
'GetValuesErr:
'    Call RaiseError(MyUnhandledError,Err.Description & "." & vbCrLf &  "modStrings: Function GetValues")
'End Function

' Reads values separated by blanks and tabs, when the line begins with a comment)
Public Function GetValues1(index As Integer, stringinput As String, v As Variant)
    On Error GoTo GetValues1Err
    Dim i As Integer
    Dim FirstOcc As Integer
    Dim FirstOccTab As Integer
    ReDim v(0 To index)
    For i = 0 To index
        stringinput = LTrim(stringinput)
        FirstOcc = InStr(1, stringinput, " ") - 1
        FirstOccTab = InStr(1, stringinput, Chr$(9)) - 1
        If FirstOcc = -1 Then
            If FirstOccTab = -1 Then
                FirstOcc = Len(stringinput)
            Else
                FirstOcc = FirstOccTab
            End If
        Else
            If FirstOcc > FirstOccTab Then
                If FirstOccTab > -1 Then
                    FirstOcc = FirstOccTab
                End If
            End If
        End If
        v(i) = Left(stringinput, FirstOcc)
        stringinput = right(stringinput, Len(stringinput) - FirstOcc)
    Next i
    For i = 1 To index
        v(i - 1) = v(i)
    Next i
    ReDim Preserve v(1 To index)
    Exit Function
GetValues1Err:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modStrings: Function GetValues1")
End Function

Public Function GetSpaceFormat(ByVal s As String) As String
    Dim i As Integer
    Dim s1() As String
    i = 0
    Dim sRest As String
    Dim bMinus As Boolean
    If Left(s, 1) = "-" Then
        bMinus = True
        s = right(s, Len(s) - 1)
    End If
    
    sRest = ""
    
    For i = 1 To Len(s)
        If Mid(s, i, 1) = "." Then
            
            sRest = right(s, Len(s) - i)
            s = Left(s, i - 1)
            Exit For
        End If
    Next i
    i = 0
1:
    If Len(s) > 3 Then
        i = i + 1
        ReDim Preserve s1(i)
        s1(i) = right(s, 3)
        s = Left(s, Len(s) - 3)
        GoTo 1
    ElseIf Len(s) <= 3 And Len(s) > 0 Then
        i = i + 1
        ReDim Preserve s1(i)
        s1(i) = s
    End If
    
    GetSpaceFormat = ""
    For i = UBound(s1) To 1 Step -1
        GetSpaceFormat = GetSpaceFormat & " " & s1(i)
    Next i
    GetSpaceFormat = LTrim(GetSpaceFormat)
    If Len(sRest) > 0 Then GetSpaceFormat = GetSpaceFormat + "." + sRest
    If bMinus = True Then GetSpaceFormat = "-" & GetSpaceFormat
End Function

' Gets value from string (forces point separator)
Public Function Val_(ByVal s As Variant)
    On Error GoTo Val_Err
    
    Dim i As Integer
    For i = 1 To Len(s)
        Select Case Mid(s, i, 1)
            Case "."
                Val_ = Val(s)
                Exit Function
            Case ","
                'Val_ = Round(CDbl(s), Len(s) - i)
                Dim a As String
                Dim b As String
                Dim c As String
                a = Left(s, i - 1)
                b = right(s, Len(s) - i)
                c = a + "." + b
                Val_ = Val(c)
                Exit Function
            Case Else
        End Select
    Next i
    Val_ = Val(s)
    Exit Function
Val_Err:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modStrings: Function Val_")
End Function

Public Function PVal(ByVal dVal As Double, sFormat As String) As String
    On Error GoTo PValErr
    PVal = Format(CStr(dVal), sFormat)
    PVal = Replace(PVal, ",", ".")
    Exit Function
PValErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modString: Function PVal")
End Function

Public Function ReadLn(ts As TextStream) As String
    'On Error GoTo  ReadLnErr
    Dim sReplace As String
1:
    ReadLn = ts.ReadLine
    sReplace = Replace(ReadLn, vbTab, " ")
    sReplace = LTrim(sReplace)
    If sReplace = "" And ts.AtEndOfStream = False Then
        GoTo 1
    End If
    ReadLn = CleanString(ReadLn)
    Exit Function
ReadLnErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modStrings: Function ReadLn")
End Function

Public Function CleanString(s As String) As String
    CleanString = Replace(s, vbTab, " ")
    CleanString = LTrim(CleanString)
    CleanString = RTrim(CleanString)
End Function

Public Function RemoveBlanksTabs(s As String) As String
    RemoveBlanksTabs = Replace(s, vbTab, "")
    RemoveBlanksTabs = Replace(s, " ", "")
    RemoveBlanksTabs = LTrim(RemoveBlanksTabs)
    RemoveBlanksTabs = RTrim(RemoveBlanksTabs)
End Function

Public Function FindApostropheAndCutRightPart(s As String)
    On Error GoTo FindApostropheAndCutRightPartErr
    Dim occurence As Integer
    occurence = InStr(1, s, Chr(145))
    s = Left(s, occurence - 1)
    Exit Function
FindApostropheAndCutRightPartErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modStrings: Function FindApostropheAndCutRightPart")
End Function

Public Function getTabs(ByVal index As Integer) As String
    getTabs = ""
    Dim i As Integer
    For i = 1 To index
        getTabs = getTabs & vbTab
    Next i
End Function

'File name
Public Function GetFileName(sFile As String)
    On Error GoTo GetFileNameErr
    Dim s As String, CountChar As Integer
    s = Left(sFile, InStrRev(sFile, "\"))
    CountChar = Len(sFile) - Len(s)
    GetFileName = right(sFile, CountChar) ' myfile.txt
    Exit Function
GetFileNameErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modStrings: Function GetFileName")
End Function

Public Function GetFileRoot(sFile As String) As String
    On Error GoTo GetFileRootErr
    Dim s As String
    s = GetFileName(sFile)
    GetFileRoot = Left(s, Len(s) - 4) 'myfile
    Exit Function
GetFileRootErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modStrings: Function GetFileRoot")
End Function

Public Function GetFilePath(sFile As String) As String
    On Error GoTo GetFilePathErr
    GetFilePath = Left(sFile, InStrRev(sFile, "\"))
    Exit Function
GetFilePathErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modStrings: Function GetFilePath")
End Function

Public Function GetFileExtension(sFile As String) As String
    On Error GoTo GetFileExtensionErr
    GetFileExtension = right(sFile, 3)
    Exit Function
GetFileExtensionErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modString: Function GetFileExtension")
End Function

Public Function RemoveEmptyLines(ByVal sFile As String)
'    Dim i As Long
'    'Dim sfile As String
'    Dim sLine() As String
'    'sfile = Project.Item(ActiveProject).sFileName
'    Dim fil, ts As TextStream
'    Set fil = CreateObject("Scripting.FileSystemObject")
'    Set ts = fil.OpenTextFile(sfile, ForReading, TristateUseDefault)
'    ts.ReadAll
'    ReDim sLine(1 To ts.Line - 1)
'    ts.Close
'    Set ts = Nothing
'    Set ts = fil.OpenTextFile(sfile, ForReading, TristateUseDefault)
'    For i = 1 To UBound(sLine)
'        sLine(i) = ts.ReadLine
'        sLine(i) = CleanLines(sLine(i))
'    Next i
'    ts.Close
'    Set ts = Nothing
'    Set ts = fil.OpenTextFile(sfile, ForWriting, TristateUseDefault)
'    For i = 1 To UBound(sLine)
'        If sLine(i) <> "" Then
'            ts.WriteLine sLine(i)
'        End If
'    Next i
'    ts.Close
'    Set ts = Nothing
'    Set fil = Nothing
    
End Function

' Validations
Public Function ValidateText(ByVal CT As Control, Cancel As Boolean)
    If Len(LTrim(CT.Text)) = 0 Then
        Cancel = True
        MsgBox "Invalid Data", vbCritical + vbOKOnly
    End If
End Function

Public Function ValidateNumeric(ByVal CT As Control, Cancel As Boolean)
    If IsNumeric(CT.Text) = False Then
        Cancel = True
        MsgBox "Invalid Format", vbCritical + vbOKOnly
        Exit Function
    End If
    If Len(LTrim(CT.Text)) = 0 Then
        Cancel = True
        MsgBox "Invalid Data", vbCritical + vbOKOnly
    End If
End Function

Public Function ValidateNumericNoneNullPozitiveOrNone(ByVal CT As Control, Cancel As Boolean)
    If LTrim(CT.Text) = "" Then Exit Function
    If IsNumeric(CT.Text) = False Then
        Cancel = True
        MsgBox "Invalid Format", vbCritical + vbOKOnly
        Exit Function
    End If
    If Val_(CT.Text) <= 0 Then
        Cancel = True
        MsgBox "Enter Non-Null Pozitive Value, Or Empty Field", vbCritical + vbOKOnly
        Exit Function
    End If
End Function

Public Function ValidateNumericPozitiveOrNone(ByVal CT As Control, Cancel As Boolean)
    If LTrim(CT.Text) = "" Then Exit Function
    If IsNumeric(CT.Text) = False Then
        Cancel = True
        MsgBox "Invalid Format", vbCritical + vbOKOnly
        Exit Function
    End If
    If Val_(CT.Text) < 0 Then
        Cancel = True
        MsgBox "Enter Null Or Pozitive Value, Or Empty Field", vbCritical + vbOKOnly
        Exit Function
    End If
End Function

Public Function ValidateNumericOrNone(ByVal CT As Control, Cancel As Boolean)
    If LTrim(CT.Text) = "" Then Exit Function
    If IsNumeric(CT.Text) = False Then
        Cancel = True
        MsgBox "Invalid Format", vbCritical + vbOKOnly
        Exit Function
    End If
    If IsNumeric(CT.Text) = False Then
        Cancel = True
        MsgBox "Invalid Format", vbCritical + vbOKOnly
        Exit Function
    End If
End Function

Public Function ValidateNonNullPozitive(ByVal CT As Control, Cancel As Boolean)
    If LTrim(CT.Text) = "" Then Exit Function
    If IsNumeric(CT.Text) = False Then
        Cancel = True
        MsgBox "Invalid Format", vbCritical + vbOKOnly
        Exit Function
    End If
    If Val_(CT.Text) <= 0 Then
        Cancel = True
        MsgBox "Enter Non-Null Pozitive Value", vbCritical + vbOKOnly
        Exit Function
    End If
End Function

Public Function ValidateSubunitary(ByVal CT As Control, Cancel As Boolean)
    If LTrim(CT.Text) = "" Then Exit Function
    If IsNumeric(CT.Text) = False Then
        Cancel = True
        MsgBox "Invalid Format", vbCritical + vbOKOnly
        Exit Function
    End If
    If Val_(CT.Text) < 0 Or Val_(CT.Text) > 1 Then
        Cancel = True
        MsgBox "Enter Value between 0 and 1.", vbCritical + vbOKOnly
        Exit Function
    End If
End Function

Public Function ValidateNullOrPozitive(ByVal CT As Control, Cancel As Boolean)
    If LTrim(CT.Text) = "" Then Exit Function
    If IsNumeric(CT.Text) = False Then
        Cancel = True
        MsgBox "Invalid Format", vbCritical + vbOKOnly
        Exit Function
    End If
    If Val_(CT.Text) < 0 Then
        Cancel = True
        MsgBox "Enter Null Or Pozitive Value", vbCritical + vbOKOnly
        Exit Function
    End If
End Function

Public Function ValidateID_Soudure(ByVal sIndex As String) As Boolean
    On Error GoTo ValidateID_SoudureErr
    'If IsNumeric(sIndex) = False Then ValidateID_Soudure = False: Exit Function
    Select Case Val(sIndex)
        Case 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 50, 51, 52
            ValidateID_Soudure = True
        Case Else
            ValidateID_Soudure = False
    End Select
    Exit Function
ValidateID_SoudureErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modStrings: Function ValidateID_Soudure")
End Function

Public Function StartTime(Optional dblStart As Double) As Double
    Static s As Double
    If dblStart <> 0 Then s = dblStart
    StartTime = s
End Function

Public Function ElapsedTime(Optional dblEnd As Double, Optional lIterations As Long) As Double
    ElapsedTime = (dblEnd - StartTime) / lIterations
    Dim fs, F
    Set fs = CreateObject("Scripting.FileSystemObject")
    Set F = fs.OpenTextFile(App.Path & "\time.log", ForAppending, TristateUseDefault)
    F.Write Project.Item(lProjectCount).sFileName & _
            vbTab & "->" & vbTab & ElapsedTime & " seconds" & vbCrLf
    F.Close
End Function

Function Windows_Path()
    Dim chaine As String, size As Integer, retval
    size = 255
    chaine = Space$(255)
    retval = GetWindowsDirectory(chaine, size)
    chaine = Left$(chaine, retval)
    If right$(chaine, 1) <> "\" Then
        chaine = chaine & "\"
    End If
    Windows_Path = chaine
End Function

Public Sub FlexCopy(MSH1 As MSFlexGrid)
    Dim i As Integer
    Dim j As Integer
    Dim Indexi As Integer
    Dim Indexj As Integer
    Dim Row1 As Integer, Row2 As Integer, Col1 As Integer, Col2 As Integer
    Indexi = 0
    Indexj = 0
    If MSH1.ColSel >= MSH1.col Then
        Col1 = MSH1.col
        Col2 = MSH1.ColSel
    Else
        Col1 = MSH1.ColSel
        Col2 = MSH1.col
    End If
    If MSH1.RowSel >= MSH1.Row Then
        Row1 = MSH1.Row
        Row2 = MSH1.RowSel
    Else
        Row1 = MSH1.RowSel
        Row2 = MSH1.Row
    End If
    ReDim ArrayCopy(Row2 - Row1 + 1, Col2 - Col1 + 1)
    For i = Row1 To Row2
        Indexi = Indexi + 1
        Indexj = 0
        For j = Col1 To Col2
            Indexj = Indexj + 1
            ArrayCopy(Indexi, Indexj) = MSH1.TextMatrix(i, j)
        Next j
    Next i
End Sub

Public Sub FlexPaste(MSH1 As MSFlexGrid)
On Error GoTo 1
    Dim i As Integer
    Dim j As Integer
    Dim Indexi As Integer
    Dim Indexj As Integer
    Dim Row1 As Integer, Row2 As Integer, Col1 As Integer, Col2 As Integer
    Indexi = 0
    Indexj = 0
    If MSH1.ColSel >= MSH1.col Then
        Col1 = MSH1.col
        Col2 = MSH1.ColSel
    Else
        Col1 = MSH1.ColSel
        Col2 = MSH1.col
    End If
    If MSH1.RowSel >= MSH1.Row Then
        Row1 = MSH1.Row
        Row2 = MSH1.RowSel
    Else
        Row1 = MSH1.RowSel
        Row2 = MSH1.Row
    End If
    ReDim ArrayPaste(Row2 - Row1 + 1, Col2 - Col1 + 1)
    If UBound(ArrayCopy, 1) = 1 And UBound(ArrayCopy, 2) = 1 Then
        For i = 1 To UBound(ArrayPaste, 1)
            For j = 1 To UBound(ArrayPaste, 2)
                ArrayPaste(i, j) = ArrayCopy(1, 1)
            Next j
        Next i
    End If
    If UBound(ArrayCopy, 1) > 1 And UBound(ArrayCopy, 2) > 1 Then
        Row2 = Row1 + UBound(ArrayCopy, 1) - 1
        MSH1.RowSel = Row1 + UBound(ArrayCopy, 1) - 1
        Col2 = Col1 + UBound(ArrayCopy, 2) - 1
        MSH1.ColSel = Col1 + UBound(ArrayCopy, 2) - 1
        ReDim ArrayPaste(Row2 - Row1 + 1, Col2 - Col1 + 1)
        ArrayPaste = ArrayCopy
    End If
    If UBound(ArrayCopy, 1) = 1 And UBound(ArrayCopy, 2) > 1 Then
        ReDim ArrayPaste(UBound(ArrayPaste, 1), UBound(ArrayCopy, 2))
        Col2 = Col1 + UBound(ArrayCopy, 2) - 1
        MSH1.ColSel = Col1 + UBound(ArrayCopy, 2) - 1
        For i = 1 To UBound(ArrayPaste, 1)
            For j = 1 To UBound(ArrayCopy, 2)
                ArrayPaste(i, j) = ArrayCopy(1, j)
            Next j
        Next i
    End If
    If UBound(ArrayCopy, 1) > 1 And UBound(ArrayCopy, 2) = 1 Then
        ReDim ArrayPaste(UBound(ArrayCopy, 1), UBound(ArrayPaste, 2))
        Row2 = Row1 + UBound(ArrayCopy, 1) - 1
        MSH1.RowSel = Row1 + UBound(ArrayCopy, 1) - 1
        For i = 1 To UBound(ArrayCopy, 1)
            For j = 1 To UBound(ArrayPaste, 2)
                ArrayPaste(i, j) = ArrayCopy(i, 1)
            Next j
        Next i
    End If
    For i = Row1 To Row2
        Indexi = Indexi + 1
        Indexj = 0
        For j = Col1 To Col2
            Indexj = Indexj + 1
            MSH1.TextMatrix(i, j) = ArrayPaste(Indexi, Indexj)
        Next j
    Next i
Exit Sub
1

MsgBox "Copied area exceeds the grid borders. Data could not be pasted.", vbCritical + vbOKOnly

End Sub

Public Function GetLetterCorrespondingToNumber(i As Integer) As String
    Select Case i
        Case 1
            GetLetterCorrespondingToNumber = "a"
        Case 2
            GetLetterCorrespondingToNumber = "b"
        Case 3
            GetLetterCorrespondingToNumber = "c"
        Case 4
            GetLetterCorrespondingToNumber = "d"
        Case 5
            GetLetterCorrespondingToNumber = "e"
        Case 6
            GetLetterCorrespondingToNumber = "f"
        Case 7
            GetLetterCorrespondingToNumber = "g"
        Case 8
            GetLetterCorrespondingToNumber = "h"
        Case 9
            GetLetterCorrespondingToNumber = "i"
        Case 10
            GetLetterCorrespondingToNumber = "j"
        Case 11
            GetLetterCorrespondingToNumber = "k"
        Case 12
            GetLetterCorrespondingToNumber = "l"
        Case 13
            GetLetterCorrespondingToNumber = "m"
        Case 14
            GetLetterCorrespondingToNumber = "n"
        Case 15
            GetLetterCorrespondingToNumber = "o"
        Case 16
            GetLetterCorrespondingToNumber = "p"
        Case 17
            GetLetterCorrespondingToNumber = "q"
        Case 18
            GetLetterCorrespondingToNumber = "r"
        Case 19
            GetLetterCorrespondingToNumber = "s"
        Case 20
            GetLetterCorrespondingToNumber = "t"
        Case 21
            GetLetterCorrespondingToNumber = "u"
        Case 22
            GetLetterCorrespondingToNumber = "v"
        Case 23
            GetLetterCorrespondingToNumber = "x"
        Case 24
            GetLetterCorrespondingToNumber = "y"
        Case 25
            GetLetterCorrespondingToNumber = "w"
        Case 26
            GetLetterCorrespondingToNumber = "z"
    End Select
End Function

Public Function Percent(ByVal a As Double, ByVal b As Double, ByVal decimals As Integer) As Double
    On Error GoTo PercentErr
    Percent = Round(Divide((a - b), a) * 100, decimals)
    Exit Function
PercentErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modStrings: Function Percent")
End Function

Public Sub WriteNOH(ByVal ProjectIndex As Integer)
    Dim fs, F
    Set fs = CreateObject("Scripting.FileSystemObject")
    Set F = fs.OpenTextFile(App.Path & "\NOH.log", ForAppending, TristateUseDefault)
    Dim cPanel As cPanel, s As String, i As Integer
    For Each cPanel In Project.Item(ProjectIndex).colPanel
        s = "P_" & cPanel.pNumber
        For i = 1 To 10
            s = s & vbTab & cPanel.colConnections.Item(i)
        Next i
        F.WriteLine s
    Next cPanel
    'f.Write Date & vbTab & time & vbTab & "Error location -> " & s & vbCrLf
    MsgBox "NOH written.", vbInformation + vbOKOnly
    F.Close
End Sub

Public Function SortAscending(ByRef v As Variant) As Variant
    On Error GoTo SortAscendingErr
    Dim V1 As Variant
    Dim i As Integer, j As Integer
    If UBound(v) < 2 Then Exit Function
    For i = 1 To UBound(v)
        v(i) = Val(v(i))
    Next i
    For i = 1 To UBound(v)
        For j = i To UBound(v) - 1
            If Val(v(i)) > Val(v(j + 1)) Then
                V1 = Val(v(j + 1))
                v((j + 1)) = Val(v(i))
                v(i) = V1
            End If
        Next j
    Next i
    SortAscending = v
    Exit Function
SortAscendingErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modStrings: Function SortAscending")
End Function

'Private Sub FlexToExcel()
'Dim xlObject As Excel.Application
'Dim xlWB As Excel.Workbook
'
'    Set xlObject = New Excel.Application
'
'    'This Adds a new woorkbook, you could open the workbook from file also
'    Set xlWB = xlObject.Workbooks.Add
'
'    Clipboard.Clear 'Clear the Clipboard
'    With MSFlexGrid1
'        'Select Full Contents (You could also select partial content)
'        .col = 0               'From first column
'        .Row = 0               'From first Row (header)
'        .ColSel = .Cols - 1    'Select all columns
'        .RowSel = .Rows - 1    'Select all rows
'        Clipboard.SetText .Clip 'Send to Clipboard
'    End With
'
'    With xlObject.ActiveWorkbook.ActiveSheet
'        .Range("A1").Select 'Select Cell A1 (will paste from here, to different cells)
'        .Paste              'Paste clipboard contents
'    End With
'
'    ' This makes Excel visible
'    xlObject.Visible = True
'End Sub
'
'Private Sub ExcelToFlexgrid()
'Dim xlObject As Excel.Application
'Dim xlWB As Excel.Workbook
'
'    Set xlObject = New Excel.Application
'    Set xlWB = xlObject.Workbooks.Open("C:\Book1.xls") 'Open your book here
'
'    Clipboard.Clear
'    With xlObject.ActiveWorkbook.ActiveSheet
'        .Range("A1:F7").Copy 'Set selection to Copy
'    End With
'
'    With MSFlexGrid1
'        .Redraw = False     'Dont draw until the end, so we avoid that flash
'        .Row = 0            'Paste from first cell
'        .col = 0
'        .RowSel = .Rows - 1 'Select maximum allowed (your selection shouldnt be greater than this)
'        .ColSel = .Cols - 1
'        .Clip = Replace(Clipboard.GetText, vbNewLine, vbCr) 'Replace carriage return with the correct one
'        .col = 1            'Just to remove that blue selection from Flexgrid
'        .Redraw = True      'Now draw
'    End With
'
'    xlObject.DisplayAlerts = False 'To avoid "Save woorkbook" messagebox
'
'    'Close Excel
'    xlWB.Close
'    xlObject.Application.Quit
'    Set xlWB = Nothing
'    Set xlObject = Nothing
'End Sub

Public Function IsDemo(ByVal ProjectIndex As Integer) As Boolean
    Dim MSG As String
    'MSG = "Demo version." & vbCrLf
    Dim iplates As Integer, ibeams As Integer, ivirtuals As Integer
    Dim index As Integer
    index = 0
    IsDemo = False
    GetNoOfPlates ProjectIndex, iplates, ibeams, ivirtuals
    If iplates + ibeams + ivirtuals > Licensing.MAX_PANELS Then
        MSG = MSG & "The maximum number of panels is restricted to " & Licensing.MAX_PANELS & "." & vbCrLf
        IsDemo = True
    End If
'    index = GetTotalNumberOfDesVar(ProjectIndex)
'    If index > Licensing.MAX_DESIGN_VARIABLES Then
'        MSG = MSG & "The maximum number of design variables is restricted to " & Licensing.MAX_DESIGN_VARIABLES & "." & vbCrLf
'        IsDemo = True
'    End If

    'structural constraints
    Dim oPan As cPanel
    Dim oStr As cStructuralConstraints
    Dim oLoadC As cLoadCase
    Dim GetTotalNumberOfStrConstr As Integer
    For Each oPan In Project.Item(ProjectIndex).colPanel
        GetTotalNumberOfStrConstr = 0
        For Each oLoadC In oPan.colLoadCase
            For Each oStr In oLoadC.colStructuralConstraints
                GetTotalNumberOfStrConstr = GetTotalNumberOfStrConstr + 1
            Next oStr
        Next oLoadC
        If GetTotalNumberOfStrConstr > Licensing.MAX_STRUCTURAL_CONSTRAINTS_ON_PANEL Then
            MSG = MSG & "The maximum number of structural constraints is restricted to " & Licensing.MAX_STRUCTURAL_CONSTRAINTS_ON_PANEL & " on each panel." & vbCrLf
            IsDemo = True
            Exit For
        End If
    Next oPan
    
    'geometrical constraints
    Dim oGeo As cGeometricalConstraints
    Dim GetTotalNumberOfGeoConstr As Integer
    For Each oPan In Project.Item(ProjectIndex).colPanel
        GetTotalNumberOfGeoConstr = 0
        For Each oGeo In oPan.colGeometricalConstraints
            GetTotalNumberOfGeoConstr = GetTotalNumberOfGeoConstr + 1
        Next oGeo
        If oPan.StiffenersFlangeThicknessUpdate <> NoUpdate Then
            GetTotalNumberOfGeoConstr = GetTotalNumberOfGeoConstr + 1
        End If
        If oPan.FramesFlangeThicknessUpdate <> NoUpdate Then
            GetTotalNumberOfGeoConstr = GetTotalNumberOfGeoConstr + 1
        End If
        If GetTotalNumberOfGeoConstr > Licensing.MAX_GEOMETRICAL_CONSTRAINTS_ON_PANEL Then
            MSG = MSG & "The maximum number of geometrical constraints is restricted to " & Licensing.MAX_GEOMETRICAL_CONSTRAINTS_ON_PANEL & " on each panel." & vbCrLf
            IsDemo = True
            Exit For
        End If
    Next oPan
    
    index = GetTotalNumberOfEqRestr(ProjectIndex)
    If index > Licensing.MAX_EQUALITY_RESTRICTIONS Then
        MSG = MSG & "The maximum number of equality restrictions is restricted to " & Licensing.MAX_EQUALITY_RESTRICTIONS & "." & vbCrLf
        IsDemo = True
    End If
    
    index = Project.Item(ProjectIndex).cHeader.colLoadCase.Count
    If index > Licensing.MAX_ACTIVE_LOAD_CASES Then
        MSG = MSG & "The maximum number of load cases is restricted to " & Licensing.MAX_ACTIVE_LOAD_CASES & "." & vbCrLf
        IsDemo = True
    End If
    If IsDemo = True Then
        MsgBox MSG, vbCritical + vbOKOnly, "LBR-5 License Limitation"
    End If
End Function
