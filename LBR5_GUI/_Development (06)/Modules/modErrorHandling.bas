Attribute VB_Name = "modErrorHandling"
' Define your custom errors here.  Be sure to use numbers
' greater than 512, to avoid conflicts with OLE error numbers.
Public Const MyObjectError1 = 1000
Public Const MyObjectError2 = 1010
Public Const MyObjectErrorN = 1234
Public Const MyUnhandledError = 9999
Public Const ErrStr As String = "." & vbCrLf & "Error Location: "

Private Function GetErrorTextFromResource(ErrorNum As Long) As String
    Dim strMsg As String
    ' this function will retrieve an error description from a resource
    ' file (.RES).  The ErrorNum is the index of the string
    ' in the resource file.  Called by RaiseError
    On Error GoTo GetErrorTextFromResourceError
    ' get the string from a resource file
    GetErrorTextFromResource = LoadResString(ErrorNum)
    Exit Function
GetErrorTextFromResourceError:
    If Err.Number <> 0 Then
          GetErrorTextFromResource = "An unknown error has occurred!"
    End If
End Function

Public Sub RaiseError(ErrorNumber As Long, Source As String)
    Dim strErrorText As String
    'there are a number of methods for retrieving the error
    'message.  The following method uses a resource file to
    'retrieve strings indexed by the error number you are
    'raising.
    strErrorText = GetErrorTextFromResource(ErrorNumber)
    'raise an error back to the client
    'Err.Raise vbObjectError + ErrorNumber, Source, strErrorText
    Call ErrLog(Source)
    'Err.Raise vbObjectError + ErrorNumber, Source, "Error location:" & Chr(13) & Source
    Static lErrFlood As Long
    lErrFlood = lErrFlood + 1
    MsgBox "Error description: " & Source & ".", vbCritical + vbOKOnly, "Error"
    'MsgBox Chr(13) & Source, vbCritical + vbOKOnly, "Error"
    Dim MSG As VbMsgBoxResult
    If lErrFlood > 4 Then
        MSG = MsgBox("Too many errors. Do you want to quit the application?", vbCritical + vbYesNo)
        Select Case MSG
            Case vbYes
                End
            Case vbNo
                lErrFlood = 0
        End Select
    End If
End Sub

Public Sub ErrLog(s As String)
    Dim fs, F
    Set fs = CreateObject("Scripting.FileSystemObject")
    Set F = fs.OpenTextFile(App.Path & "\err.log", ForAppending, TristateUseDefault)
    F.Write Date & vbTab & time & vbTab & "Error: " & s & vbCrLf
    F.Close
End Sub
