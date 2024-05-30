Attribute VB_Name = "modFunctions"
Option Explicit

Public Function InitMRU() 'Initialize MRU
    Dim fso As New FileSystemObject, fil As file, ts As TextStream
    Dim s As String
    Dim i As Integer
    'create recent file if it doesn't exist
    If Not fso.FileExists(GetCommonAppPath & "\LBR-5.7\recent.dat") Then
        fso.CreateTextFile GetCommonAppPath & "\LBR-5.7\recent.dat", False, False
    End If
    Set fil = fso.GetFile(GetCommonAppPath & "\LBR-5.7\recent.dat")
    Set ts = fil.OpenAsTextStream(ForReading)
    
    'read recent files
    Set ts = fil.OpenAsTextStream(ForReading)
    fMainForm.lstbuffMRU.Clear
    fMainForm.lstMRU.Clear
    'fill lst buffer
    Do While ts.AtEndOfStream = False
        s = ts.ReadLine
        fMainForm.lstbuffMRU.AddItem s
    Loop
    
    'fill sorted lst
    For i = fMainForm.lstbuffMRU.ListCount - 1 To 0 Step -1
        fMainForm.lstMRU.AddItem fMainForm.lstbuffMRU.List(i)
    Next i
    
    'fill menu
     For i = 1 To fMainForm.lstMRU.ListCount
        fMainForm.lstMRU.ListIndex = i - 1
        fMainForm.mnuFileMRU(i).Visible = True
        fMainForm.mnuFileMRU(i).Caption = i & " " & GetFileName(fMainForm.lstMRU.Text)
     Next i
     If fMainForm.lstMRU.ListCount > 0 Then
        fMainForm.mnuFileBar6.Visible = True
     End If

End Function

Public Function RemoveFromMRU(ByRef sFile As String)
    Dim i As Integer
    'remove from lstMRU, from the menu and from lstBuffMRU
    For i = 0 To fMainForm.lstMRU.ListCount - 1
        If fMainForm.lstMRU.List(i) = sFile Then
            fMainForm.lstMRU.RemoveItem i
            fMainForm.mnuFileMRU(i + 1).Caption = ""
            fMainForm.mnuFileMRU(i + 1).Visible = False
            If fMainForm.lstMRU.ListCount = 0 Then
                fMainForm.mnuFileBar6.Visible = False
            End If
            UpdateRecentList
        End If
        If fMainForm.lstbuffMRU.List(i) = sFile Then
            fMainForm.lstbuffMRU.RemoveItem i
        End If
    Next i
    'update recent file
    Dim fso As New FileSystemObject, fil As file, ts As TextStream
    Set fil = fso.GetFile(GetCommonAppPath & "\LBR-5.7\recent.dat")
    Set ts = fil.OpenAsTextStream(ForWriting)
    For i = 0 To fMainForm.lstbuffMRU.ListCount - 1
        ts.WriteLine fMainForm.lstbuffMRU.List(i)
    Next i
    
End Function

Public Function AddtoMRU(ByRef sFile As String)
    Dim i As Integer
    'check if file is already in the list and put it first
    For i = 0 To fMainForm.lstbuffMRU.ListCount - 1
        If sFile = fMainForm.lstbuffMRU.List(i) Then
            fMainForm.lstbuffMRU.RemoveItem i
        End If
    Next i
    'add file on last position
    fMainForm.lstbuffMRU.AddItem sFile
    
    ' if number of files > 10, remove the oldest
    If fMainForm.lstbuffMRU.ListCount > 10 Then
        fMainForm.lstbuffMRU.RemoveItem 0
    End If
    
    Dim fso As New FileSystemObject
    'create recent file if it doesn't exist
    If Not fso.FileExists(GetCommonAppPath & "\LBR-5.7\recent.dat") Then
        fso.CreateTextFile GetCommonAppPath & "\LBR-5.7\recent.dat", False, False
    End If
    
    'update recent file
    Dim fil As file, ts As TextStream
    Set fil = fso.GetFile(GetCommonAppPath & "\LBR-5.7\recent.dat")
    Set ts = fil.OpenAsTextStream(ForWriting)
    For i = 0 To fMainForm.lstbuffMRU.ListCount - 1
        ts.WriteLine fMainForm.lstbuffMRU.List(i)
    Next i
    ts.Close
    
    'update lstMRU
    fMainForm.lstMRU.Clear
    For i = fMainForm.lstbuffMRU.ListCount - 1 To 0 Step -1
        fMainForm.lstMRU.AddItem fMainForm.lstbuffMRU.List(i)
    Next i
    
    'fill menu
     For i = 1 To fMainForm.lstMRU.ListCount
        fMainForm.lstMRU.ListIndex = i - 1
        fMainForm.mnuFileMRU(i).Visible = True
        fMainForm.mnuFileMRU(i).Caption = i & " " & GetFileName(fMainForm.lstMRU.Text)
     Next i
     If fMainForm.lstMRU.ListCount > 0 Then
        fMainForm.mnuFileBar6.Visible = True
     End If

End Function

Public Function UpdateRecentList()
    Dim i As Integer
    Dim oPro As cProject
    For i = 1 To 10
        For Each oPro In Project
            Project.Item(oPro.index).frmProject.mnuFileMRU(i).Caption = fMainForm.mnuFileMRU(i).Caption
            Project.Item(oPro.index).frmProject.mnuFileMRU(i).Visible = fMainForm.mnuFileMRU(i).Visible
            Project.Item(oPro.index).frmProject.mnuFileBar6.Visible = fMainForm.mnuFileBar6.Visible
        Next oPro
    Next i
End Function

Public Function getGravityCenter(ByVal ProjectIndex As Integer) As Double
    On Error GoTo getGravityCenterErr
    Dim oPanel As cPanel
    For Each oPanel In Project.Item(ProjectIndex).colPanel
        
    Next oPanel
    
    Exit Function
getGravityCenterErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modFunctions: Function getGravityCenter")
End Function

