LOCAL CRLF
CRLF = CHR(13) + CHR(10)
_out = []
Response.Write([<html>]+ CRLF +;
   [<head>]+ CRLF +;
   [<title>])

Response.Write(TRANSFORM( EVALUATE([ TRIM(oHelp.oTopic.Topic) ]) ))

Response.Write([</title>]+ CRLF +;
   [<LINK rel="stylesheet" type="text/css" href="templates/wwhelp.css">]+ CRLF +;
   [</head>]+ CRLF +;
   [<body>]+ CRLF +;
   [<p>]+ CRLF +;
   [<h1>])

Response.Write(TRANSFORM( EVALUATE([ TRIM(oHelp.oTopic.Topic) ]) ))

Response.Write([</h1>]+ CRLF +;
   [<br><p>])

Response.Write(TRANSFORM( EVALUATE([ oHelp.FormatHTML(oHelp.oTopic.Body) ]) ))

Response.Write([]+ CRLF +;
   [<p>]+ CRLF +;
   [])

Response.Write(TRANSFORM( EVALUATE([ IIF(VARTYPE(pcToc)="C",pcToc,"") ]) ))

Response.Write([]+ CRLF +;
   [<p>])

Response.Write(TRANSFORM( EVALUATE([ oHelp.FormatHTML(oHelp.oTopic.Remarks)  ]) ))

Response.Write([]+ CRLF +;
   [</body>]+ CRLF +;
   [</html>]+ CRLF +;
   [])
