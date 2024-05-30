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
   [<h2>])

Response.Write(TRANSFORM( EVALUATE([ TRIM(oHelp.oTopic.Topic) ]) ))

Response.Write([</h2>]+ CRLF +;
   [<p>])

Response.Write(TRANSFORM( EVALUATE([ oHelp.FormatHTML( oHelp.oTopic.Body)  ]) ))

Response.Write([]+ CRLF +;
   [<p>]+ CRLF +;
   [</body>]+ CRLF +;
   [</html>])
