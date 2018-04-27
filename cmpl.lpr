{%RunCommand $MakeExe($(EdFile)) ./example/test.s}
program cmpl;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils, StrUtils, chardfa, nslex, booty, bingen, lexdfa;

function ReadTextFile(path : AnsiString) : AnsiString;
var
    f : TextFile;
    s : AnsiString;
begin

    Result := '';
    if FileExists(path) then
    begin
        AssignFile(f, path);

        (* Open for reading at top *)
        Reset(f);

        while not eof(f) do
        begin
            readln(f, s);
            Result := Result + s + #10;
        end;
    end;
end;

var
    lexer : TLfnwLexer;
    src : AnsiString;
    tokens : TLfnwLexTokenArray;
    parser : TLfnwParseGen;

    outBytes : TBytes;

    testInt : Integer;
    testBytes : Array[0..3] of Byte;

begin

  lexer := TLfnwLexer.Create();
  parser := TLfnwParseGen.Create();

  src := ReadTextFile(ParamStr(1));
  WriteLn('File: ', ParamStr(1));
  WriteLn(src);

  tokens := lexer.Lex(src);

  testInt := Hex2Dec('000000A1');
  Move(testInt, testBytes[0], 4);

  WriteLn('Hex 2 Dec:');
  WriteLn('00 00 00 01 = ', testInt);
  WriteLn('Bytes: ', testBytes[0], ' ', testBytes[1], ' ', testBytes[2], ' ', testBytes[3]);

  parser.Run(tokens);


  FreeAndNil(lexer);
  FreeAndNil(parser);
  src := '';

end.

