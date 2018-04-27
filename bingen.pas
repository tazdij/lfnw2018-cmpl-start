unit bingen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils;

type
    //TByteArray = Array of Byte;

    TBinGen = class(TObject)
        private
            FFile : File of Byte;
            FBytes : TBytes;
            FBytePos : Cardinal;
        public

            procedure WriteByte(AInput : Byte);
            procedure WriteBytePos(AInput : Byte; APos : Cardinal);
            procedure WriteHexStrBEtoLE(AInput : AnsiString; AFixedSize : Boolean = True; ASizeBytes : Byte = 4);
            procedure WriteHexStrLEtoBE(AInput : AnsiString; AFixedSize : Boolean = True; ASizeBytes : Byte = 4);
            procedure WriteHexStr(AInput : AnsiString; AFixedSize : Boolean = True; ASizeBytes : Byte = 4);
            procedure WriteDecStrLE(AInput : AnsiString; AFixedSize : Boolean = True; ASizeBytes : Byte = 4);
            procedure WriteDecStrBE(AInput : AnsiString; AFixedSize : Boolean = True; ASizeBytes : Byte = 4);
            procedure WriteChar(AInput : Char);
            procedure WriteString(AInput : AnsiString);

            procedure SaveFile(filename : AnsiString);

            constructor Create();
            destructor Destroy(); override;

            property BytePos : Cardinal read FBytePos;
    end;

implementation

procedure TBinGen.WriteByte(AInput : Byte);
var i : Cardinal;
begin
  i := Length(self.FBytes);
  SetLength(self.FBytes, i + 1);

  self.FBytes[i] := AInput;

  self.FBytePos := i;
end;

procedure TBinGen.WriteBytePos(AInput : Byte; APos : Cardinal);
begin
  self.FBytes[APos] := AInput;
end;

procedure TBinGen.WriteHexStrBEtoLE(AInput : AnsiString; AFixedSize : Boolean = True; ASizeBytes : Byte = 4);
var li : LongInt;
    bytes : Array[0..3] of Byte;
begin
  li := Hex2Dec(AInput);
  Move(li, bytes[0], 4);
  self.WriteByte(bytes[0]);
  self.WriteByte(bytes[1]);
  self.WriteByte(bytes[2]);
  self.WriteByte(bytes[3]);
end;

procedure TBinGen.WriteHexStrLEtoBE(AInput : AnsiString; AFixedSize : Boolean = True; ASizeBytes : Byte = 4);
begin

end;

procedure TBinGen.WriteHexStr(AInput : AnsiString; AFixedSize : Boolean = True; ASizeBytes : Byte = 4);
begin

end;

procedure TBinGen.WriteDecStrLE(AInput : AnsiString; AFixedSize : Boolean = True; ASizeBytes : Byte = 4);
begin

end;

procedure TBinGen.WriteDecStrBE(AInput : AnsiString; AFixedSize : Boolean = True; ASizeBytes : Byte = 4);
begin

end;

procedure TBinGen.WriteChar(AInput : Char);
begin

end;

procedure TBinGen.WriteString(AInput : AnsiString);
begin

end;

procedure TBinGen.SaveFile(filename : AnsiString);
var i : Integer;
begin
  AssignFile(self.FFile, filename);
  Rewrite(self.FFile);
  Truncate(self.FFile);

  for i := 0 to Length(self.FBytes) - 1 do
  begin
    Write(self.FFile, self.FBytes[i]);
  end;

  CloseFile(self.FFile);
end;

constructor TBinGen.Create();
begin
  SetLength(self.FBytes, 0);
  self.FBytePos := 0;
end;

destructor TBinGen.Destroy();
begin
  SetLength(self.FBytes, 0);
end;

end.

