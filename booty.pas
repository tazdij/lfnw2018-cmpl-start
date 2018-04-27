unit booty;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, nslex, lexdfa, bingen;

type

  PLfnwLabel = ^TLfnwLabel;
  TLfnwLabel = record
    LabelName : AnsiString;
    LabelPos : Cardinal;
  end;

  PLfnwLabelDefer = ^TLfnwLabelDefer;
  TLfnwLabelDefer = record
    SourcePos : Cardinal;
    LabelName : AnsiString;
  end;


  TLfnwParseGen = class(TObject)
    private
      FLabels : Array of TLfnwLabel;
      FLabelDefers : Array of TLfnwLabelDefer;

      FBinGen : TBinGen;
      FDfa : TLexDFA;

      procedure VM_LabelDef(ADfaTokens : TDFATokenArray);
      procedure AddLabel(ALabelName : AnsiString; AAddress : Cardinal);
      function GetLabelAddr(ALabelName : AnsiString) : Cardinal;


      procedure AddLabelDefer(ALabelName : AnsiString; ASourcePos : Cardinal);
      function GetLabelDefer(ALabelName : AnsiString) : Cardinal;

      procedure VM_OpNone(ADfaTokens : TDFATokenArray);
      procedure VM_OpHALT(ADfaTokens : TDFATokenArray);
      procedure VM_OpMOVRIl(ADfaTokens : TDFATokenArray);
      procedure VM_OpMOVHIl(ADfaTokens : TDFATokenArray);
      procedure VM_OpMOVHHBx(ADfaTokens : TDFATokenArray);

      procedure VM_OpADDRRI(ADfaTokens : TDFATokenArray);

      procedure VM_OpPUSHI(ADfaTokens : TDFATokenArray);
      procedure VM_OpPUSHRI(ADfaTokens : TDFATokenArray);

      procedure VM_OpPOPRI(ADfaTokens : TDFATokenArray);

      procedure VM_OpARGRI(ADfaTokens : TDFATokenArray);

      procedure VM_OpCALL_A(ADfaTokens : TDFATokenArray);

      procedure VM_OpRET(ADfaTokens : TDFATokenArray);
      procedure VM_OpRET_I(ADfaTokens : TDFATokenArray);

      procedure VM_OpPRINTHOI(ADfaTokens : TDFATokenArray);
      procedure VM_OpPRINTHOC(ADfaTokens : TDFATokenArray);

      procedure VM_OpCMPRRI(ADFATokens : TDFATokenArray);


      //procedure FreeLabels();
      //procedure NewLabel(id : AnsiString);

    public

      procedure Run(ATokens : TLfnwLexTokenArray);

      constructor Create();
      destructor Destroy(); override;
  end;

implementation

procedure TLfnwParseGen.VM_LabelDef(ADfaTokens : TDFATokenArray);
var addr : Cardinal;
begin
  //WriteLn('LABELDEF. ' + IntToStr(Length(ADfaTokens)));
  // Get the address of next instruction
  addr := self.FBinGen.BytePos + 1;
  WriteLn('LABELDEF. ' + IntToStr(addr));
  self.AddLabel(ADfaTokens[0].TokenVal, addr);
end;

procedure TLfnwParseGen.AddLabel(ALabelName : AnsiString; AAddress : Cardinal);
var newIdx : LongInt = 0;
begin
  SetLength(self.FLabels, Length(self.FLabels) + 1);
  newIdx := Length(self.FLabels) - 1;

  self.FLabels[newIdx].LabelPos := AAddress;
  self.FLabels[newIdx].LabelName := Copy(ALabelName, 1, Length(ALabelName));
end;

function TLfnwParseGen.GetLabelAddr(ALabelName : AnsiString) : Cardinal;
var i : LongInt = 0;
begin
  Result := 0;

  for i := 0 to Length(self.FLabels) - 1 do
  begin
    if AnsiCompareStr(ALabelName, self.FLabels[i].LabelName) = 0 then
    begin
        Result := self.FLabels[i].LabelPos;
        Break;
    end;
  end;

end;

procedure TLfnwParseGen.AddLabelDefer(ALabelName : AnsiString; ASourcePos : Cardinal);
var newIdx : LongInt = 0;

begin
    SetLength(self.FLabelDefers, Length(self.FLabelDefers) + 1);
    newIdx := Length(self.FLabelDefers) - 1;

    self.FLabelDefers[newIdx].SourcePos := ASourcePos;
    Self.FLabelDefers[newIdx].LabelName := Copy(ALabelName, 1, Length(ALabelName));
end;

function TLfnwParseGen.GetLabelDefer(ALabelName : AnsiString) : Cardinal;
var i : LongInt;

begin
  Result := 0;
  for i := 0 to Length(self.FLabelDefers) - 1 do
  begin
    if AnsiCompareStr(ALabelName, self.FLabelDefers[i].LabelName) = 0 then
    begin
        // We have a match
        Result := self.FLabelDefers[i].SourcePos;
        Break;
    end;
  end;

end;

procedure TLfnwParseGen.VM_OpNone(ADfaTokens : TDFATokenArray);
begin
  WriteLn('None');
end;

procedure TLfnwParseGen.VM_OpADDRRI(ADfaTokens : TDFATokenArray);
var LReg : Byte;
    RReg : Byte;
begin
  WriteLn('ADDI.');
  LReg := Byte(StrToInt(RightStr(ADfaTokens[1].TokenVal, Length(ADfaTokens[1].TokenVal) - 1)));
  RReg := Byte(StrToInt(RightStr(ADfaTokens[2].TokenVal, Length(ADfaTokens[2].TokenVal) - 1)));

  self.FBinGen.WriteByte(30);
  self.FBinGen.WriteByte(LReg);
  self.FBinGen.WriteByte(RReg);
end;

procedure TLfnwParseGen.VM_OpHALT(ADfaTokens : TDFATokenArray);
begin
  WriteLn('HALT.');
  self.FBinGen.WriteByte(0); // Write 00, OpCode for HALT
end;

procedure TLfnwParseGen.VM_OpMOVRIl(ADfaTokens : TDFATokenArray);
begin
  self.FBinGen.WriteByte(3); // Write 01, OpCode for MOV (RIl)
end;

procedure TLfnwParseGen.VM_OpMOVHIl(ADfaTokens : TDFATokenArray);
begin
  WriteLn('MOVHIl.');
  self.FBinGen.WriteByte(1); // Write 01, OpCode for MOV (HIl)
  self.FBinGen.WriteHexStrBEtoLE(ADfaTokens[1].TokenVal);
  self.FBinGen.WriteHexStrBEtoLE(ADfaTokens[2].TokenVal);
end;

procedure TLfnwParseGen.VM_OpMOVHHBx(ADfaTokens : TDFATokenArray);
begin
  WriteLn('MOVHHBx.');
  self.FBinGen.WriteByte(2); // Write 01, OpCode for MOV (HHBx)
  self.FBinGen.WriteHexStrBEtoLE(ADfaTokens[1].TokenVal);
  self.FBinGen.WriteHexStrBEtoLE(ADfaTokens[2].TokenVal);
  self.FBinGen.WriteHexStrBEtoLE(ADfaTokens[3].TokenVal);
end;

procedure TLfnwParseGen.VM_OpPUSHI(ADfaTokens : TDFATokenArray);
begin
  self.FBinGen.WriteByte(10);
  self.FBinGen.WriteHexStrBEtoLE(ADfaTokens[1].TokenVal);

  WriteLn('PUSHIl. ' + ADfaTokens[1].TokenVal);
end;

procedure TLfnwParseGen.VM_OpPUSHRI(ADfaTokens : TDFATokenArray);
begin

  self.FBinGen.WriteByte(11);
  self.FBinGen.WriteByte(StrToInt(RightStr(ADfaTokens[1].TokenVal, Length(ADfaTokens[1].TokenVal) - 1)));

  WriteLn('PUSHRI. ' + RightStr(ADfaTokens[1].TokenVal, Length(ADfaTokens[1].TokenVal) - 1));
end;

procedure TLfnwParseGen.VM_OpPOPRI(ADfaTokens : TDFATokenArray);
begin

  self.FBinGen.WriteByte(21);
  self.FBinGen.WriteByte(StrToInt(RightStr(ADfaTokens[1].TokenVal, Length(ADfaTokens[1].TokenVal) - 1)));

  WriteLn('POPRI. ' + RightStr(ADfaTokens[1].TokenVal, Length(ADfaTokens[1].TokenVal) - 1));
end;

procedure TLfnwParseGen.VM_OpARGRI(ADfaTokens : TDFATokenArray);
begin

  self.FBinGen.WriteByte(90);
  self.FBinGen.WriteByte(StrToInt(RightStr(ADfaTokens[1].TokenVal, Length(ADfaTokens[1].TokenVal) - 1)));
  self.FBinGen.WriteHexStrBEtoLE(ADfaTokens[2].TokenVal);

  WriteLn('ARGI. 90 ' + RightStr(ADfaTokens[1].TokenVal, Length(ADfaTokens[1].TokenVal) - 1) + ' ' + ADfaTokens[2].TokenVal);
end;

procedure TLfnwParseGen.VM_OpCALL_A(ADfaTokens : TDFATokenArray);
begin
  WriteLn('CALL. ');

  self.FBinGen.WriteByte(100);
  if ADfaTokens[1].TokenId = Integer(ELfnwLexLabel) then
  begin
      // Add a defer with this address
      self.AddLabelDefer(ADfaTokens[1].TokenVal, self.FBinGen.BytePos + 1);

      // Write in 32bit 0
      self.FBinGen.WriteHexStrBEtoLE('00');
  end;

  // Write the number of arguments
  self.FBinGen.WriteHexStrBEtoLE(ADfaTokens[2].TokenVal);

end;

procedure TLfnwParseGen.VM_OpRET(ADfaTokens : TDFATokenArray);
begin
  self.FBinGen.WriteByte(101);
  WriteLn('RET.');
end;

procedure TLfnwParseGen.VM_OpRET_I(ADfaTokens : TDFATokenArray);
begin
  self.FBinGen.WriteByte(102);
  self.FBinGen.WriteHexStrBEtoLE(ADfaTokens[1].TokenVal);

  WriteLn('RET. ' + ADfaTokens[1].TokenVal);
end;

(* Handle PRINT *)
procedure TLfnwParseGen.VM_OpPRINTHOI(ADfaTokens : TDFATokenArray);
begin
  WriteLn('PRINTHOI.');
  self.FBinGen.WriteByte(4);
  self.FBinGen.WriteHexStrBEtoLE(ADfaTokens[1].TokenVal);
end;

procedure TLfnwParseGen.VM_OpPRINTHOC(ADfaTokens : TDFATokenArray);
begin
  WriteLn('PRINTHOC.');
  self.FBinGen.WriteByte(5);
  self.FBinGen.WriteHexStrBEtoLE(ADfaTokens[1].TokenVal);
end;


procedure TLfnwParseGen.Run(ATokens : TLfnwLexTokenArray);
var numTokens : Integer = 0;
    lastToken, curToken : PLfnwLexToken;
    reprocessToken : Boolean;
    tmpDFAToken : TDFAToken;
    i : LongInt;
begin
  curToken := @ATokens[0];
  lastToken:= curToken + Length(ATokens);

  while curToken < lastToken do
  begin

    reprocessToken := False;

    tmpDFAToken.TokenCharStart := 0;
    tmpDFAToken.TokenId := Integer(curToken^.TokenType);
    tmpDFAToken.TokenName := Copy(curToken^.Name, 1, Length(curToken^.Name));
    tmpDFAToken.TokenVal := Copy(curToken^.LexValue, 1, Length(curToken^.LexValue));

    (* Pass char into dfa state *)
    if not self.FDfa.nextToken(@tmpDFAToken, reprocessToken) then
    begin
        WriteLn('Error: no debugging info yet.');
    end;

    if not reprocessToken then
        Inc(curToken);

  end;

  // TODO: Loop all TLfnwLabelDefer
  //       Replace 0 with actual address of label
  for i := 0 to Length(self.FLabelDefers) - 1 do
  begin
    self.FBinGen.WriteBytePos(self.GetLabelAddr(self.FLabelDefers[i].LabelName), self.FLabelDefers[i].SourcePos);
  end;

  self.FBinGen.SaveFile('test.bin');

end;

constructor TLfnwParseGen.Create();
var
    StartState, CommentState,
    LabelDefState,
    OpStartState,
    OpHALTState,

    OpMOVStartState, OpMOVRState, OpMOVRIlState,
    OpMOVHState, OpMOVHState2, OpMOVHIlState,
    OpMOVHHState, OpMOVHHBState, OpMOVHHBxState,

    OpPUSHIStartState, OpPUSHIlState, OpPUSHRIState,
    OpPOPIStartState, OpPOPRIState,

    OpADDIStartState, OpADDRIState, OpADDRRIState,

    OpARGStartState, OpARGRState, OpARGRIState, OpARGRIEndState,

    OpCALLState, OpCALLLabelState, OpCallArgsState,

    OpRETStartState, OpRETIState,

    OpPRINTIStartState, OpPRINTIHState, OpPRINTHOIState,
    OpPRINTCStartState, OpPRINTCHState, OpPRINTHOCState : TDFAState;

begin
  SetLength(self.FLabels, 0);
  self.FBinGen := TBinGen.Create();
  self.FDfa := TLexDFA.Create();


  (* Setup the DFA to Parse the LexTokens *)

  StartState := TDFAState.Create('START', 'START', @self.VM_OpNone);
  CommentState := TDFAState.Create('COMMENT', 'COMMENT', @self.VM_OpNone);
  LabelDefState := TDFAState.Create('LABELDEF', 'LABELDEF', @self.VM_LabelDef);
  OpStartState := TDFAState.Create('OPSTART', 'OPSTART', @self.VM_OpNone);
  OpHALTState := TDFAState.Create('HALT', 'HALT', @self.VM_OpHALT);
  OpMOVStartState := TDFAState.Create('MOVStart', 'MOVStart', @self.VM_OpNone);
  OpMOVRState := TDFAState.Create('MOVR', 'MOVR', @self.VM_OpNone);
  OpMOVRIlState := TDFAState.Create('MOVRIl', 'MOVRIl', @self.VM_OpMOVRIl);
  OpMOVHState := TDFAState.Create('MOVH', 'MOVH', @self.VM_OpNone);
  OpMOVHState2 := TDFAState.Create('MOVH2', 'MOVH2', @self.VM_OpNone);
  OpMOVHIlState := TDFAState.Create('MOVHIl', 'MOVHIl', @self.VM_OpMOVHIl);
  OpMOVHHState := TDFAState.Create('MOVHH', 'MOVHH', @self.VM_OpNone);
  OpMOVHHBState := TDFAState.Create('MOVHHB', 'MOVHHB', @self.VM_OpNone);
  OpMOVHHBxState := TDFAState.Create('MOVHHBx', 'MOVHHBx', @self.VM_OpMOVHHBx);

  OpPUSHIStartState := TDFAState.Create('PUSHI', 'PUSHI', @self.VM_OpPUSHI);
  OpPUSHIlState := TDFAState.Create('PUSHIl', 'PUSHIl', @self.VM_OpPUSHI);
  OpPUSHRIState := TDFAState.Create('PUSHRI', 'PUSHRI', @self.VM_OpPUSHRI);

  OpPOPIStartState := TDFAState.Create('POPI', 'POPI', @self.VM_OpNone);
  OpPOPRIState := TDFAState.Create('POPRI', 'POPRI', @self.VM_OpPOPRI);

  OpADDIStartState := TDFAState.Create('ADDI', 'ADDI', @self.VM_OpNone);
  OpADDRIState := TDFAState.Create('ADDRI', 'ADDRI', @self.VM_OpNone);
  OpADDRRIState := TDFAState.Create('ADDRRI', 'ADDRRI', @self.VM_OpADDRRI);

  OpARGStartState := TDFAState.Create('ARGRI', 'ARGRI', @self.VM_OpARGRI);
  OpARGRState := TDFAState.Create('ARGRI', 'ARGRI', @self.VM_OpARGRI);
  OpARGRIState:= TDFAState.Create('ARGRI', 'ARGRI', @self.VM_OpARGRI);
  OpARGRIEndState:= TDFAState.Create('ARGRI', 'ARGRI', @self.VM_OpARGRI);

  OpCALLState := TDFAState.Create('CALL', 'CALL', @self.VM_OpCALL_A);
  OpCALLLabelState := TDFAState.Create('CALL', 'CALL', @self.VM_OpCALL_A);
  OpCALLArgsState := TDFAState.Create('CALL', 'CALL', @self.VM_OpCALL_A);

  OpRETStartState := TDFAState.Create('RET', 'RET', @self.VM_OpRET);
  OpRETIState := TDFAState.Create('RETI', 'RETI', @self.VM_OpRET_I);


  OpPRINTIStartState := TDFAState.Create('PRINTIStart', 'PRINTIStart', @self.VM_OpNone);
  OpPRINTIHState := TDFAState.Create('PRINTIH', 'PRINTIH', @self.VM_OpNone);
  OpPRINTHOIState := TDFAState.Create('PRINTHOI', 'PRINTHOI', @self.VM_OpPRINTHOI);
  OpPRINTCStartState := TDFAState.Create('PRINTCStart', 'PRINTCStart', @self.VM_OpNone);
  OpPRINTCHState := TDFAState.Create('PRINTCH', 'PRINTCH', @self.VM_OpNone);
  OpPRINTHOCState := TDFAState.Create('PRINTHOC', 'PRINTHOC', @self.VM_OpPRINTHOC);

  OpCMPIStartState := TDFAState.Create('CMPI', 'CMPI', @self.VM_OpNone);
  OpCMPRIState := TDFAState.Create('CMPRI', 'CMPRI', @self.VM_OpNone); // TODO: Make this a Stack Reg Compare
  OpCMPRRIState := TDFAState.Create('CMPRRI', 'CMPRRI', @self.VM_OpCMPRRI);


  self.FDfa.AddState(StartState);
  self.FDfa.AddState(CommentState);
  self.FDfa.AddState(LabelDefState);
  self.FDfa.AddState(OpStartState);
  self.FDfa.AddState(OpHALTState);
  self.FDfa.AddState(OpMOVStartState);
  self.FDfa.AddState(OpMOVRState);
  self.FDfa.AddState(OpMOVRIlState);
  self.FDfa.AddState(OpMOVHState);
  self.FDfa.AddState(OpMOVHState2);
  self.FDfa.AddState(OpMOVHIlState);
  self.FDfa.AddState(OpMOVHHState);
  self.FDfa.AddState(OpMOVHHBState);
  self.FDfa.AddState(OpMOVHHBxState);

  self.FDfa.AddState(OpPUSHIStartState);
  self.FDfa.AddState(OpPUSHIlState);
  self.FDfa.AddState(OpPUSHRIState);

  self.FDfa.AddState(OpPOPIStartState);
  self.FDfa.AddState(OpPOPRIState);

  self.FDfa.AddState(OpADDIStartState);
  self.FDfa.AddState(OpADDRIState);
  self.FDfa.AddState(OpADDRRIState);


  self.FDfa.AddState(OpARGStartState);
  self.FDfa.AddState(OpARGRState);
  self.FDfa.AddState(OpARGRIState);
  self.FDfa.AddState(OpARGRIEndState);

  self.FDfa.AddState(OpPRINTIStartState);
  self.FDfa.AddState(OpPRINTIHState);
  self.FDfa.AddState(OpPRINTHOIState);
  self.FDfa.AddState(OpPRINTCStartState);
  self.FDfa.AddState(OpPRINTCHState);
  self.FDfa.AddState(OpPRINTHOCState);

  self.FDfa.AddState(OpCALLState);
  self.FDfa.AddState(OpCALLLabelState);
  self.FDfa.AddState(OpCALLArgsState);

  self.FDfa.AddState(OpRETStartState);
  self.FDfa.AddState(OpRETIState);

  (* All OP Codes *)
  StartState.AddDelta(TDFADelta.Create(TDFAComp_TypeIs.Create(Integer(ELfnwLexOp)), OpStartState, False, True));

  (* All Comments *)
  StartState.AddDelta(TDFADelta.Create(TDFAComp_TypeIs.Create(Integer(ELfnwLexComment)), CommentState));

  (* Label Def *)
  StartState.AddDelta(TDFADelta.Create(TDFAComp_TypeIs.Create(Integer(ELfnwLexLabelDef)), LabelDefState));

  (* Branch each OP Code By Name *)
  OpStartState.AddDelta(TDFADelta.Create(TDFAComp_ValIs.Create('HALT'), OpHALTState));
  OpStartState.AddDelta(TDFADelta.Create(TDFAComp_ValIs.Create('MOV'), OpMOVStartState));
  OpStartState.AddDelta(TDFADelta.Create(TDFAComp_ValIs.Create('ARGI'), OpARGStartState));
  OpStartState.AddDelta(TDFADelta.Create(TDFAComp_ValIs.Create('PRINTI'), OpPRINTIStartState));
  OpStartState.AddDelta(TDFADelta.Create(TDFAComp_ValIs.Create('PRINTC'), OpPRINTCStartState));
  OpStartState.AddDelta(TDFADelta.Create(TDFAComp_ValIs.Create('CALL'), OpCALLState));
  OpStartState.AddDelta(TDFADelta.Create(TDFAComp_ValIs.Create('PUSHI'), OpPUSHIStartState));
  OpStartState.AddDelta(TDFADelta.Create(TDFAComp_ValIs.Create('POPI'), OpPOPIStartState));
  OpStartState.AddDelta(TDFADelta.Create(TDFAComp_ValIs.Create('ADDI'), OpADDIStartState));
  OpStartState.AddDelta(TDFADelta.Create(TDFAComp_ValIs.Create('RET'), OpRETStartState));


  (* MOVH* OP Codes *)
  OpMOVStartState.AddDelta(TDFADelta.Create(TDFAComp_TypeIs.Create(Integer(ELfnwLexAddr)), OpMOVHState, False));
  OpMOVHState.AddDelta(TDFADelta.Create(TDFAComp_TypeIs.Create(Integer(ELfnwLexHexLit)), OpMOVHState2));

  (* MOVHIl Delta *)
  OpMOVHState2.AddDelta(TDFADelta.Create(TDFAComp_TypeIs.Create(Integer(ELfnwLexHexLit)), OpMOVHIlState));

  (* MOVHHBx Op - extends the MOVH *)
  OpMOVHState2.AddDelta(TDFADelta.Create(TDFAComp_TypeIs.Create(Integer(ELfnwLexAddr)), OpMOVHHState, False));
  OpMOVHHState.AddDelta(TDFADelta.Create(TDFAComp_TypeIs.Create(Integer(ELfnwLexHexLit)), OpMOVHHBState));
  OpMOVHHBState.AddDelta(TDFADelta.Create(TDFAComp_TypeIs.Create(Integer(ELfnwLexHexLit)), OpMOVHHBxState));

  (* PUSHI* Delta *)
  OpPUSHIStartState.AddDelta(TDFADelta.Create(TDFAComp_TypeIs.Create(Integer(ELfnwLexHexLit)), OpPUSHIlState));
  OpPUSHIStartState.AddDelta(TDFADelta.Create(TDFAComp_TypeIs.Create(Integer(ELfnwLexReg)), OpPUSHRIState));

  OpPOPIStartState.AddDelta(TDFADelta.Create(TDFAComp_TypeIs.Create(Integer(ELfnwLexReg)), OpPOPRIState));

  (* ADDI* Delta *)
  OpADDIStartState.AddDelta(TDFADelta.Create(TDFAComp_TypeIs.Create(Integer(ELfnwLexReg)), OpADDRIState));
  OpADDRIState.AddDelta(TDFADelta.Create(TDFAComp_TypeIs.Create(Integer(ELfnwLexReg)), OpADDRRIState));

  (* ARGRI Delta *)
  OpARGStartState.AddDelta(TDFADelta.Create(TDFAComp_TypeIs.Create(Integer(ELfnwLexReg)), OpARGRState));
  OpARGRState.AddDelta(TDFADelta.Create(TDFAComp_TypeIs.Create(Integer(ELfnwLexHexLit)), OpARGRIState));
  //OpARGRState.AddDelta(TDFADelta.Create(TDFAComp_TypeIs.Create(Integer(ELfnwLexHexLit)), OpARGRIState));

  (* PRINTHOI *)
  OpPRINTIStartState.AddDelta(TDFADelta.Create(TDFAComp_TypeIs.Create(Integer(ELfnwLexAddr)), OpPRINTIHState, False));
  OpPRINTIHState.AddDelta(TDFADelta.Create(TDFAComp_TypeIs.Create(Integer(ELfnwLexHexLit)), OpPRINTHOIState));

  (* PRINTHOC *)
  OpPRINTCStartState.AddDelta(TDFADelta.Create(TDFAComp_TypeIs.Create(Integer(ELfnwLexAddr)), OpPRINTCHState, False));
  OpPRINTCHState.AddDelta(TDFADelta.Create(TDFAComp_TypeIs.Create(Integer(ELfnwLexHexLit)), OpPRINTHOCState));

  (* CALL *)
  OpCALLState.AddDelta(TDFADelta.Create(TDFAComp_TypeIs.Create(Integer(ELfnwLexLabel)), OpCALLLabelState));
  OpCALLLabelState.AddDelta(TDFADelta.Create(TDFAComp_TypeIs.Create(Integer(ELfnwLexHexLit)), OpCallArgsState));

  (* RET *)
  opRETStartState.AddDelta(TDFADelta.Create(TDFAComp_TypeIs.Create(Integer(ELfnwLexHexLit)), OpRETIState));

end;

destructor TLfnwParseGen.Destroy();
begin
  FreeAndNil(self.FDfa);
  FreeAndNil(self.FBinGen);

  inherited Destroy();
end;

end.

