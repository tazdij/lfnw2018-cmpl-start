unit nslex;

{$mode objfpc}{$H+}

interface

uses chardfa;

type
    ELexTokenType = (
        (* None, is used when there is no Token needed *)
        ELfnwLexNone,

        ELfnwLexOp, ELfnwLexComment, ELfnwLexLabel, ELfnwLexLabelDef,

        (* Addresses can be used differently depending on the Op *)
        ELfnwLexAddr, ELfnwLexHexLit, ELfnwLexDecLit,


        ELfnwLexReg);
    
    PLfnwLexToken = ^TLfnwLexToken;
    TLfnwLexToken = record
        TokenType : ELexTokenType;
        Name : AnsiString;
        LexValue : AnsiString;
        LineNum : Cardinal;
        CharNum : Cardinal;
    end;
    
    TLfnwLexTokenArray = Array of TLfnwLexToken;
    
    TLfnwLexer = class(TObject)
        private
            FDfa : TCharDFA;
            FCurChar, FCurLine: Cardinal;
            FTokenList : TLfnwLexTokenArray;
        public
            constructor Create();
            destructor Destroy(); Override;

            procedure HandleDFAToken(token : PDFAToken);
            
            function Lex(s : AnsiString) : TLfnwLexTokenArray;
    end;

implementation

uses sysutils, lazutf8;

var
    // CharList : Array[0..1] of AnsiString = ('a', '四');
    DigitCL : Array[0..9] of AnsiString = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9');
    HexCL : Array[0..15] of AnsiString = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
    LowerAlphaCL : Array[0..25] of AnsiString = (
        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 
        'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 
        'u', 'v', 'w', 'x', 'y', 'z');
        
    UpperAlphaCL : Array[0..25] of AnsiString = (
        'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 
        'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 
        'U', 'V', 'W', 'X', 'Y', 'Z');

    WhitespaceCL : Array[0..3] of AnsiString = (#13, #10, #7, #32);


constructor TLfnwLexer.Create();
var
    StartState, WhitespaceState,
    RegisterState, RegisterNumberState, RegisterEndState,
    OpState, OpEndState,
    LabelState, LabelEndState, LabelDefState,
    CommentState, CommentEndState,
    LiteralHexState, LiteralHexDigitState, LiteralHexEndState,
    LiteralDecState, LiteralDecEndState,
    AddressState: TDFAState;

begin
    self.FCurLine := 1;
    self.FCurChar := 0;
    SetLength(self.FTokenList, 0);

    FDfa := TCharDFA.Create();

    (* Assign the LexToken Generator *)
    FDfa.SetTokenHandler(@Self.HandleDFAToken);
    
    (* configure DFA to Lex LnfwSource *)
    StartState := TDFAState.Create('START', 'START', Integer(ELfnwLexNone));
    WhitespaceState := TDFAState.Create('WHITESPACE', 'WS', Integer(ELfnwLexNone));

    CommentState := TDFAState.Create('COMMENT', 'CMNT', Integer(ELfnwLexComment));
    CommentEndState := TDFAState.Create('COMMENT', 'CMNT', Integer(ELfnwLexComment));

    RegisterState := TDFAState.Create('REGISTER', 'REGISTER', Integer(ELfnwLexReg));
    RegisterNumberState := TDFAState.Create('REGISTER', 'REGISTER', Integer(ELfnwLexReg));
    RegisterEndState := TDFAState.Create('REGISTER', 'REGISTER', Integer(ELfnwLexReg));

    OpState := TDFAState.Create('OP', 'OP', Integer(ELfnwLexOp));
    OpEndState := TDFAState.Create('OP', 'OP', Integer(ELfnwLexOp));
    LabelState := TDFAState.Create('LABEL', 'LABEL', Integer(ELfnwLexLabel));
    LabelEndState := TDFAState.Create('LABEL', 'LABEL', Integer(ELfnwLexLabel));
    LabelDefState := TDFAState.Create('LABELDEF', 'LABELDEF', Integer(ELfnwLexLabelDef));

    LiteralHexState := TDFAState.Create('HEXLIT', 'HEXLIT', Integer(ELfnwLexHexLit));
    LiteralHexDigitState := TDFAState.Create('HEXLIT', 'HEXLIT', Integer(ELfnwLexHexLit));
    LiteralHexEndState := TDFAState.Create('HEXLIT', 'HEXLIT', Integer(ELfnwLexHexLit));

    LiteralDecState := TDFAState.Create('DECLIT', 'DECLIT', Integer(ELfnwLexDecLit));
    LiteralDecEndState := TDFAState.Create('DECLIT', 'DECLIT', Integer(ELfnwLexDecLit));

    AddressState := TDFAState.Create('ADDRESS', 'ADDRESS', Integer(ELfnwLexAddr));


    FDfa.addState(StartState); (* Must add the First "Start" State, before all others *)
    FDfa.addState(WhitespaceState);
    FDfa.addState(CommentState);
    Fdfa.AddState(CommentEndState);

    FDfa.addState(RegisterState);
    FDfa.addState(RegisterNumberState);
    FDfa.addState(RegisterEndState);

    FDfa.addState(OpState);
    FDfa.AddState(OpEndState);

    FDfa.addState(LabelState);
    FDfa.addState(LabelEndState);
    FDfa.AddState(LabelDefState);

    FDfa.AddState(LiteralHexState);
    FDfa.AddState(LiteralHexDigitState);
    FDfa.AddState(LiteralHexEndState);

    FDfa.AddState(LiteralDecState);
    FDfa.AddState(LiteralDecEndState);

    FDfa.AddState(AddressState);


    (* Loop whitespace back to start, we don't care about it *)
    StartState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(WhitespaceCL), StartState, False));

    (* Handle comments *)
    StartState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create(';'), CommentState, False));
    CommentState.AddDelta(TDFADelta.Create(TDFAComp_IsNot.Create(#10), CommentState));
    CommentState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create(#10), CommentEndState, False));

    (* Handle Register *)
    StartState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create('R'), RegisterState));
    RegisterState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(DigitCL), RegisterNumberState));
    RegisterState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(UpperAlphaCL), OpState));
    RegisterNumberState.AddDelta(TDFADelta.Create(TDFAComp_IsNotIn.Create(DigitCL), RegisterEndState, False, True));

    (* Handle Ops *)
    StartState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(UpperAlphaCL), OpState));
    OpState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(UpperAlphaCL), OpState));
    OpState.AddDelta(TDFADelta.Create(TDFAComp_IsNotIn.Create(UpperAlphaCL), OpEndState, False, True));

    (* Handle Hex Literal *)
    StartState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create('x'), LiteralHexState, False));
    LiteralHexState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(LowerAlphaCL), LabelState));
    LiteralHexState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(HexCL), LiteralHexDigitState));
    LiteralHexDigitState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(HexCL), LiteralHexDigitState));
    LiteralHexDigitState.AddDelta(TDFADelta.Create(TDFAComp_IsNotIn.Create(HexCL), LiteralHexEndState, False, True));

    (* Handle Dec Literal *)
    StartState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(DigitCL), LiteralDecState));
    LiteralDecState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(DigitCL), LiteralDecState));
    LiteralDecState.AddDelta(TDFADelta.Create(TDFAComp_IsNotIn.Create(DigitCL), LiteralDecEndState, False, True));


    (* Handle Address *)
    StartState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create('@'), AddressState));

    (* Handle Label *)
    StartState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(LowerAlphaCL), LabelState));
    LabelState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(LowerAlphaCL), LabelState));
    LabelState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create('_'), LabelState));
    LabelState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create(':'), LabelDefState, False));
    LabelState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(WhitespaceCL), LabelEndState, False));

    
end;


destructor TLfnwLexer.Destroy();
begin
    FreeAndNil(Fdfa);
    SetLength(self.FTokenList, 0);
    
    inherited Destroy();
end;

procedure TLfnwLexer.HandleDFAToken(token : PDFAToken);
var pToken : PLfnwLexToken;
    i : Integer;
begin
  WriteLn('#TOKEN: ', token^.TokenName, ' -> ', token^.TokenVal);

  i := Length(self.FTokenList);
  SetLength(self.FTokenList, i + 1);
  pToken := @self.FTokenList[i];

  pToken^.LexValue := token^.TokenVal;
  pToken^.TokenType := ELexTokenType(token^.TokenId);
  pToken^.Name := token^.TokenName;

end;

function TLfnwLexer.Lex(s : AnsiString) : TLfnwLexTokenArray;
var
    len : Integer;
    curCodePoint : AnsiString;
    curP, endP : PChar;
    reprocessCodePoint : Boolean;
begin
    curP := PChar(s);
    endP := curP + Length(s);
    
    while curP < endP do
    begin
        len := UTF8CodePointSize(CurP);
        SetLength(curCodePoint, len);
        Move(curP^, curCodePoint[1], len);
        
        if curCodePoint = #10 then
        begin
            self.FCurChar := 0;
            self.FCurLine := self.FCurLine + 1;
        end
        else if curCodePoint = #13 then
        begin
            (* Ignore cariage return *)
        end
        else
        begin
            Inc(self.FCurChar);
            //WriteLn('Line: ', curLineNum, ', Char: ', curCharNum, ', => ', curCodePoint);
        end;
        //Write(curCodePoint);

        reprocessCodePoint := False;
        
        (* Pass char into dfa state *)
        if not self.FDfa.nextChar(curCodePoint, reprocessCodePoint) then
        begin
            WriteLn('Error: no debugging info yet.');
        end;

        if not reprocessCodePoint then
            Inc(curP, len)
        else
        begin

        end;
    end;

    Result := self.FTokenList;

end;

end.
