unit lexdfa;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils;

type
    PDFAToken = ^TDFAToken;
    TDFAToken = record
        TokenId : Integer;
        TokenVal : AnsiString;
        TokenName: AnsiString;
        TokenCharStart : Integer;
        TokenLine : Integer;
    end;

    TDFATokenArray = Array of TDFAToken;

    TDFATokenArrayHandler = procedure(ADfaTokens : TDFATokenArray) of object;

    TDFAComparator = class(TObject)
        public
            procedure Free(); Virtual; Abstract;
            function Compare(AInput : PDFAToken) : Boolean; Virtual; Abstract;
    end;

    TDFAComp_TypeIsIn = class(TDFAComparator)
        private
            FTypeList : Array of Byte;
        public
            constructor Create(ATypeList : Array of Byte);
            destructor Destroy(); Override;

            procedure Free(); Override;
            function Compare(AInput : PDFAToken) : Boolean; Override;
    end;

    TDFAComp_ValIsIn = class(TDFAComparator)
        private
            FValList : Array of AnsiString;
        public
            constructor Create(AValList : Array of AnsiString);
            destructor Destroy(); Override;

            procedure Free(); Override;
            function Compare(AInput : PDFAToken) : Boolean; Override;
    end;

    TDFAComp_TypeIsNotIn = class(TDFAComparator)
        private
            FTypeList : Array of Byte;
        public
            constructor Create(ATypeList : Array of Byte);
            destructor Destroy(); Override;

            procedure Free(); Override;
            function Compare(AInput : PDFAToken) : Boolean; Override;
    end;

    TDFAComp_ValIsNotIn = class(TDFAComparator)
        private
            FValList : Array of AnsiString;
        public
            constructor Create(AValList : Array of AnsiString);
            destructor Destroy(); Override;

            procedure Free(); Override;
            function Compare(AInput : PDFAToken) : Boolean; Override;
    end;

    TDFAComp_TypeIs = class(TDFAComparator)
        private
            FType : Byte;
        public
            constructor Create(AType : Byte);
            destructor Destroy(); Override;

            procedure Free(); Override;
            function Compare(AInput : PDFAToken) : Boolean; Override;
    end;

    TDFAComp_ValIs = class(TDFAComparator)
        private
            FVal : AnsiString;
        public
            constructor Create(AVal : AnsiString);
            destructor Destroy(); Override;

            procedure Free(); Override;
            function Compare(AInput : PDFAToken) : Boolean; Override;
    end;

    TDFAComp_TypeIsNot = class(TDFAComparator)
        private
            FType : Byte;
        public
            constructor Create(AType : Byte);
            destructor Destroy(); Override;

            procedure Free(); Override;
            function Compare(AInput : PDFAToken) : Boolean; Override;
    end;

    TDFAComp_ValIsNot = class(TDFAComparator)
        private
            FVal : AnsiString;
        public
            constructor Create(AVal : AnsiString);
            destructor Destroy(); Override;

            procedure Free(); Override;
            function Compare(AInput : PDFAToken) : Boolean; Override;
    end;

    TDFAComp_And = class(TDFAComparator)
        private
            FComparators : Array of TDFAComparator;
        public
            constructor Create(AComps : Array of TDFAComparator);
            destructor Destroy(); Override;

            procedure Free(); Override;
            function Compare(AInput : PDFAToken) : Boolean; Override;
    end;

    TDFAComp_Or = class(TDFAComparator)
        private
            FComparators : Array of TDFAComparator;
        public
            constructor Create(AComps : Array of TDFAComparator);
            destructor Destroy(); Override;

            procedure Free(); Override;
            function Compare(AInput : PDFAToken) : Boolean; Override;
    end;

    TDFAState = class;

    TDFADelta = class
        private
            FComparator : TDFAComparator;
            FDestination : TDFAState;
            FAddToBuffer : Boolean;
            FReprocess : Boolean;

        public
            constructor Create(AComparator : TDFAComparator; ADestination : TDFAState; AAddToBuffer : Boolean = True; AReprocess : Boolean = False);
            destructor Destroy(); Override;
    end;

    TLexDFA = class;

    TDFAState = class(TObject)
        private
            FDeltas : Array of TDFADelta;
            FName : AnsiString;
            FIdent : AnsiString;
            FOpCode : Byte;
            FHandler : TDFATokenArrayHandler;
            FDfa : TLexDFA;

            function GetIsLeaf() : Boolean;
        public
            function ProcessToken(tok : PDFAToken;  var reprocess : Boolean) : Boolean;
            procedure AddDelta(delta : TDFADelta);
            constructor Create(AName : AnsiString; AIdent : AnsiString; AHandler : TDFATokenArrayHandler);
            destructor Destroy(); Override;

            property IsLeaf : Boolean read GetIsLeaf;


    end;

    TLexDFA = class
        private
            FStates : Array of TDFAState;
            FCurState : TDFAState;
            FStartState : TDFAState;
            FBuffer : TDFATokenArray;
        protected

        public
            constructor Create();
            destructor Destroy(); Override;

            procedure AddState(state : TDFAState);
            function nextToken(tok : PDFAToken; var reprocess : Boolean) : Boolean;
    end;

implementation

constructor TDFAComp_TypeIsIn.Create(ATypeList : Array of Byte);
var
    el : Byte;
    i : Integer;
begin
    inherited Create();
    SetLength(Self.FTypeList, Length(ATypeList));
    i := 0;

    for el in ATypeList do
    begin
       Self.FTypeList[i] := el;
       Inc(i);
    end;

end;

destructor TDFAComp_TypeIsIn.Destroy();
begin
    inherited Destroy();
end;

procedure TDFAComp_TypeIsIn.Free();
begin
    SetLength(Self.FTypeList, 0);
    Self.FTypeList := Nil;
end;

function TDFAComp_TypeIsIn.Compare(AInput : PDFAToken) : Boolean;
var
    el : Byte;
begin
    Result := False;
    for el in Self.FTypeList do
    begin
        if el = AInput^.TokenId then
        begin
            Result := True;
            break;
        end;
    end;
end;

constructor TDFAComp_ValIsIn.Create(AValList : Array of AnsiString);
var
    el : AnsiString;
    i : Integer;
begin
    inherited Create();
    SetLength(Self.FValList, Length(AValList));
    i := 0;

    for el in AValList do
    begin
       Self.FValList[i] := Copy(el, 1, Length(el));
       Inc(i);
    end;

end;

destructor TDFAComp_ValIsIn.Destroy();
begin
    inherited Destroy();
end;

procedure TDFAComp_ValIsIn.Free();
begin
    SetLength(Self.FValList, 0);
    Self.FValList := Nil;
end;

function TDFAComp_ValIsIn.Compare(AInput : PDFAToken) : Boolean;
var
    el : AnsiString;
begin
    Result := False;
    for el in Self.FValList do
    begin
        if el = AInput^.TokenVal then
        begin
            Result := True;
            break;
        end;
    end;
end;

constructor TDFAComp_TypeIsNotIn.Create(ATypeList : Array of Byte);
var
    el : Byte;
    i : Integer;
begin
    inherited Create();
    SetLength(Self.FTypeList, Length(ATypeList));
    i := 0;

    for el in ATypeList do
    begin
       Self.FTypeList[i] := el;
       Inc(i);
    end;

end;

destructor TDFAComp_TypeIsNotIn.Destroy();
begin
    inherited Destroy();
end;

procedure TDFAComp_TypeIsNotIn.Free();
begin
    SetLength(Self.FTypeList, 0);
    Self.FTypeList := Nil;
end;

function TDFAComp_TypeIsNotIn.Compare(AInput : PDFAToken) : Boolean;
var
    el : Byte;
begin
    Result := True;
    for el in Self.FTypeList do
    begin
        if el = AInput^.TokenId then
        begin
            Result := False;
            break;
        end;
    end;
end;

constructor TDFAComp_ValIsNotIn.Create(AValList : Array of AnsiString);
var
    el : AnsiString;
    i : Integer;
begin
    inherited Create();
    SetLength(Self.FValList, Length(AValList));
    i := 0;

    for el in AValList do
    begin
       Self.FValList[i] := Copy(el, 1, Length(el));
       Inc(i);
    end;

end;

destructor TDFAComp_ValIsNotIn.Destroy();
begin
    inherited Destroy();
end;

procedure TDFAComp_ValIsNotIn.Free();
begin
    SetLength(Self.FValList, 0);
    Self.FValList := Nil;
end;

function TDFAComp_ValIsNotIn.Compare(AInput : PDFAToken) : Boolean;
var
    el : AnsiString;
begin
    Result := True;
    for el in Self.FValList do
    begin
        if el = AInput^.TokenVal then
        begin
            Result := False;
            break;
        end;
    end;
end;

constructor TDFAComp_TypeIs.Create(AType : Byte);
begin
    inherited Create();

    self.FType := AType;
end;

destructor TDFAComp_TypeIs.Destroy();
begin
    inherited Destroy();
end;

procedure TDFAComp_TypeIs.Free();
begin
    Self.FType := 0;
end;

function TDFAComp_TypeIs.Compare(AInput : PDFAToken) : Boolean;
var
    el : Byte;
begin
    Result := True;

    if FType <> AInput^.TokenId then
        Result := False;
end;

constructor TDFAComp_ValIs.Create(AVal : AnsiString);
begin
    inherited Create();

    self.FVal := AVal;
end;

destructor TDFAComp_ValIs.Destroy();
begin
    inherited Destroy();
end;

procedure TDFAComp_ValIs.Free();
begin
    Self.FVal := '';
end;

function TDFAComp_ValIs.Compare(AInput : PDFAToken) : Boolean;
var
    el : AnsiString;
begin
    Result := True;

    if AnsiCompareStr(AInput^.TokenVal, FVal) <> 0 then
        Result := False;
end;


constructor TDFAComp_TypeIsNot.Create(AType : Byte);
begin
    inherited Create();

    self.FType := AType;
end;

destructor TDFAComp_TypeIsNot.Destroy();
begin
    inherited Destroy();
end;

procedure TDFAComp_TypeIsNot.Free();
begin
    Self.FType := 0;
end;

function TDFAComp_TypeIsNot.Compare(AInput : PDFAToken) : Boolean;
var
    el : Byte;
begin
    Result := True;

    if AInput^.TokenId = self.FType then
        Result := False;
end;

constructor TDFAComp_ValIsNot.Create(AVal : AnsiString);
begin
    inherited Create();

    self.FVal := AVal;
end;

destructor TDFAComp_ValIsNot.Destroy();
begin
    inherited Destroy();
end;

procedure TDFAComp_ValIsNot.Free();
begin
    Self.FVal := '';
end;

function TDFAComp_ValIsNot.Compare(AInput : PDFAToken) : Boolean;
var
    el : AnsiString;
begin
    Result := True;

    if AnsiCompareStr(AInput^.TokenVal, FVal) = 0 then
        Result := False;
end;



constructor TDFAComp_And.Create(AComps : Array of TDFAComparator);
var i : Integer;
    comp : TDFAComparator;
begin
    inherited Create();

    SetLength(Self.FComparators, Length(AComps));
    i := 0;

    for comp in AComps do
    begin
        Self.FComparators[i] := comp;
        Inc(i);
    end;

end;

destructor TDFAComp_And.Destroy();
begin

    inherited Destroy();
end;

procedure TDFAComp_And.Free();
begin

end;

function TDFAComp_And.Compare(AInput : PDFAToken) : Boolean;
var comp : TDFAComparator;
begin
    Result := True;
    for comp in self.FComparators do
    begin
        if not comp.Compare(AInput) then
        begin
            Result := False;
            break;
        end;
    end;
end;

constructor TDFAComp_Or.Create(AComps : Array of TDFAComparator);
var i : Integer;
begin
    inherited Create();

    SetLength(Self.FComparators, Length(AComps));

    for i := 0 to Length(AComps) - 1 do
    begin
        self.FComparators[i] := AComps[i];
    end;
end;

destructor TDFAComp_Or.Destroy();
begin
    WriteLn('Destroy Or');
    inherited Destroy();
end;

procedure TDFAComp_Or.Free();
var comp : TDFAComparator;
    i : Integer;
begin
    WriteLn('Freeing Or');
    for i := 0 to Length(Self.FComparators) - 1 do
    begin
        comp := Self.FComparators[i];
        //Self.FComparators[i] := nil;
        Self.FComparators[i].Free();
        FreeAndNil(Self.FComparators[i]);
        //comp := nil;
    end;

    SetLength(Self.FComparators, 0);
    Self.FComparators := nil;
end;

function TDFAComp_Or.Compare(AInput : PDFAToken) : Boolean;
var comp : TDFAComparator;
begin
    Result := False;
    for comp in self.FComparators do
    begin
        if comp.Compare(AInput) then
        begin
            Result := True;
            break;
        end;
    end;
end;

constructor TDFADelta.Create(AComparator : TDFAComparator; ADestination : TDFAState; AAddToBuffer : Boolean = True; AReprocess : Boolean = False);
begin
     self.FComparator := AComparator;
     self.FDestination := ADestination;
     self.FAddToBuffer := AAddToBuffer;
     self.FReprocess := AReprocess;
end;

destructor TDFADelta.Destroy();
begin
    FreeAndNil(self.FComparator);

    inherited Destroy();
end;

constructor TDFAState.Create(AName : AnsiString; AIdent : AnsiString; AHandler : TDFATokenArrayHandler);
begin
     FName := AName;
     FIdent := AIdent;
     FHandler := AHandler;
end;

destructor TDFAState.Destroy();
var curDelta : TDFADelta;
    i : Integer;
begin
    for i := 0 to Length(self.FDeltas) - 1 do
    begin
        curDelta := self.FDeltas[i];
        FreeAndNil(curDelta);
    end;

    inherited Destroy();
end;

function TDFAState.GetIsLeaf() : Boolean;
begin
    Result := False;

    if Length(self.FDeltas) = 0 then
       Result := True;
end;

function TDFAState.ProcessToken(tok : PDFAToken; var reprocess : Boolean) : Boolean;
var delta : TDFADelta;
begin
    Result := False;

    for delta in self.FDeltas do
    begin
      if delta.FComparator.Compare(tok) then
      begin
          Result := True;

          (* We need to handle the transition *)
          if delta.FReprocess then
          begin
              (* Handle reversing the buffer, somehow *)
              reprocess := True;
          end;

          if delta.FAddToBuffer then
          begin
              (* Add the current character to the DFA Buffer for current token *)
              SetLength(self.FDfa.FBuffer, Length(self.FDfa.FBuffer) + 1);
              self.FDfa.FBuffer[Length(self.FDfa.FBuffer) - 1] := tok^;
              //self.FDfa.FBuffer := self.FDfa.FBuffer + c;
          end;

          (* TEMP: Print Token Value *)
          //WriteLn(tok^.TokenName, ' ', tok^.TokenVal);

          (* Move to Dest state *)
          self.FDfa.FCurState := delta.FDestination;


          Exit;
      end;
    end;
end;

procedure TDFAState.AddDelta(delta : TDFADelta);
var numDeltas : Integer;
begin
    numDeltas := Length(self.FDeltas);
    SetLength(self.FDeltas, numDeltas + 1);
    self.FDeltas[numDeltas] := delta;
end;

constructor TLexDFA.Create();
begin

end;

destructor TLexDFA.Destroy();
var state : TDFAState;
    i : Integer;
begin
    (* Free up all States *)
    for i := 0 to Length(self.FStates) - 1 do
    begin
        state := self.FStates[i];
        (* Call free on state, then freeandnil *)
        //state.Free();
        FreeAndNil(State);

    end;


    inherited Destroy();
end;

procedure TLexDFA.AddState(state : TDFAState);
var numStates : Integer;
begin
    numStates := Length(self.FStates);
    SetLength(self.FStates, numStates + 1);
    self.FStates[numStates] := state;

    if not Assigned(self.FCurState) then
       self.FCurState := state;

    if not Assigned(self.FStartState) then
       self.FStartState := state;

    // Put a reference to this DFA in the state (used for changing current state)
    state.FDfa := self;
end;

function TLexDFA.nextToken(tok : PDFAToken; var reprocess : Boolean) : Boolean;
begin

    (* Test each Delta in the current state, if it can process this char *)
    Result := FCurState.ProcessToken(tok, reprocess);

    if Result = True then
    begin
        (* Test if state is a leaf *)
        if self.FCurState.IsLeaf then
        begin
            (* This is a dead end, it is time to Send off the Token Buffer, Clear, and go to StartState *)
            self.FCurState.FHandler(self.FBuffer);

            SetLength(self.FBuffer, 0);

            (* Go Back to StartState *)
            self.FCurState := self.FStartState;
        end;
    end;

    (* return if the char was processed correctly *)
    //Result := True;
end;

end.
