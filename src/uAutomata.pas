unit uAutomata;

interface

uses
  System.SysUtils, System.Classes;

type
  TTransitions = array of array of TStringList;

  TAutomata = class
  private
    // automata definition
    FStates: TStringList;
    FSymbols: TStringList;
    FInitialState: string;
    FFinalStates: TStringList;
    FTransitions: TTransitions;
    FStrings: TStringList;
    // input info
    FInput: string;
    FInputSize: Integer;
    FIndexEpsilonSymbol: Integer;
    procedure CreateTransitions;
    procedure DestroyTransactions;
    function ExecuteTransition(const AState: string; const AIndexInput: Integer;
      const AStateEmpty: string; const AIndexInputEmpty: Integer): Boolean;
    function ExecuteEmptyTransition(const AState: string; const AIndexInput: Integer;
      const AStateEmpty: string; const AIndexInputEmpty: Integer): Boolean;
    function ExecuteStateTransition(const AState: string; const AIndexInput: Integer): Boolean;
    function GetTransitions(const AState, ASymbol: string): TStringList; overload;
    function GetTransitions(const AState: string; const AIndexInput: Integer): TStringList; overload;
    function IsFinalState(const AState: string): Boolean;
    function IsInputOver(const AIndexInput: Integer): Boolean;
    procedure Log(const AState: string; const AIndexInput: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load(AFileName: TFilename);
    function Execute(AInput: string): Boolean;
    // automata definition
    property States: TStringList read FStates;
    property Symbols: TStringList read FSymbols;
    property InitialState: string read FInitialState;
    property FinalStates: TStringList read FFinalStates;
    property Transitions: TTransitions read FTransitions;
    property Strings: TStringList read FStrings;
  end;

implementation

uses
  System.IniFiles, Windows;

const
  EPSILON = '"';

{ TAutomata }

constructor TAutomata.Create;
begin
  FStates := TStringList.Create;
  FSymbols := TStringList.Create;
  FInitialState := '';
  FFinalStates := TStringList.Create;
  SetLength(FTransitions, 0, 0);
  FStrings := TStringList.Create;
end;

destructor TAutomata.Destroy;
begin
  DestroyTransactions;
  FreeAndNil(FStates);
  FreeAndNil(FSymbols);
  FreeAndNil(FFinalStates);
  FreeAndNil(FStrings);
  inherited;
end;

procedure TAutomata.DestroyTransactions;
var
  IdxState, IdxSymbol: Integer;
begin
  for IdxState := 0 to FStates.Count - 1 do
    for IdxSymbol := 0 to FSymbols.Count - 1 do
      FreeAndNil(FTransitions[IdxState][IdxSymbol]);

  SetLength(FTransitions, 0, 0);
end;

procedure TAutomata.Load(AFileName: TFilename);
var
  AutomataDefinition: TIniFile;
  IdxState, IdxSymbol: Integer;
begin
  // destroy previous transitions
  DestroyTransactions;

  AutomataDefinition := TIniFile.Create(AFileName);
  try
    FStates.CommaText := AutomataDefinition.ReadString('Automata', 'States', '');
    FSymbols.CommaText := AutomataDefinition.ReadString('Automata', 'Symbols', '');
    FInitialState := AutomataDefinition.ReadString('Automata', 'InitialState', '');
    FFinalStates.CommaText := AutomataDefinition.ReadString('Automata', 'FinalStates', '');

    // the empty string (Epsilon) must be add into list of symbols to be considered in transitions
    FIndexEpsilonSymbol := FSymbols.IndexOf(EPSILON);
    if FIndexEpsilonSymbol = -1 then
      FIndexEpsilonSymbol := FSymbols.Add(EPSILON);

    CreateTransitions;
    // load transitions from NFA config file
    for IdxState := 0 to FStates.Count -1 do
      for IdxSymbol := 0 to FSymbols.Count - 1 do
        FTransitions[IdxState][IdxSymbol].CommaText := AutomataDefinition.ReadString(FStates.Strings[IdxState], FSymbols.Strings[IdxSymbol], '');
  finally
    FreeAndNil(AutomataDefinition);
  end;
end;

procedure TAutomata.CreateTransitions;
var
  IdxState, IdxSymbol: Integer;
begin
  SetLength(FTransitions, FStates.Count, FSymbols.Count);
  for IdxState := 0 to FStates.Count - 1 do
    for IdxSymbol := 0 to FSymbols.Count - 1 do
      FTransitions[IdxState][IdxSymbol] := TStringList.Create;
end;

function TAutomata.Execute(AInput: string): Boolean;
begin
  FInput := AInput;
  FInputSize := Length(AInput);

  Result := ExecuteTransition(FInitialState, 1, '', 0);
end;

function TAutomata.ExecuteTransition(const AState: string; const AIndexInput: Integer;
  const AStateEmpty: string; const AIndexInputEmpty: Integer): Boolean;

  function IsInCycle: Boolean;
  begin
    Result := (AState = AStateEmpty) and (AIndexInput = AIndexInputEmpty);
  end;

begin
  Log(AState, AIndexInput);

  if IsInputOver(AIndexInput) then
    Result := IsFinalState(AState)
  else
    Result := ExecuteStateTransition(AState, AIndexInput);

  if not Result and not IsInCycle then
  begin
    // check if start a new cycle, so use the current state and input
    if AStateEmpty = '' then
      Result := ExecuteEmptyTransition(AState, AIndexInput, AState, AIndexInput)
    else
      Result := ExecuteEmptyTransition(AState, AIndexInput, AStateEmpty, AIndexInputEmpty);
  end;
end;

function TAutomata.IsInputOver(const AIndexInput: Integer): Boolean;
begin
  Result := AIndexInput > FInputSize;
end;

function TAutomata.IsFinalState(const AState: string): Boolean;
begin
  Result := FFinalStates.IndexOf(AState) <> -1;
end;

function TAutomata.ExecuteStateTransition(const AState: string; const AIndexInput: Integer): Boolean;
var
  i: Integer;
  NextIndexInput: Integer;
  Transitions: TStringList;
begin
  Result := False;
  Transitions := GetTransitions(AState, AIndexInput);
  if not Assigned(Transitions) then
    Exit;

  NextIndexInput := AIndexInput + 1;

  for i := 0 to Transitions.Count - 1 do
    Result := Result or ExecuteTransition(Transitions.Strings[i], NextIndexInput, '', 0);
end;

function TAutomata.ExecuteEmptyTransition(const AState: string; const AIndexInput: Integer;
  const AStateEmpty: string; const AIndexInputEmpty: Integer): Boolean;
var
  i: Integer;
  Transitions: TStringList;
begin
  Result := False;
  Transitions := GetTransitions(AState, EPSILON);

  for i := 0 to Transitions.Count - 1 do
    Result := Result or ExecuteTransition(Transitions.Strings[i], AIndexInput, AStateEmpty, AIndexInputEmpty);
end;

function TAutomata.GetTransitions(const AState: string; const AIndexInput: Integer): TStringList;
begin
  Result := GetTransitions(AState, FInput[AIndexInput]);
end;

function TAutomata.GetTransitions(const AState, ASymbol: string): TStringList;
var
  IdxState: Integer;
  IdxSymbol: Integer;
begin
  IdxState := FStates.IndexOf(AState);
  IdxSymbol := FSymbols.IndexOf(ASymbol);

  if (IdxState = -1) or (IdxSymbol = -1) then
    Result := nil
  else
    Result := FTransitions[IdxState][IdxSymbol];
end;

procedure TAutomata.Log(const AState: string; const AIndexInput: Integer);
var
  Msg: string;
begin
  if IsInputOver(AIndexInput) then
    Msg := Format('S(%s,%s)', [AState, EPSILON])
  else
    Msg := Format('S(%s,%s)', [AState, FInput[AIndexInput]]);

  OutputDebugString(PChar(Msg));
end;

end.
