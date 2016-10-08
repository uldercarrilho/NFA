unit uNFA;

interface

uses
  System.SysUtils, System.Classes;

type
  TTransitions = array of array of TStringList;

  TNFA = class
  private
    FDescription: string;
    FStates: TStringList;
    FSymbols: TStringList;
    FInitialState: string;
    FFinalStates: TStringList;
    FTransitions: TTransitions;
    FStrings: TStringList;
    // running status
    FInput: string;
    FInputSize: Cardinal;
    FIndexEmptySymbol: Integer;
    procedure CreateTransitions;
    procedure DestroyTransactions;
    function ExecTransition(const AState: string; const AIndexInput: Integer): Boolean;
    function ExecEmptyTransitions(const AState: string; const AIndexInput: Integer): Boolean;
    function ExecStateTransition(const AState: string; const AIndexInput: Integer): Boolean;
    function GetTransitions(const AState, ASymbol: string): TStringList; overload;
    function GetTransitions(const AState: string; const AIndexInput: Integer): TStringList; overload;
    function IsFinalState(const AState: string): Boolean;
    function IsInputOver(const AIndexInput: Integer): Boolean;
    procedure Log(const AState: string; const AIndexInput: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load(AFileName: TFilename);
    function Run(AInput: string): Boolean;

    // automata definition
    property Description: string read FDescription;
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

{ TNFA }

constructor TNFA.Create;
begin
  FStates := TStringList.Create;
  FSymbols := TStringList.Create;
  FInitialState := '';
  FFinalStates := TStringList.Create;
  SetLength(FTransitions, 0, 0);
  FStrings := TStringList.Create;
end;

destructor TNFA.Destroy;
begin
  DestroyTransactions;
  FreeAndNil(FStates);
  FreeAndNil(FSymbols);
  FreeAndNil(FFinalStates);
  FreeAndNil(FStrings);
  inherited;
end;

procedure TNFA.DestroyTransactions;
var
  i, j: Integer;
begin
  for i := 0 to FStates.Count - 1 do
    for j := 0 to FSymbols.Count - 1 do
      FreeAndNil(FTransitions[i][j]);

  SetLength(FTransitions, 0, 0);
end;

procedure TNFA.Load(AFileName: TFilename);
var
  NFADefinition: TIniFile;
  i, j: Integer;
begin
  // destroy previous transitions
  DestroyTransactions;

  { TODO : Validar dados }

  NFADefinition := TIniFile.Create(AFileName);
  try
    FDescription := NFADefinition.ReadString('Automata', 'Description', '');
    FStates.CommaText := NFADefinition.ReadString('Automata', 'States', '');
    FSymbols.CommaText := NFADefinition.ReadString('Automata', 'Symbols', '');
    FInitialState := NFADefinition.ReadString('Automata', 'InitialState', '');
    FFinalStates.CommaText := NFADefinition.ReadString('Automata', 'FinalStates', '');

    // the empty string must be add into list of symbols to be considered in transitions
    FIndexEmptySymbol := FSymbols.IndexOf('"');
    if FIndexEmptySymbol = -1 then
      FIndexEmptySymbol := FSymbols.Add('"');

    CreateTransitions;
    // load transitions from NFA config file
    for i := 0 to FStates.Count -1 do
      for j := 0 to FSymbols.Count - 1 do
        FTransitions[i][j].CommaText := NFADefinition.ReadString(FStates.Strings[i], FSymbols.Strings[j], '');
  finally
    FreeAndNil(NFADefinition);
  end;
end;

procedure TNFA.CreateTransitions;
var
  i, j: Integer;
begin
  SetLength(FTransitions, FStates.Count, FSymbols.Count);
  for i := 0 to FStates.Count - 1 do
    for j := 0 to FSymbols.Count - 1 do
      FTransitions[i][j] := TStringList.Create;
end;

function TNFA.Run(AInput: string): Boolean;
begin
  FInput := AInput;
  FInputSize := Length(AInput);

  Result := ExecTransition(FInitialState, 1);
end;

function TNFA.ExecTransition(const AState: string; const AIndexInput: Integer): Boolean;
begin
  Log(AState, AIndexInput);

  if IsInputOver(AIndexInput) then
    Result := IsFinalState(AState)
  else
    Result := ExecStateTransition(AState, AIndexInput);

  if not Result then
    Result := ExecEmptyTransitions(AState, AIndexInput);
end;

function TNFA.IsInputOver(const AIndexInput: Integer): Boolean;
begin
  Result := AIndexInput > FInputSize;
end;

function TNFA.IsFinalState(const AState: string): Boolean;
begin
  Result := FFinalStates.IndexOf(AState) <> -1;
end;

function TNFA.ExecStateTransition(const AState: string; const AIndexInput: Integer): Boolean;
var
  i: Integer;
  NextInput: Integer;
  Transitions: TStringList;
begin
  Transitions := GetTransitions(AState, AIndexInput);

  if Assigned(Transitions) then
  begin
    NextInput := AIndexInput + 1;

    Result := False;
    for i := 0 to Transitions.Count - 1 do
      Result := Result or ExecTransition(Transitions.Strings[i], NextInput);
  end
  else
    Result := False
end;

function TNFA.ExecEmptyTransitions(const AState: string; const AIndexInput: Integer): Boolean;
var
  i: Integer;
  Transitions: TStringList;
begin
  Transitions := GetTransitions(AState, EPSILON);

  { TODO : Eliminar ciclos com transações vazias. }

  Result := False;
  for i := 0 to Transitions.Count - 1 do
    Result := Result or ExecTransition(Transitions.Strings[i], AIndexInput);
end;

function TNFA.GetTransitions(const AState: string; const AIndexInput: Integer): TStringList;
begin
  Result := GetTransitions(AState, FInput[AIndexInput]);
end;

function TNFA.GetTransitions(const AState, ASymbol: string): TStringList;
var
  IndexState: Integer;
  IndexSymbol: Integer;
begin
  IndexState := FStates.IndexOf(AState);
  IndexSymbol := FSymbols.IndexOf(ASymbol);

  if (IndexState = -1) or (IndexSymbol = -1) then
    Result := nil
  else
    Result := FTransitions[IndexState][IndexSymbol];
end;

procedure TNFA.Log(const AState: string; const AIndexInput: Integer);
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
