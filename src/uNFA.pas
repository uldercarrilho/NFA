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
    procedure CreateTransitions;
    procedure DestroyTransactions;
    function GetPair(const AState, ASymbol: string): TStringList;
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
  System.IniFiles;

{ TNFA }

procedure TNFA.DestroyTransactions;
var
  i, j: Integer;
begin
  for i := 0 to FStates.Count - 1 do
    for j := 0 to FSymbols.Count - 1 do
      FreeAndNil(FTransitions[i][j]);

  SetLength(FTransitions, 0, 0);
end;

constructor TNFA.Create;
begin
  FStates := TStringList.Create;
  FSymbols := TStringList.Create;
  FInitialState := '';
  FFinalStates := TStringList.Create;
  SetLength(FTransitions, 0, 0);
  FStrings := TStringList.Create;
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

destructor TNFA.Destroy;
begin
  DestroyTransactions;
  FreeAndNil(FStates);
  FreeAndNil(FSymbols);
  FreeAndNil(FFinalStates);
  FreeAndNil(FStrings);
  inherited;
end;

function TNFA.GetPair(const AState, ASymbol: string): TStringList;
begin

end;

procedure TNFA.Load(AFileName: TFilename);
var
  NFADefinition: TIniFile;
  i, j: Integer;
begin
  // destroy previous transitions
  DestroyTransactions;

  NFADefinition := TIniFile.Create(AFileName);
  try
    FDescription := NFADefinition.ReadString('Automata', 'Description', '');
    FStates.CommaText := NFADefinition.ReadString('Automata', 'States', '');
    FSymbols.CommaText := NFADefinition.ReadString('Automata', 'Symbols', '');
    FInitialState := NFADefinition.ReadString('Automata', 'InitialState', '');
    FFinalStates.CommaText := NFADefinition.ReadString('Automata', 'FinalStates', '');

    // the empty string must be add into list of symbols to be considered in transitions
    if FSymbols.IndexOf('"') = -1 then
      FSymbols.Add('"');

    CreateTransitions;
    // load transitions from NFA config file
    for i := 0 to FStates.Count -1 do
      for j := 0 to FSymbols.Count - 1 do
        FTransitions[i][j].CommaText := NFADefinition.ReadString(FStates.Strings[i], FSymbols.Strings[j], '');
  finally
    FreeAndNil(NFADefinition);
  end;
end;

function TNFA.Run(AInput: string): Boolean;
begin
  Result := True;
end;

end.
