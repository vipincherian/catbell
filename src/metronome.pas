unit metronome;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, audio, settings, lazlogger;

type

  { TMetronome }

  TMetronome = class(TObject)
  private
    FAudio: TAudio;
    FTickSound: TSndSound;
    FRunning: boolean;
    FLastPlayedTick: longword;
    FBpm: integer;
    FInterval: integer;
    procedure SetBpm(AValue: integer);
    procedure SetRunning(AValue: boolean);

  public
    constructor Create;
    destructor Destroy; override;
    procedure HandleTimerTrigger;
    procedure Abort;
    property Running: boolean read FRunning write SetRunning;
    property Bpm: integer read FBpm write SetBpm;
  end;

implementation

{ TMetronome }

procedure TMetronome.SetRunning(AValue: boolean);
begin
  if FRunning = AValue then
    Exit;

  FRunning := AValue;
end;

procedure TMetronome.SetBpm(AValue: integer);
begin
  if FBpm = AValue then
    Exit;
  FBpm := AValue;
  FInterval := 60000 div FBpm;
end;

constructor TMetronome.Create;
begin
  FAudio := TAudio.Create;
  FTickSound := TSndSound.Create;
  FTickSound.LoadTick;

  FLastPlayedTick := GetTickCount64;

  Bpm := 100;
  FRunning := False;
end;

destructor TMetronome.Destroy;
begin
  FTickSound.Free;
  FAudio.Free;
  inherited Destroy;
end;

procedure TMetronome.HandleTimerTrigger;
var
  CurrentTick: longword;
begin
  CurrentTick := GetTickCount64;
  if (CurrentTick - FLastPlayedTick) > FInterval then
  begin
    FAudio.Play(FTickSound, False, GlobalUserConfig.Volume);
    FLastPlayedTick := CurrentTick;
  end;
end;

procedure TMetronome.Abort;
var
  StartTickCount: longword;
begin

  FAudio.Abort;

  while FAudio.Playing do
  begin
    DebugLn('Waiting for frame metronome to stop audio');
    Application.ProcessMessages;
    if GetTickCount64 > (StartTickCount + AUDIO_ABORT_SHORT_WAIT) then
      break;
  end;
end;

end.
