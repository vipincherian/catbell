unit metronome;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, audio, settings, EventLog, ExtCtrls;

type

  { TMetronome }

  TMetronome = class(TObject)
  private
    FAudio: TAudio;
    FTickSound: TSndSound;
    FRunning: boolean;
    FLastPlayedTick: longword;
    FBpm: integer;
    //FInterval: integer;
    FSubscriptions: integer;
    FBpmTimer: TTimer;
    procedure SetBpm(AValue: integer);
    procedure SetRunning(AValue: boolean);

  public
    constructor Create;
    destructor Destroy; override;
    //procedure HandleTimerTrigger;
    procedure Abort;
    procedure Subscribe;
    procedure Unsubscribe;
    procedure OnBpmTimer(Sender: TObject);
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
  //FInterval := 60000 div FBpm;
  FBpmTimer.Interval := 60000 div FBpm
end;

constructor TMetronome.Create;
begin
  FSubscriptions:=0;
  FAudio := TAudio.Create;
  FTickSound := TSndSound.Create;
  FTickSound.LoadTick;

  FLastPlayedTick := GetTickCount64;


  FBpmTimer := TTimer.Create(nil);
  //FBpmTimer.Interval := 60000 div FBpm;
  FBpmTimer.Enabled := False;
  FBpmTimer.OnTimer := @OnBpmTimer;

  Bpm:=GlobalUserConfig.Bpm;

  //Bpm := 100;
  FRunning := False;
end;

destructor TMetronome.Destroy;
begin
  FBpmTimer.Free;
  if FAudio.Playing then
    Abort;
  FTickSound.Free;
  FAudio.Free;
  inherited Destroy;
end;

procedure TMetronome.Abort;
var
  StartTickCount: longword;
begin

  FAudio.Abort;

  StartTickCount := GetTickCount64;
  while FAudio.Playing do
  begin
    Logger.Debug('Waiting for frame metronome to stop audio');
    Application.ProcessMessages;
    if GetTickCount64 > (StartTickCount + AUDIO_ABORT_SHORT_WAIT) then
      break;
  end;
end;

procedure TMetronome.Subscribe;
begin
  Inc(FSubscriptions);
  if not FBpmTimer.Enabled then
  begin
    FBpmTimer.Enabled:=True;
  end;
end;

procedure TMetronome.Unsubscribe;
begin
  Assert(FSubscriptions > 0);
  //Assert(False);
  Dec(FSubscriptions);
  if FSubscriptions = 0 then
    FBpmTimer.Enabled:=False;
end;

procedure TMetronome.OnBpmTimer(Sender: TObject);
begin
  //if Bpm <> GlobalUserConfig.Bpm then
  //  Bpm := GlobalUserConfig.Bpm;
  if FSubscriptions > 0 then
  begin
    if not FAudio.Playing then
      FAudio.Play(FTickSound, False, GlobalUserConfig.Volume);
  end;
end;

end.
