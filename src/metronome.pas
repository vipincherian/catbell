unit metronome;

{$mode objfpc}{$H+}{$Q+}{$R+}

interface

uses
  Classes, SysUtils, Forms, audio, settings, {EventLog,} ExtCtrls, log{, sound};

type

  { TMetronome }

  TMetronome = class(TObject)
  private
    FAudioPlayer: TAudioPlayer;
    //FTickSound: TSndSound;
    FRunning: boolean;
    FSubscribers: integer;
    //FLastPlayedTick: longword;
    //FBpm: integer;
    //FInterval: integer;
    //FSubscriptions: integer;
    //FBpmTimer: TTimer;
    //procedure SetBpm(AValue: integer);
    //procedure SetRunning(AValue: boolean);
    {%H-}constructor Create;
    {%H-}destructor {%H-}Destroy; override;

  public

    //procedure HandleTimerTrigger;
    procedure Abort;
    procedure Start;
    procedure Stop;
    //procedure Subscribe;
    //procedure Unsubscribe;
    //procedure OnBpmTimer(Sender: TObject);
    property Running: boolean read FRunning;
    //property Bpm: integer read FBpm write SetBpm;
  end;

var
  MetronomeInstance: TMetronome = nil;

implementation

{ TMetronome }

//procedure TMetronome.SetRunning(AValue: boolean);
//begin
//  if FRunning = AValue then
//    Exit;

//  FRunning := AValue;
//end;

//procedure TMetronome.SetBpm(AValue: integer);
//begin
//  if FBpm = AValue then
//    Exit;
//  FBpm := AValue;
//  //FInterval := 60000 div FBpm;
//  FBpmTimer.Interval := 60000 div FBpm;
//end;

constructor TMetronome.Create;
begin
  //FSubscriptions := 0;
  FAudioPlayer := TAudioPlayer.Create;
  FAudioPlayer.Looped := True;
  FSubscribers := 0;
  //FTickSound := TSndSound.Create;
  //FTickSound.LoadTick;

  //FLastPlayedTick := GetTickCount64;


  //FBpmTimer := TTimer.Create(nil);
  //FBpmTimer.Interval := 60000 div FBpm;
  //FBpmTimer.Enabled := False;
  //FBpmTimer.OnTimer := @OnBpmTimer;

  //Bpm := GlobalUserConfig.Bpm;

  //Bpm := 100;
  FRunning := False;
end;

destructor TMetronome.Destroy;
begin
  //FBpmTimer.Free;
  if FAudioPlayer.Playing then
    Abort;
  //FTickSound.Free;
  FAudioPlayer.Free;
  inherited Destroy;
end;

procedure TMetronome.Abort;
var
  StartTickCount: longword;
begin

  FAudioPlayer.Abort;

  StartTickCount := GetTickCount64;
  while FAudioPlayer.Playing do
  begin
    Logger.Debug('Waiting for frame metronome to stop audio');
    Application.ProcessMessages;
    if GetTickCount64 > (StartTickCount + AUDIO_ABORT_SHORT_WAIT) then
      break;
  end;
end;

procedure TMetronome.Start;
begin
  if not FAudioPlayer.Playing then
  begin
    Assert(FSubscribers = 0);
    FAudioPlayer.Play(SoundPool.RawTickSound);
    Logger.Info('Metronome started.');
  end;
  Inc(FSubscribers);


end;

procedure TMetronome.Stop;
begin
  if FSubscribers > 0 then
  begin
    Assert(FAudioPlayer.Playing);
    FAudioPlayer.Abort;
    Logger.Info('Metronome stopped.');
  end;
  Dec(FSubscribers);
end;

//procedure TMetronome.Subscribe;
//begin
//  Inc(FSubscriptions);
//  if not FBpmTimer.Enabled then
//  begin
//    FBpmTimer.Enabled := True;
//  end;
//end;

//procedure TMetronome.Unsubscribe;
//begin
//  Assert(FSubscriptions > 0);
//  //Assert(False);
//  Dec(FSubscriptions);
//  if FSubscriptions = 0 then
//    FBpmTimer.Enabled := False;
//end;

//procedure TMetronome.OnBpmTimer(Sender: TObject);
//begin
//  //if Bpm <> GlobalUserConfig.Bpm then
//  //  Bpm := GlobalUserConfig.Bpm;
//  if FSubscriptions > 0 then
//  begin
//    if not FAudioPlayer.Playing then
//    ;//FAudioPlayer.Play(FTickSound, GlobalUserConfig.Volume, False);
//  end;
//end;

initialization;
  if AudioSystem.Loaded then
  begin
    MetronomeInstance := TMetronome.Create;
  end;

finalization;
  FreeAndNil(MetronomeInstance);

end.
